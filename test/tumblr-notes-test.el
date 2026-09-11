;;; tumblr-notes-test.el --- Tests for tumblr-notes.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for rendering notes and the notes view.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-notes)
(require 'tumblr-test-support)

(defun tumblr-notes-test-render (note)
  "Render NOTE and return the buffer text."
  (with-temp-buffer
    (tumblr-notes-insert note)
    (buffer-string)))

(defun tumblr-notes-test-notes ()
  "Return the notes of the fixture."
  (alist-get 'notes
             (alist-get 'response
                        (tumblr-http-parse-json (tumblr-test-fixture "notes.json")))))

(defmacro tumblr-notes-test-with-view (&rest body)
  "Show the notes of post 1003 from fixtures and run BODY there."
  (declare (indent 0))
  `(tumblr-test-logged-out
     (tumblr-test-with-backend
         `(("before_timestamp=1757548300" 200
            ,(tumblr-test-fixture "notes-page2.json"))
           ("mode=likes" 200 ,(tumblr-test-fixture "notes-page2.json"))
           ("/blog/example/notes" 200 ,(tumblr-test-fixture "notes.json"))
           ("/blog/rebloggy/posts/555" 200
            ,(tumblr-test-envelope (tumblr-test-post-fidelity "1001"))))
       (when (get-buffer "*tumblr: notes/1003*")
         (kill-buffer "*tumblr: notes/1003*"))
       (unwind-protect
           (with-current-buffer (tumblr-notes (tumblr-test-post "1003"))
             ,@body)
         (dolist (name '("*tumblr: notes/1003*" "*tumblr: post/555*"))
           (when (get-buffer name)
             (kill-buffer name)))))))

(ert-deftest tumblr-notes-test-render ()
  "Each kind of note renders as a line with the author as a button."
  (let ((notes (tumblr-notes-test-notes)))
    (should (string-match-p "\\`replier replied · .*\nGreat post!\n\\'"
                            (tumblr-notes-test-render (nth 0 notes))))
    (should (string-match-p
             "\\`↻ rebloggy reblogged this from example · .*\nAdding my thoughts\\.\n#thoughts\n\\'"
             (tumblr-notes-test-render (nth 1 notes))))
    (should (string-match-p "\\`↻ quiet reblogged this from example · [^\n]*\n\\'"
                            (tumblr-notes-test-render (nth 2 notes))))
    (should (string-match-p "\\`♥ liker liked this · [^\n]*\n\\'"
                            (tumblr-notes-test-render (nth 3 notes))))
    (should (string-match-p "\\`example posted this · [^\n]*\n\\'"
                            (tumblr-notes-test-render (nth 4 notes))))
    (should (string-match-p "\\`x (weird) · [^\n]*\n\\'"
                            (tumblr-notes-test-render
                             '((type . "weird") (blog_name . "x")
                               (timestamp . 1)))))
    (let ((out (tumblr-notes-test-render (nth 1 notes))))
      (should (equal (get-text-property 2 'tumblr-blog-name out) "rebloggy"))
      (should (equal (get-text-property (string-match "example" out)
                                        'tumblr-blog-name out)
                     "example")))))

(ert-deftest tumblr-notes-test-view ()
  "The notes view pages by timestamp and shows the totals."
  (tumblr-notes-test-with-view
    (should (equal (buffer-name) "*tumblr: notes/1003*"))
    (should (string-prefix-p
             "Notes on example/1003\n1234 notes · 1000 likes · 200 reblogs · showing conversation\n\n"
             (buffer-string)))
    (should (equal (length (ewoc-collect tumblr-feed--ewoc #'identity)) 5))
    (should (string-match-p "/blog/example/notes\\?id=1003&mode=conversation&api_key=KEY"
                            (nth 1 (tumblr-test-call 0))))
    (should (string-match-p "\\[Load more\\]" (buffer-string)))
    (tumblr-feed-load-more)
    (should (string-match-p "before_timestamp=1757548300"
                            (nth 1 (tumblr-test-call 0))))
    (should (equal (length (ewoc-collect tumblr-feed--ewoc #'identity)) 6))
    (should tumblr-feed--exhausted)
    (should (string-match-p "early-liker liked this" (buffer-string)))))

(ert-deftest tumblr-notes-test-actions-refused ()
  "Post actions do not apply to notes."
  (tumblr-notes-test-with-view
    (should-error (tumblr-feed-like) :type 'user-error)
    (should-error (tumblr-feed-reblog-with-comment) :type 'user-error)
    (should (equal (length tumblr-test-calls) 1))))

(ert-deftest tumblr-notes-test-open ()
  "RET opens the reblog behind a reblog note, or the author otherwise."
  (tumblr-notes-test-with-view
    (let (blog)
      (cl-letf (((symbol-function 'tumblr-blog) (lambda (name) (setq blog name))))
        (goto-char (point-min))
        (search-forward "Great post!")
        (tumblr-feed-open-post)
        (should (equal blog "replier"))
        (search-forward "Adding my thoughts")
        (tumblr-feed-open-post)
        (should (get-buffer "*tumblr: post/555*"))
        (should (string-match-p "/blog/rebloggy/posts/555"
                                (nth 1 (tumblr-test-call 0))))))))

(ert-deftest tumblr-notes-test-switch-mode ()
  "Switching the mode fetches the notes again with it."
  (tumblr-notes-test-with-view
    (tumblr-notes-switch-mode "likes")
    (should (string-match-p "mode=likes" (nth 1 (tumblr-test-call 0))))
    (should (equal tumblr-notes--mode "likes"))
    (should (equal (length (ewoc-collect tumblr-feed--ewoc #'identity)) 1))))

(ert-deftest tumblr-notes-test-feed-key ()
  "The v key in a feed hands the post to the notes function."
  (let (shown)
    (let ((tumblr-npf-open-notes-function (lambda (post) (setq shown post))))
      (cl-letf (((symbol-function 'tumblr-feed-post-at-point)
                 (lambda () (tumblr-test-post "1001"))))
        (with-temp-buffer
          (tumblr-feed-mode)
          (setq tumblr-feed--source (tumblr-feed-source-create :name "x"))
          (let ((inhibit-read-only t))
            (setq tumblr-feed--ewoc (ewoc-create #'ignore "" "" t))
            (ewoc-enter-last tumblr-feed--ewoc
                             (tumblr-feed-item-create
                              :post (tumblr-test-post "1001"))))
          (tumblr-feed-show-notes)
          (should (equal (tumblr-npf-post-id shown) "1001")))))
    (should (eq (lookup-key tumblr-feed-mode-map (kbd "v"))
                'tumblr-feed-show-notes))))

(provide 'tumblr-notes-test)
;;; tumblr-notes-test.el ends here
