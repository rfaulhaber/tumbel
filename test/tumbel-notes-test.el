;;; tumbel-notes-test.el --- Tests for tumbel-notes.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for rendering notes and the notes view.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-notes)
(require 'tumbel-test-support)

(defun tumbel-notes-test-render (note)
  "Render NOTE and return the buffer text."
  (with-temp-buffer
    (tumbel-notes-insert note)
    (buffer-string)))

(defun tumbel-notes-test-notes ()
  "Return the notes of the fixture."
  (alist-get 'notes
             (alist-get 'response
                        (tumbel-http-parse-json (tumbel-test-fixture "notes.json")))))

(defmacro tumbel-notes-test-with-view (&rest body)
  "Show the notes of post 1003 from fixtures and run BODY there."
  (declare (indent 0))
  `(tumbel-test-logged-out
     (tumbel-test-with-backend
         `(("before_timestamp=1757548300" 200
            ,(tumbel-test-fixture "notes-page2.json"))
           ("mode=likes" 200 ,(tumbel-test-fixture "notes-page2.json"))
           ("/blog/example/notes" 200 ,(tumbel-test-fixture "notes.json"))
           ("/blog/rebloggy/posts/555" 200
            ,(tumbel-test-envelope (tumbel-test-post-fidelity "1001"))))
       (when (get-buffer "*tumbel: notes/1003*")
         (kill-buffer "*tumbel: notes/1003*"))
       (unwind-protect
           (with-current-buffer (tumbel-notes (tumbel-test-post "1003"))
             ,@body)
         (dolist (name '("*tumbel: notes/1003*" "*tumbel: post/555*"))
           (when (get-buffer name)
             (kill-buffer name)))))))

(ert-deftest tumbel-notes-test-render ()
  "Each kind of note renders as a line with the author as a button."
  (let ((notes (tumbel-notes-test-notes)))
    (should (string-match-p "\\`replier replied · .*\nGreat post!\n\\'"
                            (tumbel-notes-test-render (nth 0 notes))))
    (should (string-match-p
             "\\`↻ rebloggy reblogged this from example · .*\nAdding my thoughts\\.\n#thoughts\n\\'"
             (tumbel-notes-test-render (nth 1 notes))))
    (should (string-match-p "\\`↻ quiet reblogged this from example · [^\n]*\n\\'"
                            (tumbel-notes-test-render (nth 2 notes))))
    (should (string-match-p "\\`♥ liker liked this · [^\n]*\n\\'"
                            (tumbel-notes-test-render (nth 3 notes))))
    (should (string-match-p "\\`example posted this · [^\n]*\n\\'"
                            (tumbel-notes-test-render (nth 4 notes))))
    (should (string-match-p "\\`x (weird) · [^\n]*\n\\'"
                            (tumbel-notes-test-render
                             '((type . "weird") (blog_name . "x")
                               (timestamp . 1)))))
    (let ((out (tumbel-notes-test-render (nth 1 notes))))
      (should (equal (get-text-property 2 'tumbel-blog-name out) "rebloggy"))
      (should (equal (get-text-property (string-match "example" out)
                                        'tumbel-blog-name out)
                     "example")))))

(ert-deftest tumbel-notes-test-view ()
  "The notes view pages by timestamp and shows the totals."
  (tumbel-notes-test-with-view
    (should (equal (buffer-name) "*tumbel: notes/1003*"))
    (should (string-prefix-p
             "Notes on example/1003\n1234 notes · 1000 likes · 200 reblogs · showing conversation\n\n"
             (buffer-string)))
    (should (equal (length (ewoc-collect tumbel-feed--ewoc #'identity)) 5))
    (should (string-match-p "/blog/example/notes\\?id=1003&mode=conversation&api_key=KEY"
                            (nth 1 (tumbel-test-call 0))))
    (should (string-match-p "\\[Load more\\]" (buffer-string)))
    (tumbel-feed-load-more)
    (should (string-match-p "before_timestamp=1757548300"
                            (nth 1 (tumbel-test-call 0))))
    (should (equal (length (ewoc-collect tumbel-feed--ewoc #'identity)) 6))
    (should tumbel-feed--exhausted)
    (should (string-match-p "early-liker liked this" (buffer-string)))))

(ert-deftest tumbel-notes-test-actions-refused ()
  "Post actions do not apply to notes."
  (tumbel-notes-test-with-view
    (should-error (tumbel-feed-like) :type 'user-error)
    (should-error (tumbel-feed-reblog-with-comment) :type 'user-error)
    (should (equal (length tumbel-test-calls) 1))))

(ert-deftest tumbel-notes-test-open ()
  "RET opens the reblog behind a reblog note, or the author otherwise."
  (tumbel-notes-test-with-view
    (let (blog)
      (cl-letf (((symbol-function 'tumbel-blog) (lambda (name) (setq blog name))))
        (goto-char (point-min))
        (search-forward "Great post!")
        (tumbel-feed-open-post)
        (should (equal blog "replier"))
        (search-forward "Adding my thoughts")
        (tumbel-feed-open-post)
        (should (get-buffer "*tumbel: post/555*"))
        (should (string-match-p "/blog/rebloggy/posts/555"
                                (nth 1 (tumbel-test-call 0))))))))

(ert-deftest tumbel-notes-test-switch-mode ()
  "Switching the mode fetches the notes again with it."
  (tumbel-notes-test-with-view
    (tumbel-notes-switch-mode "likes")
    (should (string-match-p "mode=likes" (nth 1 (tumbel-test-call 0))))
    (should (equal tumbel-notes--mode "likes"))
    (should (equal (length (ewoc-collect tumbel-feed--ewoc #'identity)) 1))))

(ert-deftest tumbel-notes-test-feed-key ()
  "The v key in a feed hands the post to the notes function."
  (let (shown)
    (let ((tumbel-npf-open-notes-function (lambda (post) (setq shown post))))
      (cl-letf (((symbol-function 'tumbel-feed-post-at-point)
                 (lambda () (tumbel-test-post "1001"))))
        (with-temp-buffer
          (tumbel-feed-mode)
          (setq tumbel-feed--source (tumbel-feed-source-create :name "x"))
          (let ((inhibit-read-only t))
            (setq tumbel-feed--ewoc (ewoc-create #'ignore "" "" t))
            (ewoc-enter-last tumbel-feed--ewoc
                             (tumbel-feed-item-create
                              :post (tumbel-test-post "1001"))))
          (tumbel-feed-show-notes)
          (should (equal (tumbel-npf-post-id shown) "1001")))))
    (should (eq (lookup-key tumbel-feed-mode-map (kbd "v"))
                'tumbel-feed-show-notes))))

(provide 'tumbel-notes-test)
;;; tumbel-notes-test.el ends here
