;;; tumbel-notifications-test.el --- Tests for tumbel-notifications.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for rendering activity items and the activity view.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-notifications)
(require 'tumbel-test-support)

(defun tumbel-notifications-test-items ()
  "Return the activity items of the fixture."
  (alist-get 'notifications
             (alist-get 'response
                        (tumbel-http-parse-json
                         (tumbel-test-fixture "notifications.json")))))

(defun tumbel-notifications-test-render (item)
  "Render ITEM and return the buffer text."
  (with-temp-buffer
    (tumbel-notifications-insert item)
    (buffer-string)))

(defun tumbel-notifications-test-unread-p (string)
  "Return non-nil when the start of STRING carries the unread face."
  (let ((face (get-text-property 0 'face string)))
    (or (eq face 'tumbel-unread)
        (and (listp face) (memq 'tumbel-unread face) t))))
(defmacro tumbel-notifications-test-with-view (&rest body)
  "Show the activity of example from fixtures and run BODY there."
  (declare (indent 0))
  `(tumbel-test-logged-in
     (tumbel-test-with-backend
         `(("before=1757548200" 200
            ,(tumbel-test-envelope '((notifications . []))))
           ("/blog/example/notifications" 200
            ,(tumbel-test-fixture "notifications.json"))
           ("/blog/example/posts/1001" 200
            ,(tumbel-test-envelope (tumbel-test-post-fidelity "1001")))
           ("/blog/rebloggy/posts/555" 200
            ,(tumbel-test-envelope (tumbel-test-post-fidelity "1001"))))
       (cl-letf (((symbol-function 'tumbel-user-default-blog)
                  (lambda () "example")))
         (when (get-buffer "*tumbel: notifications/example*")
           (kill-buffer "*tumbel: notifications/example*"))
         (unwind-protect
             (with-current-buffer (tumbel-notifications)
               ,@body)
           (dolist (name '("*tumbel: notifications/example*"
                           "*tumbel: post/1001*" "*tumbel: post/555*"))
             (when (get-buffer name)
               (kill-buffer name))))))))

(ert-deftest tumbel-notifications-test-render ()
  "Each kind of activity renders as a line, unread ones emphasized."
  (let ((items (tumbel-notifications-test-items)))
    (let ((out (tumbel-notifications-test-render (nth 0 items))))
      (should (string-match-p "\\`liker liked your post · [^\n]*\n\\'" out))
      (should (tumbel-notifications-test-unread-p out))
      (should (equal (get-text-property 0 'tumbel-blog-name out) "liker")))
    (should (string-match-p "\\`replier replied to your post · .*\nSo true\n\\'"
                            (tumbel-notifications-test-render (nth 1 items))))
    (let ((out (tumbel-notifications-test-render (nth 2 items))))
      (should (string-match-p
               "\\`rebloggy reblogged your post and added · .*\nAdding my thoughts\\.\n#thoughts\n\\'"
               out))
      (should-not (tumbel-notifications-test-unread-p out)))
    (should (string-match-p "\\`newfan followed you · [^\n]*\n\\'"
                            (tumbel-notifications-test-render (nth 3 items))))
    (should (string-match-p "\\`asker asked you a question · [^\n]*\n\\'"
                            (tumbel-notifications-test-render (nth 4 items))))
    (should (string-match-p "\\`future (something_new) · [^\n]*\n\\'"
                            (tumbel-notifications-test-render (nth 5 items))))))

(ert-deftest tumbel-notifications-test-view ()
  "The activity view needs the token and pages with the links it gets."
  (tumbel-notifications-test-with-view
    (should (equal (buffer-name) "*tumbel: notifications/example*"))
    (should (string-prefix-p "Activity on example\n" (buffer-string)))
    (should (equal (length (ewoc-collect tumbel-feed--ewoc #'identity)) 6))
    (should (string-suffix-p "/blog/example/notifications"
                             (nth 1 (tumbel-test-call 0))))
    (should (equal (tumbel-test-call-header (tumbel-test-call 0)
                                            "Authorization")
                   "Bearer TOKEN"))
    (tumbel-feed-load-more)
    (should (string-match-p "before=1757548200" (nth 1 (tumbel-test-call 0))))
    (should tumbel-feed--exhausted)
    (should-error (tumbel-feed-like) :type 'user-error)))

(ert-deftest tumbel-notifications-test-open ()
  "RET opens the post concerned, the reblog made, or the actor."
  (tumbel-notifications-test-with-view
    (let (blog)
      (cl-letf (((symbol-function 'tumbel-blog) (lambda (name) (setq blog name))))
        (goto-char (point-min))
        (search-forward "liked your post")
        (tumbel-feed-open-post)
        (should (get-buffer "*tumbel: post/1001*"))
        (should (string-match-p "/blog/example/posts/1001"
                                (nth 1 (tumbel-test-call 0))))
        (switch-to-buffer "*tumbel: notifications/example*")
        (goto-char (point-min))
        (search-forward "reblogged your post")
        (tumbel-feed-open-post)
        (should (string-match-p "/blog/rebloggy/posts/555"
                                (nth 1 (tumbel-test-call 0))))
        (switch-to-buffer "*tumbel: notifications/example*")
        (goto-char (point-min))
        (search-forward "followed you")
        (tumbel-feed-open-post)
        (should (equal blog "newfan"))))))

(provide 'tumbel-notifications-test)
;;; tumbel-notifications-test.el ends here
