;;; tumblr-notifications-test.el --- Tests for tumblr-notifications.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for rendering activity items and the activity view.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-notifications)
(require 'tumblr-test-support)

(defun tumblr-notifications-test-items ()
  "Return the activity items of the fixture."
  (alist-get 'notifications
             (alist-get 'response
                        (tumblr-http-parse-json
                         (tumblr-test-fixture "notifications.json")))))

(defun tumblr-notifications-test-render (item)
  "Render ITEM and return the buffer text."
  (with-temp-buffer
    (tumblr-notifications-insert item)
    (buffer-string)))

(defun tumblr-notifications-test-unread-p (string)
  "Return non-nil when the start of STRING carries the unread face."
  (let ((face (get-text-property 0 'face string)))
    (or (eq face 'tumblr-unread)
        (and (listp face) (memq 'tumblr-unread face) t))))
(defmacro tumblr-notifications-test-with-view (&rest body)
  "Show the activity of example from fixtures and run BODY there."
  (declare (indent 0))
  `(tumblr-test-logged-in
     (tumblr-test-with-backend
         `(("before=1757548200" 200
            ,(tumblr-test-envelope '((notifications . []))))
           ("/blog/example/notifications" 200
            ,(tumblr-test-fixture "notifications.json"))
           ("/blog/example/posts/1001" 200
            ,(tumblr-test-envelope (tumblr-test-post-fidelity "1001")))
           ("/blog/rebloggy/posts/555" 200
            ,(tumblr-test-envelope (tumblr-test-post-fidelity "1001"))))
       (cl-letf (((symbol-function 'tumblr-user-default-blog)
                  (lambda () "example")))
         (when (get-buffer "*tumblr: notifications/example*")
           (kill-buffer "*tumblr: notifications/example*"))
         (unwind-protect
             (with-current-buffer (tumblr-notifications)
               ,@body)
           (dolist (name '("*tumblr: notifications/example*"
                           "*tumblr: post/1001*" "*tumblr: post/555*"))
             (when (get-buffer name)
               (kill-buffer name))))))))

(ert-deftest tumblr-notifications-test-render ()
  "Each kind of activity renders as a line, unread ones emphasized."
  (let ((items (tumblr-notifications-test-items)))
    (let ((out (tumblr-notifications-test-render (nth 0 items))))
      (should (string-match-p "\\`liker liked your post · [^\n]*\n\\'" out))
      (should (tumblr-notifications-test-unread-p out))
      (should (equal (get-text-property 0 'tumblr-blog-name out) "liker")))
    (should (string-match-p "\\`replier replied to your post · .*\nSo true\n\\'"
                            (tumblr-notifications-test-render (nth 1 items))))
    (let ((out (tumblr-notifications-test-render (nth 2 items))))
      (should (string-match-p
               "\\`rebloggy reblogged your post and added · .*\nAdding my thoughts\\.\n#thoughts\n\\'"
               out))
      (should-not (tumblr-notifications-test-unread-p out)))
    (should (string-match-p "\\`newfan followed you · [^\n]*\n\\'"
                            (tumblr-notifications-test-render (nth 3 items))))
    (should (string-match-p "\\`asker asked you a question · [^\n]*\n\\'"
                            (tumblr-notifications-test-render (nth 4 items))))
    (should (string-match-p "\\`future (something_new) · [^\n]*\n\\'"
                            (tumblr-notifications-test-render (nth 5 items))))))

(ert-deftest tumblr-notifications-test-view ()
  "The activity view needs the token and pages with the links it gets."
  (tumblr-notifications-test-with-view
    (should (equal (buffer-name) "*tumblr: notifications/example*"))
    (should (string-prefix-p "Activity on example\n" (buffer-string)))
    (should (equal (length (ewoc-collect tumblr-feed--ewoc #'identity)) 6))
    (should (string-suffix-p "/blog/example/notifications"
                             (nth 1 (tumblr-test-call 0))))
    (should (equal (tumblr-test-call-header (tumblr-test-call 0)
                                            "Authorization")
                   "Bearer TOKEN"))
    (tumblr-feed-load-more)
    (should (string-match-p "before=1757548200" (nth 1 (tumblr-test-call 0))))
    (should tumblr-feed--exhausted)
    (should-error (tumblr-feed-like) :type 'user-error)))

(ert-deftest tumblr-notifications-test-open ()
  "RET opens the post concerned, the reblog made, or the actor."
  (tumblr-notifications-test-with-view
    (let (blog)
      (cl-letf (((symbol-function 'tumblr-blog) (lambda (name) (setq blog name))))
        (goto-char (point-min))
        (search-forward "liked your post")
        (tumblr-feed-open-post)
        (should (get-buffer "*tumblr: post/1001*"))
        (should (string-match-p "/blog/example/posts/1001"
                                (nth 1 (tumblr-test-call 0))))
        (switch-to-buffer "*tumblr: notifications/example*")
        (goto-char (point-min))
        (search-forward "reblogged your post")
        (tumblr-feed-open-post)
        (should (string-match-p "/blog/rebloggy/posts/555"
                                (nth 1 (tumblr-test-call 0))))
        (switch-to-buffer "*tumblr: notifications/example*")
        (goto-char (point-min))
        (search-forward "followed you")
        (tumblr-feed-open-post)
        (should (equal blog "newfan"))))))

(provide 'tumblr-notifications-test)
;;; tumblr-notifications-test.el ends here
