;;; tumblr-test.el --- Tests for tumblr.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the entry commands.  Run them with `just test'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr)
(require 'tumblr-test-support)

(ert-deftest tumblr-test-version ()
  "The package exposes a version string."
  (should (stringp tumblr-version)))

(defmacro tumblr-test-with-dashboard-buffer (&rest body)
  "Evaluate BODY, then kill the dashboard buffer."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (when (get-buffer "*tumblr: dashboard*")
       (kill-buffer "*tumblr: dashboard*"))))

(ert-deftest tumblr-test-dashboard ()
  "The dashboard pages by offset with the bearer token."
  (tumblr-test-logged-in
    (tumblr-test-with-backend
        `(("offset=6" 200 ,(tumblr-test-fixture "posts-page2.json"))
          ("/user/dashboard" 200 ,(tumblr-test-fixture "posts.json")))
      (tumblr-test-with-dashboard-buffer
        (tumblr-dashboard)
        (should (equal (buffer-name) "*tumblr: dashboard*"))
        (should (string-prefix-p "Dashboard\n" (buffer-string)))
        (should (equal (length (ewoc-collect tumblr-feed--ewoc #'identity))
                       6))
        (should (string-match-p
                 "/user/dashboard\\?npf=true&limit=20&offset=0&reblog_info=true"
                 (nth 1 (tumblr-test-call 0))))
        (should (equal (tumblr-test-call-header (tumblr-test-call 0)
                                                "Authorization")
                       "Bearer TOKEN"))
        (tumblr-feed-load-more)
        (should (equal (length (ewoc-collect tumblr-feed--ewoc #'identity))
                       7))
        (should (string-match-p "offset=6" (nth 1 (tumblr-test-call 0))))))))

(ert-deftest tumblr-test-dashboard-logged-out ()
  "Logged out, the dashboard buffer reports the missing login."
  (tumblr-test-logged-out
    (tumblr-test-with-backend nil
      (tumblr-test-with-dashboard-buffer
        (tumblr-dashboard)
        (should (string-match-p "Not logged in" (buffer-string)))
        (should-not tumblr-feed--loading)
        (should (null tumblr-test-calls))))))

(ert-deftest tumblr-test-entry-logs-in-first ()
  "The entry command logs in and then shows the dashboard."
  (let (then)
    (tumblr-test-logged-out
      (cl-letf (((symbol-function 'tumblr-login)
                 (lambda (&optional callback) (setq then callback))))
        (tumblr)
        (should (eq then #'tumblr-dashboard))))
    (tumblr-test-logged-in
      (tumblr-test-with-backend
          `(("/user/dashboard" 200 ,(tumblr-test-fixture "posts.json")))
        (tumblr-test-with-dashboard-buffer
          (tumblr)
          (should (equal (buffer-name) "*tumblr: dashboard*")))))))

(ert-deftest tumblr-test-dispatch ()
  "The transient menu exists and is bound in feed buffers."
  (should (fboundp 'tumblr-dispatch))
  (should (eq (lookup-key tumblr-feed-mode-map (kbd "?")) 'tumblr-dispatch)))
(provide 'tumblr-test)
;;; tumblr-test.el ends here
