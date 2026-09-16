;;; tumbel-test.el --- Tests for tumbel.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the entry commands.  Run them with `just test'.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel)
(require 'tumbel-test-support)

(ert-deftest tumbel-test-version ()
  "The package exposes a version string."
  (should (stringp tumbel-version)))

(defmacro tumbel-test-with-dashboard-buffer (&rest body)
  "Evaluate BODY, then kill the dashboard buffer."
  (declare (indent 0))
  `(unwind-protect
       (progn ,@body)
     (when (get-buffer "*tumbel: dashboard*")
       (kill-buffer "*tumbel: dashboard*"))))

(ert-deftest tumbel-test-dashboard ()
  "The dashboard pages by offset with the bearer token."
  (tumbel-test-logged-in
    (tumbel-test-with-backend
        `(("offset=6" 200 ,(tumbel-test-fixture "posts-page2.json"))
          ("/user/dashboard" 200 ,(tumbel-test-fixture "posts.json")))
      (tumbel-test-with-dashboard-buffer
        (tumbel-dashboard)
        (should (equal (buffer-name) "*tumbel: dashboard*"))
        (should (string-prefix-p "Dashboard\n" (buffer-string)))
        (should (equal (length (ewoc-collect tumbel-feed--ewoc #'identity))
                       6))
        (should (string-match-p
                 "/user/dashboard\\?npf=true&limit=20&offset=0&reblog_info=true"
                 (nth 1 (tumbel-test-call 0))))
        (should (equal (tumbel-test-call-header (tumbel-test-call 0)
                                                "Authorization")
                       "Bearer TOKEN"))
        (tumbel-feed-load-more)
        (should (equal (length (ewoc-collect tumbel-feed--ewoc #'identity))
                       7))
        (should (string-match-p "offset=6" (nth 1 (tumbel-test-call 0))))))))

(ert-deftest tumbel-test-dashboard-logged-out ()
  "Logged out, the dashboard buffer reports the missing login."
  (tumbel-test-logged-out
    (tumbel-test-with-backend nil
      (tumbel-test-with-dashboard-buffer
        (tumbel-dashboard)
        (should (string-match-p "Not logged in" (buffer-string)))
        (should-not tumbel-feed--loading)
        (should (null tumbel-test-calls))))))

(ert-deftest tumbel-test-entry-logs-in-first ()
  "The entry command logs in and then shows the dashboard."
  (let (then)
    (tumbel-test-logged-out
      (cl-letf (((symbol-function 'tumbel-login)
                 (lambda (&optional callback) (setq then callback))))
        (tumbel)
        (should (eq then #'tumbel-dashboard))))
    (tumbel-test-logged-in
      (tumbel-test-with-backend
          `(("/user/dashboard" 200 ,(tumbel-test-fixture "posts.json")))
        (tumbel-test-with-dashboard-buffer
          (tumbel)
          (should (equal (buffer-name) "*tumbel: dashboard*")))))))

(ert-deftest tumbel-test-dispatch ()
  "The transient menu exists and is bound in feed buffers."
  (should (fboundp 'tumbel-dispatch))
  (should (eq (lookup-key tumbel-feed-mode-map (kbd "?")) 'tumbel-dispatch)))
(provide 'tumbel-test)
;;; tumbel-test.el ends here
