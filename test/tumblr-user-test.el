;;; tumblr-user-test.el --- Tests for tumblr-user.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the cached account information.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-user)
(require 'tumblr-test-support)

(defmacro tumblr-user-test-with-info (&rest body)
  "Run BODY logged in, with the user-info fixture served."
  (declare (indent 0))
  `(let ((tumblr-user--info nil)
         (tumblr-default-blog nil))
     (tumblr-test-logged-in
       (tumblr-test-with-backend `(("/user/info" 200
                                    ,(tumblr-test-fixture "user-info.json")))
         ,@body))))

(ert-deftest tumblr-user-test-info-is-cached ()
  "The user info is fetched once."
  (tumblr-user-test-with-info
    (should (equal (alist-get 'name (tumblr-user-info)) "example"))
    (tumblr-user-info)
    (should (equal (length tumblr-test-calls) 1))
    (should (equal (tumblr-test-call-header (tumblr-test-call 0)
                                            "Authorization")
                   "Bearer TOKEN"))
    (tumblr-user-info t)
    (should (equal (length tumblr-test-calls) 2))
    (tumblr-user-clear)
    (tumblr-user-info)
    (should (equal (length tumblr-test-calls) 3))))

(ert-deftest tumblr-user-test-blogs ()
  "The default blog is the primary one unless customized."
  (tumblr-user-test-with-info
    (should (equal (tumblr-user-blog-names) '("secondary" "example")))
    (should (equal (tumblr-user-default-blog) "example"))
    (let ((tumblr-default-blog "secondary"))
      (should (equal (tumblr-user-default-blog) "secondary")))
    (should (tumblr-user-own-blog-p "example"))
    (should-not (tumblr-user-own-blog-p "someone-else"))))

(ert-deftest tumblr-user-test-own-blog-logged-out ()
  "Logged out, no blog is owned and nothing is fetched."
  (let ((tumblr-user--info nil))
    (tumblr-test-logged-out
      (tumblr-test-with-backend nil
        (should-not (tumblr-user-own-blog-p "example"))
        (should (null tumblr-test-calls))))))

(ert-deftest tumblr-user-test-filters ()
  "Filters are fetched once and forgotten with the login."
  (let (got)
    (tumblr-test-logged-in
      (let ((tumblr-user--filters nil))
        (tumblr-test-with-backend
            `(("/user/filtered_tags" 200
               ,(tumblr-test-envelope '((filtered_tags . ["a" "b"]))))
              ("/user/filtered_content" 200
               ,(tumblr-test-envelope '((filtered_content . ["spoilers"])))))
          (tumblr-user-load-filters (lambda (filters) (setq got filters)))
          (should (equal got '(("a" "b") . ("spoilers"))))
          (should (equal (tumblr-user-filters) got))
          (tumblr-user-load-filters (lambda (filters) (setq got (list filters))))
          (should (equal (length tumblr-test-calls) 2))
          (tumblr-user-clear)
          (should (null (tumblr-user-filters))))))
    (tumblr-test-logged-out
      (tumblr-test-with-backend nil
        (tumblr-user-load-filters (lambda (filters) (setq got filters)))
        (should (null got))
        (should (null tumblr-test-calls))))))
(provide 'tumblr-user-test)
;;; tumblr-user-test.el ends here
