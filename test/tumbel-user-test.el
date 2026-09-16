;;; tumbel-user-test.el --- Tests for tumbel-user.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the cached account information.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-user)
(require 'tumbel-test-support)

(defmacro tumbel-user-test-with-info (&rest body)
  "Run BODY logged in, with the user-info fixture served."
  (declare (indent 0))
  `(let ((tumbel-user--info nil)
         (tumbel-default-blog nil))
     (tumbel-test-logged-in
       (tumbel-test-with-backend `(("/user/info" 200
                                    ,(tumbel-test-fixture "user-info.json")))
         ,@body))))

(ert-deftest tumbel-user-test-info-is-cached ()
  "The user info is fetched once."
  (tumbel-user-test-with-info
    (should (equal (alist-get 'name (tumbel-user-info)) "example"))
    (tumbel-user-info)
    (should (equal (length tumbel-test-calls) 1))
    (should (equal (tumbel-test-call-header (tumbel-test-call 0)
                                            "Authorization")
                   "Bearer TOKEN"))
    (tumbel-user-info t)
    (should (equal (length tumbel-test-calls) 2))
    (tumbel-user-clear)
    (tumbel-user-info)
    (should (equal (length tumbel-test-calls) 3))))

(ert-deftest tumbel-user-test-blogs ()
  "The default blog is the primary one unless customized."
  (tumbel-user-test-with-info
    (should (equal (tumbel-user-blog-names) '("secondary" "example")))
    (should (equal (tumbel-user-default-blog) "example"))
    (let ((tumbel-default-blog "secondary"))
      (should (equal (tumbel-user-default-blog) "secondary")))
    (should (tumbel-user-own-blog-p "example"))
    (should-not (tumbel-user-own-blog-p "someone-else"))))

(ert-deftest tumbel-user-test-own-blog-logged-out ()
  "Logged out, no blog is owned and nothing is fetched."
  (let ((tumbel-user--info nil))
    (tumbel-test-logged-out
      (tumbel-test-with-backend nil
        (should-not (tumbel-user-own-blog-p "example"))
        (should (null tumbel-test-calls))))))

(ert-deftest tumbel-user-test-filters ()
  "Filters are fetched once and forgotten with the login."
  (let (got)
    (tumbel-test-logged-in
      (let ((tumbel-user--filters nil))
        (tumbel-test-with-backend
            `(("/user/filtered_tags" 200
               ,(tumbel-test-envelope '((filtered_tags . ["a" "b"]))))
              ("/user/filtered_content" 200
               ,(tumbel-test-envelope '((filtered_content . ["spoilers"])))))
          (tumbel-user-load-filters (lambda (filters) (setq got filters)))
          (should (equal got '(("a" "b") . ("spoilers"))))
          (should (equal (tumbel-user-filters) got))
          (tumbel-user-load-filters (lambda (filters) (setq got (list filters))))
          (should (equal (length tumbel-test-calls) 2))
          (tumbel-user-clear)
          (should (null (tumbel-user-filters))))))
    (tumbel-test-logged-out
      (tumbel-test-with-backend nil
        (tumbel-user-load-filters (lambda (filters) (setq got filters)))
        (should (null got))
        (should (null tumbel-test-calls))))))
(provide 'tumbel-user-test)
;;; tumbel-user-test.el ends here
