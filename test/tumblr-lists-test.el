;;; tumblr-lists-test.el --- Tests for tumblr-lists.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the following and followers tables.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-lists)
(require 'tumblr-test-support)

(defmacro tumblr-lists-test-with-buffer (name &rest body)
  "Kill the buffer NAME before and after evaluating BODY."
  (declare (indent 1))
  `(progn
     (when (get-buffer ,name)
       (kill-buffer ,name))
     (unwind-protect
         (progn ,@body)
       (when (get-buffer ,name)
         (kill-buffer ,name)))))

(defun tumblr-lists-test-names ()
  "Return the blog names listed in the current buffer."
  (mapcar #'car tabulated-list-entries))

(ert-deftest tumblr-lists-test-following ()
  "The following table pages by offset, opens blogs and unfollows."
  (tumblr-test-logged-in
    (tumblr-test-with-backend
        `(("offset=2" 200 ,(tumblr-test-envelope '((total_blogs . 3) (blogs . [((name . "third") (title . "Third") (updated . 1757000000))]))))
          ("/user/following" 200 ,(tumblr-test-fixture "following.json"))
          ("/user/unfollow" 200 ,(tumblr-test-envelope '((ok . t)))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'message) #'ignore))
        (tumblr-lists-test-with-buffer "*tumblr: following*"
          (tumblr-following)
          (should (derived-mode-p 'tumblr-lists-mode))
          (should (equal (tumblr-lists-test-names) '("root-blog" "middle-blog")))
          (should (string-match-p "limit=20&offset=0" (nth 1 (tumblr-test-call 0))))
          (should (tumblr-lists-more-p))
          (tumblr-lists-load-more)
          (should (equal (tumblr-lists-test-names)
                         '("root-blog" "middle-blog" "third")))
          (should-not (tumblr-lists-more-p))
          (goto-char (point-min))
          (let (opened)
            (let ((tumblr-npf-open-blog-function (lambda (name) (setq opened name))))
              (tumblr-lists-open))
            (should (equal opened "root-blog")))
          (let (url)
            (let ((tumblr-npf-open-url-function (lambda (u) (setq url u))))
              (tumblr-lists-browse-url))
            (should (equal url "https://www.tumblr.com/root-blog")))
          (tumblr-lists-copy-url)
          (should (equal (current-kill 0) "https://www.tumblr.com/root-blog"))
          (goto-char (point-max))
          (should-error (tumblr-lists-browse-url) :type 'user-error)
          (goto-char (point-min))
          (tumblr-lists-unfollow)
          (should (string-suffix-p "/user/unfollow" (nth 1 (tumblr-test-call 0))))
          (should (equal (tumblr-test-call-key (tumblr-test-call 0) :body)
                         "url=https%3A%2F%2Froot-blog.tumblr.com%2F"))
          (should (equal (tumblr-lists-test-names) '("middle-blog" "third")))
          (should (equal tumblr-lists--total 2)))))))

(ert-deftest tumblr-lists-test-followers ()
  "The followers table lists the users of the chosen blog."
  (tumblr-test-logged-in
    (tumblr-test-with-backend
        `(("/blog/example/followers" 200 ,(tumblr-test-fixture "followers.json")))
      (cl-letf (((symbol-function 'tumblr-user-default-blog) (lambda () "example"))
                ((symbol-function 'message) #'ignore))
        (tumblr-lists-test-with-buffer "*tumblr: followers/example*"
          (tumblr-followers)
          (should (equal (tumblr-lists-test-names) '("liker" "newfan")))
          (should-not (tumblr-lists-more-p))
          (should (string-match-p "liker" (buffer-string))))))))

(ert-deftest tumblr-lists-test-failure ()
  "A failed page fetch clears the loading flag."
  (tumblr-test-logged-in
    (tumblr-test-with-backend '(("/user/following" curl nil))
      (cl-letf (((symbol-function 'message) #'ignore))
        (tumblr-lists-test-with-buffer "*tumblr: following*"
          (tumblr-following)
          (should-not tumblr-lists--loading)
          (should (null tabulated-list-entries)))))))

(provide 'tumblr-lists-test)
;;; tumblr-lists-test.el ends here
