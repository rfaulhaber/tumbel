;;; tumbel-lists-test.el --- Tests for tumbel-lists.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the following and followers tables.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-lists)
(require 'tumbel-test-support)

(defmacro tumbel-lists-test-with-buffer (name &rest body)
  "Kill the buffer NAME before and after evaluating BODY."
  (declare (indent 1))
  `(progn
     (when (get-buffer ,name)
       (kill-buffer ,name))
     (unwind-protect
         (progn ,@body)
       (when (get-buffer ,name)
         (kill-buffer ,name)))))

(defun tumbel-lists-test-names ()
  "Return the blog names listed in the current buffer."
  (mapcar #'car tabulated-list-entries))

(ert-deftest tumbel-lists-test-following ()
  "The following table pages by offset, opens blogs and unfollows."
  (tumbel-test-logged-in
    (tumbel-test-with-backend
        `(("offset=2" 200 ,(tumbel-test-envelope '((total_blogs . 3) (blogs . [((name . "third") (title . "Third") (updated . 1757000000))]))))
          ("/user/following" 200 ,(tumbel-test-fixture "following.json"))
          ("/user/unfollow" 200 ,(tumbel-test-envelope '((ok . t)))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'message) #'ignore))
        (tumbel-lists-test-with-buffer "*tumbel: following*"
          (tumbel-following)
          (should (derived-mode-p 'tumbel-lists-mode))
          (should (equal (tumbel-lists-test-names) '("root-blog" "middle-blog")))
          (should (string-match-p "limit=20&offset=0" (nth 1 (tumbel-test-call 0))))
          (should (tumbel-lists-more-p))
          (tumbel-lists-load-more)
          (should (equal (tumbel-lists-test-names)
                         '("root-blog" "middle-blog" "third")))
          (should-not (tumbel-lists-more-p))
          (goto-char (point-min))
          (let (opened)
            (let ((tumbel-npf-open-blog-function (lambda (name) (setq opened name))))
              (tumbel-lists-open))
            (should (equal opened "root-blog")))
          (let (url)
            (let ((tumbel-npf-open-url-function (lambda (u) (setq url u))))
              (tumbel-lists-browse-url))
            (should (equal url "https://www.tumblr.com/root-blog")))
          (tumbel-lists-copy-url)
          (should (equal (current-kill 0) "https://www.tumblr.com/root-blog"))
          (goto-char (point-max))
          (should-error (tumbel-lists-browse-url) :type 'user-error)
          (goto-char (point-min))
          (tumbel-lists-unfollow)
          (should (string-suffix-p "/user/unfollow" (nth 1 (tumbel-test-call 0))))
          (should (equal (tumbel-test-call-key (tumbel-test-call 0) :body)
                         "url=https%3A%2F%2Froot-blog.tumblr.com%2F"))
          (should (equal (tumbel-lists-test-names) '("middle-blog" "third")))
          (should (equal tumbel-lists--total 2)))))))

(ert-deftest tumbel-lists-test-followers ()
  "The followers table lists the users of the chosen blog."
  (tumbel-test-logged-in
    (tumbel-test-with-backend
        `(("/blog/example/followers" 200 ,(tumbel-test-fixture "followers.json")))
      (cl-letf (((symbol-function 'tumbel-user-default-blog) (lambda () "example"))
                ((symbol-function 'message) #'ignore))
        (tumbel-lists-test-with-buffer "*tumbel: followers/example*"
          (tumbel-followers)
          (should (equal (tumbel-lists-test-names) '("liker" "newfan")))
          (should-not (tumbel-lists-more-p))
          (should (string-match-p "liker" (buffer-string))))))))

(ert-deftest tumbel-lists-test-failure ()
  "A failed page fetch clears the loading flag."
  (tumbel-test-logged-in
    (tumbel-test-with-backend '(("/user/following" curl nil))
      (cl-letf (((symbol-function 'message) #'ignore))
        (tumbel-lists-test-with-buffer "*tumbel: following*"
          (tumbel-following)
          (should-not tumbel-lists--loading)
          (should (null tabulated-list-entries)))))))

(provide 'tumbel-lists-test)
;;; tumbel-lists-test.el ends here
