;;; tumbel-blog-test.el --- Tests for tumbel-blog.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the blog and tag views against the fake backend.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-blog)
(require 'tumbel-test-support)

(defmacro tumbel-blog-test-with-buffer (name &rest body)
  "Kill the buffer NAME, evaluate BODY, then kill it again."
  (declare (indent 1))
  `(progn
     (when (get-buffer ,name)
       (kill-buffer ,name))
     (unwind-protect
         (progn ,@body)
       (when (get-buffer ,name)
         (kill-buffer ,name)))))

(defun tumbel-blog-test-ids ()
  "Return the ids of the posts in the current feed."
  (mapcar (lambda (item) (tumbel-npf-post-id (tumbel-feed-item-post item)))
          (ewoc-collect tumbel-feed--ewoc #'identity)))

(ert-deftest tumbel-blog-test-html-to-string ()
  "HTML descriptions become plain text with or without libxml."
  (should (equal (tumbel-blog-html-to-string "The <b>example</b> blog.")
                 "The example blog."))
  (should (equal (tumbel-blog-html-to-string "") ""))
  (should (equal (tumbel-blog-html-to-string nil) ""))
  (cl-letf (((symbol-function 'libxml-available-p) (lambda () nil)))
    (should (equal (tumbel-blog-html-to-string "a <i>b</i> c") "a b c"))))

(ert-deftest tumbel-blog-test-header-string ()
  "The header shows title, URL, counts and the follow state when logged in."
  (let ((info '((title . "Example Blog") (name . "example")
                (url . "https://example.tumblr.com/")
                (total_posts . 7) (followed . t)
                (description . "Hello <b>there</b>"))))
    (tumbel-test-logged-out
      (let ((header (tumbel-blog-header-string info)))
        (should (string-prefix-p "Example Blog\nhttps://example.tumblr.com/ · 7 posts\nHello there\n\n"
                                 header))
        (should-not (string-match-p "following" header))))
    (tumbel-test-logged-in
      (should (string-match-p "7 posts · following\n"
                              (tumbel-blog-header-string info))))))

(ert-deftest tumbel-blog-test-blog-view ()
  "A blog view fetches the header and pages its posts by offset."
  (tumbel-test-logged-out
    (tumbel-test-with-backend
        `(("offset=6" 200 ,(tumbel-test-fixture "posts-page2.json"))
          ("/blog/example/posts" 200 ,(tumbel-test-fixture "posts.json"))
          ("/blog/example/info" 200 ,(tumbel-test-fixture "blog-info.json")))
      (tumbel-blog-test-with-buffer "*tumbel: blog/example*"
        (tumbel-blog "example")
        (should (equal (buffer-name) "*tumbel: blog/example*"))
        (should (string-prefix-p "[image: example] Example Blog\n"
                                 (buffer-string)))
        (should (string-match-p "1234 posts" (buffer-string)))
        (should (equal (tumbel-blog-test-ids)
                       '("1001" "1002" "1003" "1004" "1005" "1006")))
        (should (equal (length tumbel-test-calls) 2))
        (should (string-match-p "limit=20&offset=0&reblog_info=true"
                                (nth 1 (tumbel-test-call 1))))
        (tumbel-feed-load-more)
        (should (equal (tumbel-blog-test-ids)
                       '("1001" "1002" "1003" "1004" "1005" "1006" "1007")))
        (should tumbel-feed--exhausted)
        (should (string-match-p "End of feed\\." (buffer-string)))))))

(ert-deftest tumbel-blog-test-tag-view ()
  "A tag view pages by the timestamp of the last post."
  (tumbel-test-logged-out
    (tumbel-test-with-backend
        `(("before=1757159999" 200 ,(tumbel-test-envelope '((posts . []))))
          ("/tagged" 200 ,(tumbel-test-fixture "posts.json")))
      (tumbel-blog-test-with-buffer "*tumbel: tag/cats*"
        (tumbel-tag "#cats")
        (should (equal (buffer-name) "*tumbel: tag/cats*"))
        (should (string-prefix-p "#cats\n" (buffer-string)))
        (should (equal (length (tumbel-blog-test-ids)) 6))
        (should (string-match-p "/tagged\\?tag=cats&npf=true&limit=20&api_key=KEY"
                                (nth 1 (tumbel-test-call 0))))
        (tumbel-feed-load-more)
        (should (string-match-p "before=1757159999" (nth 1 (tumbel-test-call 0))))
        (should tumbel-feed--exhausted)))))

(ert-deftest tumbel-blog-test-header-avatar ()
  "The header starts with the 64 pixel avatar placeholder."
  (let ((info '((title . "T") (name . "example")
                (avatar . (((width . 128) (url . "https://a/128.png"))
                           ((width . 64) (url . "https://a/64.png")))))))
    (tumbel-test-logged-out
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
        (let ((header (tumbel-blog-header-string info)))
          (should (string-prefix-p "[image: example] T\n" header))
          (should (equal (get-text-property 0 'tumbel-media-url header)
                         "https://a/64.png")))))))
(ert-deftest tumbel-blog-test-likes-view ()
  "Likes page by like timestamp when posts carry it."
  (tumbel-test-logged-in
    (tumbel-test-with-backend
        `(("before=1757400000" 200 ,(tumbel-test-envelope '((liked_posts . []) (liked_count . 3))))
          ("/user/likes" 200 ,(tumbel-test-fixture "likes.json")))
      (tumbel-blog-test-with-buffer "*tumbel: likes*"
        (tumbel-likes)
        (should (equal (buffer-name) "*tumbel: likes*"))
        (should (equal (tumbel-blog-test-ids) '("900" "901")))
        (should (string-match-p "/user/likes\\?npf=true&limit=20$"
                                (nth 1 (tumbel-test-call 0))))
        (should (string-match-p "liked" (buffer-string)))
        (tumbel-feed-load-more)
        (should (string-match-p "before=1757400000" (nth 1 (tumbel-test-call 0))))
        (should tumbel-feed--exhausted)))))

(ert-deftest tumbel-blog-test-likes-offset-fallback ()
  "Without like timestamps the likes page by offset up to the cap."
  (let ((post (copy-alist (tumbel-test-post "1001"))))
    (tumbel-test-logged-in
      (tumbel-test-with-backend
          `(("/user/likes" 200 ,(tumbel-test-envelope
                                 `((liked_posts . ,(vector (tumbel-test-post-fidelity "1001")))
                                   (liked_count . 2000)))))
        (tumbel-blog-test-with-buffer "*tumbel: likes*"
          (tumbel-likes)
          (should (equal (tumbel-blog-test-ids) '("1001")))
          (should (equal tumbel-feed--cursor '(offset . 1)))
          (let ((tumbel-likes-offset-limit 1))
            (tumbel-feed-load-more)
            (should (string-match-p "offset=1" (nth 1 (tumbel-test-call 0))))
            (should tumbel-feed--exhausted))))
      (ignore post))))

(ert-deftest tumbel-blog-test-tag-filter ()
  "A blog view can be narrowed to one tag."
  (tumbel-test-logged-out
    (tumbel-test-with-backend
        `(("/blog/example/posts" 200 ,(tumbel-test-fixture "posts.json"))
          ("/blog/example/info" 200 ,(tumbel-test-fixture "blog-info.json")))
      (tumbel-blog-test-with-buffer "*tumbel: blog/example/tag/cats*"
        (tumbel-blog "example" "cats")
        (should (equal (buffer-name) "*tumbel: blog/example/tag/cats*"))
        (should (string-match-p "offset=0&tag=cats&reblog_info=true"
                                (nth 1 (tumbel-test-call 1))))
        (should (string-match-p "Example Blog · #cats\n" (buffer-string)))))))
(provide 'tumbel-blog-test)
;;; tumbel-blog-test.el ends here
