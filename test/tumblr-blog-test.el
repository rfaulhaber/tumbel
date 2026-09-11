;;; tumblr-blog-test.el --- Tests for tumblr-blog.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the blog and tag views against the fake backend.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-blog)
(require 'tumblr-test-support)

(defmacro tumblr-blog-test-with-buffer (name &rest body)
  "Kill the buffer NAME, evaluate BODY, then kill it again."
  (declare (indent 1))
  `(progn
     (when (get-buffer ,name)
       (kill-buffer ,name))
     (unwind-protect
         (progn ,@body)
       (when (get-buffer ,name)
         (kill-buffer ,name)))))

(defun tumblr-blog-test-ids ()
  "Return the ids of the posts in the current feed."
  (mapcar (lambda (item) (tumblr-npf-post-id (tumblr-feed-item-post item)))
          (ewoc-collect tumblr-feed--ewoc #'identity)))

(ert-deftest tumblr-blog-test-html-to-string ()
  "HTML descriptions become plain text with or without libxml."
  (should (equal (tumblr-blog-html-to-string "The <b>example</b> blog.")
                 "The example blog."))
  (should (equal (tumblr-blog-html-to-string "") ""))
  (should (equal (tumblr-blog-html-to-string nil) ""))
  (cl-letf (((symbol-function 'libxml-available-p) (lambda () nil)))
    (should (equal (tumblr-blog-html-to-string "a <i>b</i> c") "a b c"))))

(ert-deftest tumblr-blog-test-header-string ()
  "The header shows title, URL, counts and the follow state when logged in."
  (let ((info '((title . "Example Blog") (name . "example")
                (url . "https://example.tumblr.com/")
                (total_posts . 7) (followed . t)
                (description . "Hello <b>there</b>"))))
    (tumblr-test-logged-out
      (let ((header (tumblr-blog-header-string info)))
        (should (string-prefix-p "Example Blog\nhttps://example.tumblr.com/ · 7 posts\nHello there\n\n"
                                 header))
        (should-not (string-match-p "following" header))))
    (tumblr-test-logged-in
      (should (string-match-p "7 posts · following\n"
                              (tumblr-blog-header-string info))))))

(ert-deftest tumblr-blog-test-blog-view ()
  "A blog view fetches the header and pages its posts by offset."
  (tumblr-test-logged-out
    (tumblr-test-with-backend
        `(("offset=6" 200 ,(tumblr-test-fixture "posts-page2.json"))
          ("/blog/example/posts" 200 ,(tumblr-test-fixture "posts.json"))
          ("/blog/example/info" 200 ,(tumblr-test-fixture "blog-info.json")))
      (tumblr-blog-test-with-buffer "*tumblr: blog/example*"
        (tumblr-blog "example")
        (should (equal (buffer-name) "*tumblr: blog/example*"))
        (should (string-prefix-p "[image: example] Example Blog\n"
                                 (buffer-string)))
        (should (string-match-p "1234 posts" (buffer-string)))
        (should (equal (tumblr-blog-test-ids)
                       '("1001" "1002" "1003" "1004" "1005" "1006")))
        (should (equal (length tumblr-test-calls) 2))
        (should (string-match-p "limit=20&offset=0&reblog_info=true"
                                (nth 1 (tumblr-test-call 1))))
        (tumblr-feed-load-more)
        (should (equal (tumblr-blog-test-ids)
                       '("1001" "1002" "1003" "1004" "1005" "1006" "1007")))
        (should tumblr-feed--exhausted)
        (should (string-match-p "End of feed\\." (buffer-string)))))))

(ert-deftest tumblr-blog-test-tag-view ()
  "A tag view pages by the timestamp of the last post."
  (tumblr-test-logged-out
    (tumblr-test-with-backend
        `(("before=1757159999" 200 ,(tumblr-test-envelope '((posts . []))))
          ("/tagged" 200 ,(tumblr-test-fixture "posts.json")))
      (tumblr-blog-test-with-buffer "*tumblr: tag/cats*"
        (tumblr-tag "#cats")
        (should (equal (buffer-name) "*tumblr: tag/cats*"))
        (should (string-prefix-p "#cats\n" (buffer-string)))
        (should (equal (length (tumblr-blog-test-ids)) 6))
        (should (string-match-p "/tagged\\?tag=cats&npf=true&limit=20&api_key=KEY"
                                (nth 1 (tumblr-test-call 0))))
        (tumblr-feed-load-more)
        (should (string-match-p "before=1757159999" (nth 1 (tumblr-test-call 0))))
        (should tumblr-feed--exhausted)))))

(ert-deftest tumblr-blog-test-header-avatar ()
  "The header starts with the 64 pixel avatar placeholder."
  (let ((info '((title . "T") (name . "example")
                (avatar . (((width . 128) (url . "https://a/128.png"))
                           ((width . 64) (url . "https://a/64.png")))))))
    (tumblr-test-logged-out
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
        (let ((header (tumblr-blog-header-string info)))
          (should (string-prefix-p "[image: example] T\n" header))
          (should (equal (get-text-property 0 'tumblr-media-url header)
                         "https://a/64.png")))))))
(ert-deftest tumblr-blog-test-likes-view ()
  "Likes page by like timestamp when posts carry it."
  (tumblr-test-logged-in
    (tumblr-test-with-backend
        `(("before=1757400000" 200 ,(tumblr-test-envelope '((liked_posts . []) (liked_count . 3))))
          ("/user/likes" 200 ,(tumblr-test-fixture "likes.json")))
      (tumblr-blog-test-with-buffer "*tumblr: likes*"
        (tumblr-likes)
        (should (equal (buffer-name) "*tumblr: likes*"))
        (should (equal (tumblr-blog-test-ids) '("900" "901")))
        (should (string-match-p "/user/likes\\?npf=true&limit=20$"
                                (nth 1 (tumblr-test-call 0))))
        (should (string-match-p "liked" (buffer-string)))
        (tumblr-feed-load-more)
        (should (string-match-p "before=1757400000" (nth 1 (tumblr-test-call 0))))
        (should tumblr-feed--exhausted)))))

(ert-deftest tumblr-blog-test-likes-offset-fallback ()
  "Without like timestamps the likes page by offset up to the cap."
  (let ((post (copy-alist (tumblr-test-post "1001"))))
    (tumblr-test-logged-in
      (tumblr-test-with-backend
          `(("/user/likes" 200 ,(tumblr-test-envelope
                                 `((liked_posts . ,(vector (tumblr-test-post-fidelity "1001")))
                                   (liked_count . 2000)))))
        (tumblr-blog-test-with-buffer "*tumblr: likes*"
          (tumblr-likes)
          (should (equal (tumblr-blog-test-ids) '("1001")))
          (should (equal tumblr-feed--cursor '(offset . 1)))
          (let ((tumblr-likes-offset-limit 1))
            (tumblr-feed-load-more)
            (should (string-match-p "offset=1" (nth 1 (tumblr-test-call 0))))
            (should tumblr-feed--exhausted))))
      (ignore post))))

(ert-deftest tumblr-blog-test-tag-filter ()
  "A blog view can be narrowed to one tag."
  (tumblr-test-logged-out
    (tumblr-test-with-backend
        `(("/blog/example/posts" 200 ,(tumblr-test-fixture "posts.json"))
          ("/blog/example/info" 200 ,(tumblr-test-fixture "blog-info.json")))
      (tumblr-blog-test-with-buffer "*tumblr: blog/example/tag/cats*"
        (tumblr-blog "example" "cats")
        (should (equal (buffer-name) "*tumblr: blog/example/tag/cats*"))
        (should (string-match-p "offset=0&tag=cats&reblog_info=true"
                                (nth 1 (tumblr-test-call 1))))
        (should (string-match-p "Example Blog · #cats\n" (buffer-string)))))))
(provide 'tumblr-blog-test)
;;; tumblr-blog-test.el ends here
