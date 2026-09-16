;;; tumblr-feed-test.el --- Tests for tumblr-feed.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for feed buffers: paging, navigation, guards and the
;; single-post source, driven by in-memory sources and the fake
;; backend.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-feed)
(require 'tumblr-test-support)

(defvar tumblr-feed-test-fetches 0
  "Number of pages the test source was asked for.")

(defvar tumblr-feed-test-pending nil
  "Callbacks of a deferred test source, as (THEN . ELSE).")

(defun tumblr-feed-test-source (pages &optional deferred)
  "Return a source serving PAGES, a list of post lists.
Pages are delivered synchronously unless DEFERRED, in which case the
callbacks are stored in `tumblr-feed-test-pending'."
  (tumblr-feed-source-create
   :name "test"
   :title "Test feed"
   :fetch (lambda (cursor then else)
            (cl-incf tumblr-feed-test-fetches)
            (let* ((index (or cursor 0))
                   (page (nth index pages))
                   (next (and (< (1+ index) (length pages)) (1+ index))))
              (if deferred
                  (setq tumblr-feed-test-pending
                        (cons (lambda () (funcall then page next)) else))
                (funcall then page next))
              nil))))

(defmacro tumblr-feed-test-with-feed (source &rest body)
  "Display SOURCE in a fresh feed buffer and evaluate BODY there."
  (declare (indent 1))
  `(let ((tumblr-feed-test-fetches 0)
         (tumblr-feed-test-pending nil))
     (when (get-buffer "*tumblr: test*")
       (kill-buffer "*tumblr: test*"))
     (let ((buffer (tumblr-feed-display ,source)))
       (unwind-protect
           (with-current-buffer buffer
             ,@body)
         (kill-buffer buffer)))))

(defun tumblr-feed-test-ids ()
  "Return the ids of the posts in the current feed, in order."
  (mapcar (lambda (item) (tumblr-npf-post-id (tumblr-feed-item-post item)))
          (ewoc-collect tumblr-feed--ewoc #'identity)))

(defun tumblr-feed-test-id-at-point ()
  "Return the id of the post at point."
  (tumblr-npf-post-id (tumblr-feed-post-at-point)))

(defun tumblr-feed-test-pages ()
  "Return two pages of fixture posts, the second overlapping the first."
  (let ((posts (tumblr-test-posts "posts.json")))
    (list (seq-take posts 3)
          (append (list (nth 2 posts)) (seq-drop posts 3)))))

(ert-deftest tumblr-feed-test-display ()
  "The first page renders under the title with point on the first post."
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages))
    (should (derived-mode-p 'tumblr-feed-mode))
    (should (equal (buffer-name) "*tumblr: test*"))
    (should (string-prefix-p "Test feed\n\n" (buffer-string)))
    (should (equal (tumblr-feed-test-ids) '("1001" "1002" "1003")))
    (should (equal (tumblr-feed-test-id-at-point) "1001"))
    (should (string-match-p "\\[Load more\\]" (buffer-string)))
    (should buffer-read-only)
    (should (equal tumblr-feed-test-fetches 1))))

(defun tumblr-feed-test-rules ()
  "Return how many separator rules the current feed shows."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (text-property-search-forward 'face 'tumblr-separator t)
        (cl-incf count))
      count)))

(ert-deftest tumblr-feed-test-posts-end-with-a-rule ()
  "A full-width rule follows each post, but not items a source renders."
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages))
    (should (equal (tumblr-feed-test-rules) 3))
    (goto-char (ewoc-location (ewoc-nth tumblr-feed--ewoc 1)))
    (forward-line -2)
    (should (equal (get-text-property (point) 'display) '(space :width text)))
    (should (looking-at-p " \n\n")))
  (let ((source (tumblr-feed-test-source (tumblr-feed-test-pages))))
    (setf (tumblr-feed-source-render source)
          (lambda (post) (insert (tumblr-npf-post-id post) "\n")))
    (tumblr-feed-test-with-feed source
      (should (equal (tumblr-feed-test-rules) 0)))))

(ert-deftest tumblr-feed-test-navigation-and-paging ()
  "Moving past the last post loads the next page without duplicates."
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages))
    (tumblr-feed-next)
    (should (equal (tumblr-feed-test-id-at-point) "1002"))
    (tumblr-feed-next)
    (should (equal (tumblr-feed-test-id-at-point) "1003"))
    (tumblr-feed-previous)
    (should (equal (tumblr-feed-test-id-at-point) "1002"))
    (tumblr-feed-next 2)
    (should (equal tumblr-feed-test-fetches 2))
    (should (equal (tumblr-feed-test-ids)
                   '("1001" "1002" "1003" "1004" "1005" "1006")))
    (should (equal (tumblr-feed-test-id-at-point) "1003"))
    (should (string-match-p "End of feed\\." (buffer-string)))
    (tumblr-feed-next 10)
    (should (equal (tumblr-feed-test-id-at-point) "1006"))
    (should (equal tumblr-feed-test-fetches 2))
    (tumblr-feed-previous 10)
    (should (equal (point) (point-min)))
    (tumblr-feed-next)
    (should (equal (tumblr-feed-test-id-at-point) "1001"))))

(ert-deftest tumblr-feed-test-loading-guard ()
  "Only one page fetch is in flight at a time."
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages)
                                                       t)
    (should (equal tumblr-feed-test-fetches 1))
    (should tumblr-feed--loading)
    (should (string-match-p "Loading…" (buffer-string)))
    (tumblr-feed-load-more)
    (tumblr-feed-next)
    (should (equal tumblr-feed-test-fetches 1))
    (funcall (car tumblr-feed-test-pending))
    (should-not tumblr-feed--loading)
    (should (equal (tumblr-feed-test-ids) '("1001" "1002" "1003")))
    (should (equal (tumblr-feed-test-id-at-point) "1001"))))

(ert-deftest tumblr-feed-test-failure ()
  "A failed fetch is reported in the footer and can be retried."
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages)
                                                       t)
    (funcall (cdr tumblr-feed-test-pending) '(tumblr-http-error "boom"))
    (should-not tumblr-feed--loading)
    (should (string-match-p "Tumblr HTTP error: boom" (buffer-string)))
    (should (string-match-p "\\[Load more\\]" (buffer-string)))
    (tumblr-feed-load-more)
    (should (equal tumblr-feed-test-fetches 2))))

(ert-deftest tumblr-feed-test-revert-restores-point ()
  "Reverting reloads the first page and returns to the same post."
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages))
    (tumblr-feed-next)
    (revert-buffer)
    (should (equal tumblr-feed-test-fetches 2))
    (should (equal (tumblr-feed-test-ids) '("1001" "1002" "1003")))
    (should (equal (tumblr-feed-test-id-at-point) "1002"))))

(ert-deftest tumblr-feed-test-toggle ()
  "The keep-reading button re-renders its post in full."
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages))
    (should-not (string-match-p "Third\\." (buffer-string)))
    (goto-char (point-min))
    (search-forward "[Keep reading]")
    (push-button (match-beginning 0))
    (should (string-match-p "Third\\." (buffer-string)))
    (should-not (string-match-p "Keep reading" (buffer-string)))
    (should (equal (tumblr-feed-test-id-at-point) "1003"))
    (should (equal (tumblr-feed-test-ids) '("1001" "1002" "1003")))))

(ert-deftest tumblr-feed-test-commands-at-point ()
  "Blog, tag and URL commands act on the post at point."
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages))
    (let (blog tag url)
      (let ((tumblr-npf-open-blog-function (lambda (name) (setq blog name)))
            (tumblr-npf-open-tag-function (lambda (x) (setq tag x)))
            (tumblr-npf-open-url-function (lambda (u) (setq url u))))
        (tumblr-feed-open-blog)
        (should (equal blog "example"))
        (tumblr-feed-browse-tag "emacs")
        (should (equal tag "emacs"))
        (tumblr-feed-browse-url)
        (should (equal url "https://example.tumblr.com/post/1001"))
        (tumblr-feed-copy-url)
        (should (equal (current-kill 0) "https://example.tumblr.com/post/1001"))
        (goto-char (point-min))
        (search-forward "#emacs")
        (tumblr-feed-activate)
        (should (equal tag "emacs"))))))

(ert-deftest tumblr-feed-test-urls-follow-point ()
  "`o' and `y' take the tag, link or other blog at point, else the post."
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages))
    (let (url)
      (cl-flet ((at (text)
                  (goto-char (point-min))
                  (search-forward text)
                  (goto-char (match-beginning 0)))
                (opened ()
                  (let ((tumblr-npf-open-url-function (lambda (u) (setq url u))))
                    (tumblr-feed-browse-url)
                    url))
                (copied ()
                  (tumblr-feed-copy-url)
                  (current-kill 0)))
        (at "Heading One")
        (should (equal (opened) "https://example.tumblr.com/post/1001"))
        (at "example")
        (should (equal (opened) "https://example.tumblr.com/post/1001"))
        (at "@mention")
        (should (equal (opened) "https://www.tumblr.com/friend"))
        (should (equal (copied) "https://www.tumblr.com/friend"))
        (at "link, @")
        (should (equal (opened) "https://example.com/"))
        (at "#lisp code")
        (should (equal (opened) "https://www.tumblr.com/tagged/lisp%20code"))
        (should (equal (copied) "https://www.tumblr.com/tagged/lisp%20code"))
        (at "root-blog")
        (should (equal (opened) "https://www.tumblr.com/root-blog"))))))

(ert-deftest tumblr-feed-test-empty-feed ()
  "An empty first page says so."
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (list nil))
    (should (null (tumblr-feed-test-ids)))
    (should tumblr-feed--exhausted)
    (should (string-match-p "No posts\\." (buffer-string)))
    (should-error (tumblr-feed-post-at-point) :type 'user-error)))

(ert-deftest tumblr-feed-test-post-source ()
  "A single post shows expanded at once and is fetched again on revert."
  (let ((post (tumblr-test-post "1003")))
    (when (get-buffer "*tumblr: post/1003*")
      (kill-buffer "*tumblr: post/1003*"))
    (tumblr-test-logged-out
      (tumblr-test-with-backend
          `(("/blog/example/posts/1003" 200
             ,(tumblr-test-envelope (tumblr-test-post-fidelity "1001"))))
        (let ((buffer (tumblr-feed-display
                       (tumblr-feed-post-source "example" "1003" post))))
          (unwind-protect
              (with-current-buffer buffer
                (should (equal (buffer-name) "*tumblr: post/1003*"))
                (should (null tumblr-test-calls))
                (should (string-match-p "Third\\." (buffer-string)))
                (should tumblr-feed--exhausted)
                (revert-buffer)
                (should (equal (length tumblr-test-calls) 1))
                (should (equal (tumblr-feed-test-ids) '("1001")))
                (should (string-match-p "Heading One" (buffer-string))))
            (kill-buffer buffer)))))))

(ert-deftest tumblr-feed-test-open-post-from-feed ()
  "RET away from a button opens the post at point in its own buffer."
  (when (get-buffer "*tumblr: post/1001*")
    (kill-buffer "*tumblr: post/1001*"))
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages))
    (forward-line 1)
    (should-not (button-at (point)))
    (tumblr-feed-activate)
    (unwind-protect
        (with-current-buffer "*tumblr: post/1001*"
          (should (equal (tumblr-feed-test-ids) '("1001")))
          (should (string-match-p "Heading One" (buffer-string))))
      (kill-buffer "*tumblr: post/1001*"))))

(ert-deftest tumblr-feed-test-toggle-images ()
  "Toggling images re-renders the feed with the buffer-local setting."
  (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages))
    (let ((tumblr-display-images t))
      (cl-letf (((symbol-function 'message) #'ignore))
        (tumblr-feed-toggle-images)
        (should-not tumblr-display-images)
        (should (local-variable-p 'tumblr-display-images))
        (should (equal (tumblr-feed-test-ids) '("1001" "1002" "1003")))
        (tumblr-feed-toggle-images)
        (should tumblr-display-images)))))
;;;; Actions

(defmacro tumblr-feed-test-with-actions (responses &rest body)
  "Display the fixture pages logged in, with RESPONSES served, then run BODY."
  (declare (indent 1))
  `(tumblr-test-logged-in
     (tumblr-test-with-backend ,responses
       (cl-letf (((symbol-function 'tumblr-user-default-blog)
                  (lambda () "example"))
                 ((symbol-function 'tumblr-user-own-blog-p)
                  (lambda (name) (equal name "example")))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'message) #'ignore))
         (tumblr-feed-test-with-feed (tumblr-feed-test-source
                                      (tumblr-feed-test-pages))
           ,@body)))))

(defconst tumblr-feed-test-ok "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\"response\":{}}"
  "An empty successful envelope.")

(ert-deftest tumblr-feed-test-like-and-unlike ()
  "Liking sends the id and reblog key and updates the post in place."
  (tumblr-feed-test-with-actions `(("/user/" 200 ,tumblr-feed-test-ok))
    (tumblr-feed-like)
    (let ((call (tumblr-test-call 0))
          (post (tumblr-feed-post-at-point)))
      (should (string-suffix-p "/user/like" (nth 1 call)))
      (should (equal (tumblr-test-call-key call :body) "id=1001&reblog_key=rk1001"))
      (should (equal (tumblr-test-call-header call "Content-Type")
                     "application/x-www-form-urlencoded"))
      (should (eq (alist-get 'liked post) t))
      (should (equal (alist-get 'note_count post) 4))
      (should (string-match-p "4 notes · liked" (buffer-string)))
      (should (equal (tumblr-feed-test-id-at-point) "1001")))
    (tumblr-feed-like)
    (should (string-suffix-p "/user/unlike" (nth 1 (tumblr-test-call 0))))
    (should-not (alist-get 'liked (tumblr-feed-post-at-point)))
    (should (string-match-p "3 notes\n" (buffer-string)))))

(ert-deftest tumblr-feed-test-like-refused ()
  "A post that cannot be liked is refused before any request."
  (let ((post (copy-alist (tumblr-test-post "1001"))))
    (setf (alist-get 'can_like post) nil)
    (tumblr-test-logged-in
      (tumblr-test-with-backend nil
        (tumblr-feed-test-with-feed (tumblr-feed-test-source (list (list post)))
          (should-error (tumblr-feed-like) :type 'user-error)
          (should (null tumblr-test-calls)))))))

(ert-deftest tumblr-feed-test-pending-guard ()
  "A second action on a post waits for the first to finish."
  (tumblr-feed-test-with-actions `(("/user/" 200 ,tumblr-feed-test-ok))
    (let ((tumblr-test-defer t))
      (tumblr-feed-like)
      (should-error (tumblr-feed-like) :type 'user-error)
      (should (equal (length tumblr-test-calls) 1))
      (tumblr-test-deliver)
      (should (eq (alist-get 'liked (tumblr-feed-post-at-point)) t))
      (tumblr-feed-like)
      (should (equal (length tumblr-test-calls) 2)))))

(ert-deftest tumblr-feed-test-action-failure ()
  "A failed action leaves the post untouched and ready for another try."
  (tumblr-feed-test-with-actions `(("/user/" 429
                                    ,(tumblr-test-fixture "error-429.json")))
    (tumblr-feed-like)
    (should-not (alist-get 'liked (tumblr-feed-post-at-point)))
    (should-not (tumblr-feed-item-pending (tumblr-feed--item-at-point)))
    (tumblr-feed-like)
    (should (equal (length tumblr-test-calls) 2))))

(ert-deftest tumblr-feed-test-reblog ()
  "A quick reblog posts the parent references and tags as NPF."
  (tumblr-feed-test-with-actions
      '(("/blog/example/posts" 201
         "{\"meta\":{\"status\":201,\"msg\":\"Created\"},\"response\":{\"id\":\"999\"}}"))
    (tumblr-feed-reblog '("emacs" "lisp"))
    (let ((call (tumblr-test-call 0)))
      (should (eq (car call) 'post))
      (should (string-suffix-p "/blog/example/posts" (nth 1 call)))
      (should (equal (tumblr-test-call-key call :body)
                     (concat "{\"content\":[],"
                             "\"parent_tumblelog_uuid\":\"t:AbCdEfGhIjKlMnOpQrStUv\","
                             "\"parent_post_id\":\"1001\",\"reblog_key\":\"rk1001\","
                             "\"tags\":\"emacs,lisp\"}")))
      (should (equal (tumblr-test-call-header call "Content-Type")
                     "application/json")))
    (tumblr-feed-next 5)
    (should (equal (tumblr-feed-test-id-at-point) "1005"))
    (should-error (tumblr-feed-reblog) :type 'user-error)))

(ert-deftest tumblr-feed-test-reblog-fetches-missing-uuid ()
  "A post without its blog UUID gets it from the blog info, once."
  (let ((post (copy-alist (tumblr-test-post "1001"))))
    (setf (alist-get 'blog post) nil)
    (let ((tumblr-api--blog-uuids (make-hash-table :test #'equal)))
      (tumblr-test-logged-in
        (tumblr-test-with-backend
            `(("/blog/example/info" 200 ,(tumblr-test-fixture "blog-info.json"))
              ("/blog/example/posts" 201
               "{\"meta\":{\"status\":201,\"msg\":\"Created\"},\"response\":{\"id\":\"1\"}}"))
          (cl-letf (((symbol-function 'tumblr-user-default-blog)
                     (lambda () "example"))
                    ((symbol-function 'message) #'ignore))
            (tumblr-feed-test-with-feed (tumblr-feed-test-source
                                         (list (list post)))
              (tumblr-feed-reblog)
              (tumblr-feed-reblog)
              (should (equal (length tumblr-test-calls) 3))
              (should (string-match-p "\"parent_tumblelog_uuid\":\"t:AbCdEfGhIjKlMnOpQrStUv\""
                                      (tumblr-test-call-key (tumblr-test-call 0)
                                                            :body))))))))))

(ert-deftest tumblr-feed-test-follow-and-unfollow ()
  "Following sends the blog URL and flips the state on the post."
  (tumblr-feed-test-with-actions `(("/user/" 200 ,tumblr-feed-test-ok))
    (tumblr-feed-follow)
    (let ((call (tumblr-test-call 0)))
      (should (string-suffix-p "/user/follow" (nth 1 call)))
      (should (equal (tumblr-test-call-key call :body)
                     "url=https%3A%2F%2Fexample.tumblr.com%2F")))
    (should (eq (alist-get 'followed (tumblr-feed-post-at-point)) t))
    (tumblr-feed-follow)
    (should (string-suffix-p "/user/unfollow" (nth 1 (tumblr-test-call 0))))
    (should-not (alist-get 'followed (tumblr-feed-post-at-point)))))

(ert-deftest tumblr-feed-test-delete ()
  "Deleting an own post removes it from the feed."
  (tumblr-feed-test-with-actions `(("/post/delete" 200 ,tumblr-feed-test-ok))
    (tumblr-feed-delete)
    (let ((call (tumblr-test-call 0)))
      (should (string-suffix-p "/blog/example/post/delete" (nth 1 call)))
      (should (equal (tumblr-test-call-key call :body) "id=1001")))
    (should (equal (tumblr-feed-test-ids) '("1002" "1003")))
    (should (equal (tumblr-feed-test-id-at-point) "1002"))))

(ert-deftest tumblr-feed-test-delete-refused ()
  "Posts of other blogs cannot be deleted."
  (tumblr-test-logged-in
    (tumblr-test-with-backend nil
      (cl-letf (((symbol-function 'tumblr-user-own-blog-p) (lambda (_) nil)))
        (tumblr-feed-test-with-feed (tumblr-feed-test-source
                                     (tumblr-feed-test-pages))
          (should-error (tumblr-feed-delete) :type 'user-error)
          (should (null tumblr-test-calls)))))))
(ert-deftest tumblr-feed-test-compose-entry-points ()
  "The compose keys open the matching compose buffers."
  (let (reblogged edited)
    (cl-letf (((symbol-function 'tumblr-compose-reblog)
               (lambda (post) (setq reblogged (tumblr-npf-post-id post))))
              ((symbol-function 'tumblr-compose-edit)
               (lambda (post) (setq edited (tumblr-npf-post-id post)))))
      (tumblr-feed-test-with-feed (tumblr-feed-test-source
                                   (tumblr-feed-test-pages))
        (tumblr-feed-reblog-with-comment)
        (should (equal reblogged "1001"))
        (tumblr-feed-edit)
        (should (equal edited "1001"))
        (tumblr-feed-next 5)
        (should-error (tumblr-feed-reblog-with-comment) :type 'user-error)))
    (should (eq (lookup-key tumblr-feed-mode-map (kbd "c")) 'tumblr-compose))))
;;;; Filters

(ert-deftest tumblr-feed-test-hidden-reason ()
  "Posts are hidden by filtered tags or filtered content, case-insensitively."
  (let ((post (tumblr-test-post "1003")))
    (should (null (tumblr-feed-hidden-reason post '(nil . nil))))
    (should (equal (tumblr-feed-hidden-reason (tumblr-test-post "1001")
                                              '(("Lisp Code") . nil))
                   "#Lisp Code"))
    (should (equal (tumblr-feed-hidden-reason post '(nil . ("middle COMMENT")))
                   "\"middle COMMENT\""))
    (should (null (tumblr-feed-hidden-reason post '(("cats") . ("nope")))))))

(ert-deftest tumblr-feed-test-filtered-posts-collapse ()
  "Filtered posts show as one line until revealed."
  (let ((tumblr-user--filters '(("cats") . ("Root post text"))))
    (tumblr-feed-test-with-feed (tumblr-feed-test-source (tumblr-feed-test-pages))
      (should (string-match-p "Heading One" (buffer-string)))
      (should-not (string-match-p "Caption text" (buffer-string)))
      (should (string-match-p "example · hidden, filtered by #cats \\[Show\\]"
                              (buffer-string)))
      (should (string-match-p "hidden, filtered by \"Root post text\""
                              (buffer-string)))
      (goto-char (point-min))
      (search-forward "[Show]")
      (push-button (match-beginning 0))
      (should (string-match-p "Caption text" (buffer-string)))
      (should (equal (tumblr-feed-test-ids) '("1001" "1002" "1003"))))))

(ert-deftest tumblr-feed-test-filters-loaded-on-display ()
  "Feeds of posts fetch the filters once when logged in and re-render."
  (tumblr-test-logged-in
    (let ((tumblr-user--filters nil)
          (tumblr-user--filters-loading nil))
      (tumblr-test-with-backend
          `(("/user/filtered_tags" 200
             ,(tumblr-test-envelope '((filtered_tags . ["cats"]))))
            ("/user/filtered_content" 200
             ,(tumblr-test-envelope '((filtered_content . [])))))
        (let ((tumblr-test-defer t))
          (tumblr-feed-test-with-feed (tumblr-feed-test-source
                                       (tumblr-feed-test-pages))
            (should (string-match-p "Caption text" (buffer-string)))
            (tumblr-test-deliver)
            (should (equal (tumblr-user-filters) '(("cats") . nil)))
            (should-not (string-match-p "Caption text" (buffer-string)))
            (should (equal (length tumblr-test-calls) 2))))
        (tumblr-feed-test-with-feed (tumblr-feed-test-source
                                     (tumblr-feed-test-pages))
          (should (equal (length tumblr-test-calls) 2)))))))

(ert-deftest tumblr-feed-test-filters-failure-does-not-break-feeds ()
  "A failed filter fetch is reported and the feed still shows."
  (tumblr-test-logged-in
    (let ((tumblr-user--filters nil)
          (tumblr-user--filters-loading nil))
      (tumblr-test-with-backend '(("/user/filtered_tags" curl nil))
        (cl-letf (((symbol-function 'message) #'ignore))
          (tumblr-feed-test-with-feed (tumblr-feed-test-source
                                       (tumblr-feed-test-pages))
            (should (equal (tumblr-feed-test-ids) '("1001" "1002" "1003")))
            (should-not tumblr-user--filters-loading)))))))
(provide 'tumblr-feed-test)
;;; tumblr-feed-test.el ends here
