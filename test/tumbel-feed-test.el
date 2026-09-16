;;; tumbel-feed-test.el --- Tests for tumbel-feed.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for feed buffers: paging, navigation, guards and the
;; single-post source, driven by in-memory sources and the fake
;; backend.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-feed)
(require 'tumbel-test-support)

(defvar tumbel-feed-test-fetches 0
  "Number of pages the test source was asked for.")

(defvar tumbel-feed-test-pending nil
  "Callbacks of a deferred test source, as (THEN . ELSE).")

(defun tumbel-feed-test-source (pages &optional deferred)
  "Return a source serving PAGES, a list of post lists.
Pages are delivered synchronously unless DEFERRED, in which case the
callbacks are stored in `tumbel-feed-test-pending'."
  (tumbel-feed-source-create
   :name "test"
   :title "Test feed"
   :fetch (lambda (cursor then else)
            (cl-incf tumbel-feed-test-fetches)
            (let* ((index (or cursor 0))
                   (page (nth index pages))
                   (next (and (< (1+ index) (length pages)) (1+ index))))
              (if deferred
                  (setq tumbel-feed-test-pending
                        (cons (lambda () (funcall then page next)) else))
                (funcall then page next))
              nil))))

(defmacro tumbel-feed-test-with-feed (source &rest body)
  "Display SOURCE in a fresh feed buffer and evaluate BODY there."
  (declare (indent 1))
  `(let ((tumbel-feed-test-fetches 0)
         (tumbel-feed-test-pending nil))
     (when (get-buffer "*tumbel: test*")
       (kill-buffer "*tumbel: test*"))
     (let ((buffer (tumbel-feed-display ,source)))
       (unwind-protect
           (with-current-buffer buffer
             ,@body)
         (kill-buffer buffer)))))

(defun tumbel-feed-test-ids ()
  "Return the ids of the posts in the current feed, in order."
  (mapcar (lambda (item) (tumbel-npf-post-id (tumbel-feed-item-post item)))
          (ewoc-collect tumbel-feed--ewoc #'identity)))

(defun tumbel-feed-test-id-at-point ()
  "Return the id of the post at point."
  (tumbel-npf-post-id (tumbel-feed-post-at-point)))

(defun tumbel-feed-test-pages ()
  "Return two pages of fixture posts, the second overlapping the first."
  (let ((posts (tumbel-test-posts "posts.json")))
    (list (seq-take posts 3)
          (append (list (nth 2 posts)) (seq-drop posts 3)))))

(ert-deftest tumbel-feed-test-display ()
  "The first page renders under the title with point on the first post."
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages))
    (should (derived-mode-p 'tumbel-feed-mode))
    (should (equal (buffer-name) "*tumbel: test*"))
    (should (string-prefix-p "Test feed\n\n" (buffer-string)))
    (should (equal (tumbel-feed-test-ids) '("1001" "1002" "1003")))
    (should (equal (tumbel-feed-test-id-at-point) "1001"))
    (should (string-match-p "\\[Load more\\]" (buffer-string)))
    (should buffer-read-only)
    (should (equal tumbel-feed-test-fetches 1))))

(defun tumbel-feed-test-rules ()
  "Return how many separator rules the current feed shows."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (text-property-search-forward 'face 'tumbel-separator t)
        (cl-incf count))
      count)))

(ert-deftest tumbel-feed-test-posts-end-with-a-rule ()
  "A full-width rule follows each post, but not items a source renders."
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages))
    (should (equal (tumbel-feed-test-rules) 3))
    (goto-char (ewoc-location (ewoc-nth tumbel-feed--ewoc 1)))
    (forward-line -2)
    (should (equal (get-text-property (point) 'display) '(space :width text)))
    (should (looking-at-p " \n\n")))
  (let ((source (tumbel-feed-test-source (tumbel-feed-test-pages))))
    (setf (tumbel-feed-source-render source)
          (lambda (post) (insert (tumbel-npf-post-id post) "\n")))
    (tumbel-feed-test-with-feed source
      (should (equal (tumbel-feed-test-rules) 0)))))

(ert-deftest tumbel-feed-test-navigation-and-paging ()
  "Moving past the last post loads the next page without duplicates."
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages))
    (tumbel-feed-next)
    (should (equal (tumbel-feed-test-id-at-point) "1002"))
    (tumbel-feed-next)
    (should (equal (tumbel-feed-test-id-at-point) "1003"))
    (tumbel-feed-previous)
    (should (equal (tumbel-feed-test-id-at-point) "1002"))
    (tumbel-feed-next 2)
    (should (equal tumbel-feed-test-fetches 2))
    (should (equal (tumbel-feed-test-ids)
                   '("1001" "1002" "1003" "1004" "1005" "1006")))
    (should (equal (tumbel-feed-test-id-at-point) "1003"))
    (should (string-match-p "End of feed\\." (buffer-string)))
    (tumbel-feed-next 10)
    (should (equal (tumbel-feed-test-id-at-point) "1006"))
    (should (equal tumbel-feed-test-fetches 2))
    (tumbel-feed-previous 10)
    (should (equal (point) (point-min)))
    (tumbel-feed-next)
    (should (equal (tumbel-feed-test-id-at-point) "1001"))))

(ert-deftest tumbel-feed-test-loading-guard ()
  "Only one page fetch is in flight at a time."
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages)
                                                       t)
    (should (equal tumbel-feed-test-fetches 1))
    (should tumbel-feed--loading)
    (should (string-match-p "Loading…" (buffer-string)))
    (tumbel-feed-load-more)
    (tumbel-feed-next)
    (should (equal tumbel-feed-test-fetches 1))
    (funcall (car tumbel-feed-test-pending))
    (should-not tumbel-feed--loading)
    (should (equal (tumbel-feed-test-ids) '("1001" "1002" "1003")))
    (should (equal (tumbel-feed-test-id-at-point) "1001"))))

(ert-deftest tumbel-feed-test-failure ()
  "A failed fetch is reported in the footer and can be retried."
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages)
                                                       t)
    (funcall (cdr tumbel-feed-test-pending) '(tumbel-http-error "boom"))
    (should-not tumbel-feed--loading)
    (should (string-match-p "Tumblr HTTP error: boom" (buffer-string)))
    (should (string-match-p "\\[Load more\\]" (buffer-string)))
    (tumbel-feed-load-more)
    (should (equal tumbel-feed-test-fetches 2))))

(ert-deftest tumbel-feed-test-revert-restores-point ()
  "Reverting reloads the first page and returns to the same post."
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages))
    (tumbel-feed-next)
    (revert-buffer)
    (should (equal tumbel-feed-test-fetches 2))
    (should (equal (tumbel-feed-test-ids) '("1001" "1002" "1003")))
    (should (equal (tumbel-feed-test-id-at-point) "1002"))))

(ert-deftest tumbel-feed-test-toggle ()
  "The keep-reading button re-renders its post in full."
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages))
    (should-not (string-match-p "Third\\." (buffer-string)))
    (goto-char (point-min))
    (search-forward "[Keep reading]")
    (push-button (match-beginning 0))
    (should (string-match-p "Third\\." (buffer-string)))
    (should-not (string-match-p "Keep reading" (buffer-string)))
    (should (equal (tumbel-feed-test-id-at-point) "1003"))
    (should (equal (tumbel-feed-test-ids) '("1001" "1002" "1003")))))

(ert-deftest tumbel-feed-test-commands-at-point ()
  "Blog, tag and URL commands act on the post at point."
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages))
    (let (blog tag url)
      (let ((tumbel-npf-open-blog-function (lambda (name) (setq blog name)))
            (tumbel-npf-open-tag-function (lambda (x) (setq tag x)))
            (tumbel-npf-open-url-function (lambda (u) (setq url u))))
        (tumbel-feed-open-blog)
        (should (equal blog "example"))
        (tumbel-feed-browse-tag "emacs")
        (should (equal tag "emacs"))
        (tumbel-feed-browse-url)
        (should (equal url "https://example.tumblr.com/post/1001"))
        (tumbel-feed-copy-url)
        (should (equal (current-kill 0) "https://example.tumblr.com/post/1001"))
        (goto-char (point-min))
        (search-forward "#emacs")
        (tumbel-feed-activate)
        (should (equal tag "emacs"))))))

(ert-deftest tumbel-feed-test-urls-follow-point ()
  "`o' and `y' take the tag, link or other blog at point, else the post."
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages))
    (let (url)
      (cl-flet ((at (text)
                  (goto-char (point-min))
                  (search-forward text)
                  (goto-char (match-beginning 0)))
                (opened ()
                  (let ((tumbel-npf-open-url-function (lambda (u) (setq url u))))
                    (tumbel-feed-browse-url)
                    url))
                (copied ()
                  (tumbel-feed-copy-url)
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

(ert-deftest tumbel-feed-test-empty-feed ()
  "An empty first page says so."
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (list nil))
    (should (null (tumbel-feed-test-ids)))
    (should tumbel-feed--exhausted)
    (should (string-match-p "No posts\\." (buffer-string)))
    (should-error (tumbel-feed-post-at-point) :type 'user-error)))

(ert-deftest tumbel-feed-test-post-source ()
  "A single post shows expanded at once and is fetched again on revert."
  (let ((post (tumbel-test-post "1003")))
    (when (get-buffer "*tumbel: post/1003*")
      (kill-buffer "*tumbel: post/1003*"))
    (tumbel-test-logged-out
      (tumbel-test-with-backend
          `(("/blog/example/posts/1003" 200
             ,(tumbel-test-envelope (tumbel-test-post-fidelity "1001"))))
        (let ((buffer (tumbel-feed-display
                       (tumbel-feed-post-source "example" "1003" post))))
          (unwind-protect
              (with-current-buffer buffer
                (should (equal (buffer-name) "*tumbel: post/1003*"))
                (should (null tumbel-test-calls))
                (should (string-match-p "Third\\." (buffer-string)))
                (should tumbel-feed--exhausted)
                (revert-buffer)
                (should (equal (length tumbel-test-calls) 1))
                (should (equal (tumbel-feed-test-ids) '("1001")))
                (should (string-match-p "Heading One" (buffer-string))))
            (kill-buffer buffer)))))))

(ert-deftest tumbel-feed-test-open-post-from-feed ()
  "RET away from a button opens the post at point in its own buffer."
  (when (get-buffer "*tumbel: post/1001*")
    (kill-buffer "*tumbel: post/1001*"))
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages))
    (forward-line 1)
    (should-not (button-at (point)))
    (tumbel-feed-activate)
    (unwind-protect
        (with-current-buffer "*tumbel: post/1001*"
          (should (equal (tumbel-feed-test-ids) '("1001")))
          (should (string-match-p "Heading One" (buffer-string))))
      (kill-buffer "*tumbel: post/1001*"))))

(ert-deftest tumbel-feed-test-toggle-images ()
  "Toggling images re-renders the feed with the buffer-local setting."
  (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages))
    (let ((tumbel-display-images t))
      (cl-letf (((symbol-function 'message) #'ignore))
        (tumbel-feed-toggle-images)
        (should-not tumbel-display-images)
        (should (local-variable-p 'tumbel-display-images))
        (should (equal (tumbel-feed-test-ids) '("1001" "1002" "1003")))
        (tumbel-feed-toggle-images)
        (should tumbel-display-images)))))
;;;; Actions

(defmacro tumbel-feed-test-with-actions (responses &rest body)
  "Display the fixture pages logged in, with RESPONSES served, then run BODY."
  (declare (indent 1))
  `(tumbel-test-logged-in
     (tumbel-test-with-backend ,responses
       (cl-letf (((symbol-function 'tumbel-user-default-blog)
                  (lambda () "example"))
                 ((symbol-function 'tumbel-user-own-blog-p)
                  (lambda (name) (equal name "example")))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'message) #'ignore))
         (tumbel-feed-test-with-feed (tumbel-feed-test-source
                                      (tumbel-feed-test-pages))
           ,@body)))))

(defconst tumbel-feed-test-ok "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\"response\":{}}"
  "An empty successful envelope.")

(ert-deftest tumbel-feed-test-like-and-unlike ()
  "Liking sends the id and reblog key and updates the post in place."
  (tumbel-feed-test-with-actions `(("/user/" 200 ,tumbel-feed-test-ok))
    (tumbel-feed-like)
    (let ((call (tumbel-test-call 0))
          (post (tumbel-feed-post-at-point)))
      (should (string-suffix-p "/user/like" (nth 1 call)))
      (should (equal (tumbel-test-call-key call :body) "id=1001&reblog_key=rk1001"))
      (should (equal (tumbel-test-call-header call "Content-Type")
                     "application/x-www-form-urlencoded"))
      (should (eq (alist-get 'liked post) t))
      (should (equal (alist-get 'note_count post) 4))
      (should (string-match-p "4 notes · liked" (buffer-string)))
      (should (equal (tumbel-feed-test-id-at-point) "1001")))
    (tumbel-feed-like)
    (should (string-suffix-p "/user/unlike" (nth 1 (tumbel-test-call 0))))
    (should-not (alist-get 'liked (tumbel-feed-post-at-point)))
    (should (string-match-p "3 notes\n" (buffer-string)))))

(ert-deftest tumbel-feed-test-like-refused ()
  "A post that cannot be liked is refused before any request."
  (let ((post (copy-alist (tumbel-test-post "1001"))))
    (setf (alist-get 'can_like post) nil)
    (tumbel-test-logged-in
      (tumbel-test-with-backend nil
        (tumbel-feed-test-with-feed (tumbel-feed-test-source (list (list post)))
          (should-error (tumbel-feed-like) :type 'user-error)
          (should (null tumbel-test-calls)))))))

(ert-deftest tumbel-feed-test-pending-guard ()
  "A second action on a post waits for the first to finish."
  (tumbel-feed-test-with-actions `(("/user/" 200 ,tumbel-feed-test-ok))
    (let ((tumbel-test-defer t))
      (tumbel-feed-like)
      (should-error (tumbel-feed-like) :type 'user-error)
      (should (equal (length tumbel-test-calls) 1))
      (tumbel-test-deliver)
      (should (eq (alist-get 'liked (tumbel-feed-post-at-point)) t))
      (tumbel-feed-like)
      (should (equal (length tumbel-test-calls) 2)))))

(ert-deftest tumbel-feed-test-action-failure ()
  "A failed action leaves the post untouched and ready for another try."
  (tumbel-feed-test-with-actions `(("/user/" 429
                                    ,(tumbel-test-fixture "error-429.json")))
    (tumbel-feed-like)
    (should-not (alist-get 'liked (tumbel-feed-post-at-point)))
    (should-not (tumbel-feed-item-pending (tumbel-feed--item-at-point)))
    (tumbel-feed-like)
    (should (equal (length tumbel-test-calls) 2))))

(ert-deftest tumbel-feed-test-reblog ()
  "A quick reblog posts the parent references and tags as NPF."
  (tumbel-feed-test-with-actions
      '(("/blog/example/posts" 201
         "{\"meta\":{\"status\":201,\"msg\":\"Created\"},\"response\":{\"id\":\"999\"}}"))
    (tumbel-feed-reblog '("emacs" "lisp"))
    (let ((call (tumbel-test-call 0)))
      (should (eq (car call) 'post))
      (should (string-suffix-p "/blog/example/posts" (nth 1 call)))
      (should (equal (tumbel-test-call-key call :body)
                     (concat "{\"content\":[],"
                             "\"parent_tumblelog_uuid\":\"t:AbCdEfGhIjKlMnOpQrStUv\","
                             "\"parent_post_id\":\"1001\",\"reblog_key\":\"rk1001\","
                             "\"tags\":\"emacs,lisp\"}")))
      (should (equal (tumbel-test-call-header call "Content-Type")
                     "application/json")))
    (tumbel-feed-next 5)
    (should (equal (tumbel-feed-test-id-at-point) "1005"))
    (should-error (tumbel-feed-reblog) :type 'user-error)))

(ert-deftest tumbel-feed-test-reblog-fetches-missing-uuid ()
  "A post without its blog UUID gets it from the blog info, once."
  (let ((post (copy-alist (tumbel-test-post "1001"))))
    (setf (alist-get 'blog post) nil)
    (let ((tumbel-api--blog-uuids (make-hash-table :test #'equal)))
      (tumbel-test-logged-in
        (tumbel-test-with-backend
            `(("/blog/example/info" 200 ,(tumbel-test-fixture "blog-info.json"))
              ("/blog/example/posts" 201
               "{\"meta\":{\"status\":201,\"msg\":\"Created\"},\"response\":{\"id\":\"1\"}}"))
          (cl-letf (((symbol-function 'tumbel-user-default-blog)
                     (lambda () "example"))
                    ((symbol-function 'message) #'ignore))
            (tumbel-feed-test-with-feed (tumbel-feed-test-source
                                         (list (list post)))
              (tumbel-feed-reblog)
              (tumbel-feed-reblog)
              (should (equal (length tumbel-test-calls) 3))
              (should (string-match-p "\"parent_tumblelog_uuid\":\"t:AbCdEfGhIjKlMnOpQrStUv\""
                                      (tumbel-test-call-key (tumbel-test-call 0)
                                                            :body))))))))))

(ert-deftest tumbel-feed-test-follow-and-unfollow ()
  "Following sends the blog URL and flips the state on the post."
  (tumbel-feed-test-with-actions `(("/user/" 200 ,tumbel-feed-test-ok))
    (tumbel-feed-follow)
    (let ((call (tumbel-test-call 0)))
      (should (string-suffix-p "/user/follow" (nth 1 call)))
      (should (equal (tumbel-test-call-key call :body)
                     "url=https%3A%2F%2Fexample.tumblr.com%2F")))
    (should (eq (alist-get 'followed (tumbel-feed-post-at-point)) t))
    (tumbel-feed-follow)
    (should (string-suffix-p "/user/unfollow" (nth 1 (tumbel-test-call 0))))
    (should-not (alist-get 'followed (tumbel-feed-post-at-point)))))

(ert-deftest tumbel-feed-test-delete ()
  "Deleting an own post removes it from the feed."
  (tumbel-feed-test-with-actions `(("/post/delete" 200 ,tumbel-feed-test-ok))
    (tumbel-feed-delete)
    (let ((call (tumbel-test-call 0)))
      (should (string-suffix-p "/blog/example/post/delete" (nth 1 call)))
      (should (equal (tumbel-test-call-key call :body) "id=1001")))
    (should (equal (tumbel-feed-test-ids) '("1002" "1003")))
    (should (equal (tumbel-feed-test-id-at-point) "1002"))))

(ert-deftest tumbel-feed-test-delete-refused ()
  "Posts of other blogs cannot be deleted."
  (tumbel-test-logged-in
    (tumbel-test-with-backend nil
      (cl-letf (((symbol-function 'tumbel-user-own-blog-p) (lambda (_) nil)))
        (tumbel-feed-test-with-feed (tumbel-feed-test-source
                                     (tumbel-feed-test-pages))
          (should-error (tumbel-feed-delete) :type 'user-error)
          (should (null tumbel-test-calls)))))))
(ert-deftest tumbel-feed-test-compose-entry-points ()
  "The compose keys open the matching compose buffers."
  (let (reblogged edited)
    (cl-letf (((symbol-function 'tumbel-compose-reblog)
               (lambda (post) (setq reblogged (tumbel-npf-post-id post))))
              ((symbol-function 'tumbel-compose-edit)
               (lambda (post) (setq edited (tumbel-npf-post-id post)))))
      (tumbel-feed-test-with-feed (tumbel-feed-test-source
                                   (tumbel-feed-test-pages))
        (tumbel-feed-reblog-with-comment)
        (should (equal reblogged "1001"))
        (tumbel-feed-edit)
        (should (equal edited "1001"))
        (tumbel-feed-next 5)
        (should-error (tumbel-feed-reblog-with-comment) :type 'user-error)))
    (should (eq (lookup-key tumbel-feed-mode-map (kbd "c")) 'tumbel-compose))))
;;;; Filters

(ert-deftest tumbel-feed-test-hidden-reason ()
  "Posts are hidden by filtered tags or filtered content, case-insensitively."
  (let ((post (tumbel-test-post "1003")))
    (should (null (tumbel-feed-hidden-reason post '(nil . nil))))
    (should (equal (tumbel-feed-hidden-reason (tumbel-test-post "1001")
                                              '(("Lisp Code") . nil))
                   "#Lisp Code"))
    (should (equal (tumbel-feed-hidden-reason post '(nil . ("middle COMMENT")))
                   "\"middle COMMENT\""))
    (should (null (tumbel-feed-hidden-reason post '(("cats") . ("nope")))))))

(ert-deftest tumbel-feed-test-filtered-posts-collapse ()
  "Filtered posts show as one line until revealed."
  (let ((tumbel-user--filters '(("cats") . ("Root post text"))))
    (tumbel-feed-test-with-feed (tumbel-feed-test-source (tumbel-feed-test-pages))
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
      (should (equal (tumbel-feed-test-ids) '("1001" "1002" "1003"))))))

(ert-deftest tumbel-feed-test-filters-loaded-on-display ()
  "Feeds of posts fetch the filters once when logged in and re-render."
  (tumbel-test-logged-in
    (let ((tumbel-user--filters nil)
          (tumbel-user--filters-loading nil))
      (tumbel-test-with-backend
          `(("/user/filtered_tags" 200
             ,(tumbel-test-envelope '((filtered_tags . ["cats"]))))
            ("/user/filtered_content" 200
             ,(tumbel-test-envelope '((filtered_content . [])))))
        (let ((tumbel-test-defer t))
          (tumbel-feed-test-with-feed (tumbel-feed-test-source
                                       (tumbel-feed-test-pages))
            (should (string-match-p "Caption text" (buffer-string)))
            (tumbel-test-deliver)
            (should (equal (tumbel-user-filters) '(("cats") . nil)))
            (should-not (string-match-p "Caption text" (buffer-string)))
            (should (equal (length tumbel-test-calls) 2))))
        (tumbel-feed-test-with-feed (tumbel-feed-test-source
                                     (tumbel-feed-test-pages))
          (should (equal (length tumbel-test-calls) 2)))))))

(ert-deftest tumbel-feed-test-filters-failure-does-not-break-feeds ()
  "A failed filter fetch is reported and the feed still shows."
  (tumbel-test-logged-in
    (let ((tumbel-user--filters nil)
          (tumbel-user--filters-loading nil))
      (tumbel-test-with-backend '(("/user/filtered_tags" curl nil))
        (cl-letf (((symbol-function 'message) #'ignore))
          (tumbel-feed-test-with-feed (tumbel-feed-test-source
                                       (tumbel-feed-test-pages))
            (should (equal (tumbel-feed-test-ids) '("1001" "1002" "1003")))
            (should-not tumbel-user--filters-loading)))))))
(provide 'tumbel-feed-test)
;;; tumbel-feed-test.el ends here
