;;; tumbel-manage-test.el --- Tests for tumbel-manage.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the queue, drafts and inbox views and their actions.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-manage)
(require 'tumbel-test-support)

(defconst tumbel-manage-test-ok
  "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\"response\":{\"id\":\"1\"}}"
  "A successful envelope.")

(defmacro tumbel-manage-test-with-view (command name responses &rest body)
  "Run COMMAND, whose buffer is NAME, with RESPONSES served, then BODY."
  (declare (indent 3))
  `(tumbel-test-logged-in
     (tumbel-test-with-backend ,responses
       (cl-letf (((symbol-function 'tumbel-user-default-blog)
                  (lambda () "example"))
                 ((symbol-function 'tumbel-user-own-blog-p)
                  (lambda (name) (equal name "example")))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'message) #'ignore))
         (when (get-buffer ,name)
           (kill-buffer ,name))
         (unwind-protect
             (with-current-buffer (,command)
               ,@body)
           (dolist (buffer (buffer-list))
             (when (string-prefix-p "*tumbel: " (buffer-name buffer))
               (with-current-buffer buffer
                 (set-buffer-modified-p nil))
               (kill-buffer buffer))))))))

(defun tumbel-manage-test-ids ()
  "Return the ids of the posts shown."
  (mapcar (lambda (item) (tumbel-npf-post-id (tumbel-feed-item-post item)))
          (ewoc-collect tumbel-feed--ewoc #'identity)))

(defun tumbel-manage-test-queued (ids)
  "Return a queue envelope holding posts with IDS as JSON."
  (tumbel-test-envelope
   (list (cons 'posts
               (vconcat
                (mapcar (lambda (id)
                          `((type . "blocks") (blog_name . "example")
                            (id_string . ,id) (state . "queue")
                            (reblog_key . "k") (timestamp . 1757548800)
                            (content . [((type . "text") (text . ,id))])
                            (layout . []) (trail . [])))
                        ids))))))

(ert-deftest tumbel-manage-test-queue-view ()
  "The queue pages by offset and needs the token."
  (tumbel-manage-test-with-view tumbel-queue "*tumbel: queue/example*"
      `(("offset=2" 200 ,(tumbel-manage-test-queued nil))
        ("/posts/queue" 200 ,(tumbel-manage-test-queued '("q1" "q2"))))
    (should (equal (tumbel-manage-test-ids) '("q1" "q2")))
    (should tumbel-manage--queue)
    (should (string-match-p "/blog/example/posts/queue\\?npf=true&limit=20&offset=0"
                            (nth 1 (tumbel-test-call 0))))
    (should (equal (tumbel-test-call-header (tumbel-test-call 0) "Authorization")
                   "Bearer TOKEN"))
    (tumbel-feed-load-more)
    (should tumbel-feed--exhausted)))

(ert-deftest tumbel-manage-test-drafts-view ()
  "Drafts page by the id of the last post."
  (tumbel-manage-test-with-view tumbel-drafts "*tumbel: drafts/example*"
      `(("before_id=d2" 200 ,(tumbel-manage-test-queued nil))
        ("/posts/draft" 200 ,(tumbel-manage-test-queued '("d1" "d2"))))
    (should (equal (tumbel-manage-test-ids) '("d1" "d2")))
    (should-not tumbel-manage--queue)
    (tumbel-feed-load-more)
    (should (string-match-p "before_id=d2" (nth 1 (tumbel-test-call 0))))
    (should tumbel-feed--exhausted)))

(ert-deftest tumbel-manage-test-publish ()
  "Publishing sends the post back with its layout and the published state."
  (tumbel-manage-test-with-view tumbel-inbox "*tumbel: inbox/example*"
      `(("/posts/2001\\?" 200 ,(tumbel-test-envelope
                                (seq-elt (alist-get 'posts (alist-get 'response (tumbel-http-parse-json (tumbel-test-fixture "inbox.json") t))) 0)))
        ("/posts/2001" 200 ,tumbel-manage-test-ok)
        ("/posts/submission" 200 ,(tumbel-test-fixture "inbox.json")))
    (should (equal (tumbel-manage-test-ids) '("2001")))
    (should (string-match-p "asker asked:" (buffer-string)))
    (tumbel-manage-publish)
    (let ((call (tumbel-test-call 0)))
      (should (eq (car call) 'put))
      (should (string-suffix-p "/blog/example/posts/2001" (nth 1 call)))
      (should (equal (tumbel-test-call-key call :body)
                     (concat "{\"content\":[{\"type\":\"text\",\"text\":\"What editor do you use?\"}],"
                             "\"layout\":[{\"type\":\"ask\",\"blocks\":[0],\"attribution\":{\"type\":\"blog\",\"url\":\"https://asker.tumblr.com/\",\"blog\":{\"name\":\"asker\",\"uuid\":\"t:AskerUuid\"}}}],"
                             "\"state\":\"published\"}"))))
    (should (null (tumbel-manage-test-ids)))))

(ert-deftest tumbel-manage-test-publish-refused ()
  "Published posts and posts of others cannot be published."
  (tumbel-manage-test-with-view tumbel-queue "*tumbel: queue/example*"
      `(("/posts/queue" 200 ,(tumbel-test-envelope
                              (list (cons 'posts (vector (tumbel-test-post-fidelity "1001")))))))
    (should-error (tumbel-manage-publish) :type 'user-error)
    (should (equal (length tumbel-test-calls) 1))))

(ert-deftest tumbel-manage-test-answer ()
  "Answering opens a compose buffer keeping the question and ask layout."
  (tumbel-manage-test-with-view tumbel-inbox "*tumbel: inbox/example*"
      `(("/posts/2001\\?" 200 ,(tumbel-test-envelope
                                (seq-elt (alist-get 'posts (alist-get 'response (tumbel-http-parse-json (tumbel-test-fixture "inbox.json") t))) 0)))
        ("/posts/2001" 200 ,tumbel-manage-test-ok)
        ("/posts/submission" 200 ,(tumbel-test-fixture "inbox.json")))
    (let ((buffer (progn (tumbel-manage-answer)
                         (seq-find (lambda (b) (string-prefix-p "*tumbel: compose" (buffer-name b)))
                                   (buffer-list)))))
      (with-current-buffer buffer
        (should (string-match-p "Answering example/2001" header-line-format))
        (should (string-suffix-p "#+tumblr-block: 0\n\n" (buffer-string)))
        (should (equal (point) (point-max)))
        (insert "Emacs, of course.")
        (tumbel-compose-send))
      (let ((call (tumbel-test-call 0)))
        (should (eq (car call) 'put))
        (should (equal (tumbel-test-call-key call :body)
                       (concat "{\"content\":[{\"type\":\"text\",\"text\":\"What editor do you use?\"},"
                               "{\"type\":\"text\",\"text\":\"Emacs, of course.\"}],"
                               "\"state\":\"published\","
                               "\"layout\":[{\"type\":\"ask\",\"blocks\":[0],\"attribution\":{\"type\":\"blog\",\"url\":\"https://asker.tumblr.com/\",\"blog\":{\"name\":\"asker\",\"uuid\":\"t:AskerUuid\"}}}]}")))))))

(ert-deftest tumbel-manage-test-reorder-and-shuffle ()
  "Moving a queued post sends the reorder request and moves the node."
  (tumbel-manage-test-with-view tumbel-queue "*tumbel: queue/example*"
      `(("/queue/reorder" 200 ,tumbel-manage-test-ok)
        ("/queue/shuffle" 200 ,tumbel-manage-test-ok)
        ("/posts/queue" 200 ,(tumbel-manage-test-queued '("q1" "q2" "q3"))))
    (tumbel-feed-next)
    (tumbel-manage-move-up)
    (should (equal (tumbel-test-call-key (tumbel-test-call 0) :body)
                   "post_id=q2&insert_after=0"))
    (should (equal (tumbel-manage-test-ids) '("q2" "q1" "q3")))
    (should (equal (tumbel-npf-post-id (tumbel-feed-post-at-point)) "q2"))
    (tumbel-manage-move-down)
    (should (equal (tumbel-test-call-key (tumbel-test-call 0) :body)
                   "post_id=q2&insert_after=q1"))
    (should (equal (tumbel-manage-test-ids) '("q1" "q2" "q3")))
    (tumbel-manage-move-down)
    (should (equal (tumbel-test-call-key (tumbel-test-call 0) :body)
                   "post_id=q2&insert_after=q3"))
    (should (equal (tumbel-manage-test-ids) '("q1" "q3" "q2")))
    (should-error (tumbel-manage-move-down) :type 'user-error)
    (tumbel-manage-shuffle)
    (should (string-match-p "/queue/shuffle" (nth 1 (tumbel-test-call 1))))
    (should (string-match-p "/posts/queue" (nth 1 (tumbel-test-call 0))))))

(ert-deftest tumbel-manage-test-queue-commands-need-a-queue ()
  "Queue reordering is refused outside queue buffers."
  (tumbel-manage-test-with-view tumbel-drafts "*tumbel: drafts/example*"
      `(("/posts/draft" 200 ,(tumbel-manage-test-queued '("d1"))))
    (should-error (tumbel-manage-move-up) :type 'user-error)
    (should-error (tumbel-manage-shuffle) :type 'user-error)))

(provide 'tumbel-manage-test)
;;; tumbel-manage-test.el ends here
