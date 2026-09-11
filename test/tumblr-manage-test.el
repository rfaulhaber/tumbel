;;; tumblr-manage-test.el --- Tests for tumblr-manage.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the queue, drafts and inbox views and their actions.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-manage)
(require 'tumblr-test-support)

(defconst tumblr-manage-test-ok
  "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\"response\":{\"id\":\"1\"}}"
  "A successful envelope.")

(defmacro tumblr-manage-test-with-view (command name responses &rest body)
  "Run COMMAND, whose buffer is NAME, with RESPONSES served, then BODY."
  (declare (indent 3))
  `(tumblr-test-logged-in
     (tumblr-test-with-backend ,responses
       (cl-letf (((symbol-function 'tumblr-user-default-blog)
                  (lambda () "example"))
                 ((symbol-function 'tumblr-user-own-blog-p)
                  (lambda (name) (equal name "example")))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'message) #'ignore))
         (when (get-buffer ,name)
           (kill-buffer ,name))
         (unwind-protect
             (with-current-buffer (,command)
               ,@body)
           (dolist (buffer (buffer-list))
             (when (string-prefix-p "*tumblr: " (buffer-name buffer))
               (with-current-buffer buffer
                 (set-buffer-modified-p nil))
               (kill-buffer buffer))))))))

(defun tumblr-manage-test-ids ()
  "Return the ids of the posts shown."
  (mapcar (lambda (item) (tumblr-npf-post-id (tumblr-feed-item-post item)))
          (ewoc-collect tumblr-feed--ewoc #'identity)))

(defun tumblr-manage-test-queued (ids)
  "Return a queue envelope holding posts with IDS as JSON."
  (tumblr-test-envelope
   (list (cons 'posts
               (vconcat
                (mapcar (lambda (id)
                          `((type . "blocks") (blog_name . "example")
                            (id_string . ,id) (state . "queue")
                            (reblog_key . "k") (timestamp . 1757548800)
                            (content . [((type . "text") (text . ,id))])
                            (layout . []) (trail . [])))
                        ids))))))

(ert-deftest tumblr-manage-test-queue-view ()
  "The queue pages by offset and needs the token."
  (tumblr-manage-test-with-view tumblr-queue "*tumblr: queue/example*"
      `(("offset=2" 200 ,(tumblr-manage-test-queued nil))
        ("/posts/queue" 200 ,(tumblr-manage-test-queued '("q1" "q2"))))
    (should (equal (tumblr-manage-test-ids) '("q1" "q2")))
    (should tumblr-manage--queue)
    (should (string-match-p "/blog/example/posts/queue\\?npf=true&limit=20&offset=0"
                            (nth 1 (tumblr-test-call 0))))
    (should (equal (tumblr-test-call-header (tumblr-test-call 0) "Authorization")
                   "Bearer TOKEN"))
    (tumblr-feed-load-more)
    (should tumblr-feed--exhausted)))

(ert-deftest tumblr-manage-test-drafts-view ()
  "Drafts page by the id of the last post."
  (tumblr-manage-test-with-view tumblr-drafts "*tumblr: drafts/example*"
      `(("before_id=d2" 200 ,(tumblr-manage-test-queued nil))
        ("/posts/draft" 200 ,(tumblr-manage-test-queued '("d1" "d2"))))
    (should (equal (tumblr-manage-test-ids) '("d1" "d2")))
    (should-not tumblr-manage--queue)
    (tumblr-feed-load-more)
    (should (string-match-p "before_id=d2" (nth 1 (tumblr-test-call 0))))
    (should tumblr-feed--exhausted)))

(ert-deftest tumblr-manage-test-publish ()
  "Publishing sends the post back with its layout and the published state."
  (tumblr-manage-test-with-view tumblr-inbox "*tumblr: inbox/example*"
      `(("/posts/2001\\?" 200 ,(tumblr-test-envelope
                                (seq-elt (alist-get 'posts (alist-get 'response (tumblr-http-parse-json (tumblr-test-fixture "inbox.json") t))) 0)))
        ("/posts/2001" 200 ,tumblr-manage-test-ok)
        ("/posts/submission" 200 ,(tumblr-test-fixture "inbox.json")))
    (should (equal (tumblr-manage-test-ids) '("2001")))
    (should (string-match-p "asker asked:" (buffer-string)))
    (tumblr-manage-publish)
    (let ((call (tumblr-test-call 0)))
      (should (eq (car call) 'put))
      (should (string-suffix-p "/blog/example/posts/2001" (nth 1 call)))
      (should (equal (tumblr-test-call-key call :body)
                     (concat "{\"content\":[{\"type\":\"text\",\"text\":\"What editor do you use?\"}],"
                             "\"layout\":[{\"type\":\"ask\",\"blocks\":[0],\"attribution\":{\"type\":\"blog\",\"url\":\"https://asker.tumblr.com/\",\"blog\":{\"name\":\"asker\",\"uuid\":\"t:AskerUuid\"}}}],"
                             "\"state\":\"published\"}"))))
    (should (null (tumblr-manage-test-ids)))))

(ert-deftest tumblr-manage-test-publish-refused ()
  "Published posts and posts of others cannot be published."
  (tumblr-manage-test-with-view tumblr-queue "*tumblr: queue/example*"
      `(("/posts/queue" 200 ,(tumblr-test-envelope
                              (list (cons 'posts (vector (tumblr-test-post-fidelity "1001")))))))
    (should-error (tumblr-manage-publish) :type 'user-error)
    (should (equal (length tumblr-test-calls) 1))))

(ert-deftest tumblr-manage-test-answer ()
  "Answering opens a compose buffer keeping the question and ask layout."
  (tumblr-manage-test-with-view tumblr-inbox "*tumblr: inbox/example*"
      `(("/posts/2001\\?" 200 ,(tumblr-test-envelope
                                (seq-elt (alist-get 'posts (alist-get 'response (tumblr-http-parse-json (tumblr-test-fixture "inbox.json") t))) 0)))
        ("/posts/2001" 200 ,tumblr-manage-test-ok)
        ("/posts/submission" 200 ,(tumblr-test-fixture "inbox.json")))
    (let ((buffer (progn (tumblr-manage-answer)
                         (seq-find (lambda (b) (string-prefix-p "*tumblr: compose" (buffer-name b)))
                                   (buffer-list)))))
      (with-current-buffer buffer
        (should (string-match-p "Answering example/2001" header-line-format))
        (should (string-suffix-p "#+tumblr-block: 0\n\n" (buffer-string)))
        (should (equal (point) (point-max)))
        (insert "Emacs, of course.")
        (tumblr-compose-send))
      (let ((call (tumblr-test-call 0)))
        (should (eq (car call) 'put))
        (should (equal (tumblr-test-call-key call :body)
                       (concat "{\"content\":[{\"type\":\"text\",\"text\":\"What editor do you use?\"},"
                               "{\"type\":\"text\",\"text\":\"Emacs, of course.\"}],"
                               "\"state\":\"published\","
                               "\"layout\":[{\"type\":\"ask\",\"blocks\":[0],\"attribution\":{\"type\":\"blog\",\"url\":\"https://asker.tumblr.com/\",\"blog\":{\"name\":\"asker\",\"uuid\":\"t:AskerUuid\"}}}]}")))))))

(ert-deftest tumblr-manage-test-reorder-and-shuffle ()
  "Moving a queued post sends the reorder request and moves the node."
  (tumblr-manage-test-with-view tumblr-queue "*tumblr: queue/example*"
      `(("/queue/reorder" 200 ,tumblr-manage-test-ok)
        ("/queue/shuffle" 200 ,tumblr-manage-test-ok)
        ("/posts/queue" 200 ,(tumblr-manage-test-queued '("q1" "q2" "q3"))))
    (tumblr-feed-next)
    (tumblr-manage-move-up)
    (should (equal (tumblr-test-call-key (tumblr-test-call 0) :body)
                   "post_id=q2&insert_after=0"))
    (should (equal (tumblr-manage-test-ids) '("q2" "q1" "q3")))
    (should (equal (tumblr-npf-post-id (tumblr-feed-post-at-point)) "q2"))
    (tumblr-manage-move-down)
    (should (equal (tumblr-test-call-key (tumblr-test-call 0) :body)
                   "post_id=q2&insert_after=q1"))
    (should (equal (tumblr-manage-test-ids) '("q1" "q2" "q3")))
    (tumblr-manage-move-down)
    (should (equal (tumblr-test-call-key (tumblr-test-call 0) :body)
                   "post_id=q2&insert_after=q3"))
    (should (equal (tumblr-manage-test-ids) '("q1" "q3" "q2")))
    (should-error (tumblr-manage-move-down) :type 'user-error)
    (tumblr-manage-shuffle)
    (should (string-match-p "/queue/shuffle" (nth 1 (tumblr-test-call 1))))
    (should (string-match-p "/posts/queue" (nth 1 (tumblr-test-call 0))))))

(ert-deftest tumblr-manage-test-queue-commands-need-a-queue ()
  "Queue reordering is refused outside queue buffers."
  (tumblr-manage-test-with-view tumblr-drafts "*tumblr: drafts/example*"
      `(("/posts/draft" 200 ,(tumblr-manage-test-queued '("d1"))))
    (should-error (tumblr-manage-move-up) :type 'user-error)
    (should-error (tumblr-manage-shuffle) :type 'user-error)))

(provide 'tumblr-manage-test)
;;; tumblr-manage-test.el ends here
