;;; tumblr-compose-test.el --- Tests for tumblr-compose.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for parsing the compose buffer, building request bodies
;; and the new-post, reblog and edit flows against the fake backend.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-compose)
(require 'tumblr-test-support)

(defconst tumblr-compose-test-created
  "{\"meta\":{\"status\":201,\"msg\":\"Created\"},\"response\":{\"id\":\"123\"}}"
  "The response to a created post.")

(defmacro tumblr-compose-test-with-session (responses &rest body)
  "Run BODY logged in as example, with RESPONSES served and prompts stubbed."
  (declare (indent 1))
  `(let ((tumblr-user--info nil)
         (tumblr-org-blog-uuid-function (lambda (name) (format "t:%s" name)))
         (tumblr-default-blog nil)
         (tumblr-compose-test-messages nil))
     (tumblr-test-logged-in
       (tumblr-test-with-backend
           (append ,responses
                   `(("/user/info" 200 ,(tumblr-test-fixture "user-info.json"))))
         (cl-letf (((symbol-function 'message)
                    (lambda (fmt &rest args)
                      (push (apply #'format fmt args)
                            tumblr-compose-test-messages)))
                   ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (unwind-protect
               (progn ,@body)
             (dolist (buffer (buffer-list))
               (when (string-prefix-p "*tumblr: compose" (buffer-name buffer))
                 (with-current-buffer buffer
                   (set-buffer-modified-p nil))
                 (kill-buffer buffer)))))))))

(defvar tumblr-compose-test-messages nil
  "Messages shown during the running test, most recent first.")

(defun tumblr-compose-test-body ()
  "Return the JSON body of the most recent request."
  (tumblr-test-call-key (tumblr-test-call 0) :body))

;;;; Parsing

(ert-deftest tumblr-compose-test-parse ()
  "Headers become plist entries and the rest is the body."
  (let ((plist (tumblr-compose-parse
                (concat "#+blog: example\n#+tags: emacs, lisp code ,\n"
                        "#+state: Draft\n#+publish_on: 2026-10-01T10:00:00Z\n"
                        "\nHello\n\nWorld\n"))))
    (should (equal (plist-get plist :blog) "example"))
    (should (equal (plist-get plist :tags) '("emacs" "lisp code")))
    (should (equal (plist-get plist :state) "draft"))
    (should (equal (plist-get plist :publish-on) "2026-10-01T10:00:00Z"))
    (should (equal (plist-get plist :body) "Hello\n\nWorld"))))

(ert-deftest tumblr-compose-test-parse-edge-cases ()
  "Empty headers are nil, no headers means all body, unknown keys signal."
  (let ((plist (tumblr-compose-parse "#+blog:\n#+tags:\n\nBody only")))
    (should (null (plist-get plist :blog)))
    (should (null (plist-get plist :tags)))
    (should (equal (plist-get plist :body) "Body only")))
  (should (equal (plist-get (tumblr-compose-parse "Just text\n#+blog: x")
                            :body)
                 "Just text\n#+blog: x"))
  (should-error (tumblr-compose-parse "#+colour: red\n") :type 'user-error))


;;;; New posts

(ert-deftest tumblr-compose-test-new-post ()
  "A new post is sent as NPF text blocks and the buffer is killed."
  (tumblr-compose-test-with-session
      `(("/blog/example/posts" 201 ,tumblr-compose-test-created))
    (let ((buffer (tumblr-compose)))
      (with-current-buffer buffer
        (should (derived-mode-p 'tumblr-compose-mode))
        (should (equal (buffer-substring-no-properties (point-min) (point))
                       "#+blog: example\n#+tags: \n#+state: published\n\n"))
        (should (string-match-p "New post" header-line-format))
        (insert "Hello\n\nWorld")
        (tumblr-compose-set-tags "a, b")
        (tumblr-compose-send))
      (should-not (buffer-live-p buffer))
      (let ((call (tumblr-test-call 0)))
        (should (eq (car call) 'post))
        (should (string-suffix-p "/blog/example/posts" (nth 1 call)))
        (should (equal (tumblr-test-call-key call :body)
                       (concat "{\"content\":[{\"type\":\"text\",\"text\":\"Hello\"},"
                               "{\"type\":\"text\",\"text\":\"World\"}],"
                               "\"state\":\"published\",\"tags\":\"a,b\"}"))))
      (should (equal (car tumblr-compose-test-messages)
                     "Posted https://www.tumblr.com/example/123")))))

(ert-deftest tumblr-compose-test-validation ()
  "Empty posts, missing blogs and bad states are refused before sending."
  (tumblr-compose-test-with-session nil
    (with-current-buffer (tumblr-compose)
      (should-error (tumblr-compose-send) :type 'user-error)
      (insert "Text")
      (tumblr-compose-set-state "scheduled")
      (should-error (tumblr-compose-send) :type 'user-error)
      (tumblr-compose-set-state "queue")
      (tumblr-compose--set-keyword "blog" "")
      (should-error (tumblr-compose-send) :type 'user-error)
      (should (null (seq-filter (lambda (call) (string-match-p "/posts" (nth 1 call)))
                                tumblr-test-calls))))))

(ert-deftest tumblr-compose-test-sending-guard-and-failure ()
  "While sending the buffer is read-only; a failure re-enables it."
  (tumblr-compose-test-with-session
      `(("/blog/example/posts" 429 ,(tumblr-test-fixture "error-429.json")))
    (let ((tumblr-test-defer t))
      (with-current-buffer (tumblr-compose)
        (insert "Text")
        (tumblr-compose-send)
        (should tumblr-compose--sending)
        (should buffer-read-only)
        (should (string-match-p "sending" header-line-format))
        (should-error (tumblr-compose-send) :type 'user-error)
        (tumblr-test-deliver)
        (should-not tumblr-compose--sending)
        (should-not buffer-read-only)
        (should (buffer-live-p (current-buffer)))
        (should (string-match-p "429" (car tumblr-compose-test-messages)))))))

(ert-deftest tumblr-compose-test-header-commands ()
  "Blog, tags and state commands rewrite their header lines."
  (tumblr-compose-test-with-session nil
    (with-current-buffer (tumblr-compose)
      (tumblr-compose-set-blog "secondary")
      (tumblr-compose-set-tags "x,y")
      (tumblr-compose-set-state "private")
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "#+blog: secondary\n#+tags: x, y\n#+state: private\n\n"))
      (goto-char (point-min))
      (delete-region (point) (line-beginning-position 2))
      (tumblr-compose-set-blog "example")
      (should (string-prefix-p "#+tags: x, y\n#+state: private\n#+blog: example\n"
                               (buffer-string))))))

(ert-deftest tumblr-compose-test-preview ()
  "The preview shows the JSON that would be sent."
  (tumblr-compose-test-with-session nil
    (with-current-buffer (tumblr-compose)
      (insert "Hi")
      (tumblr-compose-preview)
      (with-current-buffer "*tumblr: preview*"
        (should (string-match-p "\"text\":\"Hi\"" (buffer-string)))
        (kill-buffer)))))

(ert-deftest tumblr-compose-test-kill-confirmation ()
  "Killing a modified compose buffer asks first."
  (tumblr-compose-test-with-session nil
    (let ((buffer (tumblr-compose)))
      (with-current-buffer buffer
        (insert "Draft"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (kill-buffer buffer)
        (should (buffer-live-p buffer)))
      (kill-buffer buffer)
      (should-not (buffer-live-p buffer)))))

;;;; Reblogs and edits

(ert-deftest tumblr-compose-test-reblog ()
  "A reblog carries the parent references and the comment as content."
  (tumblr-compose-test-with-session
      `(("/blog/example/posts" 201 ,tumblr-compose-test-created))
    (with-current-buffer (tumblr-compose-reblog (tumblr-test-post "1003"))
      (should (string-match-p "Reblogging example/1003" header-line-format))
      (tumblr-compose-send))
    (should (equal (tumblr-compose-test-body)
                   (concat "{\"content\":[],"
                           "\"parent_tumblelog_uuid\":\"t:AbCdEfGhIjKlMnOpQrStUv\","
                           "\"parent_post_id\":\"1003\",\"reblog_key\":\"rk1003\","
                           "\"state\":\"published\"}")))
    (with-current-buffer (tumblr-compose-reblog (tumblr-test-post "1003"))
      (insert "Nice!")
      (tumblr-compose-send))
    (should (string-prefix-p "{\"content\":[{\"type\":\"text\",\"text\":\"Nice!\"}],"
                             (tumblr-compose-test-body)))))



(ert-deftest tumblr-compose-test-edit ()
  "Editing prefills the post as Org and sends a PUT for the same post."
  (tumblr-compose-test-with-session
      `(("/blog/example/posts/1001\\?" 200
         ,(tumblr-test-envelope (tumblr-test-post-fidelity "1001")))
        ("/blog/example/posts/1001" 200
         "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\"response\":{\"id\":\"1001\"}}"))
    (let ((buffer (tumblr-compose-edit (tumblr-test-post "1001"))))
      (with-current-buffer buffer
        (should (string-match-p "Editing example/1001" header-line-format))
        (should (string-prefix-p
                 (concat "#+blog: example\n#+tags: emacs, lisp code\n"
                         "#+state: published\n\n* Heading One\n\n"
                         "*Bold*, /italic/, a [[https://example.com/][link]], "
                         "[[tumblr:friend][@mention]] and small.")
                 (buffer-string)))
        (should-not (buffer-modified-p))
        (goto-char (point-max))
        (insert "\n\nAppended.")
        (tumblr-compose-send))
      (should-not (buffer-live-p buffer))
      (let ((call (tumblr-test-call 0)))
        (should (eq (car call) 'put))
        (should (string-suffix-p "/blog/example/posts/1001" (nth 1 call)))
        (should (string-match-p
                 (regexp-quote "{\"type\":\"text\",\"text\":\"Appended.\"}],\"state\":\"published\",\"tags\":\"emacs,lisp code\"}")
                 (tumblr-test-call-key call :body))))
      (should (equal (car tumblr-compose-test-messages)
                     "Edited https://www.tumblr.com/example/1001")))))

(ert-deftest tumblr-compose-test-edit-refusals ()
  "Only own posts can be edited."
  (tumblr-compose-test-with-session nil
    (let ((post (copy-alist (tumblr-test-post "1001"))))
      (setf (alist-get 'blog_name post) "someone-else")
      (should-error (tumblr-compose-edit post) :type 'user-error))))

(ert-deftest tumblr-compose-test-requires-login ()
  "Composing needs a login."
  (tumblr-test-logged-out
    (should-error (tumblr-compose) :type 'user-error)))

(ert-deftest tumblr-compose-test-org-body ()
  "Org markup in the body becomes formatting and headings."
  (tumblr-compose-test-with-session
      `(("/blog/example/posts" 201 ,tumblr-compose-test-created))
    (with-current-buffer (tumblr-compose)
      (should (derived-mode-p 'org-mode))
      (insert "* Title\n\nSome *bold* text.")
      (tumblr-compose-send))
    (should (equal (tumblr-compose-test-body)
                   (concat "{\"content\":[{\"type\":\"text\",\"text\":\"Title\","
                           "\"subtype\":\"heading1\"},"
                           "{\"type\":\"text\",\"text\":\"Some bold text.\","
                           "\"formatting\":[{\"type\":\"bold\",\"start\":5,"
                           "\"end\":9}]}],\"state\":\"published\"}")))))

(ert-deftest tumblr-compose-test-attachment-upload ()
  "An attached image is sent as a multipart upload."
  (let* ((dir (make-temp-file "tumblr-compose-test" t))
         (file (expand-file-name "cat.png" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "PNGBYTES"))
          (tumblr-compose-test-with-session
              `(("/blog/example/posts" 201 ,tumblr-compose-test-created))
            (with-current-buffer (tumblr-compose)
              (insert "Look:")
              (tumblr-compose-attach file)
              (should (string-match-p (concat "Look:\n\n\\[\\[file:"
                                              (regexp-quote file) "\\]\\]\n\n")
                                      (buffer-string)))
              (tumblr-compose-send))
            (let* ((call (tumblr-test-call 0))
                   (type (tumblr-test-call-header call "Content-Type"))
                   (body (tumblr-test-call-key call :body)))
              (should (string-prefix-p "multipart/form-data; boundary=" type))
              (should-not (multibyte-string-p body))
              (let ((boundary (substring type (length "multipart/form-data; boundary="))))
                (should (string-match-p (concat "^--" (regexp-quote boundary)
                                                "\r\n")
                                        body))
                (should (string-match-p "Content-Disposition: form-data; name=\"json\"\r\n"
                                        body))
                (should (string-match-p (regexp-quote
                                         "{\"content\":[{\"type\":\"text\",\"text\":\"Look:\"},{\"type\":\"image\",\"media\":[{\"type\":\"image/png\",\"identifier\":\"image0\"}]}],\"state\":\"published\"}")
                                        body))
                (should (string-match-p "name=\"image0\"; filename=\"cat.png\"\r\n"
                                        body))
                (should (string-match-p "Content-Type: image/png\r\n\r\nPNGBYTES\r\n"
                                        body))
                (should (string-suffix-p (concat "--" boundary "--\r\n") body))))))
      (delete-directory dir t))))

(ert-deftest tumblr-compose-test-edit-with-media ()
  "Editing keeps non-text blocks through placeholders."
  (tumblr-compose-test-with-session
      `(("/blog/example/posts/1002\\?" 200
         ,(tumblr-test-envelope (tumblr-test-post-fidelity "1002")))
        ("/blog/example/posts/1002" 200
         "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\"response\":{\"id\":\"1002\"}}"))
    (let ((buffer (tumblr-compose-edit (tumblr-test-post "1002"))))
      (with-current-buffer buffer
        (should (string-match-p "post_format=npf" (nth 1 (tumblr-test-call 0))))
        (should (string-suffix-p "\n\n#+tumblr-block: 0\n\nCaption text."
                                 (buffer-string)))
        (goto-char (point-max))
        (insert " Edited.")
        (tumblr-compose-send))
      (let ((call (tumblr-test-call 0)))
        (should (eq (car call) 'put))
        (should (string-match-p
                 (regexp-quote "{\"content\":[{\"type\":\"image\",\"media\":[{\"media_key\":\"k1:2048\"")
                 (tumblr-test-call-key call :body)))
        (should (string-match-p "\"has_original_dimensions\":true"
                                (tumblr-test-call-key call :body)))
        (should (string-match-p
                 (regexp-quote "{\"type\":\"text\",\"text\":\"Caption text. Edited.\"}],\"state\":\"published\",\"tags\":\"cats\"}")
                 (tumblr-test-call-key call :body)))))))
(provide 'tumblr-compose-test)
;;; tumblr-compose-test.el ends here
