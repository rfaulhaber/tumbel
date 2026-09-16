;;; tumbel-compose-test.el --- Tests for tumbel-compose.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for parsing the compose buffer, building request bodies
;; and the new-post, reblog and edit flows against the fake backend.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-compose)
(require 'tumbel-test-support)

(defconst tumbel-compose-test-created
  "{\"meta\":{\"status\":201,\"msg\":\"Created\"},\"response\":{\"id\":\"123\"}}"
  "The response to a created post.")

(defmacro tumbel-compose-test-with-session (responses &rest body)
  "Run BODY logged in as example, with RESPONSES served and prompts stubbed."
  (declare (indent 1))
  `(let ((tumbel-user--info nil)
         (tumbel-org-blog-uuid-function (lambda (name) (format "t:%s" name)))
         (tumbel-default-blog nil)
         (tumbel-compose-test-messages nil))
     (tumbel-test-logged-in
       (tumbel-test-with-backend
           (append ,responses
                   `(("/user/info" 200 ,(tumbel-test-fixture "user-info.json"))))
         (cl-letf (((symbol-function 'message)
                    (lambda (fmt &rest args)
                      (push (apply #'format fmt args)
                            tumbel-compose-test-messages)))
                   ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (unwind-protect
               (progn ,@body)
             (dolist (buffer (buffer-list))
               (when (string-prefix-p "*tumbel: compose" (buffer-name buffer))
                 (with-current-buffer buffer
                   (set-buffer-modified-p nil))
                 (kill-buffer buffer)))))))))

(defvar tumbel-compose-test-messages nil
  "Messages shown during the running test, most recent first.")

(defun tumbel-compose-test-body ()
  "Return the JSON body of the most recent request."
  (tumbel-test-call-key (tumbel-test-call 0) :body))

;;;; Parsing

(ert-deftest tumbel-compose-test-parse ()
  "Headers become plist entries and the rest is the body."
  (let ((plist (tumbel-compose-parse
                (concat "#+blog: example\n#+tags: emacs, lisp code ,\n"
                        "#+state: Draft\n#+publish_on: 2026-10-01T10:00:00Z\n"
                        "\nHello\n\nWorld\n"))))
    (should (equal (plist-get plist :blog) "example"))
    (should (equal (plist-get plist :tags) '("emacs" "lisp code")))
    (should (equal (plist-get plist :state) "draft"))
    (should (equal (plist-get plist :publish-on) "2026-10-01T10:00:00Z"))
    (should (equal (plist-get plist :body) "Hello\n\nWorld"))))

(ert-deftest tumbel-compose-test-parse-edge-cases ()
  "Empty headers are nil, no headers means all body, unknown keys signal."
  (let ((plist (tumbel-compose-parse "#+blog:\n#+tags:\n\nBody only")))
    (should (null (plist-get plist :blog)))
    (should (null (plist-get plist :tags)))
    (should (equal (plist-get plist :body) "Body only")))
  (should (equal (plist-get (tumbel-compose-parse "Just text\n#+blog: x")
                            :body)
                 "Just text\n#+blog: x"))
  (should-error (tumbel-compose-parse "#+colour: red\n") :type 'user-error))


;;;; New posts

(ert-deftest tumbel-compose-test-new-post ()
  "A new post is sent as NPF text blocks and the buffer is killed."
  (tumbel-compose-test-with-session
      `(("/blog/example/posts" 201 ,tumbel-compose-test-created))
    (let ((buffer (tumbel-compose)))
      (with-current-buffer buffer
        (should (derived-mode-p 'tumbel-compose-mode))
        (should (equal (buffer-substring-no-properties (point-min) (point))
                       "#+blog: example\n#+tags: \n#+state: published\n\n"))
        (should (string-match-p "New post" header-line-format))
        (insert "Hello\n\nWorld")
        (tumbel-compose-set-tags "a, b")
        (tumbel-compose-send))
      (should-not (buffer-live-p buffer))
      (let ((call (tumbel-test-call 0)))
        (should (eq (car call) 'post))
        (should (string-suffix-p "/blog/example/posts" (nth 1 call)))
        (should (equal (tumbel-test-call-key call :body)
                       (concat "{\"content\":[{\"type\":\"text\",\"text\":\"Hello\"},"
                               "{\"type\":\"text\",\"text\":\"World\"}],"
                               "\"state\":\"published\",\"tags\":\"a,b\"}"))))
      (should (equal (car tumbel-compose-test-messages)
                     "Posted https://www.tumblr.com/example/123")))))

(ert-deftest tumbel-compose-test-validation ()
  "Empty posts, missing blogs and bad states are refused before sending."
  (tumbel-compose-test-with-session nil
    (with-current-buffer (tumbel-compose)
      (should-error (tumbel-compose-send) :type 'user-error)
      (insert "Text")
      (tumbel-compose-set-state "scheduled")
      (should-error (tumbel-compose-send) :type 'user-error)
      (tumbel-compose-set-state "queue")
      (tumbel-compose--set-keyword "blog" "")
      (should-error (tumbel-compose-send) :type 'user-error)
      (should (null (seq-filter (lambda (call) (string-match-p "/posts" (nth 1 call)))
                                tumbel-test-calls))))))

(ert-deftest tumbel-compose-test-sending-guard-and-failure ()
  "While sending the buffer is read-only; a failure re-enables it."
  (tumbel-compose-test-with-session
      `(("/blog/example/posts" 429 ,(tumbel-test-fixture "error-429.json")))
    (let ((tumbel-test-defer t))
      (with-current-buffer (tumbel-compose)
        (insert "Text")
        (tumbel-compose-send)
        (should tumbel-compose--sending)
        (should buffer-read-only)
        (should (string-match-p "sending" header-line-format))
        (should-error (tumbel-compose-send) :type 'user-error)
        (tumbel-test-deliver)
        (should-not tumbel-compose--sending)
        (should-not buffer-read-only)
        (should (buffer-live-p (current-buffer)))
        (should (string-match-p "429" (car tumbel-compose-test-messages)))))))

(ert-deftest tumbel-compose-test-header-commands ()
  "Blog, tags and state commands rewrite their header lines."
  (tumbel-compose-test-with-session nil
    (with-current-buffer (tumbel-compose)
      (tumbel-compose-set-blog "secondary")
      (tumbel-compose-set-tags "x,y")
      (tumbel-compose-set-state "private")
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "#+blog: secondary\n#+tags: x, y\n#+state: private\n\n"))
      (goto-char (point-min))
      (delete-region (point) (line-beginning-position 2))
      (tumbel-compose-set-blog "example")
      (should (string-prefix-p "#+tags: x, y\n#+state: private\n#+blog: example\n"
                               (buffer-string))))))

(ert-deftest tumbel-compose-test-preview ()
  "The preview shows the JSON that would be sent."
  (tumbel-compose-test-with-session nil
    (with-current-buffer (tumbel-compose)
      (insert "Hi")
      (tumbel-compose-preview)
      (with-current-buffer "*tumbel: preview*"
        (should (string-match-p "\"text\":\"Hi\"" (buffer-string)))
        (kill-buffer)))))

(ert-deftest tumbel-compose-test-kill-confirmation ()
  "Killing a modified compose buffer asks first."
  (tumbel-compose-test-with-session nil
    (let ((buffer (tumbel-compose)))
      (with-current-buffer buffer
        (insert "Draft"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (kill-buffer buffer)
        (should (buffer-live-p buffer)))
      (kill-buffer buffer)
      (should-not (buffer-live-p buffer)))))

;;;; Reblogs and edits

(ert-deftest tumbel-compose-test-reblog ()
  "A reblog carries the parent references and the comment as content."
  (tumbel-compose-test-with-session
      `(("/blog/example/posts" 201 ,tumbel-compose-test-created))
    (with-current-buffer (tumbel-compose-reblog (tumbel-test-post "1003"))
      (should (string-match-p "Reblogging example/1003" header-line-format))
      (tumbel-compose-send))
    (should (equal (tumbel-compose-test-body)
                   (concat "{\"content\":[],"
                           "\"parent_tumblelog_uuid\":\"t:AbCdEfGhIjKlMnOpQrStUv\","
                           "\"parent_post_id\":\"1003\",\"reblog_key\":\"rk1003\","
                           "\"state\":\"published\"}")))
    (with-current-buffer (tumbel-compose-reblog (tumbel-test-post "1003"))
      (insert "Nice!")
      (tumbel-compose-send))
    (should (string-prefix-p "{\"content\":[{\"type\":\"text\",\"text\":\"Nice!\"}],"
                             (tumbel-compose-test-body)))))



(ert-deftest tumbel-compose-test-edit ()
  "Editing prefills the post as Org and sends a PUT for the same post."
  (tumbel-compose-test-with-session
      `(("/blog/example/posts/1001\\?" 200
         ,(tumbel-test-envelope (tumbel-test-post-fidelity "1001")))
        ("/blog/example/posts/1001" 200
         "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\"response\":{\"id\":\"1001\"}}"))
    (let ((buffer (tumbel-compose-edit (tumbel-test-post "1001"))))
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
        (tumbel-compose-send))
      (should-not (buffer-live-p buffer))
      (let ((call (tumbel-test-call 0)))
        (should (eq (car call) 'put))
        (should (string-suffix-p "/blog/example/posts/1001" (nth 1 call)))
        (should (string-match-p
                 (regexp-quote "{\"type\":\"text\",\"text\":\"Appended.\"}],\"state\":\"published\",\"tags\":\"emacs,lisp code\"}")
                 (tumbel-test-call-key call :body))))
      (should (equal (car tumbel-compose-test-messages)
                     "Edited https://www.tumblr.com/example/1001")))))

(ert-deftest tumbel-compose-test-edit-refusals ()
  "Only own posts can be edited."
  (tumbel-compose-test-with-session nil
    (let ((post (copy-alist (tumbel-test-post "1001"))))
      (setf (alist-get 'blog_name post) "someone-else")
      (should-error (tumbel-compose-edit post) :type 'user-error))))

(ert-deftest tumbel-compose-test-requires-login ()
  "Composing needs a login."
  (tumbel-test-logged-out
    (should-error (tumbel-compose) :type 'user-error)))

(ert-deftest tumbel-compose-test-org-body ()
  "Org markup in the body becomes formatting and headings."
  (tumbel-compose-test-with-session
      `(("/blog/example/posts" 201 ,tumbel-compose-test-created))
    (with-current-buffer (tumbel-compose)
      (should (derived-mode-p 'org-mode))
      (insert "* Title\n\nSome *bold* text.")
      (tumbel-compose-send))
    (should (equal (tumbel-compose-test-body)
                   (concat "{\"content\":[{\"type\":\"text\",\"text\":\"Title\","
                           "\"subtype\":\"heading1\"},"
                           "{\"type\":\"text\",\"text\":\"Some bold text.\","
                           "\"formatting\":[{\"type\":\"bold\",\"start\":5,"
                           "\"end\":9}]}],\"state\":\"published\"}")))))

(ert-deftest tumbel-compose-test-attachment-upload ()
  "An attached image is sent as a multipart upload."
  (let* ((dir (make-temp-file "tumbel-compose-test" t))
         (file (expand-file-name "cat.png" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "PNGBYTES"))
          (tumbel-compose-test-with-session
              `(("/blog/example/posts" 201 ,tumbel-compose-test-created))
            (with-current-buffer (tumbel-compose)
              (insert "Look:")
              (tumbel-compose-attach file)
              (should (string-match-p (concat "Look:\n\n\\[\\[file:"
                                              (regexp-quote file) "\\]\\]\n\n")
                                      (buffer-string)))
              (tumbel-compose-send))
            (let* ((call (tumbel-test-call 0))
                   (type (tumbel-test-call-header call "Content-Type"))
                   (body (tumbel-test-call-key call :body)))
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

(ert-deftest tumbel-compose-test-edit-with-media ()
  "Editing keeps non-text blocks through placeholders."
  (tumbel-compose-test-with-session
      `(("/blog/example/posts/1002\\?" 200
         ,(tumbel-test-envelope (tumbel-test-post-fidelity "1002")))
        ("/blog/example/posts/1002" 200
         "{\"meta\":{\"status\":200,\"msg\":\"OK\"},\"response\":{\"id\":\"1002\"}}"))
    (let ((buffer (tumbel-compose-edit (tumbel-test-post "1002"))))
      (with-current-buffer buffer
        (should (string-match-p "post_format=npf" (nth 1 (tumbel-test-call 0))))
        (should (string-suffix-p "\n\n#+tumblr-block: 0\n\nCaption text."
                                 (buffer-string)))
        (goto-char (point-max))
        (insert " Edited.")
        (tumbel-compose-send))
      (let ((call (tumbel-test-call 0)))
        (should (eq (car call) 'put))
        (should (string-match-p
                 (regexp-quote "{\"content\":[{\"type\":\"image\",\"media\":[{\"media_key\":\"k1:2048\"")
                 (tumbel-test-call-key call :body)))
        (should (string-match-p "\"has_original_dimensions\":true"
                                (tumbel-test-call-key call :body)))
        (should (string-match-p
                 (regexp-quote "{\"type\":\"text\",\"text\":\"Caption text. Edited.\"}],\"state\":\"published\",\"tags\":\"cats\"}")
                 (tumbel-test-call-key call :body)))))))
(provide 'tumbel-compose-test)
;;; tumbel-compose-test.el ends here
