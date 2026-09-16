;;; tumblr-post-test.el --- Tests for tumblr-post.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for posting an Org buffer or subtree against the fake
;; backend: how the metadata is resolved, the request bodies, the
;; write-back of the post id and the guards.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'tumblr-post)
(require 'tumblr-test-support)

(defconst tumblr-post-test-created
  "{\"meta\":{\"status\":201,\"msg\":\"Created\"},\"response\":{\"id\":\"123\"}}"
  "The response to a created or edited post.")

(defvar tumblr-post-test-messages nil
  "Messages shown during the running test, most recent first.")

(defvar tumblr-post-test-confirm t
  "What the stubbed confirmation prompt answers.")

(defmacro tumblr-post-test-with-session (responses &rest body)
  "Run BODY logged in as example, with RESPONSES served and prompts stubbed."
  (declare (indent 1))
  `(let ((tumblr-user--info nil)
         (tumblr-org-blog-uuid-function (lambda (name) (format "t:%s" name)))
         (tumblr-default-blog nil)
         (tumblr-post-test-messages nil)
         (tumblr-post-test-confirm t))
     (tumblr-test-logged-in
       (tumblr-test-with-backend
           (append ,responses
                   `(("/user/info" 200 ,(tumblr-test-fixture "user-info.json"))))
         (cl-letf (((symbol-function 'message)
                    (lambda (fmt &rest args)
                      (push (apply #'format fmt args)
                            tumblr-post-test-messages)))
                   ((symbol-function 'y-or-n-p)
                    (lambda (&rest _) tumblr-post-test-confirm)))
           (unwind-protect
               (progn ,@body)
             (when (get-buffer "*tumblr: preview*")
               (kill-buffer "*tumblr: preview*"))))))))

(defmacro tumblr-post-test-with-org (text &rest body)
  "Run BODY in an Org buffer holding TEXT, with point at its start."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defun tumblr-post-test-text (text &optional subtype)
  "Return the JSON of a text block of TEXT with SUBTYPE."
  (concat "{\"type\":\"text\",\"text\":\"" text "\""
          (if subtype (concat ",\"subtype\":\"" subtype "\"}") "}")))

(defun tumblr-post-test-post-calls ()
  "Return the requests the fake backend received for posts."
  (seq-filter (lambda (call) (string-match-p "/posts" (nth 1 call)))
              tumblr-test-calls))

;;;; Buffers

(ert-deftest tumblr-post-test-buffer-sends-whole-file ()
  "The buffer is one post: the title leads it and keywords describe it."
  (tumblr-post-test-with-session
      `(("/blog/example/posts" 201 ,tumblr-post-test-created))
    (tumblr-post-test-with-org
        (concat "#+title: Hello\n#+tags: a, b\n#+state: Draft\n\n"
                "Some text.\n\n* Section\n\nMore.")
      (tumblr-post-buffer)
      (let ((call (tumblr-test-call 0)))
        (should (eq (car call) 'post))
        (should (string-suffix-p "/blog/example/posts" (nth 1 call)))
        (should (equal (tumblr-test-call-key call :body)
                       (concat "{\"content\":["
                               (tumblr-post-test-text "Hello" "heading1") ","
                               (tumblr-post-test-text "Some text.") ","
                               (tumblr-post-test-text "Section" "heading2") ","
                               (tumblr-post-test-text "More.")
                               "],\"state\":\"draft\",\"tags\":\"a,b\"}"))))
      (should (equal (car tumblr-post-test-messages)
                     "Posted https://www.tumblr.com/example/123"))
      (should (string-prefix-p
               (concat "#+title: Hello\n#+tags: a, b\n#+state: Draft\n"
                       "#+blog: example\n#+tumblr_id: 123\n\nSome text.")
               (buffer-string))))))

(ert-deftest tumblr-post-test-buffer-edits-recorded-post ()
  "A recorded id turns the next run into an edit of that post."
  (tumblr-post-test-with-session
      `(("/blog/other/posts/123" 200 ,tumblr-post-test-created))
    (let ((text (concat "#+blog: other\n#+filetags: :x:y:\n#+state: queue\n"
                        "#+publish_on: 2026-10-01T10:00:00Z\n"
                        "#+tumblr_id: 123\n\nBody.")))
      (tumblr-post-test-with-org text
        (tumblr-post-buffer)
        (let ((call (tumblr-test-call 0)))
          (should (eq (car call) 'put))
          (should (string-suffix-p "/blog/other/posts/123" (nth 1 call)))
          (should (equal (tumblr-test-call-key call :body)
                         (concat "{\"content\":["
                                 (tumblr-post-test-text "Body.")
                                 "],\"state\":\"queue\",\"tags\":\"x,y\","
                                 "\"publish_on\":\"2026-10-01T10:00:00Z\"}"))))
        (should (equal (car tumblr-post-test-messages)
                       "Edited https://www.tumblr.com/other/123"))
        (should (equal (buffer-string) text))))))

(ert-deftest tumblr-post-test-buffer-preview ()
  "A prefix argument shows the request instead of sending it."
  (tumblr-post-test-with-session nil
    (tumblr-post-test-with-org "#+blog: other\n\nBody."
      (tumblr-post-buffer t)
      (should-not (tumblr-post-test-post-calls))
      (with-current-buffer "*tumblr: preview*"
        (should (string-match-p "Blog: other" (buffer-string)))
        (should (string-match-p (regexp-quote (tumblr-post-test-text "Body."))
                                (buffer-string)))))))

(ert-deftest tumblr-post-test-buffer-guards ()
  "Empty posts, other buffers and declined prompts send nothing."
  (tumblr-post-test-with-session nil
    (tumblr-post-test-with-org "#+blog: other\n"
      (should-error (tumblr-post-buffer) :type 'user-error))
    (with-temp-buffer
      (insert "Plain text")
      (should-error (tumblr-post-buffer) :type 'user-error))
    (let ((compose (tumblr-compose)))
      (unwind-protect
          (with-current-buffer compose
            (should-error (tumblr-post-buffer) :type 'user-error))
        (kill-buffer compose)))
    (tumblr-post-test-with-org "Body."
      (let ((tumblr-post-test-confirm nil))
        (tumblr-post-buffer))
      (should (equal (buffer-string) "Body.")))
    (should-not (tumblr-post-test-post-calls)))
  (tumblr-test-logged-out
    (tumblr-post-test-with-org "Body."
      (should-error (tumblr-post-buffer) :type 'user-error))))

;;;; Subtrees

(defconst tumblr-post-test-outline
  (concat "#+blog: filewide\n#+filetags: :f:\n"
          "* Parent :p:\n:PROPERTIES:\n:TUMBLR_STATE: queue\n:END:\n"
          "** Child :c:\nSCHEDULED: <2026-09-16 Wed>\n"
          ":PROPERTIES:\n:TUMBLR_BLOG: sub\n:END:\nIntro.\n"
          "*** Deep\nDeep text.\n** Sibling\nNot included.\n")
  "An outline whose entries carry metadata at several levels.")

(ert-deftest tumblr-post-test-subtree-sends-entry ()
  "The entry at point is promoted to a post; its metadata is inherited."
  (tumblr-post-test-with-session
      `(("/blog/sub/posts" 201 ,tumblr-post-test-created))
    (tumblr-post-test-with-org tumblr-post-test-outline
      (search-forward "Intro.")
      (tumblr-post-subtree)
      (let ((call (tumblr-test-call 0)))
        (should (eq (car call) 'post))
        (should (string-suffix-p "/blog/sub/posts" (nth 1 call)))
        (should (equal (tumblr-test-call-key call :body)
                       (concat "{\"content\":["
                               (tumblr-post-test-text "Child" "heading1") ","
                               (tumblr-post-test-text "Intro.") ","
                               (tumblr-post-test-text "Deep" "heading2") ","
                               (tumblr-post-test-text "Deep text.")
                               "],\"state\":\"queue\",\"tags\":\"f,p,c\"}"))))
      (should (equal (car tumblr-post-test-messages)
                     "Posted https://www.tumblr.com/sub/123"))
      (should (looking-back "Intro\\." (line-beginning-position)))
      (should (equal (org-entry-get nil "TUMBLR_ID") "123"))
      (should (equal (org-entry-get nil "TUMBLR_BLOG") "sub")))))

(ert-deftest tumblr-post-test-subtree-file-keywords ()
  "Without properties, the file keywords name the blog; nothing else is written."
  (tumblr-post-test-with-session
      `(("/blog/filewide/posts" 201 ,tumblr-post-test-created))
    (tumblr-post-test-with-org tumblr-post-test-outline
      (search-forward "Not included")
      (tumblr-post-subtree)
      (let ((call (tumblr-test-call 0)))
        (should (string-suffix-p "/blog/filewide/posts" (nth 1 call)))
        (should (equal (tumblr-test-call-key call :body)
                       (concat "{\"content\":["
                               (tumblr-post-test-text "Sibling" "heading1") ","
                               (tumblr-post-test-text "Not included.")
                               "],\"state\":\"queue\",\"tags\":\"f,p\"}"))))
      (should (equal (org-entry-get nil "TUMBLR_ID") "123"))
      (should-not (org-entry-get nil "TUMBLR_BLOG")))))

(ert-deftest tumblr-post-test-subtree-records-default-blog ()
  "A post sent to the default blog remembers it, and is edited next time."
  (tumblr-post-test-with-session
      `(("/blog/example/posts/123" 200 ,tumblr-post-test-created)
        ("/blog/example/posts" 201 ,tumblr-post-test-created))
    (tumblr-post-test-with-org "* Note\nText.\n"
      (tumblr-post-subtree)
      (should (eq (car (tumblr-test-call 0)) 'post))
      (should (equal (org-entry-get nil "TUMBLR_ID") "123"))
      (should (equal (org-entry-get nil "TUMBLR_BLOG") "example"))
      (tumblr-post-subtree)
      (let ((call (tumblr-test-call 0)))
        (should (eq (car call) 'put))
        (should (string-suffix-p "/blog/example/posts/123" (nth 1 call)))
        (should (equal (tumblr-test-call-key call :body)
                       (concat "{\"content\":["
                               (tumblr-post-test-text "Note" "heading1") ","
                               (tumblr-post-test-text "Text.")
                               "],\"state\":\"published\"}"))))
      (should (equal (car tumblr-post-test-messages)
                     "Edited https://www.tumblr.com/example/123")))))

(ert-deftest tumblr-post-test-subtree-guards ()
  "Outside any entry, or when the prompt is declined, nothing is sent."
  (tumblr-post-test-with-session nil
    (tumblr-post-test-with-org tumblr-post-test-outline
      (should-error (tumblr-post-subtree) :type 'user-error)
      (search-forward "Intro.")
      (let ((tumblr-post-test-confirm nil))
        (tumblr-post-subtree))
      (should-not (org-entry-get nil "TUMBLR_ID")))
    (should-not (tumblr-post-test-post-calls))))

(provide 'tumblr-post-test)
;;; tumblr-post-test.el ends here
