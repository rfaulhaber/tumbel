;;; tumbel-post-test.el --- Tests for tumbel-post.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for posting an Org buffer or subtree against the fake
;; backend: how the metadata is resolved, the request bodies, the
;; write-back of the post id and the guards.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'tumbel-post)
(require 'tumbel-test-support)

(defconst tumbel-post-test-created
  "{\"meta\":{\"status\":201,\"msg\":\"Created\"},\"response\":{\"id\":\"123\"}}"
  "The response to a created or edited post.")

(defvar tumbel-post-test-messages nil
  "Messages shown during the running test, most recent first.")

(defvar tumbel-post-test-confirm t
  "What the stubbed confirmation prompt answers.")

(defmacro tumbel-post-test-with-session (responses &rest body)
  "Run BODY logged in as example, with RESPONSES served and prompts stubbed."
  (declare (indent 1))
  `(let ((tumbel-user--info nil)
         (tumbel-org-blog-uuid-function (lambda (name) (format "t:%s" name)))
         (tumbel-default-blog nil)
         (tumbel-post-test-messages nil)
         (tumbel-post-test-confirm t))
     (tumbel-test-logged-in
       (tumbel-test-with-backend
           (append ,responses
                   `(("/user/info" 200 ,(tumbel-test-fixture "user-info.json"))))
         (cl-letf (((symbol-function 'message)
                    (lambda (fmt &rest args)
                      (push (apply #'format fmt args)
                            tumbel-post-test-messages)))
                   ((symbol-function 'y-or-n-p)
                    (lambda (&rest _) tumbel-post-test-confirm)))
           (unwind-protect
               (progn ,@body)
             (when (get-buffer "*tumbel: preview*")
               (kill-buffer "*tumbel: preview*"))))))))

(defmacro tumbel-post-test-with-org (text &rest body)
  "Run BODY in an Org buffer holding TEXT, with point at its start."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defun tumbel-post-test-text (text &optional subtype)
  "Return the JSON of a text block of TEXT with SUBTYPE."
  (concat "{\"type\":\"text\",\"text\":\"" text "\""
          (if subtype (concat ",\"subtype\":\"" subtype "\"}") "}")))

(defun tumbel-post-test-post-calls ()
  "Return the requests the fake backend received for posts."
  (seq-filter (lambda (call) (string-match-p "/posts" (nth 1 call)))
              tumbel-test-calls))

;;;; Buffers

(ert-deftest tumbel-post-test-buffer-sends-whole-file ()
  "The buffer is one post: the title leads it and keywords describe it."
  (tumbel-post-test-with-session
      `(("/blog/example/posts" 201 ,tumbel-post-test-created))
    (tumbel-post-test-with-org
        (concat "#+title: Hello\n#+tags: a, b\n#+state: Draft\n\n"
                "Some text.\n\n* Section\n\nMore.")
      (tumbel-post-buffer)
      (let ((call (tumbel-test-call 0)))
        (should (eq (car call) 'post))
        (should (string-suffix-p "/blog/example/posts" (nth 1 call)))
        (should (equal (tumbel-test-call-key call :body)
                       (concat "{\"content\":["
                               (tumbel-post-test-text "Hello" "heading1") ","
                               (tumbel-post-test-text "Some text.") ","
                               (tumbel-post-test-text "Section" "heading2") ","
                               (tumbel-post-test-text "More.")
                               "],\"state\":\"draft\",\"tags\":\"a,b\"}"))))
      (should (equal (car tumbel-post-test-messages)
                     "Posted https://www.tumblr.com/example/123"))
      (should (string-prefix-p
               (concat "#+title: Hello\n#+tags: a, b\n#+state: Draft\n"
                       "#+blog: example\n#+tumblr_id: 123\n\nSome text.")
               (buffer-string))))))

(ert-deftest tumbel-post-test-buffer-edits-recorded-post ()
  "A recorded id turns the next run into an edit of that post."
  (tumbel-post-test-with-session
      `(("/blog/other/posts/123" 200 ,tumbel-post-test-created))
    (let ((text (concat "#+blog: other\n#+filetags: :x:y:\n#+state: queue\n"
                        "#+publish_on: 2026-10-01T10:00:00Z\n"
                        "#+tumblr_id: 123\n\nBody.")))
      (tumbel-post-test-with-org text
        (tumbel-post-buffer)
        (let ((call (tumbel-test-call 0)))
          (should (eq (car call) 'put))
          (should (string-suffix-p "/blog/other/posts/123" (nth 1 call)))
          (should (equal (tumbel-test-call-key call :body)
                         (concat "{\"content\":["
                                 (tumbel-post-test-text "Body.")
                                 "],\"state\":\"queue\",\"tags\":\"x,y\","
                                 "\"publish_on\":\"2026-10-01T10:00:00Z\"}"))))
        (should (equal (car tumbel-post-test-messages)
                       "Edited https://www.tumblr.com/other/123"))
        (should (equal (buffer-string) text))))))

(ert-deftest tumbel-post-test-buffer-preview ()
  "A prefix argument shows the request instead of sending it."
  (tumbel-post-test-with-session nil
    (tumbel-post-test-with-org "#+blog: other\n\nBody."
      (tumbel-post-buffer t)
      (should-not (tumbel-post-test-post-calls))
      (with-current-buffer "*tumbel: preview*"
        (should (string-match-p "Blog: other" (buffer-string)))
        (should (string-match-p (regexp-quote (tumbel-post-test-text "Body."))
                                (buffer-string)))))))

(ert-deftest tumbel-post-test-buffer-guards ()
  "Empty posts, other buffers and declined prompts send nothing."
  (tumbel-post-test-with-session nil
    (tumbel-post-test-with-org "#+blog: other\n"
      (should-error (tumbel-post-buffer) :type 'user-error))
    (with-temp-buffer
      (insert "Plain text")
      (should-error (tumbel-post-buffer) :type 'user-error))
    (let ((compose (tumbel-compose)))
      (unwind-protect
          (with-current-buffer compose
            (should-error (tumbel-post-buffer) :type 'user-error))
        (kill-buffer compose)))
    (tumbel-post-test-with-org "Body."
      (let ((tumbel-post-test-confirm nil))
        (tumbel-post-buffer))
      (should (equal (buffer-string) "Body.")))
    (should-not (tumbel-post-test-post-calls)))
  (tumbel-test-logged-out
    (tumbel-post-test-with-org "Body."
      (should-error (tumbel-post-buffer) :type 'user-error))))

;;;; Subtrees

(defconst tumbel-post-test-outline
  (concat "#+blog: filewide\n#+filetags: :f:\n"
          "* Parent :p:\n:PROPERTIES:\n:TUMBLR_STATE: queue\n:END:\n"
          "** Child :c:\nSCHEDULED: <2026-09-16 Wed>\n"
          ":PROPERTIES:\n:TUMBLR_BLOG: sub\n:END:\nIntro.\n"
          "*** Deep\nDeep text.\n** Sibling\nNot included.\n")
  "An outline whose entries carry metadata at several levels.")

(ert-deftest tumbel-post-test-subtree-sends-entry ()
  "The entry at point is promoted to a post; its metadata is inherited."
  (tumbel-post-test-with-session
      `(("/blog/sub/posts" 201 ,tumbel-post-test-created))
    (tumbel-post-test-with-org tumbel-post-test-outline
      (search-forward "Intro.")
      (tumbel-post-subtree)
      (let ((call (tumbel-test-call 0)))
        (should (eq (car call) 'post))
        (should (string-suffix-p "/blog/sub/posts" (nth 1 call)))
        (should (equal (tumbel-test-call-key call :body)
                       (concat "{\"content\":["
                               (tumbel-post-test-text "Child" "heading1") ","
                               (tumbel-post-test-text "Intro.") ","
                               (tumbel-post-test-text "Deep" "heading2") ","
                               (tumbel-post-test-text "Deep text.")
                               "],\"state\":\"queue\",\"tags\":\"f,p,c\"}"))))
      (should (equal (car tumbel-post-test-messages)
                     "Posted https://www.tumblr.com/sub/123"))
      (should (looking-back "Intro\\." (line-beginning-position)))
      (should (equal (org-entry-get nil "TUMBLR_ID") "123"))
      (should (equal (org-entry-get nil "TUMBLR_BLOG") "sub")))))

(ert-deftest tumbel-post-test-subtree-file-keywords ()
  "Without properties, the file keywords name the blog; nothing else is written."
  (tumbel-post-test-with-session
      `(("/blog/filewide/posts" 201 ,tumbel-post-test-created))
    (tumbel-post-test-with-org tumbel-post-test-outline
      (search-forward "Not included")
      (tumbel-post-subtree)
      (let ((call (tumbel-test-call 0)))
        (should (string-suffix-p "/blog/filewide/posts" (nth 1 call)))
        (should (equal (tumbel-test-call-key call :body)
                       (concat "{\"content\":["
                               (tumbel-post-test-text "Sibling" "heading1") ","
                               (tumbel-post-test-text "Not included.")
                               "],\"state\":\"queue\",\"tags\":\"f,p\"}"))))
      (should (equal (org-entry-get nil "TUMBLR_ID") "123"))
      (should-not (org-entry-get nil "TUMBLR_BLOG")))))

(ert-deftest tumbel-post-test-subtree-records-default-blog ()
  "A post sent to the default blog remembers it, and is edited next time."
  (tumbel-post-test-with-session
      `(("/blog/example/posts/123" 200 ,tumbel-post-test-created)
        ("/blog/example/posts" 201 ,tumbel-post-test-created))
    (tumbel-post-test-with-org "* Note\nText.\n"
      (tumbel-post-subtree)
      (should (eq (car (tumbel-test-call 0)) 'post))
      (should (equal (org-entry-get nil "TUMBLR_ID") "123"))
      (should (equal (org-entry-get nil "TUMBLR_BLOG") "example"))
      (tumbel-post-subtree)
      (let ((call (tumbel-test-call 0)))
        (should (eq (car call) 'put))
        (should (string-suffix-p "/blog/example/posts/123" (nth 1 call)))
        (should (equal (tumbel-test-call-key call :body)
                       (concat "{\"content\":["
                               (tumbel-post-test-text "Note" "heading1") ","
                               (tumbel-post-test-text "Text.")
                               "],\"state\":\"published\"}"))))
      (should (equal (car tumbel-post-test-messages)
                     "Edited https://www.tumblr.com/example/123")))))

(ert-deftest tumbel-post-test-subtree-guards ()
  "Outside any entry, or when the prompt is declined, nothing is sent."
  (tumbel-post-test-with-session nil
    (tumbel-post-test-with-org tumbel-post-test-outline
      (should-error (tumbel-post-subtree) :type 'user-error)
      (search-forward "Intro.")
      (let ((tumbel-post-test-confirm nil))
        (tumbel-post-subtree))
      (should-not (org-entry-get nil "TUMBLR_ID")))
    (should-not (tumbel-post-test-post-calls))))

(provide 'tumbel-post-test)
;;; tumbel-post-test.el ends here
