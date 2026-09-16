;;; tumbel-test-support.el --- Shared test helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Fixture loading and the fake HTTP backend used by the ERT suites.
;; Load it with (require 'tumbel-test-support); the justfile puts
;; test/ on the load path.

;;; Code:

(require 'cl-lib)
(require 'plz)
(require 'seq)
(require 'tumbel-auth)
(require 'tumbel-user)
(require 'tumbel-http)

(defconst tumbel-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding the test files.")

(defun tumbel-test-fixture (name)
  "Return the contents of the fixture file NAME as a string."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name (expand-file-name "fixtures"
                                              tumbel-test-directory)))
    (buffer-string)))

(defvar tumbel-test-calls nil
  "Requests the fake backend received, most recent first.
Each entry is (METHOD URL . KEYS), KEYS being the keyword arguments
the backend was called with.")

(defun tumbel-test-call (n)
  "Return the Nth most recent request the fake backend received."
  (nth n tumbel-test-calls))

(defun tumbel-test-call-key (call key)
  "Return the keyword argument KEY of the recorded CALL."
  (plist-get (cddr call) key))

(defun tumbel-test-call-header (call name)
  "Return the value of the header NAME in the recorded CALL."
  (cdr (assoc name (tumbel-test-call-key call :headers))))

(defvar tumbel-test-defer nil
  "When non-nil, the fake backend queues asynchronous deliveries.
Run them with `tumbel-test-deliver'.")

(defvar tumbel-test-pending nil
  "Queued deliveries of the fake backend, oldest first.")

(defun tumbel-test-deliver ()
  "Run the deliveries the fake backend has queued."
  (while tumbel-test-pending
    (funcall (pop tumbel-test-pending))))

(defun tumbel-test-backend (responses)
  "Return a fake `tumbel-http-backend' serving RESPONSES.
RESPONSES is a list of (REGEXP STATUS BODY [once]): the first entry
whose REGEXP matches the request URL answers it, and an entry ending
in the symbol `once' answers a single request.  STATUS is an HTTP
status number, or the symbol `curl' to simulate a transport failure.
BODY is the response body string.  Callbacks run synchronously,
mirroring what the plz backend delivers, unless `tumbel-test-defer'
is non-nil."
  (let ((responses (copy-sequence responses)))
    (lambda (method url &rest keys)
      (push (cons method (cons url keys)) tumbel-test-calls)
      (let* ((match (or (cl-find-if (lambda (r) (string-match-p (car r) url))
                                    responses)
                        (error "tumbel-test-backend: no response for %s" url)))
             (status (nth 1 match))
             (result (cond
                      ((eq status 'curl)
                       (make-plz-error
                        :curl-error (cons 7 "Failed to connect to host.")))
                      ((<= 200 status 299)
                       (make-plz-response :status status :body (nth 2 match)))
                      (t
                       (make-plz-error
                        :response (make-plz-response :status status
                                                     :body (nth 2 match))))))
             (then (plist-get keys :then))
             (else (plist-get keys :else)))
        (when (eq (nth 3 match) 'once)
          (setq responses (delq match responses)))
        (if (eq then 'sync)
            result
          (let ((deliver (lambda ()
                           (if (plz-error-p result)
                               (funcall else result)
                             (funcall then result)))))
            (if tumbel-test-defer
                (setq tumbel-test-pending
                      (append tumbel-test-pending (list deliver)))
              (funcall deliver))
            nil))))))

(defmacro tumbel-test-with-backend (responses &rest body)
  "Evaluate BODY with the fake backend serving RESPONSES.
See `tumbel-test-backend'.  `tumbel-test-calls' starts empty."
  (declare (indent 1))
  `(let ((tumbel-http-backend (tumbel-test-backend ,responses))
         (tumbel-test-calls nil)
         (tumbel-test-pending nil))
     ,@body))

(defmacro tumbel-test-logged-out (&rest body)
  "Run BODY with a consumer key configured and no OAuth token."
  (declare (indent 0))
  `(let ((tumbel-consumer-key "KEY")
         (tumbel-consumer-secret "SECRET"))
     (cl-letf (((symbol-function 'tumbel-auth-logged-in-p) (lambda () nil)))
       ,@body)))

(defmacro tumbel-test-logged-in (&rest body)
  "Run BODY with an OAuth access token available.
The account filters are known and empty, so feeds do not fetch them;
bind `tumbel-user--filters' to nil to test that fetch."
  (declare (indent 0))
  `(let ((tumbel-consumer-key "KEY")
         (tumbel-consumer-secret "SECRET")
         (tumbel-user--filters (list nil))
         (tumbel-user--filters-loading nil))
     (cl-letf (((symbol-function 'tumbel-auth-logged-in-p) (lambda () t))
               ((symbol-function 'tumbel-auth-access-token)
                (lambda () "TOKEN")))
       ,@body)))

(defun tumbel-test-posts (fixture)
  "Return the posts of the blog-posts response in FIXTURE."
  (alist-get 'posts
             (alist-get 'response
                        (tumbel-http-parse-json (tumbel-test-fixture fixture)))))

(defun tumbel-test-post (id &optional fixture)
  "Return the post whose id is ID from FIXTURE (posts.json by default)."
  (seq-find (lambda (post) (equal (alist-get 'id_string post) id))
            (tumbel-test-posts (or fixture "posts.json"))))

(defun tumbel-test-post-fidelity (id &optional fixture)
  "Return the post ID from FIXTURE parsed in re-encodable form."
  (let ((posts (alist-get 'posts
                          (alist-get 'response
                                     (tumbel-http-parse-json
                                      (tumbel-test-fixture (or fixture "posts.json"))
                                      t)))))
    (seq-find (lambda (post) (equal (alist-get 'id_string post) id)) posts)))
(defun tumbel-test-envelope (response)
  "Return RESPONSE wrapped in a successful API envelope, as JSON text.
RESPONSE must be encodable: use vectors for arrays, never parsed
lists (see `tumbel-test-post-fidelity')."
  (tumbel-http-encode (list (cons 'meta '((status . 200) (msg . "OK")))
                            (cons 'response response))))
(provide 'tumbel-test-support)
;;; tumbel-test-support.el ends here
