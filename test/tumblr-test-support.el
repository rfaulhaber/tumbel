;;; tumblr-test-support.el --- Shared test helpers  -*- lexical-binding: t; -*-

;;; Commentary:

;; Fixture loading and the fake HTTP backend used by the ERT suites.
;; Load it with (require 'tumblr-test-support); the justfile puts
;; test/ on the load path.

;;; Code:

(require 'cl-lib)
(require 'plz)
(require 'seq)
(require 'tumblr-auth)
(require 'tumblr-user)
(require 'tumblr-http)

(defconst tumblr-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding the test files.")

(defun tumblr-test-fixture (name)
  "Return the contents of the fixture file NAME as a string."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name (expand-file-name "fixtures"
                                              tumblr-test-directory)))
    (buffer-string)))

(defvar tumblr-test-calls nil
  "Requests the fake backend received, most recent first.
Each entry is (METHOD URL . KEYS), KEYS being the keyword arguments
the backend was called with.")

(defun tumblr-test-call (n)
  "Return the Nth most recent request the fake backend received."
  (nth n tumblr-test-calls))

(defun tumblr-test-call-key (call key)
  "Return the keyword argument KEY of the recorded CALL."
  (plist-get (cddr call) key))

(defun tumblr-test-call-header (call name)
  "Return the value of the header NAME in the recorded CALL."
  (cdr (assoc name (tumblr-test-call-key call :headers))))

(defvar tumblr-test-defer nil
  "When non-nil, the fake backend queues asynchronous deliveries.
Run them with `tumblr-test-deliver'.")

(defvar tumblr-test-pending nil
  "Queued deliveries of the fake backend, oldest first.")

(defun tumblr-test-deliver ()
  "Run the deliveries the fake backend has queued."
  (while tumblr-test-pending
    (funcall (pop tumblr-test-pending))))

(defun tumblr-test-backend (responses)
  "Return a fake `tumblr-http-backend' serving RESPONSES.
RESPONSES is a list of (REGEXP STATUS BODY [once]): the first entry
whose REGEXP matches the request URL answers it, and an entry ending
in the symbol `once' answers a single request.  STATUS is an HTTP
status number, or the symbol `curl' to simulate a transport failure.
BODY is the response body string.  Callbacks run synchronously,
mirroring what the plz backend delivers, unless `tumblr-test-defer'
is non-nil."
  (let ((responses (copy-sequence responses)))
    (lambda (method url &rest keys)
      (push (cons method (cons url keys)) tumblr-test-calls)
      (let* ((match (or (cl-find-if (lambda (r) (string-match-p (car r) url))
                                    responses)
                        (error "tumblr-test-backend: no response for %s" url)))
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
            (if tumblr-test-defer
                (setq tumblr-test-pending
                      (append tumblr-test-pending (list deliver)))
              (funcall deliver))
            nil))))))

(defmacro tumblr-test-with-backend (responses &rest body)
  "Evaluate BODY with the fake backend serving RESPONSES.
See `tumblr-test-backend'.  `tumblr-test-calls' starts empty."
  (declare (indent 1))
  `(let ((tumblr-http-backend (tumblr-test-backend ,responses))
         (tumblr-test-calls nil)
         (tumblr-test-pending nil))
     ,@body))

(defmacro tumblr-test-logged-out (&rest body)
  "Run BODY with a consumer key configured and no OAuth token."
  (declare (indent 0))
  `(let ((tumblr-consumer-key "KEY")
         (tumblr-consumer-secret "SECRET"))
     (cl-letf (((symbol-function 'tumblr-auth-logged-in-p) (lambda () nil)))
       ,@body)))

(defmacro tumblr-test-logged-in (&rest body)
  "Run BODY with an OAuth access token available.
The account filters are known and empty, so feeds do not fetch them;
bind `tumblr-user--filters' to nil to test that fetch."
  (declare (indent 0))
  `(let ((tumblr-consumer-key "KEY")
         (tumblr-consumer-secret "SECRET")
         (tumblr-user--filters (list nil))
         (tumblr-user--filters-loading nil))
     (cl-letf (((symbol-function 'tumblr-auth-logged-in-p) (lambda () t))
               ((symbol-function 'tumblr-auth-access-token)
                (lambda () "TOKEN")))
       ,@body)))

(defun tumblr-test-posts (fixture)
  "Return the posts of the blog-posts response in FIXTURE."
  (alist-get 'posts
             (alist-get 'response
                        (tumblr-http-parse-json (tumblr-test-fixture fixture)))))

(defun tumblr-test-post (id &optional fixture)
  "Return the post whose id is ID from FIXTURE (posts.json by default)."
  (seq-find (lambda (post) (equal (alist-get 'id_string post) id))
            (tumblr-test-posts (or fixture "posts.json"))))

(defun tumblr-test-post-fidelity (id &optional fixture)
  "Return the post ID from FIXTURE parsed in re-encodable form."
  (let ((posts (alist-get 'posts
                          (alist-get 'response
                                     (tumblr-http-parse-json
                                      (tumblr-test-fixture (or fixture "posts.json"))
                                      t)))))
    (seq-find (lambda (post) (equal (alist-get 'id_string post) id)) posts)))
(defun tumblr-test-envelope (response)
  "Return RESPONSE wrapped in a successful API envelope, as JSON text.
RESPONSE must be encodable: use vectors for arrays, never parsed
lists (see `tumblr-test-post-fidelity')."
  (tumblr-http-encode (list (cons 'meta '((status . 200) (msg . "OK")))
                            (cons 'response response))))
(provide 'tumblr-test-support)
;;; tumblr-test-support.el ends here
