;;; tumblr-http-test.el --- Tests for tumblr-http.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the HTTP transport layer: JSON handling, request
;; delivery and error conversion, all against the fake backend.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-http)
(require 'tumblr-test-support)

(defconst tumblr-http-test-json
  "{\"a\": [1, 2, {\"b\": null, \"c\": false, \"d\": true}], \"e\": \"x\"}"
  "A document exercising every JSON value type.")

(defun tumblr-http-test--check-parse ()
  "Assert the convenient parse of `tumblr-http-test-json'."
  (let* ((parsed (tumblr-http-parse-json tumblr-http-test-json))
         (a (alist-get 'a parsed))
         (obj (nth 2 a)))
    (should (equal (alist-get 'e parsed) "x"))
    (should (equal (length a) 3))
    (should (equal (nth 0 a) 1))
    (should (null (alist-get 'b obj)))
    (should (null (alist-get 'c obj)))
    (should (eq (alist-get 'd obj) t))))

(ert-deftest tumblr-http-test-parse-json ()
  "JSON parses into alists with lists, and nil for null and false."
  (tumblr-http-test--check-parse))

(ert-deftest tumblr-http-test-parse-json-fallback ()
  "The json.el fallback parses identically."
  (let ((tumblr-http--native-json nil))
    (tumblr-http-test--check-parse)))

(ert-deftest tumblr-http-test-fidelity-round-trip ()
  "The fidelity parse re-encodes to an equivalent document."
  (dolist (native (list t nil))
    (let* ((tumblr-http--native-json native)
           (parsed (tumblr-http-parse-json tumblr-http-test-json t))
           (obj (aref (alist-get 'a parsed) 2)))
      (should (vectorp (alist-get 'a parsed)))
      (should (eq (alist-get 'b obj) :null))
      (should (eq (alist-get 'c obj) :false))
      (should (equal (tumblr-http-parse-json (tumblr-http-encode parsed) t)
                     parsed)))))

(ert-deftest tumblr-http-test-encode ()
  "Encoding uses vectors for arrays and keywords for false and null."
  (dolist (native (list t nil))
    (let ((tumblr-http--native-json native))
      (should (equal (tumblr-http-encode '((content . [])
                                           (state . "draft")
                                           (private . :false)
                                           (date . :null)))
                     (concat "{\"content\":[],\"state\":\"draft\","
                             "\"private\":false,\"date\":null}")))
      (should (equal (tumblr-http-encode nil) "{}"))
      (should (equal (tumblr-http-encode [1 "a" t]) "[1,\"a\",true]"))
      (should (equal (tumblr-http-encode (quote ((a . "é")))) "{\"a\":\"é\"}"))
      (should (multibyte-string-p (tumblr-http-encode (quote ((a . "é")))))))))

(ert-deftest tumblr-http-test-sync-success ()
  "A 2xx response is parsed and returned, with the User-Agent sent."
  (tumblr-test-with-backend `(("/blog/" 200
                               ,(tumblr-test-fixture "blog-info.json")))
    (let ((payload (tumblr-http-request
                    'get "https://api.tumblr.com/v2/blog/example/info"
                    :then 'sync)))
      (should (equal (alist-get 'msg (alist-get 'meta payload)) "OK"))
      (should (equal (tumblr-test-call-header (tumblr-test-call 0)
                                              "User-Agent")
                     tumblr-http-user-agent))
      (should (eq (car (tumblr-test-call 0)) 'get)))))

(ert-deftest tumblr-http-test-sync-api-error ()
  "A 4xx response signals `tumblr-api-error' with the envelope details."
  (tumblr-test-with-backend `(("." 400 ,(tumblr-test-fixture "error-400.json")))
    (let ((err (should-error (tumblr-http-request 'get "https://x/y"
                                                  :then 'sync)
                             :type 'tumblr-api-error)))
      (should (equal (tumblr-api-error-status err) 400))
      (should (equal (tumblr-api-error-message err) "Bad Request"))
      (should (equal (alist-get 'code (car (tumblr-api-error-errors err)))
                     8001))
      (should (equal (tumblr-http-error-string err)
                     (concat "Tumblr API error 400: Bad Request "
                             "(8001: Bad Request: Post content is invalid.)"))))))

(ert-deftest tumblr-http-test-api-error-without-json-body ()
  "A failed status with an unparsable body still signals cleanly."
  (tumblr-test-with-backend '(("." 502 "<html>Bad Gateway</html>"))
    (let ((err (should-error (tumblr-http-request 'get "https://x/y"
                                                  :then 'sync)
                             :type 'tumblr-api-error)))
      (should (equal (tumblr-api-error-status err) 502))
      (should (equal (tumblr-api-error-message err) "HTTP 502"))
      (should (null (tumblr-api-error-errors err))))))

(ert-deftest tumblr-http-test-sync-transport-error ()
  "A transport failure signals `tumblr-http-error'."
  (tumblr-test-with-backend '(("." curl nil))
    (let ((err (should-error (tumblr-http-request 'get "https://x/y"
                                                  :then 'sync)
                             :type 'tumblr-http-error)))
      (should (string-match-p "connect" (error-message-string err))))))

(ert-deftest tumblr-http-test-async-success ()
  "Asynchronous requests deliver the parsed body to THEN."
  (tumblr-test-with-backend `(("." 200 ,(tumblr-test-fixture "blog-info.json")))
    (let (got)
      (tumblr-http-request 'get "https://x/y"
                           :then (lambda (payload) (setq got payload)))
      (should (equal (alist-get 'name
                                (alist-get 'blog (alist-get 'response got)))
                     "example")))))

(ert-deftest tumblr-http-test-async-error ()
  "Asynchronous failures go to ELSE in `condition-case' shape."
  (tumblr-test-with-backend `(("." 429 ,(tumblr-test-fixture "error-429.json")))
    (let (got called)
      (tumblr-http-request 'get "https://x/y"
                           :then (lambda (_) (setq called t))
                           :else (lambda (err) (setq got err)))
      (should-not called)
      (should (eq (car got) 'tumblr-api-error))
      (should (equal (tumblr-api-error-status got) 429))
      (should (equal (tumblr-api-error-message got) "Limit Exceeded")))))

(ert-deftest tumblr-http-test-async-error-without-else ()
  "Without ELSE an asynchronous failure is reported with `message'."
  (tumblr-test-with-backend `(("." 401 ,(tumblr-test-fixture "error-401.json")))
    (let (messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (tumblr-http-request 'get "https://x/y" :then #'ignore))
      (should (string-match-p "401" (car messages))))))

(ert-deftest tumblr-http-test-async-transport-error ()
  "Asynchronous transport failures reach ELSE as `tumblr-http-error'."
  (tumblr-test-with-backend '(("." curl nil))
    (let (got)
      (tumblr-http-request 'get "https://x/y"
                           :then #'ignore
                           :else (lambda (err) (setq got err)))
      (should (eq (car got) 'tumblr-http-error)))))

(ert-deftest tumblr-http-test-as-string-and-binary ()
  "`:as' `string' and `binary' return the body unchanged."
  (tumblr-test-with-backend '(("." 200 "raw bytes"))
    (should (equal (tumblr-http-request 'get "https://x/y"
                                        :as 'string :then 'sync)
                   "raw bytes"))
    (should-not (tumblr-test-call-key (tumblr-test-call 0) :binary))
    (should (equal (tumblr-http-request 'get "https://x/y"
                                        :as 'binary :then 'sync)
                   "raw bytes"))
    (should (eq (tumblr-test-call-key (tumblr-test-call 0) :binary) t))))

(ert-deftest tumblr-http-test-as-function ()
  "`:as' may be a function applied to the body."
  (tumblr-test-with-backend '(("." 200 "abc"))
    (should (equal (tumblr-http-request 'get "https://x/y"
                                        :as #'upcase :then 'sync)
                   "ABC"))))

(ert-deftest tumblr-http-test-empty-body ()
  "An empty JSON body parses to nil."
  (tumblr-test-with-backend '(("." 204 ""))
    (should (null (tumblr-http-request 'delete "https://x/y" :then 'sync)))))

(ert-deftest tumblr-http-test-request-arguments-reach-backend ()
  "Body, body type and timeout are forwarded to the backend."
  (tumblr-test-with-backend '(("." 200 "{}"))
    (tumblr-http-request 'post "https://x/y" :body "payload"
                         :body-type 'binary :timeout 5 :then 'sync)
    (let ((call (tumblr-test-call 0)))
      (should (equal (tumblr-test-call-key call :body) "payload"))
      (should (eq (tumblr-test-call-key call :body-type) 'binary))
      (should (equal (tumblr-test-call-key call :timeout) 5)))))

(provide 'tumblr-http-test)
;;; tumblr-http-test.el ends here
