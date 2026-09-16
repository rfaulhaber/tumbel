;;; tumbel-http-test.el --- Tests for tumbel-http.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the HTTP transport layer: JSON handling, request
;; delivery and error conversion, all against the fake backend.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-http)
(require 'tumbel-test-support)

(defconst tumbel-http-test-json
  "{\"a\": [1, 2, {\"b\": null, \"c\": false, \"d\": true}], \"e\": \"x\"}"
  "A document exercising every JSON value type.")

(defun tumbel-http-test--check-parse ()
  "Assert the convenient parse of `tumbel-http-test-json'."
  (let* ((parsed (tumbel-http-parse-json tumbel-http-test-json))
         (a (alist-get 'a parsed))
         (obj (nth 2 a)))
    (should (equal (alist-get 'e parsed) "x"))
    (should (equal (length a) 3))
    (should (equal (nth 0 a) 1))
    (should (null (alist-get 'b obj)))
    (should (null (alist-get 'c obj)))
    (should (eq (alist-get 'd obj) t))))

(ert-deftest tumbel-http-test-parse-json ()
  "JSON parses into alists with lists, and nil for null and false."
  (tumbel-http-test--check-parse))

(ert-deftest tumbel-http-test-parse-json-fallback ()
  "The json.el fallback parses identically."
  (let ((tumbel-http--native-json nil))
    (tumbel-http-test--check-parse)))

(ert-deftest tumbel-http-test-fidelity-round-trip ()
  "The fidelity parse re-encodes to an equivalent document."
  (dolist (native (list t nil))
    (let* ((tumbel-http--native-json native)
           (parsed (tumbel-http-parse-json tumbel-http-test-json t))
           (obj (aref (alist-get 'a parsed) 2)))
      (should (vectorp (alist-get 'a parsed)))
      (should (eq (alist-get 'b obj) :null))
      (should (eq (alist-get 'c obj) :false))
      (should (equal (tumbel-http-parse-json (tumbel-http-encode parsed) t)
                     parsed)))))

(ert-deftest tumbel-http-test-encode ()
  "Encoding uses vectors for arrays and keywords for false and null."
  (dolist (native (list t nil))
    (let ((tumbel-http--native-json native))
      (should (equal (tumbel-http-encode '((content . [])
                                           (state . "draft")
                                           (private . :false)
                                           (date . :null)))
                     (concat "{\"content\":[],\"state\":\"draft\","
                             "\"private\":false,\"date\":null}")))
      (should (equal (tumbel-http-encode nil) "{}"))
      (should (equal (tumbel-http-encode [1 "a" t]) "[1,\"a\",true]"))
      (should (equal (tumbel-http-encode (quote ((a . "é")))) "{\"a\":\"é\"}"))
      (should (multibyte-string-p (tumbel-http-encode (quote ((a . "é")))))))))

(ert-deftest tumbel-http-test-sync-success ()
  "A 2xx response is parsed and returned, with the User-Agent sent."
  (tumbel-test-with-backend `(("/blog/" 200
                               ,(tumbel-test-fixture "blog-info.json")))
    (let ((payload (tumbel-http-request
                    'get "https://api.tumblr.com/v2/blog/example/info"
                    :then 'sync)))
      (should (equal (alist-get 'msg (alist-get 'meta payload)) "OK"))
      (should (equal (tumbel-test-call-header (tumbel-test-call 0)
                                              "User-Agent")
                     tumbel-http-user-agent))
      (should (eq (car (tumbel-test-call 0)) 'get)))))

(ert-deftest tumbel-http-test-sync-api-error ()
  "A 4xx response signals `tumbel-api-error' with the envelope details."
  (tumbel-test-with-backend `(("." 400 ,(tumbel-test-fixture "error-400.json")))
    (let ((err (should-error (tumbel-http-request 'get "https://x/y"
                                                  :then 'sync)
                             :type 'tumbel-api-error)))
      (should (equal (tumbel-api-error-status err) 400))
      (should (equal (tumbel-api-error-message err) "Bad Request"))
      (should (equal (alist-get 'code (car (tumbel-api-error-errors err)))
                     8001))
      (should (equal (tumbel-http-error-string err)
                     (concat "Tumblr API error 400: Bad Request "
                             "(8001: Bad Request: Post content is invalid.)"))))))

(ert-deftest tumbel-http-test-api-error-without-json-body ()
  "A failed status with an unparsable body still signals cleanly."
  (tumbel-test-with-backend '(("." 502 "<html>Bad Gateway</html>"))
    (let ((err (should-error (tumbel-http-request 'get "https://x/y"
                                                  :then 'sync)
                             :type 'tumbel-api-error)))
      (should (equal (tumbel-api-error-status err) 502))
      (should (equal (tumbel-api-error-message err) "HTTP 502"))
      (should (null (tumbel-api-error-errors err))))))

(ert-deftest tumbel-http-test-sync-transport-error ()
  "A transport failure signals `tumbel-http-error'."
  (tumbel-test-with-backend '(("." curl nil))
    (let ((err (should-error (tumbel-http-request 'get "https://x/y"
                                                  :then 'sync)
                             :type 'tumbel-http-error)))
      (should (string-match-p "connect" (error-message-string err))))))

(ert-deftest tumbel-http-test-async-success ()
  "Asynchronous requests deliver the parsed body to THEN."
  (tumbel-test-with-backend `(("." 200 ,(tumbel-test-fixture "blog-info.json")))
    (let (got)
      (tumbel-http-request 'get "https://x/y"
                           :then (lambda (payload) (setq got payload)))
      (should (equal (alist-get 'name
                                (alist-get 'blog (alist-get 'response got)))
                     "example")))))

(ert-deftest tumbel-http-test-async-error ()
  "Asynchronous failures go to ELSE in `condition-case' shape."
  (tumbel-test-with-backend `(("." 429 ,(tumbel-test-fixture "error-429.json")))
    (let (got called)
      (tumbel-http-request 'get "https://x/y"
                           :then (lambda (_) (setq called t))
                           :else (lambda (err) (setq got err)))
      (should-not called)
      (should (eq (car got) 'tumbel-api-error))
      (should (equal (tumbel-api-error-status got) 429))
      (should (equal (tumbel-api-error-message got) "Limit Exceeded")))))

(ert-deftest tumbel-http-test-async-error-without-else ()
  "Without ELSE an asynchronous failure is reported with `message'."
  (tumbel-test-with-backend `(("." 401 ,(tumbel-test-fixture "error-401.json")))
    (let (messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) messages))))
        (tumbel-http-request 'get "https://x/y" :then #'ignore))
      (should (string-match-p "401" (car messages))))))

(ert-deftest tumbel-http-test-async-transport-error ()
  "Asynchronous transport failures reach ELSE as `tumbel-http-error'."
  (tumbel-test-with-backend '(("." curl nil))
    (let (got)
      (tumbel-http-request 'get "https://x/y"
                           :then #'ignore
                           :else (lambda (err) (setq got err)))
      (should (eq (car got) 'tumbel-http-error)))))

(ert-deftest tumbel-http-test-as-string-and-binary ()
  "`:as' `string' and `binary' return the body unchanged."
  (tumbel-test-with-backend '(("." 200 "raw bytes"))
    (should (equal (tumbel-http-request 'get "https://x/y"
                                        :as 'string :then 'sync)
                   "raw bytes"))
    (should-not (tumbel-test-call-key (tumbel-test-call 0) :binary))
    (should (equal (tumbel-http-request 'get "https://x/y"
                                        :as 'binary :then 'sync)
                   "raw bytes"))
    (should (eq (tumbel-test-call-key (tumbel-test-call 0) :binary) t))))

(ert-deftest tumbel-http-test-binary-body-is-unibyte ()
  "A binary body is delivered as bytes, not as raw-byte characters.
plz hands back the undecoded body of a multibyte process buffer, whose
eight-bit characters image loaders cannot read."
  (let ((jpeg-header (string-to-multibyte "\377\330\377\340")))
    (should (multibyte-string-p jpeg-header))
    (tumbel-test-with-backend `(("." 200 ,jpeg-header))
      (let ((body (tumbel-http-request 'get "https://x/y"
                                       :as 'binary :then 'sync)))
        (should-not (multibyte-string-p body))
        (should (equal body "\377\330\377\340")))
      (let (delivered)
        (tumbel-http-request 'get "https://x/y" :as 'binary
                             :then (lambda (body) (setq delivered body)))
        (should-not (multibyte-string-p delivered))))))

(ert-deftest tumbel-http-test-as-function ()
  "`:as' may be a function applied to the body."
  (tumbel-test-with-backend '(("." 200 "abc"))
    (should (equal (tumbel-http-request 'get "https://x/y"
                                        :as #'upcase :then 'sync)
                   "ABC"))))

(ert-deftest tumbel-http-test-empty-body ()
  "An empty JSON body parses to nil."
  (tumbel-test-with-backend '(("." 204 ""))
    (should (null (tumbel-http-request 'delete "https://x/y" :then 'sync)))))

(ert-deftest tumbel-http-test-request-arguments-reach-backend ()
  "Body, body type and timeout are forwarded to the backend."
  (tumbel-test-with-backend '(("." 200 "{}"))
    (tumbel-http-request 'post "https://x/y" :body "payload"
                         :body-type 'binary :timeout 5 :then 'sync)
    (let ((call (tumbel-test-call 0)))
      (should (equal (tumbel-test-call-key call :body) "payload"))
      (should (eq (tumbel-test-call-key call :body-type) 'binary))
      (should (equal (tumbel-test-call-key call :timeout) 5)))))

(provide 'tumbel-http-test)
;;; tumbel-http-test.el ends here
