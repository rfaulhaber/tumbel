;;; tumbel-auth-test.el --- Tests for tumbel-auth.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for credential lookup, token storage, the OAuth flow,
;; refreshing and the redirect listener.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-auth)
(require 'tumbel-test-support)

(defconst tumbel-auth-test-token-json
  (concat "{\"access_token\":\"AT1\",\"expires_in\":2520,"
          "\"token_type\":\"bearer\",\"scope\":\"basic write offline_access\","
          "\"refresh_token\":\"RT1\"}")
  "A token endpoint response.")

(defconst tumbel-auth-test-token2-json
  (concat "{\"access_token\":\"AT2\",\"expires_in\":2520,"
          "\"token_type\":\"bearer\",\"scope\":\"basic write offline_access\","
          "\"refresh_token\":\"RT2\"}")
  "A second token endpoint response, with rotated tokens.")

(defconst tumbel-auth-test-rejection-json
  "{\"error\":\"invalid_grant\",\"error_description\":\"Refresh token revoked\"}"
  "The token endpoint refusing a refresh.")

(defmacro tumbel-auth-test-with-auth-source (entries &rest body)
  "Run BODY with `auth-source-search' returning ENTRIES."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'auth-source-search)
              (lambda (&rest _) ,entries)))
     ,@body))

(defmacro tumbel-auth-test-with-state (&rest body)
  "Run BODY with credentials, a private token file and no session."
  (declare (indent 0))
  `(let* ((tumbel-auth-test-dir (make-temp-file "tumbel-auth-test" t))
          (tumbel-token-file (expand-file-name "tokens.eld"
                                               tumbel-auth-test-dir))
          (tumbel-auth--token 'unread)
          (tumbel-auth--refreshing nil)
          (tumbel-auth--waiters nil)
          (tumbel-auth--listener nil)
          (tumbel-consumer-key "KEY")
          (tumbel-consumer-secret "SECRET"))
     (unwind-protect
         (progn ,@body)
       (delete-directory tumbel-auth-test-dir t))))

(defvar tumbel-auth-test-dir nil
  "Temporary directory of the running test.")

(defun tumbel-auth-test-token (expires-at)
  "Return a stored token plist expiring at EXPIRES-AT."
  (list :access-token "AT1" :refresh-token "RT1" :expires-at expires-at
        :scope "basic write offline_access"))

;;;; Credentials

(ert-deftest tumbel-auth-test-key-from-custom ()
  "The customization variables take precedence."
  (let ((tumbel-consumer-key "KEY")
        (tumbel-consumer-secret "SECRET"))
    (tumbel-auth-test-with-auth-source (list (list :user "AKEY" :secret "AS"))
      (should (equal (tumbel-auth-consumer-key) "KEY"))
      (should (equal (tumbel-auth-consumer-credentials) '("KEY" . "SECRET"))))))

(ert-deftest tumbel-auth-test-credentials-from-auth-source ()
  "Auth-source supplies both values, unwrapping a secret function."
  (let ((tumbel-consumer-key nil)
        (tumbel-consumer-secret nil))
    (tumbel-auth-test-with-auth-source (list (list :user "AKEY"
                                                   :secret (lambda () "AS")))
      (should (equal (tumbel-auth-consumer-key) "AKEY"))
      (should (equal (tumbel-auth-consumer-credentials) '("AKEY" . "AS"))))))

(ert-deftest tumbel-auth-test-partial-custom ()
  "A missing secret is filled in from auth-source."
  (let ((tumbel-consumer-key "KEY")
        (tumbel-consumer-secret nil))
    (tumbel-auth-test-with-auth-source (list (list :user "AKEY" :secret "AS"))
      (should (equal (tumbel-auth-consumer-credentials) '("KEY" . "AS"))))))

(ert-deftest tumbel-auth-test-missing-credentials ()
  "Without any source of credentials both lookups signal."
  (let ((tumbel-consumer-key nil)
        (tumbel-consumer-secret nil))
    (tumbel-auth-test-with-auth-source nil
      (should-error (tumbel-auth-consumer-key) :type 'tumbel-auth-error)
      (should-error (tumbel-auth-consumer-credentials)
                    :type 'tumbel-auth-error))))

;;;; Tokens

(ert-deftest tumbel-auth-test-not-logged-in ()
  "Without a token file there is no session."
  (tumbel-auth-test-with-state
    (should-not (tumbel-auth-logged-in-p))
    (should-error (tumbel-auth-access-token) :type 'tumbel-auth-error)))

(ert-deftest tumbel-auth-test-token-file-round-trip ()
  "Tokens are written privately and read back on demand."
  (tumbel-auth-test-with-state
    (let ((token (tumbel-auth-test-token 4102444800)))
      (tumbel-auth--save-token token)
      (should (equal (file-modes tumbel-token-file) #o600))
      (setq tumbel-auth--token 'unread)
      (should (tumbel-auth-logged-in-p))
      (should (equal (tumbel-auth--token) token))
      (should (equal (tumbel-auth-access-token) "AT1")))))

(ert-deftest tumbel-auth-test-unreadable-token-file ()
  "A corrupt token file counts as logged out."
  (tumbel-auth-test-with-state
    (make-directory tumbel-auth-test-dir t)
    (with-temp-file tumbel-token-file (insert "(:access-token"))
    (should-not (tumbel-auth-logged-in-p))))

(ert-deftest tumbel-auth-test-token-from-response ()
  "The expiry is derived from expires_in with a safety margin."
  (let ((token (tumbel-auth--token-from-response
                (tumbel-http-parse-json tumbel-auth-test-token-json) 1000)))
    (should (equal (plist-get token :access-token) "AT1"))
    (should (equal (plist-get token :refresh-token) "RT1"))
    (should (equal (plist-get token :expires-at) 3460))
    (should (tumbel-auth-token-expired-p token 3460))
    (should-not (tumbel-auth-token-expired-p token 3459)))
  (should-error (tumbel-auth--token-from-response '((error . "x")))
                :type 'tumbel-auth-error))

;;;; Authorization

(ert-deftest tumbel-auth-test-authorize-url ()
  "The authorization URL carries every required parameter."
  (let ((tumbel-consumer-key "KEY")
        (url (let ((tumbel-consumer-key "KEY"))
               (tumbel-auth-authorize-url "STATE"))))
    (should (string-prefix-p "https://www.tumblr.com/oauth2/authorize?" url))
    (dolist (part '("client_id=KEY" "response_type=code"
                    "scope=basic%20write%20offline_access" "state=STATE"
                    "redirect_uri=http%3A%2F%2Flocalhost%3A3000%2Fredirect"))
      (should (string-match-p (regexp-quote part) url)))))

(ert-deftest tumbel-auth-test-parse-redirect ()
  "Codes are read from request lines and URLs; refusals signal."
  (should (equal (tumbel-auth-parse-redirect
                  "GET /redirect?code=abc&state=xyz HTTP/1.1\r\nHost: x\r\n\r\n")
                 '("abc" . "xyz")))
  (should (equal (tumbel-auth-parse-redirect
                  "http://localhost:3000/redirect?state=s&code=c#frag")
                 '("c" . "s")))
  (should (null (tumbel-auth-parse-redirect "GET /favicon.ico HTTP/1.1\r\n")))
  (should (null (tumbel-auth-parse-redirect "GET /redirect?state=x HTTP/1.1")))
  (let ((err (should-error (tumbel-auth-parse-redirect
                            (concat "GET /redirect?error=access_denied"
                                    "&error_description=Nope HTTP/1.1"))
                           :type 'tumbel-auth-error)))
    (should (string-match-p "access_denied (Nope)" (cadr err)))))

(ert-deftest tumbel-auth-test-parse-code ()
  "Pasted input may be the bare code or the whole redirect URL."
  (should (equal (tumbel-auth-parse-code " abc \n") "abc"))
  (should (equal (tumbel-auth-parse-code
                  "http://localhost:3000/redirect?code=abc&state=x")
                 "abc"))
  (should-error (tumbel-auth-parse-code "http://localhost:3000/redirect")
                :type 'tumbel-auth-error))

(ert-deftest tumbel-auth-test-manual-login ()
  "Without a listener the code is pasted and exchanged for tokens."
  (tumbel-auth-test-with-state
    (tumbel-test-with-backend `(("/oauth2/token" 200
                                 ,tumbel-auth-test-token-json))
      (let ((tumbel-oauth-use-listener nil)
            (tumbel-auth-login-hook nil)
            opened called hooked)
        (add-hook 'tumbel-auth-login-hook (lambda () (setq hooked t)))
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &rest _) (setq opened url)))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "CODE")))
          (tumbel-login (lambda () (setq called t))))
        (should (string-match-p "client_id=KEY" opened))
        (should (string-match-p "state=[0-9a-f]\\{32\\}" opened))
        (should (tumbel-auth-logged-in-p))
        (should (equal (tumbel-auth-access-token) "AT1"))
        (should called)
        (should hooked)
        (let ((call (tumbel-test-call 0)))
          (should (eq (car call) 'post))
          (should (equal (nth 1 call) tumbel-oauth-token-url))
          (should (equal (tumbel-test-call-header call "Content-Type")
                         "application/x-www-form-urlencoded"))
          (should (equal (tumbel-test-call-key call :body)
                         (concat "grant_type=authorization_code&code=CODE"
                                 "&redirect_uri=http%3A%2F%2Flocalhost%3A3000"
                                 "%2Fredirect&client_id=KEY"
                                 "&client_secret=SECRET"))))))))

(ert-deftest tumbel-auth-test-manual-login-failure ()
  "A refused code is reported without leaving a session behind."
  (tumbel-auth-test-with-state
    (tumbel-test-with-backend `(("/oauth2/token" 400
                                 ,tumbel-auth-test-rejection-json))
      (let ((tumbel-oauth-use-listener nil)
            messages)
        (cl-letf (((symbol-function 'browse-url) #'ignore)
                  ((symbol-function 'read-string) (lambda (&rest _) "BAD"))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (tumbel-login))
        (should-not (tumbel-auth-logged-in-p))
        (should (string-match-p "Refresh token revoked" (car messages)))))))

(ert-deftest tumbel-auth-test-logout ()
  "Logging out removes the token file and runs the hook."
  (tumbel-auth-test-with-state
    (tumbel-auth--save-token (tumbel-auth-test-token 4102444800))
    (let ((tumbel-auth-logout-hook nil)
          hooked)
      (add-hook 'tumbel-auth-logout-hook (lambda () (setq hooked t)))
      (tumbel-logout)
      (should hooked))
    (should-not (tumbel-auth-logged-in-p))
    (should-not (file-exists-p tumbel-token-file))))

;;;; Refreshing

(ert-deftest tumbel-auth-test-expired-token-refreshes ()
  "An expired token is refreshed before it is handed out."
  (tumbel-auth-test-with-state
    (tumbel-auth--save-token (tumbel-auth-test-token 1))
    (tumbel-test-with-backend `(("/oauth2/token" 200
                                 ,tumbel-auth-test-token2-json))
      (should (equal (tumbel-auth-access-token) "AT2"))
      (should (equal (plist-get (tumbel-auth--token) :refresh-token) "RT2"))
      (should (string-match-p "grant_type=refresh_token&refresh_token=RT1&"
                              (tumbel-test-call-key (tumbel-test-call 0)
                                                    :body)))
      (setq tumbel-auth--token 'unread)
      (should (equal (tumbel-auth-access-token) "AT2"))
      (should (equal (length tumbel-test-calls) 1)))))

(ert-deftest tumbel-auth-test-refresh-keeps-refresh-token ()
  "A refresh response without a refresh token keeps the old one."
  (tumbel-auth-test-with-state
    (tumbel-auth--save-token (tumbel-auth-test-token 1))
    (tumbel-test-with-backend '(("/oauth2/token" 200
                                 "{\"access_token\":\"AT2\",\"expires_in\":10}"))
      (should (equal (tumbel-auth-access-token) "AT2"))
      (should (equal (plist-get (tumbel-auth--token) :refresh-token) "RT1")))))

(ert-deftest tumbel-auth-test-refresh-rejected ()
  "A refused refresh ends the session and asks for a new login."
  (tumbel-auth-test-with-state
    (tumbel-auth--save-token (tumbel-auth-test-token 1))
    (tumbel-test-with-backend `(("/oauth2/token" 400
                                 ,tumbel-auth-test-rejection-json))
      (let ((err (should-error (tumbel-auth-access-token)
                               :type 'tumbel-auth-error)))
        (should (string-match-p "Refresh token revoked" (cadr err))))
      (should-not (tumbel-auth-logged-in-p))
      (should-not (file-exists-p tumbel-token-file))
      (should-not tumbel-auth--refreshing))))

(ert-deftest tumbel-auth-test-refresh-transport-error ()
  "A network failure during refresh keeps the session for later."
  (tumbel-auth-test-with-state
    (tumbel-auth--save-token (tumbel-auth-test-token 1))
    (tumbel-test-with-backend '(("/oauth2/token" curl nil))
      (should-error (tumbel-auth-access-token) :type 'tumbel-http-error)
      (should (tumbel-auth-logged-in-p))
      (should (file-exists-p tumbel-token-file))
      (should-not tumbel-auth--refreshing))))

(ert-deftest tumbel-auth-test-async-refresh-single-flight ()
  "Concurrent asynchronous refreshes share one request."
  (tumbel-auth-test-with-state
    (tumbel-auth--save-token (tumbel-auth-test-token 1))
    (tumbel-test-with-backend `(("/oauth2/token" 200
                                 ,tumbel-auth-test-token2-json))
      (let ((tumbel-test-defer t)
            (done 0))
        (tumbel-auth-refresh-async (lambda () (cl-incf done)) #'ignore)
        (tumbel-auth-refresh-async (lambda () (cl-incf done)) #'ignore)
        (should tumbel-auth--refreshing)
        (should (equal (length tumbel-test-calls) 1))
        (tumbel-test-deliver)
        (should (equal done 2))
        (should-not tumbel-auth--refreshing)
        (should (equal (plist-get (tumbel-auth--token) :access-token) "AT2"))))))

(ert-deftest tumbel-auth-test-async-refresh-failure ()
  "A failed asynchronous refresh reaches every waiting ELSE."
  (tumbel-auth-test-with-state
    (tumbel-auth--save-token (tumbel-auth-test-token 1))
    (tumbel-test-with-backend `(("/oauth2/token" 400
                                 ,tumbel-auth-test-rejection-json))
      (let (errors)
        (tumbel-auth-refresh-async #'ignore (lambda (e) (push e errors)))
        (tumbel-auth-refresh-async #'ignore (lambda (e) (push e errors)))
        (should (equal (length errors) 2))
        (should (eq (car (car errors)) 'tumbel-auth-error))
        (should-not (tumbel-auth-logged-in-p))))))

;;;; The redirect listener

(defun tumbel-auth-test-connect (port request)
  "Send REQUEST to the listener on PORT and return the response text."
  (let* ((response "")
         (client (make-network-process
                  :name "tumbel-auth-test-client" :host "127.0.0.1"
                  :service port :coding 'binary :noquery t
                  :filter (lambda (_ data)
                            (setq response (concat response data))))))
    (process-send-string client request)
    (let ((deadline (+ (float-time) 5)))
      (while (and (process-live-p client) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (accept-process-output nil 0.1)
    response))

(defun tumbel-auth-test-start-listener (state callback)
  "Start a listener on a free port for STATE and CALLBACK; skip if refused."
  (condition-case nil
      (tumbel-auth--start-listener t state callback)
    (error (ert-skip "cannot listen on the loopback interface"))))

(ert-deftest tumbel-auth-test-listener-round-trip ()
  "The listener hands the code to the callback and closes."
  (tumbel-auth-test-with-state
    (let* (got
           (server (tumbel-auth-test-start-listener
                    "STATE" (lambda (code) (setq got code))))
           (port (process-contact server :service))
           (response (tumbel-auth-test-connect
                      port
                      (concat "GET /redirect?code=abc&state=STATE HTTP/1.1"
                              "\r\nHost: localhost\r\n\r\n"))))
      (should (string-prefix-p "HTTP/1.1 200 OK\r\n" response))
      (should (string-match-p "Connection: close" response))
      (should (string-match-p "return to Emacs" response))
      (should (equal got "abc"))
      (should-not tumbel-auth--listener)
      (should-not (process-live-p server)))))

(ert-deftest tumbel-auth-test-listener-rejects-state ()
  "A redirect with the wrong state ends the login without a code."
  (tumbel-auth-test-with-state
    (let* (got
           (server (tumbel-auth-test-start-listener
                    "STATE" (lambda (code) (setq got code))))
           (port (process-contact server :service))
           (response (tumbel-auth-test-connect
                      port
                      "GET /redirect?code=abc&state=OTHER HTTP/1.1\r\n\r\n")))
      (should (string-match-p "Login failed" response))
      (should-not got)
      (should-not tumbel-auth--listener))))

(ert-deftest tumbel-auth-test-listener-ignores-other-requests ()
  "Unrelated requests get a 404 and the listener keeps waiting."
  (tumbel-auth-test-with-state
    (let* ((server (tumbel-auth-test-start-listener "STATE" #'ignore))
           (port (process-contact server :service))
           (response (tumbel-auth-test-connect
                      port "GET /favicon.ico HTTP/1.1\r\n\r\n")))
      (should (string-prefix-p "HTTP/1.1 404" response))
      (should tumbel-auth--listener)
      (should (process-live-p server))
      (tumbel-auth--stop-listener)
      (should-not (process-live-p server)))))

(provide 'tumbel-auth-test)
;;; tumbel-auth-test.el ends here
