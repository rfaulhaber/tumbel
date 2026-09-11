;;; tumblr-auth-test.el --- Tests for tumblr-auth.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for credential lookup, token storage, the OAuth flow,
;; refreshing and the redirect listener.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-auth)
(require 'tumblr-test-support)

(defconst tumblr-auth-test-token-json
  (concat "{\"access_token\":\"AT1\",\"expires_in\":2520,"
          "\"token_type\":\"bearer\",\"scope\":\"basic write offline_access\","
          "\"refresh_token\":\"RT1\"}")
  "A token endpoint response.")

(defconst tumblr-auth-test-token2-json
  (concat "{\"access_token\":\"AT2\",\"expires_in\":2520,"
          "\"token_type\":\"bearer\",\"scope\":\"basic write offline_access\","
          "\"refresh_token\":\"RT2\"}")
  "A second token endpoint response, with rotated tokens.")

(defconst tumblr-auth-test-rejection-json
  "{\"error\":\"invalid_grant\",\"error_description\":\"Refresh token revoked\"}"
  "The token endpoint refusing a refresh.")

(defmacro tumblr-auth-test-with-auth-source (entries &rest body)
  "Run BODY with `auth-source-search' returning ENTRIES."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'auth-source-search)
              (lambda (&rest _) ,entries)))
     ,@body))

(defmacro tumblr-auth-test-with-state (&rest body)
  "Run BODY with credentials, a private token file and no session."
  (declare (indent 0))
  `(let* ((tumblr-auth-test-dir (make-temp-file "tumblr-auth-test" t))
          (tumblr-token-file (expand-file-name "tokens.eld"
                                               tumblr-auth-test-dir))
          (tumblr-auth--token 'unread)
          (tumblr-auth--refreshing nil)
          (tumblr-auth--waiters nil)
          (tumblr-auth--listener nil)
          (tumblr-consumer-key "KEY")
          (tumblr-consumer-secret "SECRET"))
     (unwind-protect
         (progn ,@body)
       (delete-directory tumblr-auth-test-dir t))))

(defvar tumblr-auth-test-dir nil
  "Temporary directory of the running test.")

(defun tumblr-auth-test-token (expires-at)
  "Return a stored token plist expiring at EXPIRES-AT."
  (list :access-token "AT1" :refresh-token "RT1" :expires-at expires-at
        :scope "basic write offline_access"))

;;;; Credentials

(ert-deftest tumblr-auth-test-key-from-custom ()
  "The customization variables take precedence."
  (let ((tumblr-consumer-key "KEY")
        (tumblr-consumer-secret "SECRET"))
    (tumblr-auth-test-with-auth-source (list (list :user "AKEY" :secret "AS"))
      (should (equal (tumblr-auth-consumer-key) "KEY"))
      (should (equal (tumblr-auth-consumer-credentials) '("KEY" . "SECRET"))))))

(ert-deftest tumblr-auth-test-credentials-from-auth-source ()
  "Auth-source supplies both values, unwrapping a secret function."
  (let ((tumblr-consumer-key nil)
        (tumblr-consumer-secret nil))
    (tumblr-auth-test-with-auth-source (list (list :user "AKEY"
                                                   :secret (lambda () "AS")))
      (should (equal (tumblr-auth-consumer-key) "AKEY"))
      (should (equal (tumblr-auth-consumer-credentials) '("AKEY" . "AS"))))))

(ert-deftest tumblr-auth-test-partial-custom ()
  "A missing secret is filled in from auth-source."
  (let ((tumblr-consumer-key "KEY")
        (tumblr-consumer-secret nil))
    (tumblr-auth-test-with-auth-source (list (list :user "AKEY" :secret "AS"))
      (should (equal (tumblr-auth-consumer-credentials) '("KEY" . "AS"))))))

(ert-deftest tumblr-auth-test-missing-credentials ()
  "Without any source of credentials both lookups signal."
  (let ((tumblr-consumer-key nil)
        (tumblr-consumer-secret nil))
    (tumblr-auth-test-with-auth-source nil
      (should-error (tumblr-auth-consumer-key) :type 'tumblr-auth-error)
      (should-error (tumblr-auth-consumer-credentials)
                    :type 'tumblr-auth-error))))

;;;; Tokens

(ert-deftest tumblr-auth-test-not-logged-in ()
  "Without a token file there is no session."
  (tumblr-auth-test-with-state
    (should-not (tumblr-auth-logged-in-p))
    (should-error (tumblr-auth-access-token) :type 'tumblr-auth-error)))

(ert-deftest tumblr-auth-test-token-file-round-trip ()
  "Tokens are written privately and read back on demand."
  (tumblr-auth-test-with-state
    (let ((token (tumblr-auth-test-token 4102444800)))
      (tumblr-auth--save-token token)
      (should (equal (file-modes tumblr-token-file) #o600))
      (setq tumblr-auth--token 'unread)
      (should (tumblr-auth-logged-in-p))
      (should (equal (tumblr-auth--token) token))
      (should (equal (tumblr-auth-access-token) "AT1")))))

(ert-deftest tumblr-auth-test-unreadable-token-file ()
  "A corrupt token file counts as logged out."
  (tumblr-auth-test-with-state
    (make-directory tumblr-auth-test-dir t)
    (with-temp-file tumblr-token-file (insert "(:access-token"))
    (should-not (tumblr-auth-logged-in-p))))

(ert-deftest tumblr-auth-test-token-from-response ()
  "The expiry is derived from expires_in with a safety margin."
  (let ((token (tumblr-auth--token-from-response
                (tumblr-http-parse-json tumblr-auth-test-token-json) 1000)))
    (should (equal (plist-get token :access-token) "AT1"))
    (should (equal (plist-get token :refresh-token) "RT1"))
    (should (equal (plist-get token :expires-at) 3460))
    (should (tumblr-auth-token-expired-p token 3460))
    (should-not (tumblr-auth-token-expired-p token 3459)))
  (should-error (tumblr-auth--token-from-response '((error . "x")))
                :type 'tumblr-auth-error))

;;;; Authorization

(ert-deftest tumblr-auth-test-authorize-url ()
  "The authorization URL carries every required parameter."
  (let ((tumblr-consumer-key "KEY")
        (url (let ((tumblr-consumer-key "KEY"))
               (tumblr-auth-authorize-url "STATE"))))
    (should (string-prefix-p "https://www.tumblr.com/oauth2/authorize?" url))
    (dolist (part '("client_id=KEY" "response_type=code"
                    "scope=basic%20write%20offline_access" "state=STATE"
                    "redirect_uri=http%3A%2F%2Flocalhost%3A3000%2Fredirect"))
      (should (string-match-p (regexp-quote part) url)))))

(ert-deftest tumblr-auth-test-parse-redirect ()
  "Codes are read from request lines and URLs; refusals signal."
  (should (equal (tumblr-auth-parse-redirect
                  "GET /redirect?code=abc&state=xyz HTTP/1.1\r\nHost: x\r\n\r\n")
                 '("abc" . "xyz")))
  (should (equal (tumblr-auth-parse-redirect
                  "http://localhost:3000/redirect?state=s&code=c#frag")
                 '("c" . "s")))
  (should (null (tumblr-auth-parse-redirect "GET /favicon.ico HTTP/1.1\r\n")))
  (should (null (tumblr-auth-parse-redirect "GET /redirect?state=x HTTP/1.1")))
  (let ((err (should-error (tumblr-auth-parse-redirect
                            (concat "GET /redirect?error=access_denied"
                                    "&error_description=Nope HTTP/1.1"))
                           :type 'tumblr-auth-error)))
    (should (string-match-p "access_denied (Nope)" (cadr err)))))

(ert-deftest tumblr-auth-test-parse-code ()
  "Pasted input may be the bare code or the whole redirect URL."
  (should (equal (tumblr-auth-parse-code " abc \n") "abc"))
  (should (equal (tumblr-auth-parse-code
                  "http://localhost:3000/redirect?code=abc&state=x")
                 "abc"))
  (should-error (tumblr-auth-parse-code "http://localhost:3000/redirect")
                :type 'tumblr-auth-error))

(ert-deftest tumblr-auth-test-manual-login ()
  "Without a listener the code is pasted and exchanged for tokens."
  (tumblr-auth-test-with-state
    (tumblr-test-with-backend `(("/oauth2/token" 200
                                 ,tumblr-auth-test-token-json))
      (let ((tumblr-oauth-use-listener nil)
            (tumblr-auth-login-hook nil)
            opened called hooked)
        (add-hook 'tumblr-auth-login-hook (lambda () (setq hooked t)))
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &rest _) (setq opened url)))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "CODE")))
          (tumblr-login (lambda () (setq called t))))
        (should (string-match-p "client_id=KEY" opened))
        (should (string-match-p "state=[0-9a-f]\\{32\\}" opened))
        (should (tumblr-auth-logged-in-p))
        (should (equal (tumblr-auth-access-token) "AT1"))
        (should called)
        (should hooked)
        (let ((call (tumblr-test-call 0)))
          (should (eq (car call) 'post))
          (should (equal (nth 1 call) tumblr-oauth-token-url))
          (should (equal (tumblr-test-call-header call "Content-Type")
                         "application/x-www-form-urlencoded"))
          (should (equal (tumblr-test-call-key call :body)
                         (concat "grant_type=authorization_code&code=CODE"
                                 "&redirect_uri=http%3A%2F%2Flocalhost%3A3000"
                                 "%2Fredirect&client_id=KEY"
                                 "&client_secret=SECRET"))))))))

(ert-deftest tumblr-auth-test-manual-login-failure ()
  "A refused code is reported without leaving a session behind."
  (tumblr-auth-test-with-state
    (tumblr-test-with-backend `(("/oauth2/token" 400
                                 ,tumblr-auth-test-rejection-json))
      (let ((tumblr-oauth-use-listener nil)
            messages)
        (cl-letf (((symbol-function 'browse-url) #'ignore)
                  ((symbol-function 'read-string) (lambda (&rest _) "BAD"))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) messages))))
          (tumblr-login))
        (should-not (tumblr-auth-logged-in-p))
        (should (string-match-p "Refresh token revoked" (car messages)))))))

(ert-deftest tumblr-auth-test-logout ()
  "Logging out removes the token file and runs the hook."
  (tumblr-auth-test-with-state
    (tumblr-auth--save-token (tumblr-auth-test-token 4102444800))
    (let ((tumblr-auth-logout-hook nil)
          hooked)
      (add-hook 'tumblr-auth-logout-hook (lambda () (setq hooked t)))
      (tumblr-logout)
      (should hooked))
    (should-not (tumblr-auth-logged-in-p))
    (should-not (file-exists-p tumblr-token-file))))

;;;; Refreshing

(ert-deftest tumblr-auth-test-expired-token-refreshes ()
  "An expired token is refreshed before it is handed out."
  (tumblr-auth-test-with-state
    (tumblr-auth--save-token (tumblr-auth-test-token 1))
    (tumblr-test-with-backend `(("/oauth2/token" 200
                                 ,tumblr-auth-test-token2-json))
      (should (equal (tumblr-auth-access-token) "AT2"))
      (should (equal (plist-get (tumblr-auth--token) :refresh-token) "RT2"))
      (should (string-match-p "grant_type=refresh_token&refresh_token=RT1&"
                              (tumblr-test-call-key (tumblr-test-call 0)
                                                    :body)))
      (setq tumblr-auth--token 'unread)
      (should (equal (tumblr-auth-access-token) "AT2"))
      (should (equal (length tumblr-test-calls) 1)))))

(ert-deftest tumblr-auth-test-refresh-keeps-refresh-token ()
  "A refresh response without a refresh token keeps the old one."
  (tumblr-auth-test-with-state
    (tumblr-auth--save-token (tumblr-auth-test-token 1))
    (tumblr-test-with-backend '(("/oauth2/token" 200
                                 "{\"access_token\":\"AT2\",\"expires_in\":10}"))
      (should (equal (tumblr-auth-access-token) "AT2"))
      (should (equal (plist-get (tumblr-auth--token) :refresh-token) "RT1")))))

(ert-deftest tumblr-auth-test-refresh-rejected ()
  "A refused refresh ends the session and asks for a new login."
  (tumblr-auth-test-with-state
    (tumblr-auth--save-token (tumblr-auth-test-token 1))
    (tumblr-test-with-backend `(("/oauth2/token" 400
                                 ,tumblr-auth-test-rejection-json))
      (let ((err (should-error (tumblr-auth-access-token)
                               :type 'tumblr-auth-error)))
        (should (string-match-p "Refresh token revoked" (cadr err))))
      (should-not (tumblr-auth-logged-in-p))
      (should-not (file-exists-p tumblr-token-file))
      (should-not tumblr-auth--refreshing))))

(ert-deftest tumblr-auth-test-refresh-transport-error ()
  "A network failure during refresh keeps the session for later."
  (tumblr-auth-test-with-state
    (tumblr-auth--save-token (tumblr-auth-test-token 1))
    (tumblr-test-with-backend '(("/oauth2/token" curl nil))
      (should-error (tumblr-auth-access-token) :type 'tumblr-http-error)
      (should (tumblr-auth-logged-in-p))
      (should (file-exists-p tumblr-token-file))
      (should-not tumblr-auth--refreshing))))

(ert-deftest tumblr-auth-test-async-refresh-single-flight ()
  "Concurrent asynchronous refreshes share one request."
  (tumblr-auth-test-with-state
    (tumblr-auth--save-token (tumblr-auth-test-token 1))
    (tumblr-test-with-backend `(("/oauth2/token" 200
                                 ,tumblr-auth-test-token2-json))
      (let ((tumblr-test-defer t)
            (done 0))
        (tumblr-auth-refresh-async (lambda () (cl-incf done)) #'ignore)
        (tumblr-auth-refresh-async (lambda () (cl-incf done)) #'ignore)
        (should tumblr-auth--refreshing)
        (should (equal (length tumblr-test-calls) 1))
        (tumblr-test-deliver)
        (should (equal done 2))
        (should-not tumblr-auth--refreshing)
        (should (equal (plist-get (tumblr-auth--token) :access-token) "AT2"))))))

(ert-deftest tumblr-auth-test-async-refresh-failure ()
  "A failed asynchronous refresh reaches every waiting ELSE."
  (tumblr-auth-test-with-state
    (tumblr-auth--save-token (tumblr-auth-test-token 1))
    (tumblr-test-with-backend `(("/oauth2/token" 400
                                 ,tumblr-auth-test-rejection-json))
      (let (errors)
        (tumblr-auth-refresh-async #'ignore (lambda (e) (push e errors)))
        (tumblr-auth-refresh-async #'ignore (lambda (e) (push e errors)))
        (should (equal (length errors) 2))
        (should (eq (car (car errors)) 'tumblr-auth-error))
        (should-not (tumblr-auth-logged-in-p))))))

;;;; The redirect listener

(defun tumblr-auth-test-connect (port request)
  "Send REQUEST to the listener on PORT and return the response text."
  (let* ((response "")
         (client (make-network-process
                  :name "tumblr-auth-test-client" :host "127.0.0.1"
                  :service port :coding 'binary :noquery t
                  :filter (lambda (_ data)
                            (setq response (concat response data))))))
    (process-send-string client request)
    (let ((deadline (+ (float-time) 5)))
      (while (and (process-live-p client) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (accept-process-output nil 0.1)
    response))

(defun tumblr-auth-test-start-listener (state callback)
  "Start a listener on a free port for STATE and CALLBACK; skip if refused."
  (condition-case nil
      (tumblr-auth--start-listener t state callback)
    (error (ert-skip "cannot listen on the loopback interface"))))

(ert-deftest tumblr-auth-test-listener-round-trip ()
  "The listener hands the code to the callback and closes."
  (tumblr-auth-test-with-state
    (let* (got
           (server (tumblr-auth-test-start-listener
                    "STATE" (lambda (code) (setq got code))))
           (port (process-contact server :service))
           (response (tumblr-auth-test-connect
                      port
                      (concat "GET /redirect?code=abc&state=STATE HTTP/1.1"
                              "\r\nHost: localhost\r\n\r\n"))))
      (should (string-prefix-p "HTTP/1.1 200 OK\r\n" response))
      (should (string-match-p "Connection: close" response))
      (should (string-match-p "return to Emacs" response))
      (should (equal got "abc"))
      (should-not tumblr-auth--listener)
      (should-not (process-live-p server)))))

(ert-deftest tumblr-auth-test-listener-rejects-state ()
  "A redirect with the wrong state ends the login without a code."
  (tumblr-auth-test-with-state
    (let* (got
           (server (tumblr-auth-test-start-listener
                    "STATE" (lambda (code) (setq got code))))
           (port (process-contact server :service))
           (response (tumblr-auth-test-connect
                      port
                      "GET /redirect?code=abc&state=OTHER HTTP/1.1\r\n\r\n")))
      (should (string-match-p "Login failed" response))
      (should-not got)
      (should-not tumblr-auth--listener))))

(ert-deftest tumblr-auth-test-listener-ignores-other-requests ()
  "Unrelated requests get a 404 and the listener keeps waiting."
  (tumblr-auth-test-with-state
    (let* ((server (tumblr-auth-test-start-listener "STATE" #'ignore))
           (port (process-contact server :service))
           (response (tumblr-auth-test-connect
                      port "GET /favicon.ico HTTP/1.1\r\n\r\n")))
      (should (string-prefix-p "HTTP/1.1 404" response))
      (should tumblr-auth--listener)
      (should (process-live-p server))
      (tumblr-auth--stop-listener)
      (should-not (process-live-p server)))))

(provide 'tumblr-auth-test)
;;; tumblr-auth-test.el ends here
