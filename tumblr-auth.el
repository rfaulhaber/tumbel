;;; tumblr-auth.el --- Credentials and OAuth for tumblr.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan Faulhaber

;; Author: Ryan Faulhaber <ryf@sent.as>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Consumer credentials of the registered Tumblr application and the
;; OAuth 2.0 authorization-code flow that turns a browser login into
;; an access token.  The consumer key doubles as the API key that
;; public endpoints accept without a login, so browsing blogs only
;; needs the key to be set.
;;
;; Logging in opens the authorization page in the browser.  Emacs
;; listens on the port of the registered redirect URI for the code
;; the browser is sent back with; when it cannot, the user pastes
;; the code (or the whole redirect URL) instead.  Tokens live in
;; `tumblr-token-file' and are refreshed when they expire.  Tumblr
;; rotates the refresh token on every refresh, so only one refresh
;; may ever be in flight: callers arriving during one wait for it.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'tumblr-http)

;;;; Configuration

(defcustom tumblr-consumer-key nil
  "OAuth consumer key of your registered Tumblr application.
When nil, the key is read from `auth-source' under the host
\"api.tumblr.com\": the login is the consumer key and the password
the consumer secret."
  :type '(choice (const :tag "Use auth-source" nil) string)
  :group 'tumblr)

(defcustom tumblr-consumer-secret nil
  "OAuth consumer secret of your registered Tumblr application.
When nil, the secret is read from `auth-source' as described in
`tumblr-consumer-key'."
  :type '(choice (const :tag "Use auth-source" nil) string)
  :group 'tumblr)

(defcustom tumblr-directory (locate-user-emacs-file "tumblr/")
  "Directory where tumblr.el keeps its state, such as tokens."
  :type 'directory
  :group 'tumblr)

(defcustom tumblr-token-file nil
  "File holding the OAuth tokens.
When nil, tokens.eld inside `tumblr-directory' is used.  A name
ending in .gpg is encrypted transparently by EasyPG."
  :type '(choice (const :tag "tokens.eld in tumblr-directory" nil) file)
  :group 'tumblr)

(defcustom tumblr-oauth-redirect-uri "http://localhost:3000/redirect"
  "Redirect URI registered with your Tumblr application.
When it points at a port on this machine, `tumblr-login' listens
there for the browser to come back with the authorization code."
  :type 'string
  :group 'tumblr)

(defcustom tumblr-oauth-scopes "basic write offline_access"
  "Scopes requested when logging in, separated by spaces.
Without \"offline_access\" no refresh token is issued and the login
lapses after about an hour."
  :type 'string
  :group 'tumblr)

(defcustom tumblr-oauth-use-listener t
  "Whether `tumblr-login' listens for the redirect on this machine.
When nil, or when the port cannot be opened, the code is pasted."
  :type 'boolean
  :group 'tumblr)

(defcustom tumblr-oauth-listener-timeout 300
  "Seconds to wait for the browser before giving up a login."
  :type 'integer
  :group 'tumblr)

(defvar tumblr-auth-login-hook nil
  "Hook run after logging in.")

(defvar tumblr-auth-logout-hook nil
  "Hook run after logging out.")

(defconst tumblr-oauth-authorize-url "https://www.tumblr.com/oauth2/authorize"
  "Page where the user grants access to the application.")

(defconst tumblr-oauth-token-url "https://api.tumblr.com/v2/oauth2/token"
  "Endpoint exchanging codes and refresh tokens for access tokens.")

(defconst tumblr-auth-host "api.tumblr.com"
  "Host under which the consumer credentials are stored in auth-source.")

;;;; Consumer credentials

(defun tumblr-auth--auth-source-entry ()
  "Return the auth-source entry holding the consumer credentials."
  (car (auth-source-search :host tumblr-auth-host :max 1)))

(defun tumblr-auth--entry-secret (entry)
  "Return the secret stored in the auth-source ENTRY as a string."
  (let ((secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun tumblr-auth-consumer-key ()
  "Return the consumer key, or signal `tumblr-auth-error'."
  (or tumblr-consumer-key
      (plist-get (tumblr-auth--auth-source-entry) :user)
      (signal 'tumblr-auth-error
              (list (concat "No Tumblr consumer key configured; set "
                            "`tumblr-consumer-key' or add an auth-source "
                            "entry for api.tumblr.com")))))

(defun tumblr-auth-consumer-credentials ()
  "Return the consumer key and secret as a cons (KEY . SECRET).
Signal `tumblr-auth-error' when either is missing."
  (let* ((entry (unless (and tumblr-consumer-key tumblr-consumer-secret)
                  (tumblr-auth--auth-source-entry)))
         (key (or tumblr-consumer-key (plist-get entry :user)))
         (secret (or tumblr-consumer-secret
                     (and entry (tumblr-auth--entry-secret entry)))))
    (unless (and key secret)
      (signal 'tumblr-auth-error
              (list (concat "No Tumblr consumer credentials configured; set "
                            "`tumblr-consumer-key' and "
                            "`tumblr-consumer-secret' or add an auth-source "
                            "entry for api.tumblr.com"))))
    (cons key secret)))

;;;; Token storage

(defvar tumblr-auth--token 'unread
  "The token plist, nil when logged out, or `unread' before the file is read.
The plist has the keys `:access-token', `:refresh-token',
`:expires-at' (seconds since the epoch, with a safety margin) and
`:scope'.")

(defun tumblr-auth-token-file ()
  "Return the file the tokens are stored in."
  (or tumblr-token-file (expand-file-name "tokens.eld" tumblr-directory)))

(defun tumblr-auth--read-token-file ()
  "Return the token plist stored in the token file, or nil."
  (let ((file (tumblr-auth-token-file)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (let ((data (read (current-buffer))))
              (and (plist-get data :access-token) data)))
        (error
         (message "tumblr: ignoring unreadable token file %s (%s)"
                  file (error-message-string err))
         nil)))))

(defun tumblr-auth--token ()
  "Return the current token plist, reading the token file once."
  (when (eq tumblr-auth--token 'unread)
    (setq tumblr-auth--token (tumblr-auth--read-token-file)))
  tumblr-auth--token)

(defun tumblr-auth--save-token (token)
  "Store TOKEN in memory and in the token file, readable only by the user."
  (let ((file (tumblr-auth-token-file)))
    (with-file-modes #o700
      (make-directory (file-name-directory file) t))
    (with-file-modes #o600
      (with-temp-file file
        (let ((print-length nil)
              (print-level nil))
          (insert ";; Tumblr OAuth tokens; keep this file private.\n")
          (prin1 token (current-buffer))
          (insert "\n"))))
    (setq tumblr-auth--token token)))

(defun tumblr-auth--forget (&optional delete)
  "Drop the token from memory, and with DELETE non-nil from disk too."
  (setq tumblr-auth--token nil)
  (when delete
    (let ((file (tumblr-auth-token-file)))
      (when (file-exists-p file)
        (delete-file file)))))

(defun tumblr-auth-logged-in-p ()
  "Return non-nil when an OAuth access token is available."
  (and (tumblr-auth--token) t))

(defun tumblr-auth-token-expired-p (token &optional now)
  "Return non-nil when the access token of TOKEN has expired at NOW."
  (let ((expires (plist-get token :expires-at)))
    (and expires (>= (or now (float-time)) expires))))

(defun tumblr-auth-access-token ()
  "Return a valid OAuth access token, refreshing an expired one.
Signal `tumblr-auth-error' when not logged in or when the session
cannot be refreshed."
  (let ((token (or (tumblr-auth--token)
                   (signal 'tumblr-auth-error
                           (list (concat "Not logged in to Tumblr; run "
                                         "`tumblr-login' first"))))))
    (when (tumblr-auth-token-expired-p token)
      (setq token (tumblr-auth-refresh)))
    (plist-get token :access-token)))

;;;; Token endpoint

(defun tumblr-auth--token-from-response (response &optional now)
  "Turn the token endpoint RESPONSE into a token plist.
NOW is the time the response arrived; it defaults to the current
time.  The expiry is brought forward by a minute so that a token
about to expire is never sent."
  (let ((access (alist-get 'access_token response))
        (expires-in (alist-get 'expires_in response)))
    (unless access
      (signal 'tumblr-auth-error
              (list "Tumblr did not return an access token")))
    (list :access-token access
          :refresh-token (alist-get 'refresh_token response)
          :expires-at (and expires-in
                           (+ (or now (float-time)) expires-in -60))
          :scope (alist-get 'scope response))))

(defun tumblr-auth--adopt-token (new old)
  "Return the token plist NEW, keeping the refresh token of OLD if absent."
  (if (or (plist-get new :refresh-token) (null old))
      new
    (plist-put new :refresh-token (plist-get old :refresh-token))))


(defun tumblr-auth--token-request (params &optional then else)
  "Ask the token endpoint for tokens with PARAMS and the client credentials.
With THEN nil, wait and return the token plist.  Otherwise return
at once and later call THEN with the plist, or ELSE with the error."
  (let* ((credentials (tumblr-auth-consumer-credentials))
         (body (tumblr-http-form-encode
                (append params
                        `((client_id . ,(car credentials))
                          (client_secret . ,(cdr credentials))))))
         (headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (if then
        (tumblr-http-request 'post tumblr-oauth-token-url
                             :headers headers :body body :body-type 'binary
                             :then (lambda (response)
                                     (tumblr-http-deliver
                                      (lambda ()
                                        (tumblr-auth--token-from-response
                                         response))
                                      then else))
                             :else else)
      (tumblr-auth--token-from-response
       (tumblr-http-request 'post tumblr-oauth-token-url
                            :headers headers :body body :body-type 'binary
                            :then 'sync)))))

(defun tumblr-auth--oauth-error-string (err)
  "Describe ERR, an error from the token endpoint, for the user."
  (if (eq (car-safe err) 'tumblr-api-error)
      (let ((body (tumblr-api-error-body err)))
        (or (alist-get 'error_description body)
            (alist-get 'error body)
            (tumblr-http-error-string err)))
    (tumblr-http-error-string err)))

(defun tumblr-auth--session-error (err)
  "Return the error to report when a refresh fails with ERR.
A rejection by the token endpoint means the session is over and is
reported as `tumblr-auth-error'; any other failure passes through."
  (if (eq (car-safe err) 'tumblr-api-error)
      (list 'tumblr-auth-error
            (format "Tumblr session expired (%s); run `tumblr-login' again"
                    (tumblr-auth--oauth-error-string err)))
    err))

;;;; Refreshing

(defvar tumblr-auth--refreshing nil
  "Non-nil while the tokens are being refreshed.")

(defvar tumblr-auth--waiters nil
  "Callbacks (THEN . ELSE) waiting for the refresh in flight.")

(defun tumblr-auth--finish-refresh (token err)
  "End the refresh in flight with the new TOKEN, or with ERR.
The waiting callbacks are run in the order they arrived."
  (let ((waiters (nreverse tumblr-auth--waiters)))
    (setq tumblr-auth--waiters nil
          tumblr-auth--refreshing nil)
    (if token
        (tumblr-auth--save-token token)
      (when (eq (car-safe err) 'tumblr-auth-error)
        (tumblr-auth--forget t)))
    (dolist (waiter waiters)
      (if token
          (funcall (car waiter))
        (when (cdr waiter)
          (funcall (cdr waiter) err))))))

(defun tumblr-auth--refresh-params ()
  "Return the token endpoint parameters refreshing the current session."
  (let ((token (tumblr-auth--token)))
    (unless (plist-get token :refresh-token)
      (tumblr-auth--forget t)
      (signal 'tumblr-auth-error
              (list "Tumblr session expired; run `tumblr-login' again")))
    `((grant_type . "refresh_token")
      (refresh_token . ,(plist-get token :refresh-token)))))

(defun tumblr-auth-refresh ()
  "Refresh the tokens now and return the new token plist.
A refresh already in flight is waited for instead of starting a
second one, since Tumblr rotates the refresh token every time."
  (while tumblr-auth--refreshing
    (accept-process-output nil 0.05))
  (let ((old (tumblr-auth--token))
        (params (tumblr-auth--refresh-params)))
    (setq tumblr-auth--refreshing t)
    (condition-case err
        (let ((new (tumblr-auth--adopt-token
                    (tumblr-auth--token-request params) old)))
          (tumblr-auth--finish-refresh new nil)
          new)
      (error
       (let ((reported (tumblr-auth--session-error err)))
         (tumblr-auth--finish-refresh nil reported)
         (signal (car reported) (cdr reported)))))))

(defun tumblr-auth-refresh-async (then else)
  "Refresh the tokens in the background, then call THEN.
ELSE is called with the error when the refresh fails.  Callers
arriving while a refresh is in flight share its outcome."
  (push (cons then else) tumblr-auth--waiters)
  (unless tumblr-auth--refreshing
    (setq tumblr-auth--refreshing t)
    (condition-case err
        (let ((old (tumblr-auth--token)))
          (tumblr-auth--token-request
           (tumblr-auth--refresh-params)
           (lambda (new)
             (tumblr-auth--finish-refresh (tumblr-auth--adopt-token new old)
                                          nil))
           (lambda (err)
             (tumblr-auth--finish-refresh nil
                                          (tumblr-auth--session-error err)))))
      (tumblr-error (tumblr-auth--finish-refresh nil err)))))

;;;; Logging in

(defvar tumblr-auth--listener nil
  "The login in progress, or nil.
A plist with the keys `:server', `:state', `:callback'
and `:timer'.")

(defun tumblr-auth-authorize-url (state)
  "Return the URL where the user grants access, carrying STATE."
  (concat tumblr-oauth-authorize-url "?"
          (tumblr-http-form-encode
           `((client_id . ,(tumblr-auth-consumer-key))
             (response_type . "code")
             (scope . ,tumblr-oauth-scopes)
             (state . ,state)
             (redirect_uri . ,tumblr-oauth-redirect-uri)))))

(defun tumblr-auth--random-state ()
  "Return a fresh unguessable state string."
  (md5 (format "%s %s %s %s" (random) (float-time) (emacs-pid) (user-uid))))

(defun tumblr-auth-parse-redirect (request)
  "Return (CODE . STATE) from the HTTP request line REQUEST, or nil.
REQUEST may also be the redirect URL itself.  Signal
`tumblr-auth-error' when the authorization server reports an error."
  (let* ((target (if (string-match "\\`GET \\([^ \r\n]+\\)" request)
                     (match-string 1 request)
                   request))
         (query (and (string-match "\\?\\([^#]*\\)" target)
                     (match-string 1 target)))
         (params (and query (url-parse-query-string query)))
         (error-code (cadr (assoc "error" params)))
         (description (cadr (assoc "error_description" params)))
         (code (cadr (assoc "code" params))))
    (when error-code
      (signal 'tumblr-auth-error
              (list (format "Tumblr refused the login: %s%s" error-code
                            (if description (format " (%s)" description) "")))))
    (and code (cons code (cadr (assoc "state" params))))))

(defun tumblr-auth-parse-code (input)
  "Return the authorization code in INPUT, a code or the redirect URL."
  (let ((input (string-trim input)))
    (if (string-match-p "\\`https?://" input)
        (or (car (tumblr-auth-parse-redirect input))
            (signal 'tumblr-auth-error
                    (list "That URL does not carry an authorization code")))
      input)))

(defun tumblr-auth--redirect-port ()
  "Return the port of `tumblr-oauth-redirect-uri' when it names this machine."
  (let ((url (url-generic-parse-url tumblr-oauth-redirect-uri)))
    (and (member (url-host url) '("localhost" "127.0.0.1"))
         (or (url-portspec url) 80))))

(defun tumblr-auth--respond (client status body)
  "Send an HTTP response with STATUS and the message BODY to CLIENT."
  (let ((bytes (encode-coding-string
                (format "<!doctype html><html><body><p>%s</p></body></html>"
                        body)
                'utf-8)))
    (process-send-string
     client
     (format (concat "HTTP/1.1 %s\r\nContent-Type: text/html; charset=utf-8"
                     "\r\nContent-Length: %d\r\nConnection: close\r\n\r\n")
             status (length bytes)))
    (process-send-string client bytes)
    (delete-process client)))

(defun tumblr-auth--listener-filter (client data)
  "Collect DATA from CLIENT until the request headers are complete."
  (let ((request (concat (process-get client :tumblr-request) data)))
    (process-put client :tumblr-request request)
    (when (string-match-p "\r\n\r\n\\|\n\n" request)
      (tumblr-auth--handle-redirect client request))))

(defun tumblr-auth--handle-redirect (client request)
  "Act on REQUEST, received from CLIENT on the redirect port."
  (let ((listener tumblr-auth--listener)
        code failure)
    (condition-case err
        (let ((parsed (tumblr-auth-parse-redirect request)))
          (cond ((null parsed))
                ((not (equal (cdr parsed) (plist-get listener :state)))
                 (setq failure "the login state did not match; try again"))
                (t (setq code (car parsed)))))
      (tumblr-auth-error (setq failure (cadr err))))
    (cond (code
           (tumblr-auth--respond client "200 OK"
                                 "Logged in. You can return to Emacs.")
           (tumblr-auth--stop-listener)
           (run-at-time 0 nil (plist-get listener :callback) code))
          (failure
           (tumblr-auth--respond client "200 OK"
                                 (format "Login failed: %s" failure))
           (tumblr-auth--stop-listener)
           (run-at-time 0 nil #'message "tumblr: login failed: %s" failure))
          (t
           (tumblr-auth--respond client "404 Not Found" "Not found.")))))

(defun tumblr-auth--start-listener (port state callback)
  "Listen on PORT for the redirect carrying STATE; then call CALLBACK.
CALLBACK receives the authorization code.  Return the server process."
  (tumblr-auth--stop-listener)
  (let ((server (make-network-process
                 :name "tumblr-oauth" :server t :noquery t
                 :host "127.0.0.1" :family 'ipv4 :service port
                 :coding 'binary
                 :filter #'tumblr-auth--listener-filter)))
    (setq tumblr-auth--listener
          (list :server server :state state :callback callback
                :timer (run-at-time tumblr-oauth-listener-timeout nil
                                    #'tumblr-auth--listener-timeout)))
    server))

(defun tumblr-auth--stop-listener ()
  "Stop waiting for the redirect, if a login was in progress."
  (when-let* ((listener tumblr-auth--listener))
    (setq tumblr-auth--listener nil)
    (when (timerp (plist-get listener :timer))
      (cancel-timer (plist-get listener :timer)))
    (when (process-live-p (plist-get listener :server))
      (delete-process (plist-get listener :server)))))

(defun tumblr-auth--listener-timeout ()
  "Give up a login the browser never came back from."
  (when tumblr-auth--listener
    (tumblr-auth--stop-listener)
    (message "tumblr: the login timed out; run `tumblr-login' again")))

(defun tumblr-auth--complete-login (code then)
  "Exchange CODE for tokens, store them and call THEN, if any."
  (condition-case err
      (progn
        (tumblr-auth--save-token
         (tumblr-auth--token-request
          `((grant_type . "authorization_code")
            (code . ,code)
            (redirect_uri . ,tumblr-oauth-redirect-uri))))
        (message "Logged in to Tumblr")
        (run-hooks 'tumblr-auth-login-hook)
        (when then
          (funcall then)))
    (tumblr-error
     (message "tumblr: login failed: %s"
              (tumblr-auth--oauth-error-string err)))))

;;;###autoload
(defun tumblr-login (&optional then)
  "Log in to Tumblr through the browser.
The consumer credentials must be configured first; see
`tumblr-consumer-key'.  THEN, when given, is called once the login
has completed."
  (interactive)
  (tumblr-auth-consumer-credentials)
  (let* ((state (tumblr-auth--random-state))
         (url (tumblr-auth-authorize-url state))
         (port (and tumblr-oauth-use-listener (tumblr-auth--redirect-port)))
         (server (and port
                      (condition-case err
                          (tumblr-auth--start-listener
                           port state
                           (lambda (code)
                             (tumblr-auth--complete-login code then)))
                        (error
                         (message "tumblr: cannot listen on port %s (%s)"
                                  port (error-message-string err))
                         nil)))))
    (browse-url url)
    (if server
        (message "Complete the login in your browser; Emacs is waiting")
      (let ((code (tumblr-auth-parse-code
                   (read-string
                    "Paste the code, or the URL you were redirected to: "))))
        (tumblr-auth--complete-login code then)))))

;;;###autoload
(defun tumblr-logout ()
  "Forget the Tumblr login."
  (interactive)
  (tumblr-auth--stop-listener)
  (tumblr-auth--forget t)
  (run-hooks 'tumblr-auth-logout-hook)
  (message "Logged out of Tumblr"))

(provide 'tumblr-auth)
;;; tumblr-auth.el ends here
