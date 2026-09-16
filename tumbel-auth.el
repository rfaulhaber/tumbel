;;; tumbel-auth.el --- Credentials and OAuth for tumbel.el  -*- lexical-binding: t; -*-

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
;; `tumbel-token-file' and are refreshed when they expire.  Tumblr
;; rotates the refresh token on every refresh, so only one refresh
;; may ever be in flight: callers arriving during one wait for it.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'tumbel-http)

;;;; Configuration

(defcustom tumbel-consumer-key nil
  "OAuth consumer key of your registered Tumblr application.
When nil, the key is read from `auth-source' under the host
\"api.tumblr.com\": the login is the consumer key and the password
the consumer secret."
  :type '(choice (const :tag "Use auth-source" nil) string)
  :group 'tumbel)

(defcustom tumbel-consumer-secret nil
  "OAuth consumer secret of your registered Tumblr application.
When nil, the secret is read from `auth-source' as described in
`tumbel-consumer-key'."
  :type '(choice (const :tag "Use auth-source" nil) string)
  :group 'tumbel)

(defcustom tumbel-directory (locate-user-emacs-file "tumbel/")
  "Directory where tumbel.el keeps its state, such as tokens."
  :type 'directory
  :group 'tumbel)

(defcustom tumbel-token-file nil
  "File holding the OAuth tokens.
When nil, tokens.eld inside `tumbel-directory' is used.  A name
ending in .gpg is encrypted transparently by EasyPG."
  :type '(choice (const :tag "tokens.eld in tumbel-directory" nil) file)
  :group 'tumbel)

(defcustom tumbel-oauth-redirect-uri "http://localhost:3000/redirect"
  "Redirect URI registered with your Tumblr application.
When it points at a port on this machine, `tumbel-login' listens
there for the browser to come back with the authorization code."
  :type 'string
  :group 'tumbel)

(defcustom tumbel-oauth-scopes "basic write offline_access"
  "Scopes requested when logging in, separated by spaces.
Without \"offline_access\" no refresh token is issued and the login
lapses after about an hour."
  :type 'string
  :group 'tumbel)

(defcustom tumbel-oauth-use-listener t
  "Whether `tumbel-login' listens for the redirect on this machine.
When nil, or when the port cannot be opened, the code is pasted."
  :type 'boolean
  :group 'tumbel)

(defcustom tumbel-oauth-listener-timeout 300
  "Seconds to wait for the browser before giving up a login."
  :type 'integer
  :group 'tumbel)

(defvar tumbel-auth-login-hook nil
  "Hook run after logging in.")

(defvar tumbel-auth-logout-hook nil
  "Hook run after logging out.")

(defconst tumbel-oauth-authorize-url "https://www.tumblr.com/oauth2/authorize"
  "Page where the user grants access to the application.")

(defconst tumbel-oauth-token-url "https://api.tumblr.com/v2/oauth2/token"
  "Endpoint exchanging codes and refresh tokens for access tokens.")

(defconst tumbel-auth-host "api.tumblr.com"
  "Host under which the consumer credentials are stored in auth-source.")

;;;; Consumer credentials

(defun tumbel-auth--auth-source-entry ()
  "Return the auth-source entry holding the consumer credentials."
  (car (auth-source-search :host tumbel-auth-host :max 1)))

(defun tumbel-auth--entry-secret (entry)
  "Return the secret stored in the auth-source ENTRY as a string."
  (let ((secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun tumbel-auth-consumer-key ()
  "Return the consumer key, or signal `tumbel-auth-error'."
  (or tumbel-consumer-key
      (plist-get (tumbel-auth--auth-source-entry) :user)
      (signal 'tumbel-auth-error
              (list (concat "No Tumblr consumer key configured; set "
                            "`tumbel-consumer-key' or add an auth-source "
                            "entry for api.tumblr.com")))))

(defun tumbel-auth-consumer-credentials ()
  "Return the consumer key and secret as a cons (KEY . SECRET).
Signal `tumbel-auth-error' when either is missing."
  (let* ((entry (unless (and tumbel-consumer-key tumbel-consumer-secret)
                  (tumbel-auth--auth-source-entry)))
         (key (or tumbel-consumer-key (plist-get entry :user)))
         (secret (or tumbel-consumer-secret
                     (and entry (tumbel-auth--entry-secret entry)))))
    (unless (and key secret)
      (signal 'tumbel-auth-error
              (list (concat "No Tumblr consumer credentials configured; set "
                            "`tumbel-consumer-key' and "
                            "`tumbel-consumer-secret' or add an auth-source "
                            "entry for api.tumblr.com"))))
    (cons key secret)))

;;;; Token storage

(defvar tumbel-auth--token 'unread
  "The token plist, nil when logged out, or `unread' before the file is read.
The plist has the keys `:access-token', `:refresh-token',
`:expires-at' (seconds since the epoch, with a safety margin) and
`:scope'.")

(defun tumbel-auth-token-file ()
  "Return the file the tokens are stored in."
  (or tumbel-token-file (expand-file-name "tokens.eld" tumbel-directory)))

(defun tumbel-auth--read-token-file ()
  "Return the token plist stored in the token file, or nil."
  (let ((file (tumbel-auth-token-file)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (let ((data (read (current-buffer))))
              (and (plist-get data :access-token) data)))
        (error
         (message "tumbel: ignoring unreadable token file %s (%s)"
                  file (error-message-string err))
         nil)))))

(defun tumbel-auth--token ()
  "Return the current token plist, reading the token file once."
  (when (eq tumbel-auth--token 'unread)
    (setq tumbel-auth--token (tumbel-auth--read-token-file)))
  tumbel-auth--token)

(defun tumbel-auth--save-token (token)
  "Store TOKEN in memory and in the token file, readable only by the user."
  (let ((file (tumbel-auth-token-file)))
    (with-file-modes #o700
      (make-directory (file-name-directory file) t))
    (with-file-modes #o600
      (with-temp-file file
        (let ((print-length nil)
              (print-level nil))
          (insert ";; Tumblr OAuth tokens; keep this file private.\n")
          (prin1 token (current-buffer))
          (insert "\n"))))
    (setq tumbel-auth--token token)))

(defun tumbel-auth--forget (&optional delete)
  "Drop the token from memory, and with DELETE non-nil from disk too."
  (setq tumbel-auth--token nil)
  (when delete
    (let ((file (tumbel-auth-token-file)))
      (when (file-exists-p file)
        (delete-file file)))))

(defun tumbel-auth-logged-in-p ()
  "Return non-nil when an OAuth access token is available."
  (and (tumbel-auth--token) t))

(defun tumbel-auth-token-expired-p (token &optional now)
  "Return non-nil when the access token of TOKEN has expired at NOW."
  (let ((expires (plist-get token :expires-at)))
    (and expires (>= (or now (float-time)) expires))))

(defun tumbel-auth-access-token ()
  "Return a valid OAuth access token, refreshing an expired one.
Signal `tumbel-auth-error' when not logged in or when the session
cannot be refreshed."
  (let ((token (or (tumbel-auth--token)
                   (signal 'tumbel-auth-error
                           (list (concat "Not logged in to Tumblr; run "
                                         "`tumbel-login' first"))))))
    (when (tumbel-auth-token-expired-p token)
      (setq token (tumbel-auth-refresh)))
    (plist-get token :access-token)))

;;;; Token endpoint

(defun tumbel-auth--token-from-response (response &optional now)
  "Turn the token endpoint RESPONSE into a token plist.
NOW is the time the response arrived; it defaults to the current
time.  The expiry is brought forward by a minute so that a token
about to expire is never sent."
  (let ((access (alist-get 'access_token response))
        (expires-in (alist-get 'expires_in response)))
    (unless access
      (signal 'tumbel-auth-error
              (list "Tumblr did not return an access token")))
    (list :access-token access
          :refresh-token (alist-get 'refresh_token response)
          :expires-at (and expires-in
                           (+ (or now (float-time)) expires-in -60))
          :scope (alist-get 'scope response))))

(defun tumbel-auth--adopt-token (new old)
  "Return the token plist NEW, keeping the refresh token of OLD if absent."
  (if (or (plist-get new :refresh-token) (null old))
      new
    (plist-put new :refresh-token (plist-get old :refresh-token))))


(defun tumbel-auth--token-request (params &optional then else)
  "Ask the token endpoint for tokens with PARAMS and the client credentials.
With THEN nil, wait and return the token plist.  Otherwise return
at once and later call THEN with the plist, or ELSE with the error."
  (let* ((credentials (tumbel-auth-consumer-credentials))
         (body (tumbel-http-form-encode
                (append params
                        `((client_id . ,(car credentials))
                          (client_secret . ,(cdr credentials))))))
         (headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
    (if then
        (tumbel-http-request 'post tumbel-oauth-token-url
                             :headers headers :body body :body-type 'binary
                             :then (lambda (response)
                                     (tumbel-http-deliver
                                      (lambda ()
                                        (tumbel-auth--token-from-response
                                         response))
                                      then else))
                             :else else)
      (tumbel-auth--token-from-response
       (tumbel-http-request 'post tumbel-oauth-token-url
                            :headers headers :body body :body-type 'binary
                            :then 'sync)))))

(defun tumbel-auth--oauth-error-string (err)
  "Describe ERR, an error from the token endpoint, for the user."
  (if (eq (car-safe err) 'tumbel-api-error)
      (let ((body (tumbel-api-error-body err)))
        (or (alist-get 'error_description body)
            (alist-get 'error body)
            (tumbel-http-error-string err)))
    (tumbel-http-error-string err)))

(defun tumbel-auth--session-error (err)
  "Return the error to report when a refresh fails with ERR.
A rejection by the token endpoint means the session is over and is
reported as `tumbel-auth-error'; any other failure passes through."
  (if (eq (car-safe err) 'tumbel-api-error)
      (list 'tumbel-auth-error
            (format "Tumblr session expired (%s); run `tumbel-login' again"
                    (tumbel-auth--oauth-error-string err)))
    err))

;;;; Refreshing

(defvar tumbel-auth--refreshing nil
  "Non-nil while the tokens are being refreshed.")

(defvar tumbel-auth--waiters nil
  "Callbacks (THEN . ELSE) waiting for the refresh in flight.")

(defun tumbel-auth--finish-refresh (token err)
  "End the refresh in flight with the new TOKEN, or with ERR.
The waiting callbacks are run in the order they arrived."
  (let ((waiters (nreverse tumbel-auth--waiters)))
    (setq tumbel-auth--waiters nil
          tumbel-auth--refreshing nil)
    (if token
        (tumbel-auth--save-token token)
      (when (eq (car-safe err) 'tumbel-auth-error)
        (tumbel-auth--forget t)))
    (dolist (waiter waiters)
      (if token
          (funcall (car waiter))
        (when (cdr waiter)
          (funcall (cdr waiter) err))))))

(defun tumbel-auth--refresh-params ()
  "Return the token endpoint parameters refreshing the current session."
  (let ((token (tumbel-auth--token)))
    (unless (plist-get token :refresh-token)
      (tumbel-auth--forget t)
      (signal 'tumbel-auth-error
              (list "Tumblr session expired; run `tumbel-login' again")))
    `((grant_type . "refresh_token")
      (refresh_token . ,(plist-get token :refresh-token)))))

(defun tumbel-auth-refresh ()
  "Refresh the tokens now and return the new token plist.
A refresh already in flight is waited for instead of starting a
second one, since Tumblr rotates the refresh token every time."
  (while tumbel-auth--refreshing
    (accept-process-output nil 0.05))
  (let ((old (tumbel-auth--token))
        (params (tumbel-auth--refresh-params)))
    (setq tumbel-auth--refreshing t)
    (condition-case err
        (let ((new (tumbel-auth--adopt-token
                    (tumbel-auth--token-request params) old)))
          (tumbel-auth--finish-refresh new nil)
          new)
      (error
       (let ((reported (tumbel-auth--session-error err)))
         (tumbel-auth--finish-refresh nil reported)
         (signal (car reported) (cdr reported)))))))

(defun tumbel-auth-refresh-async (then else)
  "Refresh the tokens in the background, then call THEN.
ELSE is called with the error when the refresh fails.  Callers
arriving while a refresh is in flight share its outcome."
  (push (cons then else) tumbel-auth--waiters)
  (unless tumbel-auth--refreshing
    (setq tumbel-auth--refreshing t)
    (condition-case err
        (let ((old (tumbel-auth--token)))
          (tumbel-auth--token-request
           (tumbel-auth--refresh-params)
           (lambda (new)
             (tumbel-auth--finish-refresh (tumbel-auth--adopt-token new old)
                                          nil))
           (lambda (err)
             (tumbel-auth--finish-refresh nil
                                          (tumbel-auth--session-error err)))))
      (tumbel-error (tumbel-auth--finish-refresh nil err)))))

;;;; Logging in

(defvar tumbel-auth--listener nil
  "The login in progress, or nil.
A plist with the keys `:server', `:state', `:callback'
and `:timer'.")

(defun tumbel-auth-authorize-url (state)
  "Return the URL where the user grants access, carrying STATE."
  (concat tumbel-oauth-authorize-url "?"
          (tumbel-http-form-encode
           `((client_id . ,(tumbel-auth-consumer-key))
             (response_type . "code")
             (scope . ,tumbel-oauth-scopes)
             (state . ,state)
             (redirect_uri . ,tumbel-oauth-redirect-uri)))))

(defun tumbel-auth--random-state ()
  "Return a fresh unguessable state string."
  (md5 (format "%s %s %s %s" (random) (float-time) (emacs-pid) (user-uid))))

(defun tumbel-auth-parse-redirect (request)
  "Return (CODE . STATE) from the HTTP request line REQUEST, or nil.
REQUEST may also be the redirect URL itself.  Signal
`tumbel-auth-error' when the authorization server reports an error."
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
      (signal 'tumbel-auth-error
              (list (format "Tumblr refused the login: %s%s" error-code
                            (if description (format " (%s)" description) "")))))
    (and code (cons code (cadr (assoc "state" params))))))

(defun tumbel-auth-parse-code (input)
  "Return the authorization code in INPUT, a code or the redirect URL."
  (let ((input (string-trim input)))
    (if (string-match-p "\\`https?://" input)
        (or (car (tumbel-auth-parse-redirect input))
            (signal 'tumbel-auth-error
                    (list "That URL does not carry an authorization code")))
      input)))

(defun tumbel-auth--redirect-port ()
  "Return the port of `tumbel-oauth-redirect-uri' when it names this machine."
  (let ((url (url-generic-parse-url tumbel-oauth-redirect-uri)))
    (and (member (url-host url) '("localhost" "127.0.0.1"))
         (or (url-portspec url) 80))))

(defun tumbel-auth--respond (client status body)
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

(defun tumbel-auth--listener-filter (client data)
  "Collect DATA from CLIENT until the request headers are complete."
  (let ((request (concat (process-get client :tumbel-request) data)))
    (process-put client :tumbel-request request)
    (when (string-match-p "\r\n\r\n\\|\n\n" request)
      (tumbel-auth--handle-redirect client request))))

(defun tumbel-auth--handle-redirect (client request)
  "Act on REQUEST, received from CLIENT on the redirect port."
  (let ((listener tumbel-auth--listener)
        code failure)
    (condition-case err
        (let ((parsed (tumbel-auth-parse-redirect request)))
          (cond ((null parsed))
                ((not (equal (cdr parsed) (plist-get listener :state)))
                 (setq failure "the login state did not match; try again"))
                (t (setq code (car parsed)))))
      (tumbel-auth-error (setq failure (cadr err))))
    (cond (code
           (tumbel-auth--respond client "200 OK"
                                 "Logged in. You can return to Emacs.")
           (tumbel-auth--stop-listener)
           (run-at-time 0 nil (plist-get listener :callback) code))
          (failure
           (tumbel-auth--respond client "200 OK"
                                 (format "Login failed: %s" failure))
           (tumbel-auth--stop-listener)
           (run-at-time 0 nil #'message "tumbel: login failed: %s" failure))
          (t
           (tumbel-auth--respond client "404 Not Found" "Not found.")))))

(defun tumbel-auth--start-listener (port state callback)
  "Listen on PORT for the redirect carrying STATE; then call CALLBACK.
CALLBACK receives the authorization code.  Return the server process."
  (tumbel-auth--stop-listener)
  (let ((server (make-network-process
                 :name "tumbel-oauth" :server t :noquery t
                 :host "127.0.0.1" :family 'ipv4 :service port
                 :coding 'binary
                 :filter #'tumbel-auth--listener-filter)))
    (setq tumbel-auth--listener
          (list :server server :state state :callback callback
                :timer (run-at-time tumbel-oauth-listener-timeout nil
                                    #'tumbel-auth--listener-timeout)))
    server))

(defun tumbel-auth--stop-listener ()
  "Stop waiting for the redirect, if a login was in progress."
  (when-let* ((listener tumbel-auth--listener))
    (setq tumbel-auth--listener nil)
    (when (timerp (plist-get listener :timer))
      (cancel-timer (plist-get listener :timer)))
    (when (process-live-p (plist-get listener :server))
      (delete-process (plist-get listener :server)))))

(defun tumbel-auth--listener-timeout ()
  "Give up a login the browser never came back from."
  (when tumbel-auth--listener
    (tumbel-auth--stop-listener)
    (message "tumbel: the login timed out; run `tumbel-login' again")))

(defun tumbel-auth--complete-login (code then)
  "Exchange CODE for tokens, store them and call THEN, if any."
  (condition-case err
      (progn
        (tumbel-auth--save-token
         (tumbel-auth--token-request
          `((grant_type . "authorization_code")
            (code . ,code)
            (redirect_uri . ,tumbel-oauth-redirect-uri))))
        (message "Logged in to Tumblr")
        (run-hooks 'tumbel-auth-login-hook)
        (when then
          (funcall then)))
    (tumbel-error
     (message "tumbel: login failed: %s"
              (tumbel-auth--oauth-error-string err)))))

;;;###autoload
(defun tumbel-login (&optional then)
  "Log in to Tumblr through the browser.
The consumer credentials must be configured first; see
`tumbel-consumer-key'.  THEN, when given, is called once the login
has completed."
  (interactive)
  (tumbel-auth-consumer-credentials)
  (let* ((state (tumbel-auth--random-state))
         (url (tumbel-auth-authorize-url state))
         (port (and tumbel-oauth-use-listener (tumbel-auth--redirect-port)))
         (server (and port
                      (condition-case err
                          (tumbel-auth--start-listener
                           port state
                           (lambda (code)
                             (tumbel-auth--complete-login code then)))
                        (error
                         (message "tumbel: cannot listen on port %s (%s)"
                                  port (error-message-string err))
                         nil)))))
    (browse-url url)
    (if server
        (message "Complete the login in your browser; Emacs is waiting")
      (let ((code (tumbel-auth-parse-code
                   (read-string
                    "Paste the code, or the URL you were redirected to: "))))
        (tumbel-auth--complete-login code then)))))

;;;###autoload
(defun tumbel-logout ()
  "Forget the Tumblr login."
  (interactive)
  (tumbel-auth--stop-listener)
  (tumbel-auth--forget t)
  (run-hooks 'tumbel-auth-logout-hook)
  (message "Logged out of Tumblr"))

(provide 'tumbel-auth)
;;; tumbel-auth.el ends here
