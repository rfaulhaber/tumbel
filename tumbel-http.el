;;; tumbel-http.el --- HTTP transport for tumbel.el  -*- lexical-binding: t; -*-

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

;; The one place where tumbel.el touches the network.  Requests go
;; through `tumbel-http-request', which hands the transport to
;; `tumbel-http-backend' (curl via `plz' by default) and turns the
;; result into parsed JSON or a `tumbel-error'.  Tests replace the
;; backend with a fake, so everything above this layer runs offline.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'plz)
(require 'seq)
(require 'url-util)

;; Expected failures (not logged in, rate limited, network down) that
;; should read cleanly and stay out of the debugger, hence user-error.
(define-error 'tumbel-error "Tumblr error" 'user-error)
(define-error 'tumbel-http-error "Tumblr HTTP error" 'tumbel-error)
(define-error 'tumbel-api-error "Tumblr API error" 'tumbel-error)
(define-error 'tumbel-auth-error "Tumblr authentication error" 'tumbel-error)

(defcustom tumbel-http-timeout 30
  "Seconds to wait for a request to complete before giving up."
  :type 'integer
  :group 'tumbel)

(defconst tumbel-http-user-agent
  "tumbel.el (https://github.com/rfaulhaber/tumbel)"
  "User-Agent header sent with every request.")

(defvar tumbel-http-backend #'tumbel-http--plz
  "Function that performs HTTP requests for `tumbel-http-request'.
It is called with METHOD and URL followed by the keyword arguments
`:headers', `:body', `:body-type', `:binary', `:timeout', `:then'
and `:else'.  When `:then' is the symbol `sync', it returns a
`plz-response' for a 2xx status and a `plz-error' otherwise, either
carrying the response of a failed status or describing a transport
failure.  Otherwise it returns a process object (or nil) and later
calls THEN with a `plz-response' or ELSE with a `plz-error'.")

(defvar tumbel-http--native-json (json-available-p)
  "Non-nil when the native JSON functions can be used.")

;;;; JSON

(defun tumbel-http-parse-json (string &optional fidelity)
  "Parse the JSON text STRING.
Objects become alists keyed by symbols.  By default arrays become
lists and both null and false become nil, which is convenient to
read but cannot be encoded again.  With FIDELITY non-nil arrays
become vectors and null and false become the keywords `:null' and
`:false', so that `tumbel-http-encode' reproduces the document."
  (if tumbel-http--native-json
      (json-parse-string string
                         :object-type 'alist
                         :array-type (if fidelity 'array 'list)
                         :null-object (if fidelity :null nil)
                         :false-object (if fidelity :false nil))
    (let ((json-object-type 'alist)
          (json-array-type (if fidelity 'vector 'list))
          (json-key-type 'symbol)
          (json-null (if fidelity :null nil))
          (json-false (if fidelity :false nil)))
      (json-read-from-string string))))

(defun tumbel-http-encode (object)
  "Encode OBJECT as JSON text, returned as a multibyte string.
Alists (including nil) become objects, vectors become arrays, t
becomes true and the keywords `:false' and `:null' become false and
null.  Lists are never arrays: use a vector, and [] for an empty
one."
  (let ((text (if tumbel-http--native-json
                  (json-serialize object
                                  :null-object :null :false-object :false)
                (let ((json-null :null)
                      (json-false :false)
                      (json-encoding-pretty-print nil))
                  (json-encode object)))))
    ;; The native serializer returns UTF-8 bytes; callers want text.
    (if (multibyte-string-p text)
        text
      (decode-coding-string text 'utf-8))))

(defun tumbel-http-form-encode (params)
  "Encode the alist PARAMS as application/x-www-form-urlencoded text.
Keys are symbols or strings.  Every reserved character is escaped."
  (mapconcat (lambda (param)
               (concat (url-hexify-string (format "%s" (car param)))
                       "="
                       (url-hexify-string (format "%s" (cdr param)))))
             params "&"))
;;;; Errors

(defun tumbel-api-error-message (err)
  "Return the message of ERR, a `tumbel-api-error' in `condition-case' shape."
  (nth 1 err))

(defun tumbel-api-error-status (err)
  "Return the HTTP status of ERR, a `tumbel-api-error'."
  (nth 2 err))

(defun tumbel-api-error-errors (err)
  "Return the list of error alists carried by ERR, a `tumbel-api-error'."
  (nth 3 err))

(defun tumbel-http-error-string (err)
  "Return a one-line description of ERR, an error in `condition-case' shape."
  (let ((symbol (car-safe err))
        (data (cdr-safe err)))
    (cond
     ((eq symbol 'tumbel-api-error)
      (let ((details
             (mapconcat
              (lambda (entry)
                (mapconcat #'identity
                           (delq nil
                                 (list (let ((code (alist-get 'code entry)))
                                         (and code (format "%s" code)))
                                       (alist-get 'title entry)
                                       (alist-get 'detail entry)))
                           ": "))
              (tumbel-api-error-errors err)
              "; ")))
        (format "Tumblr API error %s: %s%s"
                (tumbel-api-error-status err)
                (tumbel-api-error-message err)
                (if (equal details "") "" (format " (%s)" details)))))
     ;; Emacs would print the string with quotes for these.
     ((and (symbolp symbol) (get symbol 'error-message)
           (stringp (car data)) (null (cdr data)))
      (format "%s: %s" (get symbol 'error-message) (car data)))
     (t (error-message-string err)))))

(defun tumbel-http-report (err else)
  "Hand ERR to ELSE, or show it in the echo area when ELSE is nil."
  (if else
      (funcall else err)
    (message "%s" (tumbel-http-error-string err))))

(defun tumbel-http-deliver (thunk then else)
  "Call THEN with the value of THUNK, or ELSE with the error it signals.
Only `tumbel-error' conditions are routed to ELSE; when ELSE is nil
they are reported with `message'.  THEN may be nil."
  (let (value ok)
    (condition-case err
        (setq value (funcall thunk)
              ok t)
      (tumbel-error (tumbel-http-report err else)))
    (when (and ok then)
      (funcall then value))))

;;;; Requests

(cl-defun tumbel-http-request (method url &key headers body (body-type 'text)
                                      (as 'json) then else
                                      (timeout tumbel-http-timeout))
  "Send an HTTP request and deliver the parsed body to THEN.
METHOD is one of the symbols `get', `post', `put' and `delete'; URL
is the full URL.  HEADERS is an alist of header names and values; a
User-Agent is added.  BODY is a string sent as-is, with BODY-TYPE
`text' or `binary' as understood by `plz'.  AS selects how the body
is interpreted: `json' (see `tumbel-http-parse-json'),
`json-fidelity' (its re-encodable variant), `string', `binary', or
a function called with the body string.

When THEN is the symbol `sync', wait for the response and return the
parsed body; an HTTP status outside 2xx signals `tumbel-api-error'
and a transport failure signals `tumbel-http-error'.  Otherwise
return the process object right away and later call THEN with the
parsed body, or ELSE with the error in `condition-case' shape; when
ELSE is nil the error is reported with `message'.  TIMEOUT is in
seconds."
  (let ((headers (cons (cons "User-Agent" tumbel-http-user-agent) headers))
        (parser (tumbel-http--parser as))
        (binary (eq as 'binary)))
    (if (eq then 'sync)
        (tumbel-http--finish
         (funcall tumbel-http-backend method url
                  :headers headers :body body :body-type body-type
                  :binary binary :timeout timeout :then 'sync)
         parser)
      (let ((deliver (lambda (result)
                       (tumbel-http-deliver
                        (lambda () (tumbel-http--finish result parser))
                        then else))))
        (funcall tumbel-http-backend method url
                 :headers headers :body body :body-type body-type
                 :binary binary :timeout timeout
                 :then deliver :else deliver)))))

(defun tumbel-http-cancel (process)
  "Cancel the request running in PROCESS without invoking callbacks."
  (when (process-live-p process)
    (process-put process 'tumbel-http-cancelled t)
    (kill-process process)))

(defun tumbel-http--cancelled-p (process)
  "Return non-nil when PROCESS was cancelled with `tumbel-http-cancel'."
  (and (processp process)
       (process-get process 'tumbel-http-cancelled)))

(defun tumbel-http--bytes (body)
  "Return BODY as a unibyte string.
plz leaves an undecoded body as the raw-byte characters of its
multibyte process buffer, which image loaders cannot read."
  (if (multibyte-string-p body)
      (string-to-unibyte body)
    body))

(defun tumbel-http--parser (as)
  "Return the function turning a response body into the value AS names."
  (pcase as
    ('json (lambda (body) (tumbel-http--parse-body body nil)))
    ('json-fidelity (lambda (body) (tumbel-http--parse-body body t)))
    ('string #'identity)
    ('binary #'tumbel-http--bytes)
    ((pred functionp) as)
    (_ (error "Unknown :as value %S" as))))

(defun tumbel-http--parse-body (body fidelity)
  "Parse BODY as JSON, in the re-encodable form when FIDELITY is non-nil.
An empty BODY is nil."
  (if (zerop (length body))
      nil
    (tumbel-http-parse-json body fidelity)))

(defun tumbel-http--finish (result parser)
  "Convert RESULT, a `plz-response' or `plz-error', with PARSER.
Return the parsed body of a 2xx response.  Signal `tumbel-api-error'
for any other HTTP status and `tumbel-http-error' when no response
was received."
  (let ((response (if (plz-error-p result)
                      (plz-error-response result)
                    result)))
    (cond
     ((null response)
      (signal 'tumbel-http-error
              (list (tumbel-http--transport-message result))))
     ((<= 200 (plz-response-status response) 299)
      (funcall parser (plz-response-body response)))
     (t
      (signal 'tumbel-api-error (tumbel-http--api-error-data response))))))

(defun tumbel-http--transport-message (err)
  "Describe the transport failure recorded in ERR, a `plz-error'."
  (let ((curl (and (plz-error-p err) (plz-error-curl-error err))))
    (cond
     ((and (plz-error-p err) (plz-error-message err)))
     ((and curl (cdr curl)) (format "curl: %s" (cdr curl)))
     (curl (format "curl exited with code %s" (car curl)))
     (t "Unknown transport error"))))

(defun tumbel-api-error-body (err)
  "Return the parsed response body carried by ERR, a `tumbel-api-error'."
  (nth 4 err))

(defun tumbel-http--api-error-data (response)
  "Return the `tumbel-api-error' data for RESPONSE.
The data is the list (MESSAGE STATUS ERRORS BODY)."
  (let* ((status (plz-response-status response))
         (body (ignore-errors
                 (tumbel-http--parse-body (plz-response-body response) nil)))
         (body (and (listp body) body))
         (meta (alist-get 'meta body)))
    (list (or (alist-get 'msg meta) (format "HTTP %s" status))
          status
          (alist-get 'errors body)
          body)))

;;;; The plz backend

(cl-defun tumbel-http--plz (method url &key headers body (body-type 'text)
                                   binary timeout then else)
  "Perform the request METHOD URL with `plz'.
HEADERS, BODY, BODY-TYPE, BINARY, TIMEOUT, THEN and ELSE are as
described in `tumbel-http-backend'."
  (if (eq then 'sync)
      (condition-case err
          (plz method url
            :headers headers :body body :body-type body-type
            :as 'response :decode (not binary) :timeout timeout
            :then 'sync)
        (plz-error (tumbel-http--plz-error err)))
    (let (process)
      (setq process
            (plz method url
              :headers headers :body body :body-type body-type
              :as 'response :decode (not binary) :timeout timeout
              :then (lambda (response)
                      (unless (tumbel-http--cancelled-p process)
                        (funcall then response)))
              :else (lambda (err)
                      (unless (tumbel-http--cancelled-p process)
                        (funcall else err)))))
      process)))

(defun tumbel-http--plz-error (err)
  "Return the `plz-error' carried by the signal ERR, or build one."
  (or (seq-find #'plz-error-p (cdr err))
      (make-plz-error :message (error-message-string err))))

(provide 'tumbel-http)
;;; tumbel-http.el ends here
