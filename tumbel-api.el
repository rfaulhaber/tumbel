;;; tumbel-api.el --- Tumblr API request layer  -*- lexical-binding: t; -*-

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

;; Builds Tumblr API v2 requests on top of `tumbel-http': URL and
;; query construction, the authentication levels, unwrapping of the
;; response envelope, and thin wrappers around individual endpoints.
;; Posts are always requested in the Neue Post Format (NPF).

;;; Code:

(require 'cl-lib)
(require 'let-alist)
(require 'mailcap)
(require 'mm-url)
(require 'subr-x)
(require 'url-util)
(require 'tumbel-auth)
(require 'tumbel-http)

(defconst tumbel-api-base-url "https://api.tumblr.com/v2"
  "Root of the Tumblr API, without a trailing slash.")

;;;; Building requests

(defun tumbel-api-blog-identifier (blog)
  "Return BLOG in the form the API accepts inside a path.
BLOG may be a blog name such as \"staff\", a hostname such as
\"staff.tumblr.com\", a UUID starting with \"t:\", or a blog URL."
  (let ((blog (string-trim blog)))
    (if (string-match "\\`https?://\\([^/]+\\)" blog)
        (match-string 1 blog)
      (string-remove-suffix "/" blog))))

(defun tumbel-api--query-value (value)
  "Return VALUE as the string to put in a query parameter."
  (cond ((eq value t) "true")
        ((eq value :false) "false")
        ((numberp value) (number-to-string value))
        ((symbolp value) (symbol-name value))
        (t value)))

(defun tumbel-api-query-string (params)
  "Encode the alist PARAMS as a query string.
Keys may be symbols or strings.  Entries whose value is nil are
omitted; t and `:false' become true and false."
  (url-build-query-string
   (cl-loop for (key . value) in params
            when value
            collect (list (if (symbolp key) (symbol-name key) key)
                          (tumbel-api--query-value value)))))

(defun tumbel-api--auth-headers (auth)
  "Return the request headers that satisfy the auth level AUTH."
  (pcase auth
    ('required
     (list (cons "Authorization"
                 (concat "Bearer " (tumbel-auth-access-token)))))
    ('optional
     (when (tumbel-auth-logged-in-p)
       (list (cons "Authorization"
                   (concat "Bearer " (tumbel-auth-access-token))))))
    ('nil nil)
    (_ (error "Unknown :auth value %S" auth))))

(defun tumbel-api--auth-params (auth)
  "Return the query parameters that satisfy the auth level AUTH."
  (when (and (eq auth 'optional) (not (tumbel-auth-logged-in-p)))
    (list (cons "api_key" (tumbel-auth-consumer-key)))))

(defun tumbel-api--unwrap (payload)
  "Return the response part of the API envelope PAYLOAD.
Signal `tumbel-api-error' when the envelope reports a failure."
  (let* ((meta (alist-get 'meta payload))
         (status (alist-get 'status meta)))
    (when (and (numberp status) (>= status 400))
      (signal 'tumbel-api-error
              (list (or (alist-get 'msg meta) (format "HTTP %s" status))
                    status
                    (alist-get 'errors payload)
                    payload)))
    (alist-get 'response payload)))

(cl-defun tumbel-api-request (method path &rest keys &key params body form
                                     files content-type (auth 'optional)
                                     transform fidelity then else retried
                                     &allow-other-keys)
  "Send an API request for PATH and deliver the response to THEN.
METHOD is `get', `post', `put' or `delete'.  PATH is relative to
`tumbel-api-base-url'.  KEYS are the keyword arguments described
next.  PARAMS is an alist of query parameters, see
`tumbel-api-query-string'.  BODY is a Lisp object encoded with
`tumbel-http-encode' and sent as UTF-8 JSON or, when CONTENT-TYPE
is given, a prebuilt unibyte string sent with that content type.
FORM, an alist, is sent form-encoded instead; the older endpoints
want their parameters that way.  FILES, an alist of media
identifiers to file names, turns the JSON body into a multipart
upload (see `tumbel-api-multipart').

AUTH is the authentication level: `optional' uses the OAuth token
when logged in and the API key otherwise, `required' insists on the
token, and nil sends neither.  TRANSFORM, when non-nil, is applied
to the unwrapped response before delivery.  With FIDELITY non-nil
the JSON is parsed in the re-encodable form described in
`tumbel-http-parse-json'.  THEN and ELSE work as in
`tumbel-http-request': THEN may be the symbol `sync', in which case
the response is returned and errors are signaled.

A request refused with 401 while a token was sent is repeated once
after refreshing the token; RETRIED marks that second attempt."
  (let* ((bearer (and auth (tumbel-auth-logged-in-p)))
         (params (append params (tumbel-api--auth-params auth)))
         (query (tumbel-api-query-string params))
         (url (concat tumbel-api-base-url path
                      (if (string-empty-p query) "" (concat "?" query))))
         (upload (and files (tumbel-api-multipart (tumbel-http-encode body)
                                                  files)))
         (content-type (cond (content-type)
                             (upload (car upload))
                             (form "application/x-www-form-urlencoded")
                             (body "application/json")))
         (headers (append (tumbel-api--auth-headers auth)
                          (when content-type
                            (list (cons "Content-Type" content-type)))))
         (body (cond (upload (cdr upload))
                     (form (tumbel-http-form-encode form))
                     ((null body) nil)
                     ((stringp body) body)
                     (t (encode-coding-string (tumbel-http-encode body)
                                              'utf-8))))
         (as (if fidelity 'json-fidelity 'json))
         (finish (lambda (payload)
                   (let ((response (tumbel-api--unwrap payload)))
                     (if transform
                         (funcall transform response)
                       response))))
         (expired-p (lambda (err)
                      (and bearer (not retried)
                           (eq (car-safe err) 'tumbel-api-error)
                           (eql (tumbel-api-error-status err) 401))))
         (retry (lambda ()
                  (apply #'tumbel-api-request method path :retried t keys))))
    (if (eq then 'sync)
        (condition-case err
            (funcall finish
                     (tumbel-http-request method url
                                          :headers headers :body body
                                          :body-type 'binary :as as
                                          :then 'sync))
          (tumbel-api-error
           (if (funcall expired-p err)
               (progn (tumbel-auth-refresh)
                      (funcall retry))
             (signal (car err) (cdr err)))))
      (tumbel-http-request method url
                           :headers headers :body body
                           :body-type 'binary :as as
                           :then (lambda (payload)
                                   (tumbel-http-deliver
                                    (lambda () (funcall finish payload))
                                    then else))
                           :else (lambda (err)
                                   (if (funcall expired-p err)
                                       (tumbel-auth-refresh-async
                                        retry
                                        (lambda (refresh-err)
                                          (tumbel-http-report refresh-err
                                                              else)))
                                     (tumbel-http-report err else)))))))

(defun tumbel-api-next-params (response)
  "Return the query parameters for the next page of RESPONSE, or nil.
They come from the `_links' entry some endpoints include."
  (let-alist response ._links.next.query_params))

;;;; Endpoints

(defun tumbel-api--with-params (keys params)
  "Return KEYS with PARAMS prepended to its `:params' entry."
  (let ((keys (copy-sequence keys)))
    (plist-put keys :params (append params (plist-get keys :params)))))

(defun tumbel-api-blog-info (blog &rest keys)
  "Fetch the info of BLOG and deliver the blog alist.
KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'get
         (format "/blog/%s/info" (tumbel-api-blog-identifier blog))
         :transform (lambda (response) (alist-get 'blog response))
         keys))

(defun tumbel-api-blog-posts (blog &rest keys)
  "Fetch posts of BLOG in NPF and deliver the response alist.
The alist holds `blog', `posts' and `total_posts'.  KEYS are passed
to `tumbel-api-request'; use `:params' for `limit', `offset', `tag'
and the other filters."
  (apply #'tumbel-api-request 'get
         (format "/blog/%s/posts" (tumbel-api-blog-identifier blog))
         (tumbel-api--with-params keys '((npf . t)))))

(defun tumbel-api-blog-post (blog post-id &rest keys)
  "Fetch the single post POST-ID of BLOG in NPF and deliver it.
KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'get
         (format "/blog/%s/posts/%s"
                 (tumbel-api-blog-identifier blog) post-id)
         (tumbel-api--with-params keys '((post_format . "npf")))))

(defun tumbel-api-tagged (tag &rest keys)
  "Fetch posts tagged TAG and deliver an alist with a `posts' entry.
The endpoint answers with a bare array; it is wrapped so callers see
the same shape as `tumbel-api-blog-posts'.  KEYS are passed to
`tumbel-api-request'."
  (apply #'tumbel-api-request 'get "/tagged"
         :transform #'tumbel-api--wrap-posts
         (tumbel-api--with-params keys `((tag . ,tag) (npf . t)))))

(defun tumbel-api--wrap-posts (response)
  "Return RESPONSE as an alist with a `posts' entry.
RESPONSE is either such an alist already or a bare list of posts."
  (cond ((null response) (list (cons 'posts nil)))
        ((and (consp (car response)) (consp (caar response)))
         (list (cons 'posts response)))
        (t response)))

(defun tumbel-api-user-info (&rest keys)
  "Fetch the info of the logged-in user and deliver the user alist.
KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'get "/user/info"
         :auth 'required
         :transform (lambda (response) (alist-get 'user response))
         keys))

(defun tumbel-api-dashboard (&rest keys)
  "Fetch dashboard posts in NPF and deliver the response alist.
KEYS are passed to `tumbel-api-request'; use `:params' for `limit',
`offset' and `since_id'."
  (apply #'tumbel-api-request 'get "/user/dashboard"
         :auth 'required
         (tumbel-api--with-params keys '((npf . t)))))
;;;; Posts and actions

(defun tumbel-api--post-blog-name (post)
  "Return the name of the blog POST belongs to, or nil."
  (or (alist-get 'blog_name post)
      (alist-get 'name (alist-get 'blog post))))

(defun tumbel-api--post-id (post)
  "Return the id of POST as a string, or nil."
  (or (alist-get 'id_string post)
      (let ((id (alist-get 'id post)))
        (and id (format "%s" id)))))

(defvar tumbel-api--blog-uuids (make-hash-table :test #'equal)
  "UUIDs of blogs, by name, learned from blog info requests.")

(defun tumbel-api-blog-uuid (name)
  "Return the UUID of the blog NAME, fetching its info once."
  (or (gethash name tumbel-api--blog-uuids)
      (let ((uuid (alist-get 'uuid (tumbel-api-blog-info name :then 'sync))))
        (unless uuid
          (signal 'tumbel-api-error
                  (list (format "No UUID known for blog %s" name)
                        nil nil nil)))
        (puthash name uuid tumbel-api--blog-uuids))))

(defun tumbel-api-post-blog-uuid (post)
  "Return the UUID of the blog POST belongs to.
Posts usually carry it; otherwise the blog info is fetched, once
per blog."
  (or (alist-get 'uuid (alist-get 'blog post))
      (alist-get 'tumblelog_uuid post)
      (tumbel-api-blog-uuid (tumbel-api--post-blog-name post))))

(defun tumbel-api-content-type (file)
  "Return the media type of FILE from its extension."
  (or (mailcap-extension-to-mime (file-name-extension file t))
      "application/octet-stream"))

(defun tumbel-api-multipart (json files)
  "Return (CONTENT-TYPE . BODY) uploading FILES alongside the JSON text.
FILES is an alist of media identifiers to file names; each file is
sent as a part named by its identifier, which is how NPF media
objects refer to uploads."
  (let* ((boundary (format "tumbel-el-%s"
                           (md5 (format "%s %s" (random) (float-time)))))
         (parts (cons (cons "json" (encode-coding-string json 'utf-8))
                      (mapcar (lambda (file)
                                (cons "file"
                                      (list (cons "name" (car file))
                                            (cons "filename"
                                                  (file-name-nondirectory
                                                   (cdr file)))
                                            (cons "content-type"
                                                  (tumbel-api-content-type
                                                   (cdr file)))
                                            (cons "filedata"
                                                  (tumbel-api--file-bytes
                                                   (cdr file))))))
                              files))))
    (cons (format "multipart/form-data; boundary=%s" boundary)
          (mm-url-encode-multipart-form-data parts boundary))))

(defun tumbel-api--file-bytes (file)
  "Return the contents of FILE as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun tumbel-api-like (post &rest keys)
  "Like POST.  KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'post "/user/like"
         :auth 'required
         :form `((id . ,(tumbel-api--post-id post))
                 (reblog_key . ,(alist-get 'reblog_key post)))
         keys))

(defun tumbel-api-unlike (post &rest keys)
  "Unlike POST.  KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'post "/user/unlike"
         :auth 'required
         :form `((id . ,(tumbel-api--post-id post))
                 (reblog_key . ,(alist-get 'reblog_key post)))
         keys))

(defun tumbel-api-follow (url &rest keys)
  "Follow the blog at URL.  KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'post "/user/follow"
         :auth 'required :form `((url . ,url)) keys))

(defun tumbel-api-unfollow (url &rest keys)
  "Unfollow the blog at URL.  KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'post "/user/unfollow"
         :auth 'required :form `((url . ,url)) keys))

(defun tumbel-api-create-post (blog post &rest keys)
  "Create POST, an NPF post alist with vectors for arrays, on BLOG.
The response holds the `id' of the new post.  KEYS are passed to
`tumbel-api-request'."
  (apply #'tumbel-api-request 'post
         (format "/blog/%s/posts" (tumbel-api-blog-identifier blog))
         :auth 'required :body post keys))

(defun tumbel-api-edit-post (blog id post &rest keys)
  "Replace the post ID of BLOG with POST, an NPF post alist.
KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'put
         (format "/blog/%s/posts/%s" (tumbel-api-blog-identifier blog) id)
         :auth 'required :body post keys))

(defun tumbel-api-delete-post (blog id &rest keys)
  "Delete the post ID of BLOG.  KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'post
         (format "/blog/%s/post/delete" (tumbel-api-blog-identifier blog))
         :auth 'required :form `((id . ,id)) keys))

(cl-defun tumbel-api-reblog (blog post &rest keys &key tags content
                                  &allow-other-keys)
  "Reblog POST to BLOG.
CONTENT is a vector of NPF blocks added as the comment (none by
default) and TAGS a list of tag strings.  KEYS are passed to
`tumbel-api-request'."
  (apply #'tumbel-api-create-post blog
         `((content . ,(or content []))
           (parent_tumblelog_uuid . ,(tumbel-api-post-blog-uuid post))
           (parent_post_id . ,(tumbel-api--post-id post))
           (reblog_key . ,(alist-get 'reblog_key post))
           ,@(when tags
               `((tags . ,(mapconcat #'identity tags ",")))))
         keys))
(defun tumbel-api-notes (blog id &rest keys)
  "Fetch the notes of the post ID of BLOG and deliver the response alist.
The alist holds `notes', `total_notes' and, in conversation mode,
`total_likes' and `total_reblogs'.  KEYS are passed to
`tumbel-api-request'; use `:params' for `mode' and `before_timestamp'."
  (apply #'tumbel-api-request 'get
         (format "/blog/%s/notes" (tumbel-api-blog-identifier blog))
         (tumbel-api--with-params keys `((id . ,id)))))

(defun tumbel-api-notifications (blog &rest keys)
  "Fetch the activity of your blog BLOG and deliver the response alist.
The alist holds `notifications' and `_links'.  KEYS are passed to
`tumbel-api-request'; use `:params' for `before' and `types'."
  (apply #'tumbel-api-request 'get
         (format "/blog/%s/notifications" (tumbel-api-blog-identifier blog))
         :auth 'required
         keys))
(defun tumbel-api-likes (&rest keys)
  "Fetch the posts you liked, in NPF, and deliver the response alist.
The alist holds `liked_posts' and `liked_count'.  KEYS are passed to
`tumbel-api-request'; use `:params' for `limit', `offset' and
`before'."
  (apply #'tumbel-api-request 'get "/user/likes"
         :auth 'required
         (tumbel-api--with-params keys '((npf . t)))))

(defun tumbel-api-following (&rest keys)
  "Fetch the blogs you follow and deliver the response alist.
The alist holds `blogs' and `total_blogs'.  KEYS are passed to
`tumbel-api-request'; use `:params' for `limit' and `offset'."
  (apply #'tumbel-api-request 'get "/user/following" :auth 'required keys))

(defun tumbel-api-followers (blog &rest keys)
  "Fetch the followers of your blog BLOG and deliver the response alist.
The alist holds `users' and `total_users'.  KEYS are passed to
`tumbel-api-request'; use `:params' for `limit' and `offset'."
  (apply #'tumbel-api-request 'get
         (format "/blog/%s/followers" (tumbel-api-blog-identifier blog))
         :auth 'required keys))
(defun tumbel-api-queue (blog &rest keys)
  "Fetch the queued posts of your blog BLOG in NPF and deliver the alist.
KEYS are passed to `tumbel-api-request'; use `:params' for `limit'
and `offset'."
  (apply #'tumbel-api-request 'get
         (format "/blog/%s/posts/queue" (tumbel-api-blog-identifier blog))
         :auth 'required
         (tumbel-api--with-params keys '((npf . t)))))

(defun tumbel-api-drafts (blog &rest keys)
  "Fetch the drafts of your blog BLOG in NPF and deliver the alist.
KEYS are passed to `tumbel-api-request'; use `:params' for
`before_id'."
  (apply #'tumbel-api-request 'get
         (format "/blog/%s/posts/draft" (tumbel-api-blog-identifier blog))
         :auth 'required
         (tumbel-api--with-params keys '((npf . t)))))

(defun tumbel-api-submissions (blog &rest keys)
  "Fetch the inbox of your blog BLOG in NPF and deliver the alist.
KEYS are passed to `tumbel-api-request'; use `:params' for `offset'."
  (apply #'tumbel-api-request 'get
         (format "/blog/%s/posts/submission"
                 (tumbel-api-blog-identifier blog))
         :auth 'required
         (tumbel-api--with-params keys '((npf . t)))))

(defun tumbel-api-queue-reorder (blog post-id insert-after &rest keys)
  "Move the queued post POST-ID of BLOG after the post INSERT-AFTER.
INSERT-AFTER 0 moves it to the front.  KEYS are passed to
`tumbel-api-request'."
  (apply #'tumbel-api-request 'post
         (format "/blog/%s/posts/queue/reorder"
                 (tumbel-api-blog-identifier blog))
         :auth 'required
         :form `((post_id . ,post-id) (insert_after . ,insert-after))
         keys))

(defun tumbel-api-queue-shuffle (blog &rest keys)
  "Shuffle the queue of BLOG.  KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'post
         (format "/blog/%s/posts/queue/shuffle"
                 (tumbel-api-blog-identifier blog))
         :auth 'required
         :form '((shuffle . "true"))
         keys))
(defun tumbel-api-filtered-tags (&rest keys)
  "Fetch the tags you filter and deliver the list of them.
KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'get "/user/filtered_tags"
         :auth 'required
         :transform (lambda (response) (alist-get 'filtered_tags response))
         keys))

(defun tumbel-api-filtered-content (&rest keys)
  "Fetch the content strings you filter and deliver the list of them.
KEYS are passed to `tumbel-api-request'."
  (apply #'tumbel-api-request 'get "/user/filtered_content"
         :auth 'required
         :transform (lambda (response) (alist-get 'filtered_content response))
         keys))
(provide 'tumbel-api)
;;; tumbel-api.el ends here
