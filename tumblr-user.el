;;; tumblr-user.el --- The logged-in user for tumblr.el  -*- lexical-binding: t; -*-

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

;; Caches the account information of the logged-in user: the blogs
;; they own and which one posts are made from.

;;; Code:

(require 'seq)
(require 'tumblr-api)
(require 'tumblr-auth)

(defcustom tumblr-default-blog nil
  "Name of the blog to post from and to show activity for.
When nil, the primary blog of the account is used."
  :type '(choice (const :tag "Primary blog" nil) string)
  :group 'tumblr)

(defvar tumblr-user--info nil
  "The user alist of the logged-in user, once fetched.")

(defun tumblr-user-info (&optional refresh)
  "Return the user alist of the logged-in user.
It is fetched once and cached until REFRESH is non-nil or the login
changes."
  (when (or refresh (null tumblr-user--info))
    (setq tumblr-user--info (tumblr-api-user-info :then 'sync)))
  tumblr-user--info)

(defun tumblr-user-blogs ()
  "Return the blog alists of the logged-in user."
  (alist-get 'blogs (tumblr-user-info)))

(defun tumblr-user-blog-names ()
  "Return the names of the blogs of the logged-in user."
  (mapcar (lambda (blog) (alist-get 'name blog)) (tumblr-user-blogs)))

(defun tumblr-user-default-blog ()
  "Return the name of the blog to post from.
This is `tumblr-default-blog' when set, else the primary blog."
  (or tumblr-default-blog
      (alist-get 'name (seq-find (lambda (blog) (alist-get 'primary blog))
                                 (tumblr-user-blogs)))
      (car (tumblr-user-blog-names))))

(defun tumblr-user-own-blog-p (name)
  "Return non-nil when the logged-in user owns the blog NAME."
  (and (tumblr-auth-logged-in-p)
       (member name (tumblr-user-blog-names))
       t))

;;;; Filters

(defvar tumblr-user--filters nil
  "The filters of the account as (TAGS . CONTENT), once fetched.")

(defvar tumblr-user--filters-loading nil
  "Non-nil while the filters are being fetched.")

(defun tumblr-user-filters ()
  "Return the filters of the account as (TAGS . CONTENT), or nil.
Nil means they are not known yet; see `tumblr-user-load-filters'."
  tumblr-user--filters)

(defun tumblr-user-load-filters (then)
  "Fetch the filters of the account once, then call THEN with them.
THEN is called at once when they are already known.  Failures are
reported and leave the filters unknown."
  (cond (tumblr-user--filters (funcall then tumblr-user--filters))
        ((not (tumblr-auth-logged-in-p)) (funcall then nil))
        (tumblr-user--filters-loading nil)
        (t
         (setq tumblr-user--filters-loading t)
         (let ((fail (lambda (err)
                       (setq tumblr-user--filters-loading nil)
                       (message "tumblr: could not fetch filters: %s"
                                (tumblr-http-error-string err)))))
           (condition-case err
               (tumblr-api-filtered-tags
                :then (lambda (tags)
                        (tumblr-api-filtered-content
                         :then (lambda (content)
                                 (setq tumblr-user--filters-loading nil
                                       tumblr-user--filters (cons tags content))
                                 (funcall then tumblr-user--filters))
                         :else fail))
                :else fail)
             (error (funcall fail err)))))))
(defun tumblr-user-clear ()
  "Forget the cached user information."
  (setq tumblr-user--info nil
        tumblr-user--filters nil
        tumblr-user--filters-loading nil))

(add-hook 'tumblr-auth-login-hook #'tumblr-user-clear)
(add-hook 'tumblr-auth-logout-hook #'tumblr-user-clear)

(provide 'tumblr-user)
;;; tumblr-user.el ends here
