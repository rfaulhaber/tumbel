;;; tumbel-user.el --- The logged-in user for tumbel.el  -*- lexical-binding: t; -*-

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
(require 'tumbel-api)
(require 'tumbel-auth)

(defcustom tumbel-default-blog nil
  "Name of the blog to post from and to show activity for.
When nil, the primary blog of the account is used."
  :type '(choice (const :tag "Primary blog" nil) string)
  :group 'tumbel)

(defvar tumbel-user--info nil
  "The user alist of the logged-in user, once fetched.")

(defun tumbel-user-info (&optional refresh)
  "Return the user alist of the logged-in user.
It is fetched once and cached until REFRESH is non-nil or the login
changes."
  (when (or refresh (null tumbel-user--info))
    (setq tumbel-user--info (tumbel-api-user-info :then 'sync)))
  tumbel-user--info)

(defun tumbel-user-blogs ()
  "Return the blog alists of the logged-in user."
  (alist-get 'blogs (tumbel-user-info)))

(defun tumbel-user-blog-names ()
  "Return the names of the blogs of the logged-in user."
  (mapcar (lambda (blog) (alist-get 'name blog)) (tumbel-user-blogs)))

(defun tumbel-user-default-blog ()
  "Return the name of the blog to post from.
This is `tumbel-default-blog' when set, else the primary blog."
  (or tumbel-default-blog
      (alist-get 'name (seq-find (lambda (blog) (alist-get 'primary blog))
                                 (tumbel-user-blogs)))
      (car (tumbel-user-blog-names))))

(defun tumbel-user-own-blog-p (name)
  "Return non-nil when the logged-in user owns the blog NAME."
  (and (tumbel-auth-logged-in-p)
       (member name (tumbel-user-blog-names))
       t))

;;;; Filters

(defvar tumbel-user--filters nil
  "The filters of the account as (TAGS . CONTENT), once fetched.")

(defvar tumbel-user--filters-loading nil
  "Non-nil while the filters are being fetched.")

(defun tumbel-user-filters ()
  "Return the filters of the account as (TAGS . CONTENT), or nil.
Nil means they are not known yet; see `tumbel-user-load-filters'."
  tumbel-user--filters)

(defun tumbel-user-load-filters (then)
  "Fetch the filters of the account once, then call THEN with them.
THEN is called at once when they are already known.  Failures are
reported and leave the filters unknown."
  (cond (tumbel-user--filters (funcall then tumbel-user--filters))
        ((not (tumbel-auth-logged-in-p)) (funcall then nil))
        (tumbel-user--filters-loading nil)
        (t
         (setq tumbel-user--filters-loading t)
         (let ((fail (lambda (err)
                       (setq tumbel-user--filters-loading nil)
                       (message "tumbel: could not fetch filters: %s"
                                (tumbel-http-error-string err)))))
           (condition-case err
               (tumbel-api-filtered-tags
                :then (lambda (tags)
                        (tumbel-api-filtered-content
                         :then (lambda (content)
                                 (setq tumbel-user--filters-loading nil
                                       tumbel-user--filters (cons tags content))
                                 (funcall then tumbel-user--filters))
                         :else fail))
                :else fail)
             (error (funcall fail err)))))))
(defun tumbel-user-clear ()
  "Forget the cached user information."
  (setq tumbel-user--info nil
        tumbel-user--filters nil
        tumbel-user--filters-loading nil))

(add-hook 'tumbel-auth-login-hook #'tumbel-user-clear)
(add-hook 'tumbel-auth-logout-hook #'tumbel-user-clear)

(provide 'tumbel-user)
;;; tumbel-user.el ends here
