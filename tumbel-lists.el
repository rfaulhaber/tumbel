;;; tumbel-lists.el --- Blog lists for tumbel.el  -*- lexical-binding: t; -*-

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

;; Tables of blogs: the blogs you follow and the followers of one of
;; your blogs.  RET opens a blog, u unfollows one, and L fetches the
;; next page.

;;; Code:

(require 'cl-lib)
(require 'let-alist)
(require 'tabulated-list)
(require 'tumbel-api)
(require 'tumbel-npf)
(require 'tumbel-user)

(defcustom tumbel-lists-page-size 20
  "Number of blogs fetched per page; the API allows at most 20."
  :type 'integer
  :group 'tumbel)

(defvar-local tumbel-lists--fetch nil
  "Function fetching a page of the list.
It is called with the offset, a callback
receiving the entries and the total count.")

(defvar-local tumbel-lists--offset 0
  "Offset of the next page.")

(defvar-local tumbel-lists--total nil
  "Number of blogs the list holds in total, once known.")

(defvar-local tumbel-lists--loading nil
  "Non-nil while a page is being fetched.")

(defvar tumbel-lists-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'tumbel-lists-open)
    (define-key map (kbd "L") #'tumbel-lists-load-more)
    (define-key map (kbd "o") #'tumbel-lists-browse-url)
    (define-key map (kbd "u") #'tumbel-lists-unfollow)
    (define-key map (kbd "y") #'tumbel-lists-copy-url)
    map)
  "Keymap of `tumbel-lists-mode'.")

(define-derived-mode tumbel-lists-mode tabulated-list-mode "Tumbel-Blogs"
  "Major mode for tables of Tumblr blogs.
\\{tumbel-lists-mode-map}"
  (setq tabulated-list-format [("Blog" 24 t) ("Title" 30 t) ("Updated" 12 t)]
        tabulated-list-padding 1)
  (setq-local revert-buffer-function #'tumbel-lists-revert)
  (tabulated-list-init-header))

(defun tumbel-lists--entry (blog)
  "Return the table entry for the BLOG alist."
  (let-alist blog
    (list .name
          (vector (or .name "")
                  (or .title "")
                  (if .updated (tumbel-npf-relative-time .updated) "")))))

(defun tumbel-lists--display (name fetch)
  "Show the list NAME fed by FETCH and load its first page."
  (let ((buffer (get-buffer-create (format "*tumbel: %s*" name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'tumbel-lists-mode)
        (tumbel-lists-mode))
      (setq tumbel-lists--fetch fetch)
      (tumbel-lists-revert))
    (pop-to-buffer-same-window buffer)
    buffer))

(defun tumbel-lists-revert (&rest _)
  "Reload the list from its first page."
  (setq tabulated-list-entries nil
        tumbel-lists--offset 0
        tumbel-lists--total nil
        tumbel-lists--loading nil)
  (tabulated-list-print t)
  (tumbel-lists--load-page))

(defun tumbel-lists--load-page ()
  "Fetch the next page of the list unless one is on its way."
  (unless tumbel-lists--loading
    (let ((buffer (current-buffer)))
      (setq tumbel-lists--loading t)
      (funcall tumbel-lists--fetch tumbel-lists--offset
               (lambda (entries total)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq tumbel-lists--loading nil
                           tumbel-lists--total total
                           tumbel-lists--offset (+ tumbel-lists--offset
                                                   (length entries))
                           tabulated-list-entries (append
                                                   tabulated-list-entries
                                                   entries))
                     (tabulated-list-print t)
                     (tumbel-lists--report))))
               (lambda (err)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq tumbel-lists--loading nil)))
                 (message "%s" (tumbel-http-error-string err)))))))

(defun tumbel-lists-more-p ()
  "Return non-nil when the list has pages left to fetch."
  (or (null tumbel-lists--total)
      (< tumbel-lists--offset tumbel-lists--total)))

(defun tumbel-lists--report ()
  "Say how much of the list is shown."
  (message "%d of %s blogs%s" (length tabulated-list-entries)
           (or tumbel-lists--total "?")
           (if (tumbel-lists-more-p) "; L loads more" "")))

(defun tumbel-lists-load-more ()
  "Fetch the next page of the list."
  (interactive)
  (cond (tumbel-lists--loading (message "Still loading…"))
        ((not (tumbel-lists-more-p)) (message "Every blog is listed"))
        (t (tumbel-lists--load-page))))

(defun tumbel-lists--name-at-point ()
  "Return the name of the blog at point, or signal a user error."
  (or (tabulated-list-get-id)
      (user-error "No blog here")))

(defun tumbel-lists-open ()
  "Open the blog at point."
  (interactive)
  (funcall tumbel-npf-open-blog-function (tumbel-lists--name-at-point)))

(defun tumbel-lists-browse-url ()
  "Open the blog at point in the browser."
  (interactive)
  (funcall tumbel-npf-open-url-function
           (tumbel-npf-blog-url (tumbel-lists--name-at-point))))

(defun tumbel-lists-copy-url ()
  "Copy the URL of the blog at point to the kill ring."
  (interactive)
  (let ((url (tumbel-npf-blog-url (tumbel-lists--name-at-point))))
    (kill-new url)
    (message "Copied %s" url)))

(defun tumbel-lists-unfollow ()
  "Unfollow the blog at point."
  (interactive)
  (let ((name (tabulated-list-get-id))
        (buffer (current-buffer)))
    (unless name
      (user-error "No blog here"))
    (when (yes-or-no-p (format "Unfollow %s? " name))
      (tumbel-api-unfollow
       (format "https://%s.tumblr.com/" name)
       :then (lambda (_)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq tabulated-list-entries
                         (cl-remove name tabulated-list-entries
                                    :key #'car :test #'equal))
                   (when tumbel-lists--total
                     (cl-decf tumbel-lists--total))
                   (tabulated-list-print t)))
               (message "Unfollowed %s" name))))))

;;;###autoload
(defun tumbel-following ()
  "List the blogs you follow."
  (interactive)
  (tumbel-lists--display
   "following"
   (lambda (offset then else)
     (tumbel-api-following
      :params `((limit . ,tumbel-lists-page-size) (offset . ,offset))
      :then (lambda (response)
              (funcall then
                       (mapcar #'tumbel-lists--entry
                               (alist-get 'blogs response))
                       (alist-get 'total_blogs response)))
      :else else))))

;;;###autoload
(defun tumbel-followers (&optional blog)
  "List the followers of your blog BLOG, the default blog when nil."
  (interactive
   (list (and current-prefix-arg
              (completing-read "Blog: " (tumbel-user-blog-names) nil t))))
  (let ((blog (or blog (tumbel-user-default-blog))))
    (tumbel-lists--display
     (format "followers/%s" blog)
     (lambda (offset then else)
       (tumbel-api-followers
        blog
        :params `((limit . ,tumbel-lists-page-size) (offset . ,offset))
        :then (lambda (response)
                (funcall then
                         (mapcar #'tumbel-lists--entry
                                 (alist-get 'users response))
                         (alist-get 'total_users response)))
        :else else)))))

(provide 'tumbel-lists)
;;; tumbel-lists.el ends here
