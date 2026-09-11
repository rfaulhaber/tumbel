;;; tumblr-lists.el --- Blog lists for tumblr.el  -*- lexical-binding: t; -*-

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
(require 'tumblr-api)
(require 'tumblr-npf)
(require 'tumblr-user)

(defcustom tumblr-lists-page-size 20
  "Number of blogs fetched per page; the API allows at most 20."
  :type 'integer
  :group 'tumblr)

(defvar-local tumblr-lists--fetch nil
  "Function fetching a page of the list.
It is called with the offset, a callback
receiving the entries and the total count.")

(defvar-local tumblr-lists--offset 0
  "Offset of the next page.")

(defvar-local tumblr-lists--total nil
  "Number of blogs the list holds in total, once known.")

(defvar-local tumblr-lists--loading nil
  "Non-nil while a page is being fetched.")

(defvar tumblr-lists-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'tumblr-lists-open)
    (define-key map (kbd "L") #'tumblr-lists-load-more)
    (define-key map (kbd "u") #'tumblr-lists-unfollow)
    map)
  "Keymap of `tumblr-lists-mode'.")

(define-derived-mode tumblr-lists-mode tabulated-list-mode "Tumblr-Blogs"
  "Major mode for tables of Tumblr blogs.
\\{tumblr-lists-mode-map}"
  (setq tabulated-list-format [("Blog" 24 t) ("Title" 30 t) ("Updated" 12 t)]
        tabulated-list-padding 1)
  (setq-local revert-buffer-function #'tumblr-lists-revert)
  (tabulated-list-init-header))

(defun tumblr-lists--entry (blog)
  "Return the table entry for the BLOG alist."
  (let-alist blog
    (list .name
          (vector (or .name "")
                  (or .title "")
                  (if .updated (tumblr-npf-relative-time .updated) "")))))

(defun tumblr-lists--display (name fetch)
  "Show the list NAME fed by FETCH and load its first page."
  (let ((buffer (get-buffer-create (format "*tumblr: %s*" name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'tumblr-lists-mode)
        (tumblr-lists-mode))
      (setq tumblr-lists--fetch fetch)
      (tumblr-lists-revert))
    (pop-to-buffer-same-window buffer)
    buffer))

(defun tumblr-lists-revert (&rest _)
  "Reload the list from its first page."
  (setq tabulated-list-entries nil
        tumblr-lists--offset 0
        tumblr-lists--total nil
        tumblr-lists--loading nil)
  (tabulated-list-print t)
  (tumblr-lists--load-page))

(defun tumblr-lists--load-page ()
  "Fetch the next page of the list unless one is on its way."
  (unless tumblr-lists--loading
    (let ((buffer (current-buffer)))
      (setq tumblr-lists--loading t)
      (funcall tumblr-lists--fetch tumblr-lists--offset
               (lambda (entries total)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq tumblr-lists--loading nil
                           tumblr-lists--total total
                           tumblr-lists--offset (+ tumblr-lists--offset
                                                   (length entries))
                           tabulated-list-entries (append
                                                   tabulated-list-entries
                                                   entries))
                     (tabulated-list-print t)
                     (tumblr-lists--report))))
               (lambda (err)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq tumblr-lists--loading nil)))
                 (message "%s" (tumblr-http-error-string err)))))))

(defun tumblr-lists-more-p ()
  "Return non-nil when the list has pages left to fetch."
  (or (null tumblr-lists--total)
      (< tumblr-lists--offset tumblr-lists--total)))

(defun tumblr-lists--report ()
  "Say how much of the list is shown."
  (message "%d of %s blogs%s" (length tabulated-list-entries)
           (or tumblr-lists--total "?")
           (if (tumblr-lists-more-p) "; L loads more" "")))

(defun tumblr-lists-load-more ()
  "Fetch the next page of the list."
  (interactive)
  (cond (tumblr-lists--loading (message "Still loading…"))
        ((not (tumblr-lists-more-p)) (message "Every blog is listed"))
        (t (tumblr-lists--load-page))))

(defun tumblr-lists-open ()
  "Open the blog at point."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (unless name
      (user-error "No blog here"))
    (funcall tumblr-npf-open-blog-function name)))

(defun tumblr-lists-unfollow ()
  "Unfollow the blog at point."
  (interactive)
  (let ((name (tabulated-list-get-id))
        (buffer (current-buffer)))
    (unless name
      (user-error "No blog here"))
    (when (yes-or-no-p (format "Unfollow %s? " name))
      (tumblr-api-unfollow
       (format "https://%s.tumblr.com/" name)
       :then (lambda (_)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq tabulated-list-entries
                         (cl-remove name tabulated-list-entries
                                    :key #'car :test #'equal))
                   (when tumblr-lists--total
                     (cl-decf tumblr-lists--total))
                   (tabulated-list-print t)))
               (message "Unfollowed %s" name))))))

;;;###autoload
(defun tumblr-following ()
  "List the blogs you follow."
  (interactive)
  (tumblr-lists--display
   "following"
   (lambda (offset then else)
     (tumblr-api-following
      :params `((limit . ,tumblr-lists-page-size) (offset . ,offset))
      :then (lambda (response)
              (funcall then
                       (mapcar #'tumblr-lists--entry
                               (alist-get 'blogs response))
                       (alist-get 'total_blogs response)))
      :else else))))

;;;###autoload
(defun tumblr-followers (&optional blog)
  "List the followers of your blog BLOG, the default blog when nil."
  (interactive
   (list (and current-prefix-arg
              (completing-read "Blog: " (tumblr-user-blog-names) nil t))))
  (let ((blog (or blog (tumblr-user-default-blog))))
    (tumblr-lists--display
     (format "followers/%s" blog)
     (lambda (offset then else)
       (tumblr-api-followers
        blog
        :params `((limit . ,tumblr-lists-page-size) (offset . ,offset))
        :then (lambda (response)
                (funcall then
                         (mapcar #'tumblr-lists--entry
                                 (alist-get 'users response))
                         (alist-get 'total_users response)))
        :else else)))))

(provide 'tumblr-lists)
;;; tumblr-lists.el ends here
