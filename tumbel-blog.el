;;; tumbel-blog.el --- Blog and tag views for tumbel.el  -*- lexical-binding: t; -*-

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

;; Feeds for the posts of one blog and for the posts carrying a tag,
;; both readable with only an API key, plus the header describing a
;; blog above its posts.

;;; Code:

(require 'let-alist)
(require 'shr)
(require 'subr-x)
(require 'tumbel-api)
(require 'tumbel-auth)
(require 'tumbel-feed)
(require 'tumbel-media)
(require 'tumbel-npf)

(defvar tumbel-blog-history nil
  "History of the blogs visited with `tumbel-blog'.")

(defvar tumbel-tag-history nil
  "History of the tags browsed with `tumbel-tag'.")

;;;; Sources

(defun tumbel-blog-source (blog &optional tag)
  "Return the feed source for the posts of BLOG, paged by offset.
With TAG, only the posts of BLOG carrying that tag."
  (tumbel-feed-source-create
   :name (if tag (format "blog/%s/tag/%s" blog tag) (format "blog/%s" blog))
   :title (if tag (format "%s · #%s" blog tag) blog)
   :fetch
   (lambda (cursor then else)
     (let ((offset (or cursor 0)))
       (tumbel-api-blog-posts
        blog
        :params `((limit . ,tumbel-feed-page-size)
                  (offset . ,offset)
                  (tag . ,tag)
                  (reblog_info . t))
        :then (lambda (response)
                (let* ((posts (alist-get 'posts response))
                       (total (alist-get 'total_posts response))
                       (next (+ offset (length posts))))
                  (funcall then posts
                           (and posts (or (null total) (< next total)) next))))
        :else else)))))

(defun tumbel-tag-source (tag)
  "Return the feed source for the posts tagged TAG, paged by timestamp."
  (tumbel-feed-source-create
   :name (format "tag/%s" tag)
   :title (concat "#" tag)
   :fetch
   (lambda (cursor then else)
     (tumbel-api-tagged
      tag
      :params `((limit . ,tumbel-feed-page-size) (before . ,cursor))
      :then (lambda (response)
              (let* ((posts (alist-get 'posts response))
                     (timestamp (alist-get 'timestamp (car (last posts)))))
                ;; The endpoint treats `before' inclusively, so step
                ;; past the last post seen rather than fetching it again.
                (funcall then posts (and timestamp (1- timestamp)))))
      :else else))))

(defconst tumbel-likes-offset-limit 1000
  "Deepest offset the likes endpoint accepts.")

(defun tumbel-likes-source ()
  "Return the feed source for the posts you liked.
Pages follow the like timestamps when posts carry them, and fall
back to offsets, which the endpoint caps."
  (tumbel-feed-source-create
   :name "likes"
   :title "Likes"
   :fetch
   (lambda (cursor then else)
     (tumbel-api-likes
      :params `((limit . ,tumbel-feed-page-size)
                (before . ,(and (eq (car-safe cursor) 'before) (cdr cursor)))
                (offset . ,(and (eq (car-safe cursor) 'offset) (cdr cursor))))
      :then (lambda (response)
              (let* ((posts (alist-get 'liked_posts response))
                     (offset (if (eq (car-safe cursor) 'offset) (cdr cursor) 0))
                     (timestamp (alist-get 'liked_timestamp
                                           (car (last posts))))
                     (next (cond ((null posts) nil)
                                 (timestamp (cons 'before timestamp))
                                 ((< (+ offset (length posts))
                                     tumbel-likes-offset-limit)
                                  (cons 'offset (+ offset (length posts)))))))
                (funcall then posts next)))
      :else else))))

;;;###autoload
(defun tumbel-likes ()
  "Browse the posts you liked."
  (interactive)
  (tumbel-feed-display (tumbel-likes-source)))
;;;; Blog header

(defun tumbel-blog-html-to-string (html)
  "Render the HTML fragment HTML as plain text."
  (if (or (null html) (string-empty-p html))
      ""
    (with-temp-buffer
      (if (libxml-available-p)
          (let ((dom (progn (insert html)
                            (libxml-parse-html-region (point-min)
                                                      (point-max)))))
            (erase-buffer)
            (let ((shr-width 72)
                  (shr-use-fonts nil)
                  (shr-inhibit-images t))
              (shr-insert-document dom)))
        (insert (replace-regexp-in-string "<[^>]*>" "" html)))
      (string-trim (buffer-string)))))

(defun tumbel-blog-header-string (info &optional tag)
  "Return the text heading a blog feed, built from the blog INFO.
TAG, when given, names the tag the feed is narrowed to."
  (let-alist info
    (let ((description (tumbel-blog-html-to-string .description))
          (posts (or .total_posts .posts))
          (avatar (alist-get 'url (tumbel-media-pick .avatar 64))))
      (concat (and avatar
                   (concat (tumbel-media-image-string avatar
                                                      (or .name "avatar"))
                           " "))
              (propertize (or .title .name "") 'face 'tumbel-heading1)
              (and tag (propertize (format " · #%s" tag) 'face 'tumbel-tag))
              "\n"
              (and .url (tumbel-npf-link-button .url .url))
              (and posts
                   (propertize (format " · %s posts" posts) 'face 'tumbel-meta))
              (and (tumbel-auth-logged-in-p) .followed
                   (propertize " · following" 'face 'tumbel-meta))
              "\n"
              (and (not (string-empty-p description))
                   (concat description "\n"))
              "\n"))))

(defun tumbel-blog--fetch-header (blog buffer &optional tag)
  "Fetch the info of BLOG and put it in the header of BUFFER.
TAG names the tag the feed is narrowed to, if any."
  (tumbel-api-blog-info
   blog
   :then (lambda (info)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (tumbel-feed-set-header (tumbel-blog-header-string info tag)))))
   :else (lambda (err)
           (message "%s" (tumbel-http-error-string err)))))

;;;; Commands

;;;###autoload
;;;###autoload
(defun tumbel-blog (blog &optional tag)
  "Browse the posts of BLOG, a blog name, hostname or URL.
With TAG, or interactively with a prefix argument, only the posts
of BLOG carrying that tag."
  (interactive
   (list (read-string "Blog: " nil 'tumbel-blog-history)
         (and current-prefix-arg
              (string-remove-prefix
               "#" (read-string "Tag: " nil 'tumbel-tag-history)))))
  (let* ((blog (tumbel-api-blog-identifier blog))
         (buffer (tumbel-feed-display (tumbel-blog-source blog tag))))
    (tumbel-blog--fetch-header blog buffer tag)
    buffer))

;;;###autoload
(defun tumbel-tag (tag)
  "Browse the posts tagged TAG."
  (interactive (list (read-string "Tag: " nil 'tumbel-tag-history)))
  (tumbel-feed-display (tumbel-tag-source (string-remove-prefix "#" tag))))

(provide 'tumbel-blog)
;;; tumbel-blog.el ends here
