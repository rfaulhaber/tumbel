;;; tumbel-notes.el --- Notes of a post for tumbel.el  -*- lexical-binding: t; -*-

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

;; The notes of a post (replies, reblogs and likes) as a feed of one
;; line per note.  Conversation mode, the default, shows replies and
;; reblogs that added something; the other modes the API offers can
;; be chosen with `tumbel-notes-switch-mode'.

;;; Code:

(require 'let-alist)
(require 'tumbel-api)
(require 'tumbel-feed)
(require 'tumbel-npf)

(defconst tumbel-notes-modes
  '("conversation" "all" "likes" "reblogs_with_tags" "rollup")
  "The note selections the API offers.")

(defvar-local tumbel-notes--post nil
  "The post whose notes this buffer shows.")

(defvar-local tumbel-notes--mode nil
  "The note selection this buffer shows.")

(defun tumbel-notes--key (note)
  "Return the identity of NOTE within a post."
  (let-alist note
    (format "%s/%s/%s/%s" .type .blog_name .timestamp (or .post_id ""))))

(defun tumbel-notes-insert (note)
  "Insert NOTE as one line."
  (let-alist note
    (let ((who (if .blog_name
                   (tumbel-npf-blog-button .blog_name)
                 (propertize "someone" 'face 'tumbel-blog-name)))
          (when (if .timestamp
                    (propertize (concat " · "
                                        (tumbel-npf-relative-time .timestamp))
                                'face 'tumbel-meta)
                  "")))
      (pcase .type
        ("like"
         (insert (propertize "♥ " 'face 'tumbel-meta) who
                 (propertize " liked this" 'face 'tumbel-meta) when "\n"))
        ("reblog"
         (insert (propertize "↻ " 'face 'tumbel-meta) who
                 (propertize " reblogged this" 'face 'tumbel-meta)
                 (if .reblog_parent_blog_name
                     (concat (propertize " from " 'face 'tumbel-meta)
                             (tumbel-npf-blog-button .reblog_parent_blog_name))
                   "")
                 when "\n")
         (when .added_text
           (insert .added_text "\n"))
         (when .tags
           (insert (mapconcat #'tumbel-npf-tag-button .tags " ") "\n")))
        ("reply"
         (insert who (propertize " replied" 'face 'tumbel-meta) when "\n"
                 (or .reply_text "") "\n"))
        ("posted"
         (insert who (propertize " posted this" 'face 'tumbel-meta) when
                 "\n"))
        (type
         (insert who (propertize (format " (%s)" type) 'face 'tumbel-meta)
                 when "\n"))))))

(defun tumbel-notes--open (note)
  "Show what NOTE refers to: the reblog, or the blog of its author."
  (let-alist note
    (if (and (equal .type "reblog") .post_id .blog_name)
        (tumbel-feed-display (tumbel-feed-post-source .blog_name
                                                      (format "%s" .post_id)))
      (funcall tumbel-npf-open-blog-function .blog_name))))

(defun tumbel-notes--header (post mode response)
  "Return the header describing the notes of POST in MODE, from RESPONSE."
  (let-alist response
    (concat (propertize (format "Notes on %s/%s"
                                (tumbel-npf-post-blog-name post)
                                (tumbel-npf-post-id post))
                        'face 'tumbel-heading1)
            "\n"
            (propertize
             (concat (format "%s notes" (or .total_notes "?"))
                     (and .total_likes (format " · %s likes" .total_likes))
                     (and .total_reblogs
                          (format " · %s reblogs" .total_reblogs))
                     (format " · showing %s" mode))
             'face 'tumbel-meta)
            "\n\n")))

(defun tumbel-notes-source (post mode)
  "Return the feed source for the notes of POST in MODE."
  (let ((blog (tumbel-npf-post-blog-name post))
        (id (tumbel-npf-post-id post)))
    (tumbel-feed-source-create
     :name (format "notes/%s" id)
     :title (format "Notes on %s/%s" blog id)
     :kind 'note
     :render #'tumbel-notes-insert
     :key #'tumbel-notes--key
     :open #'tumbel-notes--open
     :fetch
     (lambda (cursor then else)
       (let ((buffer (current-buffer)))
         (tumbel-api-notes
          blog id
          :params `((mode . ,mode) (before_timestamp . ,cursor))
          :then (lambda (response)
                  (let* ((notes (alist-get 'notes response))
                         (next (alist-get 'before_timestamp
                                          (tumbel-api-next-params response))))
                    (unless cursor
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (tumbel-feed-set-header
                           (tumbel-notes--header post mode response)))))
                    (funcall then notes (and notes next))))
          :else else))))))

(defun tumbel-notes (post &optional mode)
  "Show the notes of POST, selected by MODE (conversation by default)."
  (let ((mode (or mode "conversation")))
    (with-current-buffer (tumbel-feed-display (tumbel-notes-source post mode))
      (setq tumbel-notes--post post
            tumbel-notes--mode mode)
      (current-buffer))))

(defun tumbel-notes-switch-mode (mode)
  "Show the notes of this post selected by MODE instead."
  (interactive
   (list (completing-read "Notes to show: " tumbel-notes-modes nil t)))
  (unless tumbel-notes--post
    (user-error "This buffer does not show notes"))
  (tumbel-notes tumbel-notes--post mode))

(provide 'tumbel-notes)
;;; tumbel-notes.el ends here
