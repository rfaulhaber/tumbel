;;; tumbel-notifications.el --- Activity for tumbel.el  -*- lexical-binding: t; -*-

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

;; The activity of one of your blogs: likes, reblogs, replies, new
;; followers, asks and mentions, one line each, newest first.  RET
;; opens the post an item is about, or the blog of whoever acted.

;;; Code:

(require 'let-alist)
(require 'tumbel-api)
(require 'tumbel-feed)
(require 'tumbel-npf)
(require 'tumbel-user)

(defface tumbel-unread '((t :inherit bold))
  "Face for activity not seen yet."
  :group 'tumbel)

(defvar-local tumbel-notifications--blog nil
  "The blog whose activity this buffer shows.")

(defun tumbel-notifications--key (item)
  "Return the identity of the activity ITEM."
  (let-alist item
    (or (and .id (format "%s" .id))
        (format "%s/%s/%s/%s" .type .from_tumblelog_name .timestamp
                (or .target_post_id "")))))

(defun tumbel-notifications--description (item)
  "Return the words describing what happened in ITEM."
  (let-alist item
    (pcase .type
      ("like" "liked your post")
      ("reblog_naked" "reblogged your post")
      ("reblog_with_content" "reblogged your post and added")
      ("reply" "replied to your post")
      ("follow" "followed you")
      ("ask" "asked you a question")
      ("answered_ask" "answered your ask")
      ("mention_in_post" "mentioned you in a post")
      ("mention_in_reply" "mentioned you in a reply")
      ("post_attribution" "used your content")
      ("conversational_note" "continued a conversation")
      ("post_flagged" "flagged a post of yours")
      ("new_group_blog_member" "joined your blog")
      (type (format "(%s)" (or type "activity"))))))

(defun tumbel-notifications-insert (item)
  "Insert the activity ITEM as one line, plus any text it carries."
  (let-alist item
    (let* ((who (if .from_tumblelog_name
                    (tumbel-npf-blog-button .from_tumblelog_name)
                  (propertize "someone" 'face 'tumbel-blog-name)))
           (line (concat who " "
                         (propertize (tumbel-notifications--description item)
                                     'face 'tumbel-meta)
                         (if .timestamp
                             (propertize
                              (concat " · "
                                      (tumbel-npf-relative-time .timestamp))
                              'face 'tumbel-meta)
                           ""))))
      (when .unread
        (add-face-text-property 0 (length line) 'tumbel-unread t line))
      (insert line "\n")
      (dolist (text (list .reply_text .added_text))
        (when (and text (not (string-empty-p text)))
          (insert text "\n")))
      (when .post_tags
        (insert (mapconcat #'tumbel-npf-tag-button .post_tags " ") "\n")))))

(defun tumbel-notifications--open (item)
  "Show what the activity ITEM is about."
  (let-alist item
    (cond ((and .from_tumblelog_name .post_id
                (member .type '("reblog_naked" "reblog_with_content"
                                "mention_in_post" "answered_ask")))
           (tumbel-feed-display
            (tumbel-feed-post-source .from_tumblelog_name
                                     (format "%s" .post_id))))
          ((and .target_post_id tumbel-notifications--blog)
           (tumbel-feed-display
            (tumbel-feed-post-source tumbel-notifications--blog
                                     (format "%s" .target_post_id))))
          (.from_tumblelog_name
           (funcall tumbel-npf-open-blog-function .from_tumblelog_name))
          (t (user-error "Nothing to open for this item")))))

(defun tumbel-notifications-source (blog)
  "Return the feed source for the activity of your blog BLOG."
  (tumbel-feed-source-create
   :name (format "notifications/%s" blog)
   :title (format "Activity on %s" blog)
   :kind 'notification
   :render #'tumbel-notifications-insert
   :key #'tumbel-notifications--key
   :open #'tumbel-notifications--open
   :fetch
   (lambda (cursor then else)
     (tumbel-api-notifications
      blog
      :params `((before . ,cursor))
      :then (lambda (response)
              (let* ((items (alist-get 'notifications response))
                     (last (car (last items)))
                     (next (or (alist-get 'before
                                          (tumbel-api-next-params response))
                               (and last (alist-get 'timestamp last)))))
                (funcall then items (and items next))))
      :else else))))

;;;###autoload
(defun tumbel-notifications (&optional blog)
  "Show the activity of your blog BLOG, the default blog when nil."
  (interactive
   (list (and current-prefix-arg
              (completing-read "Blog: " (tumbel-user-blog-names) nil t))))
  (let ((blog (or blog (tumbel-user-default-blog))))
    (with-current-buffer (tumbel-feed-display
                          (tumbel-notifications-source blog))
      (setq tumbel-notifications--blog blog)
      (current-buffer))))

(provide 'tumbel-notifications)
;;; tumbel-notifications.el ends here
