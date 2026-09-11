;;; tumblr-notifications.el --- Activity for tumblr.el  -*- lexical-binding: t; -*-

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
(require 'tumblr-api)
(require 'tumblr-feed)
(require 'tumblr-npf)
(require 'tumblr-user)

(defface tumblr-unread '((t :inherit bold))
  "Face for activity not seen yet."
  :group 'tumblr)

(defvar-local tumblr-notifications--blog nil
  "The blog whose activity this buffer shows.")

(defun tumblr-notifications--key (item)
  "Return the identity of the activity ITEM."
  (let-alist item
    (or (and .id (format "%s" .id))
        (format "%s/%s/%s/%s" .type .from_tumblelog_name .timestamp
                (or .target_post_id "")))))

(defun tumblr-notifications--description (item)
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

(defun tumblr-notifications-insert (item)
  "Insert the activity ITEM as one line, plus any text it carries."
  (let-alist item
    (let* ((who (if .from_tumblelog_name
                    (tumblr-npf-blog-button .from_tumblelog_name)
                  (propertize "someone" 'face 'tumblr-blog-name)))
           (line (concat who " "
                         (propertize (tumblr-notifications--description item)
                                     'face 'tumblr-meta)
                         (if .timestamp
                             (propertize
                              (concat " · "
                                      (tumblr-npf-relative-time .timestamp))
                              'face 'tumblr-meta)
                           ""))))
      (when .unread
        (add-face-text-property 0 (length line) 'tumblr-unread t line))
      (insert line "\n")
      (dolist (text (list .reply_text .added_text))
        (when (and text (not (string-empty-p text)))
          (insert text "\n")))
      (when .post_tags
        (insert (mapconcat #'tumblr-npf-tag-button .post_tags " ") "\n")))))

(defun tumblr-notifications--open (item)
  "Show what the activity ITEM is about."
  (let-alist item
    (cond ((and .from_tumblelog_name .post_id
                (member .type '("reblog_naked" "reblog_with_content"
                                "mention_in_post" "answered_ask")))
           (tumblr-feed-display
            (tumblr-feed-post-source .from_tumblelog_name
                                     (format "%s" .post_id))))
          ((and .target_post_id tumblr-notifications--blog)
           (tumblr-feed-display
            (tumblr-feed-post-source tumblr-notifications--blog
                                     (format "%s" .target_post_id))))
          (.from_tumblelog_name
           (funcall tumblr-npf-open-blog-function .from_tumblelog_name))
          (t (user-error "Nothing to open for this item")))))

(defun tumblr-notifications-source (blog)
  "Return the feed source for the activity of your blog BLOG."
  (tumblr-feed-source-create
   :name (format "notifications/%s" blog)
   :title (format "Activity on %s" blog)
   :kind 'notification
   :render #'tumblr-notifications-insert
   :key #'tumblr-notifications--key
   :open #'tumblr-notifications--open
   :fetch
   (lambda (cursor then else)
     (tumblr-api-notifications
      blog
      :params `((before . ,cursor))
      :then (lambda (response)
              (let* ((items (alist-get 'notifications response))
                     (last (car (last items)))
                     (next (or (alist-get 'before
                                          (tumblr-api-next-params response))
                               (and last (alist-get 'timestamp last)))))
                (funcall then items (and items next))))
      :else else))))

;;;###autoload
(defun tumblr-notifications (&optional blog)
  "Show the activity of your blog BLOG, the default blog when nil."
  (interactive
   (list (and current-prefix-arg
              (completing-read "Blog: " (tumblr-user-blog-names) nil t))))
  (let ((blog (or blog (tumblr-user-default-blog))))
    (with-current-buffer (tumblr-feed-display
                          (tumblr-notifications-source blog))
      (setq tumblr-notifications--blog blog)
      (current-buffer))))

(provide 'tumblr-notifications)
;;; tumblr-notifications.el ends here
