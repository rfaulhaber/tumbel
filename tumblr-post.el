;;; tumblr-post.el --- Post Org files and entries  -*- lexical-binding: t; -*-

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

;; `tumblr-post-buffer' and `tumblr-post-subtree' send Org text that
;; already exists, a whole file or one entry of it, as a post through
;; the converter and request builder of the compose buffer.  The id of
;; a created post is written back into the source so that running the
;; command again edits that post instead of creating another one.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'tumblr-api)
(require 'tumblr-compose)
(require 'tumblr-http)
(require 'tumblr-user)

(defconst tumblr-post-keywords
  '("BLOG" "TAGS" "FILETAGS" "STATE" "PUBLISH_ON" "TUMBLR_ID" "TITLE")
  "The file keywords describing a post.")

;;;; Reading the source

(defun tumblr-post--keyword (keywords name)
  "Return the value of NAME among the collected KEYWORDS, or nil when blank."
  (let ((value (string-trim (or (cadr (assoc name keywords)) ""))))
    (and (not (string-empty-p value)) value)))

(defun tumblr-post--file-tags (keywords)
  "Return the tags named by KEYWORDS: #+tags: first, else #+filetags:."
  (let ((tags (tumblr-post--keyword keywords "TAGS"))
        (filetags (tumblr-post--keyword keywords "FILETAGS")))
    (cond (tags (split-string tags "," t "[ \t]+"))
          (filetags (split-string filetags ":" t "[ \t]+")))))

(defun tumblr-post--check-buffer ()
  "Signal a user error unless the current buffer can be posted."
  (tumblr-compose--require-login)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (when (derived-mode-p 'tumblr-compose-mode)
    (user-error "Send a compose buffer with C-c C-c")))

(defun tumblr-post--buffer-body (title)
  "Return the buffer text as a post body led by TITLE when given.
Under a title, the headlines of the buffer become subheadings."
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (if title
        (concat "* " title "\n"
                (replace-regexp-in-string "^\\*+ " "*\\&" text))
      text)))

(defun tumblr-post--subtree-body ()
  "Return the subtree of the entry at point as a top-level entry."
  (save-excursion
    (org-back-to-heading t)
    (let* ((stars (org-outline-level))
           (start (point))
           (text (progn (org-end-of-subtree t t)
                        (buffer-substring-no-properties start (point)))))
      (if (> stars 1)
          (replace-regexp-in-string
           (format "^\\*\\{%d\\}\\(\\*+ \\)" (1- stars)) "\\1" text)
        text))))

;;;; Writing the id back

(defun tumblr-post--record-keywords (id blog)
  "Record ID, and BLOG when given, as keywords heading the buffer.
An existing #+tumblr_id: line is updated in place; otherwise the
lines follow the keywords the buffer starts with."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward "^#\\+tumblr_id:.*$" nil t)
          (replace-match (format "#+tumblr_id: %s" id) t t)
        (goto-char (point-min))
        (while (looking-at "#\\+[A-Za-z_-]+:")
          (forward-line 1))
        (insert (if blog (format "#+blog: %s\n" blog) "")
                (format "#+tumblr_id: %s\n" id))))))

;;;; Sending

(defun tumblr-post--send (blog id plist preview record)
  "Send the post described by PLIST to BLOG, replacing the post ID if given.
PLIST holds the `:tags', `:state', `:publish-on' and `:body' of the
post as `tumblr-compose-parse' produces them.  With PREVIEW, show the
request instead of sending it.  RECORD is called with the id of a
created post."
  (let* ((body (tumblr-compose--request-body plist))
         (blocks (length (alist-get 'content (car body)))))
    (when (zerop blocks)
      (user-error "The post is empty"))
    (cond
     (preview (tumblr-compose-preview-request blog body))
     ((y-or-n-p (if id
                    (format "Replace post %s on %s with %d block%s? "
                            id blog blocks (if (= blocks 1) "" "s"))
                  (format "Post %d block%s to %s as %s? "
                          blocks (if (= blocks 1) "" "s") blog
                          (alist-get 'state (car body)))))
      (cl-flet ((done (response)
                  (let ((posted (or (alist-get 'id response) id)))
                    (unless id
                      (funcall record posted))
                    (message "%s https://www.tumblr.com/%s/%s"
                             (if id "Edited" "Posted") blog posted)))
                (failed (err)
                  (message "%s" (tumblr-http-error-string err))))
        (if id
            (tumblr-api-edit-post blog id (car body) :files (cdr body)
                                  :then #'done :else #'failed)
          (tumblr-api-create-post blog (car body) :files (cdr body)
                                  :then #'done :else #'failed)))))))

;;;###autoload
(defun tumblr-post-buffer (&optional preview)
  "Post the current Org buffer as one post.
The #+blog:, #+tags: (or #+filetags:), #+state: and #+publish_on:
keywords describe the post; a #+title: leads it as a heading, under
which the headlines of the buffer become subheadings.  The id of the
new post is recorded as #+tumblr_id: so that running the command
again edits the post.  With PREVIEW, the prefix argument, show the
request instead of sending it."
  (interactive "P")
  (tumblr-post--check-buffer)
  (let* ((keywords (org-collect-keywords tumblr-post-keywords))
         (blog (tumblr-post--keyword keywords "BLOG"))
         (state (tumblr-post--keyword keywords "STATE"))
         (buffer (current-buffer)))
    (tumblr-post--send
     (or blog (tumblr-user-default-blog))
     (tumblr-post--keyword keywords "TUMBLR_ID")
     (list :tags (tumblr-post--file-tags keywords)
           :state (and state (downcase state))
           :publish-on (tumblr-post--keyword keywords "PUBLISH_ON")
           :body (tumblr-post--buffer-body
                  (tumblr-post--keyword keywords "TITLE")))
     preview
     (lambda (id)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (tumblr-post--record-keywords
            id (and (null blog) (tumblr-user-default-blog)))))))))

;;;###autoload
(defun tumblr-post-subtree (&optional preview)
  "Post the Org entry at point, with everything below it, as one post.
Its headline leads the post as a heading and the headlines below it
become subheadings.  The tags of the entry become the tags of the
post.  The TUMBLR_BLOG, TUMBLR_STATE and TUMBLR_PUBLISH_ON properties,
inherited from the entries above and from #+property: lines, then
the #+blog:, #+state: and #+publish_on: keywords, describe it.  The
id of the new post is recorded in the TUMBLR_ID property so that
running the command again edits the post.  With PREVIEW, the prefix
argument, show the request instead of sending it."
  (interactive "P")
  (tumblr-post--check-buffer)
  (save-excursion
    (org-back-to-heading t)
    (let* ((keywords (org-collect-keywords tumblr-post-keywords))
           (blog (or (org-entry-get nil "TUMBLR_BLOG" t)
                     (tumblr-post--keyword keywords "BLOG")))
           (state (or (org-entry-get nil "TUMBLR_STATE" t)
                      (tumblr-post--keyword keywords "STATE")))
           (marker (point-marker)))
      (tumblr-post--send
       (or blog (tumblr-user-default-blog))
       (org-entry-get nil "TUMBLR_ID")
       (list :tags (mapcar #'substring-no-properties (org-get-tags))
             :state (and state (downcase state))
             :publish-on (or (org-entry-get nil "TUMBLR_PUBLISH_ON" t)
                             (tumblr-post--keyword keywords "PUBLISH_ON"))
             :body (tumblr-post--subtree-body))
       preview
       (lambda (id)
         (when (buffer-live-p (marker-buffer marker))
           (org-entry-put marker "TUMBLR_ID" id)
           (unless blog
             (org-entry-put marker "TUMBLR_BLOG"
                            (tumblr-user-default-blog)))))))))

(provide 'tumblr-post)
;;; tumblr-post.el ends here
