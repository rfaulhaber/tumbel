;;; tumblr-manage.el --- Queue, drafts and inbox for tumblr.el  -*- lexical-binding: t; -*-

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

;; The posts of your blog that are not public yet: the queue, the
;; drafts and the inbox of asks and submissions, each as a feed of
;; your own posts.  P publishes the post at point, A answers an ask,
;; and queued posts can be moved and shuffled.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'tumblr-api)
(require 'tumblr-compose)
(require 'tumblr-feed)
(require 'tumblr-npf)
(require 'tumblr-user)

(defvar-local tumblr-manage--blog nil
  "The blog whose queue, drafts or inbox this buffer shows.")

(defvar-local tumblr-manage--queue nil
  "Non-nil in the buffer showing a queue.")

;;;; Sources

(defun tumblr-manage--offset-fetch (request)
  "Return a fetch function paging REQUEST by offset.
REQUEST is called with the query parameters and the THEN and ELSE
callbacks of the API."
  (lambda (cursor then else)
    (let ((offset (or cursor 0)))
      (funcall request
               `((limit . ,tumblr-feed-page-size) (offset . ,offset))
               (lambda (response)
                 (let ((posts (alist-get 'posts response)))
                   (funcall then posts
                            (and posts (+ offset (length posts))))))
               else))))

(defun tumblr-queue-source (blog)
  "Return the feed source for the queue of BLOG."
  (tumblr-feed-source-create
   :name (format "queue/%s" blog)
   :title (format "Queue of %s" blog)
   :fetch (tumblr-manage--offset-fetch
           (lambda (params then else)
             (tumblr-api-queue blog :params params :then then :else else)))))

(defun tumblr-drafts-source (blog)
  "Return the feed source for the drafts of BLOG, paged by post id."
  (tumblr-feed-source-create
   :name (format "drafts/%s" blog)
   :title (format "Drafts of %s" blog)
   :fetch
   (lambda (cursor then else)
     (tumblr-api-drafts
      blog
      :params `((before_id . ,cursor))
      :then (lambda (response)
              (let ((posts (alist-get 'posts response)))
                (funcall then posts
                         (and posts (tumblr-npf-post-id (car (last posts)))))))
      :else else))))

(defun tumblr-inbox-source (blog)
  "Return the feed source for the inbox of BLOG."
  (tumblr-feed-source-create
   :name (format "inbox/%s" blog)
   :title (format "Inbox of %s" blog)
   :fetch (tumblr-manage--offset-fetch
           (lambda (params then else)
             (tumblr-api-submissions blog :params params
                                     :then then :else else)))))

(defun tumblr-manage--display (source blog queue)
  "Show SOURCE for BLOG; QUEUE non-nil marks the buffer as a queue."
  (with-current-buffer (tumblr-feed-display source)
    (setq tumblr-manage--blog blog
          tumblr-manage--queue queue)
    (current-buffer)))

(defun tumblr-manage--read-blog ()
  "Return the blog to manage, asking with a prefix argument."
  (or (and current-prefix-arg
           (completing-read "Blog: " (tumblr-user-blog-names) nil t))
      (tumblr-user-default-blog)))

;;;###autoload
(defun tumblr-queue (&optional blog)
  "Show the queue of your blog BLOG, the default blog when nil."
  (interactive (list (tumblr-manage--read-blog)))
  (let ((blog (or blog (tumblr-user-default-blog))))
    (tumblr-manage--display (tumblr-queue-source blog) blog t)))

;;;###autoload
(defun tumblr-drafts (&optional blog)
  "Show the drafts of your blog BLOG, the default blog when nil."
  (interactive (list (tumblr-manage--read-blog)))
  (let ((blog (or blog (tumblr-user-default-blog))))
    (tumblr-manage--display (tumblr-drafts-source blog) blog nil)))

;;;###autoload
(defun tumblr-inbox (&optional blog)
  "Show the asks and submissions sent to your blog BLOG."
  (interactive (list (tumblr-manage--read-blog)))
  (let ((blog (or blog (tumblr-user-default-blog))))
    (tumblr-manage--display (tumblr-inbox-source blog) blog nil)))

;;;; Publishing and answering

(defconst tumblr-manage-unpublished-states '("draft" "queue" "submission"
                                             "private" "unapproved")
  "States of posts that `tumblr-manage-publish' may publish.")

(defun tumblr-manage-publish ()
  "Publish the post at point right away.
The post must be yours and not published yet.  It is fetched again
so that its content is sent back unchanged."
  (interactive)
  (let* ((node (tumblr-feed--node-at-point))
         (post (tumblr-feed-item-post (ewoc-data node)))
         (blog (tumblr-npf-post-blog-name post))
         (id (tumblr-npf-post-id post)))
    (unless (tumblr-user-own-blog-p blog)
      (user-error "You can only publish your own posts"))
    (unless (member (alist-get 'state post) tumblr-manage-unpublished-states)
      (user-error "This post is already published"))
    (when (yes-or-no-p (format "Publish post %s on %s now? " id blog))
      (let* ((full (tumblr-api-blog-post blog id :fidelity t :then 'sync))
             (body (append (list (cons 'content (alist-get 'content full)))
                           (when (alist-get 'layout full)
                             (list (cons 'layout (alist-get 'layout full))))
                           (list (cons 'state "published")))))
        (tumblr-feed--act
         node
         (lambda (then else)
           (tumblr-api-edit-post blog id body :then then :else else))
         (lambda (_)
           (message "Published post %s on %s" id blog)
           'remove))))))

(defun tumblr-manage-answer ()
  "Answer the ask at point in a compose buffer."
  (interactive)
  (let ((post (tumblr-feed-item-post (ewoc-data (tumblr-feed--node-at-point)))))
    (unless (seq-find (lambda (entry) (equal (alist-get 'type entry) "ask"))
                      (alist-get 'layout post))
      (user-error "This post is not an ask"))
    (tumblr-compose-edit post t)))

;;;; Reordering the queue

(defun tumblr-manage--require-queue ()
  "Signal a user error unless this buffer shows a queue."
  (unless tumblr-manage--queue
    (user-error "This buffer does not show a queue")))

(defun tumblr-manage--move (node before)
  "Move the queued post in NODE before the node BEFORE, or last when nil."
  (let* ((ewoc tumblr-feed--ewoc)
         (item (ewoc-data node))
         (post (tumblr-feed-item-post item))
         (id (tumblr-npf-post-id post))
         (previous (and before (ewoc-prev ewoc before)))
         (after-id (cond ((eq previous node)
                          (let ((earlier (ewoc-prev ewoc node)))
                            (if earlier
                                (tumblr-npf-post-id
                                 (tumblr-feed-item-post (ewoc-data earlier)))
                              0)))
                         (previous (tumblr-npf-post-id
                                    (tumblr-feed-item-post
                                     (ewoc-data previous))))
                         (before 0)
                         (t (let ((last (ewoc-nth ewoc -1)))
                              (tumblr-npf-post-id
                               (tumblr-feed-item-post (ewoc-data last)))))))
         (buffer (current-buffer)))
    (tumblr-api-queue-reorder
     tumblr-manage--blog id after-id
     :then (lambda (_)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (let ((inhibit-read-only t))
                   (ewoc-delete ewoc node)
                   (ewoc-goto-node ewoc (if before
                                            (ewoc-enter-before ewoc before item)
                                          (ewoc-enter-last ewoc item)))))))
     :else (lambda (err) (message "%s" (tumblr-http-error-string err))))))

(defun tumblr-manage-move-up ()
  "Move the queued post at point one place earlier."
  (interactive)
  (tumblr-manage--require-queue)
  (let* ((node (tumblr-feed--node-at-point))
         (previous (ewoc-prev tumblr-feed--ewoc node)))
    (unless previous
      (user-error "Already first in the queue"))
    (tumblr-manage--move node previous)))

(defun tumblr-manage-move-down ()
  "Move the queued post at point one place later."
  (interactive)
  (tumblr-manage--require-queue)
  (let* ((node (tumblr-feed--node-at-point))
         (next (ewoc-next tumblr-feed--ewoc node)))
    (unless next
      (user-error "Already last in the queue"))
    (tumblr-manage--move node (ewoc-next tumblr-feed--ewoc next))))

(defun tumblr-manage-shuffle ()
  "Shuffle the queue shown in this buffer."
  (interactive)
  (tumblr-manage--require-queue)
  (when (yes-or-no-p (format "Shuffle the queue of %s? " tumblr-manage--blog))
    (let ((buffer (current-buffer)))
      (tumblr-api-queue-shuffle
       tumblr-manage--blog
       :then (lambda (_)
               (message "Queue shuffled")
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (revert-buffer))))
       :else (lambda (err) (message "%s" (tumblr-http-error-string err)))))))

(define-key tumblr-feed-mode-map (kbd "P") #'tumblr-manage-publish)
(define-key tumblr-feed-mode-map (kbd "A") #'tumblr-manage-answer)
(define-key tumblr-feed-mode-map (kbd "M-<up>") #'tumblr-manage-move-up)
(define-key tumblr-feed-mode-map (kbd "M-<down>") #'tumblr-manage-move-down)
(define-key tumblr-feed-mode-map (kbd "S") #'tumblr-manage-shuffle)

(provide 'tumblr-manage)
;;; tumblr-manage.el ends here
