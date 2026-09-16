;;; tumbel-manage.el --- Queue, drafts and inbox for tumbel.el  -*- lexical-binding: t; -*-

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
(require 'tumbel-api)
(require 'tumbel-compose)
(require 'tumbel-feed)
(require 'tumbel-npf)
(require 'tumbel-user)

(defvar-local tumbel-manage--blog nil
  "The blog whose queue, drafts or inbox this buffer shows.")

(defvar-local tumbel-manage--queue nil
  "Non-nil in the buffer showing a queue.")

;;;; Sources

(defun tumbel-manage--offset-fetch (request)
  "Return a fetch function paging REQUEST by offset.
REQUEST is called with the query parameters and the THEN and ELSE
callbacks of the API."
  (lambda (cursor then else)
    (let ((offset (or cursor 0)))
      (funcall request
               `((limit . ,tumbel-feed-page-size) (offset . ,offset))
               (lambda (response)
                 (let ((posts (alist-get 'posts response)))
                   (funcall then posts
                            (and posts (+ offset (length posts))))))
               else))))

(defun tumbel-queue-source (blog)
  "Return the feed source for the queue of BLOG."
  (tumbel-feed-source-create
   :name (format "queue/%s" blog)
   :title (format "Queue of %s" blog)
   :fetch (tumbel-manage--offset-fetch
           (lambda (params then else)
             (tumbel-api-queue blog :params params :then then :else else)))))

(defun tumbel-drafts-source (blog)
  "Return the feed source for the drafts of BLOG, paged by post id."
  (tumbel-feed-source-create
   :name (format "drafts/%s" blog)
   :title (format "Drafts of %s" blog)
   :fetch
   (lambda (cursor then else)
     (tumbel-api-drafts
      blog
      :params `((before_id . ,cursor))
      :then (lambda (response)
              (let ((posts (alist-get 'posts response)))
                (funcall then posts
                         (and posts (tumbel-npf-post-id (car (last posts)))))))
      :else else))))

(defun tumbel-inbox-source (blog)
  "Return the feed source for the inbox of BLOG."
  (tumbel-feed-source-create
   :name (format "inbox/%s" blog)
   :title (format "Inbox of %s" blog)
   :fetch (tumbel-manage--offset-fetch
           (lambda (params then else)
             (tumbel-api-submissions blog :params params
                                     :then then :else else)))))

(defun tumbel-manage--display (source blog queue)
  "Show SOURCE for BLOG; QUEUE non-nil marks the buffer as a queue."
  (with-current-buffer (tumbel-feed-display source)
    (setq tumbel-manage--blog blog
          tumbel-manage--queue queue)
    (current-buffer)))

(defun tumbel-manage--read-blog ()
  "Return the blog to manage, asking with a prefix argument."
  (or (and current-prefix-arg
           (completing-read "Blog: " (tumbel-user-blog-names) nil t))
      (tumbel-user-default-blog)))

;;;###autoload
(defun tumbel-queue (&optional blog)
  "Show the queue of your blog BLOG, the default blog when nil."
  (interactive (list (tumbel-manage--read-blog)))
  (let ((blog (or blog (tumbel-user-default-blog))))
    (tumbel-manage--display (tumbel-queue-source blog) blog t)))

;;;###autoload
(defun tumbel-drafts (&optional blog)
  "Show the drafts of your blog BLOG, the default blog when nil."
  (interactive (list (tumbel-manage--read-blog)))
  (let ((blog (or blog (tumbel-user-default-blog))))
    (tumbel-manage--display (tumbel-drafts-source blog) blog nil)))

;;;###autoload
(defun tumbel-inbox (&optional blog)
  "Show the asks and submissions sent to your blog BLOG."
  (interactive (list (tumbel-manage--read-blog)))
  (let ((blog (or blog (tumbel-user-default-blog))))
    (tumbel-manage--display (tumbel-inbox-source blog) blog nil)))

;;;; Publishing and answering

(defconst tumbel-manage-unpublished-states '("draft" "queue" "submission"
                                             "private" "unapproved")
  "States of posts that `tumbel-manage-publish' may publish.")

(defun tumbel-manage-publish ()
  "Publish the post at point right away.
The post must be yours and not published yet.  It is fetched again
so that its content is sent back unchanged."
  (interactive)
  (let* ((node (tumbel-feed--node-at-point))
         (post (tumbel-feed-item-post (ewoc-data node)))
         (blog (tumbel-npf-post-blog-name post))
         (id (tumbel-npf-post-id post)))
    (unless (tumbel-user-own-blog-p blog)
      (user-error "You can only publish your own posts"))
    (unless (member (alist-get 'state post) tumbel-manage-unpublished-states)
      (user-error "This post is already published"))
    (when (yes-or-no-p (format "Publish post %s on %s now? " id blog))
      (let* ((full (tumbel-api-blog-post blog id :fidelity t :then 'sync))
             (body (append (list (cons 'content (alist-get 'content full)))
                           (when (alist-get 'layout full)
                             (list (cons 'layout (alist-get 'layout full))))
                           (list (cons 'state "published")))))
        (tumbel-feed--act
         node
         (lambda (then else)
           (tumbel-api-edit-post blog id body :then then :else else))
         (lambda (_)
           (message "Published post %s on %s" id blog)
           'remove))))))

(defun tumbel-manage-answer ()
  "Answer the ask at point in a compose buffer."
  (interactive)
  (let ((post (tumbel-feed-item-post (ewoc-data (tumbel-feed--node-at-point)))))
    (unless (seq-find (lambda (entry) (equal (alist-get 'type entry) "ask"))
                      (alist-get 'layout post))
      (user-error "This post is not an ask"))
    (tumbel-compose-edit post t)))

;;;; Reordering the queue

(defun tumbel-manage--require-queue ()
  "Signal a user error unless this buffer shows a queue."
  (unless tumbel-manage--queue
    (user-error "This buffer does not show a queue")))

(defun tumbel-manage--move (node before)
  "Move the queued post in NODE before the node BEFORE, or last when nil."
  (let* ((ewoc tumbel-feed--ewoc)
         (item (ewoc-data node))
         (post (tumbel-feed-item-post item))
         (id (tumbel-npf-post-id post))
         (previous (and before (ewoc-prev ewoc before)))
         (after-id (cond ((eq previous node)
                          (let ((earlier (ewoc-prev ewoc node)))
                            (if earlier
                                (tumbel-npf-post-id
                                 (tumbel-feed-item-post (ewoc-data earlier)))
                              0)))
                         (previous (tumbel-npf-post-id
                                    (tumbel-feed-item-post
                                     (ewoc-data previous))))
                         (before 0)
                         (t (let ((last (ewoc-nth ewoc -1)))
                              (tumbel-npf-post-id
                               (tumbel-feed-item-post (ewoc-data last)))))))
         (buffer (current-buffer)))
    (tumbel-api-queue-reorder
     tumbel-manage--blog id after-id
     :then (lambda (_)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (let ((inhibit-read-only t))
                   (ewoc-delete ewoc node)
                   (ewoc-goto-node ewoc (if before
                                            (ewoc-enter-before ewoc before item)
                                          (ewoc-enter-last ewoc item)))))))
     :else (lambda (err) (message "%s" (tumbel-http-error-string err))))))

(defun tumbel-manage-move-up ()
  "Move the queued post at point one place earlier."
  (interactive)
  (tumbel-manage--require-queue)
  (let* ((node (tumbel-feed--node-at-point))
         (previous (ewoc-prev tumbel-feed--ewoc node)))
    (unless previous
      (user-error "Already first in the queue"))
    (tumbel-manage--move node previous)))

(defun tumbel-manage-move-down ()
  "Move the queued post at point one place later."
  (interactive)
  (tumbel-manage--require-queue)
  (let* ((node (tumbel-feed--node-at-point))
         (next (ewoc-next tumbel-feed--ewoc node)))
    (unless next
      (user-error "Already last in the queue"))
    (tumbel-manage--move node (ewoc-next tumbel-feed--ewoc next))))

(defun tumbel-manage-shuffle ()
  "Shuffle the queue shown in this buffer."
  (interactive)
  (tumbel-manage--require-queue)
  (when (yes-or-no-p (format "Shuffle the queue of %s? " tumbel-manage--blog))
    (let ((buffer (current-buffer)))
      (tumbel-api-queue-shuffle
       tumbel-manage--blog
       :then (lambda (_)
               (message "Queue shuffled")
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (revert-buffer))))
       :else (lambda (err) (message "%s" (tumbel-http-error-string err)))))))

(define-key tumbel-feed-mode-map (kbd "P") #'tumbel-manage-publish)
(define-key tumbel-feed-mode-map (kbd "A") #'tumbel-manage-answer)
(define-key tumbel-feed-mode-map (kbd "M-<up>") #'tumbel-manage-move-up)
(define-key tumbel-feed-mode-map (kbd "M-<down>") #'tumbel-manage-move-down)
(define-key tumbel-feed-mode-map (kbd "S") #'tumbel-manage-shuffle)

(provide 'tumbel-manage)
;;; tumbel-manage.el ends here
