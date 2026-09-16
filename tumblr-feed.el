;;; tumblr-feed.el --- Feed buffers for tumblr.el  -*- lexical-binding: t; -*-

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

;; The buffer that shows a sequence of posts: a blog, a tag, the
;; dashboard or a single post.  Posts live in an ewoc, one node per
;; post, so that a post can be re-rendered on its own.  Pages come
;; from a source, a function fetching the posts for a cursor, and are
;; appended as they arrive.  Duplicates are dropped because offset
;; paging drifts while a feed moves.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'ewoc)
(require 'tumblr-api)
(require 'tumblr-npf)
(require 'tumblr-user)
(require 'tumblr-compose)

(defcustom tumblr-feed-page-size 20
  "Number of posts requested per page; the API allows at most 20."
  :type 'integer
  :group 'tumblr)

(cl-defstruct (tumblr-feed-source (:constructor tumblr-feed-source-create)
                                  (:copier nil))
  "A feed of posts, or of other items rendered one per node.
NAME identifies the buffer and TITLE heads it.  FETCH is called with
a cursor (nil for the first page), a function receiving the items of
that page and the cursor of the next one (nil when there is none),
and a function receiving an error; it returns the process doing the
work, or nil.  EXPANDED shows every post in full.  KIND is `post',
or another symbol for items the post actions must refuse.  RENDER
inserts an item (`tumblr-npf-insert-post' by default), KEY returns
the identity used to drop duplicates (the post id by default), and
OPEN is called with the item on RET (the post is shown on its own
by default)."
  name title fetch (expanded nil) (kind 'post) render key open)

(cl-defstruct (tumblr-feed-item (:constructor tumblr-feed-item-create)
                                (:copier nil))
  "An item shown in a feed together with its view state.
POST holds the item, usually a post alist.  PENDING is non-nil while
an action on the post is waiting for Tumblr."
  post (expanded nil) (pending nil))

(defvar-local tumblr-feed--ewoc nil
  "The ewoc holding the posts of this buffer.")

(defvar-local tumblr-feed--source nil
  "The `tumblr-feed-source' shown in this buffer.")

(defvar-local tumblr-feed--cursor nil
  "Where the next page starts; nil for the first page.")

(defvar-local tumblr-feed--loading nil
  "Non-nil while a page is being fetched: the process, or t.")

(defvar-local tumblr-feed--exhausted nil
  "Non-nil once the source has no further pages.")

(defvar-local tumblr-feed--seen nil
  "Hash table of the ids of the posts already shown.")

(defvar-local tumblr-feed--restore-id nil
  "Id of the post to move to once the next page has arrived.")

(defvar tumblr-feed-tag-history nil
  "History of tags browsed from feeds.")


;;;; Mode

(defvar tumblr-feed-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tumblr-feed-next)
    (define-key map (kbd "p") #'tumblr-feed-previous)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "S-<tab>") #'backward-button)
    (define-key map (kbd "RET") #'tumblr-feed-activate)
    (define-key map (kbd "L") #'tumblr-feed-load-more)
    (define-key map (kbd "b") #'tumblr-feed-open-blog)
    (define-key map (kbd "t") #'tumblr-feed-browse-tag)
    (define-key map (kbd "o") #'tumblr-feed-browse-url)
    (define-key map (kbd "y") #'tumblr-feed-copy-url)
    (define-key map (kbd "i") #'tumblr-feed-toggle-images)
    (define-key map (kbd "l") #'tumblr-feed-like)
    (define-key map (kbd "R") #'tumblr-feed-reblog)
    (define-key map (kbd "f") #'tumblr-feed-follow)
    (define-key map (kbd "D") #'tumblr-feed-delete)
    (define-key map (kbd "c") #'tumblr-compose)
    (define-key map (kbd "r") #'tumblr-feed-reblog-with-comment)
    (define-key map (kbd "E") #'tumblr-feed-edit)
    (define-key map (kbd "v") #'tumblr-feed-show-notes)
    map)
  "Keymap of `tumblr-feed-mode'.")

(define-derived-mode tumblr-feed-mode special-mode "Tumblr"
  "Major mode for browsing a feed of Tumblr posts.
\\{tumblr-feed-mode-map}"
  (setq buffer-undo-list t)
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (setq-local revert-buffer-function #'tumblr-feed-revert)
  (setq-local tumblr-npf-toggle-function #'tumblr-feed-toggle)
  (add-hook 'kill-buffer-hook #'tumblr-feed--cancel nil t))

;;;; Buffer management

(defun tumblr-feed-display (source)
  "Show the posts of SOURCE, a `tumblr-feed-source', in their buffer.
The buffer is reset and the first page loaded.  Return the buffer."
  (let ((buffer (get-buffer-create
                 (format "*tumblr: %s*" (tumblr-feed-source-name source)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'tumblr-feed-mode)
        (tumblr-feed-mode))
      (setq tumblr-feed--source source)
      (when (eq (tumblr-feed-source-kind source) 'post)
        (tumblr-feed--load-filters))
      (tumblr-feed--reset)
      (tumblr-feed--load-page))
    (pop-to-buffer-same-window buffer)
    buffer))

(defun tumblr-feed--header-string ()
  "Return the default header of the current feed buffer."
  (let ((title (or (tumblr-feed-source-title tumblr-feed--source)
                   (tumblr-feed-source-name tumblr-feed--source))))
    (concat (propertize title 'face 'tumblr-heading1) "\n\n")))

(defun tumblr-feed--reset ()
  "Empty the current feed buffer and forget every page."
  (tumblr-feed--cancel)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq tumblr-feed--ewoc (ewoc-create #'tumblr-feed--pretty-print
                                         (tumblr-feed--header-string)
                                         "" t)))
  (setq tumblr-feed--cursor nil
        tumblr-feed--exhausted nil
        tumblr-feed--seen (make-hash-table :test #'equal)))

(defun tumblr-feed--cancel ()
  "Cancel the page fetch in flight, if any."
  (when (processp tumblr-feed--loading)
    (tumblr-http-cancel tumblr-feed--loading))
  (setq tumblr-feed--loading nil))

(defface tumblr-separator '((t :strike-through t :inherit shadow))
  "Face of the rule drawn under each post."
  :group 'tumblr)

(defun tumblr-feed--separator ()
  "Return a rule spanning the text area of the window, on its own line."
  (concat (propertize " " 'display '(space :width text)
                      'face 'tumblr-separator)
          "\n"))

(defun tumblr-feed--pretty-print (item)
  "Insert ITEM, a `tumblr-feed-item', and a blank line.
Posts end with a rule; items a source renders itself do not."
  (let ((render (tumblr-feed-source-render tumblr-feed--source))
        (post (tumblr-feed-item-post item)))
    (cond (render (funcall render post))
          (t
           (if (and (not (tumblr-feed-item-expanded item))
                    (tumblr-feed-hidden-reason post))
               (tumblr-feed--insert-hidden post
                                           (tumblr-feed-hidden-reason post))
             (tumblr-npf-insert-post post
                                     :expanded (tumblr-feed-item-expanded
                                                item)))
           (insert (tumblr-feed--separator)))))
  (insert "\n"))

(defun tumblr-feed--key (data)
  "Return the identity of the item DATA in the current feed."
  (funcall (or (tumblr-feed-source-key tumblr-feed--source)
               #'tumblr-npf-post-id)
           data))

(defun tumblr-feed-set-header (string)
  "Replace the header of the current feed buffer with STRING."
  (let ((inhibit-read-only t))
    (ewoc-set-hf tumblr-feed--ewoc string
                 (cdr (ewoc-get-hf tumblr-feed--ewoc)))))

(defun tumblr-feed--set-footer (string)
  "Replace the footer of the current feed buffer with STRING."
  (let ((inhibit-read-only t))
    (ewoc-set-hf tumblr-feed--ewoc (car (ewoc-get-hf tumblr-feed--ewoc))
                 string)))

(defun tumblr-feed--load-more-button ()
  "Return the button that loads the next page."
  (tumblr-npf-button "[Load more]" 'tumblr-load-more))

(define-button-type 'tumblr-load-more
  'action (lambda (_button) (tumblr-feed-load-more))
  'follow-link t
  'face 'tumblr-link
  'help-echo "Load the next page")

;;;; Filtered posts

(defun tumblr-feed--post-text (post)
  "Return the text of POST, including its reblog trail, for matching."
  (let ((blocks (apply #'append
                       (alist-get 'content post)
                       (mapcar (lambda (item) (alist-get 'content item))
                               (alist-get 'trail post)))))
    (mapconcat (lambda (block)
                 (if (equal (alist-get 'type block) "text")
                     (or (alist-get 'text block) "")
                   ""))
               blocks "\n")))

(defun tumblr-feed-hidden-reason (post &optional filters)
  "Return why POST is hidden by FILTERS, or nil when it is not.
FILTERS is (TAGS . CONTENT) as `tumblr-user-filters' returns it."
  (let* ((filters (or filters (tumblr-user-filters)))
         (tags (car filters))
         (content (cdr filters))
         (post-tags (mapcar #'downcase (tumblr-npf-post-tags post)))
         (tag (seq-find (lambda (filtered)
                          (member (downcase filtered) post-tags))
                        tags)))
    (cond (tag (format "#%s" tag))
          (content
           (let* ((text (downcase (tumblr-feed--post-text post)))
                  (match (seq-find (lambda (filtered)
                                     (string-match-p (regexp-quote
                                                      (downcase filtered))
                                                     text))
                                   content)))
             (and match (format "\"%s\"" match)))))))

(defun tumblr-feed--insert-hidden (post reason)
  "Insert the one line standing in for the filtered POST, with REASON."
  (insert (tumblr-npf-blog-button (or (tumblr-npf-post-blog-name post) "?"))
          (propertize (format " · hidden, filtered by %s " reason)
                      'face 'tumblr-meta)
          (tumblr-npf-button "[Show]" 'tumblr-toggle)
          "\n"))

(defun tumblr-feed--load-filters ()
  "Fetch the account filters once and re-render this feed when they arrive."
  (let ((buffer (current-buffer)))
    (unless (tumblr-user-filters)
      (tumblr-user-load-filters
       (lambda (filters)
         (when (and filters (or (car filters) (cdr filters))
                    (buffer-live-p buffer))
           (with-current-buffer buffer
             (when tumblr-feed--ewoc
               (ewoc-refresh tumblr-feed--ewoc)))))))))
;;;; Pages

(defun tumblr-feed--load-page ()
  "Fetch the next page unless one is on its way or none is left."
  (unless (or tumblr-feed--loading tumblr-feed--exhausted)
    (let ((buffer (current-buffer)))
      (setq tumblr-feed--loading t)
      (tumblr-feed--set-footer (propertize "Loading…" 'face 'tumblr-meta))
      (let ((process
             (condition-case err
                 (funcall (tumblr-feed-source-fetch tumblr-feed--source)
                          tumblr-feed--cursor
                          (lambda (posts next)
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (tumblr-feed--append posts next))))
                          (lambda (err)
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (tumblr-feed--fail err)))))
               ;; Typically: not logged in.  Show it where the page
               ;; would have gone rather than leaving "Loading…".
               (tumblr-error (tumblr-feed--fail err) nil))))
        ;; A synchronous fetch has already cleared the flag.
        (when (and tumblr-feed--loading process)
          (setq tumblr-feed--loading process))))))

(defun tumblr-feed--append (posts next)
  "Add POSTS to the feed; NEXT is the cursor of the following page.
Posts already shown are skipped.  Return the number of posts added."
  (setq tumblr-feed--loading nil)
  (let ((ewoc tumblr-feed--ewoc)
        (first (null (ewoc-nth tumblr-feed--ewoc 0)))
        (expanded (tumblr-feed-source-expanded tumblr-feed--source))
        (added 0))
    (dolist (post posts)
      (let ((id (tumblr-feed--key post)))
        (unless (and id (gethash id tumblr-feed--seen))
          (when id
            (puthash id t tumblr-feed--seen))
          (ewoc-enter-last ewoc (tumblr-feed-item-create :post post
                                                         :expanded expanded))
          (cl-incf added))))
    (setq tumblr-feed--cursor next
          tumblr-feed--exhausted (or (null next) (null posts)))
    (tumblr-feed--set-footer
     (cond ((not tumblr-feed--exhausted) (tumblr-feed--load-more-button))
           ((ewoc-nth ewoc 0) (propertize "End of feed." 'face 'tumblr-meta))
           (t (propertize "No posts." 'face 'tumblr-meta))))
    (cond (tumblr-feed--restore-id
           (tumblr-feed--goto-id tumblr-feed--restore-id)
           (setq tumblr-feed--restore-id nil))
          ((and first (ewoc-nth ewoc 0))
           (ewoc-goto-node ewoc (ewoc-nth ewoc 0))))
    added))

(defun tumblr-feed--fail (err)
  "Report ERR, the failure of a page fetch, in the footer."
  (setq tumblr-feed--loading nil)
  (tumblr-feed--set-footer
   (concat (propertize (tumblr-http-error-string err) 'face 'error)
           "\n"
           (tumblr-feed--load-more-button)))
  (message "%s" (tumblr-http-error-string err)))

(defun tumblr-feed--goto-id (id)
  "Move point to the post whose id is ID, when it is in the feed."
  (let* ((ewoc tumblr-feed--ewoc)
         (node (ewoc-nth ewoc 0)))
    (while (and node
                (not (equal id (tumblr-feed--key
                                (tumblr-feed-item-post (ewoc-data node))))))
      (setq node (ewoc-next ewoc node)))
    (when node
      (ewoc-goto-node ewoc node))))

(defun tumblr-feed-revert (&rest _)
  "Reload the feed from its first page, keeping point on the same post."
  (setq tumblr-feed--restore-id
        (let ((post (tumblr-feed--post-at-point)))
          (and post (tumblr-feed--key post))))
  (tumblr-feed--reset)
  (tumblr-feed--load-page))

;;;; Posts at point

(defun tumblr-feed--item-at-point ()
  "Return the `tumblr-feed-item' at point, or nil."
  (let ((node (and tumblr-feed--ewoc (ewoc-locate tumblr-feed--ewoc))))
    (and node (ewoc-data node))))

(defun tumblr-feed--post-at-point ()
  "Return the post at point, or nil."
  (let ((item (tumblr-feed--item-at-point)))
    (and item (tumblr-feed-item-post item))))

(defun tumblr-feed-post-at-point ()
  "Return the post at point, or signal a user error."
  (or (tumblr-feed--post-at-point)
      (user-error "No post here")))

;;;; Commands

(defun tumblr-feed-next (&optional n)
  "Move to the next post, or the Nth next one.
Past the last post, load the next page."
  (interactive "p")
  (let ((ewoc tumblr-feed--ewoc))
    (dotimes (_ (or n 1))
      (let ((node (ewoc-locate ewoc)))
        (cond ((null node) (tumblr-feed-load-more))
              ((< (point) (ewoc-location node)) (ewoc-goto-node ewoc node))
              ((ewoc-next ewoc node)
               (ewoc-goto-node ewoc (ewoc-next ewoc node)))
              (t (tumblr-feed-load-more)))))))

(defun tumblr-feed-previous (&optional n)
  "Move to the previous post, or the Nth previous one."
  (interactive "p")
  (let ((ewoc tumblr-feed--ewoc))
    (dotimes (_ (or n 1))
      (let ((node (ewoc-locate ewoc)))
        (cond ((null node) (goto-char (point-min)))
              ((> (point) (ewoc-location node)) (ewoc-goto-node ewoc node))
              ((ewoc-prev ewoc node)
               (ewoc-goto-node ewoc (ewoc-prev ewoc node)))
              (t (goto-char (point-min))))))))

(defun tumblr-feed-load-more ()
  "Load the next page of the feed."
  (interactive)
  (cond (tumblr-feed--loading (message "Still loading…"))
        (tumblr-feed--exhausted (message "No more posts"))
        (t (tumblr-feed--load-page))))

(defun tumblr-feed-toggle (button)
  "Expand or collapse the post that BUTTON belongs to."
  (let* ((ewoc tumblr-feed--ewoc)
         (node (ewoc-locate ewoc (button-start button))))
    (when node
      (let ((item (ewoc-data node)))
        (setf (tumblr-feed-item-expanded item)
              (not (tumblr-feed-item-expanded item)))
        (ewoc-invalidate ewoc node)
        (ewoc-goto-node ewoc node)))))

(defun tumblr-feed-activate ()
  "Follow the button at point, or show the post at point on its own."
  (interactive)
  (if (button-at (point))
      (push-button (point))
    (tumblr-feed-open-post)))

(defun tumblr-feed-open-post ()
  "Show the post at point in a buffer of its own.
Feeds of other items open whatever the item refers to."
  (interactive)
  (let ((data (tumblr-feed-post-at-point))
        (open (tumblr-feed-source-open tumblr-feed--source)))
    (if open
        (funcall open data)
      (tumblr-feed-display (tumblr-feed-post-source
                            (tumblr-npf-post-blog-name data)
                            (tumblr-npf-post-id data)
                            data)))))

(defun tumblr-feed-open-blog ()
  "Show the blog of the post at point."
  (interactive)
  (funcall tumblr-npf-open-blog-function
           (tumblr-npf-post-blog-name (tumblr-feed-post-at-point))))

(defun tumblr-feed-browse-tag (tag)
  "Browse the posts carrying TAG, chosen among the tags of the post at point."
  (interactive
   (let ((tags (tumblr-npf-post-tags (tumblr-feed--post-at-point))))
     (list (completing-read "Tag: " tags nil nil nil
                            'tumblr-feed-tag-history (car tags)))))
  (funcall tumblr-npf-open-tag-function tag))

(defun tumblr-feed-browse-url ()
  "Open what is at point in the browser.
On a tag, a link or the name of another blog, that is the tag page,
the link or the blog; elsewhere, including the post's own name, it is
the post at point."
  (interactive)
  (funcall tumblr-npf-open-url-function (tumblr-feed--url-at-point)))

(defun tumblr-feed-copy-url ()
  "Copy the URL of what is at point to the kill ring.
See `tumblr-feed-browse-url' for what that is."
  (interactive)
  (let ((url (tumblr-feed--url-at-point)))
    (kill-new url)
    (message "Copied %s" url)))

(defun tumblr-feed--button-url (button)
  "Return the web URL of the tag, link or other blog BUTTON names, or nil.
The name of the post's own blog, where navigation leaves point, names
nothing: `o' after `n' must open the post."
  (let ((blog (button-get button 'tumblr-blog-name))
        (tag (button-get button 'tumblr-tag)))
    (cond (blog (and (not (equal blog (tumblr-npf-post-blog-name
                                       (tumblr-feed--post-at-point))))
                     (tumblr-npf-blog-url blog)))
          (tag (tumblr-npf-tag-url tag))
          (t (button-get button 'tumblr-url)))))

(defun tumblr-feed--url-at-point ()
  "Return the URL of the button at point, else of the post at point.
Signal a user error when there is neither."
  (let ((button (button-at (point))))
    (or (and button (tumblr-feed--button-url button))
        (tumblr-npf-post-url (tumblr-feed-post-at-point))
        (user-error "This post has no URL"))))

(defun tumblr-feed-toggle-images ()
  "Show or hide the images of this feed."
  (interactive)
  (setq-local tumblr-display-images (not tumblr-display-images))
  (ewoc-refresh tumblr-feed--ewoc)
  (message (if tumblr-display-images "Images shown" "Images hidden")))
;;;; Actions on the post at point

(defun tumblr-feed--node-at-point ()
  "Return the ewoc node of the post at point, or signal a user error.
Feeds of other items refuse, since the post actions do not apply."
  (unless (eq (tumblr-feed-source-kind tumblr-feed--source) 'post)
    (user-error "Not a post"))
  (or (and tumblr-feed--ewoc (ewoc-locate tumblr-feed--ewoc))
      (user-error "No post here")))

(defun tumblr-feed--refresh-node (node)
  "Re-render NODE, keeping point where it was inside the post."
  (let* ((ewoc tumblr-feed--ewoc)
         (start (ewoc-location node))
         (offset (and (>= (point) start) (- (point) start))))
    (ewoc-invalidate ewoc node)
    (when offset
      (let ((next (ewoc-next ewoc node)))
        (goto-char (min (+ (ewoc-location node) offset)
                        (if next (1- (ewoc-location next)) (point-max))))))))

(defun tumblr-feed--act (node request then)
  "Run REQUEST for the post in NODE and apply THEN to the response.
REQUEST is called with the success and failure callbacks to pass to
the API.  A second action on the same post is refused until the
first has completed.  Afterwards the post is re-rendered, or removed
when THEN returns the symbol `remove'."
  (let ((item (ewoc-data node))
        (buffer (current-buffer)))
    (when (tumblr-feed-item-pending item)
      (user-error "Still waiting for Tumblr to answer about this post"))
    (setf (tumblr-feed-item-pending item) t)
    (funcall request
             (lambda (response)
               (setf (tumblr-feed-item-pending item) nil)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (if (eq (funcall then response) 'remove)
                       (let ((inhibit-read-only t))
                         (ewoc-delete tumblr-feed--ewoc node))
                     (tumblr-feed--refresh-node node)))))
             (lambda (err)
               (setf (tumblr-feed-item-pending item) nil)
               (message "%s" (tumblr-http-error-string err))))))

(defun tumblr-feed--set (item key value)
  "Set KEY of the post of ITEM to VALUE."
  (let ((post (tumblr-feed-item-post item)))
    (setf (alist-get key post) value)
    (setf (tumblr-feed-item-post item) post)))

(defun tumblr-feed--allowed-p (post key)
  "Return non-nil unless POST carries KEY set to false."
  (or (not (assq key post)) (alist-get key post)))

(defun tumblr-feed-like ()
  "Like the post at point, or unlike it when already liked."
  (interactive)
  (let* ((node (tumblr-feed--node-at-point))
         (item (ewoc-data node))
         (post (tumblr-feed-item-post item))
         (liked (alist-get 'liked post)))
    (unless (tumblr-feed--allowed-p post 'can_like)
      (user-error "This post cannot be liked"))
    (tumblr-feed--act
     node
     (lambda (then else)
       (funcall (if liked #'tumblr-api-unlike #'tumblr-api-like)
                post :then then :else else))
     (lambda (_)
       (let ((notes (alist-get 'note_count post)))
         (tumblr-feed--set item 'liked (not liked))
         (when notes
           (tumblr-feed--set item 'note_count (+ notes (if liked -1 1)))))
       (message (if liked "Unliked" "Liked"))))))

(defun tumblr-feed--read-tags (prompt)
  "Read a comma-separated list of tags with PROMPT; return them as a list."
  (split-string (read-string prompt) "," t "[ \t]+"))

(defun tumblr-feed-reblog (&optional tags)
  "Reblog the post at point to your default blog, with the TAGS given.
Interactively, prompt for tags; an empty answer means none."
  (interactive
   (list (tumblr-feed--read-tags
          (format "Reblog to %s with tags (optional): "
                  (tumblr-user-default-blog)))))
  (let* ((node (tumblr-feed--node-at-point))
         (post (tumblr-feed-item-post (ewoc-data node)))
         (blog (tumblr-user-default-blog)))
    (unless (tumblr-feed--allowed-p post 'can_reblog)
      (user-error "This post cannot be reblogged"))
    (tumblr-feed--act
     node
     (lambda (then else)
       (tumblr-api-reblog blog post :tags tags :then then :else else))
     (lambda (response)
       (message "Reblogged to %s as post %s" blog (alist-get 'id response))))))

(defun tumblr-feed-follow ()
  "Follow the blog of the post at point, or unfollow it when followed."
  (interactive)
  (let* ((node (tumblr-feed--node-at-point))
         (item (ewoc-data node))
         (post (tumblr-feed-item-post item))
         (name (tumblr-npf-post-blog-name post))
         (followed (alist-get 'followed post))
         (url (or (alist-get 'url (alist-get 'blog post))
                  (format "https://%s.tumblr.com/" name))))
    (when (or (not followed) (yes-or-no-p (format "Unfollow %s? " name)))
      (tumblr-feed--act
       node
       (lambda (then else)
         (funcall (if followed #'tumblr-api-unfollow #'tumblr-api-follow)
                  url :then then :else else))
       (lambda (_)
         (tumblr-feed--set item 'followed (not followed))
         (message "%s %s" (if followed "Unfollowed" "Now following") name))))))

(defun tumblr-feed-delete ()
  "Delete the post at point, which must be on one of your blogs."
  (interactive)
  (let* ((node (tumblr-feed--node-at-point))
         (post (tumblr-feed-item-post (ewoc-data node)))
         (blog (tumblr-npf-post-blog-name post))
         (id (tumblr-npf-post-id post)))
    (unless (tumblr-user-own-blog-p blog)
      (user-error "You can only delete posts on your own blogs"))
    (when (yes-or-no-p (format "Delete post %s from %s? " id blog))
      (tumblr-feed--act
       node
       (lambda (then else)
         (tumblr-api-delete-post blog id :then then :else else))
       (lambda (_)
         (message "Deleted post %s" id)
         'remove)))))
(defun tumblr-feed-reblog-with-comment ()
  "Reblog the post at point with a comment written in a compose buffer."
  (interactive)
  (let ((post (tumblr-feed-post-at-point)))
    (unless (tumblr-feed--allowed-p post 'can_reblog)
      (user-error "This post cannot be reblogged"))
    (tumblr-compose-reblog post)))

(defun tumblr-feed-edit ()
  "Edit the post at point in a compose buffer."
  (interactive)
  (tumblr-compose-edit (tumblr-feed-post-at-point)))
(defun tumblr-feed-show-notes ()
  "Show the notes of the post at point."
  (interactive)
  (let ((post (tumblr-feed-item-post
               (ewoc-data (tumblr-feed--node-at-point)))))
    (funcall tumblr-npf-open-notes-function post)))
;;;; The single-post source

(defun tumblr-feed--complete-post (post blog)
  "Return POST with its blog name set to BLOG when it lacks one."
  (if (tumblr-npf-post-blog-name post)
      post
    (cons (cons 'blog_name blog) post)))

(defun tumblr-feed-post-source (blog id &optional post)
  "Return the source showing only the post ID of BLOG.
When POST, the post alist, is given it is shown right away and the
API is only asked when the buffer is reverted."
  (let ((cached post))
    (tumblr-feed-source-create
     :name (format "post/%s" id)
     :title (format "%s · %s" blog id)
     :expanded t
     :fetch (lambda (_cursor then else)
              (if cached
                  (let ((known cached))
                    (setq cached nil)
                    (funcall then (list known) nil)
                    nil)
                (tumblr-api-blog-post
                 blog id
                 :then (lambda (fetched)
                         (funcall then
                                  (list (tumblr-feed--complete-post fetched
                                                                    blog))
                                  nil))
                 :else else))))))

;;;; The dashboard source

(defun tumblr-dashboard-source ()
  "Return the source showing the dashboard of the logged-in user."
  (tumblr-feed-source-create
   :name "dashboard"
   :title "Dashboard"
   :fetch
   (lambda (cursor then else)
     (let ((offset (or cursor 0)))
       (tumblr-api-dashboard
        :params `((limit . ,tumblr-feed-page-size)
                  (offset . ,offset)
                  (reblog_info . t))
        :then (lambda (response)
                (let ((posts (alist-get 'posts response)))
                  (funcall then posts (and posts (+ offset (length posts))))))
        :else else)))))
(provide 'tumblr-feed)
;;; tumblr-feed.el ends here
