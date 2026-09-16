;;; tumbel-feed.el --- Feed buffers for tumbel.el  -*- lexical-binding: t; -*-

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
(require 'tumbel-api)
(require 'tumbel-npf)
(require 'tumbel-user)
(require 'tumbel-compose)

(defcustom tumbel-feed-page-size 20
  "Number of posts requested per page; the API allows at most 20."
  :type 'integer
  :group 'tumbel)

(cl-defstruct (tumbel-feed-source (:constructor tumbel-feed-source-create)
                                  (:copier nil))
  "A feed of posts, or of other items rendered one per node.
NAME identifies the buffer and TITLE heads it.  FETCH is called with
a cursor (nil for the first page), a function receiving the items of
that page and the cursor of the next one (nil when there is none),
and a function receiving an error; it returns the process doing the
work, or nil.  EXPANDED shows every post in full.  KIND is `post',
or another symbol for items the post actions must refuse.  RENDER
inserts an item (`tumbel-npf-insert-post' by default), KEY returns
the identity used to drop duplicates (the post id by default), and
OPEN is called with the item on RET (the post is shown on its own
by default)."
  name title fetch (expanded nil) (kind 'post) render key open)

(cl-defstruct (tumbel-feed-item (:constructor tumbel-feed-item-create)
                                (:copier nil))
  "An item shown in a feed together with its view state.
POST holds the item, usually a post alist.  PENDING is non-nil while
an action on the post is waiting for Tumblr."
  post (expanded nil) (pending nil))

(defvar-local tumbel-feed--ewoc nil
  "The ewoc holding the posts of this buffer.")

(defvar-local tumbel-feed--source nil
  "The `tumbel-feed-source' shown in this buffer.")

(defvar-local tumbel-feed--cursor nil
  "Where the next page starts; nil for the first page.")

(defvar-local tumbel-feed--loading nil
  "Non-nil while a page is being fetched: the process, or t.")

(defvar-local tumbel-feed--exhausted nil
  "Non-nil once the source has no further pages.")

(defvar-local tumbel-feed--seen nil
  "Hash table of the ids of the posts already shown.")

(defvar-local tumbel-feed--restore-id nil
  "Id of the post to move to once the next page has arrived.")

(defvar tumbel-feed-tag-history nil
  "History of tags browsed from feeds.")


;;;; Mode

(defvar tumbel-feed-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'tumbel-feed-next)
    (define-key map (kbd "p") #'tumbel-feed-previous)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "S-<tab>") #'backward-button)
    (define-key map (kbd "RET") #'tumbel-feed-activate)
    (define-key map (kbd "L") #'tumbel-feed-load-more)
    (define-key map (kbd "b") #'tumbel-feed-open-blog)
    (define-key map (kbd "t") #'tumbel-feed-browse-tag)
    (define-key map (kbd "o") #'tumbel-feed-browse-url)
    (define-key map (kbd "y") #'tumbel-feed-copy-url)
    (define-key map (kbd "i") #'tumbel-feed-toggle-images)
    (define-key map (kbd "l") #'tumbel-feed-like)
    (define-key map (kbd "R") #'tumbel-feed-reblog)
    (define-key map (kbd "f") #'tumbel-feed-follow)
    (define-key map (kbd "D") #'tumbel-feed-delete)
    (define-key map (kbd "c") #'tumbel-compose)
    (define-key map (kbd "r") #'tumbel-feed-reblog-with-comment)
    (define-key map (kbd "E") #'tumbel-feed-edit)
    (define-key map (kbd "v") #'tumbel-feed-show-notes)
    map)
  "Keymap of `tumbel-feed-mode'.")

(define-derived-mode tumbel-feed-mode special-mode "Tumblr"
  "Major mode for browsing a feed of Tumblr posts.
\\{tumbel-feed-mode-map}"
  (setq buffer-undo-list t)
  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (setq-local revert-buffer-function #'tumbel-feed-revert)
  (setq-local tumbel-npf-toggle-function #'tumbel-feed-toggle)
  (add-hook 'kill-buffer-hook #'tumbel-feed--cancel nil t))

;;;; Buffer management

(defun tumbel-feed-display (source)
  "Show the posts of SOURCE, a `tumbel-feed-source', in their buffer.
The buffer is reset and the first page loaded.  Return the buffer."
  (let ((buffer (get-buffer-create
                 (format "*tumbel: %s*" (tumbel-feed-source-name source)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'tumbel-feed-mode)
        (tumbel-feed-mode))
      (setq tumbel-feed--source source)
      (when (eq (tumbel-feed-source-kind source) 'post)
        (tumbel-feed--load-filters))
      (tumbel-feed--reset)
      (tumbel-feed--load-page))
    (pop-to-buffer-same-window buffer)
    buffer))

(defun tumbel-feed--header-string ()
  "Return the default header of the current feed buffer."
  (let ((title (or (tumbel-feed-source-title tumbel-feed--source)
                   (tumbel-feed-source-name tumbel-feed--source))))
    (concat (propertize title 'face 'tumbel-heading1) "\n\n")))

(defun tumbel-feed--reset ()
  "Empty the current feed buffer and forget every page."
  (tumbel-feed--cancel)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq tumbel-feed--ewoc (ewoc-create #'tumbel-feed--pretty-print
                                         (tumbel-feed--header-string)
                                         "" t)))
  (setq tumbel-feed--cursor nil
        tumbel-feed--exhausted nil
        tumbel-feed--seen (make-hash-table :test #'equal)))

(defun tumbel-feed--cancel ()
  "Cancel the page fetch in flight, if any."
  (when (processp tumbel-feed--loading)
    (tumbel-http-cancel tumbel-feed--loading))
  (setq tumbel-feed--loading nil))

(defface tumbel-separator '((t :strike-through t :inherit shadow))
  "Face of the rule drawn under each post."
  :group 'tumbel)

(defun tumbel-feed--separator ()
  "Return a rule spanning the text area of the window, on its own line."
  (concat (propertize " " 'display '(space :width text)
                      'face 'tumbel-separator)
          "\n"))

(defun tumbel-feed--pretty-print (item)
  "Insert ITEM, a `tumbel-feed-item', and a blank line.
Posts end with a rule; items a source renders itself do not."
  (let ((render (tumbel-feed-source-render tumbel-feed--source))
        (post (tumbel-feed-item-post item)))
    (cond (render (funcall render post))
          (t
           (if (and (not (tumbel-feed-item-expanded item))
                    (tumbel-feed-hidden-reason post))
               (tumbel-feed--insert-hidden post
                                           (tumbel-feed-hidden-reason post))
             (tumbel-npf-insert-post post
                                     :expanded (tumbel-feed-item-expanded
                                                item)))
           (insert (tumbel-feed--separator)))))
  (insert "\n"))

(defun tumbel-feed--key (data)
  "Return the identity of the item DATA in the current feed."
  (funcall (or (tumbel-feed-source-key tumbel-feed--source)
               #'tumbel-npf-post-id)
           data))

(defun tumbel-feed-set-header (string)
  "Replace the header of the current feed buffer with STRING."
  (let ((inhibit-read-only t))
    (ewoc-set-hf tumbel-feed--ewoc string
                 (cdr (ewoc-get-hf tumbel-feed--ewoc)))))

(defun tumbel-feed--set-footer (string)
  "Replace the footer of the current feed buffer with STRING."
  (let ((inhibit-read-only t))
    (ewoc-set-hf tumbel-feed--ewoc (car (ewoc-get-hf tumbel-feed--ewoc))
                 string)))

(defun tumbel-feed--load-more-button ()
  "Return the button that loads the next page."
  (tumbel-npf-button "[Load more]" 'tumbel-load-more))

(define-button-type 'tumbel-load-more
  'action (lambda (_button) (tumbel-feed-load-more))
  'follow-link t
  'face 'tumbel-link
  'help-echo "Load the next page")

;;;; Filtered posts

(defun tumbel-feed--post-text (post)
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

(defun tumbel-feed-hidden-reason (post &optional filters)
  "Return why POST is hidden by FILTERS, or nil when it is not.
FILTERS is (TAGS . CONTENT) as `tumbel-user-filters' returns it."
  (let* ((filters (or filters (tumbel-user-filters)))
         (tags (car filters))
         (content (cdr filters))
         (post-tags (mapcar #'downcase (tumbel-npf-post-tags post)))
         (tag (seq-find (lambda (filtered)
                          (member (downcase filtered) post-tags))
                        tags)))
    (cond (tag (format "#%s" tag))
          (content
           (let* ((text (downcase (tumbel-feed--post-text post)))
                  (match (seq-find (lambda (filtered)
                                     (string-match-p (regexp-quote
                                                      (downcase filtered))
                                                     text))
                                   content)))
             (and match (format "\"%s\"" match)))))))

(defun tumbel-feed--insert-hidden (post reason)
  "Insert the one line standing in for the filtered POST, with REASON."
  (insert (tumbel-npf-blog-button (or (tumbel-npf-post-blog-name post) "?"))
          (propertize (format " · hidden, filtered by %s " reason)
                      'face 'tumbel-meta)
          (tumbel-npf-button "[Show]" 'tumbel-toggle)
          "\n"))

(defun tumbel-feed--load-filters ()
  "Fetch the account filters once and re-render this feed when they arrive."
  (let ((buffer (current-buffer)))
    (unless (tumbel-user-filters)
      (tumbel-user-load-filters
       (lambda (filters)
         (when (and filters (or (car filters) (cdr filters))
                    (buffer-live-p buffer))
           (with-current-buffer buffer
             (when tumbel-feed--ewoc
               (ewoc-refresh tumbel-feed--ewoc)))))))))
;;;; Pages

(defun tumbel-feed--load-page ()
  "Fetch the next page unless one is on its way or none is left."
  (unless (or tumbel-feed--loading tumbel-feed--exhausted)
    (let ((buffer (current-buffer)))
      (setq tumbel-feed--loading t)
      (tumbel-feed--set-footer (propertize "Loading…" 'face 'tumbel-meta))
      (let ((process
             (condition-case err
                 (funcall (tumbel-feed-source-fetch tumbel-feed--source)
                          tumbel-feed--cursor
                          (lambda (posts next)
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (tumbel-feed--append posts next))))
                          (lambda (err)
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (tumbel-feed--fail err)))))
               ;; Typically: not logged in.  Show it where the page
               ;; would have gone rather than leaving "Loading…".
               (tumbel-error (tumbel-feed--fail err) nil))))
        ;; A synchronous fetch has already cleared the flag.
        (when (and tumbel-feed--loading process)
          (setq tumbel-feed--loading process))))))

(defun tumbel-feed--append (posts next)
  "Add POSTS to the feed; NEXT is the cursor of the following page.
Posts already shown are skipped.  Return the number of posts added."
  (setq tumbel-feed--loading nil)
  (let ((ewoc tumbel-feed--ewoc)
        (first (null (ewoc-nth tumbel-feed--ewoc 0)))
        (expanded (tumbel-feed-source-expanded tumbel-feed--source))
        (added 0))
    (dolist (post posts)
      (let ((id (tumbel-feed--key post)))
        (unless (and id (gethash id tumbel-feed--seen))
          (when id
            (puthash id t tumbel-feed--seen))
          (ewoc-enter-last ewoc (tumbel-feed-item-create :post post
                                                         :expanded expanded))
          (cl-incf added))))
    (setq tumbel-feed--cursor next
          tumbel-feed--exhausted (or (null next) (null posts)))
    (tumbel-feed--set-footer
     (cond ((not tumbel-feed--exhausted) (tumbel-feed--load-more-button))
           ((ewoc-nth ewoc 0) (propertize "End of feed." 'face 'tumbel-meta))
           (t (propertize "No posts." 'face 'tumbel-meta))))
    (cond (tumbel-feed--restore-id
           (tumbel-feed--goto-id tumbel-feed--restore-id)
           (setq tumbel-feed--restore-id nil))
          ((and first (ewoc-nth ewoc 0))
           (ewoc-goto-node ewoc (ewoc-nth ewoc 0))))
    added))

(defun tumbel-feed--fail (err)
  "Report ERR, the failure of a page fetch, in the footer."
  (setq tumbel-feed--loading nil)
  (tumbel-feed--set-footer
   (concat (propertize (tumbel-http-error-string err) 'face 'error)
           "\n"
           (tumbel-feed--load-more-button)))
  (message "%s" (tumbel-http-error-string err)))

(defun tumbel-feed--goto-id (id)
  "Move point to the post whose id is ID, when it is in the feed."
  (let* ((ewoc tumbel-feed--ewoc)
         (node (ewoc-nth ewoc 0)))
    (while (and node
                (not (equal id (tumbel-feed--key
                                (tumbel-feed-item-post (ewoc-data node))))))
      (setq node (ewoc-next ewoc node)))
    (when node
      (ewoc-goto-node ewoc node))))

(defun tumbel-feed-revert (&rest _)
  "Reload the feed from its first page, keeping point on the same post."
  (setq tumbel-feed--restore-id
        (let ((post (tumbel-feed--post-at-point)))
          (and post (tumbel-feed--key post))))
  (tumbel-feed--reset)
  (tumbel-feed--load-page))

;;;; Posts at point

(defun tumbel-feed--item-at-point ()
  "Return the `tumbel-feed-item' at point, or nil."
  (let ((node (and tumbel-feed--ewoc (ewoc-locate tumbel-feed--ewoc))))
    (and node (ewoc-data node))))

(defun tumbel-feed--post-at-point ()
  "Return the post at point, or nil."
  (let ((item (tumbel-feed--item-at-point)))
    (and item (tumbel-feed-item-post item))))

(defun tumbel-feed-post-at-point ()
  "Return the post at point, or signal a user error."
  (or (tumbel-feed--post-at-point)
      (user-error "No post here")))

;;;; Commands

(defun tumbel-feed-next (&optional n)
  "Move to the next post, or the Nth next one.
Past the last post, load the next page."
  (interactive "p")
  (let ((ewoc tumbel-feed--ewoc))
    (dotimes (_ (or n 1))
      (let ((node (ewoc-locate ewoc)))
        (cond ((null node) (tumbel-feed-load-more))
              ((< (point) (ewoc-location node)) (ewoc-goto-node ewoc node))
              ((ewoc-next ewoc node)
               (ewoc-goto-node ewoc (ewoc-next ewoc node)))
              (t (tumbel-feed-load-more)))))))

(defun tumbel-feed-previous (&optional n)
  "Move to the previous post, or the Nth previous one."
  (interactive "p")
  (let ((ewoc tumbel-feed--ewoc))
    (dotimes (_ (or n 1))
      (let ((node (ewoc-locate ewoc)))
        (cond ((null node) (goto-char (point-min)))
              ((> (point) (ewoc-location node)) (ewoc-goto-node ewoc node))
              ((ewoc-prev ewoc node)
               (ewoc-goto-node ewoc (ewoc-prev ewoc node)))
              (t (goto-char (point-min))))))))

(defun tumbel-feed-load-more ()
  "Load the next page of the feed."
  (interactive)
  (cond (tumbel-feed--loading (message "Still loading…"))
        (tumbel-feed--exhausted (message "No more posts"))
        (t (tumbel-feed--load-page))))

(defun tumbel-feed-toggle (button)
  "Expand or collapse the post that BUTTON belongs to."
  (let* ((ewoc tumbel-feed--ewoc)
         (node (ewoc-locate ewoc (button-start button))))
    (when node
      (let ((item (ewoc-data node)))
        (setf (tumbel-feed-item-expanded item)
              (not (tumbel-feed-item-expanded item)))
        (ewoc-invalidate ewoc node)
        (ewoc-goto-node ewoc node)))))

(defun tumbel-feed-activate ()
  "Follow the button at point, or show the post at point on its own."
  (interactive)
  (if (button-at (point))
      (push-button (point))
    (tumbel-feed-open-post)))

(defun tumbel-feed-open-post ()
  "Show the post at point in a buffer of its own.
Feeds of other items open whatever the item refers to."
  (interactive)
  (let ((data (tumbel-feed-post-at-point))
        (open (tumbel-feed-source-open tumbel-feed--source)))
    (if open
        (funcall open data)
      (tumbel-feed-display (tumbel-feed-post-source
                            (tumbel-npf-post-blog-name data)
                            (tumbel-npf-post-id data)
                            data)))))

(defun tumbel-feed-open-blog ()
  "Show the blog of the post at point."
  (interactive)
  (funcall tumbel-npf-open-blog-function
           (tumbel-npf-post-blog-name (tumbel-feed-post-at-point))))

(defun tumbel-feed-browse-tag (tag)
  "Browse the posts carrying TAG, chosen among the tags of the post at point."
  (interactive
   (let ((tags (tumbel-npf-post-tags (tumbel-feed--post-at-point))))
     (list (completing-read "Tag: " tags nil nil nil
                            'tumbel-feed-tag-history (car tags)))))
  (funcall tumbel-npf-open-tag-function tag))

(defun tumbel-feed-browse-url ()
  "Open what is at point in the browser.
On a tag, a link or the name of another blog, that is the tag page,
the link or the blog; elsewhere, including the post's own name, it is
the post at point."
  (interactive)
  (funcall tumbel-npf-open-url-function (tumbel-feed--url-at-point)))

(defun tumbel-feed-copy-url ()
  "Copy the URL of what is at point to the kill ring.
See `tumbel-feed-browse-url' for what that is."
  (interactive)
  (let ((url (tumbel-feed--url-at-point)))
    (kill-new url)
    (message "Copied %s" url)))

(defun tumbel-feed--button-url (button)
  "Return the web URL of the tag, link or other blog BUTTON names, or nil.
The name of the post's own blog, where navigation leaves point, names
nothing: `o' after `n' must open the post."
  (let ((blog (button-get button 'tumbel-blog-name))
        (tag (button-get button 'tumbel-tag)))
    (cond (blog (and (not (equal blog (tumbel-npf-post-blog-name
                                       (tumbel-feed--post-at-point))))
                     (tumbel-npf-blog-url blog)))
          (tag (tumbel-npf-tag-url tag))
          (t (button-get button 'tumbel-url)))))

(defun tumbel-feed--url-at-point ()
  "Return the URL of the button at point, else of the post at point.
Signal a user error when there is neither."
  (let ((button (button-at (point))))
    (or (and button (tumbel-feed--button-url button))
        (tumbel-npf-post-url (tumbel-feed-post-at-point))
        (user-error "This post has no URL"))))

(defun tumbel-feed-toggle-images ()
  "Show or hide the images of this feed."
  (interactive)
  (setq-local tumbel-display-images (not tumbel-display-images))
  (ewoc-refresh tumbel-feed--ewoc)
  (message (if tumbel-display-images "Images shown" "Images hidden")))
;;;; Actions on the post at point

(defun tumbel-feed--node-at-point ()
  "Return the ewoc node of the post at point, or signal a user error.
Feeds of other items refuse, since the post actions do not apply."
  (unless (eq (tumbel-feed-source-kind tumbel-feed--source) 'post)
    (user-error "Not a post"))
  (or (and tumbel-feed--ewoc (ewoc-locate tumbel-feed--ewoc))
      (user-error "No post here")))

(defun tumbel-feed--refresh-node (node)
  "Re-render NODE, keeping point where it was inside the post."
  (let* ((ewoc tumbel-feed--ewoc)
         (start (ewoc-location node))
         (offset (and (>= (point) start) (- (point) start))))
    (ewoc-invalidate ewoc node)
    (when offset
      (let ((next (ewoc-next ewoc node)))
        (goto-char (min (+ (ewoc-location node) offset)
                        (if next (1- (ewoc-location next)) (point-max))))))))

(defun tumbel-feed--act (node request then)
  "Run REQUEST for the post in NODE and apply THEN to the response.
REQUEST is called with the success and failure callbacks to pass to
the API.  A second action on the same post is refused until the
first has completed.  Afterwards the post is re-rendered, or removed
when THEN returns the symbol `remove'."
  (let ((item (ewoc-data node))
        (buffer (current-buffer)))
    (when (tumbel-feed-item-pending item)
      (user-error "Still waiting for Tumblr to answer about this post"))
    (setf (tumbel-feed-item-pending item) t)
    (funcall request
             (lambda (response)
               (setf (tumbel-feed-item-pending item) nil)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (if (eq (funcall then response) 'remove)
                       (let ((inhibit-read-only t))
                         (ewoc-delete tumbel-feed--ewoc node))
                     (tumbel-feed--refresh-node node)))))
             (lambda (err)
               (setf (tumbel-feed-item-pending item) nil)
               (message "%s" (tumbel-http-error-string err))))))

(defun tumbel-feed--set (item key value)
  "Set KEY of the post of ITEM to VALUE."
  (let ((post (tumbel-feed-item-post item)))
    (setf (alist-get key post) value)
    (setf (tumbel-feed-item-post item) post)))

(defun tumbel-feed--allowed-p (post key)
  "Return non-nil unless POST carries KEY set to false."
  (or (not (assq key post)) (alist-get key post)))

(defun tumbel-feed-like ()
  "Like the post at point, or unlike it when already liked."
  (interactive)
  (let* ((node (tumbel-feed--node-at-point))
         (item (ewoc-data node))
         (post (tumbel-feed-item-post item))
         (liked (alist-get 'liked post)))
    (unless (tumbel-feed--allowed-p post 'can_like)
      (user-error "This post cannot be liked"))
    (tumbel-feed--act
     node
     (lambda (then else)
       (funcall (if liked #'tumbel-api-unlike #'tumbel-api-like)
                post :then then :else else))
     (lambda (_)
       (let ((notes (alist-get 'note_count post)))
         (tumbel-feed--set item 'liked (not liked))
         (when notes
           (tumbel-feed--set item 'note_count (+ notes (if liked -1 1)))))
       (message (if liked "Unliked" "Liked"))))))

(defun tumbel-feed--read-tags (prompt)
  "Read a comma-separated list of tags with PROMPT; return them as a list."
  (split-string (read-string prompt) "," t "[ \t]+"))

(defun tumbel-feed-reblog (&optional tags)
  "Reblog the post at point to your default blog, with the TAGS given.
Interactively, prompt for tags; an empty answer means none."
  (interactive
   (list (tumbel-feed--read-tags
          (format "Reblog to %s with tags (optional): "
                  (tumbel-user-default-blog)))))
  (let* ((node (tumbel-feed--node-at-point))
         (post (tumbel-feed-item-post (ewoc-data node)))
         (blog (tumbel-user-default-blog)))
    (unless (tumbel-feed--allowed-p post 'can_reblog)
      (user-error "This post cannot be reblogged"))
    (tumbel-feed--act
     node
     (lambda (then else)
       (tumbel-api-reblog blog post :tags tags :then then :else else))
     (lambda (response)
       (message "Reblogged to %s as post %s" blog (alist-get 'id response))))))

(defun tumbel-feed-follow ()
  "Follow the blog of the post at point, or unfollow it when followed."
  (interactive)
  (let* ((node (tumbel-feed--node-at-point))
         (item (ewoc-data node))
         (post (tumbel-feed-item-post item))
         (name (tumbel-npf-post-blog-name post))
         (followed (alist-get 'followed post))
         (url (or (alist-get 'url (alist-get 'blog post))
                  (format "https://%s.tumblr.com/" name))))
    (when (or (not followed) (yes-or-no-p (format "Unfollow %s? " name)))
      (tumbel-feed--act
       node
       (lambda (then else)
         (funcall (if followed #'tumbel-api-unfollow #'tumbel-api-follow)
                  url :then then :else else))
       (lambda (_)
         (tumbel-feed--set item 'followed (not followed))
         (message "%s %s" (if followed "Unfollowed" "Now following") name))))))

(defun tumbel-feed-delete ()
  "Delete the post at point, which must be on one of your blogs."
  (interactive)
  (let* ((node (tumbel-feed--node-at-point))
         (post (tumbel-feed-item-post (ewoc-data node)))
         (blog (tumbel-npf-post-blog-name post))
         (id (tumbel-npf-post-id post)))
    (unless (tumbel-user-own-blog-p blog)
      (user-error "You can only delete posts on your own blogs"))
    (when (yes-or-no-p (format "Delete post %s from %s? " id blog))
      (tumbel-feed--act
       node
       (lambda (then else)
         (tumbel-api-delete-post blog id :then then :else else))
       (lambda (_)
         (message "Deleted post %s" id)
         'remove)))))
(defun tumbel-feed-reblog-with-comment ()
  "Reblog the post at point with a comment written in a compose buffer."
  (interactive)
  (let ((post (tumbel-feed-post-at-point)))
    (unless (tumbel-feed--allowed-p post 'can_reblog)
      (user-error "This post cannot be reblogged"))
    (tumbel-compose-reblog post)))

(defun tumbel-feed-edit ()
  "Edit the post at point in a compose buffer."
  (interactive)
  (tumbel-compose-edit (tumbel-feed-post-at-point)))
(defun tumbel-feed-show-notes ()
  "Show the notes of the post at point."
  (interactive)
  (let ((post (tumbel-feed-item-post
               (ewoc-data (tumbel-feed--node-at-point)))))
    (funcall tumbel-npf-open-notes-function post)))
;;;; The single-post source

(defun tumbel-feed--complete-post (post blog)
  "Return POST with its blog name set to BLOG when it lacks one."
  (if (tumbel-npf-post-blog-name post)
      post
    (cons (cons 'blog_name blog) post)))

(defun tumbel-feed-post-source (blog id &optional post)
  "Return the source showing only the post ID of BLOG.
When POST, the post alist, is given it is shown right away and the
API is only asked when the buffer is reverted."
  (let ((cached post))
    (tumbel-feed-source-create
     :name (format "post/%s" id)
     :title (format "%s · %s" blog id)
     :expanded t
     :fetch (lambda (_cursor then else)
              (if cached
                  (let ((known cached))
                    (setq cached nil)
                    (funcall then (list known) nil)
                    nil)
                (tumbel-api-blog-post
                 blog id
                 :then (lambda (fetched)
                         (funcall then
                                  (list (tumbel-feed--complete-post fetched
                                                                    blog))
                                  nil))
                 :else else))))))

;;;; The dashboard source

(defun tumbel-dashboard-source ()
  "Return the source showing the dashboard of the logged-in user."
  (tumbel-feed-source-create
   :name "dashboard"
   :title "Dashboard"
   :fetch
   (lambda (cursor then else)
     (let ((offset (or cursor 0)))
       (tumbel-api-dashboard
        :params `((limit . ,tumbel-feed-page-size)
                  (offset . ,offset)
                  (reblog_info . t))
        :then (lambda (response)
                (let ((posts (alist-get 'posts response)))
                  (funcall then posts (and posts (+ offset (length posts))))))
        :else else)))))
(provide 'tumbel-feed)
;;; tumbel-feed.el ends here
