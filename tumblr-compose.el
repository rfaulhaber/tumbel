;;; tumblr-compose.el --- Write posts for tumblr.el  -*- lexical-binding: t; -*-

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

;; The compose buffer: a new post, a reblog with a comment, or an
;; edit of one of your posts.  The buffer is in Org mode and starts
;; with keyword lines (#+blog:, #+tags:, #+state:); the rest is the
;; post in the Org subset `tumblr-org' understands.  `C-c C-c' sends
;; it, uploading any images the text links to.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'tumblr-api)
(require 'tumblr-auth)
(require 'tumblr-org)
(require 'tumblr-user)

(defconst tumblr-compose-states '("published" "draft" "queue" "private")
  "The states a post can be created in.")

(defconst tumblr-compose-max-images 30
  "Number of image blocks a post may hold.")

(defconst tumblr-compose-max-text-length 4096
  "Number of characters a text block may hold.")

(defvar tumblr-compose-tag-history nil
  "History of tags entered when composing.")

(defvar-local tumblr-compose--reblog nil
  "The post being reblogged, or nil.")

(defvar-local tumblr-compose--edit nil
  "The post being edited as (BLOG . ID), or nil.")

(defvar-local tumblr-compose--passthrough nil
  "The blocks of the post being edited, in re-encodable form.")

(defvar-local tumblr-compose--layout nil
  "Layout entries sent along with the edited post, or nil.
Only the ask layout is kept: its blocks stay in front through the
placeholders, so their indices remain valid.")
(defvar-local tumblr-compose--sending nil
  "Non-nil while the post is on its way to Tumblr.")

;;;; Parsing the buffer

(defun tumblr-compose-parse (text)
  "Parse TEXT, the contents of a compose buffer, into a plist.
The leading lines of the form #+KEY: VALUE set `:blog', `:tags' (a
list), `:state' and `:publish-on'; the rest is the `:body'."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let (plist)
      (while (looking-at "^#\\+\\([A-Za-z_-]+\\):[ \t]*\\(.*\\)$")
        (let ((key (downcase (match-string 1)))
              (value (string-trim (match-string 2))))
          (pcase key
            ("blog" (setq plist (plist-put plist :blog
                                           (and (not (string-empty-p value))
                                                value))))
            ("tags" (setq plist (plist-put plist :tags
                                           (split-string value ","
                                                         t "[ \t]+"))))
            ("state" (setq plist (plist-put plist :state (downcase value))))
            ("publish_on" (setq plist (plist-put plist :publish-on
                                                 (and (not (string-empty-p
                                                            value))
                                                      value))))
            (_ (user-error "Unknown header #+%s" key))))
        (forward-line 1))
      (plist-put plist :body
                 (string-trim (buffer-substring-no-properties (point)
                                                              (point-max)))))))

(defun tumblr-compose--check-limits (blocks)
  "Signal a user error when BLOCKS exceed what Tumblr accepts."
  (let ((images 0))
    (seq-doseq (block blocks)
      (pcase (alist-get 'type block)
        ("image" (cl-incf images))
        ("text"
         (when (> (length (alist-get 'text block))
                  tumblr-compose-max-text-length)
           (user-error "A paragraph is longer than %d characters"
                       tumblr-compose-max-text-length)))))
    (when (> images tumblr-compose-max-images)
      (user-error "A post may hold at most %d images"
                  tumblr-compose-max-images))))

(defun tumblr-compose--request-body (plist)
  "Return (POST . FILES) to send for the parsed PLIST.
POST is the NPF post alist and FILES the images to upload with it."
  (let ((state (or (plist-get plist :state) "published"))
        (tags (plist-get plist :tags))
        (publish-on (plist-get plist :publish-on))
        (reblog tumblr-compose--reblog)
        (converted (tumblr-org-to-npf (plist-get plist :body)
                                      tumblr-compose--passthrough)))
    (unless (member state tumblr-compose-states)
      (user-error "Unknown state %s; use one of %s" state
                  (mapconcat #'identity tumblr-compose-states ", ")))
    (tumblr-compose--check-limits (car converted))
    (cons
     (append
      (list (cons 'content (car converted)))
      (when reblog
        `((parent_tumblelog_uuid . ,(tumblr-api-post-blog-uuid reblog))
          (parent_post_id . ,(tumblr-api--post-id reblog))
          (reblog_key . ,(alist-get 'reblog_key reblog))))
      (list (cons 'state state))
      (when tumblr-compose--layout
        (list (cons 'layout tumblr-compose--layout)))
      (when tags
        (list (cons 'tags (mapconcat #'identity tags ","))))
      (when publish-on
        (list (cons 'publish_on publish-on))))
     (cdr converted))))

;;;; The buffer

(defvar tumblr-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'tumblr-compose-send)
    (define-key map (kbd "C-c C-k") #'tumblr-compose-cancel)
    (define-key map (kbd "C-c C-t") #'tumblr-compose-set-tags)
    (define-key map (kbd "C-c C-b") #'tumblr-compose-set-blog)
    (define-key map (kbd "C-c C-s") #'tumblr-compose-set-state)
    (define-key map (kbd "C-c C-a") #'tumblr-compose-attach)
    (define-key map (kbd "C-c C-p") #'tumblr-compose-preview)
    map)
  "Keymap of `tumblr-compose-mode'.")

(define-derived-mode tumblr-compose-mode org-mode "Tumblr-Compose"
  "Major mode for writing a Tumblr post in Org syntax.
\\{tumblr-compose-mode-map}"
  (setq-local word-wrap t)
  (auto-fill-mode -1)
  (add-hook 'kill-buffer-query-functions #'tumblr-compose--confirm-kill nil t))

(defun tumblr-compose--confirm-kill ()
  "Ask before killing a compose buffer with unsent changes."
  (or (not (buffer-modified-p))
      (yes-or-no-p "Discard this unsent post? ")))

(defun tumblr-compose--header-line ()
  "Return the description of what the buffer will send."
  (concat (cond (tumblr-compose--edit
                 (format "%s %s/%s"
                         (if tumblr-compose--layout "Answering" "Editing")
                         (car tumblr-compose--edit)
                         (cdr tumblr-compose--edit)))
                (tumblr-compose--reblog
                 (format "Reblogging %s/%s"
                         (tumblr-api--post-blog-name tumblr-compose--reblog)
                         (tumblr-api--post-id tumblr-compose--reblog)))
                (t "New post"))
          (if tumblr-compose--sending
              " — sending…"
            " — C-c C-c to send, C-c C-k to discard")))

(defun tumblr-compose--update-header-line ()
  "Refresh the header line."
  (setq header-line-format (tumblr-compose--header-line))
  (force-mode-line-update))

(defun tumblr-compose--buffer (blog tags state body)
  "Create a compose buffer for BLOG with TAGS, STATE and BODY filled in.
Return the buffer, with point at the start of the body."
  (let ((buffer (generate-new-buffer "*tumblr: compose*")))
    (with-current-buffer buffer
      (tumblr-compose-mode)
      (insert (format "#+blog: %s\n" (or blog ""))
              (format "#+tags: %s\n" (mapconcat #'identity tags ", "))
              (format "#+state: %s\n\n" (or state "published")))
      (let ((start (point)))
        (insert (or body ""))
        (goto-char start))
      (set-buffer-modified-p nil))
    buffer))

(defun tumblr-compose--show (buffer)
  "Show the compose BUFFER and return it."
  (with-current-buffer buffer
    (tumblr-compose--update-header-line))
  (pop-to-buffer buffer)
  buffer)

(defun tumblr-compose--require-login ()
  "Signal a user error unless logged in."
  (unless (tumblr-auth-logged-in-p)
    (user-error "Log in first with M-x tumblr-login")))

;;;###autoload
(defun tumblr-compose ()
  "Write a new post to your default blog."
  (interactive)
  (tumblr-compose--require-login)
  (tumblr-compose--show
   (tumblr-compose--buffer (tumblr-user-default-blog) nil "published" nil)))

(defun tumblr-compose-reblog (post)
  "Reblog POST with a comment written in a compose buffer."
  (tumblr-compose--require-login)
  (let ((buffer (tumblr-compose--buffer (tumblr-user-default-blog) nil
                                        "published" nil)))
    (with-current-buffer buffer
      (setq tumblr-compose--reblog post))
    (tumblr-compose--show buffer)))

(defun tumblr-compose--ask-layout (post)
  "Return the ask entries of the layout of POST, a vector or nil."
  (let ((asks (seq-filter (lambda (entry) (equal (alist-get 'type entry) "ask"))
                          (alist-get 'layout post))))
    (and asks (vconcat asks))))

(defun tumblr-compose-edit (post &optional answer)
  "Edit POST, one of your posts, in a compose buffer.
The post is fetched again so that blocks other than text survive
the edit untouched.  With ANSWER non-nil the post is an ask waiting
in the inbox: its question is kept in front and the post is
published once sent."
  (tumblr-compose--require-login)
  (let ((blog (tumblr-api--post-blog-name post))
        (id (tumblr-api--post-id post)))
    (unless (tumblr-user-own-blog-p blog)
      (user-error "You can only edit posts on your own blogs"))
    (let* ((full (tumblr-api-blog-post blog id :fidelity t :then 'sync))
           (content (alist-get 'content full))
           (buffer (tumblr-compose--buffer
                    blog (append (alist-get 'tags full) nil)
                    (if answer "published" (or (alist-get 'state full)
                                               "published"))
                    (if answer
                        ;; The ask layout points at the question by
                        ;; index, so every existing block stays put.
                        (concat (mapconcat (lambda (index)
                                             (format "#+tumblr-block: %d"
                                                     index))
                                           (number-sequence
                                            0 (1- (length content)))
                                           "\n")
                                "\n\n")
                      (tumblr-org-from-npf content)))))
      (with-current-buffer buffer
        (setq tumblr-compose--edit (cons blog id)
              tumblr-compose--passthrough content
              tumblr-compose--layout (tumblr-compose--ask-layout full))
        (when answer
          (goto-char (point-max))))
      (tumblr-compose--show buffer))))

;;;; Editing the headers and body

(defun tumblr-compose--set-keyword (key value)
  "Set the header line #+KEY to VALUE, adding the line when missing."
  (save-excursion
    (goto-char (point-min))
    (let ((line (format "#+%s: %s" key value))
          (last-header (point-min)))
      (while (looking-at "^#\\+\\([A-Za-z_-]+\\):.*$")
        (if (equal (downcase (match-string 1)) key)
            (progn (delete-region (line-beginning-position)
                                  (line-end-position))
                   (insert line)
                   (setq key nil))
          (forward-line 1)
          (setq last-header (point))))
      (when key
        (goto-char last-header)
        (insert line "\n")))))

(defun tumblr-compose-set-tags (tags)
  "Set the TAGS of the post, read as a comma-separated list."
  (interactive
   (let ((current (plist-get (tumblr-compose-parse (buffer-string)) :tags)))
     (list (read-string "Tags (comma separated): "
                        (mapconcat #'identity current ", ")
                        'tumblr-compose-tag-history))))
  (tumblr-compose--set-keyword
   "tags" (mapconcat #'identity (split-string tags "," t "[ \t]+") ", ")))

(defun tumblr-compose-set-blog (blog)
  "Set the BLOG the post is made on, chosen among yours."
  (interactive
   (list (completing-read "Blog: " (tumblr-user-blog-names) nil t)))
  (tumblr-compose--set-keyword "blog" blog))

(defun tumblr-compose-set-state (state)
  "Set the STATE the post is created in."
  (interactive
   (list (completing-read "State: " tumblr-compose-states nil t)))
  (tumblr-compose--set-keyword "state" state))

(defun tumblr-compose-attach (file)
  "Insert a link to the image FILE, which is uploaded with the post."
  (interactive (list (read-file-name "Image: " nil nil t)))
  (unless (bolp)
    (insert "\n"))
  (unless (or (bobp) (looking-back "\n\n" (- (point) 2)))
    (insert "\n"))
  (insert (format "[[file:%s]]\n\n" (expand-file-name file))))

;;;; Sending

(defun tumblr-compose--validate (plist)
  "Signal a user error when the parsed PLIST cannot be sent."
  (unless (plist-get plist :blog)
    (user-error "No blog to post to; set the #+blog: line"))
  (when (and (string-empty-p (plist-get plist :body))
             (not tumblr-compose--reblog))
    (user-error "The post is empty")))

(defun tumblr-compose-preview-request (blog body)
  "Show BODY, a (POST . FILES) request for BLOG, in a preview buffer."
  (with-current-buffer (get-buffer-create "*tumblr: preview*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Blog: %s\n\n%s\n" blog (tumblr-http-encode (car body))))
      (dolist (file (cdr body))
        (insert (format "\nUpload %s as %s" (cdr file) (car file)))))
    (special-mode)
    (pop-to-buffer (current-buffer))))

(defun tumblr-compose-preview ()
  "Show the request that `tumblr-compose-send' would make."
  (interactive)
  (let ((plist (tumblr-compose-parse (buffer-string))))
    (tumblr-compose-preview-request (plist-get plist :blog)
                                    (tumblr-compose--request-body plist))))

(defun tumblr-compose-send ()
  "Send the post to Tumblr and kill the buffer when it is accepted."
  (interactive)
  (when tumblr-compose--sending
    (user-error "This post is already being sent"))
  (let* ((plist (tumblr-compose-parse (buffer-string)))
         (blog (plist-get plist :blog))
         (edit tumblr-compose--edit)
         (buffer (current-buffer))
         body)
    (tumblr-compose--validate plist)
    (setq body (tumblr-compose--request-body plist))
    (setq tumblr-compose--sending t
          buffer-read-only t)
    (tumblr-compose--update-header-line)
    (cl-flet ((done (response)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (set-buffer-modified-p nil)
                    (kill-buffer buffer)))
                (let ((id (or (alist-get 'id response) (cdr edit))))
                  (message "%s https://www.tumblr.com/%s/%s"
                           (if edit "Edited" "Posted") blog id)))
              (failed (err)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (setq tumblr-compose--sending nil
                          buffer-read-only nil)
                    (tumblr-compose--update-header-line)))
                (message "%s" (tumblr-http-error-string err))))
      (if edit
          (tumblr-api-edit-post (car edit) (cdr edit) (car body)
                                :files (cdr body)
                                :then #'done :else #'failed)
        (tumblr-api-create-post blog (car body) :files (cdr body)
                                :then #'done :else #'failed)))))

(defun tumblr-compose-cancel ()
  "Discard the post."
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'tumblr-compose)
;;; tumblr-compose.el ends here
