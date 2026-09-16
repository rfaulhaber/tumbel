;;; tumbel-compose.el --- Write posts for tumbel.el  -*- lexical-binding: t; -*-

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
;; post in the Org subset `tumbel-org' understands.  `C-c C-c' sends
;; it, uploading any images the text links to.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'tumbel-api)
(require 'tumbel-auth)
(require 'tumbel-org)
(require 'tumbel-user)

(defconst tumbel-compose-states '("published" "draft" "queue" "private")
  "The states a post can be created in.")

(defconst tumbel-compose-max-images 30
  "Number of image blocks a post may hold.")

(defconst tumbel-compose-max-text-length 4096
  "Number of characters a text block may hold.")

(defvar tumbel-compose-tag-history nil
  "History of tags entered when composing.")

(defvar-local tumbel-compose--reblog nil
  "The post being reblogged, or nil.")

(defvar-local tumbel-compose--edit nil
  "The post being edited as (BLOG . ID), or nil.")

(defvar-local tumbel-compose--passthrough nil
  "The blocks of the post being edited, in re-encodable form.")

(defvar-local tumbel-compose--layout nil
  "Layout entries sent along with the edited post, or nil.
Only the ask layout is kept: its blocks stay in front through the
placeholders, so their indices remain valid.")
(defvar-local tumbel-compose--sending nil
  "Non-nil while the post is on its way to Tumblr.")

;;;; Parsing the buffer

(defun tumbel-compose-parse (text)
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

(defun tumbel-compose--check-limits (blocks)
  "Signal a user error when BLOCKS exceed what Tumblr accepts."
  (let ((images 0))
    (seq-doseq (block blocks)
      (pcase (alist-get 'type block)
        ("image" (cl-incf images))
        ("text"
         (when (> (length (alist-get 'text block))
                  tumbel-compose-max-text-length)
           (user-error "A paragraph is longer than %d characters"
                       tumbel-compose-max-text-length)))))
    (when (> images tumbel-compose-max-images)
      (user-error "A post may hold at most %d images"
                  tumbel-compose-max-images))))

(defun tumbel-compose--request-body (plist)
  "Return (POST . FILES) to send for the parsed PLIST.
POST is the NPF post alist and FILES the images to upload with it."
  (let ((state (or (plist-get plist :state) "published"))
        (tags (plist-get plist :tags))
        (publish-on (plist-get plist :publish-on))
        (reblog tumbel-compose--reblog)
        (converted (tumbel-org-to-npf (plist-get plist :body)
                                      tumbel-compose--passthrough)))
    (unless (member state tumbel-compose-states)
      (user-error "Unknown state %s; use one of %s" state
                  (mapconcat #'identity tumbel-compose-states ", ")))
    (tumbel-compose--check-limits (car converted))
    (cons
     (append
      (list (cons 'content (car converted)))
      (when reblog
        `((parent_tumblelog_uuid . ,(tumbel-api-post-blog-uuid reblog))
          (parent_post_id . ,(tumbel-api--post-id reblog))
          (reblog_key . ,(alist-get 'reblog_key reblog))))
      (list (cons 'state state))
      (when tumbel-compose--layout
        (list (cons 'layout tumbel-compose--layout)))
      (when tags
        (list (cons 'tags (mapconcat #'identity tags ","))))
      (when publish-on
        (list (cons 'publish_on publish-on))))
     (cdr converted))))

;;;; The buffer

(defvar tumbel-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'tumbel-compose-send)
    (define-key map (kbd "C-c C-k") #'tumbel-compose-cancel)
    (define-key map (kbd "C-c C-t") #'tumbel-compose-set-tags)
    (define-key map (kbd "C-c C-b") #'tumbel-compose-set-blog)
    (define-key map (kbd "C-c C-s") #'tumbel-compose-set-state)
    (define-key map (kbd "C-c C-a") #'tumbel-compose-attach)
    (define-key map (kbd "C-c C-p") #'tumbel-compose-preview)
    map)
  "Keymap of `tumbel-compose-mode'.")

(define-derived-mode tumbel-compose-mode org-mode "Tumbel-Compose"
  "Major mode for writing a Tumblr post in Org syntax.
\\{tumbel-compose-mode-map}"
  (setq-local word-wrap t)
  (auto-fill-mode -1)
  (add-hook 'kill-buffer-query-functions #'tumbel-compose--confirm-kill nil t))

(defun tumbel-compose--confirm-kill ()
  "Ask before killing a compose buffer with unsent changes."
  (or (not (buffer-modified-p))
      (yes-or-no-p "Discard this unsent post? ")))

(defun tumbel-compose--header-line ()
  "Return the description of what the buffer will send."
  (concat (cond (tumbel-compose--edit
                 (format "%s %s/%s"
                         (if tumbel-compose--layout "Answering" "Editing")
                         (car tumbel-compose--edit)
                         (cdr tumbel-compose--edit)))
                (tumbel-compose--reblog
                 (format "Reblogging %s/%s"
                         (tumbel-api--post-blog-name tumbel-compose--reblog)
                         (tumbel-api--post-id tumbel-compose--reblog)))
                (t "New post"))
          (if tumbel-compose--sending
              " — sending…"
            " — C-c C-c to send, C-c C-k to discard")))

(defun tumbel-compose--update-header-line ()
  "Refresh the header line."
  (setq header-line-format (tumbel-compose--header-line))
  (force-mode-line-update))

(defun tumbel-compose--buffer (blog tags state body)
  "Create a compose buffer for BLOG with TAGS, STATE and BODY filled in.
Return the buffer, with point at the start of the body."
  (let ((buffer (generate-new-buffer "*tumbel: compose*")))
    (with-current-buffer buffer
      (tumbel-compose-mode)
      (insert (format "#+blog: %s\n" (or blog ""))
              (format "#+tags: %s\n" (mapconcat #'identity tags ", "))
              (format "#+state: %s\n\n" (or state "published")))
      (let ((start (point)))
        (insert (or body ""))
        (goto-char start))
      (set-buffer-modified-p nil))
    buffer))

(defun tumbel-compose--show (buffer)
  "Show the compose BUFFER and return it."
  (with-current-buffer buffer
    (tumbel-compose--update-header-line))
  (pop-to-buffer buffer)
  buffer)

(defun tumbel-compose--require-login ()
  "Signal a user error unless logged in."
  (unless (tumbel-auth-logged-in-p)
    (user-error "Log in first with M-x tumbel-login")))

;;;###autoload
(defun tumbel-compose ()
  "Write a new post to your default blog."
  (interactive)
  (tumbel-compose--require-login)
  (tumbel-compose--show
   (tumbel-compose--buffer (tumbel-user-default-blog) nil "published" nil)))

(defun tumbel-compose-reblog (post)
  "Reblog POST with a comment written in a compose buffer."
  (tumbel-compose--require-login)
  (let ((buffer (tumbel-compose--buffer (tumbel-user-default-blog) nil
                                        "published" nil)))
    (with-current-buffer buffer
      (setq tumbel-compose--reblog post))
    (tumbel-compose--show buffer)))

(defun tumbel-compose--ask-layout (post)
  "Return the ask entries of the layout of POST, a vector or nil."
  (let ((asks (seq-filter (lambda (entry) (equal (alist-get 'type entry) "ask"))
                          (alist-get 'layout post))))
    (and asks (vconcat asks))))

(defun tumbel-compose-edit (post &optional answer)
  "Edit POST, one of your posts, in a compose buffer.
The post is fetched again so that blocks other than text survive
the edit untouched.  With ANSWER non-nil the post is an ask waiting
in the inbox: its question is kept in front and the post is
published once sent."
  (tumbel-compose--require-login)
  (let ((blog (tumbel-api--post-blog-name post))
        (id (tumbel-api--post-id post)))
    (unless (tumbel-user-own-blog-p blog)
      (user-error "You can only edit posts on your own blogs"))
    (let* ((full (tumbel-api-blog-post blog id :fidelity t :then 'sync))
           (content (alist-get 'content full))
           (buffer (tumbel-compose--buffer
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
                      (tumbel-org-from-npf content)))))
      (with-current-buffer buffer
        (setq tumbel-compose--edit (cons blog id)
              tumbel-compose--passthrough content
              tumbel-compose--layout (tumbel-compose--ask-layout full))
        (when answer
          (goto-char (point-max))))
      (tumbel-compose--show buffer))))

;;;; Editing the headers and body

(defun tumbel-compose--set-keyword (key value)
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

(defun tumbel-compose-set-tags (tags)
  "Set the TAGS of the post, read as a comma-separated list."
  (interactive
   (let ((current (plist-get (tumbel-compose-parse (buffer-string)) :tags)))
     (list (read-string "Tags (comma separated): "
                        (mapconcat #'identity current ", ")
                        'tumbel-compose-tag-history))))
  (tumbel-compose--set-keyword
   "tags" (mapconcat #'identity (split-string tags "," t "[ \t]+") ", ")))

(defun tumbel-compose-set-blog (blog)
  "Set the BLOG the post is made on, chosen among yours."
  (interactive
   (list (completing-read "Blog: " (tumbel-user-blog-names) nil t)))
  (tumbel-compose--set-keyword "blog" blog))

(defun tumbel-compose-set-state (state)
  "Set the STATE the post is created in."
  (interactive
   (list (completing-read "State: " tumbel-compose-states nil t)))
  (tumbel-compose--set-keyword "state" state))

(defun tumbel-compose-attach (file)
  "Insert a link to the image FILE, which is uploaded with the post."
  (interactive (list (read-file-name "Image: " nil nil t)))
  (unless (bolp)
    (insert "\n"))
  (unless (or (bobp) (looking-back "\n\n" (- (point) 2)))
    (insert "\n"))
  (insert (format "[[file:%s]]\n\n" (expand-file-name file))))

;;;; Sending

(defun tumbel-compose--validate (plist)
  "Signal a user error when the parsed PLIST cannot be sent."
  (unless (plist-get plist :blog)
    (user-error "No blog to post to; set the #+blog: line"))
  (when (and (string-empty-p (plist-get plist :body))
             (not tumbel-compose--reblog))
    (user-error "The post is empty")))

(defun tumbel-compose-preview-request (blog body)
  "Show BODY, a (POST . FILES) request for BLOG, in a preview buffer."
  (with-current-buffer (get-buffer-create "*tumbel: preview*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Blog: %s\n\n%s\n" blog (tumbel-http-encode (car body))))
      (dolist (file (cdr body))
        (insert (format "\nUpload %s as %s" (cdr file) (car file)))))
    (special-mode)
    (pop-to-buffer (current-buffer))))

(defun tumbel-compose-preview ()
  "Show the request that `tumbel-compose-send' would make."
  (interactive)
  (let ((plist (tumbel-compose-parse (buffer-string))))
    (tumbel-compose-preview-request (plist-get plist :blog)
                                    (tumbel-compose--request-body plist))))

(defun tumbel-compose-send ()
  "Send the post to Tumblr and kill the buffer when it is accepted."
  (interactive)
  (when tumbel-compose--sending
    (user-error "This post is already being sent"))
  (let* ((plist (tumbel-compose-parse (buffer-string)))
         (blog (plist-get plist :blog))
         (edit tumbel-compose--edit)
         (buffer (current-buffer))
         body)
    (tumbel-compose--validate plist)
    (setq body (tumbel-compose--request-body plist))
    (setq tumbel-compose--sending t
          buffer-read-only t)
    (tumbel-compose--update-header-line)
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
                    (setq tumbel-compose--sending nil
                          buffer-read-only nil)
                    (tumbel-compose--update-header-line)))
                (message "%s" (tumbel-http-error-string err))))
      (if edit
          (tumbel-api-edit-post (car edit) (cdr edit) (car body)
                                :files (cdr body)
                                :then #'done :else #'failed)
        (tumbel-api-create-post blog (car body) :files (cdr body)
                                :then #'done :else #'failed)))))

(defun tumbel-compose-cancel ()
  "Discard the post."
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'tumbel-compose)
;;; tumbel-compose.el ends here
