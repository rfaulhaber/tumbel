;;; tumbel-npf.el --- Render Neue Post Format posts  -*- lexical-binding: t; -*-

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

;; Turns NPF posts, as alists parsed by `tumbel-http-parse-json', into
;; propertized text.  Every block is built as a string first, so the
;; formatting ranges the API expresses as code point offsets map
;; straight onto string positions; only then is the text inserted.
;; Buttons carry data alone and call the `tumbel-npf-*-function'
;; variables, which keeps this file below the feed and blog layers.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'let-alist)
(require 'seq)
(require 'subr-x)
(require 'tumbel-media)
(require 'url-util)

;;;; Faces

(defface tumbel-blog-name '((t :inherit bold))
  "Face for blog names."
  :group 'tumbel)

(defface tumbel-heading1 '((t :inherit bold :height 1.3))
  "Face for level one headings."
  :group 'tumbel)

(defface tumbel-heading2 '((t :inherit bold :height 1.15))
  "Face for level two headings."
  :group 'tumbel)

(defface tumbel-quote '((t :inherit italic :height 1.1))
  "Face for quote blocks."
  :group 'tumbel)

(defface tumbel-quirky '((t :inherit italic :height 1.2))
  "Face for quirky blocks."
  :group 'tumbel)

(defface tumbel-indented '((t :inherit default))
  "Face for indented (block quote) text."
  :group 'tumbel)

(defface tumbel-chat '((t :inherit fixed-pitch))
  "Face for chat lines."
  :group 'tumbel)

(defface tumbel-small '((t :height 0.85))
  "Face for small text."
  :group 'tumbel)

(defface tumbel-tag '((t :inherit font-lock-keyword-face))
  "Face for tags."
  :group 'tumbel)

(defface tumbel-meta '((t :inherit shadow))
  "Face for dates, counts and other metadata."
  :group 'tumbel)

(defface tumbel-link '((t :inherit link))
  "Face for links."
  :group 'tumbel)

(defface tumbel-mention '((t :inherit font-lock-constant-face))
  "Face for blog mentions."
  :group 'tumbel)

(defface tumbel-ask '((t :inherit italic))
  "Face for the question of an ask."
  :group 'tumbel)

;;;; Hooks into the layers above

(defvar tumbel-npf-open-blog-function 'tumbel-blog
  "Function called with a blog name to show that blog.")

(defvar tumbel-npf-open-tag-function 'tumbel-tag
  "Function called with a tag to browse the posts carrying it.")

(defvar tumbel-npf-open-url-function #'browse-url
  "Function called with a URL to open it.")

(defvar tumbel-npf-open-notes-function 'tumbel-notes
  "Function called with a post to show its notes.")
(defvar tumbel-npf-toggle-function #'ignore
  "Function called with a \"Keep reading\" button to expand its post.
Buffers that can re-render a post set it buffer-locally.")

;;;; Buttons

(defun tumbel-npf--blog-action (button)
  "Open the blog named by BUTTON."
  (funcall tumbel-npf-open-blog-function
           (button-get button 'tumbel-blog-name)))

(defun tumbel-npf--tag-action (button)
  "Browse the tag carried by BUTTON."
  (funcall tumbel-npf-open-tag-function (button-get button 'tumbel-tag)))

(defun tumbel-npf--link-action (button)
  "Open the URL carried by BUTTON."
  (funcall tumbel-npf-open-url-function (button-get button 'tumbel-url)))

(defun tumbel-npf--toggle-action (button)
  "Expand the post that BUTTON belongs to."
  (funcall tumbel-npf-toggle-function button))

(define-button-type 'tumbel-blog
  'action #'tumbel-npf--blog-action
  'follow-link t
  'face 'tumbel-blog-name
  'help-echo "Open this blog")

(define-button-type 'tumbel-mention
  'action #'tumbel-npf--blog-action
  'follow-link t
  'face 'tumbel-mention
  'help-echo "Open this blog")

(define-button-type 'tumbel-tag
  'action #'tumbel-npf--tag-action
  'follow-link t
  'face 'tumbel-tag
  'help-echo "Browse this tag")

(define-button-type 'tumbel-link
  'action #'tumbel-npf--link-action
  'follow-link t
  'face 'tumbel-link
  'help-echo "Open this link")

(define-button-type 'tumbel-toggle
  'action #'tumbel-npf--toggle-action
  'follow-link t
  'face 'tumbel-link
  'help-echo "Show the whole post")

(defun tumbel-npf-buttonize (string start end type &rest properties)
  "Make the part of STRING from START to END a button of TYPE.
PROPERTIES are added to the button.  STRING is modified in place
and returned."
  (add-text-properties start end
                       (append (list 'category (button-category-symbol type)
                                     'button (list t))
                               properties)
                       string)
  string)

(defun tumbel-npf-button (label type &rest properties)
  "Return a copy of LABEL made into a button of TYPE with PROPERTIES."
  (apply #'tumbel-npf-buttonize (copy-sequence label) 0 (length label)
         type properties))

(defun tumbel-npf-blog-button (name)
  "Return NAME as a button opening that blog."
  (tumbel-npf-button name 'tumbel-blog 'tumbel-blog-name name))

(defun tumbel-npf-tag-button (tag)
  "Return TAG, prefixed with #, as a button browsing that tag."
  (tumbel-npf-button (concat "#" tag) 'tumbel-tag 'tumbel-tag tag))

(defun tumbel-npf-link-button (label url)
  "Return LABEL as a button opening URL."
  (tumbel-npf-button label 'tumbel-link 'tumbel-url url))

;;;; Post accessors

(defun tumbel-npf-post-id (post)
  "Return the id of POST as a string, or nil."
  (or (alist-get 'id_string post)
      (let ((id (alist-get 'id post)))
        (and id (format "%s" id)))))

(defun tumbel-npf-post-blog-name (post)
  "Return the name of the blog POST belongs to, or nil."
  (or (alist-get 'blog_name post)
      (alist-get 'name (alist-get 'blog post))))

(defun tumbel-npf-post-url (post)
  "Return the web URL of POST, or nil."
  (alist-get 'post_url post))

(defun tumbel-npf-blog-url (name)
  "Return the web URL of the blog NAME.
The tumblr.com form reaches blogs on custom domains too."
  (format "https://www.tumblr.com/%s" name))

(defun tumbel-npf-tag-url (tag)
  "Return the web URL of the posts tagged TAG."
  (format "https://www.tumblr.com/tagged/%s" (url-hexify-string tag)))

(defun tumbel-npf-post-tags (post)
  "Return the list of tags of POST."
  (alist-get 'tags post))

(defun tumbel-npf-relative-time (timestamp &optional now)
  "Describe TIMESTAMP, in seconds since the epoch, relative to NOW.
NOW defaults to the current time.  Recent times become \"now\",
\"5m\", \"3h\" or \"2d\"; older ones become a date."
  (let ((delta (- (or now (float-time)) timestamp)))
    (cond ((< delta 60) "now")
          ((< delta 3600) (format "%dm" (/ delta 60)))
          ((< delta 86400) (format "%dh" (/ delta 3600)))
          ((< delta (* 7 86400)) (format "%dd" (/ delta 86400)))
          (t (let* ((decoded (decode-time timestamp))
                    (month (format-time-string "%b" timestamp))
                    (day (nth 3 decoded)))
               (if (< delta (* 365 86400))
                   (format "%s %d" month day)
                 (format "%s %d, %d" month day (nth 5 decoded))))))))

;;;; Text blocks

(defconst tumbel-npf--subtype-faces
  '(("heading1" . tumbel-heading1)
    ("heading2" . tumbel-heading2)
    ("quote" . tumbel-quote)
    ("quirky" . tumbel-quirky)
    ("chat" . tumbel-chat)
    ("indented" . tumbel-indented))
  "Faces applied to whole text blocks, by subtype.")

(defconst tumbel-npf--run-subtypes
  '("ordered-list-item" "unordered-list-item" "chat" "indented")
  "Subtypes whose consecutive blocks are shown without a blank line.")

(defun tumbel-npf--apply-formatting (text range)
  "Apply the formatting RANGE of an NPF text block to the string TEXT."
  (let* ((len (length text))
         (start (min len (max 0 (or (alist-get 'start range) 0))))
         (end (min len (max 0 (or (alist-get 'end range) 0)))))
    (when (< start end)
      (pcase (alist-get 'type range)
        ("bold" (add-face-text-property start end 'bold nil text))
        ("italic" (add-face-text-property start end 'italic nil text))
        ("strikethrough"
         (add-face-text-property start end '(:strike-through t) nil text))
        ("small" (add-face-text-property start end 'tumbel-small nil text))
        ("color"
         (when-let* ((hex (alist-get 'hex range)))
           (add-face-text-property start end (list :foreground hex)
                                   nil text)))
        ("link"
         (when-let* ((url (alist-get 'url range)))
           (tumbel-npf-buttonize text start end 'tumbel-link 'tumbel-url url)
           (add-face-text-property start end 'tumbel-link t text)))
        ("mention"
         (when-let* ((name (alist-get 'name (alist-get 'blog range))))
           (tumbel-npf-buttonize text start end 'tumbel-mention
                                 'tumbel-blog-name name)
           (add-face-text-property start end 'tumbel-mention t text)))))))

(defun tumbel-npf--text-block-string (block ordinal)
  "Return the text BLOCK rendered as a line ending in a newline.
ORDINAL numbers an ordered list item."
  (let* ((text (copy-sequence (or (alist-get 'text block) "")))
         (subtype (alist-get 'subtype block))
         (indent (make-string (* 2 (or (alist-get 'indent_level block) 0))
                              ?\s))
         (face (cdr (assoc subtype tumbel-npf--subtype-faces)))
         (prefix (pcase subtype
                   ("unordered-list-item" (concat indent "• "))
                   ("ordered-list-item" (concat indent (format "%d. " ordinal)))
                   ("indented" (concat indent "│ "))
                   (_ (and (not (string-empty-p indent)) indent))))
         line)
    (dolist (range (alist-get 'formatting block))
      (tumbel-npf--apply-formatting text range))
    (when face
      (add-face-text-property 0 (length text) face t text))
    (setq line (concat text "\n"))
    (when prefix
      (add-text-properties 0 (length line)
                           (list 'line-prefix
                                 (propertize prefix 'face 'tumbel-meta)
                                 'wrap-prefix
                                 (make-string (length prefix) ?\s))
                           line))
    line))

;;;; Other blocks

(defun tumbel-npf--media-entries (media)
  "Return MEDIA, a media object or a list of them, as a list."
  (cond ((null media) nil)
        ((and (consp (car media)) (consp (caar media))) media)
        (t (list media))))

(defun tumbel-npf--image-block-string (block)
  "Return the image BLOCK as placeholder text and caption."
  (let* ((entry (tumbel-media-pick
                 (tumbel-npf--media-entries (alist-get 'media block))))
         (url (alist-get 'url entry))
         (caption (alist-get 'caption block)))
    (concat (if url
                (tumbel-media-image-string url (alist-get 'alt_text block))
              (propertize "[image]" 'face 'tumbel-placeholder))
            "\n"
            (and caption (not (string-empty-p caption))
                 (concat (propertize caption 'face 'tumbel-meta) "\n")))))

(defun tumbel-npf--link-block-string (block)
  "Return the link BLOCK as a button followed by its description."
  (let-alist block
    (let ((meta (delq nil (list .site_name .description))))
      (concat (if .url
                  (tumbel-npf-link-button (or .title .display_url .url) .url)
                (propertize (or .title "[link]") 'face 'tumbel-link))
              "\n"
              (and meta
                   (concat (propertize (mapconcat #'identity meta " · ")
                                       'face 'tumbel-meta)
                           "\n"))))))

(defun tumbel-npf--media-url (block)
  "Return the URL of the media of the audio or video BLOCK, or nil."
  (or (alist-get 'url block)
      (alist-get 'embed_url block)
      (alist-get 'url (car (tumbel-npf--media-entries
                            (alist-get 'media block))))))

(defun tumbel-npf--audio-block-string (block)
  "Return the audio BLOCK as one descriptive line."
  (let-alist block
    (let* ((title (mapconcat #'identity (delq nil (list .title .artist))
                             " — "))
           (label (concat "♫ " (if (string-empty-p title) "audio" title)
                          (and .provider (format " [%s]" .provider))))
           (url (tumbel-npf--media-url block)))
      (concat (if url (tumbel-npf-link-button label url) label) "\n"))))

(defun tumbel-npf--video-block-string (block)
  "Return the video BLOCK as one descriptive line."
  (let-alist block
    (let ((label (concat "▶ video"
                         (and .provider (format " [%s]" .provider))))
          (url (tumbel-npf--media-url block)))
      (concat (if url (tumbel-npf-link-button label url) label) "\n"))))

(defun tumbel-npf--poll-block-string (block)
  "Return the poll BLOCK as its question and answers."
  (let-alist block
    (concat (propertize (format "Poll: %s" (or .question "")) 'face 'bold)
            "\n"
            (mapconcat (lambda (answer)
                         (concat "  ○ " (or (alist-get 'answer_text answer) "")
                                 "\n"))
                       .answers ""))))

(defun tumbel-npf--paywall-block-string (block)
  "Return the paywall BLOCK as a short notice."
  (let-alist block
    (let ((label (format "[Tumblr+ %s]"
                         (mapconcat #'identity (delq nil (list .title .text))
                                    " — "))))
      (concat (if .url
                  (tumbel-npf-link-button label .url)
                (propertize label 'face 'tumbel-meta))
              "\n"))))

(defun tumbel-npf--block-string (block ordinal)
  "Return BLOCK rendered as text ending in a newline.
ORDINAL numbers an ordered list item."
  (pcase (alist-get 'type block)
    ("text" (tumbel-npf--text-block-string block ordinal))
    ("image" (tumbel-npf--image-block-string block))
    ("link" (tumbel-npf--link-block-string block))
    ("audio" (tumbel-npf--audio-block-string block))
    ("video" (tumbel-npf--video-block-string block))
    ("poll" (tumbel-npf--poll-block-string block))
    ("paywall" (tumbel-npf--paywall-block-string block))
    (type (concat (propertize (format "[%s block]" (or type "unknown"))
                              'face 'tumbel-meta)
                  "\n"))))

;;;; Layout

(defun tumbel-npf--layout-of (layout type)
  "Return the entry of LAYOUT whose type is TYPE, or nil."
  (seq-find (lambda (entry) (equal (alist-get 'type entry) type)) layout))

(defun tumbel-npf--truncate-after (layout)
  "Return the index of the last block LAYOUT shows before truncating."
  (seq-some (lambda (entry)
              (and (member (alist-get 'type entry) '("rows" "condensed"))
                   (alist-get 'truncate_after entry)))
            layout))

(defun tumbel-npf--ask-header-string (attribution)
  "Return the line introducing an ask sent by ATTRIBUTION."
  (let ((name (alist-get 'name (alist-get 'blog attribution))))
    (concat (if name
                (tumbel-npf-blog-button name)
              (propertize "Anonymous" 'face 'tumbel-blog-name))
            (propertize " asked:" 'face 'tumbel-meta)
            "\n")))

(defun tumbel-npf--keep-reading-button ()
  "Return the button that expands a truncated post."
  (tumbel-npf-button "[Keep reading]" 'tumbel-toggle))

(defun tumbel-npf--same-run-p (a b)
  "Return non-nil when text blocks A and B belong to one list or run."
  (let ((subtype (alist-get 'subtype a)))
    (and (equal (alist-get 'type a) "text")
         (equal (alist-get 'type b) "text")
         (equal subtype (alist-get 'subtype b))
         (member subtype tumbel-npf--run-subtypes))))

(defun tumbel-npf-insert-blocks (blocks layout &optional expanded)
  "Insert the content BLOCKS arranged according to LAYOUT at point.
Unless EXPANDED is non-nil, a layout that truncates the content ends
with a \"Keep reading\" button instead of the remaining blocks."
  (let* ((truncate (and (not expanded) (tumbel-npf--truncate-after layout)))
         (ask (tumbel-npf--layout-of layout "ask"))
         (ask-blocks (alist-get 'blocks ask))
         (ordinal 0)
         (index 0)
         (previous nil)
         (in-ask nil))
    (catch 'truncated
      (dolist (block blocks)
        (when (and truncate (> index truncate))
          (insert (tumbel-npf--keep-reading-button) "\n")
          (throw 'truncated nil))
        (let ((asked (and (memql index ask-blocks) t)))
          (when (and previous (not (tumbel-npf--same-run-p previous block)))
            (insert "\n"))
          (when (and asked (not in-ask))
            (insert (tumbel-npf--ask-header-string
                     (alist-get 'attribution ask))))
          (setq in-ask asked
                ordinal (if (equal (alist-get 'subtype block)
                                   "ordered-list-item")
                            (1+ ordinal)
                          0))
          (let ((string (tumbel-npf--block-string block ordinal)))
            (when asked
              (add-face-text-property 0 (length string) 'tumbel-ask t string))
            (insert string))
          (setq previous block
                index (1+ index)))))))

;;;; Posts

(defun tumbel-npf--insert-trail-item (item expanded)
  "Insert the reblog trail ITEM; EXPANDED shows truncated content in full."
  (let ((broken (alist-get 'broken_blog_name item))
        (name (alist-get 'name (alist-get 'blog item))))
    (cond (broken
           (insert (propertize (format "%s (deactivated):" broken)
                               'face 'tumbel-meta)
                   "\n"))
          (name
           (insert (tumbel-npf-blog-button name)
                   (propertize ":" 'face 'tumbel-meta)
                   "\n")))
    (tumbel-npf-insert-blocks (alist-get 'content item)
                              (alist-get 'layout item)
                              expanded)))

(defun tumbel-npf--footer-string (post)
  "Return the note count and like state of POST, or nil."
  (let ((notes (alist-get 'note_count post))
        parts)
    (when notes
      (push (format "%s %s" notes (if (eql notes 1) "note" "notes")) parts))
    (when (alist-get 'liked post)
      (push "liked" parts))
    (and parts
         (propertize (mapconcat #'identity (nreverse parts) " · ")
                     'face 'tumbel-meta))))

(cl-defun tumbel-npf-insert-post (post &key expanded now)
  "Insert POST, an NPF post alist, at point.
When EXPANDED is non-nil, truncated content is shown in full.  NOW
is the time relative dates are computed against; it defaults to the
current time."
  (let* ((blog (or (tumbel-npf-post-blog-name post) "?"))
         (trail (alist-get 'trail post))
         (content (alist-get 'content post))
         (from (alist-get 'reblogged_from_name post))
         (timestamp (alist-get 'timestamp post))
         (tags (tumbel-npf-post-tags post))
         (footer (tumbel-npf--footer-string post))
         (first t))
    (insert (tumbel-npf-blog-button blog))
    (cond (from
           (insert (propertize " ↻ " 'face 'tumbel-meta)
                   (tumbel-npf-blog-button from)))
          ((and trail (null content))
           (insert (propertize " ↻" 'face 'tumbel-meta))))
    (when timestamp
      (insert (propertize (concat " · "
                                  (tumbel-npf-relative-time timestamp now))
                          'face 'tumbel-meta)))
    (insert "\n")
    (dolist (item trail)
      (unless first (insert "\n"))
      (setq first nil)
      (tumbel-npf--insert-trail-item item expanded))
    (cond (content
           (unless first (insert "\n"))
           (when trail
             (insert (tumbel-npf-blog-button blog)
                     (propertize ":" 'face 'tumbel-meta)
                     "\n"))
           (tumbel-npf-insert-blocks content (alist-get 'layout post) expanded))
          ((null trail)
           (insert (propertize (or (alist-get 'summary post) "[empty post]")
                               'face 'tumbel-meta)
                   "\n")))
    (when (or tags footer)
      (insert "\n"))
    (when tags
      (insert (mapconcat #'tumbel-npf-tag-button tags " ") "\n"))
    (when footer
      (insert footer "\n"))))

(provide 'tumbel-npf)
;;; tumbel-npf.el ends here
