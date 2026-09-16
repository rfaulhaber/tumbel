;;; tumbel-org.el --- Org syntax for Tumblr posts  -*- lexical-binding: t; -*-

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

;; Posts are written in a subset of Org and converted into NPF blocks
;; by `tumbel-org-to-npf': headlines become headings, paragraphs text
;; blocks with formatting ranges for *bold*, /italic/, +struck+ text,
;; links and [[tumblr:NAME]] mentions, lists become list items, quote
;; blocks indented text, verse and source blocks chat lines, a
;; paragraph holding only a file: link to an image becomes an image
;; upload, and one holding only a URL a link block.
;;
;; `tumbel-org-from-npf' turns the text blocks of an existing post
;; back into Org so it can be edited; every other block becomes a
;; #+tumblr-block: N line that the conversion passes through
;; untouched, so edits never drop media.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)
(require 'tumbel-api)

;; Following a mention opens the blog, a command of a file above this one
;; in the dependency order.
(declare-function tumbel-blog "tumbel-blog" (blog &optional tag))

(defvar tumbel-org-blog-uuid-function #'tumbel-api-blog-uuid
  "Function returning the UUID of the blog named by its argument.
Mentions need it; the default asks the API once per blog.")

(defconst tumbel-org-image-extensions '("png" "jpg" "jpeg" "gif" "webp")
  "Extensions of files that a file: link can upload as an image.")

(org-link-set-parameters "tumblr"
                         :follow (lambda (name _arg) (tumbel-blog name))
                         :face 'tumbel-mention)

;;;; Org to NPF

(cl-defstruct (tumbel-org--state (:constructor tumbel-org--state-create)
                                 (:copier nil))
  "What a conversion has produced so far: BLOCKS (newest first),
FILES to upload, the COUNT of images so far, the SUBTYPE text blocks
currently take, and the PASSTHROUGH blocks placeholders refer to."
  (blocks nil) (files nil) (count 0) (subtype nil) (passthrough nil))

(defun tumbel-org--push (state block)
  "Add BLOCK to the output of STATE."
  (push block (tumbel-org--state-blocks state)))

(defun tumbel-org--text-block (inline &optional subtype indent)
  "Return an NPF text block from INLINE, a cons (TEXT . FORMATTING).
SUBTYPE and INDENT are added when given and meaningful."
  (append `((type . "text") (text . ,(car inline)))
          (when subtype `((subtype . ,subtype)))
          (when (and indent (> indent 0)) `((indent_level . ,indent)))
          (when (cdr inline) `((formatting . ,(vconcat (cdr inline)))))))

(defun tumbel-org--unfill (string)
  "Return STRING with the newlines that fill a paragraph turned into spaces."
  (replace-regexp-in-string "[ \t]*\n[ \t]*" " " string))

(defun tumbel-org--inline (objects)
  "Return (TEXT . FORMATTING) for the paragraph contents OBJECTS.
FORMATTING is a list of NPF formatting ranges over TEXT, whose
offsets count characters, as NPF does."
  (let ((text "")
        (formatting nil))
    (cl-labels
        ((emit (string)
           (setq text (concat text (substring-no-properties string))))
         (walk (objects) (mapc #'convert objects))
         (range (type start &rest fields)
           (push (append `((type . ,type) (start . ,start)
                           (end . ,(length text)))
                         fields)
                 formatting))
         (convert-link (link)
           (let ((type (org-element-property :type link))
                 (contents (org-element-contents link))
                 (start (length text)))
             (cond
              ((equal type "tumblr")
               (let ((name (org-element-property :path link)))
                 (if contents (walk contents) (emit (concat "@" name)))
                 (range "mention" start
                        (cons 'blog
                              `((uuid . ,(funcall tumbel-org-blog-uuid-function
                                                  name))
                                (name . ,name)
                                (url . ,(format "https://%s.tumblr.com/"
                                                name)))))))
              ((member type '("http" "https" "mailto"))
               (let ((url (org-element-property :raw-link link)))
                 (if contents (walk contents) (emit url))
                 (range "link" start (cons 'url url))))
              (contents (walk contents))
              (t (emit (org-element-property :raw-link link))))))
         (convert (object)
           (pcase (org-element-type object)
             ('plain-text (emit (tumbel-org--unfill object)))
             ('bold (let ((start (length text)))
                      (walk (org-element-contents object))
                      (range "bold" start)))
             ('italic (let ((start (length text)))
                        (walk (org-element-contents object))
                        (range "italic" start)))
             ('strike-through (let ((start (length text)))
                                (walk (org-element-contents object))
                                (range "strikethrough" start)))
             ((or 'code 'verbatim) (emit (org-element-property :value object)))
             ('line-break (emit "\n"))
             ('entity (emit (org-element-property :utf-8 object)))
             ('link (convert-link object))
             (_ (if (org-element-contents object)
                    (walk (org-element-contents object))
                  (emit (org-element-interpret-data object)))))
           ;; Org keeps the spaces after an object in its post-blank
           ;; property rather than in the text that follows.
           (unless (stringp object)
             (emit (make-string (or (org-element-property :post-blank object)
                                    0)
                                ?\s)))))
      (walk objects))
    (let* ((trimmed (string-trim-right text))
           (end (length trimmed)))
      (cons trimmed
            (nreverse
             (seq-keep (lambda (r)
                         (let ((start (alist-get 'start r)))
                           (and (< start end)
                                (progn (setf (alist-get 'end r)
                                             (min end (alist-get 'end r)))
                                       r))))
                       formatting))))))

(defun tumbel-org--significant (objects)
  "Return the OBJECTS of a paragraph that are not mere whitespace."
  (seq-remove (lambda (object)
                (and (stringp object) (string-blank-p object)))
              objects))

(defun tumbel-org--image-link (objects)
  "Return the link when OBJECTS is a lone file: link to an image."
  (let ((only (tumbel-org--significant objects)))
    (when (and (= (length only) 1)
               (eq (org-element-type (car only)) 'link)
               (equal (org-element-property :type (car only)) "file")
               (member (downcase (or (file-name-extension
                                      (org-element-property :path (car only)))
                                     ""))
                       tumbel-org-image-extensions))
      (car only))))

(defun tumbel-org--bare-url (objects)
  "Return the URL when OBJECTS is a lone link shown as itself."
  (let ((only (tumbel-org--significant objects)))
    (when (and (= (length only) 1)
               (eq (org-element-type (car only)) 'link)
               (member (org-element-property :type (car only))
                       '("http" "https"))
               (null (org-element-contents (car only))))
      (org-element-property :raw-link (car only)))))

(defun tumbel-org--convert-paragraph (element depth state)
  "Convert the paragraph ELEMENT at list DEPTH into STATE."
  (let ((objects (org-element-contents element)))
    (cond
     ((tumbel-org--image-link objects)
      (let* ((link (tumbel-org--image-link objects))
             (path (expand-file-name (org-element-property :path link)))
             (id (format "image%d" (tumbel-org--state-count state))))
        (cl-incf (tumbel-org--state-count state))
        (push (cons id path) (tumbel-org--state-files state))
        (tumbel-org--push state
                          `((type . "image")
                            (media . [((type . ,(tumbel-api-content-type path))
                                       (identifier . ,id))])))))
     ((tumbel-org--bare-url objects)
      (tumbel-org--push state `((type . "link")
                                (url . ,(tumbel-org--bare-url objects)))))
     (t
      (let ((inline (tumbel-org--inline objects)))
        (unless (string-empty-p (car inline))
          (tumbel-org--push state
                            (tumbel-org--text-block
                             inline (tumbel-org--state-subtype state)
                             depth))))))))

(defun tumbel-org--item-subtype (item)
  "Return the list item subtype for ITEM, judged by its bullet."
  (if (string-match-p "\\`[0-9]+[.)]" (or (org-element-property :bullet item)
                                          ""))
      "ordered-list-item"
    "unordered-list-item"))

(defun tumbel-org--convert-list (element depth state)
  "Convert the plain list ELEMENT at DEPTH into STATE."
  (dolist (item (org-element-contents element))
    (let ((first t))
      (dolist (child (org-element-contents item))
        (pcase (org-element-type child)
          ('paragraph
           (let ((inline (tumbel-org--inline (org-element-contents child))))
             (unless (string-empty-p (car inline))
               (tumbel-org--push
                state
                (tumbel-org--text-block
                 inline
                 (if first
                     (tumbel-org--item-subtype item)
                   (tumbel-org--state-subtype state))
                 depth))
               (setq first nil))))
          ('plain-list (tumbel-org--convert-list child (1+ depth) state))
          (_ (tumbel-org--convert-element child depth state)))))))

(defun tumbel-org--convert-with-subtype (element subtype depth state)
  "Convert the contents of ELEMENT into STATE as SUBTYPE text at DEPTH."
  (let ((previous (tumbel-org--state-subtype state)))
    (setf (tumbel-org--state-subtype state) subtype)
    (unwind-protect
        (tumbel-org--convert-elements (org-element-contents element)
                                      depth state)
      (setf (tumbel-org--state-subtype state) previous))))

(defun tumbel-org--convert-element (element depth state)
  "Convert ELEMENT at list DEPTH into STATE."
  (pcase (org-element-type element)
    ('headline
     (tumbel-org--push
      state
      (tumbel-org--text-block
       (tumbel-org--inline (org-element-property :title element))
       (if (= (org-element-property :level element) 1) "heading1" "heading2")))
     (tumbel-org--convert-elements (org-element-contents element) depth
                                   state))
    ('section
     (tumbel-org--convert-elements (org-element-contents element) depth
                                   state))
    ('paragraph (tumbel-org--convert-paragraph element depth state))
    ('plain-list (tumbel-org--convert-list element depth state))
    ('quote-block (tumbel-org--convert-with-subtype element "indented" depth
                                                    state))
    ('verse-block
     (dolist (line (split-string
                    (buffer-substring-no-properties
                     (org-element-property :contents-begin element)
                     (org-element-property :contents-end element))
                    "\n" t "[ \t]+"))
       (tumbel-org--push state (tumbel-org--text-block (cons line nil)
                                                       "chat"))))
    ((or 'src-block 'example-block)
     (tumbel-org--push
      state
      (tumbel-org--text-block
       (cons (string-trim-right (org-element-property :value element)) nil)
       "chat")))
    ('keyword
     (when (equal (org-element-property :key element) "TUMBLR-BLOCK")
       (let ((index (string-to-number (org-element-property :value element)))
             (blocks (tumbel-org--state-passthrough state)))
         (when (and blocks (< index (length blocks)))
           (tumbel-org--push state (seq-elt blocks index))))))
    ((or 'horizontal-rule 'property-drawer 'drawer 'comment 'comment-block
         'planning)
     nil)
    (_
     (let ((text (string-trim (org-element-interpret-data element))))
       (unless (string-empty-p text)
         (tumbel-org--push state (tumbel-org--text-block (cons text nil))))))))

(defun tumbel-org--convert-elements (elements depth state)
  "Convert ELEMENTS at list DEPTH into STATE."
  (dolist (element elements)
    (tumbel-org--convert-element element depth state)))

(defun tumbel-org-to-npf (string &optional passthrough)
  "Convert STRING, Org text, into NPF content.
Return (BLOCKS . FILES): BLOCKS is a vector of NPF blocks and FILES
an alist of media identifiers to the image files they stand for.
#+tumblr-block: N lines become the Nth block of PASSTHROUGH, a
sequence of blocks parsed in re-encodable form."
  (let ((state (tumbel-org--state-create :passthrough passthrough)))
    (with-temp-buffer
      (insert string)
      (let ((org-inhibit-startup t))
        (org-mode))
      (tumbel-org--convert-elements
       (org-element-contents (org-element-parse-buffer)) 0 state))
    (cons (vconcat (nreverse (tumbel-org--state-blocks state)))
          (nreverse (tumbel-org--state-files state)))))

;;;; NPF to Org

(defun tumbel-org--markup (text formatting)
  "Return TEXT with the NPF FORMATTING ranges rendered as Org markup.
Ranges overlapping an earlier one are dropped, as are kinds Org
cannot express."
  (let ((pieces nil)
        (pos 0))
    (dolist (range (seq-sort-by (lambda (r) (alist-get 'start r)) #'<
                                (seq-into formatting 'list)))
      (let* ((start (alist-get 'start range))
             (end (min (length text) (alist-get 'end range)))
             (part (and (< start end) (substring text start end)))
             (marked (and part
                          (pcase (alist-get 'type range)
                            ("bold" (format "*%s*" part))
                            ("italic" (format "/%s/" part))
                            ("strikethrough" (format "+%s+" part))
                            ("link" (format "[[%s][%s]]"
                                            (alist-get 'url range) part))
                            ("mention"
                             (format "[[tumblr:%s][%s]]"
                                     (alist-get 'name (alist-get 'blog range))
                                     part))))))
        (when (and marked (>= start pos))
          (push (substring text pos start) pieces)
          (push marked pieces)
          (setq pos end))))
    (push (substring text pos) pieces)
    (apply #'concat (nreverse pieces))))

(defun tumbel-org--block-to-org (block index)
  "Return the Org text for BLOCK, the INDEXth block of its post."
  (if (equal (alist-get 'type block) "text")
      (let* ((text (tumbel-org--markup (or (alist-get 'text block) "")
                                       (alist-get 'formatting block)))
             (indent (make-string (* 2 (or (alist-get 'indent_level block) 0))
                                  ?\s)))
        (pcase (alist-get 'subtype block)
          ("heading1" (concat "* " text))
          ("heading2" (concat "** " text))
          ((or "quote" "indented")
           (format "#+begin_quote\n%s\n#+end_quote" text))
          ("chat" (format "#+begin_verse\n%s\n#+end_verse" text))
          ("ordered-list-item" (concat indent "1. " text))
          ("unordered-list-item" (concat indent "- " text))
          (_ text)))
    (format "#+tumblr-block: %d" index)))

(defun tumbel-org--list-item-p (block)
  "Return non-nil when BLOCK is a list item."
  (member (alist-get 'subtype block)
          '("ordered-list-item" "unordered-list-item")))

(defun tumbel-org-from-npf (blocks)
  "Return Org text editing BLOCKS, a sequence of NPF blocks.
Text blocks become Org markup; the others become #+tumblr-block: N
lines that `tumbel-org-to-npf' resolves against BLOCKS."
  (let ((pieces nil)
        (previous nil)
        (index 0))
    (seq-doseq (block blocks)
      (push (concat (if (and previous
                             (tumbel-org--list-item-p previous)
                             (tumbel-org--list-item-p block))
                        ""
                      (if previous "\n" ""))
                    (tumbel-org--block-to-org block index))
            pieces)
      (setq previous block
            index (1+ index)))
    (mapconcat #'identity (nreverse pieces) "\n")))

(provide 'tumbel-org)
;;; tumbel-org.el ends here
