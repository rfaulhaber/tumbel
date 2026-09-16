;;; tumbel-media.el --- Images for tumbel.el  -*- lexical-binding: t; -*-

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

;; Inline images.  A post is rendered with a placeholder for every
;; image; the placeholder carries the image URL as a text property.
;; Images are fetched in the background (a few at a time), kept in
;; memory and on disk, and shown by putting a `display' property on
;; every placeholder that carries their URL, in every live buffer.
;; Rendering a post whose images are already cached shows them at
;; once.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'text-property-search)
(require 'tumbel-http)

(defcustom tumbel-display-images t
  "Whether to show images inline when the display can.
Buffer-local values are honoured, so a single buffer can toggle."
  :type 'boolean
  :group 'tumbel)

(defcustom tumbel-image-max-width 540
  "Width in pixels that inline images are fitted to."
  :type 'integer
  :group 'tumbel)

(defcustom tumbel-media-cache-directory (locate-user-emacs-file "tumbel/media/")
  "Directory where fetched images are kept between sessions."
  :type 'directory
  :group 'tumbel)

(defcustom tumbel-media-max-fetches 4
  "Number of images fetched at the same time."
  :type 'integer
  :group 'tumbel)

(defface tumbel-placeholder '((t :inherit shadow))
  "Face of the text standing in for an image."
  :group 'tumbel)

(defvar tumbel-media--images (make-hash-table :test #'equal)
  "Images by URL: an image descriptor, or `failed'.")

(defvar tumbel-media--fetching nil
  "URLs being fetched right now.")

(defvar tumbel-media--queue nil
  "URLs waiting to be fetched, oldest first.")

;;;; Choosing and describing images

(defun tumbel-media-pick (media &optional target-width)
  "Return the entry of MEDIA best suited to TARGET-WIDTH pixels.
MEDIA is the list of media objects of an image block.  The narrowest
entry at least TARGET-WIDTH wide wins; when none is wide enough, the
widest one does.  TARGET-WIDTH defaults to `tumbel-image-max-width'."
  (let* ((target (or target-width tumbel-image-max-width))
         (width (lambda (entry) (or (alist-get 'width entry) 0)))
         (sorted (sort (copy-sequence media)
                       (lambda (a b) (< (funcall width a) (funcall width b))))))
    (or (seq-find (lambda (entry) (>= (funcall width entry) target)) sorted)
        (car (last sorted)))))

(defun tumbel-media-display-p ()
  "Return non-nil when images should be shown in the current buffer."
  (and tumbel-display-images (display-images-p)))

(defun tumbel-media-image-string (url &optional alt)
  "Return the text standing in for the image at URL, described by ALT.
The string carries the `tumbel-media-url' property.  When images are
shown and the image is cached, it is displayed in place of the text
at once; otherwise the fetch is queued and the text is replaced when
the image arrives."
  (let ((string (propertize (format "[image: %s]" (or alt "image"))
                            'tumbel-media-url url
                            'face 'tumbel-placeholder)))
    (when (tumbel-media-display-p)
      (let ((image (tumbel-media--cached url)))
        (cond ((and image (not (eq image 'failed)))
               (put-text-property 0 (length string) 'display image string))
              ((null image)
               (tumbel-media--enqueue url)))))
    string))

;;;; Caches

(defun tumbel-media--cache-file (url)
  "Return the file caching the image at URL."
  (expand-file-name (sha1 url) tumbel-media-cache-directory))

(defun tumbel-media--make-image (source data-p)
  "Create the image from SOURCE, data when DATA-P and a file otherwise.
Return `failed' when Emacs cannot display it."
  (condition-case nil
      (create-image source nil data-p :max-width tumbel-image-max-width)
    (error 'failed)))

(defun tumbel-media--cached (url)
  "Return the cached image for URL, `failed', or nil when unknown.
An image only on disk is loaded into memory."
  (or (gethash url tumbel-media--images)
      (let ((file (tumbel-media--cache-file url)))
        (when (file-readable-p file)
          (puthash url (tumbel-media--make-image file nil)
                   tumbel-media--images)))))

(defun tumbel-media--store (url data)
  "Cache the image DATA fetched from URL in memory and on disk."
  (let ((file (tumbel-media--cache-file url)))
    (with-file-modes #o700
      (make-directory tumbel-media-cache-directory t))
    (let ((coding-system-for-write 'no-conversion))
      (write-region data nil file nil 'silent))
    (puthash url (tumbel-media--make-image data t) tumbel-media--images)))

(defun tumbel-media-clear-cache ()
  "Forget every cached image and delete the cache directory."
  (interactive)
  (clrhash tumbel-media--images)
  (when (file-directory-p tumbel-media-cache-directory)
    (delete-directory tumbel-media-cache-directory t))
  (message "Tumblr image cache cleared"))

;;;; Fetching

(defun tumbel-media--enqueue (url)
  "Queue the image at URL for fetching, unless already on its way."
  (unless (or (member url tumbel-media--fetching)
              (member url tumbel-media--queue))
    (setq tumbel-media--queue (append tumbel-media--queue (list url)))
    (tumbel-media--pump)))

(defun tumbel-media--pump ()
  "Start fetches until `tumbel-media-max-fetches' are in flight."
  (while (and tumbel-media--queue
              (< (length tumbel-media--fetching) tumbel-media-max-fetches))
    (let ((url (pop tumbel-media--queue)))
      (push url tumbel-media--fetching)
      (condition-case err
          (tumbel-http-request 'get url :as 'binary
                               :then (lambda (data)
                                       (tumbel-media--arrived url data))
                               :else (lambda (err)
                                       (tumbel-media--arrived url nil err)))
        (error (tumbel-media--arrived url nil err))))))

(defun tumbel-media--arrived (url data &optional err)
  "Record the outcome of fetching URL: the image DATA, or ERR."
  (setq tumbel-media--fetching (delete url tumbel-media--fetching))
  (if (and data (> (length data) 0))
      (tumbel-media--store url data)
    (puthash url 'failed tumbel-media--images)
    (when err
      (message "tumbel: could not fetch image %s: %s" url
               (tumbel-http-error-string err))))
  (let ((image (gethash url tumbel-media--images)))
    (unless (eq image 'failed)
      (tumbel-media--show url image)))
  (tumbel-media--pump))

(defun tumbel-media--show (url image)
  "Display IMAGE on every placeholder carrying URL in live buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (tumbel-media-display-p) (> (buffer-size) 0))
        (save-excursion
          (goto-char (point-min))
          (let ((inhibit-read-only t)
                match)
            (while (setq match (text-property-search-forward
                                'tumbel-media-url url t))
              (put-text-property (prop-match-beginning match)
                                 (prop-match-end match)
                                 'display image))))))))

(provide 'tumbel-media)
;;; tumbel-media.el ends here
