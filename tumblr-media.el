;;; tumblr-media.el --- Images for tumblr.el  -*- lexical-binding: t; -*-

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
(require 'tumblr-http)

(defcustom tumblr-display-images t
  "Whether to show images inline when the display can.
Buffer-local values are honoured, so a single buffer can toggle."
  :type 'boolean
  :group 'tumblr)

(defcustom tumblr-image-max-width 540
  "Width in pixels that inline images are fitted to."
  :type 'integer
  :group 'tumblr)

(defcustom tumblr-media-cache-directory (locate-user-emacs-file "tumblr/media/")
  "Directory where fetched images are kept between sessions."
  :type 'directory
  :group 'tumblr)

(defcustom tumblr-media-max-fetches 4
  "Number of images fetched at the same time."
  :type 'integer
  :group 'tumblr)

(defface tumblr-placeholder '((t :inherit shadow))
  "Face of the text standing in for an image."
  :group 'tumblr)

(defvar tumblr-media--images (make-hash-table :test #'equal)
  "Images by URL: an image descriptor, or `failed'.")

(defvar tumblr-media--fetching nil
  "URLs being fetched right now.")

(defvar tumblr-media--queue nil
  "URLs waiting to be fetched, oldest first.")

;;;; Choosing and describing images

(defun tumblr-media-pick (media &optional target-width)
  "Return the entry of MEDIA best suited to TARGET-WIDTH pixels.
MEDIA is the list of media objects of an image block.  The narrowest
entry at least TARGET-WIDTH wide wins; when none is wide enough, the
widest one does.  TARGET-WIDTH defaults to `tumblr-image-max-width'."
  (let* ((target (or target-width tumblr-image-max-width))
         (width (lambda (entry) (or (alist-get 'width entry) 0)))
         (sorted (sort (copy-sequence media)
                       (lambda (a b) (< (funcall width a) (funcall width b))))))
    (or (seq-find (lambda (entry) (>= (funcall width entry) target)) sorted)
        (car (last sorted)))))

(defun tumblr-media-display-p ()
  "Return non-nil when images should be shown in the current buffer."
  (and tumblr-display-images (display-images-p)))

(defun tumblr-media-image-string (url &optional alt)
  "Return the text standing in for the image at URL, described by ALT.
The string carries the `tumblr-media-url' property.  When images are
shown and the image is cached, it is displayed in place of the text
at once; otherwise the fetch is queued and the text is replaced when
the image arrives."
  (let ((string (propertize (format "[image: %s]" (or alt "image"))
                            'tumblr-media-url url
                            'face 'tumblr-placeholder)))
    (when (tumblr-media-display-p)
      (let ((image (tumblr-media--cached url)))
        (cond ((and image (not (eq image 'failed)))
               (put-text-property 0 (length string) 'display image string))
              ((null image)
               (tumblr-media--enqueue url)))))
    string))

;;;; Caches

(defun tumblr-media--cache-file (url)
  "Return the file caching the image at URL."
  (expand-file-name (sha1 url) tumblr-media-cache-directory))

(defun tumblr-media--make-image (source data-p)
  "Create the image from SOURCE, data when DATA-P and a file otherwise.
Return `failed' when Emacs cannot display it."
  (condition-case nil
      (create-image source nil data-p :max-width tumblr-image-max-width)
    (error 'failed)))

(defun tumblr-media--cached (url)
  "Return the cached image for URL, `failed', or nil when unknown.
An image only on disk is loaded into memory."
  (or (gethash url tumblr-media--images)
      (let ((file (tumblr-media--cache-file url)))
        (when (file-readable-p file)
          (puthash url (tumblr-media--make-image file nil)
                   tumblr-media--images)))))

(defun tumblr-media--store (url data)
  "Cache the image DATA fetched from URL in memory and on disk."
  (let ((file (tumblr-media--cache-file url)))
    (with-file-modes #o700
      (make-directory tumblr-media-cache-directory t))
    (let ((coding-system-for-write 'no-conversion))
      (write-region data nil file nil 'silent))
    (puthash url (tumblr-media--make-image data t) tumblr-media--images)))

(defun tumblr-media-clear-cache ()
  "Forget every cached image and delete the cache directory."
  (interactive)
  (clrhash tumblr-media--images)
  (when (file-directory-p tumblr-media-cache-directory)
    (delete-directory tumblr-media-cache-directory t))
  (message "Tumblr image cache cleared"))

;;;; Fetching

(defun tumblr-media--enqueue (url)
  "Queue the image at URL for fetching, unless already on its way."
  (unless (or (member url tumblr-media--fetching)
              (member url tumblr-media--queue))
    (setq tumblr-media--queue (append tumblr-media--queue (list url)))
    (tumblr-media--pump)))

(defun tumblr-media--pump ()
  "Start fetches until `tumblr-media-max-fetches' are in flight."
  (while (and tumblr-media--queue
              (< (length tumblr-media--fetching) tumblr-media-max-fetches))
    (let ((url (pop tumblr-media--queue)))
      (push url tumblr-media--fetching)
      (condition-case err
          (tumblr-http-request 'get url :as 'binary
                               :then (lambda (data)
                                       (tumblr-media--arrived url data))
                               :else (lambda (err)
                                       (tumblr-media--arrived url nil err)))
        (error (tumblr-media--arrived url nil err))))))

(defun tumblr-media--arrived (url data &optional err)
  "Record the outcome of fetching URL: the image DATA, or ERR."
  (setq tumblr-media--fetching (delete url tumblr-media--fetching))
  (if (and data (> (length data) 0))
      (tumblr-media--store url data)
    (puthash url 'failed tumblr-media--images)
    (when err
      (message "tumblr: could not fetch image %s: %s" url
               (tumblr-http-error-string err))))
  (let ((image (gethash url tumblr-media--images)))
    (unless (eq image 'failed)
      (tumblr-media--show url image)))
  (tumblr-media--pump))

(defun tumblr-media--show (url image)
  "Display IMAGE on every placeholder carrying URL in live buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (tumblr-media-display-p) (> (buffer-size) 0))
        (save-excursion
          (goto-char (point-min))
          (let ((inhibit-read-only t)
                match)
            (while (setq match (text-property-search-forward
                                'tumblr-media-url url t))
              (put-text-property (prop-match-beginning match)
                                 (prop-match-end match)
                                 'display image))))))))

(provide 'tumblr-media)
;;; tumblr-media.el ends here
