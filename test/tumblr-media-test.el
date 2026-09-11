;;; tumblr-media-test.el --- Tests for tumblr-media.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for rendition selection, placeholders, the fetch queue
;; and the caches, with image creation stubbed out because the test
;; Emacs has no image support.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-media)
(require 'tumblr-test-support)

(defconst tumblr-media-test-entries
  '(((width . 2048) (url . "big"))
    ((width . 640) (url . "mid"))
    ((width . 250) (url . "small")))
  "Media entries of one image, largest first as the API sends them.")

(defvar tumblr-media-test-created nil
  "Arguments of every stubbed `create-image' call, most recent first.")

(defmacro tumblr-media-test-with-display (&rest body)
  "Run BODY with images enabled, a private cache and stubbed image creation."
  (declare (indent 0))
  `(let* ((tumblr-media-test-dir (make-temp-file "tumblr-media-test" t))
          (tumblr-media-cache-directory (expand-file-name "cache/"
                                                          tumblr-media-test-dir))
          (tumblr-media--images (make-hash-table :test #'equal))
          (tumblr-media--fetching nil)
          (tumblr-media--queue nil)
          (tumblr-media-test-created nil)
          (tumblr-display-images t))
     (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
               ((symbol-function 'create-image)
                (lambda (source &rest args)
                  (push (cons source args) tumblr-media-test-created)
                  (list 'image :source source))))
       (unwind-protect
           (progn ,@body)
         (delete-directory tumblr-media-test-dir t)))))

(defvar tumblr-media-test-dir nil
  "Temporary directory of the running test.")

(ert-deftest tumblr-media-test-pick ()
  "The narrowest rendition at least as wide as the target is chosen."
  (should (equal (alist-get 'url (tumblr-media-pick tumblr-media-test-entries
                                                    540))
                 "mid"))
  (should (equal (alist-get 'url (tumblr-media-pick tumblr-media-test-entries
                                                    640))
                 "mid"))
  (should (equal (alist-get 'url (tumblr-media-pick tumblr-media-test-entries
                                                    100))
                 "small"))
  (should (equal (alist-get 'url (tumblr-media-pick tumblr-media-test-entries
                                                    4000))
                 "big"))
  (let ((tumblr-image-max-width 300))
    (should (equal (alist-get 'url (tumblr-media-pick tumblr-media-test-entries))
                   "mid")))
  (should (null (tumblr-media-pick nil))))

(ert-deftest tumblr-media-test-placeholder-without-display ()
  "Without image support the placeholder is plain and nothing is fetched."
  (tumblr-test-with-backend nil
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
      (let ((string (tumblr-media-image-string "https://x/a.jpg" "A cat")))
        (should (equal string "[image: A cat]"))
        (should (equal (get-text-property 0 'tumblr-media-url string)
                       "https://x/a.jpg"))
        (should-not (get-text-property 0 'display string))
        (should (null tumblr-test-calls))))
    (should (equal (tumblr-media-image-string "https://x/a.jpg")
                   "[image: image]"))))

(ert-deftest tumblr-media-test-fetch-and-swap ()
  "A fetched image replaces the placeholders already in buffers."
  (tumblr-media-test-with-display
    (tumblr-test-with-backend '(("a.jpg" 200 "PNGDATA"))
      (let ((tumblr-test-defer t))
        (with-temp-buffer
          (insert (tumblr-media-image-string "https://x/a.jpg" "A cat") "\n")
          (insert (tumblr-media-image-string "https://x/a.jpg" "Again") "\n")
          (should (equal (length tumblr-test-calls) 1))
          (should (eq (tumblr-test-call-key (tumblr-test-call 0) :binary) t))
          (should-not (get-text-property 1 'display))
          (tumblr-test-deliver)
          (should (equal (get-text-property 1 'display)
                         '(image :source "PNGDATA")))
          (goto-char (point-min))
          (search-forward "[image: Again]")
          (should (equal (get-text-property (match-beginning 0) 'display)
                         '(image :source "PNGDATA")))
          (should (equal (buffer-substring-no-properties 1 15) "[image: A cat]"))
          (should (file-exists-p (tumblr-media--cache-file "https://x/a.jpg")))
          (should (equal (plist-get (cdr (car tumblr-media-test-created))
                                    :max-width)
                         tumblr-image-max-width))
          (should (null tumblr-media--fetching))))
      ;; Now cached: rendered with the image at once, no new request.
      (let ((string (tumblr-media-image-string "https://x/a.jpg")))
        (should (equal (get-text-property 0 'display string)
                       '(image :source "PNGDATA")))
        (should (equal (length tumblr-test-calls) 1))))))

(ert-deftest tumblr-media-test-disk-cache ()
  "An image on disk is loaded without a request."
  (tumblr-media-test-with-display
    (tumblr-test-with-backend '(("a.jpg" 200 "PNGDATA"))
      (tumblr-media-image-string "https://x/a.jpg")
      (clrhash tumblr-media--images)
      (let ((string (tumblr-media-image-string "https://x/a.jpg")))
        (should (equal (get-text-property 0 'display string)
                       (list 'image :source
                             (tumblr-media--cache-file "https://x/a.jpg"))))
        (should (equal (length tumblr-test-calls) 1))))))

(ert-deftest tumblr-media-test-failed-fetch ()
  "A failed fetch leaves the placeholder and is not repeated."
  (tumblr-media-test-with-display
    (tumblr-test-with-backend '(("a.jpg" curl nil))
      (cl-letf (((symbol-function 'message) #'ignore))
        (tumblr-media-image-string "https://x/a.jpg")
        (should (eq (gethash "https://x/a.jpg" tumblr-media--images) 'failed))
        (let ((string (tumblr-media-image-string "https://x/a.jpg")))
          (should-not (get-text-property 0 'display string)))
        (should (equal (length tumblr-test-calls) 1))))))

(ert-deftest tumblr-media-test-queue-limit ()
  "Only a few fetches run at once; the rest start as earlier ones finish."
  (tumblr-media-test-with-display
    (tumblr-test-with-backend '(("." 200 "DATA"))
      (let ((tumblr-test-defer t)
            (tumblr-media-max-fetches 2))
        (dotimes (i 5)
          (tumblr-media-image-string (format "https://x/%d.jpg" i)))
        (should (equal (length tumblr-test-calls) 2))
        (should (equal (length tumblr-media--fetching) 2))
        (should (equal (length tumblr-media--queue) 3))
        ;; Delivering drains the queue: each arrival starts the next.
        (tumblr-test-deliver)
        (should (equal (length tumblr-test-calls) 5))
        (should (null tumblr-media--queue))
        (should (null tumblr-media--fetching))
        (should (equal (hash-table-count tumblr-media--images) 5))))))

(ert-deftest tumblr-media-test-buffer-local-toggle ()
  "A buffer with images off keeps its placeholders plain."
  (tumblr-media-test-with-display
    (tumblr-test-with-backend '(("a.jpg" 200 "DATA"))
      (let ((tumblr-test-defer t))
        (with-temp-buffer
          (insert (tumblr-media-image-string "https://x/a.jpg"))
          (setq-local tumblr-display-images nil)
          (tumblr-test-deliver)
          (should-not (get-text-property 1 'display))
          (should-not (get-text-property
                       0 'display (tumblr-media-image-string "https://x/a.jpg"))))))))

(ert-deftest tumblr-media-test-clear-cache ()
  "Clearing the cache removes images from memory and disk."
  (tumblr-media-test-with-display
    (tumblr-test-with-backend '(("a.jpg" 200 "DATA"))
      (cl-letf (((symbol-function 'message) #'ignore))
        (tumblr-media-image-string "https://x/a.jpg")
        (should (file-directory-p tumblr-media-cache-directory))
        (tumblr-media-clear-cache)
        (should (zerop (hash-table-count tumblr-media--images)))
        (should-not (file-directory-p tumblr-media-cache-directory))))))

(provide 'tumblr-media-test)
;;; tumblr-media-test.el ends here
