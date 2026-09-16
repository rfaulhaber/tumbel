;;; tumbel-media-test.el --- Tests for tumbel-media.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for rendition selection, placeholders, the fetch queue
;; and the caches, with image creation stubbed out because the test
;; Emacs has no image support.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumbel-media)
(require 'tumbel-test-support)

(defconst tumbel-media-test-entries
  '(((width . 2048) (url . "big"))
    ((width . 640) (url . "mid"))
    ((width . 250) (url . "small")))
  "Media entries of one image, largest first as the API sends them.")

(defvar tumbel-media-test-created nil
  "Arguments of every stubbed `create-image' call, most recent first.")

(defmacro tumbel-media-test-with-display (&rest body)
  "Run BODY with images enabled, a private cache and stubbed image creation."
  (declare (indent 0))
  `(let* ((tumbel-media-test-dir (make-temp-file "tumbel-media-test" t))
          (tumbel-media-cache-directory (expand-file-name "cache/"
                                                          tumbel-media-test-dir))
          (tumbel-media--images (make-hash-table :test #'equal))
          (tumbel-media--fetching nil)
          (tumbel-media--queue nil)
          (tumbel-media-test-created nil)
          (tumbel-display-images t))
     (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
               ((symbol-function 'create-image)
                (lambda (source &rest args)
                  (push (cons source args) tumbel-media-test-created)
                  (list 'image :source source))))
       (unwind-protect
           (progn ,@body)
         (delete-directory tumbel-media-test-dir t)))))

(defvar tumbel-media-test-dir nil
  "Temporary directory of the running test.")

(ert-deftest tumbel-media-test-pick ()
  "The narrowest rendition at least as wide as the target is chosen."
  (should (equal (alist-get 'url (tumbel-media-pick tumbel-media-test-entries
                                                    540))
                 "mid"))
  (should (equal (alist-get 'url (tumbel-media-pick tumbel-media-test-entries
                                                    640))
                 "mid"))
  (should (equal (alist-get 'url (tumbel-media-pick tumbel-media-test-entries
                                                    100))
                 "small"))
  (should (equal (alist-get 'url (tumbel-media-pick tumbel-media-test-entries
                                                    4000))
                 "big"))
  (let ((tumbel-image-max-width 300))
    (should (equal (alist-get 'url (tumbel-media-pick tumbel-media-test-entries))
                   "mid")))
  (should (null (tumbel-media-pick nil))))

(ert-deftest tumbel-media-test-placeholder-without-display ()
  "Without image support the placeholder is plain and nothing is fetched."
  (tumbel-test-with-backend nil
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
      (let ((string (tumbel-media-image-string "https://x/a.jpg" "A cat")))
        (should (equal string "[image: A cat]"))
        (should (equal (get-text-property 0 'tumbel-media-url string)
                       "https://x/a.jpg"))
        (should-not (get-text-property 0 'display string))
        (should (null tumbel-test-calls))))
    (should (equal (tumbel-media-image-string "https://x/a.jpg")
                   "[image: image]"))))

(ert-deftest tumbel-media-test-fetch-and-swap ()
  "A fetched image replaces the placeholders already in buffers."
  (tumbel-media-test-with-display
    (tumbel-test-with-backend '(("a.jpg" 200 "PNGDATA"))
      (let ((tumbel-test-defer t))
        (with-temp-buffer
          (insert (tumbel-media-image-string "https://x/a.jpg" "A cat") "\n")
          (insert (tumbel-media-image-string "https://x/a.jpg" "Again") "\n")
          (should (equal (length tumbel-test-calls) 1))
          (should (eq (tumbel-test-call-key (tumbel-test-call 0) :binary) t))
          (should-not (get-text-property 1 'display))
          (tumbel-test-deliver)
          (should (equal (get-text-property 1 'display)
                         '(image :source "PNGDATA")))
          (goto-char (point-min))
          (search-forward "[image: Again]")
          (should (equal (get-text-property (match-beginning 0) 'display)
                         '(image :source "PNGDATA")))
          (should (equal (buffer-substring-no-properties 1 15) "[image: A cat]"))
          (should (file-exists-p (tumbel-media--cache-file "https://x/a.jpg")))
          (should (equal (plist-get (cdr (car tumbel-media-test-created))
                                    :max-width)
                         tumbel-image-max-width))
          (should (null tumbel-media--fetching))))
      ;; Now cached: rendered with the image at once, no new request.
      (let ((string (tumbel-media-image-string "https://x/a.jpg")))
        (should (equal (get-text-property 0 'display string)
                       '(image :source "PNGDATA")))
        (should (equal (length tumbel-test-calls) 1))))))

(ert-deftest tumbel-media-test-disk-cache ()
  "An image on disk is loaded without a request."
  (tumbel-media-test-with-display
    (tumbel-test-with-backend '(("a.jpg" 200 "PNGDATA"))
      (tumbel-media-image-string "https://x/a.jpg")
      (clrhash tumbel-media--images)
      (let ((string (tumbel-media-image-string "https://x/a.jpg")))
        (should (equal (get-text-property 0 'display string)
                       (list 'image :source
                             (tumbel-media--cache-file "https://x/a.jpg"))))
        (should (equal (length tumbel-test-calls) 1))))))

(ert-deftest tumbel-media-test-failed-fetch ()
  "A failed fetch leaves the placeholder and is not repeated."
  (tumbel-media-test-with-display
    (tumbel-test-with-backend '(("a.jpg" curl nil))
      (cl-letf (((symbol-function 'message) #'ignore))
        (tumbel-media-image-string "https://x/a.jpg")
        (should (eq (gethash "https://x/a.jpg" tumbel-media--images) 'failed))
        (let ((string (tumbel-media-image-string "https://x/a.jpg")))
          (should-not (get-text-property 0 'display string)))
        (should (equal (length tumbel-test-calls) 1))))))

(ert-deftest tumbel-media-test-queue-limit ()
  "Only a few fetches run at once; the rest start as earlier ones finish."
  (tumbel-media-test-with-display
    (tumbel-test-with-backend '(("." 200 "DATA"))
      (let ((tumbel-test-defer t)
            (tumbel-media-max-fetches 2))
        (dotimes (i 5)
          (tumbel-media-image-string (format "https://x/%d.jpg" i)))
        (should (equal (length tumbel-test-calls) 2))
        (should (equal (length tumbel-media--fetching) 2))
        (should (equal (length tumbel-media--queue) 3))
        ;; Delivering drains the queue: each arrival starts the next.
        (tumbel-test-deliver)
        (should (equal (length tumbel-test-calls) 5))
        (should (null tumbel-media--queue))
        (should (null tumbel-media--fetching))
        (should (equal (hash-table-count tumbel-media--images) 5))))))

(ert-deftest tumbel-media-test-buffer-local-toggle ()
  "A buffer with images off keeps its placeholders plain."
  (tumbel-media-test-with-display
    (tumbel-test-with-backend '(("a.jpg" 200 "DATA"))
      (let ((tumbel-test-defer t))
        (with-temp-buffer
          (insert (tumbel-media-image-string "https://x/a.jpg"))
          (setq-local tumbel-display-images nil)
          (tumbel-test-deliver)
          (should-not (get-text-property 1 'display))
          (should-not (get-text-property
                       0 'display (tumbel-media-image-string "https://x/a.jpg"))))))))

(ert-deftest tumbel-media-test-clear-cache ()
  "Clearing the cache removes images from memory and disk."
  (tumbel-media-test-with-display
    (tumbel-test-with-backend '(("a.jpg" 200 "DATA"))
      (cl-letf (((symbol-function 'message) #'ignore))
        (tumbel-media-image-string "https://x/a.jpg")
        (should (file-directory-p tumbel-media-cache-directory))
        (tumbel-media-clear-cache)
        (should (zerop (hash-table-count tumbel-media--images)))
        (should-not (file-directory-p tumbel-media-cache-directory))))))

(provide 'tumbel-media-test)
;;; tumbel-media-test.el ends here
