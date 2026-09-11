;;; tumblr-npf-test.el --- Tests for tumblr-npf.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests rendering the fixture posts and inspecting the text and
;; its properties.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-npf)
(require 'tumblr-test-support)

(defconst tumblr-npf-test-now 1757635200
  "2025-09-12 00:00:00 UTC, one day after the newest fixture post.")

(defun tumblr-npf-test-render (post &rest keys)
  "Render POST with KEYS and return the propertized buffer text."
  (with-temp-buffer
    (apply #'tumblr-npf-insert-post post :now tumblr-npf-test-now keys)
    (buffer-string)))

(defun tumblr-npf-test-pos (string text)
  "Return the position of TEXT in STRING, failing when absent."
  (or (string-match-p (regexp-quote text) string)
      (ert-fail (format "%S not found in rendering" text))))

(defun tumblr-npf-test-prop (string text prop)
  "Return the property PROP at the start of TEXT in STRING."
  (get-text-property (tumblr-npf-test-pos string text) prop string))

(defun tumblr-npf-test-face-p (string text face)
  "Return non-nil when TEXT in STRING has FACE among its faces."
  (let ((faces (tumblr-npf-test-prop string text 'face)))
    (or (equal faces face)
        (and (listp faces) (member face faces) t))))

(defun tumblr-npf-test-button-type (string text)
  "Return the button type of TEXT in STRING, or nil."
  (let ((category (tumblr-npf-test-prop string text 'category)))
    (and category
         (intern (string-remove-suffix "-button" (symbol-name category))))))

(ert-deftest tumblr-npf-test-text-post ()
  "Text blocks get subtype faces, formatting ranges and prefixes."
  (let ((out (tumblr-npf-test-render (tumblr-test-post "1001"))))
    (should (tumblr-npf-test-face-p out "Heading One" 'tumblr-heading1))
    (should (tumblr-npf-test-face-p out "Heading Two" 'tumblr-heading2))
    (should (tumblr-npf-test-face-p out "Bold" 'bold))
    (should (tumblr-npf-test-face-p out "italic" 'italic))
    (should (tumblr-npf-test-face-p out "A quotable" 'tumblr-quote))
    (should (tumblr-npf-test-face-p out "Quirky!" 'tumblr-quirky))
    (should (tumblr-npf-test-face-p out "Alice" 'tumblr-chat))
    (should (tumblr-npf-test-face-p out "small." 'tumblr-small))
    (should (tumblr-npf-test-face-p out "small." '(:foreground "#00cf35")))
    (should (tumblr-npf-test-face-p out "Struck" '(:strike-through t)))
    (should-not (tumblr-npf-test-face-p out "through." '(:strike-through t)))
    (should (eq (tumblr-npf-test-button-type out "link,") 'tumblr-link))
    (should (equal (tumblr-npf-test-prop out "link," 'tumblr-url)
                   "https://example.com/"))
    (should (eq (tumblr-npf-test-button-type out "@mention") 'tumblr-mention))
    (should (equal (tumblr-npf-test-prop out "@mention" 'tumblr-blog-name)
                   "friend"))
    (should (equal (tumblr-npf-test-prop out "First bullet" 'line-prefix) "• "))
    (should (equal (tumblr-npf-test-prop out "Nested bullet" 'line-prefix)
                   "  • "))
    (should (equal (tumblr-npf-test-prop out "Step one" 'line-prefix) "1. "))
    (should (equal (tumblr-npf-test-prop out "Step two" 'line-prefix) "2. "))
    (should (equal (tumblr-npf-test-prop out "An indented" 'line-prefix) "│ "))
    (should (string-match-p "Alice: hi\nBob: hello\n" out))
    (should (string-match-p "First bullet\nSecond bullet\nNested bullet\n\nStep"
                            out))))

(ert-deftest tumblr-npf-test-code-point-offsets ()
  "Formatting offsets count code points, so emoji do not shift them."
  (let ((out (tumblr-npf-test-render (tumblr-test-post "1001"))))
    (should (tumblr-npf-test-face-p out "world" 'bold))
    (should-not (tumblr-npf-test-face-p out "Hello 🌳" 'bold))))

(ert-deftest tumblr-npf-test-header-tags-footer ()
  "The header names the blog and date; tags and notes close the post."
  (let ((out (tumblr-npf-test-render (tumblr-test-post "1001"))))
    (should (string-prefix-p "example · 1d\n" out))
    (should (eq (tumblr-npf-test-button-type out "example") 'tumblr-blog))
    (should (equal (tumblr-npf-test-prop out "example" 'tumblr-blog-name)
                   "example"))
    (should (string-match-p "\n#emacs #lisp code\n3 notes\n\\'" out))
    (should (eq (tumblr-npf-test-button-type out "#lisp code") 'tumblr-tag))
    (should (equal (tumblr-npf-test-prop out "#lisp code" 'tumblr-tag)
                   "lisp code"))))

(ert-deftest tumblr-npf-test-image-post ()
  "Images become placeholders carrying the picked URL, with captions."
  (let ((out (tumblr-npf-test-render (tumblr-test-post "1002"))))
    (should (string-match-p "\\[image: A cat\\]\n\nCaption text.\n" out))
    (should (equal (tumblr-npf-test-prop out "[image: A cat]" 'tumblr-media-url)
                   "https://64.media.tumblr.com/k1/s640x960/a.jpg"))
    (should (string-match-p "\n#cats\n42 notes · liked\n\\'" out))))

(ert-deftest tumblr-npf-test-reblog-trail ()
  "Trail items are attributed and truncated content ends in a button."
  (let ((post (tumblr-test-post "1003")))
    (let ((out (tumblr-npf-test-render post)))
      (should (string-prefix-p "example ↻ middle-blog · " out))
      (should (string-match-p
               (concat "root-blog:\nRoot post text.\n\nSecond paragraph.\n"
                       "\\[Keep reading\\]\n\nmiddle-blog:\nMiddle comment.\n"
                       "\nexample:\nMy comment on this.\n\n1234 notes\n\\'")
               out))
      (should-not (string-match-p "Third\\." out))
      (should (eq (tumblr-npf-test-button-type out "[Keep reading]")
                  'tumblr-toggle))
      (should (eq (tumblr-npf-test-button-type out "root-blog") 'tumblr-blog))
      (should (eq (tumblr-npf-test-button-type out "middle-blog")
                  'tumblr-blog)))
    (let ((out (tumblr-npf-test-render post :expanded t)))
      (should (string-match-p "Third\\.\n\nFourth\\.\n\nmiddle-blog:" out))
      (should-not (string-match-p "Keep reading" out)))))

(ert-deftest tumblr-npf-test-ask ()
  "The asked blocks follow the asker and precede the answer."
  (let ((out (tumblr-npf-test-render (tumblr-test-post "1004"))))
    (should (string-match-p
             "asker asked:\nWhat is your favorite editor\\?\n\nEmacs, obviously\\.\n"
             out))
    (should (tumblr-npf-test-face-p out "What is" 'tumblr-ask))
    (should-not (tumblr-npf-test-face-p out "Emacs, obviously" 'tumblr-ask))
    (should (eq (tumblr-npf-test-button-type out "asker") 'tumblr-blog))))

(ert-deftest tumblr-npf-test-media-blocks ()
  "Link, video, audio, poll, paywall and unknown blocks all render."
  (let ((out (tumblr-npf-test-render (tumblr-test-post "1005"))))
    (should (eq (tumblr-npf-test-button-type out "Example Article")
                'tumblr-link))
    (should (equal (tumblr-npf-test-prop out "Example Article" 'tumblr-url)
                   "https://news.example.com/article"))
    (should (string-match-p "Example News · An article about things\\." out))
    (should (string-match-p "▶ video \\[youtube\\]" out))
    (should (equal (tumblr-npf-test-prop out "▶ video" 'tumblr-url)
                   "https://www.youtube.com/watch?v=abc"))
    (should (string-match-p "♫ Song — Band \\[spotify\\]" out))
    (should (string-match-p "Poll: Tea or coffee\\?\n  ○ Tea\n  ○ Coffee\n" out))
    (should (string-match-p "\\[Tumblr\\+ Support example — Subscribe for more\\.\\]"
                            out))
    (should (string-match-p "\\[future-block block\\]" out))
    (should (string-match-p "\n0 notes\n\\'" out))))

(ert-deftest tumblr-npf-test-broken-trail ()
  "Content from a deactivated blog is attributed as such."
  (let ((out (tumblr-npf-test-render (tumblr-test-post "1006"))))
    (should (string-match-p
             "\\`example ↻ · [0-9]+d\ngone-blog (deactivated):\nContent from"
             out))))

(ert-deftest tumblr-npf-test-legacy-fallback ()
  "A post without blocks shows its summary."
  (let ((out (tumblr-npf-test-render '((blog_name . "old")
                                       (id_string . "1")
                                       (summary . "Old style post")))))
    (should (string-match-p "\\`old\nOld style post\n\\'" out))))

(ert-deftest tumblr-npf-test-relative-time ()
  "Relative times shorten with age and end as dates."
  (let ((now 1757635200))
    (should (equal (tumblr-npf-relative-time (- now 30) now) "now"))
    (should (equal (tumblr-npf-relative-time (- now 300) now) "5m"))
    (should (equal (tumblr-npf-relative-time (- now 10800) now) "3h"))
    (should (equal (tumblr-npf-relative-time (- now 172800) now) "2d"))
    (should (string-match-p "\\`[A-Z][a-z]+ [0-9]+\\'"
                            (tumblr-npf-relative-time (- now 864000) now)))
    (should (string-match-p "\\`[A-Z][a-z]+ [0-9]+, 20[0-9][0-9]\\'"
                            (tumblr-npf-relative-time (- now 40000000) now)))))

(ert-deftest tumblr-npf-test-button-actions ()
  "Buttons dispatch through the function variables with their data."
  (with-temp-buffer
    (tumblr-npf-insert-post (tumblr-test-post "1003"))
    (let (blog toggled url tag)
      (let ((tumblr-npf-open-blog-function (lambda (name) (setq blog name)))
            (tumblr-npf-toggle-function (lambda (button) (setq toggled button)))
            (tumblr-npf-open-url-function (lambda (u) (setq url u)))
            (tumblr-npf-open-tag-function (lambda (x) (setq tag x))))
        (goto-char (point-min))
        (search-forward "root-blog")
        (push-button (match-beginning 0))
        (should (equal blog "root-blog"))
        (search-forward "[Keep reading]")
        (push-button (match-beginning 0))
        (should toggled)
        (erase-buffer)
        (tumblr-npf-insert-post (tumblr-test-post "1001"))
        (goto-char (point-min))
        (search-forward "link,")
        (push-button (match-beginning 0))
        (should (equal url "https://example.com/"))
        (search-forward "#emacs")
        (push-button (match-beginning 0))
        (should (equal tag "emacs"))))))

(ert-deftest tumblr-npf-test-accessors ()
  "Post accessors fall back sensibly."
  (should (equal (tumblr-npf-post-id '((id . 42))) "42"))
  (should (equal (tumblr-npf-post-id '((id_string . "43") (id . 43))) "43"))
  (should (null (tumblr-npf-post-id nil)))
  (should (equal (tumblr-npf-post-blog-name '((blog . ((name . "b"))))) "b"))
  (should (equal (tumblr-npf-post-blog-name '((blog_name . "a")
                                              (blog . ((name . "b")))))
                 "a")))

(provide 'tumblr-npf-test)
;;; tumblr-npf-test.el ends here
