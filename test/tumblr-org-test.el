;;; tumblr-org-test.el --- Tests for tumblr-org.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; Table-driven ERT tests for the Org to NPF conversion and back.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'tumblr-org)
(require 'tumblr-test-support)

(defun tumblr-org-test-blocks (string &optional passthrough)
  "Convert STRING with PASSTHROUGH and return the blocks as a list."
  (let ((tumblr-org-blog-uuid-function (lambda (name) (format "t:%s" name))))
    (append (car (tumblr-org-to-npf string passthrough)) nil)))

(defun tumblr-org-test-text (text &rest fields)
  "Return a text block of TEXT with the extra FIELDS."
  (append `((type . "text") (text . ,text)) fields))

(ert-deftest tumblr-org-test-paragraphs ()
  "Paragraphs become text blocks; filled lines are joined."
  (should (equal (tumblr-org-test-blocks "Hello world")
                 (list (tumblr-org-test-text "Hello world"))))
  (should (equal (tumblr-org-test-blocks "First\nline\n\nSecond\n")
                 (list (tumblr-org-test-text "First line")
                       (tumblr-org-test-text "Second"))))
  (should (equal (tumblr-org-test-blocks "") nil))
  (should (equal (tumblr-org-test-blocks "Keep\\\\\nbreak")
                 (list (tumblr-org-test-text "Keep\nbreak")))))

(ert-deftest tumblr-org-test-headings ()
  "Headlines become heading blocks followed by their content."
  (should (equal (tumblr-org-test-blocks "* Title\nBody\n** Sub\nMore")
                 (list (tumblr-org-test-text "Title" '(subtype . "heading1"))
                       (tumblr-org-test-text "Body")
                       (tumblr-org-test-text "Sub" '(subtype . "heading2"))
                       (tumblr-org-test-text "More")))))

(ert-deftest tumblr-org-test-inline-formatting ()
  "Emphasis becomes formatting ranges over the plain text."
  (should (equal (tumblr-org-test-blocks "*bold* and /italic/ and +struck+")
                 (list (tumblr-org-test-text
                        "bold and italic and struck"
                        '(formatting . [((type . "bold") (start . 0) (end . 4))
                                        ((type . "italic") (start . 9)
                                         (end . 15))
                                        ((type . "strikethrough") (start . 20)
                                         (end . 26))])))))
  (should (equal (tumblr-org-test-blocks "🌳 *tree*")
                 (list (tumblr-org-test-text
                        "🌳 tree"
                        '(formatting . [((type . "bold") (start . 2)
                                         (end . 6))])))))
  (should (equal (tumblr-org-test-blocks "*/both/*")
                 (list (tumblr-org-test-text
                        "both"
                        '(formatting . [((type . "italic") (start . 0) (end . 4))
                                        ((type . "bold") (start . 0)
                                         (end . 4))]))))))

(ert-deftest tumblr-org-test-links-and-mentions ()
  "Links and tumblr: mentions become ranges with their targets."
  (should (equal (tumblr-org-test-blocks "See [[https://example.com/][Example]] now")
                 (list (tumblr-org-test-text
                        "See Example now"
                        '(formatting . [((type . "link") (start . 4) (end . 11)
                                         (url . "https://example.com/"))])))))
  (should (equal (tumblr-org-test-blocks "Go to https://example.com/ today")
                 (list (tumblr-org-test-text
                        "Go to https://example.com/ today"
                        '(formatting . [((type . "link") (start . 6) (end . 26)
                                         (url . "https://example.com/"))])))))
  (should (equal (tumblr-org-test-blocks "Hi [[tumblr:friend]]!")
                 (list (tumblr-org-test-text
                        "Hi @friend!"
                        '(formatting . [((type . "mention") (start . 3) (end . 10)
                                         (blog . ((uuid . "t:friend")
                                                  (name . "friend")
                                                  (url . "https://friend.tumblr.com/"))))])))))
  (should (equal (tumblr-org-test-blocks "[[tumblr:friend][them]]")
                 (list (tumblr-org-test-text
                        "them"
                        '(formatting . [((type . "mention") (start . 0) (end . 4)
                                         (blog . ((uuid . "t:friend")
                                                  (name . "friend")
                                                  (url . "https://friend.tumblr.com/"))))]))))))

(ert-deftest tumblr-org-test-lists ()
  "Lists become list items with their nesting as indent level."
  (should (equal (tumblr-org-test-blocks "- a\n- b\n  - nested\n\n1. x\n2. y")
                 (list (tumblr-org-test-text "a" '(subtype . "unordered-list-item"))
                       (tumblr-org-test-text "b" '(subtype . "unordered-list-item"))
                       (tumblr-org-test-text "nested"
                                             '(subtype . "unordered-list-item")
                                             '(indent_level . 1))
                       (tumblr-org-test-text "x" '(subtype . "ordered-list-item"))
                       (tumblr-org-test-text "y"
                                             '(subtype . "ordered-list-item"))))))

(ert-deftest tumblr-org-test-blocks ()
  "Quote, verse and source blocks map onto indented and chat text."
  (should (equal (tumblr-org-test-blocks "#+begin_quote\nWise\nwords\n#+end_quote")
                 (list (tumblr-org-test-text "Wise words" '(subtype . "indented")))))
  (should (equal (tumblr-org-test-blocks "#+begin_verse\nline one\nline two\n#+end_verse")
                 (list (tumblr-org-test-text "line one" '(subtype . "chat"))
                       (tumblr-org-test-text "line two" '(subtype . "chat")))))
  (should (equal (tumblr-org-test-blocks "#+begin_src elisp\n(+ 1 2)\n(+ 3 4)\n#+end_src")
                 (list (tumblr-org-test-text "(+ 1 2)\n(+ 3 4)"
                                             '(subtype . "chat"))))))

(ert-deftest tumblr-org-test-images-and-link-blocks ()
  "A lone image link uploads an image; a lone URL becomes a link block."
  (let* ((dir (make-temp-file "tumblr-org-test" t))
         (file (expand-file-name "pic.PNG" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "PNG"))
          (let ((result (tumblr-org-to-npf
                         (format "Before\n\n[[file:%s]]\n\nhttps://example.com/\n"
                                 file))))
            (should (equal (append (car result) nil)
                           (list (tumblr-org-test-text "Before")
                                 `((type . "image")
                                   (media . [((type . "image/png")
                                              (identifier . "image0"))]))
                                 '((type . "link")
                                   (url . "https://example.com/")))))
            (should (equal (cdr result) (list (cons "image0" file))))))
      (delete-directory dir t))))

(ert-deftest tumblr-org-test-keywords-and-passthrough ()
  "Header keywords are skipped; placeholders bring blocks back verbatim."
  (should (equal (tumblr-org-test-blocks "#+blog: x\n#+tags: a, b\n\nBody")
                 (list (tumblr-org-test-text "Body"))))
  (let ((image '((type . "image") (media . [((url . "u") (width . 1))])
                 (alt_text . :null))))
    (should (equal (tumblr-org-test-blocks "#+tumblr-block: 0\n\nAfter"
                                           (vector image))
                   (list image (tumblr-org-test-text "After"))))
    (should (equal (tumblr-org-test-blocks "#+tumblr-block: 5" (vector image))
                   nil))))

(ert-deftest tumblr-org-test-planning-and-drawers-dropped ()
  "Planning lines and property drawers carry no post text."
  (should (equal (tumblr-org-test-blocks
                  (concat "* Title\nSCHEDULED: <2026-09-16 Wed>\n"
                          ":PROPERTIES:\n:TUMBLR_ID: 1\n:END:\nBody"))
                 (list (tumblr-org-test-text "Title" '(subtype . "heading1"))
                       (tumblr-org-test-text "Body")))))

(ert-deftest tumblr-org-test-from-npf ()
  "Text blocks become Org markup and other blocks placeholders."
  (should (equal (tumblr-org-from-npf
                  (vector (tumblr-org-test-text "Title" '(subtype . "heading1"))
                          (tumblr-org-test-text
                           "bold link"
                           '(formatting . [((type . "bold") (start . 0) (end . 4))
                                           ((type . "link") (start . 5) (end . 9)
                                            (url . "https://x/"))]))
                          '((type . "image") (media . []))
                          (tumblr-org-test-text "a" '(subtype . "unordered-list-item"))
                          (tumblr-org-test-text "b" '(subtype . "unordered-list-item")
                                                '(indent_level . 1))
                          (tumblr-org-test-text "q" '(subtype . "indented"))
                          (tumblr-org-test-text "c" '(subtype . "chat"))))
                 (concat "* Title\n\n*bold* [[https://x/][link]]\n\n"
                         "#+tumblr-block: 2\n\n- a\n  - b\n\n"
                         "#+begin_quote\nq\n#+end_quote\n\n"
                         "#+begin_verse\nc\n#+end_verse"))))

(ert-deftest tumblr-org-test-round-trip ()
  "Text posts survive a conversion to Org and back."
  (let ((blocks (list (tumblr-org-test-text "Title" '(subtype . "heading1"))
                      (tumblr-org-test-text
                       "Some bold and a link here"
                       '(formatting . [((type . "bold") (start . 5) (end . 9))
                                       ((type . "link") (start . 16) (end . 20)
                                        (url . "https://x/"))]))
                      (tumblr-org-test-text "one" '(subtype . "ordered-list-item"))
                      (tumblr-org-test-text "two" '(subtype . "ordered-list-item"))
                      (tumblr-org-test-text "Plain end"))))
    (should (equal (tumblr-org-test-blocks (tumblr-org-from-npf (vconcat blocks)))
                   blocks))))

(ert-deftest tumblr-org-test-overlapping-markup-dropped ()
  "Overlapping ranges keep the first and drop the rest when going to Org."
  (should (equal (tumblr-org--markup
                  "abcdef"
                  [((type . "bold") (start . 0) (end . 4))
                   ((type . "italic") (start . 2) (end . 6))
                   ((type . "small") (start . 4) (end . 6))])
                 "*abcd*ef")))

(provide 'tumblr-org-test)
;;; tumblr-org-test.el ends here
