;;; tumbel-evil-test.el --- Tests for tumbel-evil.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the evil bindings.  They look keys up with evil switched
;; on in a real buffer, because what matters is which map wins, not what
;; the binding tables say.

;;; Code:

(require 'ert)
(require 'evil)
(require 'evil-snipe)
(require 'tumbel-evil)

;; Stands in for evil-collection, which binds keys on the parent of
;; `tumbel-lists-mode-map' before tumbel-evil is set up.
(evil-define-key* 'normal tabulated-list-mode-map
  "{" #'tabulated-list-narrow-current-column)

(tumbel-evil-setup)

(defmacro tumbel-evil-test-with-mode (mode &rest body)
  "Evaluate BODY in a MODE buffer in evil normal state."
  (declare (indent 1))
  `(with-temp-buffer
     (funcall ,mode)
     (evil-local-mode 1)
     (evil-normal-state)
     ,@body))

(defun tumbel-evil-test-bindings (alist)
  "Check that each key of ALIST runs its command in the current buffer.
ALIST maps key descriptions to commands."
  (dolist (entry alist)
    (ert-info ((format "key %s" (car entry)))
      (should (eq (key-binding (kbd (car entry))) (cdr entry))))))

(defun tumbel-evil-test-unreachable (map)
  "Return the commands bound in MAP that no key runs in the current buffer.
The parent of MAP is not searched."
  (let (missing)
    (map-keymap-internal
     (lambda (_event command)
       (when (and (commandp command)
                  (not (where-is-internal command nil t)))
         (push command missing)))
     map)
    missing))

(ert-deftest tumbel-evil-test-feed-commands-reachable ()
  "Every command of the feed keymap has a key in normal state."
  (tumbel-evil-test-with-mode #'tumbel-feed-mode
    (should-not (tumbel-evil-test-unreachable tumbel-feed-mode-map))))

(ert-deftest tumbel-evil-test-lists-commands-reachable ()
  "Every command of the lists keymap has a key in normal state."
  (tumbel-evil-test-with-mode #'tumbel-lists-mode
    (should-not (tumbel-evil-test-unreachable tumbel-lists-mode-map))))

(ert-deftest tumbel-evil-test-feed-moved-keys ()
  "Commands whose default key is a vim essential answer to another key."
  (tumbel-evil-test-with-mode #'tumbel-feed-mode
    (tumbel-evil-test-bindings
     '(("gj" . tumbel-feed-next)
       ("gk" . tumbel-feed-previous)
       ("]]" . tumbel-feed-next)
       ("[[" . tumbel-feed-previous)
       ("C-j" . tumbel-feed-next)
       ("C-k" . tumbel-feed-previous)
       ("s" . tumbel-feed-like)
       ("B" . tumbel-feed-open-blog)
       ("Y" . tumbel-feed-copy-url)
       ("gn" . tumbel-feed-show-notes)
       ("g?" . tumbel-dispatch)
       ("gr" . revert-buffer)
       ("q" . quit-window)))))

(ert-deftest tumbel-evil-test-feed-kept-keys ()
  "Default keys that only shadow an editing operator still work."
  (tumbel-evil-test-with-mode #'tumbel-feed-mode
    (tumbel-evil-test-bindings
     '(("c" . tumbel-compose)
       ("r" . tumbel-feed-reblog-with-comment)
       ("R" . tumbel-feed-reblog)
       ("D" . tumbel-feed-delete)
       ("i" . tumbel-feed-toggle-images)
       ("o" . tumbel-feed-browse-url)
       ("f" . tumbel-feed-follow)
       ("t" . tumbel-feed-browse-tag)
       ("E" . tumbel-feed-edit)
       ("L" . tumbel-feed-load-more)
       ("P" . tumbel-manage-publish)
       ("A" . tumbel-manage-answer)
       ("S" . tumbel-manage-shuffle)
       ("RET" . tumbel-feed-activate)
       ("TAB" . forward-button)))))

(ert-deftest tumbel-evil-test-feed-vim-essentials ()
  "The motions, search, visual state and yank keep their vim meaning."
  (tumbel-evil-test-with-mode #'tumbel-feed-mode
    (tumbel-evil-test-bindings
     '(("h" . evil-backward-char)
       ("j" . evil-next-line)
       ("k" . evil-previous-line)
       ("l" . evil-forward-char)
       ("w" . evil-forward-word-begin)
       ("b" . evil-backward-word-begin)
       ("e" . evil-forward-word-end)
       ("v" . evil-visual-char)
       ("y" . evil-yank)
       ("gg" . evil-goto-first-line)
       ("G" . evil-goto-line)))
    (dolist (key '("/" "n" "N"))
      (ert-info ((format "key %s" key))
        (should (string-prefix-p "evil-"
                                 (symbol-name (key-binding (kbd key)))))))))

(ert-deftest tumbel-evil-test-feed-motion-state ()
  "The bindings also hold in motion state."
  (tumbel-evil-test-with-mode #'tumbel-feed-mode
    (evil-motion-state)
    (tumbel-evil-test-bindings
     '(("s" . tumbel-feed-like)
       ("gj" . tumbel-feed-next)
       ("c" . tumbel-compose)
       ("l" . evil-forward-char)))))

(ert-deftest tumbel-evil-test-feed-beats-evil-snipe ()
  "With evil-snipe on everywhere its keys still run tumbel commands."
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  (unwind-protect
      (tumbel-evil-test-with-mode #'tumbel-feed-mode
        (tumbel-evil-test-bindings
         '(("s" . tumbel-feed-like)
           ("S" . tumbel-manage-shuffle)
           ("f" . tumbel-feed-follow)
           ("t" . tumbel-feed-browse-tag))))
    (evil-snipe-override-mode -1)
    (evil-snipe-mode -1)))

(ert-deftest tumbel-evil-test-default-maps-untouched ()
  "Without evil in the buffer the documented keys are what they were."
  (with-temp-buffer
    (tumbel-feed-mode)
    (tumbel-evil-test-bindings
     '(("n" . tumbel-feed-next)
       ("l" . tumbel-feed-like)
       ("y" . tumbel-feed-copy-url)))))

(ert-deftest tumbel-evil-test-lists-keys ()
  "The lists follow the feed: only the URL copy leaves its default key."
  (tumbel-evil-test-with-mode #'tumbel-lists-mode
    (tumbel-evil-test-bindings
     '(("RET" . tumbel-lists-open)
       ("L" . tumbel-lists-load-more)
       ("o" . tumbel-lists-browse-url)
       ("u" . tumbel-lists-unfollow)
       ("Y" . tumbel-lists-copy-url)
       ("g?" . tumbel-dispatch)
       ("gr" . revert-buffer)
       ("q" . quit-window)
       ("y" . evil-yank)
       ("j" . evil-next-line)))))

(ert-deftest tumbel-evil-test-lists-parent-bound-before-setup ()
  "An evil binding on the parent map made before the setup survives it."
  (tumbel-evil-test-with-mode #'tumbel-lists-mode
    (tumbel-evil-test-bindings
     '(("{" . tabulated-list-narrow-current-column)))))

(ert-deftest tumbel-evil-test-lists-parent-bound-after-setup ()
  "An evil binding on the parent map made after the setup is seen too."
  (evil-define-key* 'normal tabulated-list-mode-map
    "}" #'tabulated-list-widen-current-column)
  (tumbel-evil-test-with-mode #'tumbel-lists-mode
    (tumbel-evil-test-bindings
     '(("}" . tabulated-list-widen-current-column)))))

(ert-deftest tumbel-evil-test-evil-loaded-by-setup-only ()
  "Loading tumbel-evil leaves evil alone; the setup loads it and binds.
This runs in a child Emacs, since evil cannot be unloaded from this one."
  (let ((form '(progn
                 (require 'tumbel-evil)
                 (when (featurep 'evil)
                   (kill-emacs 2))
                 (tumbel-evil-setup)
                 (unless (eq (lookup-key
                              (evil-get-auxiliary-keymap
                               tumbel-feed-mode-map 'normal)
                              "s")
                             'tumbel-feed-like)
                   (kill-emacs 3)))))
    (with-temp-buffer
      (let ((status (apply #'call-process
                           (expand-file-name invocation-name
                                             invocation-directory)
                           nil t nil
                           `("-Q" "--batch"
                             ,@(mapcan (lambda (dir) (list "-L" dir))
                                       (copy-sequence load-path))
                             "--eval" ,(prin1-to-string form)))))
        (ert-info ((buffer-string) :prefix "Child output: ")
          (should (eql status 0)))))))

(provide 'tumbel-evil-test)
;;; tumbel-evil-test.el ends here
