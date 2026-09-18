;;; tumbel-evil.el --- Evil bindings for tumbel.el  -*- lexical-binding: t; -*-

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

;; Opt-in key bindings for evil users.  Call `tumbel-evil-setup' once,
;; for instance from (with-eval-after-load 'tumbel ...).
;;
;; In the normal and motion states the motions worth having in a
;; read-only buffer keep their vim meaning: h j k l, w b e, / n N, v, y,
;; gg and G.  A command whose default key is one of those moves:
;;
;;   n p  ->  gj gk, ]] [[, C-j C-k     l  ->  s     b  ->  B
;;   y    ->  Y     v  ->  gn     ?  ->  g?     g  ->  gr
;;
;; Every other command stays on its default key, at the cost of the
;; motions f, t, E and L.  The default keymaps are left alone, so the
;; Emacs state behaves as documented.
;;
;; Evil is not a dependency of the package: loading this file does not
;; load it, only calling `tumbel-evil-setup' does.

;;; Code:

(require 'tumbel-feed)
(require 'tumbel-lists)
(require 'tumbel-manage)

(declare-function evil-define-key* "ext:evil-core"
                  (state keymap key def &rest bindings))

(defvar evil-snipe-disabled-modes)

;; A key missing from this table falls through to evil's own maps before
;; it reaches `tumbel-feed-mode-map', so the commands that keep their
;; default key are listed too.
(defconst tumbel-evil--feed-bindings
  '(("gj" . tumbel-feed-next)
    ("gk" . tumbel-feed-previous)
    ("]]" . tumbel-feed-next)
    ("[[" . tumbel-feed-previous)
    ("C-j" . tumbel-feed-next)
    ("C-k" . tumbel-feed-previous)
    ("TAB" . forward-button)
    ("<backtab>" . backward-button)
    ("S-<tab>" . backward-button)
    ("RET" . tumbel-feed-activate)
    ("L" . tumbel-feed-load-more)
    ("B" . tumbel-feed-open-blog)
    ("t" . tumbel-feed-browse-tag)
    ("o" . tumbel-feed-browse-url)
    ("Y" . tumbel-feed-copy-url)
    ("i" . tumbel-feed-toggle-images)
    ("s" . tumbel-feed-like)
    ("R" . tumbel-feed-reblog)
    ("f" . tumbel-feed-follow)
    ("D" . tumbel-feed-delete)
    ("c" . tumbel-compose)
    ("r" . tumbel-feed-reblog-with-comment)
    ("E" . tumbel-feed-edit)
    ("gn" . tumbel-feed-show-notes)
    ("P" . tumbel-manage-publish)
    ("A" . tumbel-manage-answer)
    ("S" . tumbel-manage-shuffle)
    ("g?" . tumbel-dispatch)
    ("gr" . revert-buffer)
    ("q" . quit-window))
  "Evil bindings of `tumbel-feed-mode', as (KEY . COMMAND).
KEY is a key description as read by `kbd'.")

(defconst tumbel-evil--lists-bindings
  '(("RET" . tumbel-lists-open)
    ("L" . tumbel-lists-load-more)
    ("o" . tumbel-lists-browse-url)
    ("u" . tumbel-lists-unfollow)
    ("Y" . tumbel-lists-copy-url)
    ("g?" . tumbel-dispatch)
    ("gr" . revert-buffer)
    ("q" . quit-window))
  "Evil bindings of `tumbel-lists-mode', as (KEY . COMMAND).
KEY is a key description as read by `kbd'.")

(defun tumbel-evil--bind (map bindings)
  "Bind BINDINGS in MAP for the evil normal and motion states.
BINDINGS is an alist of key descriptions and commands."
  (pcase-dolist (`(,key . ,command) bindings)
    (evil-define-key* '(normal motion) map (kbd key) command)))

(defun tumbel-evil--disable-snipe ()
  "Keep evil-snipe out of the current buffer.
It binds s, S, f and t in minor mode maps, which outrank the bindings
of a major mode.  Major mode hooks run before global minor modes turn
themselves on, so the buffer-local value is in place by the time
evil-snipe reads it, whether or not it is loaded yet."
  (setq-local evil-snipe-disabled-modes (list major-mode)))

(defun tumbel-evil-setup ()
  "Give the tumbel modes key bindings that fit evil.
They hold in the normal and motion states; the default keymaps are
left alone.  This loads evil.  See the commentary of tumbel-evil.el
for the keys."
  (require 'evil)
  (tumbel-evil--bind tumbel-feed-mode-map tumbel-evil--feed-bindings)
  (tumbel-evil--bind tumbel-lists-mode-map tumbel-evil--lists-bindings)
  ;; `tumbel-lists-mode' needs no such hook: evil-snipe leaves its parent,
  ;; `tabulated-list-mode', alone by default.
  (add-hook 'tumbel-feed-mode-hook #'tumbel-evil--disable-snipe))

(provide 'tumbel-evil)
;;; tumbel-evil.el ends here
