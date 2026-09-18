;;; tumbel.el --- Interactive Tumblr client  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan Faulhaber

;; Author: Ryan Faulhaber <ryf@sent.as>
;; Maintainer: Ryan Faulhaber <ryf@sent.as>
;; URL: https://github.com/rfaulhaber/tumbel
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (plz "0.9"))
;; Keywords: comm, hypermedia

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

;; An interactive Tumblr client for Emacs, in the spirit of
;; twittering-mode and md4rd: browse your dashboard and blogs, and
;; post, reblog and like without leaving Emacs.
;;
;; Entry points: `tumbel' (the dashboard, logging in first when
;; needed), `tumbel-blog', `tumbel-tag', `tumbel-login' and
;; `tumbel-logout'.

;;; Code:

(require 'transient)
(require 'tumbel-http)
(require 'tumbel-auth)
(require 'tumbel-api)
(require 'tumbel-user)
(require 'tumbel-media)
(require 'tumbel-npf)
(require 'tumbel-feed)
(require 'tumbel-blog)
(require 'tumbel-compose)
(require 'tumbel-post)
(require 'tumbel-notes)
(require 'tumbel-notifications)
(require 'tumbel-lists)
(require 'tumbel-manage)
(require 'tumbel-evil)

(defgroup tumbel nil
  "Interactive Tumblr client."
  :group 'comm
  :prefix "tumbel-")

(defconst tumbel-version "0.1.0"
  "Version of the tumbel package.")

;;;###autoload
(defun tumbel-dashboard ()
  "Show your Tumblr dashboard."
  (interactive)
  (tumbel-feed-display (tumbel-dashboard-source)))

;;;###autoload
(defun tumbel ()
  "Show your Tumblr dashboard, logging in first when needed."
  (interactive)
  (if (tumbel-auth-logged-in-p)
      (tumbel-dashboard)
    (tumbel-login #'tumbel-dashboard)))

;;;###autoload (autoload 'tumbel-dispatch "tumbel" nil t)
(transient-define-prefix tumbel-dispatch ()
  "Show the commands available in Tumblr feed buffers."
  [["Move"
    ("n" "Next post" tumbel-feed-next)
    ("p" "Previous post" tumbel-feed-previous)
    ("L" "Load more" tumbel-feed-load-more)
    ("g" "Reload" revert-buffer)]
   ["Post"
    ("l" "Like / unlike" tumbel-feed-like)
    ("R" "Reblog" tumbel-feed-reblog)
    ("f" "Follow / unfollow blog" tumbel-feed-follow)
    ("D" "Delete (own post)" tumbel-feed-delete)
    ("r" "Reblog with comment" tumbel-feed-reblog-with-comment)
    ("E" "Edit (own post)" tumbel-feed-edit)
    ("c" "Compose" tumbel-compose)
    ("v" "Notes" tumbel-feed-show-notes)
    ("P" "Publish now" tumbel-manage-publish)
    ("A" "Answer ask" tumbel-manage-answer)]
   ["Open"
    ("RET" "Post or button" tumbel-feed-activate)
    ("b" "Blog of post" tumbel-feed-open-blog)
    ("t" "Tag of post" tumbel-feed-browse-tag)
    ("o" "In browser" tumbel-feed-browse-url)
    ("y" "Copy URL" tumbel-feed-copy-url)]
   ["Views"
    ("d" "Dashboard" tumbel-dashboard)
    ("N" "Activity" tumbel-notifications)
    ("K" "Likes" tumbel-likes)
    ("F" "Following" tumbel-following)
    ("Q" "Queue" tumbel-queue)
    ("W" "Drafts" tumbel-drafts)
    ("I" "Inbox" tumbel-inbox)
    ("B" "A blog" tumbel-blog)
    ("T" "A tag" tumbel-tag)
    ("i" "Toggle images" tumbel-feed-toggle-images)
    ("q" "Quit" quit-window)]])

(define-key tumbel-feed-mode-map (kbd "?") #'tumbel-dispatch)
(provide 'tumbel)
;;; tumbel.el ends here
