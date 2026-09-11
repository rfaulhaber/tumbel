;;; tumblr.el --- Interactive Tumblr client  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ryan Faulhaber

;; Author: Ryan Faulhaber <ryf@sent.as>
;; Maintainer: Ryan Faulhaber <ryf@sent.as>
;; URL: https://github.com/rfaulhaber/tumblr-mode
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
;; Entry points: `tumblr' (the dashboard, logging in first when
;; needed), `tumblr-blog', `tumblr-tag', `tumblr-login' and
;; `tumblr-logout'.

;;; Code:

(require 'transient)
(require 'tumblr-http)
(require 'tumblr-auth)
(require 'tumblr-api)
(require 'tumblr-user)
(require 'tumblr-media)
(require 'tumblr-npf)
(require 'tumblr-feed)
(require 'tumblr-blog)
(require 'tumblr-compose)
(require 'tumblr-notes)
(require 'tumblr-notifications)
(require 'tumblr-lists)
(require 'tumblr-manage)

(defgroup tumblr nil
  "Interactive Tumblr client."
  :group 'comm
  :prefix "tumblr-")

(defconst tumblr-version "0.1.0"
  "Version of the tumblr package.")

;;;###autoload
(defun tumblr-dashboard ()
  "Show your Tumblr dashboard."
  (interactive)
  (tumblr-feed-display (tumblr-dashboard-source)))

;;;###autoload
(defun tumblr ()
  "Show your Tumblr dashboard, logging in first when needed."
  (interactive)
  (if (tumblr-auth-logged-in-p)
      (tumblr-dashboard)
    (tumblr-login #'tumblr-dashboard)))

;;;###autoload (autoload 'tumblr-dispatch "tumblr" nil t)
(transient-define-prefix tumblr-dispatch ()
  "Show the commands available in Tumblr feed buffers."
  [["Move"
    ("n" "Next post" tumblr-feed-next)
    ("p" "Previous post" tumblr-feed-previous)
    ("L" "Load more" tumblr-feed-load-more)
    ("g" "Reload" revert-buffer)]
   ["Post"
    ("l" "Like / unlike" tumblr-feed-like)
    ("R" "Reblog" tumblr-feed-reblog)
    ("f" "Follow / unfollow blog" tumblr-feed-follow)
    ("D" "Delete (own post)" tumblr-feed-delete)
    ("r" "Reblog with comment" tumblr-feed-reblog-with-comment)
    ("E" "Edit (own post)" tumblr-feed-edit)
    ("c" "Compose" tumblr-compose)
    ("v" "Notes" tumblr-feed-show-notes)
    ("P" "Publish now" tumblr-manage-publish)
    ("A" "Answer ask" tumblr-manage-answer)]
   ["Open"
    ("RET" "Post or button" tumblr-feed-activate)
    ("b" "Blog of post" tumblr-feed-open-blog)
    ("t" "Tag of post" tumblr-feed-browse-tag)
    ("o" "In browser" tumblr-feed-browse-url)
    ("y" "Copy URL" tumblr-feed-copy-url)]
   ["Views"
    ("d" "Dashboard" tumblr-dashboard)
    ("N" "Activity" tumblr-notifications)
    ("K" "Likes" tumblr-likes)
    ("F" "Following" tumblr-following)
    ("Q" "Queue" tumblr-queue)
    ("W" "Drafts" tumblr-drafts)
    ("I" "Inbox" tumblr-inbox)
    ("B" "A blog" tumblr-blog)
    ("T" "A tag" tumblr-tag)
    ("i" "Toggle images" tumblr-feed-toggle-images)
    ("q" "Quit" quit-window)]])

(define-key tumblr-feed-mode-map (kbd "?") #'tumblr-dispatch)
(provide 'tumblr)
;;; tumblr.el ends here
