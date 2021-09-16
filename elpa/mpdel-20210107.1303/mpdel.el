;;; mpdel.el --- Play and control your MPD music  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: multimedia
;; Url: https://gitea.petton.fr/mpdel/mpdel
;; Package-requires: ((emacs "25.1") (libmpdel "1.2.0") (navigel "0.7.0"))
;; Version: 1.0.0

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

;; MPDel is an Emacs client for Music Player Daemon (MPD), a flexible,
;; powerful, server-side application for playing music.  This project
;; provides an Emacs user interface including playlists, navigation in
;; the database and playback control.  Read the README.org file for
;; more information.

;;; Code:

(require 'mpdel-song)
(require 'mpdel-playlist)
(require 'mpdel-tablist)
(require 'mpdel-browser)


;;; Customization
(defgroup mpdel nil
  "Configure MPDel."
  :group 'libmpdel)

(defcustom mpdel-prefix-key (kbd "C-x Z")
  "Prefix key to all global mpdel keybindings."
  :type 'key-sequence)


;;; Minor mode: Define the global minor mode so users can control MPD
;;; from non-MPDel buffers

(defvar mpdel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map mpdel-prefix-key 'mpdel-core-map)
    map)
  "Keymap activating variable `mpdel-core-map'.")

(define-minor-mode mpdel-mode
  "Activate keybindings to play and control your MPD server.

\\{mpdel-mode-map}"
  :global t
  :require 'mpdel
  :lighter " MPDel")

(provide 'mpdel)
;;; mpdel.el ends here

;;; LocalWords:  Mpdel mpdel
