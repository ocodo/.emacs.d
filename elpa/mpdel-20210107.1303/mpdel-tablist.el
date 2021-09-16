;;; mpdel-tablist.el --- Support representing MPD entities in tablists  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: multimedia
;; Url: https://gitea.petton.fr/mpdel/mpdel
;; Package-requires: ((emacs "25.1"))
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

;; This file provides some common ground for all MPDel
;; user-interfaces.

;;; Code:
(require 'libmpdel)
(require 'navigel)

(require 'mpdel-core)


;;; Customization

(defgroup mpdel-tablist nil
  "Display and manipulate MPD through tablists."
  :group 'libmpdel)

(defface mpdel-tablist-song-name-face
  '((t . (:inherit default)))
  "Face for song names in playlist.")

(defface mpdel-tablist-track-face
  '((t . (:inherit default)))
  "Face for track numbers in playlist.")

(defface mpdel-tablist-album-face
  '((t . (:inherit default)))
  "Face for album names in playlist.")

(defface mpdel-tablist-disk-face
  '((t . (:inherit default)))
  "Face for disk numbers in playlist.")

(defface mpdel-tablist-date-face
  '((t . (:inherit default)))
  "Face for dates in playlist.")

(defface mpdel-tablist-artist-face
  '((t . (:inherit default)))
  "Face for artist names in playlist.")


;;; `navigel' major-mode configuration

(navigel-method mpdel navigel-entity-tablist-mode ((_entity (eql artists)))
  (mpdel-tablist-mode))

(navigel-method mpdel navigel-entity-tablist-mode ((_entity (eql albums)))
  (mpdel-tablist-mode))

(navigel-method mpdel navigel-entity-tablist-mode ((_entity libmpdel-search-criteria))
  (mpdel-tablist-mode))

(navigel-method mpdel navigel-entity-tablist-mode ((_entity libmpdel-filter))
  (mpdel-tablist-mode))

(navigel-method mpdel navigel-entity-tablist-mode ((_entity libmpdel-artist))
  (mpdel-tablist-mode))

(navigel-method mpdel navigel-entity-tablist-mode ((_entity libmpdel-album))
  (mpdel-tablist-mode))



;;; `navigel' tabulated list configuration

(defun mpdel-tablist--album-format ()
  "Return `tabulated-list-format' value for albums."
  (vector (list "Name" 40 t)
          (list "Artist" 0 t)))

(defun mpdel-tablist--song-format ()
  "Return `tabulated-list-format' value for songs."
  (vector (list "Title" 30 t)
          (list "#" 6 nil)
          (list "Album" 30 t)
          (list "Disk" 4 t)
          (list "Date" 5 t)
          (list "Artist" 0 t)))

(navigel-method mpdel navigel-entity-to-columns ((entity libmpdel-album))
  (vector (libmpdel-entity-name entity)
          (libmpdel-artist-name entity)))

(navigel-method mpdel navigel-entity-to-columns ((song libmpdel-song))
  (vector
   (propertize (or (libmpdel-entity-name song) "") 'face 'mpdel-tablist-song-name-face)
   (propertize (or (libmpdel-song-track song) "") 'face 'mpdel-tablist-track-face)
   (propertize (or (libmpdel-album-name song) "") 'face 'mpdel-tablist-album-face)
   (propertize (or (libmpdel-song-disc song) "") 'face 'mpdel-tablist-disk-face)
   (propertize (or (libmpdel-entity-date song) "") 'face 'mpdel-tablist-date-face)
   (propertize (or (libmpdel-artist-name song) "") 'face 'mpdel-tablist-artist-face)))

(navigel-method mpdel navigel-tablist-format ((_entity libmpdel-artist))
  (mpdel-tablist--album-format))

(navigel-method mpdel navigel-tablist-format ((_entity libmpdel-album))
  (mpdel-tablist--song-format))

(navigel-method mpdel navigel-tablist-format ((_entity libmpdel-search-criteria))
  (mpdel-tablist--song-format))

(navigel-method mpdel navigel-tablist-format ((_entity libmpdel-filter))
  (mpdel-tablist--song-format))

(navigel-method mpdel navigel-tablist-format ((_entity (eql current-playlist)))
  (mpdel-tablist--song-format))

(navigel-method mpdel navigel-tablist-format ((_entity libmpdel-stored-playlist))
  (mpdel-tablist--song-format))


;;; Major-mode

(defvar mpdel-tablist-mode-map
  (let ((map (make-sparse-keymap)))
    ;; inherit from both `mpdel-core-map' and
    ;; `navigel-tablist-mode-map':
    (set-keymap-parent
     map
     (make-composed-keymap mpdel-core-map navigel-tablist-mode-map))
    (define-key map (kbd "k") #'tablist-do-delete)
    map)
  "Keybindings for `mpdel-core-tablist-mode'.")

(define-derived-mode mpdel-tablist-mode navigel-tablist-mode "MPDel Tablist")

(provide 'mpdel-tablist)
;;; mpdel-tablist.el ends here
