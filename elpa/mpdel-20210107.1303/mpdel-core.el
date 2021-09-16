;;; mpdel-core.el --- Provide code to be reused by mpdel modes  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020  Damien Cassou

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


;;; `navigel' general configuration

(navigel-method mpdel navigel-name (entity)
  (libmpdel-entity-name entity))

(navigel-method mpdel navigel-children (entity callback)
  (libmpdel-list entity callback))

(navigel-method mpdel navigel-equal (entity1 entity2)
  (libmpdel-equal entity1 entity2))

(navigel-method mpdel navigel-parent (entity)
  (libmpdel-entity-parent entity))

(defcustom mpdel-core-volume-step 5
  "Amount to increase / decrease the playback volume by."
  :type 'integer
  :group 'mpdel-core)


;;; Public functions

(defun mpdel-core-open (entity &optional target)
  "Open a buffer showing ENTITY.
If TARGET is non nil and visible on the buffer, move point to
it."
  (let ((navigel-app 'mpdel))
    (navigel-open entity target)))

(defun mpdel-core-selected-entities ()
  "Return the selected entities in the current buffer.

If any entity is marked, return the list of all marked entities.
If no entity is marked but there is an entity at point, return a
list with this entity.  Otherwise, return nil."
  (navigel-marked-entities t))

(defun mpdel-core-add-to-current-playlist ()
  "Add selected entities to current playlist."
  (interactive)
  (libmpdel-current-playlist-add (mpdel-core-selected-entities)))

(defun mpdel-core-add-to-stored-playlist ()
  "Add selected entities to a stored playlist."
  (interactive)
  (libmpdel-stored-playlist-add (mpdel-core-selected-entities)))

(defun mpdel-core-replace-current-playlist ()
  "Replace current playlist with selected entities."
  (interactive)
  (libmpdel-current-playlist-replace (mpdel-core-selected-entities)))

(defun mpdel-core-replace-stored-playlist ()
  "Replace a stored playlist with selected entities."
  (interactive)
  (libmpdel-stored-playlist-replace (mpdel-core-selected-entities)))

(defun mpdel-core-insert-current-playlist ()
  "Insert selected entities after currently-played song.
Start playing the first.

If no entity is selected, restart playing the current song."
  (interactive)
  (let ((entities (mpdel-core-selected-entities)))
    (if (not entities)
        (libmpdel-playback-seek "0")
      (libmpdel-current-playlist-insert entities))))

(defun mpdel-core-dired ()
  "Open dired on the entity at point."
  (interactive)
  (libmpdel-dired (navigel-entity-at-point)))

;;;###autoload
(defun mpdel-core-open-artists ()
  "Display all artists in the MPD database."
  (interactive)
  (mpdel-core-open 'artists))

;;;###autoload
(defun mpdel-core-open-stored-playlists ()
  "Display all stored playlists in MPD."
  (interactive)
  (mpdel-core-open 'stored-playlists))

;;;###autoload
(defun mpdel-core-open-albums ()
  "Display all albums in the MPD database."
  (interactive)
  (mpdel-core-open 'albums))

;;;###autoload
(defun mpdel-core-open-directories ()
  "Display all top-level directories in the MPD database."
  (interactive)
  (mpdel-core-open 'directories))

;;;###autoload
(defun mpdel-core-search-by-artist (name)
  "Display all songs whose artist's name match NAME.
Interactively, ask for NAME."
  (interactive (list (read-from-minibuffer "Search for artist: ")))
  (mpdel-core-open (libmpdel-search-criteria-create :type "artist" :what name)))

;;;###autoload
(defun mpdel-core-search-by-album (name)
  "Display all songs whose album's name match NAME.
Interactively, ask for NAME."
  (interactive (list (read-from-minibuffer "Search for album: ")))
  (mpdel-core-open (libmpdel-search-criteria-create :type "album" :what name)))

;;;###autoload
(defun mpdel-core-search-by-title (title)
  "Display all songs matching TITLE.
Interactively, ask for TITLE."
  (interactive (list (read-from-minibuffer "Search for title: ")))
  (mpdel-core-open (libmpdel-search-criteria-create :type "title" :what title)))

;;;###autoload
(defun mpdel-core-search-by-filter (filter)
  "Display all songs matching the mpd filter expression FILTER.
Interactively, ask for FILTER.

Example: ((artist == 'name') AND (any contains 'text'))
Documentation: https://www.musicpd.org/doc/html/protocol.html#filters"
  (interactive (list (read-from-minibuffer "Search with filter: ")))
  (mpdel-core-open (libmpdel-filter-create :text filter)))

(defun mpdel-core-volume-adjust (amount)
  "Adjust the playback volume by AMOUNT."
  (let* ((volume (string-to-number (libmpdel-volume)))
         (new-volume (max 0 (min 100 (+ volume amount)))))
    (libmpdel-playback-set-volume new-volume)))

;;;###autoload
(defun mpdel-core-volume-increase (&optional amount)
  "Increase the playback volume by AMOUNT.
If AMOUNT is nil, `mpdel-core-volume-step' is used instead.
Called interactively, AMOUNT can be passed as a prefix argument."
  (interactive "P")
  (cond
   ((null amount) (mpdel-core-volume-adjust mpdel-core-volume-step))
   ((listp amount)
    (mpdel-core-volume-adjust (* (car amount) mpdel-core-volume-step)))
   (t (mpdel-core-volume-adjust amount))))

;;;###autoload
(defun mpdel-core-volume-decrease (&optional amount)
  "Decrease the playback volume by AMOUNT.
If AMOUNT is nil, `mpdel-core-volume-step' is used instead.
Called interactively, AMOUNT can be passed as a prefix argument."
  (interactive "P")
  (cond
   ((null amount) (mpdel-core-volume-adjust (- mpdel-core-volume-step)))
   ((listp amount)
    (mpdel-core-volume-adjust (- (* (car amount) mpdel-core-volume-step))))
   (t (mpdel-core-volume-adjust (- amount)))))


;;; Mode

(defvar mpdel-core-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'libmpdel-playback-play-pause)
    (define-key map (kbd "M-n") #'libmpdel-playback-next)
    (define-key map (kbd "M-p") #'libmpdel-playback-previous)
    (define-key map (kbd "a") #'mpdel-core-add-to-current-playlist)
    (define-key map (kbd "A") #'mpdel-core-add-to-stored-playlist)
    (define-key map (kbd "r") #'mpdel-core-replace-current-playlist)
    (define-key map (kbd "R") #'mpdel-core-replace-stored-playlist)
    (define-key map (kbd "P") #'mpdel-core-insert-current-playlist)
    (define-key map (kbd "C-x C-j") #'mpdel-core-dired)
    (define-key map (kbd "N") #'mpdel-core-open-artists)
    (define-key map (kbd "L") #'mpdel-core-open-stored-playlists)
    (define-key map (kbd "s s") #'mpdel-core-search-by-title)
    (define-key map (kbd "s l") #'mpdel-core-search-by-album)
    (define-key map (kbd "s r") #'mpdel-core-search-by-artist)
    (define-key map (kbd "^") #'navigel-open-parent)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "+") #'mpdel-core-volume-increase)
    (define-key map (kbd "-") #'mpdel-core-volume-decrease)
    (define-key map (kbd "C") #'libmpdel-connect-profile)
    map)
  "Keybindings for all MPDel buffers.")

;; Make it possible to activate `mpdel-core-map' from a keybinding:
(fset 'mpdel-core-map mpdel-core-map)

(provide 'mpdel-core)
;;; mpdel-core.el ends here

;; Local Variables:
;; checkdoc-symbol-words: ("top-level")
;; End:
