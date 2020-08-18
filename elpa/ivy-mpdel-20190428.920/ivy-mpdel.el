;;; ivy-mpdel.el --- Ivy interface to navigate MPD     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: multimedia
;; Package-Version: 20190428.920
;; Package-Commit: a42dcc943914c71975c115195d38c739f25e475c
;; Url: https://gitlab.petton.fr/mpdel/ivy-mpdel
;; Package-requires: ((emacs "25.1") (ivy "0.10.0") (libmpdel "1.0.0") (mpdel "1.0.0"))
;; Keywords: multimedia
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

;; The package ivy-mpdel provides an ivy/counsel interface to navigate
;; your Music Player Daemon database (MPD) database, a flexible,
;; powerful,server-side application for playing music.  ivy-mpdel
;; works in cooperation with (and depends on) mpdel, an Emacs client
;; for MPD.

;;; Code:

(require 'ivy)

(require 'libmpdel)
(require 'mpdel-core)
(require 'mpdel-song)


;;; Helper functions

(defun ivy-mpdel--wrap (entity)
  "Wrap ENTITY into an object suitable for `ivy-read'."
  (cons (substring-no-properties (libmpdel-entity-name entity)) entity))

(defun ivy-mpdel--wrap-all (entities)
  "Wrap all ENTITIES into objects suitable for `ivy-read'."
  (mapcar #'ivy-mpdel--wrap entities))

(defun ivy-mpdel--unwrap (pair)
  "Return the entity wrapped within PAIR."
  (cdr pair))

(defun ivy-mpdel--apply-unwrapped (function)
  "Return a function applying FUNCTION after unwrapping its argument."
  (lambda (pair) (funcall function (ivy-mpdel--unwrap pair))))

(cl-defgeneric ivy-mpdel--main-action (entity)
  "Return a function called when selecting ENTITY."
  (ivy-mpdel-list entity))

(cl-defmethod ivy-mpdel--main-action ((song libmpdel-song))
  (mpdel-song-open song))

(cl-defgeneric ivy-mpdel--caller (_entity)
  "Return a symbol representing ivy's caller for _ENTITY."
  'ivy-mpdel-list)

(defun ivy-mpdel--list-parent (entity)
  "Start ivy-mpdel interface on parent of ENTITY."
  (let* ((parent (libmpdel-entity-parent entity))
         (ancestor (and parent (libmpdel-entity-parent parent))))
    (when (or ancestor parent)
      (ivy-mpdel-list (or ancestor parent)))))


;;; Ivy interface

;;;###autoload
(defun ivy-mpdel-list (&optional entity)
  "Select a child of ENTITY.
If ENTITY is nil, select from all artists."
  (interactive)
  (libmpdel-list
   (or entity 'artists)
   (lambda (children)
     (ivy-read "Entity: "
               (ivy-mpdel--wrap-all children)
               :action (ivy-mpdel--apply-unwrapped #'ivy-mpdel--main-action)
               :caller (ivy-mpdel--caller entity)))))

;;;###autoload
(defun ivy-mpdel-artists ()
  "Select music from a list of artists."
  (interactive)
  (ivy-mpdel-list 'artists))

;;;###autoload
(defun ivy-mpdel-stored-playlists ()
  "Select music from a stored playlist or edit one."
  (interactive)
  (ivy-mpdel-list 'stored-playlists))

(ivy-add-actions
 'ivy-mpdel-list
 `(("a" ,(ivy-mpdel--apply-unwrapped #'libmpdel-current-playlist-add) "Add to current playlist")
   ("A" ,(ivy-mpdel--apply-unwrapped #'libmpdel-stored-playlist-add) "Add to a stored playlist")
   ("r" ,(ivy-mpdel--apply-unwrapped #'libmpdel-current-playlist-replace) "Replace current playlist")
   ("R" ,(ivy-mpdel--apply-unwrapped #'libmpdel-current-playlist-replace) "Replace stored playlist")
   ("p" ,(ivy-mpdel--apply-unwrapped #'libmpdel-current-playlist-insert) "Insert after currently-played song")
   ("^" ,(ivy-mpdel--apply-unwrapped #'ivy-mpdel--list-parent) "List parent entity")))

(define-key mpdel-core-map (kbd "i") #'ivy-mpdel-artists)
(define-key mpdel-core-map (kbd "I") #'ivy-mpdel-stored-playlists)

(provide 'ivy-mpdel)
;;; ivy-mpdel.el ends here
