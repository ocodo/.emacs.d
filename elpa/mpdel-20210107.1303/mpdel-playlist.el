;;; mpdel-playlist.el --- Display and manipulate MPD playlists  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020  Damien Cassou

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

;; Let the user view and manipulate MPD's current playlist and stored
;; playlists.

;;; Code:
(require 'mpdel-core)
(require 'mpdel-tablist)


;;; Customization

(defgroup mpdel-playlist nil
  "Display and manipulate MPD playlists."
  :group 'libmpdel)

(defface mpdel-playlist-current-song-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face to highlight current song in playlist."
  :group 'mpdel-playlist)


;;; `navigel' major-mode configuration

(navigel-method mpdel navigel-entity-tablist-mode ((_entity (eql current-playlist)))
  (mpdel-playlist-current-playlist-mode))

(navigel-method mpdel navigel-entity-tablist-mode ((_entity libmpdel-stored-playlist))
  (mpdel-playlist-stored-playlist-mode))

(navigel-method mpdel navigel-delete ((songs list) &context (major-mode mpdel-playlist-mode) &optional _callback)
  (libmpdel-playlist-delete songs navigel-entity))


;;; Private functions

(defun mpdel-playlist--highlight ()
  "Highlight currently played song in current buffer."
  (save-excursion
    (when (navigel-go-to-entity (libmpdel-current-song))
      (let ((inhibit-read-only t))
        (put-text-property (line-beginning-position) (line-end-position)
                           'face 'mpdel-playlist-current-song-face)))))


;;; Private functions

(defun mpdel-playlist--register-to-hooks ()
  "Register to several hooks to refresh automatically refresh the current buffer.."
  (let ((buffer (current-buffer)))
    (let* ((refresh-fn (lambda ()
                         (with-current-buffer buffer
                           (navigel-refresh))))
           (playlist navigel-entity)
           (hooks (if (libmpdel-stored-playlist-p playlist)
                      '(libmpdel-stored-playlist-changed-hook)
                    '(libmpdel-current-playlist-changed-hook
                      libmpdel-current-song-changed-hook
                      libmpdel-player-changed-hook))))
      (dolist (hook hooks)
        (add-hook hook refresh-fn))
      (add-hook 'kill-buffer-hook
                (lambda () (dolist (hook hooks) (remove-hook hook refresh-fn)))
                nil t))))


;;; Public functions

;;;###autoload
(defun mpdel-playlist-open ()
  "Display the current playlist."
  (interactive)
  (mpdel-core-open 'current-playlist))

(define-key mpdel-core-map (kbd "l") #'mpdel-playlist-open)

;;;###autoload
(defun mpdel-playlist-open-stored-playlist ()
  "Ask for a stored playlist and open it."
  (interactive)
  (libmpdel-funcall-on-stored-playlist #'mpdel-core-open))

(defun mpdel-playlist-play ()
  "Start playing the song at point."
  (interactive)
  (if (derived-mode-p 'mpdel-playlist-current-playlist-mode)
      (libmpdel-play-song (navigel-entity-at-point))
    (mpdel-core-insert-current-playlist)))

(defun mpdel-playlist-move-up ()
  "Move selected songs up in the current playlist."
  (interactive)
  (let ((songs (mpdel-core-selected-entities)))
    (when songs
      (libmpdel-playlist-move-up songs))))

(defun mpdel-playlist-move-down ()
  "Move selected songs down in the current playlist."
  (interactive)
  (let ((songs (mpdel-core-selected-entities)))
    (when songs
      (libmpdel-playlist-move-down songs))))

(defun mpdel-playlist-save ()
  "Save current playlist into a new stored playlist.
Ask for stored playlist name."
  (interactive)
  (if (libmpdel-current-playlist-p navigel-entity)
      (call-interactively #'libmpdel-playlist-save)
    (user-error "You can only save from the current playlist")))


;;; Modes

(defvar mpdel-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keybindings for `mpdel-playlist-mode'.")

(define-derived-mode mpdel-playlist-mode mpdel-tablist-mode "MPDel Playlist"
  (add-hook 'navigel-init-done-hook #'mpdel-playlist--register-to-hooks nil t))

(defvar mpdel-playlist-current-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<M-up>") #'mpdel-playlist-move-up)
    (define-key map (kbd "<M-down>") #'mpdel-playlist-move-down)
    (define-key map (kbd "C-x C-s") #'mpdel-playlist-save)
    (define-key map (kbd "P") #'mpdel-playlist-play)
    map)
  "Keybindings for `mpdel-playlist-current-playlist-mode'.")

(define-derived-mode mpdel-playlist-current-playlist-mode mpdel-playlist-mode "MPDel Current playlist"
  "Major mode to display the current playlist."
  (add-hook 'navigel-changed-hook #'mpdel-playlist--highlight nil t))

(defvar mpdel-playlist-stored-playlist-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keybindings for `mpdel-playlist-stored-playlist-mode'.")


(define-derived-mode mpdel-playlist-stored-playlist-mode mpdel-playlist-mode "MPDel Stored playlist"
  "Major mode to display a stored playlist.")

(provide 'mpdel-playlist)
;;; mpdel-playlist.el ends here
