;;; mpdel-song.el --- Display song information and control playback -*- lexical-binding: t; -*-

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

;; Executing `mpdel-song' opens a small buffer letting the user get
;; information and control the current song.

;;; Code:
(require 'libmpdel)
(require 'mpdel-core)


;;; Customization

(defgroup mpdel-song nil
  "Control MPD's current song."
  :group 'libmpdel)

(defcustom mpdel-song-small-increment "+2"
  "Time to seek to slowly move forward."
  :type 'string
  :group 'mpdel-song)

(defcustom mpdel-song-small-decrement "-2"
  "Time to seek to slowly move backward."
  :type 'string
  :group 'mpdel-song)

(defcustom mpdel-song-normal-increment "+10"
  "Time to seek to move forward."
  :type 'string
  :group 'mpdel-song)

(defcustom mpdel-song-normal-decrement "-10"
  "Time to seek to move backward."
  :type 'string
  :group 'mpdel-song)

(defcustom mpdel-song-large-increment "+30"
  "Time to seek to quickly move forward."
  :type 'string
  :group 'mpdel-song)

(defcustom mpdel-song-large-decrement "-30"
  "Time to seek to quickly move backward."
  :type 'string
  :group 'mpdel-song)


;;; Private variables

(defvar-local mpdel-song-song nil
  "Song displayed by current buffer.")

(defvar-local mpdel-song-current-song-p nil
  "Non-nil if buffer displays the current song.

When non-nil, the buffer keeps showing the current song, even
when the song changes.")


;;; `navigel' configuration

(navigel-method mpdel navigel-open ((song libmpdel-song) _target)
  (mpdel-song-open song))

(navigel-method mpdel navigel-entity-at-point (&context (major-mode mpdel-song-mode))
  mpdel-song-song)


;;; Helper functions

(defvar mpdel-song--timer nil
  "Store timer to refresh the seek buffer.")

(defun mpdel-song--start-timer ()
  "Start refresh timer."
  (unless mpdel-song--timer
    (setq mpdel-song--timer (run-at-time t 1 #'mpdel-song-refresh (current-buffer)))))

(defun mpdel-song--stop-timer ()
  "Stop refresh timer."
  (when mpdel-song--timer
    (cancel-timer mpdel-song--timer)
    (setq mpdel-song--timer nil)))

(defun mpdel-song--seek (time)
  "Seek TIME within current song and refresh current buffer."
  (libmpdel-playback-seek time))

(defun mpdel-song--display-play-state ()
  "Give information about current play state and control timer."
  (pcase (libmpdel-play-state)
    ('play (insert "Currently playing\n")
           (mpdel-song--start-timer))
    ('pause (insert "Currently paused\n")
            (mpdel-song--stop-timer))
    ('stop (insert "Currently stopped\n")
           (mpdel-song--stop-timer))))

(defun mpdel-song--display-play-time (data)
  "Give information about current play time in DATA."
  (insert
   (format "%s / %s"
           (libmpdel-time-to-string (cdr (assq 'elapsed data)))
           (libmpdel-time-to-string (cdr (assq 'duration data))))))

(defun mpdel-song--display-metadata ()
  "Give information about current song metadata."
  (let* ((song mpdel-song-song)
         (title (or (libmpdel-entity-name song) ""))
         (album (or (libmpdel-album-name song) ""))
         (artist (or (libmpdel-artist-name song) "")))
    (insert (format "Title: %s\nArtist: %s\nAlbum: %s\n"
                    title
                    artist
                    album))))

(defun mpdel-song--refresh-current-song (data buffer)
  "Write information about currently-played song in DATA to BUFFER.

DATA is an alist returned by MPD server as answer to 'status'.
In particular, it must contain key symbol `elapsed' and symbol
`duration'."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq mpdel-song-song (libmpdel-current-song))
      (mpdel-song--display-play-state)
      (mpdel-song--display-metadata)
      (mpdel-song--display-play-time data))))

(defun mpdel-song--refresh-non-current-song (buffer)
  "Write information about the song associated with BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (mpdel-song--display-metadata))))


;;; Public interface

(defun mpdel-song-refresh (&optional buffer)
  "Refresh MPDEL seek BUFFER, current buffer if nil."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (if mpdel-song-current-song-p
        (libmpdel-send-command "status" (lambda (data) (mpdel-song--refresh-current-song data buffer)))
      (mpdel-song--refresh-non-current-song (current-buffer)))))

;;;###autoload
(defun mpdel-song-open (&optional song)
  "Open a buffer to display information about SONG.
If SONG is nil, use current song instead.

When SONG is nil, the buffer updates itself to keep showing
latest song.  Additionally, the buffer lets the user control
playback."
  (interactive)
  (let* ((current-song-p (not song))
         (buffer (get-buffer-create (if current-song-p
                                        "*MPDEL Current Song*"
                                      (format "*MPDEL Song: %s*" (libmpdel-entity-name song)))))
         (refresh-fn (lambda () (mpdel-song-refresh buffer)))
         (song (or song (libmpdel-current-song))))
    (with-current-buffer buffer
      (mpdel-song-mode)
      (setq mpdel-song-song song)
      (setq mpdel-song-current-song-p current-song-p)
      (mpdel-song-refresh buffer)
      (when current-song-p
        (add-hook 'libmpdel-player-changed-hook refresh-fn)
        (add-hook 'kill-buffer-hook #'mpdel-song--stop-timer nil t)
        (add-hook 'kill-buffer-hook (lambda () (remove-hook 'libmpdel-player-changed-hook refresh-fn)) nil t))
      (pop-to-buffer (current-buffer)))))

(define-key mpdel-core-map (kbd "v") #'mpdel-song-open)

(defun mpdel-song-play ()
  "Start playing the song of the current buffer."
  (interactive)
  (libmpdel-play-song mpdel-song-song))

(defun mpdel-song-quit-window ()
  "Quit window and kill its buffer.

Killing the buffer instead of just burrying it is important when
displaying current song: this is because the buffer keeps
refreshing itself to display playback position."
  (interactive)
  (quit-window t))

(defun mpdel-song-small-increment ()
  "Move forward by value of variable `mpdel-song-small-increment'."
  (interactive)
  (mpdel-song--seek mpdel-song-small-increment))

(define-key mpdel-core-map (kbd "F") #'mpdel-song-small-increment)

(defun mpdel-song-normal-increment ()
  "Move forward by value of variable `mpdel-song-normal-increment'."
  (interactive)
  (mpdel-song--seek mpdel-song-normal-increment))

(define-key mpdel-core-map (kbd "f") #'mpdel-song-normal-increment)

(defun mpdel-song-large-increment ()
  "Move forward by value of variable `mpdel-song-large-increment'."
  (interactive)
  (mpdel-song--seek mpdel-song-large-increment))

(define-key mpdel-core-map (kbd "M-f") #'mpdel-song-large-increment)

(defun mpdel-song-small-decrement ()
  "Move backward by value of variable `mpdel-song-small-decrement'."
  (interactive)
  (mpdel-song--seek mpdel-song-small-decrement))

(define-key mpdel-core-map (kbd "B") #'mpdel-song-small-decrement)

(defun mpdel-song-normal-decrement ()
  "Move backward by value of variable `mpdel-song-normal-decrement'."
  (interactive)
  (mpdel-song--seek mpdel-song-normal-decrement))

(define-key mpdel-core-map (kbd "b") #'mpdel-song-normal-decrement)

(defun mpdel-song-large-decrement ()
  "Move backward by value of variable `mpdel-song-large-decrement'."
  (interactive)
  (mpdel-song--seek mpdel-song-large-decrement))

(define-key mpdel-core-map (kbd "M-b") #'mpdel-song-large-decrement)


;;; Major-mode

(defvar mpdel-song-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map
     (make-composed-keymap mpdel-core-map special-mode-map))
    (define-key map (kbd "g") #'mpdel-song-refresh)
    (define-key map (kbd "p") #'mpdel-song-play)
    (define-key map (kbd "q") #'mpdel-song-quit-window)
    map)
  "Keybindgs for `mpdel-song-mode'.")

(define-derived-mode mpdel-song-mode special-mode "MPDEL song"
  "Guide the user to seek inside current song."
  (setq-local navigel-app 'mpdel))

(provide 'mpdel-song)
;;; mpdel-song.el ends here
