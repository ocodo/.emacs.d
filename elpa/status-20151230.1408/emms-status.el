;;; emms-status.el --- status area support for EMMS

;; Copyright (C) 2007, 2012, 2014 Tom Tromey <tromey@redhat.com>

;; Author: Tom Tromey <tromey@redhat.com>
;; Created: 3 June 2007
;; Version: 0.2
;; Keywords: multimedia

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This makes a status icon for controlling EMMS.  It relies on
;; status.el.

(require 'status)

(defvar emms-status-icon-directory
  (file-name-directory load-file-name))

(defvar emms-status-icon nil)

(defun emms-status-update-icon ()
  ;; Set the icon.
  (status-set-icon emms-status-icon
		   (expand-file-name
			   (if (or emms-player-paused-p
				   (not emms-player-playing-p))
			       "player_play.png"
			     "player_pause.png")
			   emms-status-icon-directory))
  ;; Set the tooltip.
  (status-set-tooltip emms-status-icon
		      (if emms-player-playing-p
			  (concat "EMMS: "
				  (emms-track-description
				   (emms-playlist-current-selected-track)))
			"EMMS - Emacs Music Player")))

(defun emms-status-start ()
  (unless emms-status-icon
    (setq emms-status-icon (status-new))
    (emms-status-update-icon)
    (status-set-visible emms-status-icon t)
    ;; Click to pause or play.
    (status-set-click-callback emms-status-icon 'emms-pause)
    ;; Have EMMS tell us when something happens.
    ;; FIXME: Could display new song in notification area.
    (add-hook 'emms-player-stopped-hook 'emms-status-update-icon)
    (add-hook 'emms-player-started-hook 'emms-status-update-icon)
    (add-hook 'emms-player-paused-hook 'emms-status-update-icon)))

(provide 'emms-status)

;;; emms-status.el ends here
