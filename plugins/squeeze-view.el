;;; squeeze-view.el - set and unset wide margins on the current buffer view.
(defconst squeeze-view-version "0.0.1")

;; Copyright (c)2010 Jasonm23. (by)(nc)(sa) Some rights reserved.
;; Author: Jasonm23 <jasonm23@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA
;;
;; squeeze-view.el
;;
;; Give the current buffer left and right margins.
;; squeeze-view-wide for a wide screen
;; squeeze-view-narrow for a portrait mode screen
;;

(defun squeeze-view-wide () ""
  (interactive)
  (linum-mode 0)
  (sqz-view 40)
)

(defun squeeze-view-narrow () ""
  (interactive)
  (linum-mode 0)
  (sqz-view 10)
)

(defun unsqueeze-view () ""
  (interactive)
  (linum-mode 1)
  (sqz-view 0)
)

(defun sqz-view (m)
  (set-variable 'left-margin-width m nil)
  (set-variable 'right-margin-width m nil)
  (set-window-margins (selected-window)
		      left-margin-width
		      right-margin-width)
  )

(provide 'squeeze-view)

