;;; squeeze-view.el - set and unset wide margins on the current buffer view.
(defconst squeeze-view-version "0.1.0")

;; Copyright (c)2010,2012 Jason Milkins. (by)(nc)(sa) Some rights reserved.
;; Author: Jasonm23 (concat "jason" "m23" "@" "gmail" ".com")

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
;; Intended to improve readability/focus reduce eye strain, by
;; horizontally narrowing the window viewable area, and removing other
;; windows. Similar to WriteRoom, and other reduced distraction modes
;; in various editors.
;;
;; `squeeze-view' - uses the default margin size. (can be set with
;; customizing `squeeze-view-margin')
;; `squeeze-view-size' - asks for a margin size.
;;

(defcustom squeeze-view-linum t
  "Specifies if line numbers are shown in a squeeze view")

(defcustom squeeze-view-fringe 550
  "Specifies the fringe width for squeeze-view")

(defcustom squeeze-view-margin 0
  "Specifies the standard margin size for a squeezed
  view. Defaults to 70.")

(defun squeeze-view () 
  "Squeeze the view, designed to improve readability/focus by
reducing the window viewable area, similar to WriteRoom"
  (interactive)
  (if (equal t squeeze-view-linum)
      (linum-mode 1)
    (linum-mode 0))
  (delete-other-windows)
  (set-window-fringes nil squeeze-view-fringe squeeze-view-fringe t)
  ;;(set-window-margins nil squeeze-view-margin squeeze-view-margin)
  )

(defun unsqueeze-view () 
  "Reset the fringe margins to 0 and turn linum-mode back on"
  (interactive)
  (linum-mode 1)
  (set-window-fringes nil 8 8 nil)
  (set-window-margins nil 0 0)
  )

(defun squeeze-view-size (margin)
  "Interactively squeeze the view using a supplied margin"
  (interactive "nSqueeze margins: ")
  (linum-mode 0)
  (delete-other-windows)
  (set-window-fringes nil squeeze-view-fringe squeeze-view-fringe squeeze-view-linum)
  (set-window-margins nil margin margin)
  )

(provide 'squeeze-view)

