;;; text-sizer.el - Increase/decrease default font size
(defconst text-sizer-version "0.1")

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

(defvar text-size-step 10)

(setq text-size-step 10)

(defun default-text-size-increase ()
  "increase default face height by step"
  (interactive)
  (set-face-attribute 'default nil
		      :height (+ 
			       text-size-step
			       (face-attribute 'default :height)
			       )
		      )
  (w32-fullscreen-on)
  )

(defun default-text-size-decrease ()
  "increase default face height by step"
  (interactive)
  (set-face-attribute 'default nil
		      :height (- (face-attribute 'default :height) text-size-step
				 ) 
		      )
  (w32-fullscreen-on)
  )

