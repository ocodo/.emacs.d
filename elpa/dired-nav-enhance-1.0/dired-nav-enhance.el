;;; dired-nav-enhance.el --- Enhanced navigation for dired buffers

;; Copyright (C) 2013 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; URL: http://github.com/nflath/dired-nav-enhance
;; Version: 1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; This package enhances a few of the navigational commands for dired.  Right
;;; now it only modifies beginning-of-buffer and end-of-buffer to go to the
;;; first/last line with files.

;;; Installation:

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;; (require 'dired-nav-enhance)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defun dired-nav-enhance-beginning-of-buffer ()
  """ Goes to the first actual file in a dired buffer."""
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 2))

(defun dired-nav-enhance-end-of-buffer ()
  """ Goes to the last actual file in a dired buffer."""
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-nav-enhance-beginning-of-buffer)
(define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-nav-enhance-end-of-buffer)

(provide `dired-nav-enhance)
;;; dired-nav-enhance.el ends here
