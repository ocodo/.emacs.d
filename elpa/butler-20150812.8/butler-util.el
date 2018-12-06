;; -*- lexical-binding: t -*-
;;; butler-util.el

;; Copyright © 2012-2013 Ashton Kemerling
;;
;; Author: Ashton Kemerling <ashtonkemerling@gmail.com>
;; URL: http://www.github.com/AshtonKem/Butler.git

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utilities for Butler


(defface butler-failure
  '((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "red"))
  "Face for failed Butler jobs."
  :group 'butler)

(defface butler-unstable
  '((((class color) (background light))
     :foreground "yellow")
    (((class color) (background dark))
     :foreground "yellow"))
  "Face for unstable Butler jobs."
  :group 'butler)


(defface butler-success
  '((((class color) (background light))
     :foreground "green")
    (((class color) (background dark))
     :foreground "green"))
  "Face for successful Butler jobs."
  :group 'butler)

(defface butler-aborted
  '((((class color) (background light))
     :foreground "gray")
    (((class color) (background dark))
     :foreground "gray"))
  "Face for aborted Butler jobs."
  :group 'butler)

(defface butler-disabled
  '((((class color) (background light))
     :foreground "black")
    (((class color) (background dark))
     :foreground "black"))
  "Face for disabled Butler jobs."
  :group 'butler)

(defun colorize-dot (color)
  (cond
   ((string= color  "red")
    (propertize "●" 'face 'butler-failure))
   ((string= color "yellow")
    (propertize "●" 'face 'butler-unstable))
   ((string= color  "blue")
    (propertize "●" 'face 'butler-success))
   ((string= color  "grey")
    (propertize "●" 'face 'butler-aborted))
   ((string= color  "aborted")
    (propertize "●" 'face 'butler-aborted))
   ((string= color "disabled")
    (propertize "●" 'face 'butler-disabled))
   ((string= (subseq color -6) "_anime")
    (colorize-dot (subseq color 0 -6)))
   (t (concat "Unknown: " "'" color "' "))))


;;; Code:
(provide 'butler-util)
;;; butler-util.el ends here
