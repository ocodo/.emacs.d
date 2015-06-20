;;; es-lib-total-line.el --- A family of functions to naturally move with folded code
;;; Version: 0.4
;;; Author: sabof
;;; URL: https://github.com/sabof/es-lib

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; The project is hosted at https://github.com/sabof/es-lib

;;; Code:

(require 'cl-lib)
(require 'es-lib-core-macros)

(defun es-total-line-end-position (&optional pos)
  "Kind of like \(max \(end-of-line\) \(end-of-visual-line\)\)."
  (save-excursion
    (when pos (goto-char pos))
    (es-while-point-moving
     (goto-char (max (save-excursion
                       (vertical-motion 1)
                       (1- (point)))
                     (line-end-position))))
    (point)))

(defun es-total-line-beginning-position (&optional pos)
  "Kind of like \(min \(beginning-of-line\) \(beginning-of-visual-line\)\)."
  (save-excursion
    (when pos (goto-char pos))
    (es-while-point-moving
     (beginning-of-line)
     (beginning-of-visual-line))
    (point)))

(defun es-total-forward-line (arg)
  (ignore-errors
    (cond ( (cl-plusp arg)
            (cl-dotimes (ignore arg)
              (goto-char (es-total-line-end-position))
              (forward-char)))
          ( t
            (goto-char (es-total-line-beginning-position))
            (cl-dotimes (ignore (* -1 arg))
              (backward-char)
              (goto-char (es-total-line-beginning-position))
              )))))

(defun es-total-line-end ()
  "Interactive version of `es-total-line-end-position'."
  (interactive)
  (if truncate-lines
      (end-of-visual-line)
      (goto-char (es-total-line-end-position))
      ))
(put 'es-total-line-end 'CUA 'move)

(defun es-total-line-beginning ()
  "Interactive version of `es-total-line-beginning-position'."
  (interactive)
  (let* (( indentation-end
           (save-excursion
             (goto-char (es-total-line-beginning-position))
             (+ (line-beginning-position)
                (es-current-character-indentation)))))
    (if (= (point) indentation-end)
        (goto-char (es-total-line-beginning-position))
        (goto-char indentation-end))))
(put 'es-total-line-beginning 'CUA 'move)

(provide 'es-lib-total-line)

;;; es-lib-total-line.el ends here
