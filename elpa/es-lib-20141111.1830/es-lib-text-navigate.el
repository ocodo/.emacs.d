;;; es-lib-text-navigate.el --- Functions for moving around a buffer, and getting information about the surrounding text.
;;; Version: 0.4
;;; Author: sabof
;;; URL: https://github.com/sabof/es-lib

;;; Commentary:

;; The project is hosted at https://github.com/sabof/es-lib
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
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

;;; Code:

(require 'cl-lib)
(require 'es-lib-core-macros)

(defun es-point-between-pairs-p ()
  (let ((result nil))
    (cl-mapcar
     (lambda (character-pair)
       (if (and (characterp (char-before))
                (characterp (char-after))
                (equal (char-to-string (char-before))
                       (car character-pair))
                (equal (char-to-string (char-after))
                       (cdr character-pair)))
           (setq result t)))
     ;; Should query the syntax table for these
     '(("\"" . "\"")
       ("\'" . "\'")
       ("{" . "}")
       ("(" . ")")
       ("[" . "]")))
    result))

(defun es-mark-symbol-at-point ()
  (cl-destructuring-bind
      (start . end)
      (bounds-of-thing-at-point 'symbol)
    (es-set-region start end)))

(defun es-active-region-string ()
  (when (region-active-p)
    (buffer-substring
     (region-beginning)
     (region-end))))

(defun es-goto-previous-non-blank-line ()
  (save-match-data
    (beginning-of-line)
    (re-search-backward "[^ \n\t]" nil t)
    (beginning-of-line)))

(defun es-current-character-indentation ()
  "Like (current-indentation), but counts tabs as single characters."
  (save-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

(defun es-visible-end-of-line ()
  (save-excursion
    (end-of-line)
    (skip-syntax-backward
     " " (line-beginning-position))
    (point)))

(defun es-line-folded-p ()
  "Check whether the line contains a multiline folding."
  (not (equal (list (line-beginning-position)
                    (line-end-position))
              (list (es-total-line-beginning-position)
                    (es-total-line-end-position)))))

(defun es-set-region (point mark)
  (set-mark mark)
  (goto-char point)
  (activate-mark)
  (setq deactivate-mark nil))

(defun es-line-matches-p (regexp)
  (string-match-p
   regexp
   (buffer-substring
    (line-beginning-position)
    (line-end-position))))

(defun es-indentation-end-pos (&optional position)
  (save-excursion
    (when position (goto-char position))
    (back-to-indentation)
    (point)))

(defun es-line-empty-p ()
  (es-line-matches-p "^[ \t]*$"))

(defun es-line-visible-p ()
  (not (es-line-empty-p)))

(defun es-goto-line-prog (line-num)
  "Like goto-line, but simplified for programmatic use."
  (goto-char (point-min))
  (forward-line (1- line-num)))

(provide 'es-lib-text-navigate)
;; es-lib-text-navigate.el ends here
