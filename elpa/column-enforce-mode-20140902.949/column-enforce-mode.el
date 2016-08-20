;;; column-enforce-mode.el --- Highlight text that extends beyond a  column
;;
;; Filename: column-enforce-mode.el
;; Description:
;; Author: Jordon Biondo
;; Maintainer:
;; Created: Fri Oct 11 12:14:25 2013 (-0400)
;; Version: 1.0.4
;; Package-Version: 20140902.949
;; Package-Requires: ()
;; Last-Updated: Sun Dec  8 20:23:51 2013 (-0500)
;;           By: Jordon Biondo
;;     Update #: 13
;; URL: www.github.com/jordonbiondo/column-enforce-mode
;; Keywords: 
;; Compatibility: >= Emacs 22.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Highlight text that extends beyond a certain column (80 column rule)
;;
;;  By default, text after 80 columns will be highlighted in red
;;
;;  To customize behavior, see `column-enforce-column' and `column-enforce-face'
;;
;;  To enable: M-x column-enforce-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; don't judge me
(require 'cl)


(defgroup column-enforce nil
  "Highlight text that extends beyond a certain column (80 column rule)"
  :group 'convenience)


(defcustom column-enforce-column 80
  "Highlight text extending beyond this many columns \
when using function `column-enforce-mode'."
  :type 'integer
  :group 'column-enforce)

(defcustom column-enforce-comments t
  "Non-nil means to mark comments that exceed the column limit."
  :type 'boolean
  :group 'column-enforce)


(defun column-enforce-get-column ()
  "Gets the value of variable `column-enforce-column' or if nil, \
the value of variable `fill-column', or if nil, 80."
  (or column-enforce-column fill-column 80))


(defface column-enforce-face
  `((t (:inherit font-lock-warning-face :underline t)))
  "Face to be used to highlight lines confilicting the the current column rule"
  :group 'column-enforce)


(defvar column-enforce-face 'column-enforce-face
  "Face to be used to highlight lines confilicting the the current column rule")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun column-enforce-n (n)
  "Turn on `column-enforce-mode' with warnings at column N.
N can be given as a prefix argument.

Ex:
  C-u 70 M-x column-enforce-n <enter>
  sets up `column-enforce-mode' to mark \
text that extends beyond 70 columns."
  (interactive "P")
  (let ((n (if (and n (integerp n)) n column-enforce-column)))
    (setq column-enforce-mode-line-string
	  (column-enforce-make-mode-line-string n))
    (column-enforce-mode -1)
    (set (make-local-variable 'column-enforce-column) n)
    (column-enforce-mode t)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predefined column rules
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defmacro make-column-rule(n)
  "Create an interactive function to enforce an N-column-rule."
  `(let ((__n ,n))
     (assert (integerp __n) nil "Wrong type argument")
     (eval `(defun ,(intern (format "%d-column-rule" __n)) ()
	      ,(format "Visually mark text after %d columns." __n)
	      (interactive)
	      (if (and column-enforce-mode (= ,__n (column-enforce-get-column)))
		  (column-enforce-mode -1)
		(column-enforce-n ,__n))))))


(make-column-rule 100)
(make-column-rule 90)
(make-column-rule 80)
(make-column-rule 70)
(make-column-rule 60)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun column-enforce-make-mode-line-string(rule)
  "Returns the string to display in the mode line"
  (format " %dcol" rule))


(defvar column-enforce-mode-line-string
  (column-enforce-make-mode-line-string (column-enforce-get-column))
  "The current string for the mode line.")


;;;###autoload
(define-minor-mode column-enforce-mode
  "Minor mode for highlighting text that extends beyond a certain column.

Variable `column-enforce-column' decides which column to start warning at.
 Default is 80
Variable `column-enforce-face' decides how to display the warnings"
  :init-value nil
  :lighter column-enforce-mode-line-string
  :keymap nil
  :global nil
  (setq column-enforce-mode-line-string
	(column-enforce-make-mode-line-string (column-enforce-get-column)))
  (if column-enforce-mode
      ;; use add-hook so we can append it, (force it to run last)
      (progn
	(jit-lock-register 'column-enforce-warn-on-region t)
	(column-enforce-warn-on-region (point-min) (point-max)))
    (progn
      (dolist (ov (column-enforce-get-cem-overlays-in (point-min) (point-max)))
	(delete-overlay ov))
      (jit-lock-unregister 'column-enforce-warn-on-region))))

(defun column-enforce-mode-toggle-if-applicable ()
  (if column-enforce-mode
      (column-enforce-mode -1)
    (when (derived-mode-p  'prog-mode)
      (column-enforce-mode t))))

(define-global-minor-mode global-column-enforce-mode column-enforce-mode
  column-enforce-mode-toggle-if-applicable)

;; internal
(defun column-enforce-get-cem-overlays-in (beg end)
  "Get all overlays between BEG and END that have a 'is-cem-ov property."
  (remove-if-not (lambda (ov) (overlay-get ov 'is-cem-ov))
		 (overlays-in beg end)))


(defun column-enforce-warn-on-region (beg end)
  "Jit lock function for function `column-enforce-mode' that will \
mark text that extends beyond `column-enforce-column' with the \
`column-enforce-face' using overlays between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let ((cem-ovs (column-enforce-get-cem-overlays-in
		   (point-at-bol) (point-at-eol))))
	(dolist (ov cem-ovs) (delete-overlay ov))
	(move-to-column (column-enforce-get-column))
	(if (and (not (= (point) (point-at-eol)))
                 (or column-enforce-comments
                     (not (equal (syntax-ppss-context (syntax-ppss (point)))
                                 'comment))))
	  (let ((new-ov (make-overlay (point)
				      (point-at-eol)
				      nil t t)))
	    (overlay-put new-ov 'face 'column-enforce-face)
	    (overlay-put new-ov 'is-cem-ov t)))
	(forward-line 1)))))


(provide 'column-enforce-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; column-enforce-mode.el ends here
