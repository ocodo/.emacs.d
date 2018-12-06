;;; evil-textobj-column.el --- Provides column text objects.

;; Author: Fox Kiester <noct@openmailbox.org>
;; URL: https://github.com/noctuid/evil-textobj-column
;; Package-Version: 20170905.1905
;; Created: October 7, 2015
;; Keywords: evil, column, text-object
;; Package-Requires: ((names "0.5") (emacs "24") (evil "0"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package is a port of the vim textobj-word-column plugin. It provides a
;; a means to create text objects for acting on a column whose width is
;; determined by motions. The default text objects are for columns having the
;; width of an evil word or WORD (big word). This can be a convenient way to
;; start a visual block selection, for example.

;; For more information see the README in the github repo.

;;; Code:
(require 'evil)
(require 'rx) ; would be autoloaded

;; `define-namespace' is autoloaded, so there's no need to require
;; `names'. However, requiring it here means it will also work for
;; people who don't install through package.el.
(eval-when-compile (require 'names))

(define-namespace evil-textobj-column-
:package evil-textobj-column
:group evil

(defun -get-basis (backward-motion forward-motion)
  "Move to the start of a column delimiter and return type of the column start.
This function determines the left boundary of the column and the anchor position
for searching for the other column boundaries. BACKWARD-MOTION should be a
function that moves the point to the column start (e.g. to the start of a word
for a column having the width of a word). If the point is already at the start
of a word, calling BACKWARD-MOTION followed by FORWARD-MOTION should not change
the location of the point.

Return nil if no suitable column basis is found."
  (if (and (looking-at (rx (1+ whitespace)))
           (looking-back (rx bol (0+ whitespace))))
      nil
    ;; move to start of column (as determined by motion functions)
    ;; if not already there
    (when (or (looking-at (rx (0+ whitespace) eol))
              (let ((current-pos (point)))
                (save-excursion
                  (funcall backward-motion)
                  (funcall forward-motion)
                  (not (= current-pos (point))))))
      (funcall backward-motion))
    (cond ((looking-back (rx bol))
           'bol)
          ;; ((looking-back (rx whitespace))
          ;;  'whitespace)
          (t
           'char))))

(defun -continue-p
    (behind-char-type initial-col backward-motion forward-motion)
  "Determine whether current position still meets the column start requirements
based on BEHIND-CHAR-TYPE. INITIAL-COL, BACKWARD-MOTION, and FORWARD-MOTION
are used when the BEHIND-CHAR-TYPE is 'char."
  (cond ;; ((eq behind-char-type 'whitespace)
   ;;  (and (looking-back (rx whitespace))
   ;;       ;; (not (looking-back (rx bol)))
   ;;       ;; (not (looking-at (rx eol)))
   ;;       (looking-at (rx any))))
   ((eq behind-char-type 'bol)
    (not (looking-at (rx (0+ whitespace) eol))))
   ((eq behind-char-type 'char)
    (funcall backward-motion)
    (funcall forward-motion)
    (= initial-col (current-column)))))

(defun -get-bounds
    (line-motion
     behind-char-type
     forward-begin-motion backward-begin-motion forward-end-motion
     &optional max-right-col)
  "Helper function to get the bounds in a direction.
BEHIND-CHAR-TYPE is passed on to evil-textobj-column--continue-p.
LINE-MOTION is a function to be called (to move up or down).
FORWARD-BEGIN-MOTION, BACKWARD-BEGIN-MOTION, and FORWARD-END-MOTION are
used when determining the column boundaries.
MAX-RIGHT-COL is specified when a previous maximum column position (determined
by the end the word at each line) has been found.

Return a list of of buffer positions designating the column bounds where the
first is the upper left bound and the second is the lower right bound."
  (let ((initial-col (current-column))
        (max-right-col (or max-right-col 0))
        max-pos)
    (save-excursion
      (while
          (progn
            (let* ((current-pos (point))
                   (current-line (line-number-at-pos))
                   (right-col
                    (if (or
                         ;; 1 char column followed by space
                         (looking-at (rx any space))
                         ;; other 1 char columns
                         (save-excursion
                           (funcall forward-begin-motion)
                           (= (point) (1+ current-pos))))
                        (current-column)
                      (save-excursion
                        (funcall forward-end-motion)
                        (if (= current-line (line-number-at-pos))
                            (current-column)
                          ;; handle motion jumping to next line
                          ;; (and 1 char column at eol)
                          (goto-line current-line)
                          ;; (end-of-line)
                          (evil-last-non-blank)
                          (current-column))))))
              (setq max-pos current-pos)
              (when (> right-col max-right-col)
                (setq max-right-col right-col)))
            ;; continue if
            ;; 1. can still move up/down lines (line motion succeeds)
            ;; 2. line motion maintains column position
            ;; 3. continuation check returns t
            (and (condition-case err
                     (progn (funcall line-motion) t)
                   ('error nil))
                 ;; workaround due to current-column giving 1 when
                 ;; it should give 0
                 (let ((new-column (if (and (looking-at (rx eol))
                                            (looking-back (rx bol)))
                                       0
                                     (current-column))))
                   (= initial-col new-column))
                 (-continue-p behind-char-type initial-col
                              backward-begin-motion forward-begin-motion)))))
    (list max-pos max-right-col)))

(defun -get-top-bounds
    (behind-char-type
     forward-begin-motion backward-begin-motion forward-end-motion
     &optional max-right-col)
  "Find the upper bounds of a column."
  (-get-bounds #'evil-previous-line
               behind-char-type
               forward-begin-motion backward-begin-motion forward-end-motion
               max-right-col))

(defun -get-bottom-bounds
    (behind-char-type
     forward-begin-motion backward-begin-motion forward-end-motion
     &optional max-right-col)
  "Find the lower bounds of a column."
  (-get-bounds #'evil-next-line
               behind-char-type
               forward-begin-motion backward-begin-motion forward-end-motion
               max-right-col))

(defun -create-range
    (forward-begin-motion backward-begin-motion forward-end-motion)
  "Return a column range based on the point.
FORWARD-BEGIN-MOTION, BACKWARD-BEGIN-MOTION, and FORWARD-END-MOTION are used
when determining the column boundaries."
  ;; HACK? this is necessary so c and d don't alter evil-next/previous-line:
  (evil-normal-state)
  (save-excursion
    (let* ((behind-char-type
            ;; also ensures at column start
            (-get-basis backward-begin-motion forward-begin-motion))
           (top-bounds (when behind-char-type
                         (-get-top-bounds behind-char-type
                                          forward-begin-motion
                                          backward-begin-motion
                                          forward-end-motion)))
           (top-left-pos (car top-bounds))
           (max-right-col (cadr top-bounds))
           (bottom-bounds (when top-bounds
                            (-get-bottom-bounds behind-char-type
                                                forward-begin-motion
                                                backward-begin-motion
                                                forward-end-motion
                                                max-right-col)))
           (bottom-right-pos (when bottom-bounds
                               (goto-char (car bottom-bounds))
                               ;; case where beyond eol?
                               (evil-goto-column (1+ (cadr bottom-bounds)))
                               (point))))
      (when (and top-left-pos bottom-right-pos)
        ;; note:
        ;; 'rectangle will make selection one char to the left (so 1+ above)
        ;; 'block gives more incorrect selection
        (evil-range top-left-pos bottom-right-pos 'rectangle)))))

;; end of namespace
)

;;;###autoload (autoload 'evil-textobj-column-word "evil-textobj-column" nil t)
(evil-define-text-object evil-textobj-column-word
  (count &optional beg end type)
  "Select a word column.
COUNT, BEG, END, and TYPE have no effect. This text object cannot take a count."
  (evil-textobj-column--create-range #'evil-forward-word-begin
                                     #'evil-backward-word-begin
                                     #'evil-forward-word-end))

;;;###autoload (autoload 'evil-textobj-column-WORD "evil-textobj-column" nil t)
(evil-define-text-object evil-textobj-column-WORD
  (count &optional beg end type)
  "Select a WORD (big word) column.
COUNT, BEG, END, and TYPE have no effect. This text object cannot take a count."
  (evil-textobj-column--create-range #'evil-forward-WORD-begin
                                     #'evil-backward-WORD-begin
                                     #'evil-forward-WORD-end))

(provide 'evil-textobj-column)
;;; evil-textobj-column.el ends here
