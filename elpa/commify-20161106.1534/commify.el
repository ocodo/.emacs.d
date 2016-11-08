;;; commify.el --- Toggle grouping commas in numbers

;; Copyright (C) 2015 Daniel E. Doherty

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Daniel E. Doherty <ded-commify@ddoherty.net>
;; Version: 1.2.1
;; Package-Version: 20161106.1534
;; Package-Requires: ((s "1.9.0"))
;; Keywords: convenience, editing, numbers, grouping, commas
;; URL: https://github.com/ddoherty03/commify

;;; Commentary:

;; This package provides a simple command to toggle a number under the cursor
;; between having grouped digits and not.  For example, if the buffer is as
;; shown with the cursor at the '*':
;;
;; Travel expense is 4654254654*
;;
;; invoking commify-toggle will change the buffer to:
;;
;; Travel expense is 4,654,254,654*
;;
;; Calling commify-toggle again removes the commas.  The cursor can also be
;; anywhere in the number or immediately before or after the number.
;; commify-toggle works on floating or scientific numbers as well, but it only
;; ever affects the digits before the decimal point.  Afterwards, the cursor
;; will be placed immediately after the affected number.
;;
;; You can configure these variables:
;;   - commify-group-char (default ",") to the char used for grouping
;;   - commify-group-size (default 3) to number of digits per group
;;   - commify-decimal-char (default ".") to the char used as a decimal point.
;;
;; Bind the main function to a convenient key in you init.el file:
;;
;;    (key-chord-define-global ",," 'commify-toggle)

;;; Code:

(require 's)

;; Customize options.

(defgroup commify nil
  "Toggle insertion of commas in numbers in buffer."
  :group 'convenience)

(defcustom commify-group-char ","
  "Character to use for separating groups of digits."
  :type 'string
  :group 'commify)

(defcustom commify-decimal-char "."
  "Character recognized as the decimal point."
  :type 'string
  :group 'commify)

(defcustom commify-group-size 3
  "Number of digits in each group."
  :type 'integer
  :group 'commify)

;; Utility functions

(defun commify--number-chars ()
  "Characters that can appear in a number."
  (concat commify-decimal-char commify-group-char "0-9eE+-"))

(defun commify--number-re ()
  "Regular expression of a valid number string.

A valid number has a mandatory whole number part, which it
captures as the second group.  The number may contain the
`commify-group-char' in the whole number part and uses
`commify-decimal-char' as the separator between the whole and
fractional part of the number.  A leading sign, `+' or `-' is
optional, as is a trailing exponent introduced by `e' or `E'.

The matched sub-parts are:
  1. the optional sign,
  2. the whole number part,
  3. the optional fractional part, including the decimal point, and
  4. the optional exponent part."
  (let ((sign "\\([-+]\\)?")
        (whole (concat "\\([0-9" (regexp-quote commify-group-char) "]+\\)"))
        (frac (concat "\\(" (regexp-quote commify-decimal-char) "[0-9]+\\)?"))
        (exp "\\([eE][0-9]+\\)?"))
    (concat sign whole frac exp)))

(defun commify--exception-p ()
  "Should the current buffer position be excluded from commify?"
  (or (commify--on-date-p)
      (commify--on-indentifier-p)
      (commify--on-zero-filled-p)
      (commify--on-hex-p)))

(defun commify--on-date-p ()
  "Is text to the right of the cursor part of a date?"
  (or (looking-at-p "\\(?:19\\|20\\)[[:digit:]]\\{2\\}[-/]")
      (save-excursion
        (backward-char)
        (looking-at-p "[-/]\\(?:19\\|20\\)[[:digit:]]\\{2\\}"))))

(defun commify--on-indentifier-p ()
  "Is text to the right of the cursor part of an identifier?"
  (save-excursion
    (backward-char)
    (looking-at-p "\\s_")))

(defun commify--on-zero-filled-p ()
  "Does text to the right of the cursor start with zero?"
  (looking-at-p "0"))

(defun commify--on-hex-p ()
  "Is text to the right of the cursor part of a hexadecimal number?"
  (save-excursion
    (backward-char 2)
    (looking-at-p "0x")))

(defun commify--commas (n  &optional group-char group-size)
  "For an integer string N, insert GROUP-CHAR between groups of GROUP-SIZE digits."
  (unless group-char (setq group-char commify-group-char))
  (unless group-size (setq group-size commify-group-size))
  (let ((num nil)
        (grp-re nil)
        (rpl-str nil))
    ;; reverse the string so we can insert the commas left-to-right
    (setq num (s-reverse n))
    ;; form the re to look for groups of group-size digits, e.g. "[0-9]\{3\}"
    (setq grp-re (concat "[0-9]\\{" (format "%s" group-size) "\\}"))
    ;; form the replacement, e.g., "\&,"
    (setq rpl-str (concat "\\&" group-char))
    ;; do the replacement in the reversed string
    (setq num (replace-regexp-in-string grp-re rpl-str num))
    ;; now chop off any trailing group-char and re-reverse the string.
    (s-reverse (s-chop-suffix group-char num))))

(defun commify--uncommas (n &optional group-char)
  "For an integer string N, remove all instances of GROUP-CHAR."
  (unless group-char (setq group-char commify-group-char))
  (s-replace-all `((,commify-group-char . "")) n))

;; Commands

;;;###autoload
(defun commify-toggle-at-point ()
  "Toggle insertion or deletion of grouping characters in the number around point."
  (interactive)
  (save-excursion
    ;; find the beginning of the number the cursor is in or after.
    (skip-chars-backward (commify--number-chars))
    (when (looking-at (commify--number-re))
      (unless (commify--exception-p)
        (let ((num (match-string 2))
              (num-beg (match-beginning 2))
              (num-end (match-end 2)))
          (delete-region num-beg num-end)
          ;; We may have point at a +/- sign, skip over
          (skip-chars-forward "+-")
          (if (s-contains? commify-group-char num)
              (insert (commify--uncommas num))
            (insert (commify--commas num)))))))
  ;; move cursor to the end of the number.
  (skip-chars-forward (commify--number-chars)))

;;;###autoload
(defun commify-toggle-on-region (beg end)
  "Toggle insertion or deletion of numeric grouping characters.
Do so for all numbers in the region between BEG and END."
  (interactive "r")
  (let (deactivate-mark)
    (save-excursion
      (goto-char beg)
      (while (search-forward-regexp (commify--number-re) (+ 30 end) t)
        (commify-toggle-at-point))
      (search-forward-regexp (commify--number-re) (+ 30 (point)) t))))

;;;###autoload
(defun commify-toggle ()
  "Toggle commas at point or on the region from BEG to END."
  (interactive)
  (if (use-region-p)
      (commify-toggle-on-region (region-beginning) (region-end))
    (commify-toggle-at-point)))


(provide 'commify)
;;; commify.el ends here

