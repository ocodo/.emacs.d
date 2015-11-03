;;; commify.el --- Toggle grouping commas in numbers in buffer

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
;; Version: 1.0
;; Package-Version: 20151102.1248
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
;; commify/toggle works on floating or scientific numbers as well, but in only
;; ever affect the digits before the decimal point.
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
  "Character to use for decimal point."
  :type 'string
  :group 'commify)

(defcustom commify-group-size 3
  "Number of digits in each group."
  :type 'integer
  :group 'commify)

;; Main code:

(defun commify--commas (n  &optional group-char group-size)
  "For an integer N, return string version and insert GROUP-CHAR between groups of GROUP-SIZE digits."
  (unless group-char (setq group-char commify-group-char))
  (unless group-size (setq group-size commify-group-size))
  (let ((num nil)
        (grp-re nil)
        (rpl-str nil))
    (setq num (s-reverse (format "%s" n)))
    (setq grp-re (concat "[0-9]\\{" (format "%s" group-size) "\\}"))
    (setq rpl-str (concat "\\&" group-char))
    (setq num (replace-regexp-in-string grp-re rpl-str num))
    (s-reverse (replace-regexp-in-string ",$" "" num))))

;;;###autoload
(defun commify-toggle ()
  "Toggle insertion or deletion of grouping characters in the number around point."
  (interactive)
  (save-excursion
    (skip-chars-backward (concat commify-decimal-char commify-group-char "0-9e+-"))
    (when (looking-at "[-+]")
      (skip-chars-forward "-+"))
    (when (looking-at "[0-9]")
      (let ((beg-num (point))
            (num nil))
        (skip-chars-forward (concat commify-group-char "0-9"))
        (setq num (delete-and-extract-region beg-num (point)))
        (if (s-contains? commify-group-char num)
            (insert (s-replace-all `((,commify-group-char . "")) num))
          (insert (commify--commas (string-to-number num))))
        (goto-char beg-num))))
  (skip-chars-forward (concat commify-decimal-char commify-group-char "0-9e+-")))

(provide 'commify)
;;; commify.el ends here
