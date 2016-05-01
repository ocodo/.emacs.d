;;; describe-number.el --- Describe arbitrarily large number at point.

;; Copyright (C) 2015  Morten Slot Kristensen

;; Author: Morten Slot Kristensen <msk AT nullpointer DOT dk>
;; Keywords: describe value help
;; Package-Version: 20151031.1755
;; URL: https://github.com/netromdk/describe-number
;; Version: 0.3.1
;; Package-Requires: ((yabin "1.1"))

;; This program is free software; you can redistribute it and/or modify it under the terms of the
;; GNU General Public License as published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;; even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License along with this program.  If
;; not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Describe arbitrarily large number value at point/region. If value is a number then
;; binary/octal/decimal/hexadecimal/character values and conversions are shown. For strings each
;; character is processed in the same way.
;;
;; Use `describe-number-at-point' on point/region or `describe-number' to input value manually.
;;
;; Might be preferable to bind `describe-number-at-point' to some key:
;;   (global-set-key (kbd "M-?") 'describe-number-at-point)

;;; Code:

(require 'yabin)

(defun describe-number--get-bin-value (value)
  "Retrieve binary VALUE from string with optional prefixes 'b', '0b', and '#b'."
  (if (string-match "\\`[ ]*\\(?:[0#]?b\\)?\\([0-1]+\\)[ ]*\\'" value)
      (match-string 1 value)
    nil))

(defun describe-number--get-oct-value (value)
  "Retrieve octal VALUE from string with optional prefixes 'o', '0o', and '#o'."
  (if (string-match "\\`[ ]*\\(?:[0#]?o\\)?\\([0-7]+\\)[ ]*\\'" value)
      (match-string 1 value)
    nil))

(defun describe-number--get-dec-value (value)
  "Retrieve decimal VALUE from string."
  (if (string-match "\\`[ ]*\\([0-9]+\\)[ ]*\\'" value)
      (match-string 1 value)
    nil))

(defun describe-number--get-hex-value (value)
  "Retrieve hexadecimal VALUE from string with optional prefixes 'x', '0x', and '#x'."
  (if (string-match "\\`[ ]*\\(?:[0#]?x\\)?\\([0-9a-f]+\\)[ ]*\\'" value)
      (match-string 1 value)
    nil))

(defun describe-number--is-number-p (value)
  "Check if VALUE is binary, octal, decimal, or hexadecimal."
  (or (describe-number--get-bin-value value)
      (describe-number--get-oct-value value)
      (describe-number--get-dec-value value)
      (describe-number--get-hex-value value)))

(defun describe-number--is-char-value-p (value)
  "Check if VALUE is representable as a character."
  (yabin-less-than-equal value (yabin-radix (max-char) 10)))

(defun describe-number--get-char-value (value)
  "Retrieve character value of VALUE if representable, otherwise nil."
  (if (describe-number--is-char-value-p value)
      (format "%c" (string-to-number (yabin-radix value 10)))
    nil))

(defun describe-number--char-or-empty-string (value)
  "Retrieve character representation of VALUE or empty string if VALUE is too large."
  (let ((ch (describe-number--get-char-value value)))
    (if ch (format " '%s'" ch) "")))

(defun describe-number--describe (value)
  "Return number description of VALUE as a string."
  (let* ((bin (describe-number--get-bin-value value))
         (oct (describe-number--get-oct-value value))
         (dec (describe-number--get-dec-value value))
         (hex (describe-number--get-hex-value value))
         (msg ""))
    (if dec
        (setq msg
              (format "%s [%s #o%s #x%s%s]"
                      msg
                      (yabin-format "%d" dec)
                      (yabin-format "%o" dec)
                      (yabin-format "%X" dec)
                      (describe-number--char-or-empty-string dec))))
    (if bin
        (let ((bin (concat "2#" bin)))
          (setq msg
                (format "%s [b->d=%s #o%s #x%s%s]"
                        msg
                        (yabin-format "%d" bin)
                        (yabin-format "%o" bin)
                        (yabin-format "%X" bin)
                        (describe-number--char-or-empty-string bin)))))
    (if oct
        (let ((oct (concat "8#" oct)))
          (setq msg
                (format "%s [o->d=%s #x%s%s]"
                        msg
                        (yabin-format "%d" oct)
                        (yabin-format "%X" oct)
                        (describe-number--char-or-empty-string oct)))))
    (if hex
        (let ((hex (concat "16#" hex)))
          (setq msg
                (format "%s [x->d=%s #o%s%s]"
                        msg
                        (yabin-format "%d" hex)
                        (yabin-format "%o" hex)
                        (describe-number--char-or-empty-string hex)))))
    msg))

;;;###autoload
(defun describe-number (value)
  "Describe information about VALUE, which can be a number or a string."
  (interactive (list (read-string "Value: ")))
  (if (not (zerop (length value)))
      (let ((msg ""))
        (if (not (describe-number--is-number-p value)) ;; If not bin, oct, dec, or hex..
            (dolist (val (string-to-list value))
              (setq msg (format "%s\n%s" msg (describe-number--describe (number-to-string val)))))
          (setq msg (concat msg (describe-number--describe value))))
        (if (zerop (length msg))
            (message "No results for '%s'." value)
          (message "'%s':%s" value msg)))
    (message "Must input value!")))

;;;###autoload
(defun describe-number-at-point ()
  "Describe number at point or region by using `describe-number'."
  (interactive)
  (if (use-region-p)
      (describe-number
       (buffer-substring-no-properties (region-beginning) (region-end)))
    (let ((thing (thing-at-point 'word)))
      (if thing
          (describe-number (substring-no-properties thing))
        (message "Nothing at point!")))))


(provide 'describe-number)
;;; describe-number.el ends here
