;;; 0xc-live.el --- Base conversion made live

;; Copyright 2020 Adam Niederer

;; Author: Adam Niederer <adam.niederer@gmail.com>
;; URL: http://github.com/AdamNiederer/0xc
;; Version: 0.1
;; Keywords: base conversion
;; Package-Requires: ((emacs "25.1") (s "1.11.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 0xc-live-convert will convert any number with base inference and
;; display it in another buffer.
;;
;; Exported names start with "0xc-live"; private names start with
;; "0xc-live--".

;;; Code:

(require '0xc)
(require 'subr-x)

(defgroup 0xc nil
  "Live base conversion functions"
  :prefix "0xc-live"
  :group 'languages
  :link '(url-link :tag "Github" "https://github.com/AdamNiederer/0xc")
  :link '(emacs-commentary-link :tag "Commentary" "0xc-live"))

(defcustom 0xc-live-display-bases '(16 10 8 2)
  "The bases to which all inputs will be converted and displayed."
  :tag "0xc Live Displayed Bases"
  :group '0xc-live
  :type '(repeat number))

(defcustom 0xc-live-input-bases '(16 10 8 2)
  "The bases which all inputs may be interpreted as."
  :tag "0xc Live Displayed Bases"
  :group '0xc-live
  :type '(repeat number))

(defun 0xc-live--table-rows (number-string)
  "Return all interpretations and conversions of NUMBER-STRING."
  (let* ((highest-base (0xc--highest-base number-string))
         (input-bases (if (0xc--base-prefix number-string)
                          (list (0xc--infer-base number-string))
                        (or (seq-filter (lambda (base) (>= base highest-base)) 0xc-live-input-bases)
                            (0xc--infer-base number-string)))))
    (cons (cons "Input" (mapcar (lambda (base) (format "Base %d" base)) 0xc-live-display-bases))
          (mapcar (lambda (in-base) (cons (format "%s%s"
                                             (0xc--prefix-for-base in-base)
                                             (0xc--strip-base-hint number-string))
                                     (mapcar (lambda (out-base) (0xc-number-to-string (0xc-string-to-number number-string in-base) out-base))
                                             0xc-live-display-bases)))
                  input-bases))))

(defun 0xc-live--display (input)
  "Display the converted reperestantions of INPUT in another buffer."
  (with-current-buffer-window
      "*0xc Live Conversion*" nil nil
    (erase-buffer)
    (condition-case err
        (let* ((rows (0xc-live--table-rows input))
               (col-widths (seq-reduce (lambda (acc row) (seq-mapn #'max acc (seq-map (lambda (cell) (length cell)) row)))
                                       rows (make-list (length (car rows)) 0))))
          (dolist (row rows)
            (dolist (cell (seq-mapn (lambda (v w) (list v w)) row col-widths))
              (let* ((value (car cell))
                     (width (- (cadr cell) (length value))))
                (insert value (s-repeat width " ") "  ")))
            (insert "\n")))
      ('error (insert (format "0xc-live: %s" err))))))

(defun 0xc-live--maybe-number-at-point ()
  "Return the text at at point if it's a number, else return nothing."
  (if (0xc--is-number-string (or (word-at-point) "")) (word-at-point) ""))

;;;###autoload
(defun 0xc-live-convert ()
  "Show all possible conversions of a number in another buffer as you type it."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda () (add-hook 'after-change-functions
                      (lambda (&rest _) (0xc-live--display (minibuffer-contents)))
                      nil 'local))
    (read-string "Number: " nil nil (0xc-live--maybe-number-at-point))))

(provide '0xc-live)
;;; 0xc-live.el ends here
