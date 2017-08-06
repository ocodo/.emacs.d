;;; fxrd-mode.el --- Major mode for editing fixed field width files

;; Copyright (C) 2015-2017 Marc Sherry

;; Author: Marc Sherry (msherry@gmail.com)
;; URL: https://github.com/msherry/fxrd-mode
;; Package-Requires: ((s "1.2"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:

;; This package implements `fxrd-mode', a major mode for editing files with
;; fixed field widths.  These files are commonly used in the financial
;; industry, such as in ACH transactions.  This package provides:

;; - `nacha-mode': a mode for editing NACHA (ACH transaction) files
;; - `rm37-mode': a mode for editing RM37 (Mastercard rebate transaction) files
;; - `tso6-mode': a mode for editing TSO6 (Mastercard rebate confirmation) files
;; - `cbnot-mode': a mode for editing CBNOT (Amex chargeback notification) files

;; In each mode, the current field is highlighted with
;; `fxrd-current-field-face', and the field's name is shown in the
;; modeline.  All fields with errors are highlighted with
;; `fxrd-invalid-field-face', and if the current field has an error, the error
;; is also displayed in the modeline.

;; In each of these modes, the following commands are available:

;; - M-<right> (`fxrd-next-field') and M-<left> (`fxrd-previous-field') move to
;;   the next and previous fields, respectively.
;; - C-. (`fxrd-next-error') moves to the next invalid field.

;; Installation:

;; Installation via MELPA is easiest.

;;; Code:

(defgroup fxrd nil
  "Major mode for editing fixed field width files"
  :group 'convenience)

;; This changed around emacs 25
(eval-when-compile
  (when (not (boundp 'default-mode-line-format))
    (defvar default-mode-line-format (default-value 'mode-line-format))))

(defface fxrd-current-field-face
  '((t (:inherit highlight
        :background "pink")))
  "Highlight the current field."
  :group 'fxrd)
(defvar fxrd-current-field-face 'fxrd-current-field-face)

(defface fxrd-invalid-field-face
  '((t (:inherit highlight
        :background "red")))
  "Face for fields failing validation."
  :group 'fxrd)
(defvar fxrd-invalid-field-face 'fxrd-invalid-field-face)

(defconst fxrd-mode-line-help-echo
  ;; See bindings.el for details of `mode-line-format' construction.
  (get-text-property 0 'help-echo (car default-mode-line-format))
  "Primary default mode line help echo text.")

(defconst fxrd-mode-line-format
  ;; See bindings.el for details of `mode-line-format' construction.
  (append (butlast default-mode-line-format 2)
	  (cons `(fxrd-field-name-string
		  ("" fxrd-field-name-string
		   ,(propertize "" 'help-echo fxrd-mode-line-help-echo)))
		(last default-mode-line-format 2)))
  "Mode line format string for FXRD mode.")

(defconst fxrd-font-lock-keywords-1
  (list
   '()
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for FXRD mode.")

(defvar fxrd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<right>") 'fxrd-next-field)
    (define-key map (kbd "M-<left>") 'fxrd-previous-field)
    (define-key map (kbd "C-.") 'fxrd-next-error)
    map)
  "Keymap for FXRD major mode")

(defvar fxrd-mode-syntax-table
  (let ((st (make-syntax-table)))
    st))

(defvar fxrd-font-lock-keywords fxrd-font-lock-keywords-1
  "Default highlighting expressions for FXRD mode")

(defvar fxrd-current-spec nil)
(make-variable-buffer-local 'fxrd-current-spec)

(defvar fxrd-mode-hook nil)

(defvar fxrd--current-field-priority 1)

(defvar fxrd--invalid-field-priority 2)

(defvar fxrd--last-buffer-modified-tick nil)

(defvar fxrd--last-point nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'fxrd-validators)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; save-mark-and-excursion in Emacs 25 works like save-excursion did before
(eval-when-compile
  (when (not (fboundp 'save-mark-and-excursion))
    (defmacro save-mark-and-excursion (&rest body)
      `(save-excursion ,@body))))

(defun disable-fxrd-mode ()
  (fxrd-field-name-mode -1)
  (fxrd--clear-overlays))

(defun current-line-pos ()
  "Yields the current position within the line"
  ;; TODO: find a better way to find position within a line
  (+ 1 (- (point) (line-beginning-position))))

(defun get-spec-for-line ()
  "Finds the correct record spec to use for the current line, based on the first character."
  (when fxrd-current-spec
    (let* ((type (line-type))
           (record-spec (cadr (assoc type fxrd-current-spec))))
      record-spec)))

(defun first-spec-hit (record-spec pos)
  "Given a record spec and a position within a line, return the first spec-item hit.

Returns nil if no hit found."
  (dolist (spec-item record-spec)
    (let ((start (nth 0 spec-item))
          (end (nth 1 spec-item)))
      (when (<= start pos end)
        (return spec-item)))))

(defun get-field-boundaries (&optional field-spec)
  "Find the (absolute) [start, end + 1] position of the field
defined by `field-spec', or the field at point if none is supplied.

`end' will actually be one more than the final position of the
field, due to the way most elisp functions (make-overlay,
buffer-substring, etc.) handle ranges."
  (unless field-spec (setq field-spec (current-field-spec)))
  (when field-spec
    (let* ((line-start (line-beginning-position))
           (start (1- (+ line-start (nth 0 field-spec))))
           (end (+ line-start (nth 1 field-spec))))
      (list start end))))

(defun get-name-from-field-spec (field-spec)
  "Given a field spec, extract the name part."
  (nth 2 field-spec))

(defun get-validator-from-field-spec (field-spec)
  "Given a field spec, extract the validator, if present."
  (nth 3 field-spec))

(defun line-type ()
  "Determines the record type of the current line"
  (let* ((char (char-after (line-beginning-position)))
         (type (if char (char-to-string char))))
    type))

(defun current-field-spec ()
  (let ((record-spec (get-spec-for-line)))
    (if record-spec
        (let ((field-spec (first-spec-hit record-spec (current-line-pos))))
          field-spec))))

(defun field-name (&optional field-spec)
  "Find the name of the field defined by `field-spec', or the
field at point if no field-spec is provided."
  (unless field-spec (setq field-spec (current-field-spec)))
  (when field-spec
    (get-name-from-field-spec field-spec)))

(defun fxrd-field-value (&optional field-boundaries)
  "Find the contents of the field defined by `field-boundaries',
or the current field if not supplied."
  (unless field-boundaries (setq field-boundaries (get-field-boundaries)))
  (when field-boundaries
    (let ((start (nth 0 field-boundaries))
          (end (nth 1 field-boundaries)))
      (when (and start end
                 (<= start (point-max))
                 (<= end (point-max)))
        (buffer-substring start end)))))

(defun fxrd-current-field-valid-p ()
  "Returns t if the current field is valid, or nil otherwise."
  (unless (fxrd-field-error) t))

(defun fxrd-field-error (&optional field-spec)
  "Returns an error string if the field defined by field-spec is invalid,
 or nil otherwise."
  (unless field-spec (setq field-spec (current-field-spec)))
  (let* ((field-boundaries (get-field-boundaries field-spec))
         (validator (get-validator-from-field-spec field-spec))
         (value (fxrd-field-value field-boundaries)))
    ;; If no validator defined, field is valid by default.
    (when validator
      (condition-case err
          ;; Negate t-returning validators to indicate no error.
          (not
           (cond ((fxrd-validator-child-p validator)
                  ;; fxrd-validator object
                  (fxrd-validate validator value))
                 ((functionp validator)
                  ;; generic function validator
                  (funcall validator value))
                 (t
                  ;; unknown validator type
                  (signal 'validator-error "Unknown validator type for field"))))
        (validation-error (cdr err))))))

(defun fxrd-point-in-field-boundaries-p (field-boundaries cur-pos)
  "Returns t if the point is inside the given field-boundaries, nil otherwise."
  (let ((begin (nth 0 field-boundaries))
        (end (nth 1 field-boundaries)))
    ;; Remember to account for `end' being off by one
    (<= begin cur-pos (1- end))))

(defun fxrd--next-field-start-pos (field-boundaries)
  "Return the start position of the field after the one defined by `field-boundaries'"
  (let ((next-field-start (nth 1 field-boundaries)))
    (min next-field-start (point-max))))

(defun fxrd--make-overlay (begin end type)
  (let (values)
    (cond ((eq :current type) (setq values (list 'fxrd-current-overlay
                                            fxrd-current-field-face
                                            fxrd--current-field-priority)))
          ((eq :invalid type) (setq values (list 'fxrd-invalid-overlay
                                                 fxrd-invalid-field-face
                                                 fxrd--invalid-field-priority)))
          (t nil))
    (let ((overlay (make-overlay begin end)))
      (overlay-put overlay (nth 0 values) t)
      (overlay-put overlay 'face (nth 1 values))
      (overlay-put overlay 'priority (nth 2 values))
      overlay)))

(defun fxrd--clear-overlays ()
  (remove-overlays nil nil 'fxrd-current-overlay t)
  (remove-overlays nil nil 'fxrd-invalid-overlay t))


(defun fxrd--buffer-changed-p ()
  (let* ((tick (buffer-modified-tick))
         (changed (not (eq tick fxrd--last-buffer-modified-tick))))
    (setq fxrd--last-buffer-modified-tick tick)
    changed))

(defun fxrd--point-changed-p ()
  (let* ((p (point))
         (changed (not (eq p fxrd--last-point))))
    (setq fxrd--last-point p)
    changed))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fxrd-next-field (&optional field-boundaries)
  "Move to the start of the next field, or the field following
the one described by `field-boundaries'"
  (interactive)
  (unless field-boundaries (setq field-boundaries
                                 (or (get-field-boundaries)
                                     (list (1+ (point)) (1+ (point))))))
  (when field-boundaries
    (goto-char (fxrd--next-field-start-pos field-boundaries))))

(defun fxrd-previous-field ()
  "Move to the start of the previous field."

  ;; TODO: fix near newlines at end of line
  (interactive)
  (let ((field-boundaries (get-field-boundaries)))
    (when field-boundaries
      (let ((prev-field-end (1- (nth 0 field-boundaries))))
        (goto-char (max prev-field-end (point-min)))
        (let ((prev-field-boundaries (get-field-boundaries)))
          (when prev-field-boundaries
            (let ((begin (nth 0 prev-field-boundaries)))
              (goto-char begin))))))))

(defun fxrd-next-error ()
  "Move to the next invalid field."
  (interactive)
  ;; Stay here if we're already in an invalid field
  (if (and (get-field-boundaries)
           (fxrd-current-field-valid-p))
      (let ((found-error nil)
            (start-of-error nil))
        (save-mark-and-excursion
          (while (and (not found-error)
                      (not (equal (point) (point-max))))
            (fxrd-next-field)
            (when (and (get-field-boundaries)
                       (not (fxrd-current-field-valid-p)))
              (setq found-error t
                    start-of-error (nth 0 (get-field-boundaries))))))
        (when found-error
          (goto-char start-of-error)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Field name mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom fxrd-field-name-delay 0.125
  "Time in seconds to delay before updating field name display."
  :group 'fxrd
  :type '(number :tag "seconds"))

(defvar fxrd-field-name-idle-timer nil)

(defvar fxrd-field-name-string nil)
(make-variable-buffer-local 'fxrd-field-name-string)

(defvar fxrd-field-name-string-old
  "The last highlighted field's name" nil)
(make-variable-buffer-local 'fxrd-field-name-string-old)

(defvar fxrd-field-value-old
  "The last computed value of the current field"
  nil)
(make-variable-buffer-local 'fxrd-field-value-old)

(defvar fxrd-field-boundaries-old
  "The last computed boundaries of the current field"
  nil)
(make-variable-buffer-local 'fxrd-field-boundaries-old)

(define-minor-mode fxrd-field-name-mode
  ;; TODO: this probably shouldn't be a minor mode
  "Toggle FXRD-field-name mode.
When enabled, the name of the current field appears in the mode line."
  :group 'fxrd
  :global t
  :init-value t
  ;; First, always disable current timer to avoid having two timers.
  (when fxrd-field-name-idle-timer
    (cancel-timer fxrd-field-name-idle-timer)
    (setq fxrd-field-name-idle-timer nil))
  ;; Now, if mode is on and any buffer is in FXRD mode then re-initialize and
  ;; enable by setting up a new timer
  (if fxrd-field-name-mode
      (if (memq t (mapcar (lambda (buffer)
                            (with-current-buffer buffer
                              (when (derived-mode-p 'fxrd-mode)
                                ;; TODO: should we be setting these in the map fn?
                                (setq fxrd-field-name-string nil
                                      fxrd-field-name-string-old nil)
                                t)))
                          (buffer-list)))
          (setq fxrd-field-name-idle-timer
                (run-with-idle-timer fxrd-field-name-delay t
                                     'fxrd--update-display)))
    ;; but if the mode is off then remove the display from the mode lines of
    ;; all FXRD buffers
    (mapc (lambda (buffer)
            (with-current-buffer buffer
              (when (derived-mode-p 'fxrd-mode)
                (setq fxrd-field-name-string nil
                      fxrd-field-name-string-old nil)
                (force-mode-line-update)
                (fxrd--clear-overlays))))
          (buffer-list))))

(defun fxrd--update-display ()
  "Called by `fxrd-field-name-idle-timer' to update all displays."
  (when (derived-mode-p 'fxrd-mode)
    (when (fxrd--buffer-changed-p)
      (fxrd-highlight-invalid-fields))
    (when (fxrd--point-changed-p)
      (fxrd-update-current-field))))

(defun fxrd--maybe-set-modeline (text)
  (when (not (string= text fxrd-field-name-string-old))
        ;; Update modeline
        (setq fxrd-field-name-string-old text
              fxrd-field-name-string
              (and text (propertize (format "%s" text)
                                          'help-echo fxrd-mode-line-help-echo)))
        (force-mode-line-update)))

(defun fxrd-update-current-field ()
  "Highlight the current field, and construct `fxrd-field-name-string' to display in the mode line."
  (let* ((field-spec (current-field-spec))
         (field-boundaries (get-field-boundaries field-spec)))
    (if (and field-spec field-boundaries)
        (let ((field-name (field-name field-spec))
              (field-value (fxrd-field-value field-boundaries)))
          (fxrd--maybe-set-modeline field-name)
          ;; Highlight current field, update modeline with error text if necessary
          (let ((validation-error (fxrd-field-error field-spec)))
            (when validation-error
              ;; If not t, it's a validation error message
              (fxrd--maybe-set-modeline (format "%s:%s" field-name validation-error)))
            (when (not (and (string= fxrd-field-value-old field-value)
                            (equal fxrd-field-boundaries-old field-boundaries)))
              (setq fxrd-field-value-old field-value
                    fxrd-field-boundaries-old field-boundaries)
              (remove-overlays nil nil 'fxrd-current-overlay t)
              (let* ((begin (nth 0 field-boundaries))
                     (end (nth 1 field-boundaries)))
                (fxrd--make-overlay begin end :current)))))
      ;; Not in a field, clear the overlay and modeline
      (progn
        (setq fxrd-field-value-old nil
              fxrd-field-boundaries-old nil)
        (fxrd--maybe-set-modeline nil)
        (remove-overlays nil nil 'fxrd-current-overlay t)))))

(defun fxrd-highlight-invalid-fields ()
  "Highlight all invalid fields"
  (let ((cur-pos (point)))
    (save-mark-and-excursion
      (goto-char (point-min))
      (remove-overlays nil nil 'fxrd-invalid-overlay t)
      (let ((done nil)
            (last-pos (point)))
        (while (not done)
          (let ((field-boundaries (get-field-boundaries)))
            (when (and field-boundaries
                       ;; Field not valid
                       (not (fxrd-current-field-valid-p)))
              (let* ((begin (nth 0 field-boundaries))
                     (end (nth 1 field-boundaries)))
                (fxrd--make-overlay begin end :invalid))))
          (fxrd-next-field)
          (if (eq (point) last-pos)
              (setq done t))
          (setq last-pos (point)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode fxrd-mode nil "FXRD"
  "Major mode for editing fixed field width files.

\\{fxrd-mode-map}"
  :group 'fxrd
  :syntax-table fxrd-mode-syntax-table
  (use-local-map fxrd-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(fxrd-font-lock-keywords))
  (set (make-local-variable 'mode-line-format) fxrd-mode-line-format)
  (set (make-local-variable 'show-trailing-whitespace) nil)
  (set (make-local-variable 'require-final-newline) nil)
  (fxrd-field-name-mode 1)
  (overwrite-mode)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook (make-local-variable 'change-major-mode-hook) 'disable-fxrd-mode))

(provide 'fxrd-mode)
;;; fxrd-mode.el ends here
