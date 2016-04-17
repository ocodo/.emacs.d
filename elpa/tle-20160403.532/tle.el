;;; tle.el --- Tabulated List Extensions

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/tabulated-list-extensions
;; Package-Version: 20160403.532
;; Keywords: tabulated-list, extension
;; Version: 0.2.0
;; Package-Requires: ((dash "1.5.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package provides extensions to `tabulated-list-mode'.
;;

;;; Code:

(require 'dash)
(require 'dired)

(defgroup tle nil
  "Tabulated list extensions."
  :group 'tabulated-list
  :group 'convenience)

(defgroup tle-faces nil
  "Faces used by tabulated list extensions."
  :group 'tle
  :group 'faces)

(defface tle-marked
  '((t (:inherit dired-marked)))
  "Face used for marked files."
  :group 'tle-faces)

(defcustom tle-marker-string (string dired-marker-char)
  "Default string used for marking."
  :group 'tle
  :type 'string)

(defun tle-selected-p ()
  "Return true if the current row is selected."
  (save-excursion
    (beginning-of-line)
    (string-equal tle-marker-string (buffer-substring (point) (+ (point) (string-width tle-marker-string))))))

(defun tle-mark (&optional count)
  "Mark the next COUNT lines (default 1)."
  (interactive "p")
  (when (null count)
    (setq count 1))
  (--dotimes count (tabulated-list-put-tag tle-marker-string t)))

(defun tle-mark-all ()
  "Mark all."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tle-mark 1))))

(defun tle-unmark (&optional count)
  "Unmark the next COUNT lines (default 1)."
  (interactive "p")
  (when (null count)
    (setq count 1))
  (--dotimes count (tabulated-list-put-tag "" t)))

(defun tle-unmark-all ()
  "Unmark all."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tle-unmark 1))))

(defun tle-toggle ()
  "Toggle mark for current row."
  (interactive)
  (save-excursion
    (if (tle-selected-p)
        (tle-unmark 1)
      (tle-mark 1))))

(defun tle-toggle-all ()
  "Toggle all marks."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tle-toggle)
      (forward-line))))

(defun tle-selection-ids ()
  "Get the marked items `tabulated-list-get-id' data."
  (save-excursion
    (goto-char (point-min))
    (let ((selection ()))
      (while (not (eobp))
        (when (tle-selected-p)
          (add-to-list 'selection (tabulated-list-get-id) t))
        (forward-line))
      selection)))

(defun tle-selection-entries ()
  "Get the marked items `tabulated-list-get-entry' data."
  (save-excursion
    (goto-char (point-min))
    (let ((selection ()))
      (while (not (eobp))
        (when (tle-selected-p)
          (add-to-list 'selection (tabulated-list-get-entry) t))
        (forward-line))
      selection)))

(defun tle-selection-empty-p ()
  "Return t if the selection is empty."
  (save-excursion
    (null (tle-selection-ids))))

(defvar tle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'tle-mark)
    (define-key map "u" 'tle-unmark)
    (define-key map "t" 'tle-toggle-all)
    (define-key map "U" 'tle-unmark-all)
    map)
  "Keymap for `tle-mode'.")

(define-minor-mode tle-mode
  "Toggle Tabulated List Extensions mode.
With a prefix argument ARG, enable tle-mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil."
  nil
  " tle"
  tle-mode-map
  (font-lock-add-keywords nil `((,(format "^[%s].*" tle-marker-string) 0 'tle-marked prepend))))

(provide 'tle)

;;; tle.el ends here
