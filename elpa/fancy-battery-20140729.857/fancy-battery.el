;;; fancy-battery.el --- Fancy battery display       -*- lexical-binding: t; -*-
;; Version: 20140729.857

;; Copyright (C) 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; URL: https://github.com/lunaryorn/fancy-battery.el
;; Keywords: convenience tools hardware
;; Package-Version: 0.2-cvs
;; Package-Requires: ((emacs "24.1"))

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide `fancy-battery-mode', which is like `display-battery-mode' but with
;; fancier display, and more customization options.

;; Customize `fancy-battery-mode-line' to change the appearance of the battery
;; status information.  Take a look at `fancy-battery-default-mode-line' for the
;; default value and for inspiration.

;; Customize `mode-line-format' and `mode-line-misc-info' to change the position
;; at which the battery status appears in the mode line.  Typically it's at the
;; very end after the minor mode list, so you may want to move
;; `mode-line-misc-info` more to the front of `mode-line-format`.

;;; Code:

(require 'battery)

(defcustom fancy-battery-mode-line
  '(:eval (fancy-battery-default-mode-line))
  "Mode line string for `fancy-battery-mode'.

This variable is a mode line format sexp.  See Info
Node `(elisp)Mode Line Format' for more information, and
`fancy-battery-default-mode-line' for the default value.

Do *not* call `battery-status-function' in the mode line format.
This would *significantly* slow down mode line updates.  Instead,
use the cached status in `fancy-battery-last-status'."
  :type 'sexp
  :group 'battery
  :risky t)

(defface fancy-battery-critical '((t :inherit error))
  "Face for critical battery status"
  :group 'battery)

(defface fancy-battery-charging '((t :inherit success))
  "Face for charging battery status."
  :group 'battery)

(defface fancy-battery-discharging '((t :inherit warning))
  "Face for charging battery status."
  :group 'battery)

(defvar fancy-battery-timer nil
  "Timer to update the battery information.")

(defvar fancy-battery-last-status nil
  "Last battery status.")

(defun fancy-battery-update ()
  "Update battery information and update the mode line."
  (setq fancy-battery-last-status (and battery-status-function
                                       (funcall battery-status-function)))
  (force-mode-line-update))

(defun fancy-battery-default-mode-line ()
  "Assemble a mode line string for Fancy Battery Mode.

Display the remaining battery time, if available, otherwise the
percentage.  If the battery is critical, use
`battery-critical-face'.  Otherwise use `fancy-battery-charging'
or `fancy-battery-discharging', depending on the current state."
  (when fancy-battery-last-status
    (let* ((time (cdr (assq ?t fancy-battery-last-status)))
           (face (pcase (cdr (assq ?b fancy-battery-last-status))
                   ("!" 'fancy-battery-critical)
                   ((or "" "+") 'fancy-battery-charging)
                   (_ 'fancy-battery-discharging)))
           (percentage (cdr (assq ?p fancy-battery-last-status)))
           (status (if (string= time "N/A") (concat percentage "%%") time)))
      (if time
          (propertize status 'face face)
        ;; Battery status is not available
        (propertize "N/A" 'face 'error)))))

;;;###autoload
(define-minor-mode fancy-battery-mode
  "Display battery status in the mode line.

Like `display-battery-mode', but fancier, and with more
customization options.

With prefix argument ARG, enable Fancy Battery Mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

If `battery-status-function' is nil, the mode is not enabled.

The text in the mode line is controlled by
`fancy-battery-mode-line'.  Battery information is obtained from
`battery-status-function', and updated every
`battery-update-interval' seconds."
  :global t :group 'battery
  (when fancy-battery-timer
    (cancel-timer fancy-battery-timer))
  (unless global-mode-string
    (setq global-mode-string '("")))

  (cond
   ((not fancy-battery-mode)
    (setq global-mode-string (delq 'fancy-battery-mode-line
                                   global-mode-string)))
   ((not battery-status-function)
    (fancy-battery-mode -1))
   (t
    (add-to-list 'global-mode-string 'fancy-battery-mode-line t)
    (setq fancy-battery-timer (run-at-time nil battery-update-interval
                                           #'fancy-battery-update)))))

(provide 'fancy-battery)

;;; fancy-battery.el ends here
