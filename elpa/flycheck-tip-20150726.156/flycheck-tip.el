;;; flycheck-tip.el --- Show flycheck/flymake errors by tooltip -*- lexical-binding: t; -*-

;; Copyright (C) 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/flycheck-tip
;; Version: 0.5.0
;; Package-Requires: ((flycheck "0.13") (emacs "24.1") (popup "0.5.0"))
;; Keywords: flycheck

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Usage:
;; Basic setting
;;
;;   (require 'flycheck-tip)
;;   (define-key your-prog-mode (kbd "C-c C-n") 'flycheck-tip-cycle)
;;
;; If you are still using flymake, you can use combined function that
;; show error by popup in flymake-mode or flycheck-mode.
;;
;;   (define-key global-map (kbd "C-0") 'error-tip-cycle-dwim)
;;   (define-key global-map (kbd "C-9") 'error-tip-cycle-dwim-reverse)
;;
;; If you build Emacs with D-Bus option, you may configure following setting.
;; This keeps the errors on notification area. Please check
;; ‘error-tip-notify-timeout’ to change limit of the timeout as well.
;;
;;   (setq error-tip-notify-keep-messages t)
;;; Code:

(require 'cl-lib)
(require 'flycheck)
(require 'error-tip)

(defvaralias 'flycheck-tip-timer-delay 'error-tip-timer-delay
  "Alias of `error-tip-timer-delay'.")

(defcustom flycheck-tip-avoid-show-func t
  "Avoid `flycheck-show-error-at-point' function's behavior.
This variable is true by default."
  :group 'flycheck-tip
  :type 'boolean)

;; memo flycheck-current-errors
;; 0 : err name?
;; 1 : buffer
;; 2 : lang
;; 3 : file
;; 4 : line
;; 5 : line?
;; 6 : message
;; 7 : err type?

;;;###autoload
(defun flycheck-tip-cycle (&optional reverse)
  "Move to next error if it's exists.
If it wasn't exists then move to previous error.
Move to previous error if REVERSE is non-nil."
  (interactive)
  (error-tip-cycle
   (error-tip-collect-current-file-errors flycheck-current-errors) reverse))

;;;###autoload
(defun flycheck-tip-cycle-reverse ()
  "Do `flycheck-tip-cycle by reverse order."
  (interactive)
  (flycheck-tip-cycle t))

(defadvice flycheck-display-error-at-point
  (around flycheck-tip-avoid-function activate)
  "Avoid flycheck's displaying feature on echo ares if you set non-nil to `flycheck-tip-avoid-show-func'."
  (if flycheck-tip-avoid-show-func
      nil
    ad-do-it))

(defun flycheck-tip-use-timer (order)
  "You can set 'normal, 'verbose or nil to ORDER.
The normal means, use error popup and using timer or not is configurable.
The verbose means, use error popup and popup current-line error if it's exists
after `error-tip-timer-delay' seconds.
If you set nil, show popup error immediately after you invoke flycheck-tip-cycle
or flycheck-tip-cycle-reverse."
  (cl-case order
    (normal
     (setq flycheck-tip-avoid-show-func t))
    (verbose
     (setq flycheck-tip-avoid-show-func nil
           flycheck-idle-change-delay error-tip-timer-delay
           flycheck-display-errors-function
           'flycheck-tip-display-current-line-error-message))
    ;; do not use timer
    (t (setq flycheck-tip-avoid-show-func t
             error-tip-timer-delay nil))))

(defun flycheck-tip-display-current-line-error-message (errors)
  "Show current line's ERRORS by popup.
This function is used to replace ‘flycheck-display-errors-function’."
  (error-tip-delete-popup)
  (let ((current-line-errors (mapcar #'flycheck-error-message errors))
        ;; prevents frequently notification update
        (error-tip-notify-keep-messages nil))
    (when current-line-errors
      (setq error-tip-current-errors current-line-errors)
      (error-tip-popup-error-message current-line-errors (point)))))

(provide 'flycheck-tip)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; flycheck-tip.el ends here
