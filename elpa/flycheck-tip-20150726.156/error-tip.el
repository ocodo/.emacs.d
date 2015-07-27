;;; error-tip.el --- showing error library by popup.el -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/flycheck-tip
;; Version: 0.5.0
;; Package-Requires: ((emacs "24.1") (popup "0.5.0"))

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

;;; Code:

(require 'cl-lib)
(require 'popup)
(require 'notifications) ; this introduced from Emacs 24

(defvar error-tip-notify-keep-messages nil
  "If the value is non-nil, keep error messages to notification area.
This feature only activates when you leave from popup's message.")

(defvar error-tip-notify-last-notification nil
  "Last notification id.")

(defvar error-tip-notify-timeout (* 60 1000)
  "Value for time out.  The default value is 1 minute.")

(defvar error-tip-notify-parametors
  '(:title "flycheck-tip" :category "im.error")
  "Parameters for ‘error-tip-notify’.
You can add ‘notifications-notify’s parametors without :body, :replaces-id and
:timeout.

Example:

  (setq error-tip-notify-parametors
        (append error-tip-notify-parametors '(:app-icon \"/path/to/icon-file\")))")

;; INTERNAL VARIABLE
(defvar error-tip-popup-object nil)
(defvar error-tip-timer-object nil)
(defvar error-tip-current-errors nil)
(defvar error-tip-timer-delay 0.3
  "Whether how much delay showing error popup.
If you set nil to this variable, then do not use delay timer.")
(defvar error-tip-newline-character nil
  "Use this variable if you want change specific characters to turn to newlines.")

(defun error-tip-cycle (errors &optional reverse)
  (error-tip-delete-popup)
  (when errors
    (let*
        ((next     (assoc-default :next         errors))
         (previous (assoc-default :previous     errors))
         (cur-line (assoc-default :current-line errors))
         (jump (lambda (errs)
                 (goto-char (point-min))
                 (forward-line (1- (error-tip-get (car errs) 'line)))
                 (setq error-tip-current-errors errs)
                 (if (null error-tip-timer-delay)
                     (error-tip-popup-error-message (error-tip-get-errors))
                   (error-tip-cancel-timer)
                   (error-tip-register-timer))))
         (target (if (not reverse)
                     (or next previous cur-line)
                   (reverse (or previous next cur-line)))))
      (funcall jump target))))

(defun error-tip-get (err element)
  (cond
   ((bound-and-true-p flycheck-mode)
    (cl-case element
      (line    (elt err 4))
      (file    (elt err 3))
      (message (elt err 6))))
   ((bound-and-true-p eclim-mode)
    (cl-case element
      (line    (assoc-default 'line     err))
      (file    (assoc-default 'filename err))
      (message (assoc-default 'message  err))))))

(defun error-tip-collect-current-file-errors (errors)
  "Collect errors from ERRORS."
  (cl-loop with c-line = (line-number-at-pos (point))
           for err in errors
           for err-line = (error-tip-get err 'line)
           if (and buffer-file-truename ; whether file or buffer
                   (not (equal (expand-file-name buffer-file-truename)
                               (error-tip-get err 'file))))
           do '() ; skip
           else if (< c-line err-line)
           collect err into next
           else if (> c-line err-line)
           collect err into previous
           else if (= c-line err-line)
           collect err into current-line
           finally return (when (or next previous current-line)
                            (list (cons :next         next)
                                  (cons :previous     previous)
                                  (cons :current-line current-line)))))

(defun error-tip-popup-error-message (errors &optional point)
  "Popup error message(s) from ERRORS.
If there are multiple errors on current line, all current line's errors are
appeared.  The POINT arg is a point to show up error(s)."
  (setq error-tip-popup-object
        (popup-tip (error-tip-format errors) :nowait t :point (or point (error-tip-get-point))))
  (add-hook 'pre-command-hook 'error-tip-delete-popup))

(defun error-tip-get-point ()
  "Return point where the popup message emerges."
  (1+ (point-at-bol)))

(defun error-tip-format (errors)
  "Format ERRORS."
  (let ((messages (format "*%s" (mapconcat 'identity errors "\n*"))))
    (if error-tip-newline-character
        (replace-regexp-in-string error-tip-newline-character "\n" messages)
      messages)))

(defun error-tip-get-errors ()
  "Get errors."
  (cl-loop with current-line = (line-number-at-pos (point))
           for error in error-tip-current-errors
           for e-line = (error-tip-get error 'line)
           for e-str  = (error-tip-get error 'message)
           if (or (equal current-line e-line)
                  (and (equal 1 current-line)
                       (equal 0 e-line)))
           collect e-str into result
           else if (and (< (- 1 current-line) e-line)
                        (> (+ 1 current-line) e-line))
           collect e-str into fallback
           finally return (or result fallback)))

(defun error-tip-delete-popup ()
  "Delete popup object."
  (condition-case err
      (when (popup-live-p error-tip-popup-object)
        (popup-delete error-tip-popup-object)
        (when error-tip-notify-keep-messages (error-tip-notify)))
    (error err))
  (remove-hook 'pre-command-hook 'error-tip-delete-popup))

(defun error-tip-register-timer ()
  "Register timer that show error message."
  (setq error-tip-timer-object
        (run-with-timer error-tip-timer-delay nil
                        (lambda ()
                          (error-tip-popup-error-message (error-tip-get-errors))))))

(defun error-tip-cancel-timer ()
  "Cancel `error-tip-timer-object'."
  (when (timerp error-tip-timer-object)
    (cancel-timer error-tip-timer-object)))

(defun error-tip-error-p ()
  "Return non-nil if error is occurred in current buffer.
This function can catch error against flycheck, flymake and emcas-eclim."
  (or (bound-and-true-p flycheck-current-errors)
      (bound-and-true-p flymake-err-info)
      (and (fboundp 'eclim--problems-filtered)
           (eclim--problems-filtered))))

;;;###autoload
(defun error-tip-cycle-dwim (&optional reverse)
  "Showing error function.
This function switches proper error showing function by context.
 (whether flycheck or flymake) The REVERSE option jumps by inverse if
the value is non-nil."
  (interactive)
  (let ((func (cond
               ((bound-and-true-p flycheck-mode)
                'flycheck-tip-cycle)
               ((bound-and-true-p eclim-mode)
                'eclim-tip-cycle)
               ((bound-and-true-p flymake-mode)
                'flymake-tip-cycle))))
    (funcall func reverse)))

;;;###autoload
(defun error-tip-cycle-dwim-reverse ()
  "Same as ‘error-tip-cycle-dwim’, but it jumps to inverse direction."
  (interactive)
  (error-tip-cycle-dwim t))

;; Show errors by using notifications.el(D-Bus)
(defun error-tip-notify ()
  "Keep ERROR-MESSAGES on notification area.
See also ‘error-tip-notify-keep-messages’"
  (setq error-tip-notify-last-notification
        (apply `((lambda ()
                   (notifications-notify
                    ,@(and error-tip-notify-parametors)
                    :body ,(format "%s" (error-tip-format
                                         (if (cl-struct-p (car error-tip-current-errors))
                                             (error-tip-get-errors)
                                           error-tip-current-errors)))
                    :replaces-id error-tip-notify-last-notification
                    :timeout error-tip-notify-timeout))))))

(provide 'error-tip)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; error-tip.el ends here
