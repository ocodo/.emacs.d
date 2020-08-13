;;; point-stack.el --- Back and forward navigation through buffer locations

;; Copyright (c) 2010 Matt Harrison
;; Copyright (c) 2011-2014 Dmitry Gutov

;; Author: Matt Harrison <matthewharrison@gmail.com>
;;         Dmitry Gutov <dgutov@yandex.ru>
;; Version: 1.1
;; Package-Version: 20200427.107
;; Package-Commit: cddcea2c91038710c245819b3cda2dd739726134

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Provides window-local back and forward stacks for point.
;; Each stack value contains buffer reference, point position,
;; and window scroll position.
;; Both stacks are local to current window, in other words, stacks in
;; different windows are not related.
;;
;; To navigate the stacks, bind back and forward commands to some keys:
;; (global-set-key (kbd "C-M-,") 'point-stack-pop)
;; (global-set-key (kbd "C-M-.") 'point-stack-forward-stack-pop)
;;
;; The recommended usage is to save locations automatically on navigation:
;; (point-stack-setup-advices)
;;
;; To push values to stack manually instead, bind the push command:
;; (global-set-key [f5] 'point-stack-push)
;;
;; Based on https://github.com/mattharrison/point-stack
;; which is in turn based on http://www.emacswiki.org/emacs/JohnConnors

;;; Code:

(defgroup point-stack nil
  "Back and forward stacks for point location"
  :group 'convenience)

(defcustom point-stack-advised-functions
  '(isearch-mode find-function-do-it find-library
    imenu beginning-of-buffer end-of-buffer
    xref-find-definitions counsel-imenu counsel-git-grep)
  "Functions that will be advised by `point-stack-setup-advices'."
  :group 'point-stack)

;;;###autoload
(defun point-stack-push ()
  "Push current buffer, point, and window scroll position onto the stack."
  (interactive)
  (point-stack--store 'stack)
  (point-stack--value 'forward 'set nil) ; New step resets forward history.
  (when (called-interactively-p 'interactive)
    (message "Location saved")))

;;;###autoload
(defun point-stack-pop ()
  "Push current location on forward stack, move to previous location."
  (interactive)
  (if (point-stack--value 'stack 'null)
      (message "Stack is empty")
    (point-stack--store 'forward)
    (point-stack--go 'stack)
    (point-stack--value 'stack 'shift)))

;;;###autoload
(defun point-stack-forward-stack-pop ()
  "Push current location on stack, pop and move to location from forward stack."
  (interactive)
  (if (point-stack--value 'forward 'null)
      (message "Forward stack is empty")
    (point-stack--store 'stack)
    (point-stack--go 'forward)
    (point-stack--value 'forward 'shift)))

(defun point-stack--store (stack)
  (let ((loc (point-stack--value stack 'car)))
    ;; Don't push the same location twice.
    (unless (and (eq (current-buffer) (car loc))
                 (eq (point) (cadr loc)))
      (point-stack--value stack 'push
                          (list (current-buffer) (point) (window-start))))))

(defun point-stack--go (stack)
  (let ((loc (point-stack--value stack 'car)))
    (switch-to-buffer (car loc))
    (set-window-start nil (nth 2 loc))
    (goto-char (cadr loc))))

(defun point-stack--value (name action &optional arg)
  (let* ((parameter (intern (concat "point-stack-" (symbol-name name))))
         (value (window-parameter nil parameter)))
    (cond ((eq action 'car)
           (car value))
          ((eq action 'null)
           (null value))
          (t (set-window-parameter nil parameter
                                   (pcase action
                                     (`set arg)
                                     (`push (cons arg value))
                                     (`shift (cdr value))))))))

;;;###autoload
(defun point-stack-setup-advices ()
  "Advise navigation functions to call `point-stack-push' before
any navigation is made. This way, it can be used as a replacement
for the global mark ring."
  (mapc (lambda (func)
          (eval
           `(defadvice ,func (around point-stack-push activate)
              (point-stack-push)
              ad-do-it)))
        point-stack-advised-functions))

(provide 'point-stack)

;;; point-stack.el ends here
