;;; mc-jump.el --- like "jump-char", but "multiple-cursors" friendly

;; Copyright (C) 2013 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.0.0

;;; Commentary:

;; To install, Load this library
;;
;;   (require 'mc-jump)
;;
;; and bind keys.
;;
;;   (global-set-key (kbd "M-m") 'mc-jump-char)
;;
;; Now "M-m a" will jump to the next occurence of "a", for example. You may
;; press "a" again and again to browse more matches.

;; "mc-jump-char" is designed to be used with "multiple-cursors". That is,
;; you may also use this command to move all pseudo cursors. Still, this
;; command does not depend on "multiple-cursors".

;;; Change Log:

;; 1.0.0 first released

;;; Code:

;; * constants

(defconst mc-jump-version "1.0.0")

;; * variables

(defvar mc-jump-command-p nil
  "true if the last command is mc-jump-char")

(defvar mc-jump-original-binding nil
  "stores the original key definition in form (key . value)")
(make-variable-buffer-local 'mc-jump-original-binding)

;; * hooks

(defun mc-jump-pre-command-function ()
  (setq mc-jump-command-p nil))

(defun mc-jump-post-command-function ()
  (when (and (not mc-jump-command-p)
             mc-jump-original-binding)
    (local-set-key (car mc-jump-original-binding)
                   (cdr mc-jump-original-binding))))

(add-hook 'post-command-hook 'mc-jump-post-command-function)
(add-hook 'pre-command-hook 'mc-jump-pre-command-function)

;; * command

(defun mc-jump-char ()
  (interactive)
  (let ((chr (char-to-string (read-char "goto-char ? "))))
    (setq this-original-command `(lambda ()
                                   (interactive)
                                   (let ((pos (save-excursion
                                                (skip-chars-forward
                                                 ,(concat "^" chr (upcase chr)))
                                                (point))))
                                     (if (= pos (point-max))
                                         (error "not found")
                                       (setq mc-jump-command-p t)
                                       (goto-char pos)))
                                   (forward-char 1))
          ;; let multiple-cursors forget his "note"
          mc--this-command this-original-command)
    ;; store original key definition
    (setq mc-jump-original-binding (cons chr (key-binding chr)))
    ;; and override it with mc-jump-char
    (local-set-key chr this-original-command)
    ;; then call it
    (call-interactively this-original-command)))

;; * provide

(provide 'mc-jump)

;;; mc-jump.el ends here
