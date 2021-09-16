;;; easy-kill-mc.el --- multiple-cursors-mode support for easy-kill.

;; Copyright (c) 2018 Akinori MUSHA
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/easy-kill-extras.el
;; Created: 11 Sep 2018
;; Package-Requires: ((easy-kill "0.9.4") (multiple-cursors "1.4.0"))
;; Keywords: killing, cursors, convenience

;;; Commentary:
;;
;; This module adds multiple-cursors-mode support to easy-kill.

;;; Code:

(require 'easy-kill)
(require 'multiple-cursors)

(add-to-list 'mc/cursor-specific-vars 'easy-kill-candidate)

(defvar-local easy-kill-mc-easy-mark-or-kill-command nil)
(defvar-local easy-kill-mc-execute-nest-level 0)
(defvar-local easy-kill-mc-keep-keymap-p nil)
(defvar-local easy-kill-mc-destroy-candidate-p nil)

(defadvice easy-mark
    (before easy-kill-mc activate)
  (setq easy-kill-mc-easy-mark-or-kill-command 'easy-mark))

(defadvice easy-kill
    (before easy-kill-mc activate)
  (setq easy-kill-mc-easy-mark-or-kill-command 'easy-kill))

;; The original `easy-kill-destroy-candidate' registers a one-time
;; function to `post-command-hook', but it is not sufficient in
;; multiple cursors mode.  We have to destroy all candidates at once
;; while in `mc/execute-this-command-for-all-cursors'.
(defadvice mc/execute-this-command-for-all-cursors
    (around easy-kill-mc activate)
  (setq easy-kill-mc-execute-nest-level (1+ easy-kill-mc-execute-nest-level))
  (unwind-protect
      ad-do-it
    (setq easy-kill-mc-execute-nest-level (1- easy-kill-mc-execute-nest-level))
    (when (zerop easy-kill-mc-execute-nest-level)
      (remove-hook 'pre-command-hook 'easy-kill-mc-save-candidate-1)
      (setq easy-kill-mc-easy-mark-or-kill-command nil)
      (when easy-kill-mc-destroy-candidate-p
        (easy-kill-mc-destroy-candidate)
        (setq easy-kill-mc-destroy-candidate-p nil)))))

(defadvice easy-kill-init-candidate
    (after easy-kill-mc activate)
  (overlay-put easy-kill-candidate 'type 'easy-kill-candidate))

(defadvice easy-kill-activate-keymap
    (around easy-kill-mc activate)
  (if (bound-and-true-p multiple-cursors-mode)
      (or
       ;; Set a transient key map just once, ignoring subsequent calls
       ;; for fake cursors.
       (< 0 easy-kill-mc-execute-nest-level)
       (set-transient-map
        (easy-kill-map)
        (lambda ()
          (or
           ;; Ignore calls via pre-command-hook triggered by
           ;; subsequent calls of easy-kill/easy-mark for fake
           ;; cursors, returning `t' for the transient map to persist.
           easy-kill-mc-easy-mark-or-kill-command
           ;; Prevent any error from activating the keymap forever.
           (condition-case err
               (or
                ;; This command is not marked as exit and it is coming
                ;; from this keymap, continue.
                (and
                 (not (easy-kill-exit-p this-command))
                 (let ((map (easy-kill-map)))
                   (or (eq this-command
                           (lookup-key map (this-single-command-keys)))
                       (let ((cmd (key-binding
                                   (this-single-command-keys) nil t)))
                         (command-remapping cmd nil (list map))))))
                (ignore
                 ;; When leaving from this keymap, schedule
                 ;; easy-kill-mc-destroy-candidate after the current
                 ;; command is executed for eash cursor and save the
                 ;; current candidates for each cursor.
                 (setq easy-kill-mc-destroy-candidate-p t)
                 (easy-kill-mc-save-candidate-1)
                 (add-hook 'pre-command-hook 'easy-kill-mc-save-candidate-1 t)))
             (error (message "%s:%s" this-command (error-message-string err))
                    nil))))))
    ad-do-it))

(defun easy-kill-mc-save-candidate-1 ()
  (unless (or (easy-kill-get mark) (easy-kill-exit-p this-command))
    (easy-kill-save-candidate)))

(defun easy-kill-mc-destroy-candidate ()
  (mapc #'(lambda (o)
            (when (eq (overlay-get o 'type) 'easy-kill-candidate)
              (let ((i (overlay-get o 'origin-indicator)))
                (and (overlayp i) (delete-overlay i)))
              (delete-overlay o)))
        (overlays-in (point-min) (point-max))))

(dolist (func '(easy-kill-help))
  (add-to-list 'mc/cmds-to-run-once func))

(dolist (func '(easy-kill
                easy-kill-abort
                easy-kill-append
                easy-kill-delete-region
                easy-kill-digit-argument
                easy-kill-expand
                easy-kill-mark-region
                easy-kill-region
                easy-kill-shrink
                easy-kill-thing
                easy-kill-unhighlight
                easy-mark))
  (add-to-list 'mc/cmds-to-run-for-all func))

(provide 'easy-kill-mc)
;;; easy-kill-mc.el ends here
