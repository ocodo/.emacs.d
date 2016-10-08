;;; easy-kill-aj.el --- ace-jump integration for easy-kill.

;; Copyright (c) 2015 Akinori MUSHA
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
;; Created: 3 Mar 2015
;; Package-Requires: ((easy-kill "0.9.4") (ace-jump-mode "1.0"))
;; Keywords: killing, convenience

;;; Commentary:
;;
;; This tweak allows using ace-jump commands within
;; easy-mark/easy-kill as measures for selection.
;;
;; This library is part of the easy-kill-extras package and not meant
;; to be loaded standalone.

;;; Code:

(require 'easy-kill)
(require 'ace-jump-mode)

;;;###autoload
(defcustom easy-kill-ace-jump-enable-p t
  "If non-nil, ace-jump commands can be used in easy-kill/easy-mark mode for selection."
  :type 'boolean
  :group 'easy-kill-extras)

(defvar easy-kill-aj--easy-command nil)
(defvar easy-kill-aj--original-pos nil)

(defun easy-kill-aj--save-state ()
  (if (and easy-kill-ace-jump-enable-p
           easy-kill-candidate
           (eq (easy-kill-get buffer)
               (current-buffer)))
      (progn
        (easy-kill-abort)
        (setq easy-kill-aj--easy-command (if (easy-kill-get mark) 'easy-mark 'easy-kill)
              easy-kill-aj--original-pos (point)))
    (easy-kill-aj--clear-state)))

(defun easy-kill-aj--clear-state ()
  (setq easy-kill-aj--easy-command nil
        easy-kill-aj--original-pos nil))

(defun easy-kill-aj--after-jump (command orig-pos)
  (let* ((pos (point))
         (backward (> orig-pos pos))
         (line-mode (eq ace-jump-current-mode 'ace-jump-line-mode))
         (beg orig-pos)
         (end (if line-mode pos
                (if backward pos (1+ pos)))))
    (goto-char orig-pos)
    (funcall command)
    (if line-mode
        (let ((lines (count-lines beg end)))
          (easy-kill-thing 'line (if backward (- 1 lines)
                                   (1- lines))))
      (easy-kill-adjust-candidate
       (if backward 'string-to-char-backward 'string-to-char-forward)
       beg end)
      (overlay-put easy-kill-candidate 'zap-char (char-after pos))
      (overlay-put easy-kill-candidate 'zap-pos pos))
    (easy-kill-aj--clear-state)))

(dolist (mode '(ace-jump-char-mode ace-jump-word-mode ace-jump-line-mode))
  (eval
   `(defadvice ,mode (around easy-kill-extras activate)
      (easy-kill-aj--save-state)
      (let ((command easy-kill-aj--easy-command)
            (orig-pos easy-kill-aj--original-pos))
        ad-do-it
        (if (/= (point) orig-pos)
            ;; jumped directly
            (easy-kill-aj--after-jump command orig-pos))))))

(defadvice ace-jump-move (around easy-kill-extras activate)
  (let ((command easy-kill-aj--easy-command)
        (orig-pos easy-kill-aj--original-pos))
    ad-do-it
    (cond
     ((null command)
      ;; not called in easy-mark/easy-kill
      )
     (easy-kill-aj--easy-command
      ;; ace-jump-done not called
      (or ace-jump-mode ;; still in ace-jump-mode; branch mode
          (/= (point) orig-pos)
          (easy-kill-aj--after-jump command orig-pos)))
     (t
      (easy-kill-aj--after-jump command orig-pos)))))

(defadvice ace-jump-done (after easy-kill-extras activate)
  (easy-kill-aj--clear-state))

(provide 'easy-kill-aj)
;;; easy-kill-aj.el ends here
