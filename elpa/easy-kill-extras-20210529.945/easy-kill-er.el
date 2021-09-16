;;; easy-kill-er.el --- Integration of `expand-region' into `easy-kill'.

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
;; Created: 20 Sep 2018
;; Package-Requires: ((easy-kill "0.9.4") (expand-region "0"))
;; Keywords: killing, cursors, convenience

;;; Commentary:
;;
;; This module integrates the `expand-region' functionality with `easy-kill'.

;;; Code:

(require 'easy-kill)

(defvar easy-kill-er-history nil)

(defun easy-kill-er-expand ()
  (interactive)
  "Expand current candidate using `expand-region'."
  (pcase (easy-kill-get bounds)
    (`(nil . nil))
    (`(,beg . ,end)
     (apply 'easy-kill-adjust-candidate (easy-kill-get thing)
            (save-excursion
              (push-mark beg t t)
              (goto-char end)
              (er--expand-region-1)
              (list (point) (mark)))))))

;;;###autoload
(defun easy-kill-er-expand (arg)
  "Expand current candidate using the algorithms used by `expand-region'.

This applies the `er/expand-region' effect to the current
candidate ARG times."
  (interactive "p")
  (pcase (easy-kill-get bounds)
    (`(nil . nil))
    (`(,beg . ,end)
     (apply 'easy-kill-adjust-candidate (easy-kill-get thing)
            (save-mark-and-excursion
             (or (memq last-command '(easy-kill-er-expand easy-kill-er-unexpand))
                 (setq easy-kill-er-history nil))
             (if (< 0 arg)
                 (let ((er/history))
                   (push-mark beg t t)
                   (goto-char end)
                   (loop repeat arg
                         until (let ((prev (list (point) (mark))))
                                 (or (eq 'early-exit (er--expand-region-1))
                                     (ignore (push prev easy-kill-er-history)))))
                   (list (point) (mark)))
               (loop repeat (1- (if (zerop arg) (length easy-kill-er-history) arg))
                     do (pop easy-kill-er-history))
               (pop easy-kill-er-history)))))))

;;;###autoload
(defun easy-kill-er-unexpand (arg)
  "Undo `easy-kill-er-expand'.

This applies the `er/contract-region' effect to the
current candidate ARG times."
  (interactive "p")
  (easy-kill-er-expand (- arg)))

;;;###autoload
(with-eval-after-load 'multiple-cursors
  (add-to-list 'mc--default-cmds-to-run-for-all 'easy-kill-er-expand)
  (add-to-list 'mc--default-cmds-to-run-for-all 'easy-kill-er-unexpand)
  (add-to-list 'mc/cursor-specific-vars 'easy-kill-er-history))

(provide 'easy-kill-er)
;;; easy-kill-er.el ends here
