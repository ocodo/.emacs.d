;;; ido-at-point.el --- ido-style completion-at-point -*- lexical-binding: t; -*-

;; Copyright (C) 2013 katspaugh

;; Author: katspaugh
;; Keywords: convenience, abbrev
;; URL: https://github.com/katspaugh/ido-at-point
;; Version: 20131221.1344
;; X-Original-Version: 0.0.3
;; Package-Requires: ((emacs "24"))

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

;; This package is an alternative frontend for `completion-at-point'.
;; It replaces the standard completions buffer with ido prompt.
;; Press <M-tab> or <C-M-i> to complete a symbol at point.

;;; Installation:

;; (require 'ido-at-point) ; unless installed from a package
;; (ido-at-point-mode)

;;; Code:

(require 'ido)

(defvar ido-at-point-partial t
  "If nil, don't complete partial match on the first completion attempt.")

(defvar ido-at-point-fuzzy nil
  "If t, use fuzzy completion for abbreviations.

For example, this would suggest \"ido-at-point-complete\" for the
query \"iapc\".")

(defvar ido-at-point-use-helm nil
  "If t, use helm completion frontend instead of ido.")

(defun ido-at-point-complete (_ start end collection &optional predicate)
  "Completion for symbol at point using `ido-completing-read'."
  (let* ((input (buffer-substring-no-properties start end))
         (choices (all-completions
                   input
                   (if ido-at-point-fuzzy
                       (apply-partially 'ido-at-point-fuzzy-match collection)
                     collection)
                   predicate)))
    ;; No candidates
    (if (null choices)
        (message "No match")
      ;; A single candidate
      (if (null (cdr choices))
          (ido-at-point-insert end input choices (car choices))
        ;; Many candidates
        (let ((common (try-completion input choices)))
          (if (and ido-at-point-partial
                   (stringp common) (not (string= common input)))
              (ido-at-point-insert end input choices common)
            (ido-at-point-do-read end common choices)))))))

(defun ido-at-point-do-read (&rest args)
  (if ido-at-point-use-helm
      (apply #'ido-at-point-helm-read args)
    (apply #'ido-at-point-read args)))

(defun ido-at-point-read (end common choices)
  (run-with-idle-timer
   0 nil
   (lambda ()
     (ido-at-point-insert
      end common choices
      (ido-completing-read
       "" choices nil nil common)))))

(defun ido-at-point-helm-read (end common choices)
  (run-with-idle-timer
   0 nil
   (lambda ()
     (ido-at-point-insert
      end common choices
      (helm-comp-read
       "" choices
       :initial-input common
       :must-match t
       :alistp nil)))))

(defun ido-at-point-insert (end common choices completion)
  "Replaces text in buffer from END back to common part length with COMPLETION."
  ;; Completion text can have a property of `(face completions-common-part)'
  ;; which we'll use to determine whether the completion contains
  ;; the common part.
  ;; Note that not all completions come with text properties.
  (let ((len (or (next-property-change 0 (car choices)) (length common) 0)))
    (goto-char end)
    (delete-region (- end len) end)
    (insert completion)))

(defun ido-at-point-fuzzy-match (collection input &rest args)
  (let ((matched (list))
        (fuzzy-target
         (mapconcat #'regexp-quote (split-string input "" t) ".*?")))
    (mapc
     (lambda (ob)
       (let ((name (if (symbolp ob) (symbol-name ob) ob)))
         (when (string-match-p fuzzy-target name)
           (push name matched))))
     collection)
    matched))

(defun ido-at-point-mode-set (enable)
  (if enable
      (add-to-list 'completion-in-region-functions
                   'ido-at-point-complete)
    (setq completion-in-region-functions
          (delq 'ido-at-point-complete
                completion-in-region-functions))))

;;;###autoload
(define-minor-mode ido-at-point-mode
  "Global minor mode to use ido for `completion-at-point'.

When called interactively, toggle `ido-at-point-mode'.  With
prefix ARG, enable `ido-at-point-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `ido-at-point-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`ido-at-point-mode'.  Otherwise behave as if called
interactively.

With `ido-at-point-mode' use ido for `completion-at-point'."
  :variable ((memq 'ido-at-point-complete
                   completion-in-region-functions)
             .
             ido-at-point-mode-set))

(provide 'ido-at-point)

;;; ido-at-point.el ends here
