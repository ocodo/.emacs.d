;;; edit-list.el --- edit a single list

;; Copyright (C) 2008  Michael Olson

;; Author: Michael Olson <mwolson@gnu.org>
;; Date: Mon 31-Mar-2008
;; Version: 0.3
;; Package-Version: 20100930.1443
;; URL: http://mwolson.org/static/dist/elisp/edit-list.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
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

;; So you've just added an incorrect entry to auto-mode-alist and want
;; to fix it quickly.  `M-x edit-list RET auto-mode-alist RET' to the
;; rescue.  Make your changes and hit either `C-x C-s' or `C-c C-c'
;; when done.  Or just kill the buffer if you change your mind.

;;; History:

;; 0.3:
;;
;; - Grab list name at point.
;;
;; - Limit completions to lists.
;;
;; - Set buffer-modified-p to nil when buffer is ready for user edits.
;;
;; - Handle edge case where argument passed to `edit-list' is an
;;   unbound symbol.

;; 0.2: Add completion for symbols, header, footer, and (require 'pp).

;; 0.1: Initial release.

;;; Contributors:

;; rubikitch (rubikitch AT ruby-lang.org) provided a solution for
;; completing symbol names.

;; Rupert Swarbrick (rswarbrick AT gmail.com) provided the initial
;; implementations for defaulting to the list name at point and
;; limiting the completions to names of lists.

;;; Code:

(require 'pp)

(defgroup edit-list nil
  "Options controlling the behavior of Edit List Mode.
Edit List mode allows the editing of a single list in a buffer."
  :group 'convenience)

(defcustom edit-list-completing-read-function 'completing-read
  "Function to use for completing read in `edit-list'."
  :type 'function
  :group 'edit-list)

(defvar edit-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'edit-list-done)
    (define-key map (kbd "C-x C-s") 'edit-list-done)

    map))

(defvar edit-list-list-name nil
  "Name of list currently being edited.")
(make-variable-buffer-local 'edit-list-list-name)

(define-derived-mode edit-list-mode emacs-lisp-mode "Edit-List"
  "Major mode to edit a single list.
\\{edit-list-mode-map}")

(defun edit-list-read-list-name (prompt)
  "Read the name of a list from the user."
  (let ((sym (cond ((fboundp 'symbol-nearest-point) ; thingatpt+.el
                    (symbol-nearest-point))
                   ((fboundp 'symbol-at-point)      ; thingatpt.el
                    (symbol-at-point))
                   (t nil))))
    (unless (and (boundp sym) (consp (symbol-value sym)))
      (setq sym nil))
    (intern-soft (funcall edit-list-completing-read-function
                          prompt obarray
                          (lambda (x)
                            (and (boundp x)
                                 (symbol-value x)
                                 (consp (symbol-value x))))
                          t
                          (and sym (symbol-name sym))))))

(defun edit-list (list-name)
  "Edit a list called LIST-NAME interactively."
  (interactive (list (edit-list-read-list-name "Edit list: ")))
  (unless (and list-name (symbolp list-name))
    (error "Given list name is not a symbol"))
  (unless (boundp list-name)
    (error "Given symbol is not bound"))
  (let ((list-val (symbol-value list-name)))
    (unless (consp list-val)
      (error "Given symbol does not contain a list"))
    (let ((buffer (generate-new-buffer (format "*Edit-List: %s*" list-name))))
      (with-current-buffer buffer
        (edit-list-mode)
        (setq edit-list-list-name list-name)
        (let ((print-escape-newlines pp-escape-newlines)
              (print-quoted t))
          (prin1 list-val (current-buffer)))
        (pp-buffer)
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil))
      (switch-to-buffer buffer)
      (message "Make changes and hit `C-c C-c' when done"))))

(defun edit-list-done ()
  "Save the given buffer back to the original list.
This finishes the work begun by `edit-list'."
  (interactive)
  (unless (and edit-list-list-name (symbolp edit-list-list-name)
               (derived-mode-p 'edit-list-mode))
    (error "This is not an Edit-List buffer"))
  (goto-char (point-min))
  (let ((name edit-list-list-name))
    (set name (read (current-buffer)))
    (kill-buffer (current-buffer))
    (message "Saved changes to list `%s'" name)))

(provide 'edit-list)

;; edit-list.el ends here

;;; edit-list.el ends here
