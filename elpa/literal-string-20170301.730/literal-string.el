;;; literal-string.el --- edit string literals in a dedicated buffer -*- lexical-binding: t -*-

;; Copyright 2017 Joost Diepenmaat

;; Author: Joost Diepenmaat <joost@zeekat.nl>
;; Keywords: lisp, tools, docs
;; URL: https://github.com/joodie/literal-string-mode/
;; Package-Requires: ((markdown-mode "2.0") (emacs "25"))
;; Package-Version: 20170301.730
;; Package-X-Original-Version: 0.1

;; Contributors:
;; - Joost Diepenmaat
;; - Syohei YOSHIDA

;;; Commentary:
;;;
;;; Literal String Mode is a minor mode for editing multi-line literal
;;; strings in a dedicated buffer.
;;;
;;; When enabled, edit the literal string at point using C-c '
;;; (literal-string-edit-string), this will copy the (unescaped and
;;; deindented) content of the string to a dedicated literal string
;;; editing buffer that has Literal String Editing Mode (a minor mode)
;;; enabled.
;;;
;;; To exit the current literal string buffer copy the edited string
;;; back into the original source buffer with correct quoting and
;;; escape sequences, press C-c '
;;; (literal-string-edit-string-exit).
;;;
;;; To discard your changes to the editing buffer, press C-c C-k
;;; (literal-string-edit-string-abort)
;;;
;;; To enable literal-string-mode in your preferred programming modes,
;;; turn it on using the relevant mode hooks.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'subr-x)

(defun literal-string--inside-string? ()
  "Return non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (nth 3 (syntax-ppss)))

(defun literal-string--region ()
  "Return start and end markers of current literal string.
`nil` if point is not at or in a string literal"
  (when (literal-string--inside-string?)
    (save-excursion
      (search-forward-regexp "[^\\\\]\"")
      (backward-char 1)
      (let ((end (point-marker)))
        (search-backward-regexp "[^\\\\]\"")
        (forward-char 2)
        (let ((start (point-marker)))
          (list start end))))))
(defun literal-string--docstring-indent-level ()
  "Find indent level of current buffer after first line."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((indent-count nil))
      (while (not (eobp))
        (when (not (looking-at "[[:space:]]*$"))
          (setq indent-count (if indent-count
                                 (min indent-count (current-indentation))
                               (current-indentation))))
        (forward-line 1))
      indent-count)))

(defun literal-string--docstring-deindent ()
  "Remove extraneous indentation of lines after the first one.
Returns the amount of indentation removed."
  (when-let (level (literal-string--docstring-indent-level))
    (when (not (zerop level))
      (indent-rigidly (point-min) (point-max) (- level))
      level)))

(defvar-local literal-string-source-indent-level nil)
(defvar-local literal-string-source-region nil)

(defun literal-string--docstring-reindent ()
  "Re-indent literal string editing buffer.
Uses indent level removed by `literal-string--docstring-deindent`."
  (when-let (level literal-string-source-indent-level)
    (when (not (zerop level))
      (save-excursion
        (goto-char (point-min))
        (forward-line)
        (when (not (eobp))
          (indent-rigidly (point) (point-max) level))))))

(defun literal-string--replace-all (from to)
  "Replace all occurences of `FROM` TO `to`."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward from nil t)
      (replace-match to t t))))

(defun literal-string--unescape ()
  "Unescape quotes and backslashes in buffer."
  (literal-string--replace-all "\\\"" "\"")
  (literal-string--replace-all "\\\\" "\\"))

(defun literal-string--escape ()
  "Escape quotes and backslashes in buffer."
  (literal-string--replace-all "\\" "\\\\")
  (literal-string--replace-all "\"" "\\\""))

(defgroup literal-string
  ()
  "Minor modes for editing string literals in source code."
  :group 'tools :group 'lisp)

(defcustom literal-string-fill-column 62
  "Fill column to use in the string editing buffer.
`nil` means do not set `fill-column`"
  :type 'integer)

;;;###autoload
(defun literal-string-edit-string ()
  "Indent current string literal.
Removes docstring indentation"
  (interactive)
  (require 'markdown-mode)
  (if-let (region (literal-string--region))
    (let ((edit-buffer (get-buffer-create (format "*Edit Literal String <%s>*" (buffer-name)))))
      (apply #'copy-to-buffer edit-buffer region)
      (switch-to-buffer edit-buffer)
      (markdown-mode) ; first - changing major mode clears local vars!
      (setq literal-string-source-region region
            literal-string-source-indent-level (literal-string--docstring-deindent))
      (literal-string--unescape)
      (literal-string-editing-mode t))
    (user-error "Not at a string literal")))

(defun literal-string-edit-string-exit ()
  "Quit editing the current literal string bufffer.
Replaces content of original string literal with content of
buffer."
  (interactive)
  (if-let (region literal-string-source-region) ; copy buffer-local var
    (let ((string-buffer (current-buffer))
          (source-buffer (marker-buffer (car region))))
      (literal-string--escape)
      (literal-string--docstring-reindent)
      (switch-to-buffer source-buffer)
      (apply #'delete-region region)
      (insert-buffer-substring string-buffer)
      (set-marker (car region) nil nil)
      (set-marker (cadr region) nil nil)
      (kill-buffer string-buffer))
    (user-error "Not editing a string literal")))

(defun literal-string-edit-string-abort ()
  "Quit and discard edits to the current literal string buffer."
  (interactive)
  (when literal-string-editing-mode
    (kill-buffer (current-buffer))))

(defvar literal-string-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") 'literal-string-edit-string)
    map))

;;;###autoload
(define-minor-mode literal-string-mode
  "A minor mode for editing literal (documentation) strings in
source code.

Provides support for editing strings formatted in markdown,
automatic (un)escaping of quotes and docstring indentation."
  :lighter " str"
  :keymap literal-string-mode-keymap)

(defvar literal-string-editing-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") 'literal-string-edit-string-exit)
    (define-key map (kbd "C-c C-k") 'literal-string-edit-string-abort)
    map))

;;;###autoload
(define-minor-mode literal-string-editing-mode
  "Minor mode used in edit buffer by `literal-string-mode`."
  :lighter " edit-str"
  :keymap literal-string-editing-mode-keymap
  (setq-local header-line-format
              (substitute-command-keys
               "Edit, then exit with `\\[literal-string-edit-string-exit]' or abort with \
`\\[literal-string-edit-string-abort]'"))
  (when literal-string-fill-column
    (set-fill-column literal-string-fill-column)))

(provide 'literal-string)

;;; literal-string.el ends here
