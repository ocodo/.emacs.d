;;; closure-lint-mode.el --- minor mode for the Closure Linter

;; Copyright (C) 2010 Roman Scherer

;; Author:  Roman Scherer <roman@burningswell.com>
;; Maintainer: Roman Scherer <roman@burningswell.com>
;; Created: 18 Nov 2010
;; Version: 20101118.2224
;; X-Original-Version: 0.1
;; Keywords: tools closure javascript lint flymake
;; Url: https://github.com/r0man/closure-lint-mode

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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

;; An Emacs minor mode to check and fix the syntax of Javascript
;; buffers with the Closure Linter.
;;
;; The mode uses flymake to check the syntax of Javascript buffers
;; with the "gjslint" program. Errors will be highlighted via flymake
;; and some of them can be resolved by calling the "fixjsstyle"
;; program.
;;
;; Key bindings:
;;
;; C-c C-e  -  Show the error message of a line with errors.
;; C-c C-f  -  Fix the current buffer by calling fixjsstyle.

;;; Code:

(require 'flymake)

(defcustom closure-lint-gjs-lint "gjslint"
  "The name of the Closure Linter program."
  :type 'string
  :group 'closure-lint)

(defcustom closure-lint-fix-js-style "fixjsstyle"
  "The name of the Closure Fix JS Style program."
  :type 'string
  :group 'closure-lint)

(defvar closure-lint-mode-map
  (let ((map (make-sparse-keymap)))    
    (define-key map "\C-c\C-e" 'flymake-display-err-menu-for-current-line)
    (define-key map "\C-c\C-f" 'closure-lint-fix-buffer)
    map)
  "The keymap used in `closure-lint-mode buffers.")

;;;###autoload 
(define-minor-mode closure-lint-mode
  "Closure Lint mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode."
  :init-value nil
  :lighter " GJSLint"
  :keymap '(("\C-c\C-f" 'closure-lint-fix-buffer))
  :group 'closure-lint)

(defun closure-lint-find-file-hook ()
  "Hook function to enable closure-lint-mode and flymake-mode if
the current buffer's file is a Javascript file."
  (when (string-match "\\.js$" buffer-file-name)    
    (closure-lint-mode 't)
    (flymake-mode 't)))

(defun closure-lint-fix-file (filename)
  "Run the \"fixjsstyle\" program on `filename."
  (let* ((command (concat closure-lint-fix-js-style " " filename))
         (output (shell-command-to-string command)))
    (message output)))

(defun closure-lint-fix-buffer ()
  "Run the \"fixjsstyle\" program on the content of the current
  buffer and replace the current buffer's content with the fixed
  content."
  (interactive)
  (let* ((current-point (point))
         (temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace)))
    (progn
      (closure-lint-fix-file temp-file)
      (erase-buffer)
      (insert-file-contents temp-file)
      (goto-char current-point)
      (delete-file temp-file))))

(defun closure-lint-flymake-init ()
  "Flymake init function using the closure linter."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list closure-lint-gjs-lint (list (expand-file-name local-file)))))

(add-hook 'find-file-hooks 'closure-lint-find-file-hook)

(add-to-list 'flymake-allowed-file-name-masks
             '(".+\\.js$" closure-lint-flymake-init flymake-simple-cleanup flymake-get-real-file-name))

(add-to-list 'flymake-err-line-patterns
             '("^Line \\([[:digit:]]+\\), E:\\([[:digit:]]+\\):\\(.+\\)$" nil 1 nil 3))

(provide 'closure-lint-mode)

;;; closure-lint-mode.el ends here
