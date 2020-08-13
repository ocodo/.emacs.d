;;; pickle.el --- Major mode for editing cucumber gherkin files.  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Matthew Carter <m@ahungry.com>

;; Author: Matthew Carter <m@ahungry.com>
;; Maintainer: Matthew Carter <m@ahungry.com>
;; URL: https://github.com/ahungry/pickle-mode
;; Package-Version: 20190923.354
;; Package-Commit: 3a0a717f2a24827667f34bc53830a3b81cd57460
;; Version: 0.0.3
;; Date: 2018-02-15
;; Keywords: ahungry languages cucumber gherkin
;; Package-Requires: ((emacs "25.1") (cl-lib "0.6.1"))

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for handling Gherkin files and syntax highlighting.
;;
;; This mode is not coupled to an underlying Cucumber system such as feature-mode.
;;
;; If you want a full-fledged IDE like experience, see: feature-mode (no affiliation).
;;
;; If you just want syntax highlighting in a lightweight mode, use this.
;;

;;; Code:

(require 'cl-lib)

(defvar pickle-mode-default-tab-width 2)

(defconst pickle-mode-font-lock-keywords-1
  (list
   '("#.*" . font-lock-comment-face)
   '(" \"\\(.*?\\)\" " . font-lock-variable-name-face)
   '("\\(When\\|Then\\|Given\\)" . font-lock-keyword-face)
   '("\\(\\w+\\)(" 1 font-lock-function-name-face)
   '("Feature: \\(.*\\)" 1 font-lock-variable-name-face)
   '("Scenario: \\(.*\\)" 1 font-lock-preprocessor-face)
   '("\\(.*?\\): " . font-lock-type-face)
   '(") is \\(\\w+\\)" 1 font-lock-preprocessor-face)
   ))

(defvar default-tab-width 2)

;; This is easy because the syntax just cascades the indent until it
;; resets to 0 again.  No need to go back one level at a time etc.
(defun pickle-indent-line ()
  "Properly indent based on what we're looking at."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t)
          cur-indent)
      (if (looking-at "^[ \t]*Feature:")
          (setq cur-indent 0)
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*\\(Feature\\|Scenario\\):")
                (progn
                  (setq cur-indent (+ default-tab-width (current-indentation)))
                  (setq not-indented nil))
              (if (bobp) (setq not-indented nil))
              )))
        )
      (if cur-indent (indent-line-to cur-indent) (indent-line-to 0))))
  )

;;;###autoload
(define-derived-mode pickle-mode text-mode "Pickle" ()
  "Major mode for editing Gherkin (Cucumber) files."
  :group 'languages
  (set (make-local-variable 'indent-line-function) 'pickle-indent-line)
  (set (make-local-variable 'font-lock-defaults)
       '(pickle-mode-font-lock-keywords-1)))

;;;###autoload
(defun pickle-config ()
  "Default pickle setup and bindings."
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.feature\\'" . pickle-mode)))

(provide 'pickle)

;;; pickle.el ends here
