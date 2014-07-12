;;; flymake-elixir.el --- A flymake handler for elixir-mode .ex files.     
;;
;; Copyright (C) 2010-2013 Sylvain Benner
;;
;;; Author: Sylvain Benner <syl20bnr@gmail.com>
;;; Created: 10 Apr 2013
;; Version: 20130810.717
;; X-Original-Version: 0.5
;;; Package-Pequires: ((flymake-easy "0.1"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Usage:
;;   (require 'flymake-elixir)
;;   (add-hook 'elixir-mode-hook 'flymake-elixir-load)
;;
;; Based on flymake-ruby, from Steve Purcell
;;
;; Uses flymake-easy, from Steve Purcell

;;; Code:

(require 'flymake-easy)

(defconst flymake-elixir-err-line-patterns
  '(("^\\(** (.*) \\)?\\(.*\.ex\\):\\([0-9]+\\): \\(.*\\)$" 2 3 nil 4)))
(defconst flymake-elixir-warn-regex
  (regexp-opt (list "^redefining" "^export_all" "future reserved" "deprecated"
                    "shadowed" "always matches$" "obsolete$" "unused$") t))

(defvar flymake-elixir-executable "elixirc"
  "The elixir executable to use for syntax checking.")

(defun flymake-elixir-command (filename)
  "Construct a command that flymake can use to check elixir source."
  (list flymake-elixir-executable
        "--ignore-module-conflict" ; needed to prevent from module redefinition warning.
        "+warn_obsolete_guard"
        "+warn_unused_import"
        "+warn_shadow_vars"
        "+warn_export_vars"
        "+strong_validation"
        "+report"
        filename))

;;;###autoload
(defun flymake-elixir-load ()
  "Configure flymake mode to check the current buffer's elixir syntax."
  (interactive)
  (flymake-easy-load 'flymake-elixir-command
                     flymake-elixir-err-line-patterns
                     'tempdir
                     "ex"
                     flymake-elixir-warn-regex))

(provide 'flymake-elixir)
;;; flymake-elixir.el ends here
