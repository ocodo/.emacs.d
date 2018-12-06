;;; buster-mode.el --- Minor mode to speed up development when writing tests with Buster.js

;; Copyright (C) 2011 Magnar Sveen, Christian Johansen

;; Authors: Magnar Sveen <magnars@gmail.com>
;;          Christian Johansen <christian@cjohansen.no>
;; Keywords: buster testing javascript
;; Package-Version: 20140928.1213

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

;; A work in progress. You can watch us livecode it on http://emacsrocks.com

;; All keybindings in buster-mode start with `C-c C-b` and then a two-letter mnemonic shortcut.

;; * `td`: toggle-deferred will toggle // in the name of the current test.
;; * `tf`: toggle-focus-rocket will toggle => in the name of the current test.
;; * `ra`: run-all-tests

;;; Code:

(require 'compile)

(defun buster-goto-current-test ()
  (search-backward-regexp "[\"'][^ ]* .+[\"']: function" nil t))

(defun buster-toggle-test-name-prefix (prefix)
  (save-excursion
    (buster-goto-current-test)
    (forward-char 1)
    (if (not (looking-at prefix))
        (insert prefix " ")
      (delete-char (length prefix))
      (delete-horizontal-space))))

(defun buster-toggle-deferred ()
  (interactive)
  (buster-toggle-test-name-prefix "//"))

(defun buster-toggle-focus-rocket ()
  (interactive)
  (buster-toggle-test-name-prefix "=>"))

(defvar buster-compile-command "buster-test"
  "Command used to run Buster tests")

(defun buster-run-all-tests ()
  (interactive)
  (compile buster-compile-command t))

(defvar buster-mode-map (make-sparse-keymap)
  "buster-mode keymap")

(define-key buster-mode-map
  (kbd "C-c C-b td") 'buster-toggle-deferred)
(define-key buster-mode-map
  (kbd "C-c C-b tf") 'buster-toggle-focus-rocket)
(define-key buster-mode-map
  (kbd "C-c C-b ra") 'buster-run-all-tests)

(defun buster-mode--clean-up-ansi-mess (&rest ignore)
  (with-current-buffer (buster-mode--compilation-buffer-name)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "[1A" nil t)
        (delete-char -5)
        (delete-char (- (current-column)))))))

(defun buster-mode--compilation-buffer-name (&rest ignore)
  "*buster-test*")

(define-minor-mode buster-mode
  "Buster mode" nil " Buster" buster-mode-map
  (if buster-mode
      (progn
        (add-to-list 'compilation-error-regexp-alist '("(\\([^: ]+\\):\\([0-9]+\\):\\([0-9]+\\))" 1 2 3))
        (set (make-local-variable 'compilation-buffer-name-function) 'buster-mode--compilation-buffer-name)
        (add-hook 'comint-output-filter-functions 'buster-mode--clean-up-ansi-mess t))
    (remove-hook 'comint-output-filter-functions 'buster-mode--clean-up-ansi-mess)))

(provide 'buster-mode)

;;; buster-mode.el ends here
