;;; ansible-doc.el --- Ansible documentation Minor Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Sebastian Wiesner <swiesner@lunaryorn.com>

;; Author: Sebastian Wiesner <swiesner@lunaryorn>
;; URL: https://github.com/lunaryorn/ansible-doc.el
;; Keywords: tools, help
;; Version: 20140911.427
;; X-Original-Version: 0.1
;; Package-Requires: ((emacs "24.1"))

;; This file is part of GNU Emacs.

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

;; Ansible Documentation Minor Mode for GNU Emacs.
;;
;; Provide `ansible-doc-mode' which enables documentation lookup for Ansible.
;;
;; Enable with:
;;
;; (add-hook 'yaml-mode-hook #'ansible-doc-mode)

;;; Code:

(defgroup ansible nil
  "Ansible configuration and provisioning system."
  :group 'languages
  :prefix "ansible-")

(defgroup ansible-doc nil
  "Ansible documentation lookup."
  :group 'ansible
  :prefix 'ansible-doc)

(defconst ansible-doc--buffer-name "*ansible-doc %s*"
  "Template for the names of Ansible Doc buffers.")

(defvar ansible-doc--modules nil
  "A list of all known Ansible modules.")

(defun ansible-doc-modules ()
  "Get a list of all known Ansible modules."
  (unless ansible-doc--modules
    (message "Finding Ansible modules...")
    (with-temp-buffer
      (call-process "ansible-doc" nil '(t nil) nil "--list")
      (goto-char (point-max))
      (while (re-search-backward (rx line-start
                                     (group (one-or-more (not (any space))))
                                     (any space)
                                     (one-or-more not-newline)
                                     line-end)
                                 nil 'noerror)
        (push (match-string 1) ansible-doc--modules))))
  ansible-doc--modules)

(defun ansible-doc-read-module (prompt)
  "Read a Ansible module name from minibuffer with PROMPT."
  (let* ((modules (ansible-doc-modules))
         (symbol (thing-at-point 'symbol 'no-properties))
         (default (if (member symbol modules) symbol nil))
         (reply (completing-read prompt modules nil 'require-match
                                 nil nil default)))
    (if (string= reply "") default reply)))

;;;###autoload
(defun ansible-doc (module)
  "Show ansible documentation for MODULE."
  (interactive
   (list (ansible-doc-read-module "Documentation for Ansible Module: ")))
  (let* ((buffer-name (format ansible-doc--buffer-name module))
         (buffer (get-buffer buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create buffer-name))
      (with-current-buffer buffer
        (setq buffer-read-only t)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (call-process "ansible-doc" nil t t module))
        (goto-char (point-min))))
    (view-buffer-other-window buffer)))

(defvar ansible-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ?") #'ansible-doc)
    map)
  "Keymap for `ansible-mode'.")

;;;###autoload
(define-minor-mode ansible-doc-mode
  "Minor mode for Ansible documentation.

When called interactively, toggle `ansible-doc-mode'.  With
prefix ARG, enable `ansible-doc-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `ansible-doc-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`ansible-doc-mode'.  Otherwise behave as if called interactively.

In `ansible-doc-mode' provide the following keybindings for
Ansible documentation lookup:

\\{ansible-doc-mode-map}"
  :init-value nil
  :keymap ansible-doc-mode-map
  :lighter " ADoc"
  :group 'ansible-doc
  :require 'ansible-doc)

(provide 'ansible-doc)

;;; ansible-doc.el ends here
