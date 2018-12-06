;;; x-path-walker.el --- Navigation feature for JSON/XML/HTML based on path (imenu like)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015

;; Author:  <lompik@ArchOrion>
;; Keywords: convenience
;; Package-Requires: ((helm-core "1.9.2"))

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

;;

;;; Code:

(require 'helm)

(defvar x-path-walker-source-dir (concat (if load-file-name
                                             (file-name-directory load-file-name)
                                           default-directory)))

(defvar x-path-walker-objects-separators ".")

(defun x-path-walker-command ()
  (list (concat "PYTHONPATH=" x-path-walker-source-dir)
        (executable-find "python3")
        "-m xpathwalker"))

(defvar x-path-walker-verbose nil)

(defun x-path-build-cmd-path (args)
  (let ((cmdpython (x-path-walker-command)))
    (mapconcat #'identity
             (append cmdpython
                     args)
             " ")))

(defun x-path-run-py-script (args)
  (let ((cmd (x-path-build-cmd-path args)))
    (shell-command-to-string cmd)))

(defun x-path-get-mode ()
  (let ((mm (pcase major-mode
              (`json-mode "JSON")
              (`xml-mode "XML")
              (`nxml-mode "XML")
              (`html-mode "HTML")
              (`web-mode "HTML")
              (code nil))))
    (unless (and (buffer-file-name) mm)
      (keyboard-quit))
    mm))

(defun x-path-walker-ask ()
  (interactive)
  (if (and (buffer-modified-p)
           (yes-or-no-p "Buffer will be linted. Do you want to save the current buffer (required)?"))
      (save-current-buffer)
    (if (buffer-modified-p)
        (keyboard-quit))))

(defun x-path-walker-jump-path (path)
  (let* ((mode (x-path-get-mode))
         (path (if (not (bound-and-true-p x-path-walker-verbose))
                   path
                 (if (or (string= mode "HTML")
                         (string= mode "XML"))
                     (car (split-string path " | "))
                   (if (and (string= mode "JSON") )
                       (car (cdr (split-string path " | ")))
                     ""))))
         (line (replace-regexp-in-string
                "\n$" ""
                (x-path-run-py-script `("-x"
                                        ,(shell-quote-argument path)
                                        "-m"
                                        ,mode
                                        ,(buffer-file-name) )))))
    (if (string= mode "JSON")
        (progn  (x-path-walker-ask)
                (erase-buffer)
                (shell-command-on-region (point-min) (point-max)
                                         (concat "python3 -m json.tool "
                                                 (buffer-file-name))
                                         (current-buffer))))
    (unless (or (string= "" line))
      (goto-char 0)
      (forward-line (1- (string-to-number line)))
      (recenter)
      (back-to-indentation))))

;;;###autoload
(defun helm-x-path-walker()
  (interactive)
  (let* ((mode (x-path-get-mode))
         (file (buffer-file-name))
         (cmd-line `(,(if (bound-and-true-p x-path-walker-verbose)
                          "-a"
                        "")
                     "-m"
                     ,mode
                     ,file ))
         (cands  (split-string (x-path-run-py-script cmd-line)"\n")))
    (helm
     :sources (helm-build-sync-source "PATH-WALKER"
                :keymap helm-map
                :candidates  cands
                :candidate-number-limit 500
                :action (helm-make-actions
                         "Jump to path" 'x-path-walker-jump-path))
     :prompt "Select Path:"
     :resume 'noresume
     :keymap helm-map
     :buffer "*helm path-walker*")))

(provide 'x-path-walker)
;;; x-path-walker.el ends here
