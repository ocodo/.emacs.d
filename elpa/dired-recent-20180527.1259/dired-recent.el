;;; dired-recent.el --- Dired visited paths history     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wojciech Siewierski

;; Author: Wojciech Siewierski <wojciech dot siewierski at onet dot pl>
;; URL: https://github.com/vifon/dired-recent
;; Package-Version: 20180527.1259
;; Keywords: files
;; Version: 0.9
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple history keeping for dired buffers.  All the visited
;; directories get saved for reuse later.  Works great with Ivy and
;; other `completing-read' replacements.

;;  HOW TO USE IT:
;;
;;   (require 'dired-recent)
;;   (dired-recent-mode 1)
;;
;;  C-x C-d (`dired-recent-open')

;;; Code:

(require 'seq)

(defgroup dired-recent nil
  "Dired visited paths history."
  :group 'dired)

(defvar dired-recent-directories nil
  "List of the directories recently visited with `dired'.")

(defcustom dired-recent-directories-file (locate-user-emacs-file "dired-history")
  "File with the directories recently visited with dired."
  :type 'file)

(defcustom dired-recent-ignored-prefix nil
  "Directories ignored by `dired-recent-mode'.

A single string or list of strings.  Prefixes ignored by
`dired-recent-mode'.  Should include the trailing slash if the
prefix should be treated as a complete directory."
  :type '(repeat directory))

(defcustom dired-recent-max-directories nil
  "How many last directories should be remembered.

nil means to remember all."
  :type '(choice
          (const :tag "All" nil)
          (integer)))

;;;###autoload
(defun dired-recent-open ()
  "Show the dired history.  See: `dired-recent-mode'."
  (interactive)
  (unless dired-recent-directories
    (dired-recent-load-list))
  (dired (completing-read "Dired recent: " dired-recent-directories)))

(defun dired-recent-ignored-p (path prefix)
  "Check if PATH starts with PREFIX and should be ignored by the dired history.

PREFIX is a list of paths that should not be stored in the dired history."
  (when prefix
    (or (string-prefix-p (car prefix) path)
        (dired-recent-ignored-p path (cdr prefix)))))

(defun dired-recent-path-save (&optional path)
  "Add PATH or `default-directory' to the dired history.

Remove the last elements as appropriate according to
`dired-recent-max-directories'."
  (let ((path (or path default-directory)))
    (unless (dired-recent-ignored-p (file-name-as-directory path)
                                    dired-recent-ignored-prefix)
      (setq dired-recent-directories
            (let ((new-list (cons path
                                  (delete path dired-recent-directories))))
              (if dired-recent-max-directories
                  (seq-take new-list dired-recent-max-directories)
                new-list))))))

;;; The default C-x C-d (`list-directory') is utterly useless. I took
;;; the liberty to use this handy keybinding.
(defvar dired-recent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-d") #'dired-recent-open)
    map))

(defun dired-recent-load-list ()
  "Load the dired history from `dired-recent-directories-file'."
  (interactive)
  (when (file-readable-p dired-recent-directories-file)
    (with-temp-buffer
      (insert-file-contents dired-recent-directories-file)
      (goto-char (point-min))
      (setq dired-recent-directories (read (current-buffer))))))

(defun dired-recent-save-list ()
  "Save the dired history to `dired-recent-directories-file'."
  (interactive)
  (with-temp-file dired-recent-directories-file
    (prin1 dired-recent-directories (current-buffer))))

;;;###autoload
(define-minor-mode dired-recent-mode
  "Toggle `dired-recent-mode' on or off.
Turn `dired-recent-mode' if ARG is positive, off otherwise.
Turning it on makes dired save each opened path."
  :keymap dired-recent-mode-map
  :global t
  :require 'dired-recent
  (if dired-recent-mode
      (progn
        (dired-recent-load-list)
        (add-hook 'dired-mode-hook #'dired-recent-path-save)
        (add-hook 'kill-emacs-hook #'dired-recent-save-list))
    (remove-hook 'dired-mode-hook #'dired-recent-path-save)
    (remove-hook 'kill-emacs-hook #'dired-recent-save-list)
    (dired-recent-save-list)))


(provide 'dired-recent)
;;; dired-recent.el ends here
