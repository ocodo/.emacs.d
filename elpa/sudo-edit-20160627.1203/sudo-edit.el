;;; sudo-edit.el --- Utilities for opening files with sudo

;; Copyright (C) 2014 Nathaniel Flath <flat0103@gmail.com>

;; Author: Nathaniel Flath <flat0103@gmail.com>
;; Version: 0.0.1
;; Package-Version: 20160627.1203

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides several utility functions for opening buffers
;; as root using 'sudo'.  They are:

;; sudo-edit
;; sudo-edit-current-file

;; Suggested keybinding:
;; (global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)

;;; Installation

;; To use this mode, put the following in your init.el:
;; (require 'sudo-edit)

;;; License:

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

;;; Code:

;;;###autoload
(defun sudo-edit (&optional arg)
  "Find a file and open it as root."
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;###autoload
(defun sudo-edit-current-file ()
  "Edit the current file as root."
  (interactive)
  (let ((pos (point)))
    (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer))))
    (goto-char pos)))

(provide 'sudo-edit)
;;; sudo-edit.el ends here
