;;; anything-git-goto.el --- Quick listing of:

;; This file is not part of Emacs

;; Author: Jose Pablo Barrantes
;; Copyright (C) 2011 Jose Pablo Barrantes
;; Created: 18/Dec/11
;; Version: 0.1.0

;;; Installation:

;; Put this file where you defined your `load-path` directory or just
;; add the following line to your emacs config file:

;; (load-file "/path/to/anything-git-goto.el")

;; Finally require it:

;; (require 'anything-git-goto)

;; Usage:
;; M-x anything-git-goto

;; There is no need to setup load-path with add-to-list if you copy
;; `anything-git-goto.el` to load-path directories.

;; Requirements:

;; http://www.emacswiki.org/emacs/Anything

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'anything)
(require 'vc-git)

;;; --------------------------------------------------------------------
;;; - Customization
;;;
(defvar *anything-git-goto-buffer-name*
  "*Anything git-goto*")

(defvar git-goto-cmd
  "cd %s && git       \
  --no-pager ls-files \
  -cmo                \
  --exclude-standard  \
  --directory")

(defun anything-git-goto-find-git-repo (dir)
  "Recursively search for a .git/ directory."
  (if (string= "/" dir)
      nil ;; not in a git repo
    (if (file-exists-p (expand-file-name ".git/" dir))
        dir
      (anything-git-goto-find-git-repo (expand-file-name "../" dir)))))

(defun anything-git-goto-file  (file-content)
  "Visit the source for the file result."
  (setq full-file-path
           (expand-file-name file-content
                             (expand-file-name
                              (anything-git-goto-find-git-repo file-content)
                              (anything-attr 'pwd))))
  (if (file-exists-p full-file-path)
      (find-file full-file-path))
  (kill-buffer *anything-git-goto-buffer-name*))

(defvar anything-c-source-git-goto
  '((name . "Git goto")
    (init . (lambda ()
              (let ((git-root
                     (anything-git-goto-find-git-repo  default-directory)))
                (and git-root
                    (call-process-shell-command
                     (format git-goto-cmd
                             (anything-git-goto-find-git-repo  default-directory))
                     nil (anything-candidate-buffer 'global))))))
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (action . anything-git-goto-file))
  "Git goto.")

;;;###autoload
(defun anything-git-goto ()
  "Git Find files."
  (interactive)
  (if (anything-git-goto-find-git-repo  default-directory)
      (anything-other-buffer
       '(anything-c-source-git-goto) *anything-git-goto-buffer-name*)
    (message "Not in a git repo")))

(provide 'anything-git-goto)


;;; anything-git-goto.el ends here
