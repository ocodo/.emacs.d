;;; ego-mode.el --- a major mode for managing posts in ego

;; Copyright (C)  2016 DarkSun

;; Author:   DarkSun   <lujun9972@gmail.com>
;; Keywords: org-mode, convenience, beautify
;; Homepage: https://github.com/emacs-china/EGO

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

;; a major mode for managing posts in ego

;;; Code:

(require 'ego)

(defun ego-mode--select-current-project ()
  (setq ego--current-project-name
        (or ego--default-project-name
            (ido-completing-read "Which project do you want to publish? "
                                 (delete-dups
                                  (mapcar 'car ego-project-config-alist))
                                 nil t nil nil ego--last-project-name))))

;; (ego-mode-list-all-posts)
(defun ego-mode-list-all-posts ()
  "list all posts"
  (ego-mode--select-current-project)
  (let* ((repo-dir (ego--get-repository-directory))
         (repo-files-function (ego--get-config-option :repo-files-function))
         (ignore-file-name-regexp (ego--get-config-option :ignore-file-name-regexp))
         (repo-files (-filter (lambda (string)
                                   (not (string-match ignore-file-name-regexp string)))
                                 (when (functionp repo-files-function)
                                   (funcall repo-files-function repo-dir))))
         entries)
    (dolist (file repo-files)
	  (with-temp-buffer
		(insert-file-contents file)
		(goto-char (point-min))
		(let ((title (propertize (ego--get-title) 'help-echo file))
			  (author (or (ego--read-org-option "AUTHOR")
						  user-full-name "Unknown Author"))
			  (description  (or (ego--read-org-option "DESCRIPTION")
								"No DESCRIPTION"))
			  (category (ego--get-category file)))
		  (push (list nil (vector title author description category )) entries))))
	(reverse entries)))

(defun ego-mode--get-file-path ()
  (save-excursion
	(move-beginning-of-line nil)
	(get-text-property (point) 'help-echo)));help-echo is also the file path

(defun ego-mode-edit ()
  (interactive)
  (find-file-other-window (ego-mode--get-file-path)))

(defun ego-mode-do-publication ()
  (interactive)
  (ego-do-publication ego--current-project-name))

(defun ego-mode-test-current-page ()
  (interactive)
  (find-file-other-window (ego-mode--get-file-path))
  (ego-test-current-page ego--current-project-name))

(define-derived-mode ego-mode tabulated-list-mode "ego-mode"
  "mode for managing ego post"
  (setq tabulated-list-format [("title" 30 nil)
							   ("author" 10 t)
							   ("description" 40 t)
							   ("category" 10 t)]
		tabulated-list-entries 'ego-mode-list-all-posts)
  (tabulated-list-init-header)
  (define-key ego-mode-map (kbd "e") 'ego-mode-edit)
  (define-key ego-mode-map (kbd "<RET>") 'ego-mode-edit)
  (define-key ego-mode-map (kbd "<down-mouse-1>") 'ego-mode-edit)
  (define-key ego-mode-map (kbd "p") 'ego-mode-do-publication)
  (define-key ego-mode-map (kbd "t") 'ego-mode-test-current-page)
  (define-key ego-mode-map (kbd "+") 'ego-new-post)
  (define-key ego-mode-map (kbd "a") 'ego-new-post))

;;;###autoload
(defun ego-list-posts ()
  "list posts"
  (interactive)
  (switch-to-buffer (get-buffer-create "*ego-manager*"))
  (ego-mode)
  (tabulated-list-print t))

(provide 'ego-mode)
