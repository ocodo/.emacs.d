;;; helm-make.el --- Select a Makefile target with helm

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/helm-make
;; Package-Version: 20150511.53
;; Version: 0.1.0
;; Package-Requires: ((helm "1.5.3") (projectile "0.11.0"))
;; Keywords: makefile

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A call to `helm-make' will give you a `helm' selection of this directory
;; Makefile's targets.  Selecting a target will call `compile' on it.

;;; Code:

(require 'helm)
(require 'helm-match-plugin)

(defgroup helm-make nil
  "Select a Makefile target with helm."
  :group 'convenience)

(defcustom helm-make-do-save nil
  "If t, save all open buffers visiting files from Makefile's directory."
  :type 'boolean
  :group 'helm-make)

(defcustom helm-make-build-dir ""
  "Specify a build directory for an out of source build.
The path should be relative to the project root."
  :type '(string)
  :group 'helm-make)
(make-variable-buffer-local 'helm-make-build-dir)

(defvar helm-make-command nil
  "Store the make command.")

(defun helm-make-action (target)
  "Make TARGET."
  (compile (format helm-make-command target)))

(defcustom helm-make-completion-method 'helm
  "Method to select a candidate from a list of strings."
  :type '(choice
          (const :tag "Helm" helm)
          (const :tag "Ido" ido)
          (const :tag "Ivy" ivy)))

;;;###autoload
(defun helm-make (&optional arg)
  "Call \"make -j ARG target\". Target is selected with completion."
  (interactive "p")
  "make %s"
  (setq helm-make-command (format "make -j%d %%s" arg))
  (helm--make
   "Makefile"))

(defun helm--make (makefile)
  "Call make for MAKEFILE."
  (let ((file (expand-file-name makefile))
        targets)
    (if (file-exists-p file)
        (progn
          (when helm-make-do-save
            (let* ((regex (format "^%s" default-directory))
                   (buffers
                    (cl-remove-if-not
                     (lambda (b)
                       (let ((name (buffer-file-name b)))
                         (and name
                              (string-match regex (expand-file-name name)))))
                     (buffer-list))))
              (mapc
               (lambda (b)
                 (with-current-buffer b
                   (save-buffer)))
               buffers)))
          (with-helm-default-directory (file-name-directory file)
              (with-temp-buffer
                (insert-file-contents file)
                (goto-char (point-min))
                (let (targets target)
                  (while (re-search-forward "^\\([^: \n]+\\):" nil t)
                    (let ((str (match-string 1)))
                      (unless (string-match "^\\." str)
                        (push str targets))))
                  (setq targets (nreverse targets))
                  (cl-case helm-make-completion-method
                    (helm
                     (helm :sources
                           `((name . "Targets")
                             (candidates . ,targets)
                             (action . helm-make-action))))
                    (ivy
                     (when (setq target (ivy-read "Target: " targets))
                       (helm-make-action target)))
                    (ido
                     (when (setq target (ido-completing-read "Target: " targets))
                       (helm-make-action target))))))))
      (error "No Makefile in %s" default-directory))))

;;;###autoload
(defun helm-make-projectile (&optional arg)
  "Call `helm-make' for `projectile-project-root'.
ARG specifies the number of cores."
  (interactive "p")
  (require 'projectile)
  (setq helm-make-command (format "make -j%d %%s" arg))
  (let ((makefile (expand-file-name
                   "Makefile"
		   (concat (projectile-project-root) helm-make-build-dir))))

    (helm--make
     (if (file-exists-p makefile) makefile "Makefile"))))

(provide 'helm-make)

;;; helm-make.el ends here
