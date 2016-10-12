;;; spu-dark.el --- spu backend package   -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2016 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/spu
;;
;;; License:
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
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
;;
;;; Commentary:
;; spu backend package
;;
;;; code:
(require 'cl-lib)
(require 'package)
(eval-when-compile (require 'timp-server))

(defvar spu-log-path (file-name-as-directory
                      (concat
                       (file-name-as-directory (expand-file-name user-emacs-directory))
                       "spu_log"))
  "Path to save SPU packages upgrade log.")

(defvar spu-max-log-number 60
  "Maximum number of SPU package upgrade log.")

(defun spu-dark-set-package-acrhives (archives)
  "Set the `package-acrhives' to ARCHIVES."
  (setq package-archives archives))

;; When user requires prompt
(defun spu-dark-get-package-upgrade-list ()
  "Return a list of package that need upgrade."
  (let (upgrade-list)
    (with-timp-server-inhibited-message
     (package-list-packages)
     (setq upgrade-list (package-menu--find-upgrades)))
  upgrade-list))

;; When user does not require prompt
(defun spu-dark-upgrade-packages ()
  "Upgrade all packages."
  (with-timp-server-inhibited-message
   (package-list-packages))
  (with-current-buffer "*Packages*"
    (with-timp-server-inhibited-message
     (package-menu-mark-upgrades))
    (spu-dark-package-menu-execute)))

(defun spu-dark-package-menu-mark-upgrades (upgrade-list)
  "Mark package in UPGRADE-LIST to be upgraded in the Package Menu.
For each installed package with a newer version available, place
an (I)nstall flag on the available version and a (D)elete flag on
the installed version."
  (interactive)
  (if (null upgrade-list)
      (message "No packages to upgrade.")
    (with-current-buffer "*Packages*"
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((pkg-desc (tabulated-list-get-id))
               (matched (memq (package-desc-name pkg-desc) (mapcar #'package-desc-name upgrade-list)))
               (upgrade (and matched (nth (- (length upgrade-list) (length matched)) upgrade-list))))
          (cond ((null upgrade)
                 (forward-line 1))
                ((equal pkg-desc upgrade)
                 (package-menu-mark-install))
                (t
                 (package-menu-mark-delete)))))
      (spu-dark-package-menu-execute))))


(defun spu-dark-package-menu-execute ()
  "Perform marked Package Menu actions.
Packages marked for installation are downloaded and installed;
packages marked for deletion are removed."
  (interactive)
  (unless (derived-mode-p 'package-menu-mode)
    (error "The current buffer is not in Package Menu mode"))
  
  (let (install-list delete-list cmd pkg-desc installed-list error-list report)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq cmd (char-after))
        (unless (eq cmd ?\s)
          ;; This is the key PKG-DESC.
          (setq pkg-desc (tabulated-list-get-id))
          (cond ((eq cmd ?D)
                 (push pkg-desc delete-list))
                ((eq cmd ?I)
                 (push pkg-desc install-list))))
        (forward-line)))
    (setq report (format "SPU Packages Upgrade Report - %s\n\n%sVersion\t\t\tOperation\tStatus\n"
                         (format-time-string "%H:%M:%S")
                         (concat "Package" (make-string (- 25 (length "Package")) ? ))))
    ;; Install packages
    (dolist (pkg install-list)
      (condition-case nil
          (progn
            (with-timp-server-inhibited-message
             (package-install pkg))
            (push (package-desc-name pkg) installed-list)
            (setq report (concat report (format "%s%s\t\tInstall\t\tSucceeded\n"
                                                (concat (symbol-name (package-desc-name pkg))
                                                        (make-string (max 0 (- 25 (length (symbol-name (package-desc-name pkg))))) ? ))
                                                (package-version-join (package-desc-version pkg))))))
        (error (setq report (concat report (format "%s%s\t\tInstall\t\tFailed\n"
                                                   (concat (symbol-name (package-desc-name pkg))
                                                           (make-string (max 0 (- 25 (length (symbol-name (package-desc-name pkg))))) ? ))
                                                   (package-version-join (package-desc-version pkg)))))
               (cl-pushnew (package-desc-name pkg) error-list))))
    
    ;; Delete packages
    (dolist (pkg delete-list)
      (if (memq (package-desc-name pkg) installed-list)
          (condition-case nil
              (progn
                (with-timp-server-inhibited-message
                 (package-delete pkg))
                (setq report (concat report (format "%s%s\t\tDelete\t\tSucceeded\n"
                                                    (concat (symbol-name (package-desc-name pkg))
                                                            (make-string (max 0 (- 25 (length (symbol-name (package-desc-name pkg))))) ? ))
                                                    (package-version-join (package-desc-version pkg))))))
            (error (setq report (concat report (format "%s%s\t\tDelete\t\tFailed\n"
                                                       (concat (symbol-name (package-desc-name pkg))
                                                               (make-string (max 0 (- 25 (length (symbol-name (package-desc-name pkg))))) ? ))
                                                       (package-version-join (package-desc-version pkg)))))
                   (cl-pushnew (package-desc-name pkg) error-list)))
        (setq report (concat report (format "%s%s\t\tDelete\t\tAborted\n"
                                            (concat (symbol-name (package-desc-name pkg))
                                                    (make-string (max 0 (- 25 (length (symbol-name (package-desc-name pkg))))) ? ))
                                            (package-version-join (package-desc-version pkg)))))))

    (setq report (concat report "\n"))
    
    (when (or installed-list error-list)
        (write-region report nil (concat spu-log-path (format-time-string "%Y%m%d") ".log") t))
    (cons installed-list error-list)))

(defun spu-dark-init ()
 ;; Ensure log path directory exists
 (make-directory spu-log-path t)
 ;; Clean up log path directory if necessary
 (let ((files (directory-files spu-log-path t ".*\\.log")))
   (when (>= (length files) spu-max-log-number)
     (dotimes (_var (- (length files) (/ spu-max-log-number 2)))
       (delete-file (car files))
       (setq files (cdr files))))))

(provide 'spu-dark)
;;; spu-dark.el ends here

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
