;;; spu.el --- Silently upgrade package in the background   -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2016 Mola-T
;; Author: Mola-T <Mola@molamola.xyz>
;; URL: https://github.com/mola-T/spu
;; Version: 1.0.1
;; Package-Requires: ((emacs "24.4") (signal "1.0") (timp "1.2.0"))
;; Keywords: convenience, package
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
;; spu stands for Silent Package Upgrader.
;; It can upgrade installed packages completely in the backgroud.
;; You will never being blocked by "Contacting host: elpa.gnu.org:80...."
;; when upgrading package.
;;
;; You will never worry about packages ugrade
;; with just one line : (spu-package-upgrade-daily) added to your init file,
;;
;; See https://github.com/mola-T/spu for more information.
;;
;;; code:
(require 'signal)
(require 'timp)
(require 'subr-x)
(require 'package)

(defgroup spu nil
  "Group for Silenct Package Upgrader."
  :group 'convenience
  :group 'package)

(defcustom spu-log-path (file-name-as-directory
                         (concat
                          (file-name-as-directory (expand-file-name user-emacs-directory))
                          "spu_log"))
  "Path to save SPU packages upgrade log."
  :group 'spu)

(defcustom spu-require-confirm-upgrade-package nil
  "Non-nil value will prompt for confirmation before upgrading packages."
  :group 'spu)

(defvar spu-thread nil
  "Store thread for upgrading packages.")

(defvar spu-package-upgrade-buffer "*SPU Package Upgrades*"
  "Buffer name for package upgrade prompt.")

(defvar spu-upgrade-in-process nil
  "Ensure one `spu-package-upgrade' is doing at a time.")

(defvar spu-upgrade-timer nil
  "Store the timer for next scheduled package upgrade.")

(defsignal spu-package-upgrade-finished-signal
  "Signal emitted when upgrade finished.")
(defsignal spu-package-upgraded-list-signal
  "Signal emitted with upgraded list when upgrade finished.")
(defsignal spu-package-upgraded-error-list-signal
  "Signal emitted with error list when upgrade finished.")

(define-derived-mode spu-package-menu-mode tabulated-list-mode "Package Upgrade Menu"
  "Major mode for browsing a list of upgradable packages."
  (setq tabulated-list-format
        `[("Package" 18 package-menu--name-predicate)
          ("Version" 13 nil)
          ("Status"  10 package-menu--status-predicate)
          ,@(if (cdr package-archives)
                '(("Archive" 10 package-menu--archive-predicate)))
          ("Description" 0 nil)])
  (setq tabulated-list-padding 10)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (tabulated-list-init-header))

;;;###autoload
(defun spu-package-upgrade (&optional prefix)
  "Upgrade package."
  (interactive "p")
  ;; Ensure only run once a day
  (catch 'ran-already
    (when (and (not prefix) (file-exists-p (concat spu-log-path (format-time-string "%Y%m%d") ".log")))
      (throw 'ran-already nil))
    (when spu-upgrade-in-process
      (message "[SPU] package upgrade is already running.")
      (throw 'ran-already nil))

    (setq spu-upgrade-in-process t)

    (when prefix
      (message "[SPU] Checking package information."))
    
    (unless (timp-validate spu-thread)
      (setq spu-thread (timp-get :persist t)))
    (timp-require-package spu-thread 'spu-dark)
    (timp-send-variable spu-thread spu-log-path)
    (timp-send-exec spu-thread 'spu-dark-set-package-acrhives package-archives)
    (timp-send-exec spu-thread 'spu-dark-init)
    (if spu-require-confirm-upgrade-package
        (timp-send-exec spu-thread 'spu-dark-get-package-upgrade-list
                        :reply-func #'spu-confirm-upgrade-package
                        :error-handler #'spu-print-error-message)
      (timp-send-exec spu-thread 'spu-dark-upgrade-packages
                      :reply-func #'spu-upgrade-finished
                      :error-handler #'spu-print-error-message))))

;;;###autoload
(defun spu-package-upgrade-daily ()
  "Upgrade package daily.  Don't call it by any interactive way."
  (spu-package-upgrade)
  (when spu-upgrade-timer
    (cancel-timer spu-upgrade-timer)
    (setq spu-upgrade-timer nil))
  (setq spu-upgrade-timer (run-at-time (time-add (current-time) (seconds-to-time 86401)) nil #'spu-package-upgrade-daily)))

(defun spu-upgrade-finished (result)
  ;; result is in form of (installed-list . error-list)
  "Echo the package upgrade RESULT."
  (let ((installed-list (car result))
        (error-list (cdr result)))
    
    (if (and (= (length installed-list) 0) (= (length error-list) 0))
        (message "[SPU] All packages are up to date.")
      (signal-emit 'spu-package-upgrade-finished-signal)
      (when installed-list
        (signal-emit 'spu-package-upgraded-list-signal :arg (list installed-list)))
      (when error-list
        (signal-emit 'spu-package-upgraded-error-list-signal :arg (list error-list)))
      (message "[SPU] %d package%s upgraded. %s\n      M-x %s for details."
               (length installed-list)
                    (if (> (length installed-list) 1) "s" "")
                    (if (> (length error-list) 0)
                        (propertize (format "%d error%s occurs."
                                            (length error-list)
                                            (if (> (length error-list) 1) "s" ""))
                                    'face
                                    'error)
                      "")
                    (propertize "spu-view-upgrade-log" 'face 'font-lock-builtin-face)))

    (timp-quit spu-thread)
    (setq spu-upgrade-in-process nil)))

(defun spu-confirm-upgrade-package (packages)
  "Generate a buffer prompt for upgrading PACKAGES."
  (if packages
      (let ((buf (get-buffer-create spu-package-upgrade-buffer)))
        (with-current-buffer buf
          (spu-package-menu-mode)
          (spu-package-menu--generate packages)
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (point-max))
              (unless (eq (point) (line-beginning-position))
                (newline))
              (insert "\n** Press 'c' to toggle cancelling upgrade.\n"
                      "** Press 'x' to execute upgrade.\n"
                      "** Press 'q' or 'C-g' to abort upgrade.")))
          (switch-to-buffer buf)))
    (spu-upgrade-finished nil)))

(defun spu-package-menu--generate (packages)
  "Populate the Package Menu.
If REMEMBER-POS is non-nil, keep point on the same entry.
PACKAGES should be t, which means to display all known packages,
or a list of package names (symbols) to display.

With KEYWORDS given, only packages with those keywords are
shown."
  (spu-package-menu--refresh packages)
  (setf (car (aref tabulated-list-format 0)) "Package")
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun spu-package-menu--refresh (packages)
  "Re-populate the `tabulated-list-entries'.
PACKAGES should be nil or t, which means to display all known packages.
KEYWORDS should be nil or a list of keywords."
  ;; Construct list of (PKG-DESC . STATUS).
  (let (info-list)
    (dolist (package packages)
      (push (cons (cdr package) "Upgradable") info-list))
    ;; Print the result.
    (setq tabulated-list-entries
          (mapcar #'spu-package-menu--print-info info-list))))

(defun spu-package-menu--print-info (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG has the form (PKG-DESC . STATUS).
Return (PKG-DESC [NAME VERSION STATUS DOC])."
  (let* ((pkg-desc (car pkg))
         (status  (cdr pkg))
         (face 'bold)) ; obsolete.
    (list pkg-desc
          `[,(list (symbol-name (package-desc-name pkg-desc))
                   'face 'link
                   'follow-link t
                   'package-desc pkg-desc
                   'action 'package-menu-describe-package)
            ,(propertize (package-version-join
                          (package-desc-version pkg-desc))
                         'font-lock-face face)
            ,(propertize status 'font-lock-face face)
            ,@(if (cdr package-archives)
                  (list (propertize (or (package-desc-archive pkg-desc) "")
                                    'font-lock-face face)))
            ,(propertize (package-desc-summary pkg-desc)
                         'font-lock-face face)])))

(defun spu-package-menu-toggle-cancel-upgrades ()
  "Cancel the selected package from updating."
  (interactive)
  (unless (derived-mode-p 'spu-package-menu-mode)
    (error "The current buffer is not a SDPU Package Menu"))
  (unless (or (eobp) (string-match "\\` *\\*\\*" (thing-at-point 'line t)))
    (if (equal (save-excursion (beginning-of-line) (thing-at-point 'word t)) "Cancel")
        (tabulated-list-put-tag "" t)
      (tabulated-list-put-tag "Cancel" t))))

(defun spu-package-menu-abort-upgrades ()
  "Abort package upgrades."
  (interactive)
  (unless (derived-mode-p 'spu-package-menu-mode)
    (error "The current buffer is not a SDPU Package Menu"))
  (when (timp-validate spu-thread)
    (timp-quit spu-thread))
  (kill-buffer spu-package-upgrade-buffer))

(defun spu-package-menu-execute-upgrades ()
  "Execute package upgrades."
  (interactive)
  (unless (derived-mode-p 'spu-package-menu-mode)
    (error "The current buffer is not a SDPU Package Menu"))
  (let (upgrade-list)
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not (string-blank-p (string-trim (thing-at-point 'line t))))
                  (not (string-match "\\` *\\*\\*" (thing-at-point 'line t))))
        (unless (string= (thing-at-point 'word t) "Cancel")
          (push (tabulated-list-get-id) upgrade-list))
        (forward-line)))
    (timp-send-exec spu-thread 'spu-dark-package-menu-mark-upgrades upgrade-list
                    :reply-func #'spu-upgrade-finished
                    :error-handler #'spu-print-error-message)
    (message "Packages will be upgraded.")
    (kill-buffer spu-package-upgrade-buffer)))

;;;###autoload
(defun spu-view-upgrade-log (prefix)
  "Open the last package upgrade log.
With PREFIX, open the directory containing the upgrade logs."
  (interactive "p")
  (if (= prefix 1)
      (let ((files (directory-files spu-log-path t ".*\\.log")))
            (if files
                (view-file (car (last files)))
              (message "[SPU] No package upgrade log availiable.")))
    (dired spu-log-path)))


(defun spu-print-error-message (err)
  "Print ERR to message log."
  (message (concat
            (propertize "[SPU]" 'face 'error) (pp-to-string err)))
  (timp-quit spu-thread))

(define-key spu-package-menu-mode-map (kbd "c") #'spu-package-menu-toggle-cancel-upgrades)
(define-key spu-package-menu-mode-map (kbd "x") #'spu-package-menu-execute-upgrades)
(define-key spu-package-menu-mode-map (kbd "q") #'spu-package-menu-abort-upgrades)
(define-key spu-package-menu-mode-map (kbd "C-g") #'spu-package-menu-abort-upgrades)

(provide 'spu)
;;; spu.el ends here
