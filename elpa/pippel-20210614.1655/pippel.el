;;; pippel.el --- Frontend to python package manager pip -*- lexical-binding: t -*-

;; Copyright (C) 2017  Fritz Stelzer <brotzeitmacher@gmail.com>

;; Author: Fritz Stelzer <brotzeitmacher@gmail.com>
;; Maintainer: Arif Er <arifer612@protonmail.me>
;; URL: https://github.com/arifer612/pippel
;; Version: 1.3
;; Package-Requires: ((emacs "25.1") (s "1.11.0") (dash "2.12.0"))

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:

(require 'python)
(require 'tabulated-list)
(require 'json)
(require 's)
(require 'sort)
(require 'dash)

(defgroup pippel nil
  "Manager for pip packages."
  :prefix "pippel-"
  :group 'applications)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization Variables

(defcustom pippel-column-width-package 15
  "Width of the Package column."
  :type 'integer
  :group 'pippel)

(defcustom pippel-column-width-version 10
  "Width of the Version and Latest columns."
  :type 'integer
  :group 'pippel)

(defcustom pippel-menu-latest-face "orange"
  "Face for latest version when newer than installed version."
  :type 'face
  :group 'pippel)

(defcustom pippel-python-command "python"
  "Used Python interpreter."
  :type '(choice (const :tag "python" "python")
                 (const :tag "python2" "python2")
                 (const :tag "python3" "python3")
                 (string :tag "Other"))
  :group 'pippel)

(defcustom pippel-package-path (file-name-directory (locate-library "pippel"))
  "Directory for pippel.py.

If this is nil, it's assumed pippel can be found in the standard path."
  :type 'directory
  :group 'pippel)

(defcustom pippel-display-status-reporter t
  "Display progress-reporter."
  :type 'boolean
  :group 'pippel)

(defcustom pippel-buffer-display-method 'display-buffer
  "Default action when `pippel-list-packages' finished."
  :type '(choice (const :tag "Pop to buffer." 'display-buffer)
                 (const :tag "Pop to buffer." 'pop-to-buffer)
                 (const :tag "Don't display menu mode buffer." nil))
  :group 'pippel)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package menu mode

(defvar pippel-package-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "m") 'pippel-menu-mark-unmark)
    (define-key map (kbd "d") 'pippel-menu-mark-delete)
    (define-key map (kbd "U") 'pippel-menu-mark-all-upgrades)
    (define-key map (kbd "u") 'pippel-menu-mark-upgrade)
    (define-key map (kbd "r") 'pippel-list-packages)
    (define-key map (kbd "i") 'pippel-install-package)
    (define-key map (kbd "x") 'pippel-menu-execute)
    (define-key map (kbd "RET") 'pippel-menu-visit-homepage)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Local keymap for `pippel-package-menu-mode' buffers.")

(define-derived-mode pippel-package-menu-mode tabulated-list-mode "Package Menu"
  "Major mode for browsing a list of installed pip packages."
  (setq buffer-read-only nil)
  (setq truncate-lines t)
  (setq tabulated-list-format
        `[("Package" ,pippel-column-width-package nil)
          ("Version" ,pippel-column-width-version nil)
          ("Latest" ,pippel-column-width-version nil)
          ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun pippel-menu-entry (pkg)
  "Return a package entry of PKG suitable for `tabulated-list-entries'."
  (let ((name (alist-get 'name pkg))
        (version (alist-get 'version pkg))
        (latest (alist-get 'latest pkg))
        (description (alist-get 'summary pkg))
        (home-page (alist-get 'home-page pkg)))
    (list name `[,(progn
                    (put-text-property 0 (length name) 'link home-page name)
                    name)
                 ,version
                 ,(if (or (string-match "[A-Za-z]+" version)
                          (string-match "[A-Za-z]+" latest))
                      (if (string= version latest)
                          latest
                        (propertize latest 'font-lock-face `(:foreground ,pippel-menu-latest-face)))
                    (if (version< version latest)
                        (propertize latest 'font-lock-face `(:foreground ,pippel-menu-latest-face))
                      latest))
                 ,description])))

(defun pippel-menu-generate (packages)
  "Re-populate the `tabulated-list-entries' with PACKAGES."
  (let ((buf (get-buffer-create
              (concat (when python-shell-virtualenv-root
                        (nth 1 (reverse (split-string python-shell-virtualenv-root "\\/"))))
                      "*Pip-Packages*"))))
    (with-current-buffer buf
      (pippel-package-menu-mode)
      (erase-buffer)
      (goto-char (point-min))
      (setq tabulated-list-entries
            (mapcar #'pippel-menu-entry (car packages)))
      (tabulated-list-print t)
      (let ((sort-fold-case t))
        (sort-lines nil (point-min) (point-max))))
    (when pippel-buffer-display-method
      (funcall pippel-buffer-display-method buf))))

;;;;;;;;;;;;;
;;; Server

(defvar pippel-process-name "pip-process"
  "Process name for pippel processes.")

(defvar pippel-process-buffer "*pip-process-buffer*"
  "Buffer name for pippel process buffers.")

(defun pippel-running-p ()
  "Is pippel process running."
  (interactive)
  (or (get-buffer pippel-process-buffer)
      (process-live-p pippel-process-name)))

(defun pippel-open-process ()
  "Start and return pip process."
  (let ((buf pippel-process-buffer)
        (file (expand-file-name "pippel.py"
                                pippel-package-path)))
    (unless (file-exists-p file)
      (user-error "Can't find pippel in pippel-package-path"))
    (start-process pippel-process-name
                   buf
                   pippel-python-command
                   file)
    (let ((proc (get-buffer-process buf)))
      (set-process-filter proc 'pippel-process-filter)
      (set-process-sentinel proc 'pippel-process-sentinel)
      (accept-process-output proc 0.1)
      proc)))

(defun pippel-process-sentinel (proc _output)
  "The sentinel for pip-process."
  (with-current-buffer (process-buffer proc)
    (let ((objects nil)
          (json-array-type 'list))
      (goto-char (point-min))
      (while (not (eobp))
        (when (memq (char-after) '(?\{ ?\[))
          (push (json-read) objects))
        (forward-line))
      (when objects
        (pippel-menu-generate objects)))
    (while (process-live-p proc)
      (sleep-for 0.01))
    (kill-buffer (process-buffer proc)))
  (when pippel-display-status-reporter
    (remove-hook 'post-command-hook 'pippel-status-reporter)))

(defun pippel-process-filter (proc output)
  "Filter for pip-process."
  (let ((buf (process-buffer proc)))
    (with-current-buffer buf
      (insert output)
      (goto-char (point-max))
      (cond
       ((looking-back "Pip finished\n" nil)
        (message "Pip finished")
        (kill-process proc))
       ((looking-back "Pip error\n" nil)
        (message "Pip error")
        (kill-process proc))))))

(defun pippel-call-pip-process (proc command &optional packages params)
  "Send request to pip process."
  (process-send-string proc (concat (json-encode `((method . ,command)
                                                   (packages . ,packages)
                                                   (params . ,params)))
                                    "\n"))
  (when pippel-display-status-reporter
   (add-hook 'post-command-hook 'pippel-status-reporter)))

(defun pippel-status-reporter ()
  "Status indicator is shown in the echo area while pip process alive."
  (unless (active-minibuffer-window)
    (let ((progress-reporter (make-progress-reporter "Pip processing...")))
      (dotimes (i 1000)
        (when (pippel-running-p)
          (progress-reporter-update progress-reporter i)
          (sit-for 0.1))))))

;;;;;;;;;;;;;;;;;;
;;; Interaction

(defun pippel-remove-package (packages)
  "Uninstall provided PACKAGES."
  (pippel-call-pip-process (pippel-open-process) "remove_package" packages))

(defun pippel-upgrade-package (packages)
  "Update provided PACKAGES."
  (pippel-call-pip-process (pippel-open-process) "install_package" packages))

(defun pippel-menu-mark-unmark ()
  "Clear any marks on a package."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun pippel-menu-mark-upgrade ()
  "Mark an upgradable package."
  (interactive)
  (unless (string= (aref (tabulated-list-get-entry) 1) (aref (tabulated-list-get-entry) 2))
    (tabulated-list-put-tag "U" t)))

(defun pippel-menu-mark-delete ()
  "Mark a package for deletion and move to the next line."
  (interactive)
  (tabulated-list-put-tag "D" t))

(defun pippel-menu-mark-all-upgrades ()
  "Mark all upgradable packages in the Package Menu."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (unless (string= (aref (tabulated-list-get-entry) 1)
                       (aref (tabulated-list-get-entry) 2))
        (tabulated-list-put-tag "U" t))
      (forward-line))))

(defun pippel-menu-visit-homepage ()
  "Follow link provided by pip."
  (interactive)
  (save-excursion
    (beginning-of-line-text)
    (browse-url (get-text-property (point) 'link))))

(defun pippel-menu-execute ()
  "Perform marked Package Menu actions."
  (interactive)
  (let (upgrade-list
        delete-list
        cmd
        pkg-desc)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq cmd (char-after))
        (setq pkg-desc (tabulated-list-get-id))
        (cond ((eq cmd ?D)
               (push (substring-no-properties pkg-desc) delete-list))
              ((eq cmd ?U)
               (push (substring-no-properties pkg-desc) upgrade-list)))
        (forward-line)))
    (unless (or delete-list upgrade-list)
      (user-error "No operations specified"))
    (let* ((del (when delete-list
                  (concat (format "Delete %d package%s "
                                  (length delete-list)
                                  (if (> (length delete-list) 1)
                                      "s" ""))
                          (replace-regexp-in-string " " ", "
                                                    (format "%s" delete-list)))))
           (up (when upgrade-list
                 (concat (format "Upgrade %d package%s "
                                 (length upgrade-list)
                                 (if (> (length upgrade-list) 1)
                                     "s" ""))
                         (replace-regexp-in-string " " ", "
                                                   (format "%s" upgrade-list)))))
           (msg (if (and del up)
                    (concat del " and " up)
                  (or del up))))
      (when (yes-or-no-p (format "%s" msg))
        (when upgrade-list
          (pippel-upgrade-package (mapconcat 'identity upgrade-list " ")))
        (while (pippel-running-p)
          (sleep-for 0.01))
        (when delete-list
          (pippel-remove-package (mapconcat 'identity delete-list " ")))
        (while (pippel-running-p)
          (sleep-for 0.01))
        (pippel-list-packages)))))

;;;###autoload
(defun pippel-install-package (arg)
  "Prompt user for a string containing packages to be installed."
  (interactive "P")
  (let ((pkg (read-from-minibuffer "Enter package name: "))
        (proc (pippel-open-process))
        (dir (--if-let arg
                 (read-file-name "Directory: "))))
    (pippel-call-pip-process proc "install_package" (s-trim pkg) dir)
    (when (string= major-mode "pippel-package-menu-mode")
      (while (pippel-running-p)
        (sleep-for 0.01))
      (pippel-list-packages))))

;;;###autoload
(defun pippel-list-packages ()
  "Display a list of installed packages."
  (interactive)
  (pippel-call-pip-process (pippel-open-process) "get_installed_packages"))
  
(provide 'pippel)
;;; pippel.el ends here
