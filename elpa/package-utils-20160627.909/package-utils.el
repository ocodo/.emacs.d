;;; package-utils.el --- Extensions for package.el

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/package-utils
;; Package-Version: 20160627.909
;; Keywords: package, convenience
;; Version: 0.4.1
;; Package-Requires: ((epl "0.8") (async "1.6"))

;; This file is NOT part of GNU Emacs.

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

;;; Commentary:
;;
;; This library provides extensions for package.el
;;
;;; Code:

(require 'epl)

(defun package-utils-upgradable-packages ()
  "Return the list of upgradable packages as a list of symbols."
  (mapcar #'epl-package-name (epl-outdated-packages)))

(defun package-utils-installed-packages ()
  "Return the list of installed packages as a list of symbols."
  (mapcar #'epl-package-name (epl-installed-packages)))

(defun package-utils-has-upgradable-packages-p ()
  "Returns true if there are packages to upgrade, nil otherwise."
  (not (null (package-utils-upgradable-packages))))

(defun package-utils-read-upgradable-package ()
  "Read the name of a package to upgrade."
  (completing-read "Upgrade package: "
                   (mapcar #'symbol-name (package-utils-upgradable-packages))
                   nil
                   'require-match))

(defun package-utils-upgradable-p (name)
  "Returns true if NAME can be upgraded, nil otherwise.

NAME can be a string or a symbol."
  (unless (symbolp name)
    (setq name (intern name)))
  (not (null (member name (package-utils-upgradable-packages)))))

(defun package-utils-installed-p (name)
  "Returns true if NAME is installed, nil otherwise.

NAME can be a string or a symbol."
  (unless (symbolp name)
    (setq name (intern name)))
  (not (null (member name (package-utils-installed-packages)))))

;;;###autoload
(defun package-utils-list-upgrades (&optional no-fetch)
  "List all packages that can be upgraded.

With prefix argument NO-FETCH, do not call `package-refresh-contents'."
  (interactive "P")
  (package-utils-upgrade-all current-prefix-arg t))

;;;###autoload
(defun package-utils-upgrade-all (&optional no-fetch dry-run)
  "Upgrade all packages that can be upgraded.

With prefix argument NO-FETCH, do not call `package-refresh-contents'.
When DRY-RUN is true, only display what packages would be upgraded."
  (interactive "P")
  (unless no-fetch
    (package-refresh-contents))
  (if (package-utils-has-upgradable-packages-p)
      (let ((packages (package-utils-upgradable-packages)))
        (unless dry-run
          (epl-upgrade))
        (message "%s packages: %s" (if dry-run "Upgradable" "Upgraded") (mapconcat 'symbol-name packages ", ")))
    (message "All packages are already up to date.")))

;;;###autoload
(defun package-utils-upgrade-all-no-fetch ()
  "Upgrade all packages that can be upgraded without calling `package-refresh-contents' first."
  (interactive)
  (package-utils-upgrade-all t))

;;;###autoload
(defun package-utils-upgrade-by-name (name &optional no-fetch)
  "Upgrade the package NAME.

NAME can be a string or a symbol.

With prefix argument NO-FETCH, do not call `package-refresh-contents'."
  (interactive
   (progn
     (unless current-prefix-arg
       (package-refresh-contents))
     (list (package-utils-read-upgradable-package)
           current-prefix-arg)))
  (unless (symbolp name)
    (setq name (intern name)))
  (unless (package-utils-upgradable-p name)
    (error "Package \"%s\" not found in the list of upgradable packages" name))
  (epl-upgrade (epl-find-installed-packages name))
  (message "Package \"%s\" was upgraded." name))

;;;###autoload
(defun package-utils-upgrade-by-name-no-fetch (name)
  "Upgrade the package NAME, without calling `package-refresh-contents' first.

NAME can be a string or a symbol."
  (interactive (list (package-utils-read-upgradable-package)))
  (package-utils-upgrade-by-name name t))

;;;###autoload
(defun package-utils-remove-by-name (name)
  "Uninstall the package NAME.

NAME can be a string or a symbol."
  (interactive
   (list (completing-read "Remove package: "
                          (mapcar #'symbol-name (package-utils-installed-packages))
                          nil
                          'require-match)))
  (unless (symbolp name)
    (setq name (intern name)))
  (epl-package-delete (car (epl-find-installed-packages name))))

;;;###autoload
(defun package-utils-list-packages-async ()
  "Like `package-list-packages', but works asynchronously."
  (interactive)
  (async-start
   `(lambda ()
      ,(async-inject-variables "^package-archives$")
      (require 'finder-inf nil t)
      ;; Initialize the package system if necessary.
      (package-initialize t)
      (let (old-archives new-packages)
        ;; Read the locally-cached archive-contents.
        (package-read-all-archive-contents)
        (setq old-archives package-archive-contents)
        ;; Fetch the remote list of packages.
        (package-refresh-contents)
        ;; Find which packages are new.
        (dolist (elt package-archive-contents)
          (unless (assq (car elt) old-archives)
            (push (car elt) new-packages)))
        (setq result-prev (list new-packages package-archive-contents))))
   `(lambda (result)
      (setq package-archive-contents (cadr result))
      (let ((new-packages (car result)))
        ;; Generate the Package Menu.
        (let ((buf (get-buffer-create "*Packages*")))
          (with-current-buffer buf
            (package-menu-mode)
            (set (make-local-variable 'package-menu--new-package-list)
                 new-packages)
            (package-menu--generate nil t))
          ;; The package menu buffer has keybindings.  If the user types
          ;; `M-x list-packages', that suggests it should become current.
          (switch-to-buffer buf))
        (let ((upgrades (package-menu--find-upgrades)))
          (if upgrades
              (message "%d package%s can be upgraded; type `%s' to mark %s for upgrading."
                       (length upgrades)
                       (if (= (length upgrades) 1) "" "s")
                       (substitute-command-keys "\\[package-menu-mark-upgrades]")
                       (if (= (length upgrades) 1) "it" "them"))))))))

;;;###autoload
(defun package-utils-install-async (package)
  "Like `package-install', but works asynchronously."
  (interactive "SInstall package?")
  (async-start
   `(lambda ()
      ,(async-inject-variables "^package-archives$")
      ;; Initialize the package system if necessary.
      (package-initialize t)
      (package-install ',package))
   `(lambda (result)
      (package-initialize nil)
      (message "%s installed" ',package))))

(provide 'package-utils)

;;; package-utils.el ends here
