;;; package-utils.el --- Extensions for package.el

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>
;; URL: https://github.com/Silex/package-utils
;; Keywords: package, convenience
;; Version: 20141005.628
;; X-Original-Version: 0.2.0
;; Package-Requires: ((epl "0.7-cvs"))

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

(defun package-utils-has-upgradable-packages-p ()
  "Returns true if there are packages to upgrade, nil otherwise."
  (not (null (epl-outdated-packages))))

(defun package-utils-ensure-upgrades-available ()
  "Make an error if there is no upgradable packages."
  (unless (package-utils-has-upgradable-packages-p)
    (error "All packages are already up to date")))

(defun package-utils-read-upgradable-package ()
  "Read the name of a package to upgrade."
  (package-utils-ensure-upgrades-available)
  (completing-read "Upgrade package: "
                   (mapcar #'symbol-name (mapcar #'epl-package-name (epl-outdated-packages)))
                   nil
                   'require-match))

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
When DRY-RUN is true, only display what packages would be upgraded"
  (interactive "P")
  (unless no-fetch
    (package-refresh-contents))
  (package-utils-ensure-upgrades-available)
  (let ((packages (mapcar #'epl-package-name (epl-outdated-packages))))
    (unless dry-run
      (epl-upgrade))
    (message "%s packages: %s" (if dry-run "Upgradable" "Upgraded") (mapconcat 'symbol-name packages ", "))))

;;;###autoload
(defun package-utils-upgrade-all-no-fetch ()
  "Upgrade all packages that can be upgraded without calling `package-refresh-contents' first."
  (interactive)
  (package-utils-upgrade-all t))

;;;###autoload
(defun package-utils-upgrade-by-name (name &optional no-fetch)
  "Upgrade the package NAME.

With prefix argument NO-FETCH, do not call `package-refresh-contents'."
  (interactive
   (progn
     (unless current-prefix-arg
       (package-refresh-contents))
     (list (package-utils-read-upgradable-package)
           current-prefix-arg)))
  (epl-upgrade (epl-find-installed-packages (intern name))))

;;;###autoload
(defun package-utils-upgrade-by-name-no-fetch (name)
  "Upgrade the package NAME, without calling `package-refresh-contents' first."
  (interactive (list (package-utils-read-upgradable-package)))
  (package-utils-upgrade-by-name name t))

;;;###autoload
(defun package-utils-remove-by-name (name)
  "Uninstall the package NAME."
  (interactive
   (list (completing-read "Remove package: "
                          (mapcar #'symbol-name (mapcar #'epl-package-name (epl-installed-packages)))
                          nil
                          'require-match)))
  (epl-package-delete (car (epl-find-installed-packages (intern name)))))

(provide 'package-utils)

;;; package-utils.el ends here
