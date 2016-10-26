;;; packages-outdated-packages.el --- Work with your outdated packages.

;; Author: Jason Milkins <jasonm23@gmail.com>
;; Version: 0.1.0

;; URL: https://github.com/ocodo/.emacs.d/plugins/packages-outdated-packages

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
;;
;;  Tabulated List mode for working with outdated packages:
;;
;;  - Mark/unmark packages (Bonus! built in to tblui!)
;;  - Install/upgrade marked packages ()
;;
;;  TODO:
;;  - View the package info
;;  - Jump to the currently installed version of the package (file system)
;;  - Browse/view the latest version of the package code (if on GitHub)
;;  - Browse/view the package github issues (if on GitHub)
;;  - Filter list on search string
;;  MAYBE:
;;  - Fuzzy filter list
;;

;;; Code:
(require 'epl)
(require 'bind-key)

(defun packages-outdated-packages-make-plist (pack)
  "Destructure PACK package metadata into a list."
  (pcase-let* ((`[cl-struct-epl-package ,package ,package-desc] pack)
               (`[cl-struct-package-desc ,name ,version-list ,summary ,_ ,_ ,_ ,path ,extras ,_] package-desc)
               (version (format "%s.%s" (car version-list) (car (cdr version-list)))))
    (list :pack pack
          :name (symbol-name package)
          :version version
          :summary summary
          :version-list version-list
          :path path
          :extras extras
          :package-desc package-desc)))

(defun packages-outdated-packages-list-vector (pack)
  "Build a vector for the tabulated list from PACK."
  (let* ((package-plist (packages-outdated-packages-make-plist pack))
         (name (plist-get package-plist :name))
         (version (plist-get package-plist :version))
         (summary (plist-get package-plist :summary)))
    (list pack (vector
                (or name "")
                (or version "")
                (or summary "")))))

(defun packages-outdated-packages-get-list-entries ()
  "Get the list of outdated packages."
  (mapcar 'packages-outdated-packages-list-vector (epl-outdated-packages)))

(defvar packages-outdated-packages-table-layout
  "Packages outdated packages table layout.")

(defun packages-outdated-packages-upgrade-marked-packages (packages)
  "Upgrade the selected PACKAGES."
  (epl-upgrade packages)
  (revert-buffer))

(defun packages-outdated-packages-refresh ()
  "Refresh the outdated package list."
  (interactive)
  (package-refresh-contents)
  (revert-buffer))

(defun packages-outdated-packages-visit-url (&optional pkg)
  "Visit the url of the package PKG."
  (interactive)
  (let* ((package (tabulated-list-get-id))
         (pkg-plist (packages-outdated-packages-make-plist package))
         (name (plist-get pkg-plist :name))
         (extras (plist-get pkg-plist :extras))
         (url (alist-get :url extras)))
    (if url
        (browse-url url)
      (message "There's no URL for %s" (or name "this package")))))

(defun packages-outdated-packages-open-package (&optional pkg)
  "Show info for the package PKG."
  (interactive)
  (let* ((package (tabulated-list-get-id))
         (pkg-plist (packages-outdated-packages-make-plist package))
         (path (plist-get pkg-plist :path)))
    (find-file path)))

(tblui-define
 packages-outdated-packages
 packages-outdated-packages-get-list-entries
 [("Package" 18 t)
  ("Version" 15 t)
  ("Summary" 40 nil)]
 ((:key "?"
        :name packages-outdated-packages-info-popup
        :funcs ((?v "Visit package URL" packages-outdated-packages-visit-url)
                (?o "Open package" packages-outdated-packages-open-package))
        )
  (:key "x"
        :name packages-outdated-packages-upgrade-popup
        :funcs ((?y "Confirm upgrade" packages-outdated-packages-upgrade-marked-packages)))))

(bind-key "g" 'packages-outdated-packages-refresh packages-outdated-packages-mode-map)

(defun packages-outdated-packages ()
  "Something."
  (interactive)
  (package-refresh-contents)
  (packages-outdated-packages-goto-ui))

(provide 'packages-outdated-packages)

;;; packages-outdated-packages.el ends here
