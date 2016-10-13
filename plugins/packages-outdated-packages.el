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

(defun packages-outdated-packages-get-list-entries ()
  "Get the list of outdated packages."
  (mapcar (lambda (pack)
            (pcase-let* ((`[cl-struct-epl-package ,package ,package-desc] pack)
                         (`[cl-struct-package-desc ,name ,version-list ,summary ,_ ,_ ,_ ,path ,extras ,_] package-desc)
                         (version (format "%s.%s" (car version-list) (car (cdr version-list))))
                         (keywords (alist-get :keywords extras nil))
                         (url (alist-get :url extras nil)))
              (list pack
                    (vector
                     (or (symbol-name package) "")
                     (or  version "")
                     (or summary "")
                     (or  url "")
                     (or path "")))))
          (epl-outdated-packages)))

(defvar packages-outdated-packages-table-layout
  "Packages outdated packages table layout.")

(defun packages-outdated-packages-upgrade-marked-packages (packages)
  "Upgrade the selected PACKAGES."
  (epl-upgrade packages)
  (revert-buffer))

(tblui-define
 packages-outdated-packages
 packages-outdated-packages-get-list-entries
 [("Package" 25 t)
  ("Version" 16 t)
  ("Summary" 40 nil)
  ("Url"     30 nil)
  ("Path"    30 nil)]
 ((:key "x"
   :name packages-outdated-packages-upgrade-popup
   :funcs((?y "Confirm upgrade" packages-outdated-packages-upgrade-marked-packages)))))

(define-key packages-outdated-packages-mode-map (kbd "g") (lambda ()
                                  (interactive)
                                  (package-refresh-contents)
                                  (revert-buffer)))

(defun packages-outdated-packages ()
  "Something."
  (interactive)
  (package-refresh-contents)
  (packages-outdated-packages-goto-ui))

(provide 'packages-outdated-packages)

;;; packages-outdated-packages.el ends here
