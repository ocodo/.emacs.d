;;; ac-html-complete-data.el --- auto complete data for ac-html

;; Copyright (C) 2015 Zhang Kai Yu, Olexandr Sydorchuk

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Keywords: html, auto-complete

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

;; This is just dependency for ac-html, company-web
;;
;;
;; `ac-html-source-dirs' is pair list of framework-name and directory of completion data
;;
;; This package provide default html completion data.
;;
;; Completion data directory structure:
;;
;; html-attributes-complete - attribute completion
;; html-attributes-list - attributes of tags-add-tables
;; html-attributes-short-docs - attributes documantation
;; html-tag-short-docs  - tags documantation

;;; Code:

 (defconst ac-html-package-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The directory where `ac-html' package exists.")

(defconst ac-html-basic-source-dir
  (expand-file-name "html-stuff" ac-html-package-dir)
  "The directory where basic completion source of `ac-html' exists.")

(defcustom ac-html-source-dirs
  '(("html" . ac-html-basic-source-dir))
  "Alist of source directories. 
car is source name, cdr is source location."
  :type 'alist
  :group 'auto-complete-html)

(provide 'ac-html-complete-data)
;;; ac-html-complete-data.el ends here
