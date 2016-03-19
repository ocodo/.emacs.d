;;; web-completion-data.el --- Shared completion data for ac-html and company-web 

;; Copyright (C) 2015 Zhang Kai Yu, Olexandr Sydorchuk

;; Author: Olexandr Sydorchuk <olexandr.syd@gmail.com>
;; Version: 0.2
;; Keywords: html, auto-complete, company
;; URL: https://github.com/osv/web-completion-data

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
;; `web-completion-data-sources' is pair list of framework-name and directory of completion data
;;
;; This package provide default "html" completion data.
;;
;; Completion data directory structure:
;;
;; html-attributes-complete - attribute completion
;; html-attributes-list - attributes of tags-add-tables
;; html-attributes-short-docs - attributes documantation
;; html-tag-short-docs  - tags documantation

;; If you decide extend with own completion data, let say "Bootstrap" data:
;;
;; (unless (assoc "Bootstrap" web-completion-data-sources)
;;   (setq web-completion-data-sources 
;;         (cons (cons "Bootstrap" "/path/to/complete/data")
;;               web-completion-data-sources)))

;;; Code:


 (defconst web-completion-data-package-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The directory where `web-completion-data' package exists.")

(defconst web-completion-data-html-source-dir
  (expand-file-name "html-stuff" web-completion-data-package-dir)
  "The directory where basic completion source of `web-completion-data' exists.")

(defcustom web-completion-data-sources
  '(("html" . web-completion-data-html-source-dir))
  "Alist of source directories. 
car is source name, cdr is source location."
  :type 'alist)

(provide 'web-completion-data)
;;; web-completion-data.el ends here
