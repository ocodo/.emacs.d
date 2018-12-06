;;; ac-html-angular.el --- auto complete angular15 data for `ac-html' and `company-web'

;; Copyright (C) 2015, 2016 Olexandr Sydorchuk

;; Author: Olexandr Sydorchuk <olexandr.syd@gmail.com>
;; Version: 1.5.0
;; Keywords: html, auto-complete, angular
;; Package-Requires: ((web-completion-data "0.1"))
;; URL: https://github.com/osv/ac-html-bootstrap

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

;; Angular 1.5 completion with doc.
;;
;; Install and configure one of next packages:
;; 
;; `ac-html' or `company-web'
;;
;; Usage:
;;
;; Use `ac-html-angular1+' or `company-web-angular1+'
;;

;;; Code:

(require 'web-completion-data)

(defconst ac-html-angular-source-dir
  (expand-file-name "html-stuff" (file-name-directory load-file-name))
  "The directory where source of `ac-html-angular' exists.")

;;;###autoload
(defun ac-html-angular+ ()
  "Enable angular ac-html completion"
  (interactive)
  (make-local-variable 'web-completion-data-sources)
  (unless (assoc "Angular15" web-completion-data-sources)
    (push (cons "Angular15" 'ac-html-angular-source-dir) web-completion-data-sources)))

;;;###autoload
(defalias 'company-web-angular+ 'ac-html-angular+)

(provide 'ac-html-angular)
;;; ac-html-angular.el ends here
