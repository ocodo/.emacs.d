;;; ac-html-fa.el --- auto complete CSS font-awesome classes

;; Copyright (C) 2015 Olexandr Sydorchuk

;; Author: Olexandr Sydorchuk <olexandr.syd@gmail.com>
;; Keywords: html, auto-complete, font-awesome

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

;; Usage:
;;
;; Use `ac-html-fa+' or `company-web-fa+'
;;
;; Contribute:
;;
;; Here is ./font-awesome.pl script for updating, git submodule must be initialized

;;; Code:

(require 'web-completion-data)

(defconst ac-html-fa-source-dir
  (expand-file-name "fa-html-stuff" (file-name-directory load-file-name))
  "The directory where source of `ac-html-fa' exists.")

;;;###autoload
(defun ac-html-fa+ ()
  "Enable Font Awesome completion for `ac-html' or `company-web'"
  (interactive)
  (make-local-variable 'web-completion-data-sources)
  (unless (assoc "Font Aws" web-completion-data-sources)
    (push (cons "Font Aws" 'ac-html-fa-source-dir) web-completion-data-sources)))

;;;###autoload
(defalias 'company-web-fa+ 'ac-html-fa+)

(provide 'ac-html-fa)
;;; ac-html-fa.el ends here
