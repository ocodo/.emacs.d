;;; ac-html-bootstrap.el --- auto complete bootstrap3/fontawesome classes for `ac-html' and `company-web'

;; Copyright (C) 2014 Olexandr Sydorchuk

;; Author: Olexandr Sydorchuk <olexandr.syd@gmail.com>
;; Version: 0.9.3
;; Keywords: html, auto-complete, bootstrap, cssx
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

;; Bootstrap and Font Awesome
;;
;;   If  you ok  that  "glyphicon"  classes allowed  only  for <i>  or
;; "label-default" for  <span>, <label> and  so on, this may  good for
;; you.   Alternative you  can use  package `ac-html-csswatcher'  that
;; parse all css in project.

;;   However  this  package  provide documentation  with  samples  and
;; additional   data-  attributes   that  `ac-html-csswatcher'   can't
;; provide, so ac-html-bootstrap may be helpful for you.
;;
;; Install `ac-html' for `auto-complete' completion framework
;; or `company-web' if you are using `company' framework.
;;
;; For  `ac-html' users:  Since ac-html  0.4 is  alpha stage,  and not
;; stable yet. This package works with  ac-html 0.3 series, if you are
;; using ac-html 0.4  series, you may downgrade  ac-html aka reinstall
;; from melpa-stable. `ac-html' 0.4 will be supported in the future.
;;
;; Usage:
;;
;; Use `ac-html-bootstrap+' or `company-web-bootstrap+'
;; and if you want Font Awesome: `ac-html-fa+' or `company-web-fa+'
;;
;; Note: Font Awesome completion only available for <i> tag
;;
;; Contribute:
;;
;; All definition are in bootstrap.yaml
;; Build script here: https://github.com/osv/h5doc.git

;;; Code:

(require 'web-completion-data)

(defconst ac-html-bootstrap-source-dir
  (expand-file-name "html-stuff" (file-name-directory load-file-name))
  "The directory where source of `ac-html-bootstrap' exists.")

;;;###autoload
(defun ac-html-bootstrap+ ()
  "Enable bootstrap ac-html completion"
  (interactive)
  (make-local-variable 'web-completion-data-sources)
  (unless (assoc "Bootstrap" web-completion-data-sources)
    (push (cons "Bootstrap" 'ac-html-bootstrap-source-dir) web-completion-data-sources)))

;;;###autoload
(defalias 'company-web-bootstrap+ 'ac-html-bootstrap+)

(provide 'ac-html-bootstrap)
;;; ac-html-bootstrap.el ends here
