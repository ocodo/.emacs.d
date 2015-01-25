;;; ac-html-bootstrap.el --- auto complete bootstrap3 classes, tags and attributes

;; Copyright (C) 2014 Olexandr Sydorchuk

;; Author: Olexandr Sydorchuk <olexandr.syd@gmail.com>
;; Version: 0.9.2
;; Keywords: html, auto-complete, bootstrap, cssx
;; Package-Requires: ((ac-html "0.3.0"))
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

;;   If  you ok  that  "glyphicon"  classes allowed  only  for <i>  or
;; "label-default" for  <span>, <label> and  so on, this may  good for
;; you.   Alternative you  can use  package `ac-html-csswatcher'  that
;; parse   all  css   in   prject.   However   this  package   provide
;; documentation  with samples  and additional  data- attributes  that
;; `ac-html-csswatcher'  can't provide,  so  ac-html-bootstrap may  be
;; helpful for you.
;;
;; Usage:
;;
;; Use `ac-html-bootstrap+'
;;
;; Contribute:
;;
;; All definition are in bootstrap.yaml
;; Build script here: https://github.com/osv/h5doc.git

;;; Code:

(require 'ac-html)

(defconst ac-html-bootstrap-source-dir
  (expand-file-name "html-stuff" (file-name-directory load-file-name))
  "The directory where source of `ac-html-bootstrap' exists.")

;;;###autoload
(defun ac-html-bootstrap+ ()
  "Enable bootstrap ac-html completion"
  (interactive)
  (make-local-variable 'ac-html-source-dirs)
  (unless (assoc "Bootstrap" ac-html-source-dirs)
    (setq ac-html-source-dirs (cons (cons "Bootstrap" 'ac-html-bootstrap-source-dir) ac-html-source-dirs))))

(provide 'ac-html-bootstrap)
;;; ac-html-bootstrap.el ends here
