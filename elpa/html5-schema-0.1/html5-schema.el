;;; html5-schema.el --- Add HTML5 schemas for use by nXML  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: html, xml
;; Version: 0.1
;; URL: https://github.com/validator/validator
;; Package-Type: multi

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

;; The RelaxNG files are taken straight from
;; https://github.com/validator/validator.git's via:
;;
;;     git subtree -P schema/html5 split
;;
;; To which we manually add `locating-rules.xml` and this file.

;;; Code:

;;;###autoload
(when load-file-name
  (let* ((dir (file-name-directory load-file-name))
         (file (expand-file-name "locating-rules.xml" dir)))
    (eval-after-load 'rng-loc
      `(progn
         (add-to-list 'rng-schema-locating-files-default 'file)
         ;; FIXME: We should only push to rng-schema-locating-files-default,
         ;; since rng-schema-locating-files is a custom var, but by the time
         ;; we run, rng-schema-locating-files has already been initialized
         ;; from rng-schema-locating-files-default, so setting
         ;; rng-schema-locating-files-default doesn't have any effect
         ;; any more!
         (add-to-list 'rng-schema-locating-files ,file)
         (put 'http://whattf.org/datatype-draft 'rng-dt-compile
              #'nxml-html5-dt-compile)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.html?\\'" . nxml-mode))

(defvar nxml-html5-dt-params nil)
(defvar nxml-html5-dt-names nil)

;;;###autoload
(defun nxml-html5-dt-compile (name params)
  ;; FIXME: How/when is `params' ever used?  It seems to always be nil!
  (add-to-list 'nxml-html5-dt-params params)
  (add-to-list 'nxml-html5-dt-names name)
  ;; FIXME: We currently don't do any actual validation of datatypes!
  '(t identity))

(provide 'html5-schema)
;;; html5-schema.el ends here
