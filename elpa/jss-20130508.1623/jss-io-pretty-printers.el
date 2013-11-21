;;; jss-io-pretty-printers.el -- utility code for formating/highlighting/indenting browser responses
;;
;; Copyright (C) 2013 Edward Marco Baringer
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(require 'cl)
(require 'json)

(defvar jss-io-cleaners (make-hash-table :test 'equal))

(defmacro* define-jss-io-cleaner (content-type (data) &body body)
  (declare (indent 2))
  (let ((type (cl-gensym)))
    `(let ((,type ',content-type))
       (dolist (type (if (listp ,type) ,type (list ,type)))
         (setf (gethash type jss-io-cleaners) (lambda (,data) ,@body)))
       ',content-type)))

(defun jss-io-fontify-data-with-mode (data mode)
  (let (out (buffer (generate-new-buffer " *jss-fontification*")))
    (with-current-buffer buffer
      (insert data)
      (funcall mode)
      (indent-region (point-min) (point-max))
      (setf out (buffer-substring (point-min) (point-max))))
    out))

(define-jss-io-cleaner "text/html" (html)
  (jss-io-fontify-data-with-mode html 'nxml-mode))

(define-jss-io-cleaner "text/css" (css)
  (jss-io-fontify-data-with-mode css 'css-mode))

(define-jss-io-cleaner ("application/javascript" "text/javascript") (js)
  (jss-io-fontify-data-with-mode js (if (fboundp 'js2-mode)
                                        'js2-mode
                                      'js-mode)))

(define-jss-io-cleaner ("application/json") (json)
  (let (parsed)
    (with-temp-buffer
      (insert json)
      (goto-char (point-min))
      (setf parsed (json-read)))
    (pp-to-string parsed)))

(provide 'jss-io-pretty-printers)
