;;; multi-line-decorator.el --- multi-line statements -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Ivan Malison

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

;; multi-line-decorator defines a collection of decorator respacers
;; that can be used to add behavior to existing respacers.

;;; Code:

(require 'eieio)
(require 'multi-line-respace)
(require 'multi-line-shared)

(put 'multi-line-pre-decorator 'lisp-indent-function 'defun)
(put 'multi-line-post-decorator 'lisp-indent-function 'defun)
(put 'multi-line-post-all-decorator 'lisp-indent-function 'defun)

(defmacro multi-line-compose (name &rest funcs)
  "Build a new function with NAME that is the composition of FUNCS."
  `(defun ,name (arg)
     (multi-line-compose-helper ,funcs)))

(defmacro multi-line-compose-helper (funcs)
  "Builds funcalls of FUNCS applied to the arg."
  (if (equal (length funcs) 0)
      (quote arg)
    `(funcall ,(car funcs) (multi-line-compose-helper ,(cdr funcs)))))

(defclass multi-line-each-decorator (multi-line-respacer)
  ((respacer :initarg :respacer)
   (decorator :initarg :decorator)))

(defmethod multi-line-respace-one ((decorator multi-line-each-decorator)
                                   index markers)
  (funcall (oref decorator :decorator) (oref decorator :respacer) index markers))

(defclass multi-line-decorator (multi-line-respacer)
  ((respacer :initarg :respacer)
   (decorator :initarg :decorator)))

(defmethod multi-line-respace ((decorator multi-line-decorator) markers)
  (funcall
   (oref decorator :decorator) (oref decorator :respacer) markers))

(defmacro multi-line-pre-decorator (name &rest forms)
  "Build a constructor with name NAME that builds respacers that
execute FORMS before respacing.  FORMS can use the variables index
and markers which will be appropriately populated by the
executor."
  `(defun ,name (respacer)
     (make-instance
      multi-line-each-decorator
      :respacer respacer
      :decorator (lambda (respacer index markers)
                   ,@forms
                   (multi-line-respace-one respacer index markers)))))

(defmacro multi-line-post-decorator (name &rest forms)
  "Build a constructor with name NAME that builds respacers that
qexecute FORMS after respacing.  FORMS can use the variables index
and markers which will be appropriately populated by the
executor."
  `(defun ,name (respacer)
     (make-instance
      multi-line-each-decorator
      :respacer respacer
      :decorator (lambda (respacer index markers)
                   (multi-line-respace-one respacer index markers)
                   ,@forms))))

(defmacro multi-line-post-all-decorator (name &rest forms)
  "Build a constructor with name NAME that builds respacers that
execute FORMS after respacing all splits.  FORMS can use the
variables index and markers which will be appropriately populated
by the executor."
  `(multi-line-post-decorator
   ,name (when (equal index (- (length markers) 1))
            ,@forms)))

(multi-line-pre-decorator multi-line-space-clearing-respacer
  (multi-line-clear-whitespace-at-point))

(multi-line-post-decorator multi-line-trailing-comma-respacer
  (multi-line-add-trailing-comma index markers))

(multi-line-post-all-decorator multi-line-reindenting-respacer
  (indent-region (marker-position (car markers))
                 (marker-position (nth index markers))))

(multi-line-compose multi-line-clearing-reindenting-respacer
                    'multi-line-reindenting-respacer
                    'multi-line-space-clearing-respacer)

(provide 'multi-line-decorator)
;;; multi-line-decorator.el ends here
