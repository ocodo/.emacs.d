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
(require 'shut-up)

(require 'multi-line-candidate)
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

;; TODO: Get rid of inheritance here
(defclass multi-line-each-decorator (multi-line-respacer)
  ((respacer :initarg :respacer)
   (decorator :initarg :decorator)))

(defmethod multi-line-respace-one ((decorator multi-line-each-decorator)
                                   index candidates)
  (funcall (oref decorator decorator) (oref decorator respacer) index
           candidates))

(defmacro multi-line-pre-decorator (name &rest forms)
  "Build a constructor with name NAME that builds respacers that
execute FORMS before respacing.  FORMS can use the variables index
and candidates which will be appropriately populated by the
executor."
  `(defun ,name (decorated-respacer)
     (make-instance 'multi-line-each-decorator
                    :respacer decorated-respacer
                    :decorator (lambda (respacer index candidates)
                                 ,@forms
                                 (multi-line-respace-one respacer index candidates)))))

(defmacro multi-line-post-decorator (name &rest forms)
  "Build a constructor with name NAME that builds respacers that
execute FORMS after respacing.  FORMS can use the variables index
and candidates which will be appropriately populated by the
executor."
  `(defun ,name (respacer)
     (make-instance 'multi-line-each-decorator
                    :respacer respacer
                    :decorator (lambda (respacer index candidates)
                                 (multi-line-respace-one respacer index candidates)
                                 ,@forms))))

(defmacro multi-line-post-all-decorator (name &rest forms)
  "Build a constructor with name NAME that builds respacers that
execute FORMS after respacing all splits.  FORMS can use the
variables index and candidates which will be appropriately populated
by the executor."
  `(multi-line-post-decorator
     ,name (when (equal index (- (length candidates) 1))
             (goto-char (multi-line-candidate-position (car (last candidates))))
             ,@forms)))

(multi-line-pre-decorator multi-line-space-clearing-respacer
  (multi-line-clear-whitespace-at-point))

(multi-line-post-all-decorator multi-line-trailing-comma-respacer
  (multi-line-add-remove-or-leave-final-comma))

(multi-line-post-decorator
  multi-line-reindenting-respacer
  (shut-up
    (indent-region (multi-line-candidate-position (car candidates))
                   (multi-line-candidate-position (car (last candidates))))))

(multi-line-compose multi-line-clearing-reindenting-respacer
                    'multi-line-reindenting-respacer
                    'multi-line-space-clearing-respacer)

(defclass multi-line-space-restoring-respacer ()
  ((respacer :initarg :respacer)))

(defmethod multi-line-respace-one ((respacer multi-line-space-restoring-respacer)
                                   index candidates)
  (cl-destructuring-bind (startm . endm) (multi-line-space-markers)
    (let* ((start (marker-position startm))
           (end (marker-position endm))
           (spanning-start (progn
                             (goto-char (- start 1))
                             (point-marker)))
           (spanning-end (progn
                           (goto-char (+ end 1))
                           (point-marker)))
           (space-to-restore (buffer-substring start end)))
      (delete-region start end)
      (let ((spanning-string (buffer-substring (marker-position spanning-start)
                                               (marker-position spanning-end))))
        (multi-line-respace-one (oref respacer respacer) index candidates)
        (when (equal (buffer-substring (marker-position spanning-start)
                                       (marker-position spanning-end))
                     spanning-string)
          (goto-char (marker-position startm))
          (insert space-to-restore))))))

(defun multi-line-restoring-reindenting-respacer (respacer)
  (multi-line-reindenting-respacer
   (multi-line-space-restoring-respacer :respacer respacer)))

(provide 'multi-line-decorator)
;;; multi-line-decorator.el ends here
