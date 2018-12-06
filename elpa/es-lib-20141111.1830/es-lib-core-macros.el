;;; es-lib-core-macros.el --- Random macros
;;; Version: 0.4
;;; Author: sabof
;;; URL: https://github.com/sabof/es-lib

;;; Commentary:

;; The project is hosted at https://github.com/sabof/es-lib
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:
(require 'cl-lib)

(defmacro es-silence-messages (&rest body)
  (declare (indent defun))
  `(cl-letf (((symbol-function 'message)
              (symbol-function 'ignore)))
     ,@body))
(put 'es-silence-messages 'common-lisp-indent-function
     '(&body))

(defmacro es-while-point-moving (&rest rest)
  (let ((old-point (cl-gensym)))
    `(let (,old-point)
       (while (not (equal (point) ,old-point))
         (setq ,old-point (point))
         ,@rest))))

(defmacro es-neither (&rest args)
  `(not (or ,@args)))

(defmacro es-define-buffer-local-vars (&rest list)
  "Syntax example:
\(es-define-buffer-local-vars
 mvi-current-image-file nil\)"
  (let (result)
    (while list
      (let ((name (pop list))
            (value (pop list)))
        (push `(defvar ,name ,value) result)
        (push `(make-variable-buffer-local (quote ,name)) result)))
    (cons 'progn (nreverse result))))

(defmacro es-back-pop (symbol)
  (let ( (result (cl-gensym)))
    `(let ( (,result (first (last ,symbol))))
       (setq ,symbol (butlast ,symbol))
       ,result)))

(defmacro es-back-push (what where)
  `(setq ,where (append ,where (list ,what))))

(cl-defmacro es-preserve-functions ((&rest funcs) &rest body)
  "A helper for loading packages.
Example of usage:

\(es-preserve-functions
  \(default-function-i-like1
    default-function-i-like2\)
\(require 'some-package-that-redefines-them-at-top-level\)
\)

This is a hack, and in no way it excuses package-authors who do that.
They should provide initialization functions that execute the redefinitions."
  (declare (indent 1))
  (let (( list-sym (cl-gensym))
        ( list (mapcar (lambda (func)
                         `(,func . ,(symbol-function func)))
                       funcs)))
    `(let (( ,list-sym (quote ,list))
           ( result (progn ,@body)))
       (mapcar
        (lambda (func)
          (fset (car func) (cdr func)))
        ,list-sym))))
(put 'es-preserve-functions 'common-lisp-indent-function
     '(2 2 &body))

(defmacro es-after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent 1))
  `(eval-after-load ',mode
     '(progn ,@body)))

(defmacro es-opts (mode &rest body)
  (declare (indent 1))
  (cl-assert (symbolp mode))
  (cl-assert (not (string-match-p "mode" (symbol-name mode))))
  (let (( opts-func-sym (intern (concat (symbol-name mode) "-mode-options")))
        ( hook-sym (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (cl-defun ,opts-func-sym ()
         ,@body)
       (add-hook ',hook-sym ',opts-func-sym t))))

(provide 'es-lib-core-macros)
;; es-lib-core-macros.el ends here
