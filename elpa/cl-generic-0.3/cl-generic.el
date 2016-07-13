;;; cl-generic.el --- Forward cl-generic compatibility for Emacs<25

;; Copyright (C) 2015, 2016  Free Software Foundation, Inc

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; vcomment: Emacs-25's version is 1.0 so this has to stay below.
;; Version: 0.3

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

;; This is a forward compatibility package, which provides (a subset of) the
;; features of the cl-generic package introduced in Emacs-25, for use on
;; previous emacsen.

;; Make sure this is installed *late* in your `load-path`, i.e. after Emacs's
;; built-in .../lisp/emacs-lisp directory, so that if/when you upgrade to
;; Emacs≥25, the built-in version of the file will take precedence, otherwise
;; you could get into trouble (although we try to hack our way around the
;; problem in case it happens).

;; AFAIK, the main incompatibilities between cl-generic and EIEIO's defmethod
;;  are:
;; - EIEIO does not support multiple dispatch.  We ignore this difference here
;;   and rely on EIEIO to detect and signal the problem.
;; - EIEIO only supports primary, :before, and :after qualifiers.  We ignore
;;   this difference here and rely on EIEIO to detect and signal the problem.
;; - EIEIO does not support specializers other than classes.  We ignore this
;;   difference here and rely on EIEIO to detect and signal the problem.
;; - EIEIO uses :static instead of (subclass <foo>) and :static methods match
;;   both class arguments as well as object argument of that class.  Here we
;;   turn (subclass <foo>) into a :static qualifier and ignore the semantic
;;   difference, hoping noone will notice.
;; - EIEIO's defgeneric does not reset the function.  We ignore this difference
;;   and hope for the best.
;; - EIEIO uses `call-next-method' and `next-method-p' while cl-defmethod uses 
;;   `cl-next-method-p' and `cl-call-next-method' (simple matter of renaming).
;;   We handle that by renaming the calls in the `cl-defmethod' macro.
;; - The errors signaled are slightly different.  We make
;;   cl-no-applicable-method into a "parent" error of no-method-definition,
;;   which should cover the usual cases.
;; - EIEIO's no-next-method and no-applicable-method have different calling
;;   conventions from cl-generic's.  We don't try to handle this, so just
;;   refrain from trying to call (or add methods to) `cl-no-next-method' or
;;   `cl-no-applicable-method'.
;; - EIEIO's `call-next-method' and `next-method-p' have dynamic scope whereas
;;   cl-generic's `cl-next-method-p' and `cl-call-next-method' are lexically
;;   scoped.  The cl-defmethod here handles the common subset between the two.

;;; Code:

;; We need to handle the situation where this package is used with an Emacs
;; that comes with a real cl-generic (i.e. ≥25.1).

;; First line of defense: try to make sure the built-in cl-lib comes earlier in
;; load-path so we never get loaded:
;;;###autoload (let ((d (file-name-directory #$)))
;;;###autoload   (when (member d load-path)
;;;###autoload     (setq load-path (append (remove d load-path) (list d)))))

(require 'cl-lib nil 'noerror)

;; In Emacs≥25, cl-lib autoloads cl-defmethod and friends.

(unless (fboundp 'cl-defmethod)
  (require 'eieio)
  (require 'cl)                         ;For `labels'.

  (defalias 'cl-defgeneric 'defgeneric)

  ;; Compatibility with code which tries to catch
  ;; `cl-no-applicable-method' errors.
  (push 'cl-no-applicable-method (get 'no-method-definition 'error-conditions))

  (defalias 'cl-generic-apply #'apply)

  (defmacro cl-defmethod (name args &rest body)
    (let ((qualifiers nil))
      (while (not (listp args))
        (push args qualifiers)
        (setq args (pop body)))
      (let ((docstring (if (and (stringp (car body)) (cdr body)) (pop body))))
        ;; Backward compatibility for `no-next-method' and
        ;; `no-applicable-method', which have slightly different calling
        ;; convention than their cl-generic counterpart.
        (pcase name
          (`cl-no-next-method
           (setq name 'no-next-method)
           (setq args (cddr args)))
          (`cl-no-applicable-method
           (setq name 'no-applicable-method)
           (setq args `(,(nth 1 args) ,(nth 0 args)
                        ,(make-symbol "_ignore") . ,(nthcdr 2 args)))))
        (let ((arg1 (car args)))
          (when (eq (car-safe (car (cdr-safe arg1))) 'subclass)
            ;; There's no exact equivalent to `subclass', but :static
            ;; provides a superset which should work just as well in practice.
            (push :static qualifiers)
            (setf (cadr arg1) (cadr (cadr arg1)))))

        `(defmethod ,name ,@qualifiers ,args
           ,@(if docstring (list docstring))
           ;; We could just alias `cl-call-next-method' to `call-next-method',
           ;; and that would work, but then files compiled with this cl-generic
           ;; wouldn't work in Emacs-25 any more.
           ;; Also we fallback on `labels' if `cl-flet' is not available
           ;; (ELPA's cl-lib emulation doesn't provide cl-flet).
           ;; We don't always use `labels' because that generates warnings
           ;; in newer Emacsen where `cl-flet' is available.
           ,@(if qualifiers
                 ;; Must be :before or :after, so can't call next-method.
                 body
               `((,(if (fboundp 'cl-flet) 'cl-flet 'labels)
                      ((cl-call-next-method (&rest args)
                                            (apply #'call-next-method args))
                       (cl-next-method-p () (next-method-p)))
                  ,@body))))))))

;;;; ChangeLog:

;; 2016-07-12  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* cl-generic/cl-generic.el (cl-defmethod): Improve compatibility
;; 
;; 	More specifically, map cl-no-applicable-method to no-applicable-method.
;; 	(cl-generic-apply): New function.
;; 
;; 2015-02-18  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* cl-generic/cl-generic.el (cl-defmethod): Use cl-flet if available.
;; 
;; 2015-02-03  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/cl-generic/cl-generic.el (cl-defmethod): Fix handling of
;; 	subclass.
;; 
;; 2015-02-03  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* packages/cl-generic: Add new package
;; 


(provide 'cl-generic)
;;; cl-generic.el ends here
