;;; finalize.el --- lisp object finalization -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/elisp-finalize

;;; Commentary:

;; This package will immediately run a callback (a finalizer) after
;; its registered lisp object has been garbage collected. This allows
;; for extra resources, such as buffers and processes, to be cleaned
;; up after the object has been freed.

;; Unlike finalizers in other languages, the actual object to be
;; finalized will *not* be available to the finalizer. To help deal
;; with this, arguments can be passed to the finalizer to provide
;; context as to which object was collected. The object itself must
;; *not* be on of these arguments.

;; -- Function: `finalize-register' object finalizer &rest finalizer-args
;;      Registers an object for finalization. FINALIZER will be called
;;      with FINALIZER-ARGS when OBJECT has been garbage collected.

;; Usage:

;;     (cl-defstruct (pinger (:constructor pinger--create))
;;       process host)
;;
;;     (defun pinger-create (host)
;;       (let* ((process (start-process "pinger" nil "ping" host))
;;              (object (pinger--create :process process :host host)))
;;         (finalize-register object #'kill-process process)
;;         object))

;; There is also a "finalizable" mixin class for EIEIO that provides a
;; `finalize' generic function.

;;     (require 'finalizable)

;;; Code:

(require 'cl-lib)

(defvar finalize-objects ()
  "Collection of all objects and their finalizers to be run.")

(defun finalize--ref (thing)
  "Create a weak reference to THING."
  (let ((ref (make-hash-table :test 'eq :size 1 :weakness 'value)))
    (prog1 ref
      (setf (gethash t ref) thing))))

(defun finalize--empty-p (ref)
  "Return non-nil if value behind REF is still present."
  (zerop (hash-table-count ref)))

(cl-defun finalize-register (object finalizer &rest finalizer-args)
  "Run FINALIZER with FINALIZER-ARGS when OBJECT is garbage collected.
Returns OBJECT.

You *cannot* pass OBJECT as a finalizer argument."
  (let ((ref (finalize--ref object)))
    ;; FINALIZER-ARGS could be instead captured in a closure, but
    ;; establishing a closure here would require this package to be
    ;; byte-compiled in order to operate properly. Interpreted
    ;; closures capture the entire environment.
    (prog1 object
      (when (memq object finalizer-args)
        (error "Cannot use OBJECT as a finalizer argument."))
      (push (list finalizer finalizer-args ref) finalize-objects))))

(defun finalize--check-entry (entry)
  "Attempt to finalize ENTRY if uncollected, returning non-nil if so."
  (cl-destructuring-bind (finalizer finalizer-args ref) entry
    (when (finalize--empty-p ref)
      (prog1 t
        (apply #'run-at-time 0 nil finalizer finalizer-args)))))

(defun finalize-check ()
  "Run finalizers for any dead, registered objects."
  (setf finalize-objects
        (cl-delete-if #'finalize--check-entry finalize-objects)))

(add-hook 'post-gc-hook #'finalize-check)

(provide 'finalize)

;;; finalize.el ends here
