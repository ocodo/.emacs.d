;;; finalizable.el --- finalize generic function for EIEIO -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Summary:

;; This extends finalizers to EIEIO with a `finalize' generic function
;; and a `finalizable' mixin class.

;;; Code:

(require 'eieio)
(require 'finalize)

(defclass finalizable ()
  ()
  (:documentation
   "Classes that inherit from this mixin must implement the
`finalize' method, which will receive a copy of the original
object. The copy is made after `initialize-instance'.")
  :abstract t)

(defmethod initialize-instance :after ((object finalizable) &key)
  (finalize-register object #'finalize (copy-sequence object)))

(defgeneric finalize (object-copy)
  "Finalize OBJECT-COPY after it has been garbage collected.
OBJECT-COPY is a copy made just after `initialize-instance' using
`copy-sequence'. The object itself is unavailable to this method.")

(provide 'finalizable)

;;; finalizable.el ends here
