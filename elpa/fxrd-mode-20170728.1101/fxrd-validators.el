;;; fxrd-validators.el --- Validators for fixed-field-width files  -*- lexical-binding:t -*-
;;; We need lexical-binding so we can create closures.

(require 'eieio-base)
(require 's)

(defclass fxrd-validator (eieio-named)
  (;; Public slots
   (pad :initarg :pad
        :initform ""
        :type string
        :custom string
        :documentation "The value to pad with")
   (align :initarg :align
          :initform :right
          :type symbol
          :custom symbol
          :documentation "The alignment of the field. One of :right or :left")
   (const :initarg :const
          :initform nil
          :type (or null integer string)
          :custom (or null integer string)
          :documentation "A constant value for this field")
   (enum :initarg :enum
         :initform nil
         :type list
         :custom list
         :documentation "Possible enum values for this field")
   (min :initarg :min
        :initform nil
        :type (or null integer)
        :custom (or null integer)
        :documentation "Minimum value for this field")
   (max :initarg :max
        :initform nil
        :type (or null integer)
        :custom (or null integer)
        :documentation "Maximum value for this field")
   (reserved :initarg :reserved
             :initform nil
             :type boolean
             :custom boolean
             :documentation "Indicates that the field is reserved")
   ;; Private slots
   (comp-transform :initform #'identity
                   :documentation "Transform to be used when comparing fields")
   (const-eq :initform #'eq
             :documentation "Equality function for const values")
   (regex :initform "^.*$"
          :documentation "Regex to validate field against"))
  "The base validator class for all field validation types")
(defmethod fxrd-validate (val field)
  "Validate the field with the given validator"
  (fxrd-general-validator val field))

(define-error 'validation-error "Validation error")

(defun fxrd-general-validator (val field-value)
  "Performs general validations on a field value.

Returns t on success, signals `validation-error' with an
appropriate message in the DATA field on errors.

This is the base validator for all fields. It may be further
specialized if necessary."
  (unless field-value
    (signal 'validation-error (format "nil value for field")))
  (with-slots (comp-transform
               align
               const
               enum
               const-eq
               pad
               regex) val
    (let ((regex-w-pad (cond ((eq align :right) (concat "^" pad "*" regex "$"))
                              (t (concat "^" regex pad "*$" )))))
      (unless (string-match regex-w-pad field-value)
        (signal 'validation-error (format "Failed to match regex %s" regex-w-pad)))
      (let* ((val-for-comparison (funcall comp-transform field-value))
             (trimmed-val (cond ((eq align :right) (s-trim-left field-value))
                                (t (s-trim-right field-value)))))
        (when enum
          (unless (or (member val-for-comparison enum)
                      ;; Only works/necessary for strings
                      (member trimmed-val enum))
            (signal 'validation-error (format "%s not one of enum values %s" val-for-comparison enum))))
        (when const
          (unless (funcall const-eq const val-for-comparison)
            (signal 'validation-error (format "%s not equal to const %s" const val-for-comparison))))
        ;; All done
        t))))


(defclass fxrd-numeric-v (fxrd-validator)
  ((pad :initform "0")
   (comp-transform :initform #'string-to-number)
   (regex :initform "[[:digit:]]*"))
  "Integer fields")
(defmethod fxrd-validate :after ((val fxrd-numeric-v) field-value)
  (let ((value (funcall (slot-value val 'comp-transform) field-value))
        (min (slot-value val 'min))
        (max (slot-value val 'max)))
    (or (cond ((and min max) (<= min value max))
              (min (<= min value))
              (max (<= value max))
              (t t))
        (signal 'validation-error (format "Value %s is outside of range %s - %s"
                                          value min max)))))

(defclass fxrd-decimal-v (fxrd-numeric-v)
  ((comp-transform :initform #'string-to-number)
   (regex :initform "-?[[:digit:]]*\\(\\.[[:digit:]]+\\)"))
  "Numeric fields with a decimal point (floating-point values)")

(defclass fxrd-alphanumeric-v (fxrd-validator)
  ((pad :initform " ")
   (align :initform :left)
   (const-eq :initform #'string=)
   (regex :initform "[[:print:]]*" field-value))
  "Any printable characters")

(provide 'fxrd-validators)
