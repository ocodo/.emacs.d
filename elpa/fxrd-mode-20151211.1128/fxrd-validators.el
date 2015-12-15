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
          :initform "RIGHT"
          :type string
          :custom string
          :documentation "The alignment of the field")
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
  (let* ((comp-transform (slot-value val 'comp-transform))
         (val-for-comparison (funcall comp-transform field-value))
         (align (slot-value val 'align))
         (trimmed-val (cond ((string= align "RIGHT")
                             (s-trim-left field-value))
                            (t (s-trim-right field-value))))
         (const (slot-value val 'const))
         (enum (slot-value val 'enum))
         (const-eq (slot-value val 'const-eq))
         (pad (slot-value val 'pad))
         (regex (slot-value val 'regex))
         (regex-w-pad (cond ((string= align "RIGHT") (concat "^" pad "*" regex "$"))
                            (t (concat "^" regex pad "*$" )))))
    (unless (string-match regex-w-pad field-value)
      (signal 'validation-error (format "Failed to match regex %s" regex-w-pad)))
    (when enum
      (unless (or (member val-for-comparison enum)
                  ;; Only works/necessary for strings
                  (member trimmed-val enum))
        (signal 'validation-error (format "%s not one of enum values %s" val-for-comparison enum))))
    (when const
      (unless (funcall const-eq const val-for-comparison)
        (signal 'validation-error (format "%s not equal to const %s" const val-for-comparison))))
    ;; All done
    t))


(defclass fxrd-numeric-v (fxrd-validator)
  ((pad :initform "0")
   (comp-transform :initform #'string-to-int)
   (regex :initform "[[:digit:]]*"))
  "Integer fields")
(defmethod fxrd-validate ((val fxrd-numeric-v) field-value)
  (unless field-value
    (signal 'validation-error (format "nil value for numeric field")))
  (let ((value (funcall (slot-value val 'comp-transform) field-value))
        (min (slot-value val 'min))
        (max (slot-value val 'max)))
    (and (fxrd-general-validator val field-value)
         (or (cond ((and min max) (<= min value max))
                (min (<= min value))
                (max (<= value max))
                (t t))
             (signal 'validation-error (format "Value %s is outside of range %s - %s"
                                               value min max))))))

(defclass fxrd-decimal-v (fxrd-numeric-v)
  ((comp-transform :initform #'string-to-number)
   (regex :initform "-?[[:digit:]]*\\(\\.[[:digit:]]+\\)"))
  "Numeric fields with a decimal point (floating-point values)")

(defclass fxrd-alphanumeric-v (fxrd-validator)
  ((pad :initform " ")
   (align :initform "LEFT")
   (const-eq :initform #'string=)
   (regex :initform "[[:print:]]*" field-value))
  "Any printable characters")

(provide 'fxrd-validators)
