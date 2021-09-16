;;; clisp-format.el --- Partial CLISP format port

;;; Commentary:

;; This could need a some clean up.

(require 'cl-lib)
(eval-when-compile (require 'gv))
(require 'cl-format-def)
(eval-when-compile
  (push '(fixnum ignore) defun-declarations-alist))

;;; Code:

(defun clisp-float-scale-exponent (arg)
  (floor (1+ (log (abs arg) 10))))

(defun clisp-format-base (base stream colon-modifier atsign-modifier
                               mincol padchar commachar commainterval
                               arg)
  (if (or (and (zerop mincol) (not colon-modifier) (not atsign-modifier))
          (not (integerp arg)))
      (let ((clisp-format-print-base base))
        (if (integerp arg)
            (clisp-princ-with-base arg stream)
          (clisp-format-padded-string
           mincol 1 0 padchar t
           (clisp-princ-with-base-to-string arg) stream)))
    (clisp-format-integer base mincol padchar commachar commainterval
                          colon-modifier atsign-modifier arg stream)))

(defun clisp-format-float-for-e
  (w d e k overflowchar padchar exponentchar plus-sign-flag arg stream)
  ;; (cl-multiple-value-bind (oldexponent mantissa)
  ;;     (float-scale-exponent (abs arg))
  
  (let* ((oldexponent (clisp-float-scale-exponent arg))
         (exponent (if (zerop arg) 0 (- oldexponent k))) ; Exponent to be printed
         (expdigits (format "%s" (abs exponent)))
         ;; expdigitsneed = number of digits, that are necessary
         ;; for the Exponent.
         (expdigitsneed (if e (max (length expdigits) e) (length expdigits)))
         ;; mantd = number of Mantissa-Digits behind the point
         (mantd (if d (if (> k 0) (1+ (- d k)) d) nil))
         ;; no rounding takes place within the first (+ 1 (abs k)) digits.
         (dmin (if (cl-minusp k) (- 1 k) nil)) ; hereafter: demand, that
         ;; mantwidth = number of available characters (or nil)
         ;; for the Mantissa (incl. sign, point)
         (mantwidth (if w (- w 2 expdigitsneed) nil)))
    (declare (simple-string expdigits) (fixnum exponent expdigitsneed))
    (if (and overflowchar w e (> expdigitsneed e))
        ;; if Overflowchar and w and e being stated, Exponent needs more room:
        (clisp-format-padding w overflowchar stream)
      (progn
        (when (and w (or plus-sign-flag (cl-minusp arg)))
          (setq mantwidth (1- mantwidth)))
        ;; mantwidth = number of available characters (or nil)
        ;;  for the Mantissa (without sign,including point)
        (cl-multiple-value-bind (mantdigits mantdigitslength
                                            leadingpoint trailingpoint point-pos)
            (clisp-format-float-to-string
             (abs arg) mantwidth mantd (- k oldexponent) dmin)
          (when w
            (setq mantwidth (- mantwidth mantdigitslength))
            (when trailingpoint
              (if (or (null mantd) (> mantd 0))
                  (setq mantwidth (- mantwidth 1))
                (setq trailingpoint nil)))
            (when leadingpoint
              (if (> mantwidth 0)
                  (setq mantwidth (- mantwidth 1))
                (setq leadingpoint nil))))
          ;; mantwidth characters remain.
          (if (and overflowchar w (cl-minusp mantwidth))
              (clisp-format-padding w overflowchar stream) ; not enough room -> overflow
            (progn
              (when (and (cl-plusp k) (< k point-pos))
                ;; format-float-to-string rounded the mantissa up above 1
                ;; so that all our assumptions are now wrong and we are
                ;; about to get (clisp-format nil "~8e" .999999d9) => " 10.0d+8"
                (let* ((shift (- point-pos 1))
                       (new-exponent (+ exponent shift))
                       (new-expdigits
                        (format "%s" (abs new-exponent))))
                  ;; shift the decimal point left
                  (dotimes (i shift)
                    (cl-rotatef (aref mantdigits (- point-pos i))
                             (aref mantdigits (- point-pos i 1))))
                  ;; update for the the exponent change
                  (when mantwidth
                    (cl-incf mantwidth (- (length expdigits)
                                       (length new-expdigits))))
                  (setq exponent new-exponent
                        expdigits new-expdigits)
                  ;; update for the trailing point change
                  (when trailingpoint
                    (setq trailingpoint nil)
                    (when mantwidth (cl-incf mantwidth)))))
              (when (and w (> mantwidth 0))
                (clisp-format-padding mantwidth padchar stream))
              (if (cl-minusp arg)
                  (write-char ?- stream)
                (if plus-sign-flag (write-char ?+ stream)))
              (when leadingpoint (write-char ?0 stream))
              (princ mantdigits stream)
              (when trailingpoint (write-char ?0 stream))
              (write-char
               (or exponentchar ?e)
               stream)
              (write-char (if (cl-minusp exponent) ?- ?+) stream)
              (when (and e (> e (length expdigits)))
                (clisp-format-padding (- e (length expdigits)) ?0 stream))
              (princ expdigits stream))))))))

(defun clisp-format-float-for-f (w d k overflowchar padchar plus-sign-flag
                                   arg stream)
  (let ((width (if w (if (or plus-sign-flag (cl-minusp arg)) (1- w) w) nil)))
    ;; width = available characters without sign
    (cl-multiple-value-bind (digits digitslength leadingpoint trailingpoint)
        (clisp-format-float-to-string arg width d k 0)
      (when (eql d 0)
        (setq trailingpoint nil)) ; d=0 -> no additional zero behind
      (when w
        (setq width (- width digitslength))
        (when leadingpoint      ; plan possibly additional zero ahead
          (if (> width 0) (setq width (1- width)) (setq leadingpoint nil)))
        (when trailingpoint     ; plan possibly additional zero behind
          (if (> width 0) (setq width (1- width)) (setq trailingpoint nil))))
      ;; width characters still remain.
      (if (and overflowchar w (cl-minusp width))
          (clisp-format-padding w overflowchar stream) ; not enough room -> overflow
        (progn
          (when (and w (> width 0)) (clisp-format-padding width padchar stream))
          (if (cl-minusp arg)
              (write-char ?- stream)
            (if plus-sign-flag (write-char ?+ stream)))
          (when leadingpoint (write-char ?0 stream))
          (princ digits stream)
          (when trailingpoint (write-char ?0 stream)))))))

(defun clisp-format-integer (base
                             mincol
                             padchar
                             commachar
                             commainterval
                             commaflag
                             positive-sign-flag
                             arg
                             stream)
  (let* ((clisp-format-print-base base))
    (if (and (zerop mincol) (not commaflag) (not positive-sign-flag))
        (clisp-princ-with-base arg stream) ; normal output does the job
      (let* ((oldstring (clisp-princ-with-base-to-string arg))
             (oldstring-length (length oldstring))
             (number-of-digits
              (if (cl-minusp arg) (1- oldstring-length) oldstring-length) )
             (number-of-commas
              (if commaflag (floor (1- number-of-digits) commainterval) 0) )
             (positive-sign (and positive-sign-flag (>= arg 0)))
             (newstring-length
              (+ (if positive-sign 1 0)              ; sign
                 oldstring-length number-of-commas)) ; digits, commas
             (newstring (make-string newstring-length 32)) )
        ;; first the sign +:
        (when positive-sign (setf (aref newstring 0) ?+))
        ;; Then convert oldstring in newstring, skipping the commas:
        (let ((oldpos oldstring-length) (newpos newstring-length))
          (cl-loop
           (cl-decf oldpos)
           (when (cl-minusp oldpos) (cl-return))
           (cl-decf newpos)
           (setf (aref newstring newpos) (aref oldstring oldpos))
           (when (and (cl-plusp number-of-commas) ; insert a comma?
                      (zerop (mod (- oldstring-length oldpos) commainterval)))
             (cl-decf newpos)
             (setf (aref newstring newpos) commachar)
             (cl-decf number-of-commas))))
        (if (zerop mincol)
            (princ newstring stream)    ; faster
          (clisp-format-padded-string mincol 1 0 padchar t newstring stream))))))


(cl-defun clisp-format-do-format-justification (stream colon-modifier atsign-modifier
                                                       mincol colinc minpad padchar
                                                       pos check-on-line-overflow firstpiece
                                                       supplementary-need line-length piecelist) ; ABI
  (if (null mincol) (setq mincol 0))
  (if (null colinc) (setq colinc 1))
  (if (null minpad) (setq minpad 0))
  (if (null padchar) (setq padchar ?\s))
  (if piecelist
      (cl-multiple-value-bind (padblocklengths width)
          (clisp-format-justified-segments
           mincol colinc minpad
           colon-modifier atsign-modifier piecelist)
        (when (and check-on-line-overflow
                   (> (+ (or pos 0) width (or supplementary-need 0))
                      (or line-length   ;(sys::line-length stream)
                          72)))
          (princ firstpiece stream))
        (cl-do ((i 0 (1+ i)))
            (nil)
          (when (aref padblocklengths i)
            (clisp-format-padding (aref padblocklengths i) padchar stream))
          (when (null piecelist) (cl-return))
          (princ (pop piecelist) stream)))
    (clisp-format-padding mincol padchar stream)))

(defun clisp-format-justified-segments
    (mincol colinc minpad justify-left justify-right piecelist)
  (declare (fixnum mincol colinc minpad))
  (let ((piecesnumber 0)
        (pieceswidth 0))
    (dolist (piece piecelist)
      (declare (simple-string piece))
      (cl-incf piecesnumber)
      (cl-incf pieceswidth (string-width piece)))
    (let* ((new-justify-left
            (or justify-left (and (= piecesnumber 1) (not justify-right))))
           (padblocks (+ piecesnumber -1 ; number of insertion-points
                         (if new-justify-left 1 0) (if justify-right 1 0)))
           (width-need (+ pieceswidth (* padblocks minpad)))
           (width (+ mincol
                     (if (<= width-need mincol)
                         0
                       (* (ceiling (- width-need mincol) colinc) colinc)))))
      (declare (fixnum piecesnumber pieceswidth padblocks width-need width))
      (cl-multiple-value-bind (padwidth rest)
          (cl-values 
           (floor (- width pieceswidth) padblocks)
           (mod (- width pieceswidth)
                padblocks)) 
        (let ((padblock-lengths
               (make-vector (1+ piecesnumber) padwidth)))
          (unless new-justify-left (setf (aref padblock-lengths 0) nil))
          (unless justify-right
            (setf (aref padblock-lengths piecesnumber) nil))
          (cl-do ((i 0 (1+ i)))
              ((zerop rest))
            (when (aref padblock-lengths i)
              (cl-incf (aref padblock-lengths i))
              (cl-decf rest)))
          (cl-values padblock-lengths width))))))



(defun clisp-format-new-roman (arg stream)
  (cl-format-check-type arg 'integerp)
  (unless (and (<= 1 arg) (<= arg 3999))
    (cl-format-eval-error
     "Argument out of range" '(and (<= 1 arg) (<= arg 3999))))
  (cl-do ((charlistr       '(?M ?D ?C ?L ?X ?V ?I) (cdr charlistr))
          (valuelistr     '(1000 500 100 50  10   5   1 ) (cdr valuelistr))
          (lowercharlistr  '(?C ?C ?X ?X ?I ?I    ) (cdr lowercharlistr))
          (lowervaluelistr '(100 100 10  10   1   1   0 ) (cdr lowervaluelistr))
          (value arg
                 (cl-multiple-value-bind (multiplicity restvalue)
                     (cl-values 
                      (floor value (cl-first valuelistr))
                      (mod value
                           (cl-first valuelistr))) 
                   (dotimes (i multiplicity) (write-char (cl-first charlistr)
                                                         stream))
                   (let ((loweredvalue (- (cl-first valuelistr)
                                          (cl-first lowervaluelistr))))
                     (if (>= restvalue loweredvalue)
                         (progn
                           (write-char (cl-first lowercharlistr) stream)
                           (write-char (cl-first charlistr) stream)
                           (- restvalue loweredvalue))
                       restvalue)))))
      ((zerop value))))

(defun clisp-format-old-roman (arg stream)
  (cl-format-check-type arg 'integerp)
  (unless (and (<= 1 arg) (<= arg 4999))
    (cl-format-eval-error
     "Argument out of range" '(and (<= 1 arg) (<= arg 4999))))
  (cl-do ((charlistr  '(?M  ?D ?C ?L ?X ?V ?I) (cdr charlistr))
          (valuelistr '(1000 500 100 50  10   5   1) (cdr valuelistr))
          (value arg (cl-multiple-value-bind (multiplicity restvalue)
                         (cl-values 
                          (floor value (cl-first valuelistr))
                          (mod value
                               (cl-first valuelistr))) 
                       (dotimes (i multiplicity)
                         (write-char (cl-first charlistr) stream))
                       restvalue)))
      ((zerop value))))


(defconst clisp-format-ordinal-ones
  (vector nil "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth"
          "ninth" "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
          "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))

(defconst clisp-format-cardinal-ones
  (vector nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
          "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen"
          "seventeen" "eighteen" "nineteen"))

(defconst clisp-format-cardinal-tens
  (vector nil nil "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty"
          "ninety"))

(defun clisp-format-ordinal (arg stream) ; arg Integer
  (if (zerop arg)
      (princ "zeroth" stream)
    (progn
      (when (cl-minusp arg) (princ "minus " stream) (setq arg (- arg)))
      (cl-multiple-value-bind (hundreds tens-and-ones)
          (cl-values (floor arg 100)
                     (mod arg 100))
        (when (> hundreds 0) (clisp-format-cardinal (* hundreds 100) stream))
        (if (zerop tens-and-ones)
            (princ "th" stream)
          (cl-multiple-value-bind (tens ones) (cl-values (floor tens-and-ones 10)
                                                         (mod tens-and-ones 10)) 
            (when (> hundreds 0) (write-char ?\s stream))
            (cond ((< tens 2)
                   (princ (aref clisp-format-ordinal-ones tens-and-ones) stream))
                  ((zerop ones)
                   (princ
                    (aref (vector nil "tenth" "twentieth" "thirtieth" "fortieth"
                                  "fiftieth" "sixtieth" "seventieth" "eightieth"
                                  "ninetieth")
                          tens)
                    stream))
                  (t (princ (aref clisp-format-cardinal-tens tens) stream)
                     (write-char ?- stream)
                     (princ (aref clisp-format-ordinal-ones ones)
                                                stream)))))))))

(defun clisp-format-cardinal (arg stream) ; arg Integer
  (if (zerop arg)
      (princ "zero" stream)
    (progn
      (when (cl-minusp arg) (princ "minus " stream) (setq arg (- arg)))
      (cl-labels ((blocks1000 (illions-list arg) ; decomposition in 1000er-Blocks
                    (when (null illions-list)
                      (cl-format-eval-error "The argument too large" arg))
                    (cl-multiple-value-bind (thousands small)
                        (cl-values (truncate arg 1000)
                                   (- arg
                                      (* 1000
                                         (truncate arg
                                                   1000)))) 
                      (when (> thousands 0)
                        (blocks1000 (cdr illions-list) thousands))
                      (when (> small 0)
                        (when (> thousands 0)
                          (princ ", " stream))
                        (clisp-format-small-cardinal small stream)
                        (princ (car illions-list) stream)))))
        (blocks1000
                                        ; American (billion=10^9)
         '("" " thousand" " million" " billion" " trillion" " quadrillion"
           " quintillion" " sextillion" " septillion" " octillion"
           " nonillion" " decillion" " undecillion" " duodecillion"
           " tredecillion" " quattuordecillion" " quindecillion"
           " sexdecillion" " septendecillion" " octodecillion"
           " novemdecillion" " vigintillion")
         arg)))))

(defun clisp-format-small-cardinal (arg stream)
  (cl-multiple-value-bind (hundreds tens-and-ones)
      (cl-values
       (truncate arg 100)
       (- arg
          (* 100
             (truncate arg 100)))) 
    (when (> hundreds 0)
      (princ (aref clisp-format-cardinal-ones hundreds) stream)
      (princ " hundred" stream))
    (when (> tens-and-ones 0)
      (when (> hundreds 0) (princ " and " stream))
      (cl-multiple-value-bind (tens ones)
          (cl-values (truncate tens-and-ones 10)
                     (- tens-and-ones
                        (* 10
                           (truncate tens-and-ones
                                     10)))) 
        (if (< tens 2)
            (princ
             (aref clisp-format-cardinal-ones tens-and-ones) stream)
          (progn
            (princ
             (aref clisp-format-cardinal-tens tens) stream)
            (when (> ones 0)
              (write-char ?- stream)
              (princ (aref clisp-format-cardinal-ones ones) stream))))))))



(defun clisp-format-padded-string (mincol colinc minpad padchar padleftflag
                                          str stream)
  (let* ((need (+ (string-width str) minpad)) ; it least that number of columns
         (auxpad (if (< need mincol)
                     (* (ceiling (- mincol need) colinc) colinc)
                   0))) ; this many additional characters
    (unless padleftflag (princ str stream))
    (clisp-format-padding (+ minpad auxpad) padchar stream)
    (when padleftflag (princ str stream))))

(defun clisp-format-padding (count char stream)
  (dotimes (i count) (write-char char stream)))

(defun clisp-princ-with-base (object &optional stream)
  (let ((base (if (boundp 'clisp-format-print-base)
                  clisp-format-print-base
                10)))
    (princ (cond
            ((/= base 10)
             (require 'calc)
             (let ((calc-eval-error t)
                   (numstart (if (> base 9) 3 2))
                   (number (calc-eval `(,object calc-number-radix
                                                ,base))))
               (when (and (stringp object)
                          (> (length object) 0)
                          (eq ?+ (aref object 0)))
                 (cl-decf numstart)
                 (aset number numstart ?+))
               (downcase (substring number numstart))))
            (t object))
           stream)))

(defun clisp-princ-with-base-to-string (object)
  (with-output-to-string
    (clisp-princ-with-base object)))



(defun clisp-format-float-to-string (arg &optional width digits scale dmin)
  (or scale (setq scale 0))
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (princ (abs arg))
      (goto-char (point-min))
      (if (looking-at "[+-]?\\([01]\\.0e\\+\\)NaN\\|INF")
          (list (buffer-string)
                (1- (point-max))
                nil nil nil t)
        (cl-incf scale (clisp-format-float-to-string--expand-exponent))
        (goto-char 1)
        (let (dot-pos)
          (if (search-forward "." nil t)
              (setq dot-pos (match-beginning 0))
            (goto-char (point-max))
            (setq dot-pos (point-max))
            (princ "."))
          (setq dot-pos (copy-marker dot-pos t))
          (cond
           ((< scale 0)
            (goto-char 1)
            (princ (make-string (abs (min 0 (- dot-pos (1+ (abs scale)))))
                                ?0)))
           ((> scale 0)
            (end-of-line)
            (princ (make-string (max 0 (- (+ scale dot-pos)
                                          (point) -1))
                                ?0))))
          (goto-char dot-pos)
          (when (/= scale 0)
            (delete-char 1)
            (forward-char scale)
            (when (bobp)
              (insert ?0))
            (princ ".")
            (set-marker dot-pos (1- (point))))

          (goto-char 1)
          (while (= (char-after 1) ?0)
            (delete-char 1))
          (goto-char dot-pos)

          (when (or width digits)
            (clisp-format-float-to-string--round
             (max (or dmin 0)
                  (or digits (max (if (< (abs arg) 1) 1 0)
                                  (+ (if (< (abs arg) 1) 1 0)
                                     (- width dot-pos)))))
             (+ 0 dot-pos)))

          (when (not digits)
            (goto-char (point-max))
            (while (= (char-before) ?0)
              (delete-char -1)))

          (when width
            (if (and (> (1- (point-max)) width)
                     (= ?0 (char-after 1)))
                (progn (goto-char 1)
                       (delete-char 1)))
            (if (and (not digits)
                     (> (1- (point-max)) width)
                     (= dot-pos (- (point-max) 2))
                     (= ?0 (char-before (point-max))))
                (progn
                  (goto-char (point-max))
                  (delete-char -1))))
          (when (and dmin (> dmin 0)
                     (< (- (point-max) dot-pos 1)
                        dmin))
            (goto-char (point-max))
            (insert (make-string (- dmin (- (point-max) dot-pos 1)) ?0)))
          (list (buffer-string)
                (1- (point-max))
                (= ?. (char-after 1))
                (= ?. (char-before (point-max)))
                (- dot-pos 1)))))))

(defun clisp-format-float-to-string--expand-exponent ()
  (goto-char (point-max))
  (skip-chars-backward "0-9")
  (skip-chars-backward "\\-+")
  (if (eq (char-before) ?e)
      (prog1
          (save-excursion (read (current-buffer)))
        (delete-region (1- (point)) (point-max)))
    0))

(eval-when-compile
  (defmacro cl-defsetf (name arg1 &rest args)
    "Define a `setf' method.
This macro is an easy-to-use substitute for `define-setf-expander'
that works well for simple place forms.

In the simple `defsetf' form, `setf's of the form (setf (NAME
ARGS...) VAL) are transformed to function or macro calls of the
form (FUNC ARGS... VAL).  For example:

  (defsetf aref aset)

You can replace this form with `gv-define-simple-setter'.

Alternate form: (defsetf NAME ARGLIST (STORE) BODY...).

Here, the above `setf' call is expanded by binding the argument
forms ARGS according to ARGLIST, binding the value form VAL to
STORE, then executing BODY, which must return a Lisp form that
does the necessary `setf' operation.  Actually, ARGLIST and STORE
may be bound to temporary variables which are introduced
automatically to preserve proper execution order of the arguments.
For example:

  (defsetf nth (n x) (v) \\=`(setcar (nthcdr ,n ,x) ,v))

You can replace this form with `gv-define-setter'.

\(fn NAME [FUNC | ARGLIST (STORE) BODY...])"
    (declare (debug
              (&define name
                       [&or [symbolp &optional stringp]
                            [cl-lambda-list (symbolp)]]
                       cl-declarations-or-string def-body)))
    (if (and (listp arg1) (consp args))
        ;; Like `gv-define-setter' but with `cl-function'.
        `(gv-define-expander ,name
           (lambda (do &rest args)
             (gv--defsetter ',name
                            (cl-function
                             (lambda (,@(car args) ,@arg1) ,@(cdr args)))
			    do args)))
      `(gv-define-simple-setter ,name ,arg1 ,(car args)))))

(cl-defsetf char-after (&optional pos) (char)
  `(save-excursion
     (let ((char ,char)
           (pos ,pos))
       (and pos (goto-char pos))
       (delete-char 1)
       (insert char)
       char)))

(cl-defsetf char-before (&optional pos) (char)
  `(setf (char-after (1- ,pos)) ,char))

(defun clisp-format-float-to-string--round (digits dot-pos)
  (if (< (max digits 0) (- (point-max) (1+ dot-pos)))
      (progn
        (setq digits (max digits 0))
        (goto-char (1+ dot-pos))
        (forward-char digits)
        (let ((digit (char-after)))
          (delete-region (point) (point-max))
          (backward-char)
          (when (>= digit ?5)
            (when (= ?. (char-after))
              (backward-char))
            (setq digit (cl-incf (char-after)))
            (while (and (not (bobp))
                        (= digit (1+ ?9)))
              (cl-decf (char-after) 10)
              (backward-char)
              (when (= ?. (char-after))
                (backward-char))
              (setq digit (cl-incf (char-after))))
            (when (= digit (1+ ?9))
              (cl-decf (char-after) 10)
              (insert ?1)))))
    (if (> digits 0)
        (progn
          (goto-char (point-max))
          (insert (make-string (- digits (- (point-max) (1+ dot-pos))) ?0))))))


(provide 'clisp-format)

;;; clisp-format.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
