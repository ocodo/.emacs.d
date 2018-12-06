;;; cl-format-builtins.el --- Common Lisp format directives.

;; Copyright (C) 2012  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: extensions

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
;;

(require 'clisp-format)
(require 'cl-format-def)

;; when debugging
;; (setq cl-format-directives nil)

;;; Code:

;; 22.3.1 FORMAT Basic Output

(define-cl-format-directive ?c (args at-flag colon-flag)
  "The next ARG should be a character; it is printed according to
the modifier flags.

Without modifier, format the character as `format's %c would.

~:c spells out the names of the control bits and represents
non-printing characters by their names.  ~:@c is even more
forthcoming, e.g. it prints Control-Meta-x instead of just C-M-x.
For this two variants, ARG may be any value accepted by
`single-key-description' and does not actually have to be a
character.

~@c prints the character so that the Lisp reader can read it,
using ?\\ syntax.

Signals an error, if ARG is not a character, respectively a key
event."
  (let ((char (cl-format-check-and-pop args)))
    (if (not colon-flag)
        (progn
          (cl-format-check-type char 'characterp)
          (if (not at-flag)
              (progn
                (princ (format "%c" char)))
            (princ (cl-format-char-to-read-syntax char))))
      (if (not at-flag)
          (princ (single-key-description char))
        (princ (single-key-description-forthcoming char))))
    args))

(defun single-key-description-forthcoming (key)
  (let* ((trans '((?C . "Control")
                  (?M . "Meta")
                  (?A . "Alt")
                  (?s . "Super")
                  (?H . "Hyper")
                  (?S . "Shift")))
         (rx (format "\\([%s]\\)-" (apply 'string (mapcar 'car trans)))))
    (with-temp-buffer
      (save-excursion
        (insert
         (single-key-description key)))
      (while (looking-at rx)
        (replace-match (cdr (assq (string-to-char
                                   (match-string 1))
                                  trans)) t t nil 1)
        ;; replace-match moved to the end of Modifier
        (or (eobp) (forward-char)))
      (buffer-string))))

(defun cl-format-char-to-read-syntax (char)
  (or (characterp char)
      (signal 'wrong-type-argument
              (list 'characterp char)))
  (let ((repr (list ?\?)))
    (when (memq char
                ;;(append "()[]\\;'`\"#., \n\t\l\r\e" nil)
                '(40 41 91 93 92 59 39 96 34 35 46 44 32 10 9 108 13 27))
      (push ?\\ repr))
    (push (case char
            (?\s ?s)
            (?\n ?n)
            (?\t ?t)
            (?\l ?l)
            (?\r ?r)
            (?\e ?e)
            (t char)) repr)
    (apply 'string (nreverse repr))))

(define-cl-format-directive ?% (args at &parameter (count 1))
  "This outputs a newline character, thereby terminating the
current output line and beginning a new one (see `terpri').

~n% outputs n newlines.

No arg is used. Simply putting a newline in the control string
would work, but ~% is often used because it makes the control
string look nicer in the middle of a Lisp program.

~n@% indents each newline according to the `major-mode', if the
output stream is a buffer."

  (dotimes (i count)
    (terpri)
    ;; cl-format has set up a buffer, which is either the output
    ;; stream or a unrelated temp buffer.  Which makes this a no-op
    ;; in the later case.
    (and at (indent-according-to-mode)))
  args)

;; This directive is sacrificed for the case modifying directive ~(,
;; in order to implement generalized directive regions like ~x(...~).
;; Who needs this anyway ?

;; (define-cl-format-directive ?| (args n)
;;   (princ (make-string n ?\C-l))
;;   args)

(define-cl-format-directive ?& (args at &parameter (count 1))
  "Unless it can be determined that the output stream is already
at the beginning of a line (i.e. it is a buffer), this outputs a
newline

~n& inserts a newline under the above condition and then
outputs n-1 newlines. ~0& does nothing.

~n@& indents each newline according to the `major-mode', if the
output stream is a buffer."
  
  (let ((buf-p (buffer-live-p standard-output)))
    (when (and buf-p
               (bolp))
      (setq count (1- count)))
    (dotimes (i count)
      (terpri)
      (and at (indent-according-to-mode))))
  args)

(define-cl-format-directive ?~ (args &parameter (count 1))
  "This outputs a tilde. ~n~ outputs n tildes."
  (princ (make-string count ?~))
  args)


;; 22.3.2 FORMAT Radix Control


(define-cl-format-directive ?r (args atsign colon
                                     (radix nil) (mincol 0)
                                     (padchar ?\s) (commachar ?\,)
                                     (commainterval 3))
"~r prints ARG in radix RADIX. The modifier flags and any
remaining parameters are used as for the ~d directive. Indeed, ~d
is the same as ~10r.

If no parameters are given to ~r, then an entirely different
interpretation is given.

The argument should be an integer; suppose it is 4. Then ~r prints ARG
as a cardinal English number: four; ~:r prints ARG as an ordinal
English number: fourth; ~@r prints ARG as a Roman numeral: IV; and
~:@r prints ARG as an old Roman numeral: IIII.

For ~@r and ~:@r the following limits must be observed.

    ~@:r : 1 <= ARG <= 4999
     ~@r : 1 <= ARG <= 3999

If ARG is outside this limits, ~@r and ~@:r signal an error."

  (let ((arg (cl-format-check-and-pop args)))
    (if radix
        (clisp-format-integer radix mincol padchar commachar commainterval
                              colon atsign
                              arg standard-output)
      (if atsign
          (if (integerp arg)
              (if colon
                  (clisp-format-old-roman arg standard-output)
                (clisp-format-new-roman arg standard-output))
            (cl-format-eval-error
             "The ~r and ~:r directives require an integer argument"
             arg))
        (if colon
            (clisp-format-ordinal arg standard-output)
          (clisp-format-cardinal arg standard-output)))))
  args)

(define-cl-format-directive ?d (args at-flag colon-flag
                                     (mincol 0) (padchar ?\s)
                                     (commachar ?\,) (commainterval 3))
"An ARG, which should be an integer, is printed in decimal radix. ~d
will never put a decimal point after the number.

~mincolD uses a column width of mincol; spaces are inserted on the
left if the number requires fewer than mincol columns for its digits
and sign. If the number doesn't fit in mincol columns, additional
columns are used as needed.

~mincol,padcharD uses padchar as the pad character instead of space.

If ARG is not an integer, it is printed in ~a format and decimal base.

The @ modifier causes the number's sign to be printed always; the
default is to print it only if the number is negative. The : modifier
causes commas to be printed between groups of three digits; the third
prefix parameter may be used to change the character used as the
comma. Thus the most general form of ~d is ~mincol,padchar,commacharD.

The fourth parameter is the commainterval. This must be an integer; if
it is not provided, it defaults to 3. This parameter controls the
number of digits in each group separated by the commachar."

  (clisp-format-base
   10 standard-output colon-flag at-flag
   mincol padchar commachar commainterval
   (cl-format-check-and-pop args))
  args)

(define-cl-format-directive ?b (args at-flag colon-flag
                                     (mincol 0) (padchar ?\s)
                                     (commachar ?\,) (commainterval 3))
"This is just like ~d but prints in binary radix (radix 2) instead of
decimal."

  (clisp-format-base
   2 standard-output colon-flag at-flag
   mincol padchar commachar commainterval
   (cl-format-check-and-pop args))
  args)

(define-cl-format-directive ?o (args at-flag colon-flag
                                     (mincol 0) (padchar ?\s)
                                     (commachar ?\,) (commainterval 3))
"This is just like ~d but prints in octal radix (radix 8) instead of
decimal."

  (clisp-format-base
   8 standard-output colon-flag at-flag
   mincol padchar commachar commainterval
   (cl-format-check-and-pop args))
  args)


(define-cl-format-directive ?x (args at-flag colon-flag
                                     (mincol 0) (padchar ?\s)
                                     (commachar ?\,) (commainterval 3))
"This is just like ~d but prints in hexadecimal radix (radix 16)
instead of decimal."

  (clisp-format-base
   16 standard-output colon-flag at-flag
   mincol padchar commachar commainterval
   (cl-format-check-and-pop args))
  args)



;; 22.3.3 FORMAT Floating-Point Printers

(defun cl-format-float-nan/inf (arg width at-flag overflowchar padchar)
  (let* ((str (with-output-to-string (princ arg)))
         (print-plus-sign (and at-flag
                               (not (= ?- (aref str 0)))))
         (abbrev (concat (if (eq (aref str 0) ?-)
                             "-")
                         (substring str (- (length str) 3))))
         (rest-width (and width
                          (max (- width (length str))
                               (- width (length abbrev))))))

    (when (and width print-plus-sign) (decf rest-width))
    (if (and width
             overflowchar
             (< rest-width 0))
        (clisp-format-padding width overflowchar standard-output)
      (and rest-width
           (> rest-width 0)
           (clisp-format-padding rest-width padchar standard-output))
      (and print-plus-sign (princ "+"))
      (if (and width (> (length str) width))
          (princ abbrev)
        (princ str)))))

(define-cl-format-directive ?f (args at-flag colon-flag
                                     &parameter
                                     (width nil) (digits nil) (scale 0)
                                     (overflowchar nil)
                                     (padchar ?\s))
  "The parameter WIDTH is the width of the field to be printed; DIGITS
is the number of digits to print after the decimal point; SCALE is a
scale factor that defaults to zero.

Exactly WIDTH characters will be output. First, leading copies of the
character PADCHAR \(which defaults to a space\) are printed, if
necessary, to pad the field on the left. If the arg is negative, then
a minus sign is printed; if the arg is not negative, then a plus sign
is printed if and only if the @ modifier was specified.  Then a
sequence of digits, containing a single embedded decimal point, is
printed; this represents the magnitude of the value of arg times
\[_24769\], rounded to DIGITS fractional digits. \(When rounding up and
rounding down would produce printed values equidistant from the scaled
value of arg, then the implementation is free to use either one. For
example, printing the argument 6.375 using the format ~4,2F may
correctly produce either 6.37 or 6.38.\) Leading zeros are not
permitted, except that a single zero digit is output before the
decimal point if the printed value is less than 1, and this single
zero digit is not output after all if WIDTH=DIGITS+1.

If it is impossible to print the value in the required format in a
field of width WIDTH, then one of two actions is taken. If the
parameter OVERFLOWCHAR is specified, then WIDTH copies of that
parameter are printed instead of the scaled value of arg. If the
OVERFLOWCHAR parameter is omitted, then the scaled value is printed
using more than WIDTH characters, as many more as may be needed.

If the WIDTH parameter is omitted, then the field is of variable
width. In effect, a value is chosen for WIDTH in such a way that no
leading pad characters need to be printed and exactly DIGITS
characters will follow the decimal point. For example, the directive
~,2F will print exactly two digits after the decimal point and as many
as necessary before the decimal point.

If the parameter DIGITS is omitted, then there is no constraint on the
number of digits to appear after the decimal point. A value is chosen
for DIGITS in such a way that as many digits as possible may be
printed subject to the width constraint imposed by the parameter WIDTH
and the constraint that no trailing zero digits may appear in the
fraction, except that if the fraction to be printed is zero, then a
single zero digit should appear after the decimal point if permitted
by the width constraint.

If both WIDTH and DIGITS are omitted, then the effect is to print the
value using ordinary free-format output; prin1 uses this format for
any number whose magnitude is either zero or between \[_24769\]
\(inclusive\) and \[_24769\] \(exclusive\).

If WIDTH is omitted, then if the magnitude of arg is so large \(or, if
DIGITS is also omitted, so small\) that more than 100 digits would
have to be printed, then an implementation is free, at its discretion,
to print the number using exponential notation instead, as if by the
directive ~f \(with all parameters to ~f defaulted, not taking their
values from the ~f directive\).

If arg is a rational number, then it is coerced to be a single-float
and then printed. \(Alternatively, an implementation is permitted to
process a rational number by any other method that has essentially the
same behavior but avoids such hazards as loss of precision or overflow
because of the coercion. However, note that if WIDTH and DIGITS are
unspecified and the number has no exact decimal representation, for
example 1/3, some precision cutoff must be chosen by the
implementation: only a finite number of digits may be printed.\)

If arg is a complex number or some non-numeric object, then it is
printed using the format directive ~wd, thereby printing it in decimal
radix and a minimum field width of WIDTH. \(If it is desired to print
each of the real part and imaginary part of a complex number using a
~f directive, then this must be done explicitly with two ~f directives
and code to extract the two parts of the complex number.\)

change_b X3J13 voted in January 1989 \(FORMAT-PRETTY-PRINT\) to
specify that format binds *print-escape* to nil during the processing
of the ~f directive.  change_exponentchar

\(defun foo \(x\)
  \(cl-format nil \"~6,2f|~6,2,1,'*f|~6,2,,'?f|~6f|~,2f|~f\"
          x x x x x x\)\)"

  (let* ((arg (cl-format-check-and-pop args)))
    (if (integerp arg) (setq arg (float arg)))
    (if (floatp arg)
        (if (cl-format-float-regular-p arg)
            (clisp-format-float-for-f width digits scale overflowchar padchar
                                      at-flag arg standard-output)
          (cl-format-float-nan/inf arg width at-flag overflowchar padchar))
      (if width
          (clisp-format-padded-string
           width 1 0 padchar t
           (clisp-princ-with-base-to-string arg)
           standard-output)
        (princ arg))))
  args)

(define-cl-format-directive ?e (args
                                atsign colon
                                (width nil) (digits nil) (exponent nil) (scale 1)
                                (overflowchar nil) (padchar ?\s)
                                (exponentchar ?e))
"The next arg is printed in exponential notation.

The parameter WIDTH is the width of the field to be printed;
DIGITS is the number of digits to print after the decimal point;
EXPONENTCHAR is the number of digits to use when printing the
exponent; SCALE is a scale factor that defaults to 1 \(not
zero\).

Exactly WIDTH characters will be output.  First, leading copies
of the character PADCHAR \(which defaults to a space\) are
printed, if necessary, to pad the field on the left.  If the arg
is negative, then a minus sign is printed; if the arg is not
negative, then a plus sign is printed if and only if the @
modifier was specified.  Then a sequence of digits, containing a
single embedded decimal point, is printed.  The form of this
sequence of digits depends on the scale factor SCALE.

If SCALE is zero, then DIGITS digits are printed after the
decimal point, and a single zero digit appears before the decimal
point if the total field width will permit it.  If SCALE is
positive, then it must be strictly less than DIGITS+2; SCALE
significant digits are printed before the decimal point, and
DIGITS-SCALE+1 digits are printed after the decimal point.  If
SCALE is negative, then it must be strictly greater than -DIGITS;
a single zero digit appears before the decimal point if the total
field width will permit it, and after the decimal point are
printed first -SCALE zeros and then DIGITS+SCALE significant
digits.  The printed fraction will be properly rounded.

Following the digit sequence, the exponent is printed.  First the
character parameter EXPONENTCHAR is printed; if this parameter is
omitted, then the character `e' is printed.  Next, either a plus
sign or a minus sign is printed, followed by EXPONENT digits
representing the power of 10 by which the printed fraction must
be multiplied to properly represent the rounded value of arg .

If it is impossible to print the value in the required format in
a field of width WIDTH, possibly because SCALE is too large or
too small or because the exponent cannot be printed in EXPONENT
character positions, then one of two actions is taken.  If the
parameter OVERFLOWCHAR is specified, then WIDTH copies of that
parameter are printed instead of the scaled value of arg.  If the
OVERFLOWCHAR parameter is omitted, then the scaled value is
printed using more than WIDTH characters, as many more as may be
needed; if the problem is that DIGITS is too small for the
specified SCALE or that EXPONENTCHAR is too small, then a larger
value is used for DIGITS or EXPONENT as may be needed.

If the WIDTH parameter is omitted, then the field is of variable
width.  In effect a value is chosen for WIDTH in such a way that
no leading pad characters need to be printed.

If the parameter DIGITS is omitted, then there is no constraint
on the number of digits to appear.  A value is chosen for DIGITS
in such a way that as many digits as possible may be printed
subject to the width constraint imposed by the parameter WIDTH,
the constraint of the scale factor SCALE, and the constraint that
no trailing zero digits may appear in the fraction, except that
if the fraction to be printed is zero, then a single zero digit
should appear after the decimal point if the width constraint
allows it.

If all of WIDTH, DIGITS, and EXPONENTCHAR are omitted, then the
effect is to print the value using ordinary free-format
exponential-notation output.

If arg is an integer, then it is coerced to be a float and then
printed.  If arg is some non-numeric object, then it is printed
using the format directive ~WIDTHd, thereby printing it in
decimal radix and a minimum field width of WIDTH.

Here is an example of the effects of varying the scale factor:

\(dotimes \(scale 13\)
  \(cl-format t \"~&Scale factor  ~2d: | ~:*~13,6,2,ve|\"
  \(- scale 5\) float-pi))"
  (let ((arg (cl-format-check-and-pop args)))
    (when (integerp arg) (setq arg (float arg)))
    (if (floatp arg)
        (if (not (cl-format-float-regular-p arg))
            (cl-format-float-nan/inf
             arg width atsign overflowchar padchar)

          (clisp-format-float-for-e
           width digits exponent scale overflowchar padchar exponentchar
           atsign arg standard-output))
      (if width
          (clisp-format-padded-string
           width 1 0 padchar t
           (clisp-princ-with-base-to-string arg)
           standard-output)
        (clisp-format-ascii-decimal arg standard-output))))
  args)

(define-cl-format-directive ?g (args atsign colon
                                     (width nil) (digits nil) (exponent nil) (scale 1)
                                     (overflowchar nil)
                                     (padchar ?\s)
                                     (exponentchar nil))
"The next arg is printed as a floating-point number in either
fixed-format or exponential notation as appropriate.

The full form is ~w,d,e,k,overflowchar,padchar,exponentcharG. The
format in which to print arg depends on the magnitude \(absolute
value\) of the arg. Let n be an integer such that \[_24769\]. \(If
arg is zero, let n be 0.\) Let ee equal e+2, or 4 if e is omitted.
Let ww equal w-ee, or nil if w is omitted. If d is omitted, first
let q be the number of digits needed to print arg with no loss of
information and without leading or trailing zeros; then let d
equal \(max q \(min n 7\)\). Let dd equal d-n.

If 0ddd, then arg is printed as if by the format directives

~ww,dd,,overflowchar,padcharF~ee@T

Note that the scale factor k is not passed to the ~g directive.
For all other values of dd, arg is printed as if by the format
directive

~w,d,e,k,overflowchar,padchar,exponentcharE

In either case, an @ modifier is specified to the ~g or ~g
directive if and only if one was specified to the ~g directive.

change_b
X3J13 voted in January 1989 \(FORMAT-PRETTY-PRINT\)   to specify
that format binds *print-escape* to nil during the processing of
the ~g directive.
change_e

old_chan
Examples:

\(defun foo \(x\)
  \(cl-format nil
          \"~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g\"
          x x x\)\)

\(foo 0.0314159\) => \"  3.14e-2|314.2$-04|0.314e-01|  3.14e-2\"
\(foo 0.314159\)  => \"  0.31   |0.314    |0.314    | 0.31    \"
\(foo 3.14159\)   => \"   3.1   | 3.14    | 3.14    |  3.1    \"
\(foo 31.4159\)   => \"   31.   | 31.4    | 31.4    |  31.    \"
\(foo 314.159\)   => \"  3.14e+2| 314.    | 314.    |  3.14e+2\"
\(foo 3141.59\)   => \"  3.14e+3|314.2$+01|0.314e+04|  3.14e+3\"
\(foo 3141.59e0\) => \"  3.14e+3|314.2$+01|0.314e+04|  3.14e+3\"
\(foo 3.14e12\)   => \"*********|314.0$+10|0.314e+13| 3.14e+12\"
\(foo 3.14e120\)  => \"*********|?????????|%%%%%%%%%|3.14e+120\"
\(foo 3.14e1200\) => \"*********|?????????|%%%%%%%%%|3.14e+1200\"

old_chan

change_b
Notice of correction. In the first edition, the example for the
value 3.14e12 contained two typographical errors:

\(foo 3.14e12\)   => \"*********|314.2$+10|0.314e+13| 3.14e+12\"
                                  ^                    ^
                                  should be 0          should be e

These have been corrected above.
change_e"

  (let ((arg (cl-format-check-and-pop args)))
    (if (integerp arg) (setq arg (float arg)))
    (if (floatp arg)
        (if (not (cl-format-float-regular-p arg))
            (cl-format-float-nan/inf
             arg width atsign overflowchar padchar)
          (let ((n (clisp-float-scale-exponent arg)))
            (if (null digits)
                (setq digits
                      (multiple-value-bind (digits digitslength)
                          (clisp-format-float-to-string
                           (abs arg) nil nil nil nil)
                        (max (1- digitslength) 1 (min n 7)))))
            (let* ((ee (if exponent (+ 2 exponent) 4))
                   (dd (- digits n)))
              (if (and (<= 0 dd) (<= dd digits))
                  (progn
                    (clisp-format-float-for-f
                     (if width (- width ee) nil)
                     dd 0
                     overflowchar padchar
                     atsign arg standard-output)
                    (clisp-format-padding ee ?\s standard-output))
                (clisp-format-float-for-e
                 width digits exponent scale overflowchar padchar exponentchar
                 atsign arg standard-output)))))
      (if width
          (clisp-format-padded-string
           width 1 0 padchar t
           (clisp-princ-with-base-to-string arg)
           standard-output)
        (princ arg))))
  args)

(define-cl-format-directive ?$ (args atsign colon
                                     (digits 2) (n 1) (w 0) (padchar ?\s))
  "~$  Dollars floating-point. The next arg is printed as a
floating-point number in fixed-format notation. This format is
particularly convenient for printing a value as dollars and
cents.

The full form is ~d,n,w,padchar$. The parameter d is the number
of digits to print after the decimal point \(default value 2\); n
is the minimum number of digits to print before the decimal point
\(default value 1\); w is the minimum total width of the field to
be printed \(default value 0\).

First padding and the sign are output. If the arg is negative,
then a minus sign is printed; if the arg is not negative, then a
plus sign is printed if and only if the @ modifier was specified.
If the : modifier is used, the sign appears before any padding,
and otherwise after the padding. If w is specified and the number
of other characters to be output is less than w, then copies of
padchar \(which defaults to a space\) are output to make the total
field width equal w. Then n digits are printed for the integer
part of arg, with leading zeros if necessary; then a decimal
point; then d digits of fraction, properly rounded.

If the magnitude of arg is so large that more than m digits would
have to be printed, where m is the larger of w and 100, then an
implementation is free, at its discretion, to print the number
using exponential notation instead, as if by the directive ~w,q
,,,,padchar_e, where w and padchar are present or omitted
according to whether they were present or omitted in the ~$
directive, and where q=d+n-1, where d and n are the \(possibly
default\) values given to the ~$ directive.

If arg is a rational number, then it is coerced to be a
single-float and then printed. \(Alternatively, an implementation
is permitted to process a rational number by any other method
that has essentially the same behavior but avoids such hazards as
loss of precision or overflow because of the coercion.\)

If arg is a complex number or some non-numeric object, then it is
printed using the format directive ~wd, thereby printing it in
decimal radix and a minimum field width of w. \(If it is desired
to print each of the real part and imaginary part of a complex
number using a ~$ directive, then this must be done explicitly
with two ~$ directives and code to extract the two parts of the
complex number.\)

change_b
X3J13 voted in January 1989 \(FORMAT-PRETTY-PRINT\)   to specify
that format binds *print-escape* to nil during the processing of
the ~$ directive.
change_e"

  (let ((arg (cl-format-check-and-pop args)))
    (when (integerp arg) (setq arg (float arg)))
    (if (floatp arg)
        (if (not (cl-format-float-regular-p arg))
            (cl-format-float-nan/inf arg w atsign nil padchar)
          (multiple-value-bind (num numlength
                                       leadingpoint trailingpoint leadings)
              (clisp-format-float-to-string arg nil digits 0 nil)
            (let* ((lefts (max leadings n))
                   (totalwidth (+ (if (or atsign (minusp arg)) 1 0)
                                  lefts 1 digits))
                   (padcount (max (- w totalwidth) 0)))
              (if (not colon)
                  (clisp-format-padding
                   padcount padchar standard-output))
              (if (minusp arg)
                  (write-char ?- standard-output)
                (if atsign (write-char ?+ standard-output)))
              (if colon
                  (clisp-format-padding padcount padchar standard-output))
              (clisp-format-padding (- lefts leadings) ?0 standard-output)
              (princ num standard-output))))
      (if digits
          (clisp-format-padded-string
           digits 1 0 padchar t
           (clisp-princ-with-base-to-string arg)
           standard-output)
        (clisp-format-ascii-decimal arg standard-output))))
  args)


;; 22.3.4.1 Tilde A: Aesthetic

(define-cl-format-directive ?a (args at colon
                                     (mincol 0)
                                     (colinc 1)
                                     (minpad 0)
                                     (padchar ?\s))

"An ARG, any Lisp object, is printed without escape characters \(as by
princ\). In particular, if ARG is a string, its characters will be
output verbatim. If ARG is nil, it will be printed as nil; the colon
modifier \(~:a\) will cause an ARG of nil to be printed as \(\), but if
ARG is a composite structure, such as a list or vector, any contained
occurrences of nil will still be printed as nil.

~mincolA inserts spaces on the right, if necessary, to make the width
at least mincol columns. The @ modifier causes the spaces to be
inserted on the left rather than the right.

~mincol,colinc,minpad,padcharA is the full form of ~a, which allows
elaborate control of the padding. The string is padded on the right
\(or on the left if the @ modifier is used\) with at least minpad copies
of padchar; padding characters are then inserted colinc characters at
a time until the total width is at least mincol. The defaults are 0
for mincol and minpad, 1 for colinc, and the space character for
padchar."

  (let ((arg (cl-format-check-and-pop args)))
    (when (and colon (null arg)) (setq arg "()"))
    (if (and (zerop mincol) (zerop minpad))
        (princ arg standard-output)
      (clisp-format-padded-string
       mincol colinc minpad padchar
       at ;; =: padleftflag
       (clisp-princ-with-base-to-string arg)
       standard-output))
    args))

(define-cl-format-directive ?s (args at colon
                                     (mincol 0)
                                     (colinc 1)
                                     (minpad 0)
                                     (padchar ?\s))
"This is just like ~a, but ARG is printed with escape characters (as
by prin1 rather than princ). The output is therefore suitable for
input to read. ~s accepts all the arguments and modifiers that ~a
does."

  (let ((arg (cl-format-check-and-pop args)))
    (clisp-format-padded-string
     mincol colinc minpad padchar at
     (if (and colon (null arg))
         "()"
       (let  ((print-length nil)
              (print-circle t)
              (print-level nil)
              (print-escape-newlines t)
              (print-quoted t))
         (prin1-to-string arg)))
     standard-output))
  args)

(define-cl-format-directive ?w (args atsign colon
                                     (mincol 0)
                                     (colinc 1)
                                     (minpad 0)
                                     (padchar ?\s))
"An ARG, any Lisp object, is printed obeying every printer control
variable. ~w does not accept parameters. If given the colon modifier,
~w prints with `pp'. If given the atsign modifier, ~w binds
`print-level' and `print-length' to nil."
  (let* ((arg (cl-format-check-and-pop args))
         (print-length (if (not atsign) print-length))
         (print-level (if (not atsign) print-level)))
    (if (and (zerop mincol) (zerop minpad))
        (if colon
            (pp arg)
          (prin1 arg))
      (clisp-format-padded-string
       mincol colinc minpad padchar
       atsign ;; =: padleftflag
       (if (not colon)
           (prin1-to-string arg)
         (pp-to-string arg))
       standard-output)))
  args)

;;22.3.5 FORMAT Pretty Printer Operations

(define-cl-format-directive ?_ (args at colon)
  "Not implemented."
  (cl-format-eval-error "Format directive ~_ not implemented"))

(define-cl-format-directive (?< ?>) (args at colon
                                          (mincol 0)
                                          (colinc 1)
                                          (minpad 0)
                                          (padchar ?\s)
                                          &contained contained
                                          &separator separator)
  (let* ((pos (current-column))
         (tempstream (with-current-buffer
                         (generate-new-buffer " *cl-format-justify*")
                       (current-buffer)))
         (check-on-line-overflow nil)
         supplementary-need line-length
         (first-part-p t) pieces done)
    (unwind-protect
        (progn
          (while (and contained (not done))
            (let* ((standard-output tempstream)
                   (part (pop contained))
                   (up-and-out t))
              (setq args (catch 'cl-format-up-and-out
                           (prog1 (funcall part args)
                             (setq up-and-out nil))))
              (when (and first-part-p
                         separator
                         (cdr (assq :colon-flag (car separator))))
                (setq check-on-line-overflow t)
                ;; Fixme: Parameter need expansion.
                (let ((args-parms
                       (cl-format-expand-parameter
                        args (cdr (assq :parameter (car separator))))))
                  (setq args (car args-parms)
                        supplementary-need (car (cdr args-parms))
                        line-length (cadr (cdr args-parms)))))
              (setq first-part-p nil)
              (if up-and-out
                  (setq done t)
                (push (with-current-buffer tempstream
                        (prog1 (buffer-string)
                          (erase-buffer))) pieces))))
          (setq pieces (nreverse pieces))
          (clisp-format-do-format-justification
           standard-output colon at mincol colinc minpad padchar pos
           check-on-line-overflow
           (if check-on-line-overflow (car pieces))
           supplementary-need line-length
           (if check-on-line-overflow
               (cdr pieces)
             pieces))
          args)
      (and (buffer-live-p tempstream)
           (kill-buffer tempstream)))))

(define-cl-format-directive ?i (args at colon)
  "Not implemented."
  (error "Format directive ~i not implemented"))

(define-cl-format-directive (?/ ?/) (args at colon
                                          &contained contained)
  "Not implemented."
  (error "Format directive ~// not implemented"))


;; 22.3.6 FORMAT Layout Control

(define-cl-format-directive ?t (args atsign colon
                                     (colnum 1) (colinc 1))
  (if (null colnum) (setq colnum 1))
  (if (null colinc) (setq colinc 1))
  (let* ((new-colnum (+ (max colnum 0)
                        (if (and colon
                                 (boundp '*prin-indentation*))
                            *prin-indentation*
                          0)))
         (new-colinc (max colinc 1))    ; >0
         (pos (current-column) ;; (sys::line-position standard-output)
              ))               ; actual position, fixnum>=0 or NIL
    (if atsign
        (clisp-format-padding
         (if pos
             (+ new-colnum (mod (- (+ pos new-colnum)) new-colinc))
           new-colnum)
         ?\s standard-output)
      (if pos
          (if (< pos new-colnum)
              (clisp-format-padding
               (- new-colnum pos) ?\s standard-output)
            (unless (zerop colinc)
              (clisp-format-padding
               (+ colinc (mod (- new-colnum pos) (- colinc)))
               ?\s standard-output)))
        (clisp-format-padding 2 ?\s standard-output))))
  args)


;; 22.3.7 FORMAT Control-Flow Operations

(define-cl-format-directive ?* (args at colon n)
  "The next arg is ignored. ~n* ignores the next n arguments.

~:* ``ignores backwards''; that is, it backs up in the list of
arguments so that the argument last processed will be processed
again. ~n:* backs up n arguments.

When within a ~{ construct, the ignoring (in either direction) is
relative to the list of arguments being processed by the
iteration.

~n@* is an ``absolute goto'' rather than a ``relative goto'': it goes
to the nth arg, where 0 means the first one; n defaults to 0, so
~@* goes back to the first arg. Directives after a ~n@* will take
arguments in sequence beginning with the one gone to.  When within a
~{ construct, the ``goto'' is relative to the list of arguments being
processed by the iteration."

  (when (and at colon)
    (cl-format-eval-error
     "The ~* directive cannot take both modifiers"))

  (cond
   (at
    (nthcdr (or n 0) cl-format-arguments))
   (colon
    (nthcdr (max (min (- (length cl-format-arguments)
                         (length args)
                         (or n 1))
                      (length cl-format-arguments))
                 0)
            cl-format-arguments))
   (t
    (nthcdr (or n 1) args))))

(define-cl-format-directive (?\[ ?\]) (args at colon n
                                            &contained contained
                                            &separator separator)
  "This is a set of control strings, called clauses, one of which
is chosen and used. The clauses are separated by ~; and the
construct is terminated by ~]. For example,

\"~[Siamese~;Manx~;Persian~] Cat\"

The argth clause is selected, where the first clause is number 0.
If a prefix parameter is given \(as ~n[\), then the parameter is
used instead of an argument. \(This is useful only if the
parameter is specified by #, to dispatch on the number of
arguments remaining to be processed.\) If arg is out of range,
then no clause is selected \(and no error is signaled\). After the
selected alternative has been processed, the control string
continues after the ~].

~[str0~;str1~;...~;strn~:;default~] has a default case. If the
last ~; used to separate clauses is ~:; instead, then the last
clause is an ``else'' clause that is performed if no other clause
is selected. For example:

\"~[Siamese~;Manx~;Persian~:;Alley~] Cat\"

~:[false~;true~] selects the false control string if arg is nil,
and selects the true control string otherwise.

~@[true~] tests the argument. If it is not nil, then the argument
is not used up by the ~@[ command but remains as the next one to
be processed, and the one clause true is processed. If the arg is
nil, then the argument is used up, and the clause is not
processed. The clause therefore should normally use exactly one
argument, and may expect it to be non-nil. For example:

\(setq print-level nil print-length 5\)
\(cl-format nil \"~@[ print level = ~d~]~@[ print length = ~d~]\"
         print-level print-length\)
     =>  \" print length = 5\"

The combination of ~[ and # is useful, for example, for dealing
with English conventions for printing lists:

\(setq foo \"Items:~#[ none~; ~s~; ~s and ~s~
             ~:;~@{~#[~; and~] ~s~^,~}~].\"\)
\(cl-format nil foo\)
     =>  \"Items: none.\"
\(cl-format nil foo 'foo\)
     =>  \"Items: foo.\"
\(cl-format nil foo 'foo 'bar\)
     =>  \"Items: foo and bar.\"
\(cl-format nil foo 'foo 'bar 'baz\)
     =>  \"Items: foo, bar, and baz.\"
\(cl-format nil foo 'foo 'bar 'baz 'quux\)
     =>  \"Items: foo, bar, baz, and quux.\""

  (when (and at colon)
    (cl-format-eval-error
     "The ~[ directive cannot take both modifiers"))

  (cond
   (colon
    (setq args
          (if (cl-format-check-and-pop args)
              (if (cdr contained)
                  (funcall
                   (cadr contained) args))
            (if contained
                (funcall
                 (car contained)
                 args)
              args))))
   (at
    (let ((arg (cl-format-check-and-pop args)))
      (when arg
        (when separator
          (cl-format-eval-error
           "The ~; directive is not allowed at this point" arg))
        (setq args (funcall (car contained)
                            (cons arg args))))))
   (t
    (or n (setq n (cl-format-check-and-pop args)))
    (unless (integerp n)
      (cl-format-eval-error "The ~[ parameter must be an integer" n))
    (let ((default-p (and separator
                          (cdr (assq :colon-flag
                                     (car (last separator)))))))
      (if (and (>= n 0)
               (< n (- (length contained)
                       (if default-p 1 0))))
          (setq args (funcall (nth n contained) args))
        (if default-p
            (setq args (funcall
                        (car (last contained))
                        args)))))))
  args)

(define-cl-format-directive (?{ ?}) (args at colon (max nil)
                                          &contained contained
                                          &end-separator end)
  "This is an iteration construct. The argument should be a list,
a keymap, a hash-table, a vector or an alist, which is used as a
set of arguments as if for a recursive call to format. The string
str is used repeatedly as the control string. Each iteration can
absorb as many elements of the container as it likes as
arguments; if str uses up two arguments by itself, then two
elements of the container will get used up each time around the
loop. If before any iteration step the container is empty, then
the iteration is terminated. Also, if a prefix parameter n is
given, then there will be at most n repetitions of processing of
str. Finally, the ~^ directive can be used to terminate the
iteration prematurely.

Here are some simple examples:

\(cl-format nil
        \"The winners are:~{ ~s~}.\"
        '\(fred harry jill\)\)
     => \"The winners are: fred harry jill.\"

\(cl-format nil \"Pairs:~{ <~s,~s>~}.\" '\(a 1 b 2 c 3\)\)
     => \"Pairs: <a,1> <b,2> <c,3>.\"

~:{str~} is similar, but the argument should be a list of
sublists. At each repetition step, one sublist is used as the set
of arguments for processing str; on the next repetition, a new
sublist is used, whether or not all of the last sublist had been
processed. Example:

\(cl-format nil \"Pairs:~:{ <~s,~s>~}.\"
            '\(\(a 1\) \(b 2\) \(c 3\)\)\)
     => \"Pairs: <a,1> <b,2> <c,3>.\"

~@{str~} is similar to ~{str~}, but instead of using one argument
that is a list, all the remaining arguments are used as the list
of arguments for the iteration. Example:

\(cl-format nil \"Pairs:~@{ <~s,~s>~}.\"
            'a 1 'b 2 'c 3\)
     => \"Pairs: <a,1> <b,2> <c,3>.\"

If the iteration is terminated before all the remaining arguments
are consumed, then any arguments not processed by the iteration
remain to be processed by any directives following the iteration
construct.

~:@{str~} combines the features of ~:{str~} and ~@{str~}. All the
remaining arguments are used, and each one must be a list. On
each iteration, the next argument is used as a list of arguments
to str. Example:

\(cl-format nil \"Pairs:~:@{ <~s,~s>~}.\"
            '\(a 1\) '\(b 2\) '\(c 3\)\)
     => \"Pairs: <a,1> <b,2> <c,3>.\"

Terminating the repetition construct with ~:} instead of ~}
forces str to be processed at least once, even if the initial
list of arguments is null \(however, it will not override an
explicit prefix parameter of zero\).

If str is empty, then an argument is used as str. It must be a
string and precede any arguments processed by the iteration. As
an example, the following are equivalent:

\(apply #'format stream string arguments\)
\(cl-format stream \"~1{~:}\" string arguments\)

This will use string as a formatting string. The ~1{ says it will
be processed at most once, and the ~:} says it will be processed
at least once. Therefore it is processed exactly once, using
arguments as the arguments. This case may be handled more clearly
by the ~? directive, but this general feature of ~{ is more
powerful than ~?."
    (let ((force-once
           (cdr (assq :colon-flag end))))

    (when (eq contained 'identity)
      (setq contained
            (cl-format-parse-and-compile
             (cl-format-check-and-pop args))))

    (if (not at)
        (cl-format-iterate
         (cl-format-check-and-pop args)
         contained max force-once colon)
      (setq args (cl-format-iterate
                  args
                  contained max force-once colon))))
  args)

(defun cl-format-iterate (container fmt &optional
                                    max force-once nested)

  (unless (and (listp container)
               (not (keymapp container)))
    (cond
     ((and (vectorp container)
           (not (eq container obarray)))
      (setq container (mapcar 'identity container)))
     ((eq obarray container)
      (let (result)
        (mapatoms (lambda (sym) (push sym result)))
        (setq container result)))
     ((or (hash-table-p container)
          (keymapp container))
      (let ((accessor (if (keymapp container)
                          'map-keymap
                        'maphash))
            result)
        (funcall accessor
                 (lambda (k v)
                   (push (list k v) result))
                 container)
        (setq container result)))
     (t
      (cl-format-eval-error
       "Argument is neither a list, vector, hash-table nor keymap"
       container))))
  ;; expand dotted pairs
  (when (and (consp container)
             (not (listp (cdr container))))
    (setq container (list (car container) (cdr container))))

  (let ((count 0)
        (max (or max (length container))))
    (while (or (and container (< count max))
               force-once)
      (let ((cl-format-arguments container))
        (if nested
            (cl-format-iterate (pop container) fmt 1)
          (setq container
                (catch 'cl-format-up-and-out
                  (funcall fmt container)))))
      (incf count)
      (setq force-once nil)))
  container)

(define-cl-format-directive ?? (args at)
  "The next arg must be a string, and the one after it a list;
both are consumed by the ~? directive. The string is processed as
a format control string, with the elements of the list as the
arguments. Once the recursive processing of the control string
has been finished, then processing of the control string
containing the ~? directive is resumed. Example:

\(cl-format nil \"~? ~d\" \"<~a ~d>\" '\(\"Foo\" 5\) 7\)
      => \"<Foo 5> 7\"
\(cl-format nil \"~? ~d\" \"<~a ~d>\" '\(\"Foo\" 5 14\) 7\)
      => \"<Foo 5> 7\"

Note that in the second example three arguments are supplied to
the control string \"<~a ~d>\", but only two are processed and the
third is therefore ignored.

With the @ modifier, only one arg is directly consumed. The arg
must be a string; it is processed as part of the control string
as if it had appeared in place of the ~@? construct, and any
directives in the recursively processed control string may
consume arguments of the control string containing the ~@?
directive. Example:

\(cl-format nil \"~@? ~d\" \"<~a ~d>\" \"Foo\" 5 7\)
      => \"<Foo 5> 7\"
\(cl-format nil \"~@? ~d\" \"<~a ~d>\" \"Foo\" 5 14 7\)
      => \"<Foo 5> 14\"

Here is a rather sophisticated example. The format function
itself, as implemented at one time in Lisp Machine Lisp, used a
routine internal to the format package called format-error to
signal error messages; format-error in turn used error, which
used format recursively. Now format-error took a string and
arguments, just like format, but also printed the control string
to format \(which at this point was available in the global
variable *ctl-string*\) and a little arrow showing where in the
processing of the control string the error occurred. The variable
*ctl-index* pointed one character after the place of the error.

\(defun format-error \(string &rest args\)     ;Example
  \(error nil \"~?~%~v@t[_24769]~%~3@t \"~a \"~%\"
         string args \(+ *ctl-index* 3\) *ctl-string*\)\)

\(The character set used in the Lisp Machine Lisp implementation
contains a down-arrow character [_24769], which is not a standard
Common Lisp character.\) This first processed the given string and
arguments using ~?, then output a newline, tabbed a variable
amount for printing the down-arrow, and printed the control
string between double quotes \(note the use of \" to include double
quotes within the control string\). The effect was something like
this:

\(cl-format t \"The item is a ~[Foo~;Bar~;Loser~].\" 'quux\)
FIXME: Insert actual error message.
>>ERROR: The argument to the FORMAT \"~[\" command
         must be a number.
                   [_24769]
   \"The item is a ~[Foo~;Bar~;Loser~].\""

  (let ((fmt (cl-format-check-and-pop args)))
    (cond
     (at
      (setq args
            (funcall
             (cl-format-parse-and-compile fmt)
             args)))
     (t
      (let ((contained-args (cl-format-check-and-pop args)))
        (cl-format-check-type contained-args 'listp)
        (funcall (cl-format-parse-and-compile fmt)
                 contained-args)))))
  args)


;; 22.3.8 FORMAT Miscellaneous Operations

;;(define-cl-format-directive (?\( ?\)) nil)
(define-cl-format-directive (?|) (args at-flag
                                       colon-flag
                                       &contained contained)
  "Case conversion. The contained control string str is processed,
and what it produces is subject to case conversion: ~| converts
every uppercase character to the corresponding lowercase
character; ~:| capitalizes all words, as if by
string-capitalize; ~@| capitalizes just the first word and
forces the rest to lowercase; ~:@| converts every lowercase
character to the corresponding uppercase character. In this
example, ~@| is used to cause the first word produced by ~@r to
be capitalized:

\(cl-format nil \"~@r ~|\(~@r~\)\" 14 14\)
      => \"XIV xiv\"
\(defun f \(n\)
  \(cl-format nil \"~@|\(~r~\) error~:p detected.\" n\)\)
\(f 0\) => \"Zero errors detected.\"
\(f 1\) => \"One error detected.\"
\(f 23\) => \"Twenty-three errors detected.\""

  (let ((beg (point))
        (args (catch 'cl-format-up-and-out
                (funcall contained args)))
        (end (point)))
    (save-excursion
      (cond
       ((and at-flag colon-flag)
        (upcase-region beg end))
       (at-flag
        (goto-char beg)
        (capitalize-word 1)
        (downcase-region (point) end))
       (colon-flag
        (capitalize-region beg end))
       (t
        (downcase-region beg end))))
    args))

(define-cl-format-directive ?p (args at colon)
"If ARG is not eql to the integer 1, a lowercase s is printed; if
ARG is eql to 1, nothing is printed. (Notice that if ARG is a
floating-point 1.0, the s is printed.) ~:p does the same thing,
after doing a ~:* to back up one argument; that is, it prints a
lowercase s if the last argument was not 1. This is useful after
printing a number using ~d. ~@p prints y if the argument is 1, or
ies if it is not. ~:@p does the same thing, but backs up first."
  (let ((arg
         (if colon
             (nth (- (length cl-format-arguments)
                     (length args)
                     1)
                  cl-format-arguments)
           (cl-format-check-and-pop args))))
    (let ((singular (eql arg 1)))
      (if at
          (princ (if singular "y" "ies") standard-output)
        (unless singular (write-char ?s standard-output)))))
  args)


;; 22.3.9 FORMAT Miscellaneous Pseudo-Operations

;;(define-cl-format-directive ?\; nil)
(define-cl-format-directive ?^ (args)
  "This is an escape construct. If there are no more arguments
remaining to be processed, then the immediately enclosing ~{ or
~< construct is terminated. If there is no such enclosing
construct, then the entire formatting operation is terminated. In
the ~< case, the formatting is performed, but no more segments
are processed before doing the justification. The ~ ^ should
appear only at the beginning of a ~< clause, because it aborts
the entire clause it appears in \(as well as all following
clauses\). ~^ may appear anywhere in a ~{ construct.

\(setq donestr \"Done.~^  ~d warning~:p.~^  ~d error~:p.\"\)
\(cl-format nil donestr\) => \"Done.\"
\(cl-format nil donestr 3\) => \"Done.  3 warnings.\"
\(cl-format nil donestr 1 5\) => \"Done.  1 warning.  5 errors.\"

If a prefix parameter is given, then termination occurs if the
parameter is zero. \(Hence ~^ is equivalent to ~#^.\) If two
parameters are given, termination occurs if they are equal. If
three parameters are given, termination occurs if the first is
less than or equal to the second and the second is less than or
equal to the third. Of course, this is useless if all the prefix
parameters are constants; at least one of them should be a # or a
V parameter.

If ~^ is used within a ~:{ construct, then it merely terminates
the current iteration step \(because in the standard case it tests
for remaining arguments of the current step only\); the next
iteration step commences immediately. To terminate the entire
iteration process, use ~:^.

change_b
X3J13 voted in March 1988 \(FORMAT-COLON-UPARROW-SCOPE\)   to
clarify the behavior of ~:^ as follows. It may be used only if
the command it would terminate is ~:{ or ~:@{. The entire
iteration process is terminated if and only if the sublist that
is supplying the arguments for the current iteration step is the
last sublist \(in the case of terminating a ~:{ command\) or the
last argument to that call to format \(in the case of terminating
a ~:@{ command\). Note furthermore that while ~^ is equivalent to
~#^ in all circumstances, ~:^ is not equivalent to ~:#^ because
the latter terminates the entire iteration if and only if no
arguments remain for the current iteration step \(as opposed to no
arguments remaining for the entire iteration process\).

Here are some examples of the differences in the behaviors of ~^,
~:^, and ~:#^.

\(cl-format nil
        \"~:{/~s~^ ...~}\"
        '\(\(hot dog\) \(hamburger\) \(ice cream\) \(french fries\)\)\)
 => \"/HOT .../HAMBURGER/ICE .../FRENCH ...\"

For each sublist, `` ...'' appears after the first word unless
there are no additional words.

\(cl-format nil
        \"~:{/~s~:^ ...~}\"
        '\(\(hot dog\) \(hamburger\) \(ice cream\) \(french fries\)\)\)
 => \"/HOT .../HAMBURGER .../ICE .../FRENCH\"

For each sublist, `` ...'' always appears after the first word,
unless it is the last sublist, in which case the entire iteration
is terminated.

\(cl-format nil
        \"~:{/~s~:#^ ...~}\"
        '\(\(hot dog\) \(hamburger\) \(ice cream\) \(french fries\)\)\)
 => \"/HOT .../HAMBURGER\"

For each sublist, `` ...'' appears after the first word, but if
the sublist has only one word then the entire iteration is
terminated.
change_e

If ~^ appears within a control string being processed under the
control of a ~? directive, but not within any ~{ or ~< construct
within that string, then the string being processed will be
terminated, thereby ending processing of the ~? directive.
Processing then continues within the string containing the ~?
directive at the point following that directive.

If ~^ appears within a ~\[ or ~\( construct, then all the commands
up to the ~^ are properly selected or case-converted, the ~\[ or ~
\( processing is terminated, and the outward search continues for
a ~{ or ~< construct to be terminated. For example:

\(setq tellstr \"~@\(~@\[~r~\]~^ ~a.~\)\"\)
\(cl-format nil tellstr 23\) => \"Twenty-three.\"
\(cl-format nil tellstr nil \"losers\"\) => \"Losers.\"
\(cl-format nil tellstr 23 \"losers\"\) => \"Twenty-three losers.\"

Here are some examples of the use of ~^ within a ~< construct.

\(cl-format nil \"~15<~s~;~^~s~;~^~s~>\" 'foo\)
        =>  \"            FOO\"
\(cl-format nil \"~15<~s~;~^~s~;~^~s~>\" 'foo 'bar\)
        =>  \"FOO         BAR\"
\(cl-format nil \"~15<~s~;~^~s~;~^~s~>\" 'foo 'bar 'baz\)
        =>  \"FOO   BAR   BAZ\""

  (or args
      (throw 'cl-format-up-and-out nil)))

;;(define-cl-format-directive ?\n nil)


(defun cl-format-float-regular-p (float)
  "Check that FLOAT is a `float' and neither NaN nor INF."
  (require 'cl)
  (cl-float-limits)
  (and (= float float)
       (<= float most-positive-float)
       (>= float most-negative-float)))

(provide 'cl-format-builtins)

;;; cl-format-builtins.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
