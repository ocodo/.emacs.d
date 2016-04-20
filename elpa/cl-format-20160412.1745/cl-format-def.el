;;; cl-format-def.el --- CL format. Code needed for defining directives.

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

;;; Code:

(require 'cl)

(defstruct (cl-format-directive
            (:conc-name cl-format-directive/))
  char-beg
  char-end
  parameter
  format-fn
  contained-p
  separator-p
  end-separator-p
  at-flag-p
  colon-flag-p
  documentation)

(defstruct (cl-format-base
            (:conc-name cl-format-base/))
  at-flag
  colon-flag
  parameter)

(defstruct (cl-format-part
            (:conc-name cl-format-part/)
            (:include cl-format-base))
  directive
  contained
  separator
  end-separator)

(defstruct (cl-format-clause-separator
            (:conc-name cl-format-clause-separator/)
            (:include cl-format-base)))

(defstruct (cl-format-clause-end-separator
            (:conc-name cl-format-clause-end-separator/)
            (:include cl-format-clause-separator)))


(defvar cl-format-directives nil
  "Alist of implemented directives.

Has the form \(\(char . clx\) ...\), where 'char' is the starting
character of the directive and 'clx' is the corresponding struct.")

(defvar cl-format-reserved-chars
  '(?\( ?\) ;; generalized blocks
        ?v ;; value parameter
        ?: ?@ ;; directive flags
        ?\#   ;; args-left parameter
        ?\,   ;; parameter separator
        ?\;   ;; block separator
        ?\n   ;; ignored newline
        ;; parameter
        ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 
        )
  "Reserved characters which may not be used as a directive.")

(defun cl-define-format-directive-parse-lambda-list (list)
  "Parse lambda-list LIST for `define-cl-format-directive'."
  (flet ((check-arg (arg)
           (and (not (keywordp* arg))
                (or (symbolp arg)
                    (and (consp arg)
                         (= (length arg) 2)
                         (symbolp (car arg))))))
         (keywordp* (symbol)
           (and (symbolp symbol)
                (eq ?& (aref (symbol-name symbol) 0)))))
    (let* ((order (copy-sequence '(&args &at-flag &colon-flag &parameter
                                         &contained &separator
                                         &end-separator &stash)))
           (keywords-with-def '(&parameter))
           parameter
           (keyword (car order))
           args)
      (while list
        (let* ((a (pop list))
               (kw-p (keywordp* a)))
          (when kw-p
            (unless (memq a order)
              (error "Unknown keyword : %s" a))
            (or (and list
                     (not (keywordp* (car list))))
                (error "Missing an argument after %s" a))
            (setq keyword a
                  a (pop list)))
          (or (check-arg a)
              (error "Invalid argument %s" a))
          (or keyword
              (error "Missing a keyword argument %s" a))
          (if (eq keyword '&parameter)
              (if (assq keyword args)
                  (setcar args
                          (cons '&parameter
                                (append (cdar args) (list a))))
                (push (cons keyword (list a)) args))
            (if (and (consp a)
                     (not (memq keyword keywords-with-def)))
                (error "Only %s may have default value, not %s: %s"
                       keywords-with-def keyword a)
              (push (cons keyword a) args)))
          (unless (eq keyword '&parameter)
            (setq keyword (cadr (memq keyword order))))))
      (nreverse args))))

(defun cl-format-directive-generate-documentation (char-beg char-end at colon
                                                            parameter separator-p)
  "Generate a one-line documentation string for a format directive."
  (concat
   "~"
   (mapconcat
    (lambda (p)
      (let* ((name (if (consp p)
                       (car p)
                     p))
             (value (and (consp p)
                         (cadr p)))
             (char-p (string-match
                      "char"
                      (symbol-name name))))
        (when (and char-p value)
          (setq value (char-to-string value)))
        (if value
            (format "%s=%s%s" 
                    name (if char-p "'" "") value)
          (format "%s" name))))
    parameter ",")
   (if at (string ?@))
   (if colon (string ?:))
   (string
    char-beg)
   (and char-end
        (eq char-end t)
        (string ?\())
   (and char-end
        "...")
   (and (and char-end separator-p)
        "~;...")
   (if (eq char-end t)
       "~)"
     (if char-end
         (string ?~ char-end)))
   "\n"))

(defmacro define-cl-format-directive (char args &optional doc &rest body)
  "Define the `cl-format' directive ~CHAR.

This macro defines a directive, which may be used in a format
string.  CHAR is the character to use, ARGS is a special
lambda-list, DOC is the documentation and BODY contains the
actual definition.

CHAR may have 3 different formats, depending on what kind of
directive should be defined.

The most basic form is a single character.  This defines a
directive like ~c, ~f and ~e , i.e. it has no end character and
therefore can not wrap around other directives or literal text,
like e.g ~{~} \(the CL iteration directive\).

If CHAR is a list \(CHAR-BEG CHAR-END\) containing 2 characters,
a directive like ~{~} is defined, i.e. it may wrap over a
substring of the format string.

Finally if CHAR is a list \(CHAR\) with only a single character,
the directive will be defined using the generalized block
construct ~CHAR\(...~\).  That means it may contain other format
strings \(the ... part\), but the end character is fixed.  \(Note:
The case conversion directive was sacrificed, in order to
implement this form.  In `cl-format', you have to use ~|\(...~\)
.  Note2: The ~| was, again, sacrificed for this.\)

After the character follows a special lambda-list.  This list
defines the arguments of the directive.  CL directives may
actually have 5 different kinds of arguments: Dynamic arguments
\(provided at runtime\), parameter, the two flags `@' and `:',
potentially a contained format string and separator
sub-directives.  In order to differentiate between them and make
them available to the BODY, special markers are used \(like
&optional in ordinary Lisp functions.\) :

&args : The next argument in the list is treated as the dynamic
argument.  It'll hold the list of at runtime provided arguments
to the `cl-format' function.  This is the only mandatory
argument.

&at-flag : The next argument contains the value of the @-flag.

&colon-flag : The next argument will be bound to the value of
the :-flag.

&parameter : All arguments following this marker, up until the
next one, are treated as parameters.

&contained : The next argument holds the wrapped format string.
The occurence of this marker must be consistent with the CHAR
form.

&separator : The next argument holds a list of ~; separators
including their own flags and parameter.  Again this must be
consistent with the CHAR form and the occurence of the &contained
marker.

&end-separator : The next argument will be bound to the end
separator.  \(E.g.  the ~@:\] in a ~\[...~@:\].\)

&stash : Finally the marks an argument, which will be bound to a
fresh symbol.  The property list of this symbol may be used \(with
`get' and `put'\), if the directive wants to implement some kind
of state.  The symbol will be the same for all calls to the
directive during a single call to `cl-format'.

Besides declaring the argument type of a symbol in the
lambda-list, these marker also declare certain properties of the
directive.  For example, if the directive does not have an @-flag
argument, the parser will give an error in case one is used in
the format string.  Following are all properties declared by the
different markers.

&at-flag and &colon-flag : The directive accepts the @-flag
respectivley :-flag, otherwise it won't.

&parameter : The directive accepts 0-N parameter, where N is the
number of arguments up to the next marker \(or the end of the
lambda-list\).

&separator : The directive accepts ~; as a subdirective.

The other markers \(&args, &contained, and &end-separator\) don't
declare anything.


If this is all confusing, here is an example of how the ~%
directive may be defined.

\(define-cl-format-directive ?& \(&args args &parameter \(n 1\)\)
  \"Print N newline characters.  N defaults to 1.
  \(dotimes \(i n\)
    \(terpri\)\)\)

This declares ~% to accept no flags and a maximum of one
parameter.  This example also introduces the ability of parameter
to have default values.  This feature is exclusive to parameter
arguments.

There is an implicit order defined on the argument types,
respectivley markers, which is as follows.

\(&args &at-flag &colon-flag &parameter
       &contained &separator &end-separator
       &stash\)

This order is followed, if the lambda-list does not contain
markers \(up to a certain position\). For example, the list

\(args at-flag colon-flag parameter1 parameter2\)

does not need any markers, because it follows the above implict
order.  

\(args at-flag &parameter parameter1\)

On the other hand, this one must include the &parameter marker,
because otherwise parameter1 would be treated as the :-flag
argument.

The macro knows, that &parameter may have an indefinite number of
arguments and treats it like a &rest in `defun'.  All other
argument types are single valued.


The different argument types have different value domains, as
follows.

&args is the list of, at runtime, provided arguments.

&colon-flag and &at-flag are either t or nil.

&parameter are either integer, which contain the domain of
characters as a subset, or have the default value. Note, that the
dynamic parameter \(# and *\) are already expanded.

&contained : If the directive accepts the ~; subdirective
\(i.e. it contains a &separator argument\) this is a list of
functions, otherwise a single function.  This functions implement
the contained \(compiled\) format string.  In order to expand it
\(make it print what it wants to\), just funcall it (or them)
with the current value of &args as argument and store it's return
value as the new argument list \(the subformat may have used up a
couple of them\).  The value is the symbol `identity' if, and
only if, the contained format string is empty.

&separator, &end-separator : These are always alists containing
exactly 3 keys, namely :at-flag, :colon-flag and :parameter. The
value of these keys represent the corresponding values of the ~;
or end separator subdirectives.  The parameter value is
unexpanded.  If this value is used, it should be passed to
`cl-format-expand-parameter'.

Finally the value of the &stash argument is always a symbol.


The directive must obey certain conventions.  First and foremost,
it must always return the current value of the &args argument.
Next it must always print it's output to `standard-output'.
Escpecially `insert' may fail if `standard-output' is `t` or a
function.  If the directive wants to implement buffer related
functionality \(for example by inspecting the value of
`current-column' or using `indent-according-to-mode'\), it should
check beforehand if `standard-output' is really buffer.  The
`cl-format' function always sets up a buffer context, using a
`with-temp-buffer' if `standard-output' is not a buffer itself.

Here is a complete example of a directive, implementing a fill
operation.  It uses the generalized ~CHAR(...~) form, an
@-flag (but no :-flag) and a single parameter, the `fill-column'.


\(define-cl-format-directive \(?F\) \(args at-flag
                                     &parameter \(column 70\)
                                     &contained region\)
  \"Fill the region using COLUMN as the `fill-column'.

Giving an @-flag means justify as well.\"
  \(let \(\(in-buffer-p \(buffer-live-p standard-output\)\)
        \(pos \(point\)\)\)
    ;; Region may have used some of the arguments, so we have to store
    ;; the new value of it.
    \(setq args \(funcall region args\)\)
    ;; Now check if filling would actually be meaningful.
    \(when in-buffer-p
      ;; When the at-flag was given, also justify the region.  The
      ;; first and only parameter contains the column to fill to.
      \(let \(\(fill-column column\)\)
        \(fill-region pos \(point\) at-flag\)\)\)
    ;; Obey the calling convention and return the probably modified
    ;; value of args.
    args\)\)

\(cl-format nil \"~5F\(Fill this region.~\) But not this.\"\)
=> 
\"Fill
this
region. But not this.\"

Using an argument as the parameter:

\(cl-format nil \"~vF\(Fill this region.~\)\" 10\)
=>
\"Fill this
region.\"\"

\(cl-format nil \"~v:F\(Fill this region.~\)\" 10\)
=>
error: \"Directive ~F does not support :-flag\""

  (declare (indent 2) 
           (debug (&define sexp sexp
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body)))
  (or (listp args)
      (error "Argument list should be a list"))
  (let* ((char-beg (if (consp char)
                       (car char)
                     char))
         (char-end (if (consp char)
                       (if (cdr char)
                           (cadr char)
                         t)))
         (args (cl-define-format-directive-parse-lambda-list args))
         (parameter (cdr (assq '&parameter args)))
         (contained-p (not (not (assq '&contained args))))
         (separator-p (not (not (assq '&separator args))))
         (end-separator-p (not (not (assq '&end-separator args))))
         (at-flag-p (not (not (assq '&at-flag args))))
         (colon-flag-p (not (not (assq '&colon-flag args))))
         (stash-p (not (not (assq '&stash args)))))

    (or (characterp char-beg)
        (error "Character expected: %s" char-beg))
    (or (not char-end)
        (eq char-end t)
        (characterp char-end)
        (error "Character expected: %s" char-beg))
    (or (not (memq char-beg cl-format-reserved-chars))
        (error "Invalid character for directive: %c" char))
    (or (not char-end)
        (not (memq char-beg cl-format-reserved-chars))
        (error "Invalid character for directive: %c" char-beg))
    (or (and (not char-end)
             (not contained-p)
             (not separator-p))
        (and char-end
             (or contained-p
                 separator-p))
        (error "Argument list does not match char-spec %s" char-end))
    (or (assq '&args args)
        (error "&args argument is required"))
    (or (stringp doc)
        (progn (push doc body)
               (setq doc nil)))

    (setq doc (concat (cl-format-directive-generate-documentation
                       char-beg char-end at-flag-p
                       colon-flag-p parameter
                       separator-p)
                      "\n"
                      (or doc "FIXME: Not documented.")))
    
    (while (keywordp (car body))
      (case (car body)
        (otherwise (error "Unrecognized keyword: %s" (car body))))
      (setq body (cddr body)))
               
    (setq args (remove-if-not 'cdr args))
    (let* ((parm-only (cdr (assq '&parameter args)))
           (arg-only (cdr (assq '&args args)))
           (contained-only (cdr (assq '&contained args)))
           (non-parm-arg (remove-if (lambda (kw)
                                      (memq (car kw)
                                            '(&parameter
                                              &args &contained)))
                                    args))
           (ll (mapcar
                (lambda (s)
                  (cons s (make-symbol
                           (substring (symbol-name s) 1))))
                '(&at-flag &colon-flag 
                           &contained &separator
                           &end-separator &stash)))
           (args-sym (make-symbol "args"))
           (let-args-1 `((list ',arg-only ',args-sym)))
           (let-args-2
            (mapcar (lambda (arg)
                      `(list ',(cdr arg)
                             (list
                              'quote ,(cdr (assq (car arg) ll)))))
                    non-parm-arg))
           (idx -1)
           (parm-symbol (make-symbol "parameter"))
           (args-parameter (make-symbol "args-parameter"))
           (let-args-3
            (mapcar
             (lambda (a)
               (or (consp a)
                   (setq a (list a)))
               `(list ',(car a)
                      (list 'or (list 'nth ,(incf idx)
                                      ',parm-symbol)
                            ',(eval (cadr a)))))
             parm-only))
           (let-args-4
            ;; make shure these functions get compiled
            (and contained-only
                 (let ((fn-arg (cdr (assq '&contained ll))))
                   `((list ',contained-only
                           (if (and (consp ,fn-arg)
                                    (not (eq 'lambda (car ,fn-arg))))
                               (cons 'list
                                     (mapcar
                                      (lambda (a)
                                        (list 'function a))
                                      ,fn-arg))
                             (list 'function ,fn-arg))))))))

      ;; macro expansion time
      `(let (format-fn clx)
       
         (cl-format-check-directive-chars ,char-beg ,char-end)
         ;; This constructs a function with 7 args for the runtime
         ;; values of the corresponding flags, parameter, etc. .  This
         ;; function will then return a cons (ARG-SYM . FORM), where
         ;; FORM may be evaluated, with ARG-SYM bound to the list of
         ;; arguments.  FORM of course implements the directive. 

         ;; (funcall (lambda (at-flag colon-flag contained separator
         ;;               end-separator stash &rest parameter))
         ;;  ...
         ;;  => (cons ARG-SYM FORM)
         (setq format-fn
               (lambda (,@(mapcar 'cdr ll) &rest ,parm-symbol)
                 (cons ',args-sym
                       (cons 'let
                             (cons
                              (list
                               (list ',args-parameter
                                     (list 'cl-format-expand-parameter
                                           ',args-sym
                                           (list 'quote ,parm-symbol)))
                               ',parm-symbol)
                              (append
                               (list
                                (list 'setq ',parm-symbol
                                      (list 'cdr ',args-parameter)
                                      ',args-sym (list 'car ',args-parameter))
                                (cons 'let
                                      (cons
                                       (list 
                                        ,@let-args-1
                                        ,@let-args-2
                                        ,@let-args-3
                                        ,@let-args-4)
                                       ',body)))))))))

         (setq clx (make-cl-format-directive
                    :documentation ,doc
                    :format-fn format-fn
                    :char-beg ,char-beg
                    :char-end ,char-end
                    :parameter ',parameter
                    :at-flag-p ,at-flag-p
                    :colon-flag-p ,colon-flag-p
                    :contained-p ,contained-p
                    :separator-p ,separator-p
                    :end-separator-p ,end-separator-p))
         (cl-format-register-directive clx)
         format-fn))))

(defun cl-format-check-directive-chars (char-beg char-end)
  "Check CHAR-BEG/CHAR-END for inconsistencies with existing directives."
;; FIXME: Das brauch noch etwas arbeit (recursive load etc.)
;; (when (memq char-beg
;;               cl-format-builtin-directives)
;;     (when cl-format-inhibit-redefining-builtins
;;       (error "Not redefining builtin directive ~%c.
;; See `cl-format-inhibit-redefining-builtins', if this
;; is what you really want to do." char-beg)))
  ;; FIXME: This does not handle cases, where a shadowing directive is
  ;; later removed.  But maybe this is good enough anyway.
  (let* ((directives (assq-delete-all
                      char-beg
                      (copy-seq cl-format-directives))))
    (dolist (d directives)
      (let ((other-end (cl-format-directive/char-end
                        (cdr d)))
            (other-beg (cl-format-directive/char-beg
                        (cdr d))))
        (when (or (eq char-beg other-end)
                  (eq char-end other-beg))
          (apply 'error
                 "Character `%c' used as end char in directive ~%c"
                 (if (eq char-beg other-end)
                     (list char-beg other-beg)
                   (list other-beg char-beg))))))))

(defun cl-format-register-directive (clx)
  "Make CLX known as a directive.

CLX is a `cl-format-directive' struct and it will shadow any
existing directives starting with the same char."
  (let* ((char (cl-format-directive/char-beg clx))
         (current (assoc char cl-format-directives)))
    (unless (eq (cdr current) clx)
      (if current
          (setcdr current clx)
        (setq cl-format-directives
              (cons (cons char clx)
                    cl-format-directives))))))

(defun cl-format-unregister-directive (char)
  "Delete the newest directive corresponding to CHAR."
  ;; (interactive (list (string-to-char
  ;;                     (completing-read
  ;;                      "Delete directive: "
  ;;                      (mapcar 'string (mapcar 'car cl-format-directives))
  ;;                      nil t))))
  (let ((dir (assq char cl-format-directives)))
    (when dir
      (setq cl-format-directives
            (delq dir cl-format-directives))
    t)))


;; Support for directive functions.

(put 'cl-format-error
     'error-conditions '(error cl-format-error))
(put 'cl-format-error
     'error-message "Error in cl-format")

(put 'cl-format-parse-error
     'error-conditions '(error cl-format-error cl-format-parse-error))
(put 'cl-format-parse-error
       'error-message "Error parsing cl-format string")

(put 'cl-format-eval-error
     'error-conditions '(error cl-format-error cl-format-eval-error))
(put 'cl-format-eval-error
     'error-message  "Error evaluating cl-format")

(defun cl-format-parse-error (msg fmt err-idx)
  (when err-idx
    (setq fmt (concat (substring fmt 0 err-idx)
                      "<!>"
                      (substring fmt err-idx))))
  (signal 'cl-format-parse-error (list msg fmt err-idx)))

(defun cl-format-eval-error (msg &optional data)
  (signal 'cl-format-eval-error
          (list msg data)))

(defmacro cl-format-check-and-pop (args)
  "Assert that the list ARGS is not nil and pop its `car'.

Signal a `cl-format-eval-error', if ARGS is the empty list."
  (or (symbolp args)
      (error "First argument is not a symbol"))
  `(progn
     (or ,args
         (cl-format-eval-error "Not enough arguments for format string"))
     (pop ,args)))
  
(defun cl-format-check-type (arg predicate)
  (or (funcall predicate arg)
      (cl-format-eval-error
       "Wrong type argument"
       (list predicate arg))))

(provide 'cl-format-def)

;;; cl-format-def.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
