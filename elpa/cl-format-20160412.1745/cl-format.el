;;; cl-format.el --- Common Lisp format function.

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

;; This package implements a CL format routine.
;;
;; Features:
;; * Supports most CL format directives.
;; * CL `format' and `formatter' functions implemented.
;; * Format strings are compiled to elisp.
;; * Output stream may be anything accepted by `standard-output', nil
;;   or t.
;; * Custom directives may be easily implemented (see
;;   `define-cl-format-directive')
;; * Partially based on GNU CLISP's format.
;; * Fontifys format strings.
;; * Extends CL format in some ways.

;; Deviations from CL:
;; * Directives are case-sensitive
;; 
;;   All CL letter directives use a lower case character (the `v' as
;;   well).  This leaves upper-case character for user defined
;;   directives.
;;
;; * Different case-change directive
;;
;;   I wanted to have a general way of defining directives like ~[~],
;;   without running out of memorizable end character.  For this the
;;   ~(~) directive was moved.  The extended syntax allows for
;;   any char to open a enclosing region:
;;       ~CHAR(....~)
;;   The CL case change operation is now
;;       ~|(...~)
;;   The original CL directive ~| (inserts ) is gone.
;; * Some minor additions (e.g. ~% may indent the line)

;; Installation:
;;
;; Put all files in `load-path' and (require 'cl-format) .
;;
;; Put the next line in your init file, if you want colored format strings.
;; (add-hook 'emacs-lisp-mode-hook 'cl-format-font-lock-mode)
;;
;; Usage:
;;
;; See the documentation of `cl-format'.  If you want to write your
;; own directives, take at look at some simple CL directives (e.g. ~%)
;; in cl-format-builtins.el and then proceed to the
;; `define-cl-format-directive' macro.

;;; Code:

(require 'cl)
(require 'cl-format-def)
(require 'cl-format-builtins)

(defvar cl-format-arguments nil)

(defconst cl-format-builtin-directives
  "List of CL directives."
  '(?c ?% ?~ ?& ?r ?d ?b ?o ?x ?f
       ?e ?g ?$ ?a ?s ?w ?_ ?< ?i
       ?/ ?t ?* ?\[?{ ?? ?| ?p ?^))

(defun cl-format-expand-parameter (args parms)
  "Use ARGS to expand the dynamic parameter in PARMS.

This expands :next-arg and :arg-count parameter \(`#' and `v' in
CL format parlance\)."
  (if (not (or (memq :next-arg parms)
               (memq :arg-count parms)))
      (cons args parms)
    (let* ((count (length args))
           (parms (mapcar (lambda (p)
                            (cond
                             ((not (keywordp p))
                              p)
                             ((eq p :arg-count)
                              count
                              ;;or (length args) ?
                              )
                             ((eq p :next-arg)
                              (or args
                                  (cl-format-eval-error
                                   "Not enough arguments for format string"))
                              (pop args))))
                          parms)))
      (cons args parms))))

(defun cl-format-base/alistify (clx)
  "Turn a `cl-format-base' CLX into an equivalent alist."
  (when clx
    `((:at-flag . ,(cl-format-base/at-flag clx))
      (:colon-flag . ,(cl-format-base/colon-flag clx))
      (:parameter . ,(cl-format-base/parameter clx)))))

(defun cl-format-get-directive (char)
  "Return the clx directive struct corresponding to CHAR."
  (cdr (assq char cl-format-directives)))
      ;; (if (eq char (downcase char))
      ;;     (cdr (assq (upcase char) cl-format-directives))
      ;;   (cdr (assq (downcase char) cl-format-directives)))


(defun cl-format-parse (fmt &optional start contained-end)
  "Parse format string FMT, starting at START until CONTAINED-END char.

START defaults to 0.  CONTAINED-END nil means parse until the end
of FMT.  This function returns a cons (FMT-LIST . END), where FMT-LIST is a list of cl-format-part structs corresponding to FMT and END is 1- the last processed CHAR of FMT."
  (let* (parts done char)
    (or start (setq start 0))
    (flet ((peek ()
             (when (< start (length fmt))
               (aref fmt start)))
           (next ()
             (prog1 (peek)
               (incf start))))
      (while (and (not done)
                  (< start (length fmt)))
        (destructuring-bind (literal &rest literal-end)
            (cl-format-parse-literal fmt start)
          (if literal
              (progn
                (push literal parts)
                (setq start literal-end))
            (assert (eq (peek) ?~))
            (next)
            (setq char (peek))
            (cond
             ((not char)
              (cl-format-parse-error
               "Format ends prematurely" fmt start))
             (t
              ;; edebug does not understand this
              ;; (destructuring-bind (parms . parms-end)
              (destructuring-bind (parms &rest parms-end)
                  (cl-format-parse-parameter fmt start)
                (setq start parms-end)
                (destructuring-bind (flags &rest flags-end)
                    (cl-format-parse-flags fmt start)
                  (setq start flags-end)
                  (let ((at-flag (cdr (assq :at-flag flags)))
                        (colon-flag (cdr (assq :colon-flag flags)))
                        (directive (cl-format-get-directive
                                    (setq char (next)))))
                    (cond
                     ((eq char contained-end)
                      (when (or (null parts)
                                (cl-format-clause-separator-p
                                 (car parts)))
                        (push nil parts))
                      (push (make-cl-format-clause-end-separator
                             :at-flag (cdr (assq :at-flag flags))
                             :colon-flag (cdr (assq :colon-flag flags))
                             :parameter parms) parts)
                      (setq done t
                            contained-end nil))
                     ((eq char ?\;)
                      (or contained-end
                          (cl-format-parse-error
                           "No ~; allowed here"
                           fmt start))
                      (when (or (null parts)
                                (cl-format-clause-separator-p
                                 (car parts)))
                        (push nil parts))
                      (push (make-cl-format-clause-separator
                             :at-flag (cdr (assq :at-flag flags))
                             :colon-flag (cdr (assq :colon-flag flags))
                             :parameter parms)
                            parts))
                     ((eq char ?\n)
                      (decf start)
                      (destructuring-bind (text &rest text-end)
                          (cl-format-parse-skip-whitespace
                           fmt start at-flag colon-flag)
                        (setq start text-end)
                        (when text
                          (push text parts))))
                     ((not directive)
                      (cl-format-parse-error
                       (format "Unknown directive ~%c%s" char
                               (if (eq char ?\()
                                   " ,use ~|(...~)" ""))
                       fmt start))
                     ((and at-flag
                           (not (cl-format-directive/at-flag-p
                                 directive)))
                      (cl-format-parse-error
                       (format "Directive ~%c does not support @-flag" char)
                       fmt start))
                     ((and colon-flag
                           (not (cl-format-directive/colon-flag-p
                                 directive)))
                      (cl-format-parse-error
                       (format "Directive ~%c does not support :-flag" char)
                       fmt start))
                     (t
                      (let (separator contained end-separator)
                        (when (cl-format-directive/char-end directive)
                          (let ((contained-end (cl-format-directive/char-end
                                                directive)))
                            (when (eq contained-end t)
                              (if (eq (peek) ?\()
                                  (setq char (next)
                                        contained-end ?\))))
                            (unless (eq contained-end t)
                              (setq contained
                                    (cl-format-parse
                                     fmt start contained-end)
                                    start (cdr contained)
                                    contained (cl-format-parse-clausify
                                               (car contained))
                                    separator (remove-if-not
                                               'cl-format-clause-separator-p
                                               contained)
                                    contained (remove-if
                                               'cl-format-clause-separator-p
                                               contained)))
                            (when (cl-format-clause-end-separator-p
                                   (car (last separator)))
                              (setq end-separator (car (last separator))
                                    separator (butlast separator))
                              (when (not (cl-format-directive/end-separator-p
                                          directive))
                                (setq end-separator nil)))
                            (when (and (not (cl-format-directive/separator-p
                                             directive))
                                       (> (length separator) 0))
                              (cl-format-parse-error
                               (format "Directive %c does not support ~; separator"
                                       (cl-format-directive/char-beg
                                        directive))
                               fmt start))))
                        (when (> (length parms)
                                 (length
                                  (cl-format-directive/parameter directive)))
                          (cl-format-parse-error
                           (format "Excess parameter given for directive ~%c"
                                   (cl-format-directive/char-beg
                                    directive))
                           fmt start))
                        (while (< (length parms)
                                  (length
                                   (cl-format-directive/parameter directive)))
                          (setq parms (append parms (list nil))))
                        (push (make-cl-format-part
                               :directive directive
                               :parameter parms
                               :at-flag (cdr (assq :at-flag flags))
                               :colon-flag (cdr (assq :colon-flag flags))
                               :separator separator
                               :contained contained
                               :end-separator end-separator)
                              parts))))))))))))
      (and contained-end
           (cl-format-parse-error
            (format "Format ends prematurely, expected ~%c" contained-end)
            fmt start))
      (cons (nreverse parts) start))))

(defun cl-format-parse-skip-whitespace (fmt start at-flag colon-flag)
  "Implements the ~\n directive."
  (when (and at-flag colon-flag)
    (error "Not both flags allowed here"))

  (if (or colon-flag
          (and (not at-flag)
               (= (1+ start) (length fmt))))
      (cons nil (incf start))
    (incf start)
    (destructuring-bind (text &rest start)
        (cl-format-parse-literal fmt start)
      (let ((idx 0))
        (while (and (< idx (length text))
                    (memq (aref text idx)
                          ;; (append " \t\f" nil)
                          '(32 9 12)))
        
          (incf idx))
        (cons (concat (if at-flag
                          "\n"
                        "")
                      (and text (substring text idx)))
              start)))))
  
(defun cl-format-parse-clausify (parts)
  (let (combined contained)
    (while parts
      (if (not (cl-format-clause-separator-p (car parts)))
          (push (pop parts) contained)
        (when contained
          (push (nreverse contained) combined)
          (setq contained nil))
        (push (pop parts) combined)))
    (when contained
      (push (nreverse contained) combined))
    (nreverse combined)))

(defun cl-format-parse-flags (fmt start)
  (let (flags done)
    (while (and (not done)
                (< start (length fmt)))
      (case (aref fmt start)
        (?@ (push (cons :at-flag t) flags))
        (?: (push (cons :colon-flag t) flags))
        (t (setq done t)))
      (unless done
        (incf start)))
    (cons flags start)))
  
(defun cl-format-parse-literal (fmt start)
  (let ((end (or (position ?~ fmt :start start)
                 (length fmt))))
    (cons (unless (= start end)
            (substring fmt start end))
          end)))

(defun cl-format-parse-parameter (fmt start)
  (let* ((done (not (< start (length fmt))))
         parms arg-read not-first)
    (while (not done)
      (when (setq arg-read
                  (eq (string-match
                       (eval-when-compile
                         (rx (or (group
                                  (? (char "+-"))
                                  (+ digit))
                                 (group ?\' anything)
                                 (group ?\#)
                                 (group (char "v")))))
                       fmt
                       start)
                      start))
        (setq start (+ start (length (match-string 0 fmt))))
        (cond
         ((match-string 1 fmt)
          (push (string-to-number (match-string 1 fmt)) parms))
         ((match-string 2 fmt)
          (push (aref ;;(substring
                 (match-string 2 fmt) 1) parms))
         ((match-string 3 fmt)
          (push :arg-count parms))
         (t
          (push :next-arg  parms))))
      
      (if (or (>= start (length fmt))
              (not (eq ?, (aref fmt start))))
          (setq done t)
        (incf start))
      
      (when (and (or (not done) not-first)
                 (not arg-read))
        (push nil parms))
      (setq not-first t))
        
    (cons (nreverse parms) start)))

(defun cl-format-parse-and-compile (fmt)
  "Parse and compile FMT into a function.

Return a function of one argument, which implements the FMT
format string.  The argument should be a list, representing the
dynamic arguments (as in `cl-format').  Output goes to
`standard-output'."
  (cl-format-compile
   (car (cl-format-parse fmt))))

(defun cl-format-compile (formatter)
  "Compile FORMATTER, a list of `cl-format-part' structs into a function.

See also `cl-format-parse-and-compile'."
  (unless (listp formatter)
    (setq formatter (list formatter)))
  (let ((stash (make-symbol "cl-format-stash"))
        format-forms)
    (dolist (clx formatter)
      (cond
       ((not clx))
       ((stringp clx)
        (push `(nil princ ,clx) format-forms))
       (t
        (let* ((dir (cl-format-part/directive clx))
               (fn (cl-format-directive/format-fn dir)))
          (push (apply fn (cl-format-part/at-flag clx)
                       (cl-format-part/colon-flag clx)
                       (if (cl-format-directive/separator-p dir)
                           (mapcar 'cl-format-compile
                                   (cl-format-part/contained clx))
                         (cl-format-compile
                          (car (cl-format-part/contained clx))))
                       (mapcar 'cl-format-base/alistify
                               (cl-format-part/separator clx))
                       (cl-format-base/alistify
                        (cl-format-part/end-separator clx))
                       stash
                       (cl-format-part/parameter clx))
                format-forms)))))
    (if (null format-forms)
        'identity
      `(lambda (args)
         ,@(mapcar (lambda (fmt)
                     (let ((arg-sym (car fmt))
                           (fmt (cdr fmt)))
                       (if (not arg-sym)
                           fmt
                         `(setq args (let ((,arg-sym args))
                                       ,fmt)))))
                   (nreverse format-forms))
         args))))
    
    
;;;###autoload
(defun cl-format (stream fmt &rest args)
  "Format FMT using ARGS and print it to STREAM.

The full documentation not available until this function is
loaded."
  ;; For the real documentation see
  ;; `cl-format-build-cl-format-documentation'.
  ;; (let ((fmt byte-compile (eval `(cl-formatter ,fmt))))
  (if (functionp fmt)
      (apply fmt stream args)
    (apply 'cl-format-1
           stream
           (cl-format-parse-and-compile fmt)
           args)))

(defun cl-format-1 (stream formatter &rest args)
  (let* ((tmp-buffer (generate-new-buffer-name " *cl-format*")))
    (unwind-protect
        (progn
          ;; Setup a buffer context.  If stream is not connected to
          ;; a buffer, use tmp-buffer.
          (set-buffer
           (cond
            ((bufferp stream)
             stream)
            ((markerp stream)
             (or (marker-buffer stream)
                 (cl-format-eval-error
                  "Marker points nowhere" stream)))
            (t (get-buffer-create tmp-buffer))))
          (let ((standard-output (or stream
                                     (get-buffer-create
                                      tmp-buffer))))
            (let ((cl-format-arguments args))
              (catch 'cl-format-up-and-out
                (funcall formatter args))))
          ;; output to string ?
          (and (null stream)
               (buffer-string)))
      (and (buffer-live-p (get-buffer tmp-buffer))
           (kill-buffer tmp-buffer)))))

(put 'cl-format 'function-documentation
     '(cl-format-build-cl-format-documentation))

(defun cl-format-build-cl-format-documentation ()
  (let (docs processed shadowed)
    (with-temp-buffer
      (dolist (d (mapcar 'cdr cl-format-directives))
        (let* ((char-beg (cl-format-directive/char-beg d)))
          (let ((fn-doc (or (cl-format-directive/documentation d)
                            "FIXME: Not documented.")))
            (let ((d (concat
                      (format "`%c'\n" char-beg)
                      (progn
                        (erase-buffer)
                        (princ fn-doc (current-buffer))
                        (indent-rigidly 1 (point-max) 4)
                        (buffer-string))
                      "\n")))
              (if (memq char-beg processed)
                  (push d shadowed)
                (push char-beg processed)
                (push d docs)))))))
    (concat
     "Format FMT using ARGS and print it to STREAM.

FMT is a Common Lisp format string, ARGS are the arguments for
it.  STREAM may be `nil', `t' or any other value that `print'
accepts as printcharfun argument.  Note that certain directives
which use buffer related functions (like e.g. `current-column'),
may not work properly, if STREAM is a function, i.e. has no
associated buffer.

When STREAM is `nil', return the result as a string.  In all
other cases, this function returns `nil'.

When STREAM is `t', output to `standard-output'.

Otherwise output to STREAM.

In `cl-format' directives are case-sensitive.  All CL directives
are implemented using the lower-case form (e.g. ~e and ~f).  This
leaves the uppercase letters for user defined directives.

See also `cl-message' and `cl-error'.

The following format directives are currently defined and active:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"

     (mapconcat 'identity (or docs '("\n\n")) "\n\n")
     (and docs "\n\n")

     "`\\n'
    ~@:\\n

    Tilde immediately followed by a newline ignores the newline and
    any following non-newline whitespace characters. With a :, the
    newline is ignored, but any following whitespace is left in
    place. With an @, the newline is left in place, but any following
    whitespace is ignored. This directive is typically used when a
    format control string is too long to fit nicely into one line of
    the program.
"
     (and shadowed
          (concat
           "


The following format directives are currently defined but not
active, since they are shadowed by other directives:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"
           (mapconcat 'identity shadowed "\n\n")))


     "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Documentation for most builtin directives adapted for Emacs from:
Guy L. Steele Jr., Common Lisp the Language, 2nd Edition
URL `http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/clm.html'")))

;;;###autoload
(defmacro cl-formatter (fmt)
  "Compile FMT into a function.

This macro parses and compiles FMT into a function, which may be
passed as format argument to `cl-format'."
  (or (stringp fmt)
      (signal 'wrong-type-argument
              (list 'stringp fmt)))
  `(lambda (stream &rest args)
     (apply 'cl-format-1 stream
            ,(cl-format-parse-and-compile fmt)
            args)))

;;;###autoload
(defun cl-error (fmt &rest args)
  "Like `error', but use CL format strings."
  (error "%s" (apply 'cl-format nil fmt args)))
         
;;;###autoload
(defun cl-message (fmt &rest args)
  "Like `error', but use CL format strings."
  (message "%s" (apply 'cl-format nil fmt args)))
  

;; font-lock support for format strings

(defgroup cl-format nil
  "Common lisp format"
  :group 'extensions)
  
(defface cl-format-tilde-face
  '((t (:inherit font-lock-keyword-face)))
  "Face used for the tilde escape"
  :group 'cl-format)

(defface cl-format-parameter-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for parameter."
  :group 'cl-format)

(defface cl-format-flags-face
  '((t (:inherit font-lock-builtin-face)))
  "Face used for flags."
  :group 'cl-format)

(defface cl-format-directive-face
  '((t (:inherit font-lock-type-face)))
  "Face used for directive chars."
  :group 'cl-format)
  
(defconst cl-format-directive-regexp
  (eval-when-compile
    (let* ((single-parameter '(or (: (? (char ?+ ?-))
                                     (+ digit))
                                  (: (char ?')
                                     not-newline)
                                  (char ?v ?#)))
           (parameter `(:
                        (? ,single-parameter)
                        (* (char ?,)
                           (? ,single-parameter))))
           (flags '(* (char ?@ ?:))))
      (rx-to-string
       `(: (group-n 1 (char ?~))
           (group-n 2 ,parameter)
           (group-n 3 ,flags)
           (group-n 4 (: not-newline
                         (? (group-n 5 ?\( )
                            )))))))

  "This matches a single `cl-format' directive.

After a successful match, the various parts of the directive may
be accessed via `match-string':

Group 1 contains the single character `~'.

Group 2 contains all parameter.

Group 3 contains the directives flags.

Group 4 contains the directive, including a potential trailing
`\('.

Group 5 is a submatch of group 4 and matches the above trailing
`\(' or nothing.")

(defvar cl-format-fontify-defforms-alist
  '((cl-format . 2)
    (cl-error . 1)
    (cl-message . 1))
  "List of functions containing format strings.

Each element is a cons \(FN . ARG-NR\), where ARG-NR is the
number of the argument of FN that should be fontified as a format
string.")


(defun cl-format-beginning-of-string ()
  (while (and (nth 3 (syntax-ppss))
              (not (bobp)))
    (if (= (skip-syntax-backward "^\"") 0)
        (backward-char))))
  
(defun cl-format-in-format-string-p (&optional pos)
  "Check whether POS is inside a format string.

POS defaults to point.  This function uses
`cl-format-fontify-defforms-alist'."
  
  (save-excursion
    (and pos (goto-char pos))
    ;; when in string ...
    (when (nth 3 (syntax-ppss))
      ;; ... go to the beginning of it
      (cl-format-beginning-of-string)
      ;; when inside a list ...
      (let ((paren-pos (nth 1 (syntax-ppss))))
        (when paren-pos
          (let ((i 0))
            ;; ... count the arguments
            (while (and (< paren-pos (point))
                        (ignore-errors
                          (backward-sexp) t))
              (incf i))
            (let ((form (assq (intern-soft (current-word t))
                              cl-format-fontify-defforms-alist)))
              (and form
                   (= (cdr form) i)))))))))

(defun cl-format-fontify-format-string (limit)
  (let (found)
    (put-text-property (point) limit
                       'cl-format-directive
                       nil)
    (while (and (not found)
                (re-search-forward cl-format-directive-regexp
                                   limit t))
      (setq found (save-match-data
                    (cl-format-in-format-string-p)))
      ;; When matching a trailing `(', check that it is a proper
      ;; directive for this construct (char-end is `t' in this case).
      (when (and found
                 (match-string 5))
        (let ((dir (cl-format-get-directive
                    (aref (match-string 4) 0))))
          (unless (and dir (eq t (cl-format-directive/char-end dir)))
            ;; Remove the paren from the match.
            (let* ((mdata (match-data))
                   (marker (car (nthcdr (1+ (* 2 4)) mdata))))
              (when (markerp marker)
                (move-marker marker (1- marker))
                (set-match-data mdata))))))
      (when found
        (put-text-property (match-beginning 0)
                           (match-end 0)
                           'cl-format-directive
                           (aref (match-string 4) 0))))
    found))


;; This is here to set a different syntax on format strings, but it
;;does not seem to work.

;; (defvar cl-format-font-lock-syntax-table nil)
;; (make-variable-buffer-local 'cl-format-font-lock-syntax-table)

;; (defun cl-format-font-lock-add-syntax (table)
;;   (dolist (chars '((?< ?>) (?{ ?}) (?\( ?\)) (?{ ?})))
;;     (modify-syntax-entry (car chars)
;;                          (string ?\( (cadr chars))
;;                          table)
;;     (modify-syntax-entry (cadr chars)
;;                          (string ?\) (car chars))
;;                          table))
;;   table)

;; (defun cl-format-fontify-format-string-syntax (limit)
;;   (let (found)
;;     (while (and (not found)
;;                 (re-search-forward
;;                  (rx (char ?\")
;;                      (* (or (not (any ?\" ?\\))
;;                             (: (char ?\\)
;;                                (* (: (char ?\\) (char ?\\))))))
;;                      (char ?\"))
;;                  limit t))
;;       (save-excursion
;;         (backward-char)
;;         (setq found (save-match-data
;;                       (cl-format-in-format-string-p)))))
;;     found))

;;;###autoload
(define-minor-mode cl-format-font-lock-mode
  "Adds font-lock support for cl format strings."
  nil nil nil
  (let ((kw '((cl-format-fontify-format-string
               (1 'cl-format-tilde-face t)
               (2 'cl-format-parameter-face t)
               (3 'cl-format-flags-face t)
               (4 'cl-format-directive-face t))))
        ;; This doesn't seem to work.
        ;; (sy '(cl-format-fontify-format-string-syntax
        ;;       0 cl-format-font-lock-syntax-table t))
        )
    (cond
     (cl-format-font-lock-mode
      (unless font-lock-mode
        (font-lock-mode 1))
      (font-lock-add-keywords nil kw)
      ;; (setq cl-format-font-lock-syntax-table
      ;;       (cl-format-font-lock-add-syntax
      ;;        (copy-syntax-table (syntax-table))))
      ;; (add-hook 'font-lock-syntactic-keywords sy nil t)
      )

     (t
      ;; (remove-hook 'font-lock-syntactic-keywords sy t)
      (font-lock-remove-keywords nil kw)))
    (font-lock-fontify-buffer)))

(defvar cl-format-eldoc-saved-doc-fn nil)
(make-variable-buffer-local 'cl-format-eldoc-saved-doc-fn)

(define-minor-mode cl-format-eldoc-mode
  "Adds eldoc support for cl format strings."
  :require 'eldoc
  (cond
   (cl-format-eldoc-mode
    (cl-format-font-lock-mode 1)
    (setq cl-format-eldoc-saved-doc-fn
          eldoc-documentation-function)
    (set (make-local-variable
          'eldoc-documentation-function)
         'cl-format-eldoc-documentation-function))
   (t
    (setq eldoc-documentation-function
          cl-format-eldoc-saved-doc-fn
          cl-format-eldoc-saved-doc-fn nil))))

(defun cl-format-eldoc-documentation-function ()
  (if (null (plist-get (text-properties-at (point))
                       'cl-format-directive))
      (let ((eldoc-documentation-function
             cl-format-eldoc-saved-doc-fn))
        (eldoc-print-current-symbol-info))
    (let* ((char (plist-get (text-properties-at (point))
                            'cl-format-directive))
           (dir (cl-format-get-directive char)))
      (if (null dir)
          (cl-format nil "Unknown directive: ~~~c" char)
        (eldoc-docstring-first-line
         (cl-format-directive/documentation dir))))))
          
;; font lock + imenu for define-cl-format-directive
(let ((re (rx "(" (group "define-cl-format-directive") symbol-end
              (* space)
              (or (group (or (: (or (: "?" (opt "\\") anything) (+ digit)))
                             (: "(" (or (: "?" (opt "\\") anything) (+ digit)) ")")
                             (: "("
                                (or (: "?" (opt "\\") anything) (+ digit))
                                space
                                (or (: "?" (opt "\\") anything) (+ digit))
                                ")")))
                  (group (+ (or (syntax word) (syntax symbol))))))))
  
  (font-lock-add-keywords 'emacs-lisp-mode
    `((,re
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t)
       (3 'error nil t))))

  (add-hook 'emacs-lisp-mode-hook
            `(lambda nil
               (add-to-list 'imenu-generic-expression
                            '(nil ,re 2)))))

(provide 'cl-format)

;;; cl-format.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
