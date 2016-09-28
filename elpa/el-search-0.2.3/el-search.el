;;; el-search.el --- Expression based incremental search for emacs-lisp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 29 Jul 2015
;; Keywords: lisp
;; Compatibility: GNU Emacs 25
;; Version: 0.2.3
;; Package-Requires: ((emacs "25"))


;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Introduction
;; ============
;;
;;
;; The main user entry point is `el-search-pattern'.  This command
;; prompts for a `pcase' pattern and searches the current buffer for
;; matching expressions by iteratively `read'ing buffer contents.  For
;; any match, point is put at the beginning of the expression found
;; (unlike isearch which puts point at the end of matches).
;;
;; Why is it based on `pcase'?  Because pattern matching (and the
;; ability to combine destructuring and condition testing) is well
;; suited for this task.  In addition, pcase allows to add specialized
;; pattern types and to combine them with other patterns in a natural
;; and transparent way out of the box.
;;
;; It doesn't matter how the code is actually formatted.  Comments are
;; ignored, and strings are treated as atomic objects, their contents
;; are not being searched.
;;
;;
;; Example 1: if you enter
;;
;;    97
;;
;; at the prompt, this will find any occurrence of the number 97 in
;; the code, but not 977 or (+ 90 7) or "My string containing 97".
;; But it will find anything `eq' to 97 after reading, e.g. #x61 or
;; ?a.
;;
;;
;; Example 2: If you enter the pattern
;;
;;   `(defvar ,_)
;;
;; you search for all defvar forms that don't specify an init value.
;;
;; The following will search for defvar forms with a docstring whose
;; first line is longer than 70 characters:
;;
;;   `(defvar ,_ ,_
;;      ,(and s (guard (< 70 (length (car (split-string s "\n")))))))
;;
;;
;; When a search pattern is processed, the searched buffer is current
;; with point at the beginning of the currently tested expression.
;;
;;
;; Convenience
;; ===========
;;
;; For pattern input, the minibuffer is put into `emacs-lisp-mode'.
;;
;; Any input PATTERN is silently transformed into (and exp PATTERN)
;; so that you can always refer to the whole currently tested
;; expression via the variable `exp'.
;;
;;
;; Example 3:
;;
;; If you want to search a buffer for symbols that are defined in
;; "cl-lib", you can use this pattern
;;
;;   (guard (and (symbolp exp)
;;               (when-let ((file (symbol-file exp)))
;;                 (string-match-p "cl-lib\\.elc?$" file))))
;;
;;
;; ,----------------------------------------------------------------------
;; | Q: "But I hate `pcase'!  Can't we just do without?"                 |
;; |                                                                     |
;; | A: Respect that you kept up until here! Just use (guard CODE), where|
;; | CODE is any normal Elisp expression that returns non-nil when and   |
;; | only when you have a match.  Use the variable `exp' to refer to     |
;; | the currently tested expression.  Just like in the last example!    |
;; `----------------------------------------------------------------------
;;
;;
;; It's cumbersome to write out the same complicated pattern
;; constructs in the minibuffer again and again.  You can define your
;; own pcase pattern types for the purpose of el-search with
;; `el-search-defpattern'.  It is just like `pcase-defmacro', but the
;; effect is limited to this package.  See C-h f `el-search-pattern'
;; for a list of predefined additional pattern forms.
;;
;; Some additional pattern definitions can be found in the file
;; "el-search-x.el".
;;
;;
;; Replacing
;; =========
;;
;; You can replace expressions with command `el-search-query-replace'.
;; You are queried for a (pcase) pattern and a replacement expression.
;; For each match of the pattern, the replacement expression is
;; evaluated with the bindings created by the pcase matching in
;; effect, and printed to produce the replacement string.
;;
;; Example: In some buffer you want to swap the two expressions at the
;; places of the first two arguments in all calls of function `foo',
;; so that e.g.
;;
;;   (foo 'a (* 2 (+ 3 4)) t)
;;
;; becomes
;;
;;   (foo (* 2 (+ 3 4)) 'a t).
;;
;; This will do it:
;;
;;    M-x el-search-query-replace RET
;;    `(foo ,a ,b . ,rest) RET
;;    `(foo ,b ,a . ,rest) RET
;;
;; Type y to replace a match and go to the next one, r to replace
;; without moving, SPC to go to the next match and ! to replace all
;; remaining matches automatically.  q quits.  n is like SPC, so that
;; y and n work like in isearch (meaning "yes" and "no") if you are
;; used to that.
;;
;; It is possible to replace a match with multiple expressions using
;; "splicing mode".  When it is active, the replacement expression
;; must evaluate to a list, and is spliced instead of inserted into
;; the buffer for any replaced match.  Use s to toggle splicing mode
;; in a `el-search-query-replace' session.
;;
;;
;; Suggested key bindings
;; ======================
;;
;;    (define-key emacs-lisp-mode-map [(control ?S)] #'el-search-pattern)
;;    (define-key emacs-lisp-mode-map [(control ?%)] #'el-search-query-replace)
;;
;;    (define-key isearch-mode-map [(control ?S)] #'el-search-search-from-isearch)
;;    (define-key isearch-mode-map [(control ?%)] #'el-search-replace-from-isearch)
;;
;;    (define-key el-search-read-expression-map [(control ?S)] #'exit-minibuffer)
;;
;; The bindings in `isearch-mode-map' let you conveniently switch to
;; "el-search" searching from isearch.  The binding in
;; `el-search-read-expression-map' allows you to hit C-S twice to
;; start a search for the last search pattern.
;;
;;
;; Bugs, Known Limitations
;; =======================
;;
;; - Replacing: in some cases the reader syntax of forms
;; is changing due to reading+printing.  "Some" because we can treat
;; that problem in most cases.
;;
;; - Similarly: Comments are normally preserved (where it makes
;; sense).  But when replacing like `(foo ,a ,b) -> `(foo ,b ,a)
;;
;; in a content like
;;
;;   (foo
;;     a
;;     ;;a comment
;;     b)
;;
;; the comment will be lost.
;;
;; FIXME: when we have resumable sessions, pause and warn about this case.
;;
;;
;;  Acknowledgments
;;  ===============
;;
;; Thanks to Stefan Monnier for corrections and advice.
;;
;;
;; TODO:
;;
;; - implement backward searching
;;
;; - Make `el-search-pattern' accept an &optional limit, at least for
;;   the non-interactive use case?
;;
;; - improve docstrings
;;
;; - Make it work in comments, too? (-> `parse-sexp-ignore-comments').
;;   Related: should the pattern `symbol' also match strings that
;;   contain matches for a symbol so that it's possible to replace
;;   also occurrences of a symbol in docstrings?
;;
;; - handle more reader syntaxes, e.g. #n, #n#
;;
;; - Implement sessions; add multi-file support based on iterators.  A
;;   file list is read in (or the user can specify an iterator as a
;;   variable).  The state in the current buffer is just (buffer
;;   . marker).  Or should this be abstracted into an own lib?  Could
;;   be named "files-session" or so.
;;
;; - Make `el-search--format-replacement' work non-heuristically.
;;   Idea: When replacing, for every variable V bound by the search
;;   pattern that directly corresponds to some text T, provide some
;;   "match data" V -> T.  Use this when formatting the replacement.
;;   Maybe use a special marker to "paste" in expressions, like (paste
;;   V), whereby the `paste' flag lands in the replacement and can be
;;   replaced textually afterwards.



;;; Code:

;;;; Requirements

(eval-when-compile
  (require 'subr-x))

(require 'cl-lib)
(require 'elisp-mode)
(require 'thingatpt)
(require 'help-fns) ;el-search--make-docstring


;;;; Configuration stuff

(defgroup el-search nil
  "Expression based search and replace for `emacs-lisp-mode'."
  :group 'lisp)

(defcustom el-search-this-expression-identifier 'exp
  "Identifier ID referring to the current expression in pattern input.
When entering a PATTERN in an interactive \"el-search\" command,
the pattern actually used will be (and ID PATTERN).
The default value is `exp'."
  :type 'symbol)

(defface el-search-match '((((background dark)) (:background "#0000A0"))
			   (t                   (:background "DarkSlateGray3")))
  "Face for highlighting the current match.")

(defface el-search-other-match '((((background dark)) (:background "#202060"))
                                 (t                   (:background "DarkSlateGray1")))
  "Face for highlighting the other matches.")

(defcustom el-search-smart-case-fold-search t
  "Whether to use smart case folding in pattern matching.
When an \"el-search\" pattern involves regexp matching (like for
\"string\" or \"source\") and this option is non-nil,
case-fold-search will be temporarily bound to t if the according
regexp contains any upper case letter, and nil else.  This is
done independently for every single matching operation.

If nil, the value of `case-fold-search' is decisive."
  :type 'boolean)

(defcustom el-search-use-sloppy-strings nil
  "Whether to allow the usage of \"sloppy strings\".
When this option is turned on, for faster typing you are allowed
to specify symbols instead of strings as arguments to an
\"el-search\" pattern type that would otherwise accept only
strings, and their names will be used as input (with other words,
this spares you to type the string delimiters in many cases).

For example,

  \(source ^cl\)

is then equivalent to

  \(source \"^cl\"\)

When this option is off, the first form would just signal an
error."
  :type 'boolean)


;;;; Helpers

(defun el-search--smart-string-match-p (regexp string)
  "`string-match-p' taking `el-search-smart-case-fold-search' into account."
  (let ((case-fold-search (if el-search-smart-case-fold-search
                              (not (let ((case-fold-search nil))
                                     (string-match-p "[[:upper:]]" regexp)))
                            case-fold-search)))
    (string-match-p regexp string)))

(defun el-search--pp-to-string (expr)
  (let ((print-length nil)
        (print-level nil))
    (pp-to-string expr)))

(defvar el-search-read-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map read-expression-map)
    (define-key map [(control ?g)] #'abort-recursive-edit)
    (define-key map [up]   nil)
    (define-key map [down] nil)
    (define-key map [(control ?j)] #'newline)
    map)
  "Map for reading input with `el-search-read-expression'.")

(defun el-search--setup-minibuffer ()
  (let ((inhibit-read-only t))
    (put-text-property 1 (minibuffer-prompt-end) 'font-lock-face 'minibuffer-prompt))
  (emacs-lisp-mode)
  (use-local-map el-search-read-expression-map)
  (setq font-lock-mode t)
  (funcall font-lock-function 1)
  (goto-char (minibuffer-prompt-end))
  (when (looking-at ".*\n")
    (indent-sexp))
  (goto-char (point-max))
  (when-let ((this-sexp (with-current-buffer (window-buffer (minibuffer-selected-window))
                          (thing-at-point 'sexp))))
    (let ((more-defaults (list (concat "'" this-sexp))))
      (setq-local minibuffer-default-add-function
                  (lambda () (if (listp minibuffer-default)
                            (append minibuffer-default more-defaults)
                          (cons minibuffer-default more-defaults)))))))

;; $$$$$FIXME: this should be in Emacs!  There is only a helper `read--expression'.
(defun el-search-read-expression (prompt &optional initial-contents hist default read)
  "Read expression for `my-eval-expression'."
  (minibuffer-with-setup-hook #'el-search--setup-minibuffer
    (read-from-minibuffer prompt initial-contents el-search-read-expression-map read
                          (or hist 'read-expression-history) default)))

(defvar el-search-history '()
  "List of search input strings.")

(defvar el-search-query-replace-history '()
  "List of input strings from `el-search-query-replace'.")

(defvar el-search--initial-mb-contents nil)

(defun el-search--pushnew-to-history (input histvar)
  (let ((hist-head (car (symbol-value histvar))))
    (unless (or (string-match-p "\\`\\'" input)
                (and (stringp hist-head)
                     (or (string= input hist-head)
                         (ignore-errors (equal (read input) (read hist-head))))))
      (push (if (string-match-p "\\`.+\n" input)
                (with-temp-buffer
                  (emacs-lisp-mode)
                  (insert "\n" input)
                  (indent-region 1 (point))
                  (buffer-string))
              input)
            (symbol-value histvar)))))

(defun el-search--read-pattern (prompt &optional default histvar)
  (cl-callf or histvar 'el-search-history)
  (let ((input (el-search-read-expression
                prompt el-search--initial-mb-contents histvar default)))
    (el-search--pushnew-to-history input histvar)
    (if (not (string= input "")) input (car (symbol-value histvar)))))

(defun el-search--end-of-sexp ()
  ;;Point must be at sexp beginning
  (or (scan-sexps (point) 1) (point-max)))

(defun el-search--ensure-sexp-start ()
  "Move point to the next sexp beginning position.
Don't move if already at beginning of a sexp.  Point must not be
inside a string or comment.  `read' the expression at that point
and return it."
  ;; This doesn't catch end-of-buffer to keep the return value non-ambiguous
  (let ((not-done t) res)
    (while not-done
      (let ((stop-here nil)
            (looking-at-from-back (lambda (regexp n)
                                    (and (<= n (- (point) (point-min)))
                                         (save-excursion
                                           (backward-char n)
                                           (looking-at regexp))))))
        (while (not stop-here)
          (cond
           ((eobp) (signal 'end-of-buffer nil))
           ((looking-at (rx (and (* space) ";"))) (forward-line))
           ((looking-at (rx (+ (or space "\n")))) (goto-char (match-end 0)))

           ;; FIXME: can the rest be done more generically?
           ((and (looking-at (rx (or (syntax symbol) (syntax word))))
                 (not (looking-at "\\_<"))
                 (not (funcall looking-at-from-back ",@" 2)))
            (forward-symbol 1))
           ((or (and (looking-at "'") (funcall looking-at-from-back "#" 1))
                (and (looking-at "@") (funcall looking-at-from-back "," 1)))
            (forward-char))
           (t (setq stop-here t)))))
      (condition-case nil
          (progn
            (setq res (save-excursion (read (current-buffer))))
            (setq not-done nil))
        (error (forward-char))))
    res))

(defvar el-search--pcase-macros '()
  "List of additional \"el-search\" pcase macros.")

(defun el-search--make-docstring ()
  ;; code mainly from `pcase--make-docstring'
  (let* ((main (documentation (symbol-function 'el-search-pattern) 'raw))
         (ud (help-split-fundoc main 'pcase)))
    (with-temp-buffer
      (insert (or (cdr ud) main))
      (mapc
       (pcase-lambda (`(,symbol . ,fun))
         (unless (string-match-p "\\`-\\|--" (symbol-name symbol)) ;let's consider these "internal"
           (when-let ((doc (documentation fun)))
             (insert "\n\n\n-- ")
             (setq doc (help-fns--signature symbol doc fun fun nil))
             (insert "\n" (or doc "Not documented.")))))
       (reverse el-search--pcase-macros))
      (let ((combined-doc (buffer-string)))
        (if ud (help-add-fundoc-usage combined-doc (car ud)) combined-doc)))))

(put 'el-search-pattern 'function-documentation '(el-search--make-docstring))

(defmacro el-search-defpattern (name args &rest body)
  "Like `pcase-defmacro', but limited to el-search patterns.
The semantics is exactly that of `pcase-defmacro', but the scope
of the definitions is limited to \"el-search\".

\(fn NAME ARGLIST &optional DOCSTRING &rest BODY)"
  (declare (indent 2) (debug defun))
  `(setf (alist-get ',name el-search--pcase-macros)
         (lambda ,args ,@body)))

(defun el-search--macroexpand-1 (pattern)
  "Expand \"el-search\" PATTERN.
This is like `pcase--macroexpand', but expands only patterns
defined with `el-search-defpattern' and performs only one
expansion step.

Return PATTERN if this pattern type was not defined with
`el-search-defpattern'."
  (if-let ((expander (alist-get (car-safe pattern) el-search--pcase-macros)))
      (apply expander (cdr pattern))
    pattern))

(defmacro el-search--with-additional-pcase-macros (&rest body)
  `(cl-letf ,(mapcar (pcase-lambda (`(,symbol . ,fun))
                       `((get ',symbol 'pcase-macroexpander) #',fun))
                     el-search--pcase-macros)
     ,@body))

(defun el-search--matcher (pattern &optional result)
  (eval ;use `eval' to allow for user defined pattern types at run time
   (let ((expression (make-symbol "expression")))
     `(el-search--with-additional-pcase-macros
       (let ((byte-compile-debug t) ;make undefined pattern types raise an error
             (warning-suppress-log-types '((bytecomp)))
             (pcase--dontwarn-upats (cons '_ pcase--dontwarn-upats)))
         (byte-compile (lambda (,expression)
                         (pcase ,expression
                           (,pattern ,(or result t))
                           (_        nil)))))))))

(defun el-search--match-p (matcher expression)
  (funcall matcher expression))

(defun el-search--wrap-pattern (pattern)
  `(and ,el-search-this-expression-identifier ,pattern))

(defun el-search--skip-expression (expression &optional read)
  ;; Move forward at least one character.  Don't move into a string or
  ;; comment.  Don't move further than the beginning of the next sexp.
  ;; Try to move as far as possible.  Point must be at the beginning
  ;; of an expression.
  ;; If there are positions where `read' would succeed, but that do
  ;; not represent a valid sexp start, move past them (e.g. when
  ;; before "#'" move past both characters).
  ;;
  ;; EXPRESSION must be the (read) expression at point, but when READ
  ;; is non-nil, ignore the first argument and read the expression at
  ;; point instead.
  (when read (setq expression (save-excursion (read (current-buffer)))))
  (cond
   ((or (null expression)
        (equal [] expression)
        (not (or (listp expression) (vectorp expression))))
    (goto-char (el-search--end-of-sexp)))
   ((looking-at (rx (or ",@" "," "#'" "'")))
    (goto-char (match-end 0)))
   (t (forward-char))))

(defun el-search--search-pattern-1 (matcher &optional noerror)
  (let ((match-beg nil) (opoint (point)) current-expr)

    ;; when inside a string or comment, move past it
    (let ((syntax-here (syntax-ppss)))
      (when (nth 3 syntax-here) ;inside a string
        (goto-char (nth 8 syntax-here))
        (forward-sexp))
      (when (nth 4 syntax-here) ;inside a comment
        (forward-line 1)
        (while (and (not (eobp)) (looking-at (rx (and (* space) ";"))))
          (forward-line 1))))

    (if (catch 'no-match
          (while (not match-beg)
            (condition-case nil
                (setq current-expr (el-search--ensure-sexp-start))
              (end-of-buffer
               (goto-char opoint)
               (throw 'no-match t)))
            (if (el-search--match-p matcher current-expr)
                (setq match-beg (point)
                      opoint (point))
              (el-search--skip-expression current-expr))))
        (if noerror nil (signal 'end-of-buffer nil)))
    match-beg))

(defun el-search--search-pattern (pattern &optional noerror)
  "Search for el-search PATTERN in current buffer.
Set point to the beginning of the occurrence found and return
point.  Optional second argument, if non-nil, means if fail just
return nil (no error)."
  (el-search--search-pattern-1 (el-search--matcher pattern) noerror))

(defun el-search--replace-hunk (region to-insert)
  "Replace the text in REGION in current buffer with string TO-INSERT.
Add line breaks before and after TO-INSERT when appropriate and
reindent."
  (atomic-change-group
    (let* ((inhibit-message t)
           (opoint (point))
           (original-text (prog1 (apply #'buffer-substring-no-properties region)
                            (goto-char (car region))
                            (apply #'delete-region region)))
           ;; care about other sexps in this line
           (sexp-before-us (not (looking-back "\(\\|^\\s-*" (line-beginning-position))))
           (sexp-after-us  (not (looking-at "\\s-*[;\)]\\|$")))
           (insert-newline-before
            (or
             (and (string-match-p "\n" to-insert)
                  (not (string-match-p "\n" original-text))
                  (or (and sexp-before-us sexp-after-us)
                      (looking-back
                       (rx (or (syntax word) (syntax symbol))
                           (+ blank)
                           (or (syntax word) (syntax symbol))
                           (* any))
                       (line-beginning-position))))
             ;; (and sexp-before-us
             ;;      (> (+ (apply #'max (mapcar #'length (split-string to-insert "\n")))
             ;;            (- (point) (line-beginning-position)))
             ;;         fill-column))
             ))
           (insert-newline-after (and insert-newline-before sexp-after-us)))
      (when insert-newline-before
        (when (looking-back "\\s-+" (line-beginning-position))
          (delete-region (match-beginning 0) (match-end 0)))
        (insert "\n"))
      (insert to-insert)
      (when insert-newline-after
        (insert "\n"))
      (indent-region opoint (1+ (point))))))

(defun el-search--format-replacement (replacement original replace-expr-input splice)
  ;; Return a printed representation of REPLACEMENT.  Try to reuse the
  ;; layout of subexpressions shared with the original (replaced)
  ;; expression and the replace expression.
  (if (and splice (not (listp replacement)))
      (error "Expression to splice in is an atom")
    (let ((orig-buffer (generate-new-buffer "orig-expr")))
      (with-current-buffer orig-buffer
        (emacs-lisp-mode)
        (insert original)
        (when replace-expr-input (insert "\n\n" replace-expr-input)))
      (unwind-protect
          (with-temp-buffer
            (emacs-lisp-mode)
            (insert (if splice
                        (mapconcat #'el-search--pp-to-string replacement " ")
                      (el-search--pp-to-string replacement)))
            (goto-char 1)
            (let (start this-sexp end orig-match-start orig-match-end done)
              (while (and (< (point) (point-max))
                          (condition-case nil
                              (progn
                                (setq start (point)
                                      this-sexp (read (current-buffer))
                                      end   (point))
                                t)
                            (end-of-buffer nil)))
                (setq done nil orig-match-start nil)
                (with-current-buffer orig-buffer
                  (goto-char 1)
                  (if (el-search--search-pattern `',this-sexp t)
                      (setq orig-match-start (point)
                            orig-match-end (progn (forward-sexp) (point)))
                    (setq done t)))
                ;; find out whether we have a sequence of equal expressions
                (while (and (not done)
                            (condition-case nil
                                (progn (setq this-sexp (read (current-buffer))) t)
                              ((invalid-read-syntax end-of-buffer end-of-file) nil)))
                  (if (with-current-buffer orig-buffer
                        (condition-case nil
                            (if (not (equal this-sexp (read (current-buffer))))
                                nil
                              (setq orig-match-end (point))
                              t)
                          ((invalid-read-syntax end-of-buffer end-of-file) nil)))
                      (setq end (point))
                    (setq done t)))
                ;; FIXME: there could be another occurrence of THIS-SEXP in ORIG-BUFFER with more
                ;; subsequent equal expressions after it
                (if orig-match-start
                    (el-search--replace-hunk
                     (list start end)
                     (with-current-buffer orig-buffer
                       (buffer-substring-no-properties orig-match-start orig-match-end)))
                  (goto-char start)
                  (el-search--skip-expression nil t))
                (condition-case nil
                    (el-search--ensure-sexp-start)
                  (end-of-buffer (goto-char (point-max))))))
            (goto-char 1)
            (forward-sexp (if splice (length replacement) 1))
            (let ((result (buffer-substring 1 (point))))
              (if (equal replacement (read (if splice (format "(%s)" result) result)))
                  result
                (error "Error in `el-search--format-replacement' - please make a bug report"))))
        (kill-buffer orig-buffer)))))

(defun el-search--check-pattern-args (type args predicate &optional message)
  "Check whether all ARGS fulfill PREDICATE.
Raise an error if not.  The string arguments TYPE and optional
MESSAGE are used to construct the error message."
  (mapc (lambda (arg)
          (unless (funcall predicate arg)
            (error (concat "Pattern `%s': "
                           (or message (format "argument doesn't fulfill %S" predicate))
                           ": %S")
                   type arg)))
        args))

(defvar el-search-current-pattern nil)

(defvar el-search-success nil)


;;;; Additional pattern type definitions

(defun el-search--split (matcher1 matcher2 list)
  "Helper for the append pattern type.

When a splitting of LIST into two lists L1, L2 exist so that Li
is matched by MATCHERi, return (L1 L2) for such Li, else return
nil."
  (let ((try-match (lambda (list1 list2)
                     (when (and (el-search--match-p matcher1 list1)
                                (el-search--match-p matcher2 list2))
                       (list list1 list2))))
        (list1 list) (list2 '()) (match nil))
    ;; don't use recursion, this could hit `max-lisp-eval-depth'
    (while (and (not (setq match (funcall try-match list1 list2)))
                (consp list1))
      (let ((last-list1 (last list1)))
        (if-let ((cdr-last-list1 (cdr last-list1)))
            ;; list1 is a dotted list.  Then list2 must be empty.
            (progn (setcdr last-list1 nil)
                   (setq list2 cdr-last-list1))
          (setq list1 (butlast list1 1)
                list2 (cons (car last-list1) list2)))))
    match))

(el-search-defpattern append (&rest patterns)
  "Matches any list factorable into lists matched by PATTERNS in order.

PATTERNS is a list of patterns P1..Pn.  Match any list L for that
lists L1..Ln exist that are matched by P1..Pn in order and L is
equal to the concatenation of L1..Ln.  Ln is allowed to be no
list.

When different ways of matching are possible, it is unspecified
which one is chosen.

Example: the pattern

   (append '(1 2 3) x (app car-safe 7))

matches the list (1 2 3 4 5 6 7 8 9) and binds `x' to (4 5 6)."
  (if (null patterns)
      '(pred null)
    (pcase-let ((`(,pattern . ,more-patterns) patterns))
      (cond
       ((null more-patterns)  pattern)
       ((null (cdr more-patterns))
        `(and (pred listp)
              (app ,(apply-partially #'el-search--split
                                     (el-search--matcher pattern)
                                     (el-search--matcher (car more-patterns)))
                   `(,,pattern ,,(car more-patterns)))))
       (t `(append ,pattern (append ,@more-patterns)))))))

(defun el-search--stringish-p (thing)
  (or (stringp thing) (and el-search-use-sloppy-strings (symbolp thing))))

(el-search-defpattern string (&rest regexps)
  "Matches any string that is matched by all REGEXPS."
  (el-search--check-pattern-args "string" regexps #'el-search--stringish-p
                                 "Argument not a string")
  `(and (pred stringp)
        ,@(mapcar (lambda (thing) `(pred (el-search--smart-string-match-p
                                     ,(if (symbolp thing) (symbol-name thing) thing))))
                  regexps)))

(el-search-defpattern symbol (&rest regexps)
  "Matches any symbol whose name is matched by all REGEXPS."
  (el-search--check-pattern-args "symbol" regexps #'el-search--stringish-p
                                 "Argument not a string")
  `(and (pred symbolp)
        (app symbol-name (string ,@regexps))))

(defun el-search--contains-p (matcher exp)
  "Return non-nil when tree EXP contains a match for MATCHER.
Recurse on all types of sequences.  In the positive case the
return value is (t elt), where ELT is a matching element found in
EXP."
  (if (el-search--match-p matcher exp)
      (list t exp)
    (and (sequencep exp)
         (let ((try-match (apply-partially #'el-search--contains-p matcher)))
           (if (consp exp)
               (or (funcall try-match (car exp))
                   (funcall try-match (cdr exp)))
             (cl-some try-match exp))))))

(el-search-defpattern contains (&rest patterns)
  "Matches trees that contain a match for all PATTERNs.
Searches any tree of sequences recursively for matches.  Objects
of any kind matched by all PATTERNs are also matched.

  Example: (contains (string \"H\") 17) matches ((\"Hallo\") x (5 [1 17]))"
  (cond
   ((null patterns) '_)
   ((null (cdr patterns))
    (let ((pattern (car patterns)))
      `(app ,(apply-partially #'el-search--contains-p (el-search--matcher pattern))
            `(t ,,pattern))))
   (t `(and ,@(mapcar (lambda (pattern) `(contains ,pattern)) patterns)))))

(el-search-defpattern not (pattern)
  "Matches any object that is not matched by PATTERN."
  `(app ,(apply-partially #'el-search--match-p (el-search--matcher pattern))
        (pred not)))

(defun el-search--match-symbol-file (regexp symbol)
  (when-let ((symbol-file (and (symbolp symbol)
                               (symbol-file symbol))))
    (el-search--smart-string-match-p
     (if (symbolp regexp) (concat "\\`" (symbol-name regexp) "\\'") regexp)
     (file-name-sans-extension (file-name-nondirectory symbol-file)))))

(el-search-defpattern source (regexp)
  "Matches any symbol whose `symbol-file' is matched by REGEXP.

This pattern matches when the object is a symbol for that
`symbol-file' returns a (non-nil) FILE-NAME that fulfills
  (string-match-p REGEXP (file-name-sans-extension
                           (file-name-nondirectory FILENAME)))

REGEXP can also be a symbol, in which case

  (concat \"^\" (symbol-name regexp) \"$\")

is used as regular expression."
  (el-search--check-pattern-args "source" (list regexp) #'el-search--stringish-p
                                 "Argument not a string")
  `(pred (el-search--match-symbol-file ,(if (symbolp regexp) (symbol-name regexp) regexp))))

(defun el-search--match-key-sequence (keys expr)
  (when-let ((expr-keys (pcase expr
                          ((or (pred stringp) (pred vectorp))  expr)
                          (`(kbd ,(and (pred stringp) string)) (ignore-errors (kbd string))))))
    (apply #'equal
           (mapcar (lambda (keys) (ignore-errors (key-description keys)))
                   (list keys expr-keys)))))

(el-search-defpattern keys (key-sequence)
  "Matches descriptions of the KEY-SEQUENCE.
KEY-SEQUENCE is a string or vector representing a key sequence,
or an expression of the form (kbd STRING).

Match any description of the same key sequence in any of these
formats.

Example: the pattern

    (keys (kbd \"C-s\"))

matches any of these expressions:

    \"\\C-s\"
    \"\C-s\"
    (kbd \"C-s\")
    [(control ?s)]"
  (when (eq (car-safe key-sequence) 'kbd)
    (setq key-sequence (kbd (cadr key-sequence))))
  (el-search--check-pattern-args "keys" (list key-sequence) (lambda (x) (or (stringp x) (vectorp x)))
                                 "argument not a string or vector")
  `(pred (el-search--match-key-sequence ,key-sequence)))

(defun el-search--transform-nontrivial-lpat (expr)
  (pcase expr
    ((and (pred symbolp) (let symbol-name (symbol-name expr)))
     `(or (symbol ,symbol-name)
          `',(symbol  ,symbol-name)
          `#',(symbol ,symbol-name)))
    ((pred stringp) `(string ,expr))
    (_ expr)))

(el-search-defpattern l (&rest lpats)
  "Alternative pattern type for matching lists.
Match any list with subsequent elements matched by all LPATS in
order.

The idea is to be able to search for pieces of code (i.e. lists)
with very brief input by using a specialized syntax.

An LPAT can take the following forms:

SYMBOL  Matches any symbol S matched by SYMBOL's name interpreted
        as a regexp.  Matches also 'S and #'S for any such S.
STRING  Matches any string matched by STRING interpreted as a
        regexp
_       Matches any list element
__      Matches any number of list elements (including zero)
^       Matches zero elements, but only at the beginning of a list
$       Matches zero elements, but only at the end of a list
PAT     Anything else is interpreted as a normal pcase pattern, and
        matches one list element matched by it

^ is only valid as the first, $ as the last of the LPATS.

Example: To match defuns that contain \"hl\" in their name and
have at least one mandatory, but also optional arguments, you
could use this pattern:

    (l ^ 'defun hl (l _ &optional))"
  (let ((match-start nil) (match-end nil))
    (when (eq (car-safe lpats) '^)
      (setq match-start t)
      (cl-callf cdr lpats))
    (when (eq (car-safe (last lpats)) '$)
      (setq match-end t)
      (cl-callf butlast lpats 1))
    `(append ,@(if match-start '() '(_))
             ,@(mapcar
                (lambda (elt)
                  (pcase elt
                    ('__ '_)
                    ('_ '`(,_))
                    ('_? '(or '() `(,_))) ;FIXME: useful - document? or should we provide a (? PAT)
                                          ;thing?
                    (_ ``(,,(el-search--transform-nontrivial-lpat elt)))))
                lpats)
             ,@(if match-end '() '(_)))))

(el-search-defpattern char-prop (property)
  "Matches the object if completely covered with PROPERTY.
This pattern matches the object if its representation in the
search buffer is completely covered with the character property
PROPERTY.

This pattern always tests the complete expression in the search
buffer, it is not possible to test subexpressions calculated in
the search pattern."
  `(guard (and (get-char-property (point) ',property)
               ,(macroexp-let2 nil limit '(scan-sexps (point) 1)
                  `(= (next-single-char-property-change
                       (point) ',property nil ,limit)
                      ,limit)))))

(el-search-defpattern includes-prop (property)
  "Matches the object if partly covered with PROPERTY.
This pattern matches the object if its representation in the
search buffer is partly covered with the character property
PROPERTY.

This pattern always tests the complete expression in the search
buffer, it is not possible to test subexpressions calculated in
the search pattern."
  `(guard (or (get-char-property (point) ',property)
              ,(macroexp-let2 nil limit '(scan-sexps (point) 1)
                 `(not (= (next-single-char-property-change
                           (point) ',property nil ,limit)
                          ,limit))))))


;;;; Highlighting

(defvar-local el-search-hl-overlay nil)

(defvar-local el-search-hl-other-overlays '())

(defvar el-search-keep-hl nil)

(defun el-search-hl-sexp (&optional bounds)
  (let ((bounds (or bounds
                    (list (point) (el-search--end-of-sexp)))))
    (if (overlayp el-search-hl-overlay)
        (apply #'move-overlay el-search-hl-overlay bounds)
      (overlay-put (setq el-search-hl-overlay (apply #'make-overlay bounds))
                   'face 'el-search-match))
    (overlay-put el-search-hl-overlay 'priority 1002))
  (add-hook 'post-command-hook #'el-search-hl-post-command-fun t t))

(defun el-search--hl-other-matches-1 (pattern from to)
  (mapc #'delete-overlay el-search-hl-other-overlays)
  (setq el-search-hl-other-overlays '())
  (let ((matcher (el-search--matcher pattern))
        this-match-beg this-match-end
        (done nil))
    (save-excursion
      (goto-char from)
      (while (not done)
        (setq this-match-beg (el-search--search-pattern-1 matcher t))
        (if (not this-match-beg)
            (setq done t)
          (goto-char this-match-beg)
          (setq this-match-end (el-search--end-of-sexp))
          (let ((ov (make-overlay this-match-beg this-match-end)))
            (overlay-put ov 'face 'el-search-other-match)
            (overlay-put ov 'priority 1001)
            (push ov el-search-hl-other-overlays)
            (goto-char this-match-end)
            (when (>= (point) to) (setq done t))))))))

(defun el-search-hl-other-matches (pattern)
  "Highlight all matches visible in the selected window."
  (el-search--hl-other-matches-1 pattern
                                 (save-excursion
                                   (goto-char (window-start))
                                   (beginning-of-defun-raw)
                                   (point))
                                 (window-end))
  (add-hook 'window-scroll-functions #'el-search--after-scroll t t))

(defun el-search--after-scroll (_win start)
  (el-search--hl-other-matches-1 el-search-current-pattern
                                 (save-excursion
                                   (goto-char start)
                                   (beginning-of-defun-raw)
                                   (point))
                                 (window-end nil t)))

(defun el-search-hl-remove ()
  (when (overlayp el-search-hl-overlay)
    (delete-overlay el-search-hl-overlay))
  (remove-hook 'window-scroll-functions #'el-search--after-scroll t)
  (mapc #'delete-overlay el-search-hl-other-overlays)
  (setq el-search-hl-other-overlays '()))

(defun el-search-hl-post-command-fun ()
  (unless (or el-search-keep-hl
              (eq this-command 'el-search-query-replace)
              (eq this-command 'el-search-pattern))
    (el-search-hl-remove)
    (remove-hook 'post-command-hook 'el-search-hl-post-command-fun t)))


;;;; Core functions

;;;###autoload
(defun el-search-pattern (pattern &optional no-error)
  "Start new or resume last elisp search.

Search current buffer for expressions that are matched by `pcase'
PATTERN.  Use `read' to transform buffer contents into
expressions.

Use `emacs-lisp-mode' for reading input.  Some keys in the
minibuffer have a special binding: to make it possible to edit
multi line input, C-j inserts a newline, and up and down move the
cursor vertically - see `el-search-read-expression-map' for more
details.


Additional `pcase' pattern types to be used with this command can
be defined with `el-search-defpattern'.

The following additional pattern types are currently defined:"
  (interactive (list (if (and (eq this-command last-command)
                              el-search-success)
                         el-search-current-pattern
                       (let* ((input (el-search--read-pattern "Find pcase pattern: "
                                                              (car el-search-history)))
                              (pattern (read input)))
                         ;; A very common mistake: input "foo" instead of "'foo"
                         (when (and (symbolp pattern)
                                    (not (eq pattern '_))
                                    (or (not (boundp pattern))
                                        (not (eq (symbol-value pattern) pattern))))
                           (error "Please don't forget the quote when searching for a symbol"))
                         ;; Make input available also in query-replace history
                         (el-search--pushnew-to-history input 'el-search-query-replace-history)
                         ;; and wrap the PATTERN
                         (el-search--wrap-pattern pattern)))))
  (if (not (called-interactively-p 'any))
      (el-search--search-pattern pattern no-error)
    (setq this-command 'el-search-pattern) ;in case we come from isearch
    (setq el-search-current-pattern pattern)
    (let ((opoint (point)))
      (when (and (eq this-command last-command) el-search-success)
        (el-search--skip-expression nil t))
      (setq el-search-success nil)
      (when (condition-case nil
                (el-search--search-pattern pattern)
              (end-of-buffer (message "No match")
                             (goto-char opoint)
                             (el-search-hl-remove)
                             (ding)
                             nil))
        (setq el-search-success t)
        (el-search-hl-sexp)
        (unless (eq this-command last-command)
          (el-search-hl-other-matches pattern))))))

(defvar el-search-search-and-replace-help-string
  "\
y         Replace this match and move to the next.
SPC or n  Skip this match and move to the next.
r         Replace this match but don't move.
!         Replace all remaining matches automatically.
q         Quit.  To resume, use e.g. `repeat-complex-command'.
?         Show this help.
s         Toggle splicing mode.  When splicing mode is
          on (default off), the replacement expression must
          evaluate to a list, and the result is spliced into the
          buffer, instead of just inserted.

Hit any key to proceed."
  "Help string for ? in `el-search-query-replace'.")

(defun el-search--search-and-replace-pattern (pattern replacement &optional splice to-input-string)
  (let ((replace-all nil) (nbr-replaced 0) (nbr-skipped 0) (done nil)
        (el-search-keep-hl t) (opoint (point))
        (get-replacement (el-search--matcher pattern replacement))
        (skip-matches-in-replacement 'ask))
    (unwind-protect
        (while (and (not done) (el-search--search-pattern pattern t))
          (setq opoint (point))
          (unless replace-all
            (el-search-hl-sexp)
            (unless (eq this-command last-command)
              (el-search-hl-other-matches pattern)))
          (let* ((region (list (point) (el-search--end-of-sexp)))
                 (original-text (apply #'buffer-substring-no-properties region))
                 (expr      (read original-text))
                 (replaced-this nil)
                 (new-expr  (funcall get-replacement expr))
                 (get-replacement-string
                  (lambda () (el-search--format-replacement new-expr original-text to-input-string splice)))
                 (to-insert (funcall get-replacement-string))
                 (replacement-contains-another-match
                  (with-temp-buffer
                    (emacs-lisp-mode)
                    (insert to-insert)
                    (goto-char 1)
                    (el-search--skip-expression new-expr)
                    (condition-case nil
                        (progn (el-search--ensure-sexp-start)
                               (el-search--search-pattern pattern t))
                      (end-of-buffer nil))))
                 (do-replace
                  (lambda ()
                    (save-excursion
                      (save-restriction
                        (widen)
                        (el-search--replace-hunk (list (point) (el-search--end-of-sexp)) to-insert)))
                    (el-search--ensure-sexp-start) ;skip potentially newly added whitespace
                    (el-search-hl-sexp (list opoint (point)))
                    (cl-incf nbr-replaced)
                    (setq replaced-this t))))
            (if replace-all
                (funcall do-replace)
              (while (not (pcase (if replaced-this
                                     (read-char-choice "[SPC ! q]  (? for help)"
                                                       '(?\ ?! ?q ?\C-g ?n ??))
                                   (read-char-choice
                                    (concat "Replace this occurrence"
                                            (if (or (string-match-p "\n" to-insert)
                                                    (< 40 (length to-insert)))
                                                "" (format " with `%s'" to-insert))
                                            "? "
                                            (if splice "{splice} " "")
                                            "[y SPC r ! s q]  (? for help)" )
                                    '(?y ?n ?r ?\ ?! ?q ?\C-g ?s ??)))
                            (?r (funcall do-replace)
                                nil)
                            (?y (funcall do-replace)
                                t)
                            ((or ?\ ?n)
                             (unless replaced-this (cl-incf nbr-skipped))
                             t)
                            (?! (unless replaced-this
                                  (funcall do-replace))
                                (setq replace-all t)
                                t)
                            (?s (cl-callf not splice)
                                (setq to-insert (funcall get-replacement-string))
                                nil)
                            ((or ?q ?\C-g)
                             (setq done t)
                             t)
                            (?? (ignore (read-char el-search-search-and-replace-help-string))
                                nil)))))
            (unless (or done (eobp))
              (cond
               ((not (and replaced-this replacement-contains-another-match))
                (el-search--skip-expression nil t))
               ((eq skip-matches-in-replacement 'ask)
                (if (setq skip-matches-in-replacement
                          (yes-or-no-p "Match in replacement - always skip? "))
                    (forward-sexp)
                  (el-search--skip-expression nil t)
                  (when replace-all
                    (setq replace-all nil)
                    (message "Falling back to interactive mode")
                    (sit-for 3.))))
               (skip-matches-in-replacement (forward-sexp))
               (t
                (el-search--skip-expression nil t)
                (message "Replacement contains another match%s"
                         (if replace-all " - falling back to interactive mode" ""))
                (setq replace-all nil)
                (sit-for 2.)))))))
    (el-search-hl-remove)
    (goto-char opoint)
    (message "Replaced %d matches%s"
             nbr-replaced
             (if (zerop nbr-skipped)  ""
               (format "   (%d skipped)" nbr-skipped)))))

(defun el-search-query-replace--read-args ()
  (barf-if-buffer-read-only)
  (let ((from-input (let ((el-search--initial-mb-contents
                           (or el-search--initial-mb-contents
                               (and (eq last-command 'el-search-pattern)
                                    (car el-search-history)))))
                      (el-search--read-pattern "Query replace pattern: " nil
                                               'el-search-query-replace-history)))
        from to)
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert from-input)
      (goto-char 1)
      (forward-sexp)
      (skip-chars-forward " \t\n")
      ;; FIXME: maybe more sanity tests here...
      (if (not (looking-at "->"))
          (setq from from-input
                to (let ((el-search--initial-mb-contents nil))
                     (el-search--read-pattern "Replace with result of evaluation of: " from)))
        (delete-char 2)
        (goto-char 1)
        (forward-sexp)
        (setq from (buffer-substring 1 (point)))
        (skip-chars-forward " \t\n")
        (setq to (buffer-substring (point) (progn (forward-sexp) (point))))))
    (unless (and el-search-query-replace-history
                 (not (string= from from-input))
                 (string= from-input (car el-search-query-replace-history)))
      (push (with-temp-buffer
              (emacs-lisp-mode)
              (insert (let ((newline-in-from (string-match-p "\n" from))
                            (newline-in-to   (string-match-p "\n" to)))
                        (format "%s%s%s ->%s%s"
                                (if (and (or newline-in-from newline-in-to)
                                         (not (string-match-p "\\`\n" from))) "\n" "")
                                (if     newline-in-from                       "\n" "" ) from
                                (if (and (or newline-in-from newline-in-to)
                                         (not (string-match-p "\\`\n" to)))   "\n" " ") to)))
              (indent-region 1 (point-max))
              (buffer-string))
            el-search-query-replace-history))
    (el-search--pushnew-to-history from 'el-search-history)
    (list (el-search--wrap-pattern (read from)) (read to) to)))

;;;###autoload
(defun el-search-query-replace (from-pattern to-expr &optional textual-to)
  "Replace some matches of \"el-search\" pattern FROM-PATTERN.

TO-EXPR is an Elisp expression that is evaluated repeatedly for
each match with bindings created in FROM-PATTERN in effect to
produce a replacement expression.  Operate from point
to (point-max).

As each match is found, the user must type a character saying
what to do with it.  For directions, type ? at that time.

As an alternative to enter FROM-PATTERN and TO-EXPR separately,
you can also give an input of the form

   FROM-PATTERN -> TO-EXPR

to the first prompt and specify both expressions at once.  This
format is also used for history entries."
  (interactive (el-search-query-replace--read-args))
  (setq this-command 'el-search-query-replace) ;in case we come from isearch
  (setq el-search-current-pattern from-pattern)
  (barf-if-buffer-read-only)
  (el-search--search-and-replace-pattern from-pattern to-expr nil textual-to))

(defun el-search--take-over-from-isearch (&optional goto-left-end)
  (let ((other-end (and goto-left-end isearch-other-end))
        (input isearch-string))
    (isearch-exit)
    (when (and other-end (< other-end (point)))
      (goto-char other-end))
    input))

;;;###autoload
(defun el-search-search-from-isearch ()
  ;; FIXME: an interesting alternative would be to really integrate it
  ;; with Isearch, using `isearch-search-fun-function'.
  ;; Alas, this is not trivial if we want to transfer our optimizations.
  (interactive)
  (let ((el-search--initial-mb-contents (concat "'" (el-search--take-over-from-isearch))))
    ;; use `call-interactively' so we get recorded in `extended-command-history'
    (call-interactively #'el-search-pattern)))

;;;###autoload
(defun el-search-replace-from-isearch ()
  (interactive)
  (let ((el-search--initial-mb-contents (concat "'" (el-search--take-over-from-isearch t))))
    (call-interactively #'el-search-query-replace)))



(provide 'el-search)
;;; el-search.el ends here
