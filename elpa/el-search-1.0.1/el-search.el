;;; el-search.el --- Expression based interactive search for emacs-lisp-mode -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 29 Jul 2015
;; Keywords: lisp
;; Compatibility: GNU Emacs 25
;; Version: 1.0.1
;; Package-Requires: ((emacs "25") (stream "2.2.3"))


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

;; Suggested key bindings
;; ======================
;;
;; You can eval the following key definitions to try things out while
;; reading this introduction.  These are the bindings I use
;; personally:
;;
;;   (define-key emacs-lisp-mode-map [(control ?S)] #'el-search-pattern)
;;   (define-key emacs-lisp-mode-map [(control ?%)] #'el-search-query-replace)
;;   (define-key global-map          [(control ?J)] #'el-search-jump-to-search-head)
;;   (define-key global-map          [(control ?N)] #'el-search-continue-in-next-buffer)
;;
;;   (define-key el-search-read-expression-map [(control ?S)] #'exit-minibuffer)
;;
;;   (define-key isearch-mode-map [(control ?S)] #'el-search-search-from-isearch)
;;
;;   (with-eval-after-load 'dired
;;     (define-key dired-mode-map [(control ?S)] #'el-search-dired-marked-files))
;;
;; These bindings may not work in a console (if you have an idea for
;; official bindings that fit better into the Emacs ecosystem, please
;; mail me).
;;
;; The binding in `isearch-mode-map' lets you switch to "el-search"
;; from isearch reusing already the given input.  The binding in
;; `el-search-read-expression-map' allows you to hit C-S twice to
;; start a search using the last search pattern, similar to isearch.
;;
;; Don't be afraid of the long introduction, it's only verbose (sorry)
;; but not complicated.
;;
;;
;; Introduction
;; ============
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
;; When searching, it doesn't matter how code is actually formatted.
;; Comments are ignored, and strings are treated as atomic objects,
;; their contents are not being searched.
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
;; The following pattern will search for defvar forms with a docstring
;; whose first line is longer than 70 characters:
;;
;;   `(defvar ,_ ,_
;;      ,(and (pred stringp) s
;;            (guard (< 70 (length (car (split-string s "\n")))))))
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
;; If you wanted to search a buffer for symbols that are defined in
;; "cl-lib", you could use this pattern
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
;; own `pcase' pattern types for the purpose of el-searching with
;; `el-search-defpattern'.  It is just like `pcase-defmacro', but the
;; effect is limited to this package (i.e. it uses a separate name
;; space).  See C-h f `el-search-pattern' for a list of predefined
;; pattern forms.
;;
;; Some additional pattern definitions can be found in the file
;; "el-search-x" which is part of this package.
;;
;;
;; Replacing
;; =========
;;
;; You can replace expressions with command `el-search-query-replace'.
;; You are queried for a (pcase) pattern and a replacement expression.
;; For each match of the pattern, the replacement expression is
;; evaluated with the bindings created by the pcase matching in
;; effect, and printed to a string to produce the replacement.
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
;; Multi Searching
;; ===============
;;
;; "el-search" is capable of performing "multi searches" - searches
;; spanning multiple files or buffers.  When no more matches can be
;; found in the current buffer, the search automatically switches to
;; the next buffer.  Examples for search commands starting a multi
;; search are `el-search-buffers' (search all living elisp mode
;; buffers), `el-search-directory' (search all elisp files in a
;; specified directory), `el-search-emacs-elisp-sources' (search all
;; Emacs elisp sources) and `el-search-dired-marked-files'.  Actually,
;; every search is internally a multi search.
;;
;; You can pause any (multi) search by just doing something different,
;; the state of the search is automatically saved. You can continue
;; searching by calling `el-search-jump-to-search-head': this command
;; jumps to the last match and re-activates the search.

;; `el-search-continue-in-next-buffer' skips all remaining matches in
;; the current buffer and continues searching in the next buffer.
;;
;; Matches found in the current buffer are recorded; use
;; `el-search-previous-match' to revisit them in reverse order (this
;; is actually the poor-man's version of a backward search, since a
;; real backward el-search would be slow).
;;
;; There is no multi query-replace currently implemented; I don't know
;; if it would be that useful as a separate command anyway.  If you
;; want to query-replace in multiple buffers or files, call an
;; appropriate multi-search command, and every time a first match is
;; found in any buffer, start an ordinary `el-search-query-replace';
;; after finishing, check that everything is ok, save etc, and resume
;; the multi search with one of the above commands.
;;
;; There is currently nothing like `occur' for el-search.  However,
;; you can get a list of matches in the form
;; (file-name-or-buffer . match-position) with
;;
;;    (el-search-all-matches
;;      (el-search-make-search (el-search--matcher pattern) stream))
;;
;; where PATTERN is the search pattern and STREAM is a stream of
;; buffers or files.  For example,
;;
;;   (el-search-all-matches
;;    (el-search-make-search
;;     (el-search--matcher ''require)
;;     (seq-filter
;;      (lambda (buffer)
;;         (with-current-buffer buffer (derived-mode-p 'emacs-lisp-mode)))
;;      (stream (buffer-list)))))
;;
;; would return a list of matches for the symbol require in all elisp
;; mode buffers.
;;
;;
;; Multiple multi searches
;; =======================
;;
;; Every search is collected in a history.  You can resume older
;; searches from the position of the last match by calling
;; `el-search-jump-to-search-head' with a prefix argument.  That let's
;; you select an older search to resume and switches to the buffer and
;; position where this search had been suspended.
;;
;; There is no special command to restart a prior search from the
;; beginning.  I suggest to use `repeat-complex-command'.
;;
;;
;; Writing replacement rules for semi-automatic code rewriting
;; ===========================================================
;;
;; When you want to rewrite larger code parts programmatically, it is
;; often useful to define dedicated patterns for performing the
;; replacement.  Here is an example:
;;
;; You heard that in many situations, `dolist' is faster than an
;; equivalent `mapc'.  You use `mapc' quite often in your code and
;; want to query-replace many occurrences in your stuff.  Instead of
;; using an ad hoc replacing rule, it's cleaner to define a dedicated
;; named pattern using `el-search-defpattern'.  Make this pattern
;; accept an argument and use this argument to bind the replacement
;; expression to a variable you specify.  In our case, the pattern
;; could look like this:
;;
;;   (el-search-defpattern el-search-mapc->dolist (new)
;;     (let ((var  (make-symbol "var"))
;;           (body (make-symbol "body"))
;;           (list (make-symbol "list")))
;;       `(and `(mapc (lambda (,,var) . ,,body) ,,list)
;;             (let ,new `(dolist (,,var ,,list) . ,,body)))))
;;
;; The first condition in the `and' performs the matching and binds
;; the essential parts of the `mapc' form to variables.  The second,
;; the `let' part, binds the pattern specified argument NEW (as said,
;; typically just a variable to receive the rewritten code) to the
;; rewritten expression - in our case, a `dolist' form is constructed
;; with the remembered code parts filled in.
;;
;; Now, in `el-search-query-replace', you just specify the following
;; rule:
;;
;;   (el-search-mapc->dolist replacement) -> replacement
;;
;; And when you want to replace in multiple buffers or files, call an
;; appropriate multi el-search command, e.g. `el-search-directory',
;; and specify
;;
;;   (el-search-mapc->dolist replacement)
;;
;; as search pattern.
;;
;;
;;
;; Bugs, Known Limitations
;; =======================
;;
;; - Replacing: in some cases the reader syntax of forms is changing
;; due to reading+printing.  "Some" because we can handle this problem
;; in most cases.
;;
;; - Similarly: Comments are normally preserved (where it makes
;; sense).  But when replacing like `(foo ,a ,b) -> `(foo ,b ,a)
;;
;; in a content like
;;
;;   (foo
;;     a
;;     ;; comment
;;     b)
;;
;; the comment will be lost.
;;
;;
;;  Acknowledgments
;;  ===============
;;
;; Thanks to Stefan Monnier for corrections and advice.
;;
;;
;;  Notes for developers
;;  ====================
;;
;; We use the following model for multi buffer/file searches: a search
;; (object) is represented by a struct "el-search-object" that
;; contains a stream of matches M and a search head object H that
;; contains a compiled matcher according to the given search pattern,
;; the buffer and buffer position where to continue searching, and the
;; stream of buffers or files yet to search.
;;
;; When elements are requested from M, H is updated accordingly.  H
;; can be manipulated directly to influence how M will find further
;; elements when requested (useful for skipping buffers on the fly).
;;
;;
;; TODO:
;;
;; - The default keys are not available in the terminal
;;
;; - Handle buffers killed/files closed when resuming a search
;;
;; - Make el-search-previous-match behave correctly when a buffer has
;;   been modified and data is outdated
;;
;; - Make the non-command `el-search-forward' accept an &optional
;;   LIMIT argument
;;
;; - Make searching work in comments, too? (->
;;   `parse-sexp-ignore-comments').  Related: should the pattern
;;   `symbol' also match strings that contain matches for a symbol so
;;   that it's possible to replace also occurrences of a symbol in
;;   docstrings?
;;
;; - Implement an occur like interface?
;;
;; - Port this to non Emacs Lisp modes?  How?  Would it already
;;   work using only syntax tables, sexp scanning and font-lock?
;;
;; - For query-replace, maybe we should save the original buffer
;;   string in a buffer-local variable, and make that ediff'able
;;   against the new version.  Or should we even save multiple
;;   versions when appropriate?
;;
;; - Replace: pause and warn when replacement might be wrong
;;   (ambiguous reader syntaxes; lost comments, comments that can't
;;   non-ambiguously be assigned to rewritten code)




;;; Code:

;;;; Requirements

(eval-when-compile
  (require 'subr-x))

(require 'cl-lib)
(require 'elisp-mode)
(require 'thingatpt)
(require 'stream)
(require 'help-fns) ;el-search--make-docstring
(require 'ring)     ;el-search-history


;;;; Configuration stuff

(defgroup el-search nil
  "Expression based search and replace for `emacs-lisp-mode'."
  :group 'lisp)

(defcustom el-search-this-expression-identifier 'exp
  "Identifier ID referring to the current expression in pattern input.
When entering a pattern in an interactive \"el-search\" command,
the pattern actually used will be (and ID PATTERN).  The default
value is `exp'."
  :type 'symbol)

(defface el-search-match '((((background dark))
                            ;; (:background "#0000A0")
                            (:background "#600000"))
			   (t (:background "DarkSlateGray3")))
  "Face for highlighting the current match.")

(defface el-search-other-match '((((background dark))
                                  ;; (:background "#202060")
                                  (:background "#603030"))
                                 (t (:background "DarkSlateGray1")))
  "Face for highlighting the other matches.")

(defvar el-search-display-buffer-action
  '((display-buffer-reuse-window display-buffer-pop-up-frame)
    (reusable-frames . visible))
  "The `display-buffer' action used by `el-search-jump-to-search-head'.")

(defcustom el-search-ignored-directory-regexps
  (mapcar
   (lambda (name) (format "\\`%s\\'" (regexp-quote name)))
   ;; this is just the default value of `grep-find-ignored-directories':
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}"))
  "List of regexps defining directories that el-search should ignore.

The value influences the behavior of the commands that perform
directory searches like `el-search-directory' or
`el-search-dired-marked-files'.  It is consulted by all streams
`el-search-stream-of-directory-files' returns."
  :type '(choice (repeat :tag "Ignored directories" regexp)
		 (const  :tag "No ignored directories" nil)))

(defvar el-search-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control ?D)] #'el-search-skip-directory)
    (define-key map [(control ?N)] #'el-search-continue-in-next-buffer)
    (define-key map [(control ?R)] #'el-search-previous-match)
    map)
  "Used by search commands as transient-map.")

(defvar el-search-read-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map read-expression-map)
    (define-key map [(control ?g)] #'abort-recursive-edit)
    (define-key map [up]   nil)
    (define-key map [down] nil)
    (define-key map [(control ?j)] #'newline)
    map)
  "Map for reading input with `el-search-read-expression'.")


;;;; Helpers and Definitions

(defvar el-search--current-pattern nil
  "Current search pattern or nil when no search in progress.")

(defvar el-search--current-search nil
  "The currently active search, an `el-search-object'.")

(defvar el-search--current-matcher nil
  "The matcher according to `el-search--current-pattern'.")

(defvar-local el-search--temp-buffer-flag nil
  "Non-nil tags (file) buffers as temporarily opened for searching.")

(defvar el-search--success nil
  "Non-nil when last search command was successful.")

(defvar-local el-search--this-buffer-matches (stream-empty)
  "Stream of the matches that were found in current buffer."
  ;; Internally, simply the substream of the matches of the current
  ;; search with all matches before current buffer cut off
  )


(defun el-search--string-match-p (eregexp string)
  "Non-nil when extended regexp EREGEXP matches the STRING."
  (let ((match-bindings ()) regexp)
    (pcase eregexp
      ((pred stringp) (setq regexp eregexp))
      (`(,binds ,real-regexp)
       (setq regexp real-regexp)
       (setq match-bindings binds)))
    (setq match-bindings
          (mapcar (lambda (binding)
                    (pcase binding
                      ((pred symbolp)                  (list binding nil))
                      (`(,(and (pred symbolp) symbol)) (list symbol nil))
                      (_                               binding)))
                  match-bindings))
    (cl-progv (mapcar #'car match-bindings) (mapcar #'cadr match-bindings)
      (string-match-p regexp string))))

(defun el-search--pp-to-string (expr)
  (let ((print-length nil)
        (print-level nil))
    (pp-to-string expr)))

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

(defun el-search-read-expression (prompt &optional initial-contents hist default read)
  "Conveniently read an expression from the minibuffer."
  (minibuffer-with-setup-hook #'el-search--setup-minibuffer
    (read-from-minibuffer prompt initial-contents el-search-read-expression-map read
                          (or hist 'read-expression-history) default)))

(defvar el-search-pattern-history ()
  "List of search pattern input strings.")

(defvar el-search-history (make-ring 10) ;$$$$FIXME: Make `10' customizable?
  "History of previous searches."
  ;; Elements have the form (search-object pattern)
  )

(defvar el-search-query-replace-history ()
  "List of input strings from `el-search-query-replace'.")

(defvar el-search--initial-mb-contents nil)

(defun el-search--pushnew-to-history (input histvar)
  ;; Push INPUT to history in HISTVAR unless it's already "the same" as
  ;; the history's head element
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

(defun el-search--maybe-warn-about-unquoted-symbol (pattern)
  (when (and (symbolp pattern)
             (not (eq pattern '_))
             (or (not (boundp pattern))
                 (not (eq (symbol-value pattern) pattern))))
    (error "Unbound symbol: %S" pattern)))

(defun el-search--read-pattern (prompt &optional default histvar)
  (cl-callf or histvar 'el-search-pattern-history)
  (let ((input (el-search-read-expression prompt el-search--initial-mb-contents histvar default)))
    (el-search--pushnew-to-history input histvar)
    (if (not (string= input "")) input (car (symbol-value histvar)))))

(defun el-search--read-pattern-for-interactive ()
  (let* ((input (el-search--read-pattern "Find pcase pattern: " (car el-search-pattern-history)))
         (pattern (read input)))
    ;; A very common mistake: input "foo" instead of "'foo"
    (el-search--maybe-warn-about-unquoted-symbol pattern)
    (setq this-command 'el-search-pattern) ;in case we come from isearch
    ;; Make input available also in query-replace history
    (el-search--pushnew-to-history input 'el-search-query-replace-history)
    pattern))


(defun el-search--end-of-sexp ()
  ;; Assumes point is at sexp beginning
  (or (scan-sexps (point) 1) (point-max)))

(defun el-search--skip-expression (expression &optional read)
  ;; Move forward at least one character.  Don't move into a string or
  ;; comment.  Don't move further than the beginning of the next sexp.
  ;; Try to move as far as possible under these conditions.  Point must
  ;; be at the beginning of an expression.  If there are positions where
  ;; `read' would succeed, but that do not represent a valid sexp start,
  ;; move past them (e.g. when before "#'" move past both characters).
  ;;
  ;; EXPRESSION must equal the (read) expression at point, but with READ
  ;; non-nil, ignore the first argument and use the read expression at
  ;; point instead.
  (when read (setq expression (save-excursion (read (current-buffer)))))
  (cond
   ((eq '@ expression) ;bug#24542
    (forward-char))
   ((or (null expression)
        (equal [] expression)
        (not (or (listp expression) (vectorp expression))))
    (goto-char (el-search--end-of-sexp)))
   ((looking-at (rx (or ",@" "," "#'" "'")))
    (goto-char (match-end 0)))
   (t (forward-char))))

(defun el-search--ensure-sexp-start ()
  "Move point to the next sexp beginning position.
Do nothing if already at beginning of a sexp.  `read' the
expression at that point and return it.  Point must not be inside
a string or comment."
  ;; We donn't catch end-of-buffer to keep the return value
  ;; non-ambiguous
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

(defun el-search--make-docstring (name)
  ;; code mainly from `pcase--make-docstring'
  (let* ((main (documentation (symbol-function name) 'raw))
         (ud (help-split-fundoc main name)))
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

(defmacro el-search-defpattern (name args &rest body)
  "Like `pcase-defmacro', but limited to el-search patterns.
The semantics is exactly that of `pcase-defmacro', but the scope
of the definitions is limited to \"el-search\", using a separate
name space.

\(fn NAME ARGLIST &optional DOCSTRING &rest BODY)"
  (declare (indent 2) (debug defun))
  `(setf (alist-get ',name el-search--pcase-macros)
         (lambda ,args ,@body)))

(defmacro el-search--with-additional-pcase-macros (&rest body)
  `(cl-letf ,(mapcar (pcase-lambda (`(,symbol . ,fun)) `((get ',symbol 'pcase-macroexpander) #',fun))
                     el-search--pcase-macros)
     ,@body))

(defun el-search--macroexpand-1 (pattern)
  "Expand \"el-search\" PATTERN.
This is like `pcase--macroexpand' but expands only patterns
defined with `el-search-defpattern' and performs only one
expansion step.

Return PATTERN if this pattern type was not defined with
`el-search-defpattern'."
  (if-let ((expander (alist-get (car-safe pattern) el-search--pcase-macros)))
      (apply expander (cdr pattern))
    pattern))

(defun el-search--macroexpand (pattern)
  "Like `pcase--macroexpand' but also expanding \"el-search\" patterns."
  (eval `(el-search--with-additional-pcase-macros (pcase--macroexpand ',pattern))))

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


(defun el-search--search-pattern-1 (matcher &optional noerror)
  (if (not (derived-mode-p 'emacs-lisp-mode))
      (if noerror nil (error "Buffer not in emacs-lisp-mode: %s" (buffer-name)))
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
          (if noerror nil (signal 'end-of-buffer nil))
        match-beg))))

(defun el-search-forward (pattern &optional noerror)
  "Search for el-search PATTERN in current buffer from point.
Set point to the beginning of the occurrence found and return the
new value of point.  Optional second argument, if non-nil, means
if fail just return nil (no error)."
  (el-search--search-pattern-1 (el-search--matcher pattern) noerror))


(defun el-search-defpattern--check-args (type args predicate &optional message)
  "Check whether all ARGS fulfill PREDICATE.
Raise an error if not.  The string arguments TYPE and optional
MESSAGE are used to construct the error message."
  (dolist (arg args)
    (unless (funcall predicate arg)
      (error (concat "Pattern `%s': "
                     (or message (format "argument doesn't fulfill %S" predicate))
                     ": %S")
             type arg))))

(defun el-search--elisp-file-name-p (file)
  (and (string-match-p "\\.el\\'" file)
       (file-exists-p file)
       (not (file-directory-p file))))

(cl-defstruct el-search-object
  head       ;an `el-search-head' instance, modified ("moved") while searching
  matches    ;the stream of matches in the form (buffer position file)
  last-match ;position of last match found
  )

(cl-defstruct el-search-head
  matcher  ;for the current search pattern
  buffer   ;currently searched buffer, or nil meaning "continue in next buffer"
  file     ;name of currently searched file, or nil
  position ;where to continue search in this buffer
  buffers  ;stream of buffers and/or files yet to search
  )

(defun el-search-kill-left-over-search-buffers (&optional not-current-buffer)
  "Kill all buffers that were opened for searching."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer el-search--temp-buffer-flag)
      (unless (or (buffer-modified-p buffer)
                  (and not-current-buffer (eq buffer (current-buffer))))
        (kill-buffer buffer)))))

(defvar warning-minimum-level)
(defun el-search--next-buffer (search &optional predicate)
  ;; Prepare to continue SEARCH in the next buffer in line.  Move
  ;; SEARCH's head accordingly.  When specified, PREDICATE should accept
  ;; a file name or a buffer, and we skip all buffers and files not
  ;; fulfilling it.  Return the new buffer to search in or nil if done.
  (el-search-hl-remove)
  (el-search-kill-left-over-search-buffers t)
  (let ((head (el-search-object-head search)))
    (let ((buffer-stream (el-search-head-buffers head))
          (buffer-list-before (buffer-list))
          (done nil)  next  buffer)
      (while (and (not done) (not (stream-empty-p buffer-stream)))
        (setq next          (stream-first buffer-stream)
              buffer-stream (stream-rest buffer-stream)
              done          (or (not predicate) (funcall predicate next))))
      (if (not done)
          (progn
            (setf (el-search-head-buffer   head) nil
                  (el-search-head-buffers  head) buffer-stream) ;i.e. the empty stream
            nil)
        (setf (el-search-head-buffers  head) buffer-stream
              (el-search-head-position head) 1)
        (if (bufferp next)
            (setq buffer next)
          (setf (el-search-head-file head) next)
          (setq buffer (let ((warning-minimum-level :error)) (find-file-noselect next))))
        (unless (memq buffer buffer-list-before)
          (with-current-buffer buffer
            (setq-local el-search--temp-buffer-flag t)))
        (setf (el-search-head-buffer head) buffer)
        (when (bufferp buffer)
          (with-current-buffer buffer
            (setq-local el-search--this-buffer-matches
                        (el-search-object-matches search))
            (when (and (buffer-narrowed-p)
                       (y-or-n-p (format "Widen buffer \"%s\"? "
                                         (buffer-name))))
              ;;FIXME: Is this always appropriate?
              (widen))))
        buffer))))

(defun el-search--skip-to-next-buffer (&optional predicate)
  ;; Find next buffer fulfilling the PREDICATE and continue search there
  (el-search--next-buffer el-search--current-search predicate)
  (el-search-continue-search))

(defun el-search-make-search (matcher stream)
  "Create and return a new `el-search-object' instance.
MATCHER is the result of calling `el-search--matcher' on the
pattern to search.  STREAM is a stream of buffers and/or files to
search."
  (let ((head (make-el-search-head :matcher matcher :buffers stream)))
    (letrec ((search
              (make-el-search-object
               :head head
               :matches
               (seq-filter
                #'identity ;we use `nil' as a "skip" tag
                (funcall
                 (letrec ((get-stream
                           (lambda ()
                             (stream-make
                              (if-let ((buffer (or (el-search-head-buffer head)
                                                   (el-search--next-buffer search))))
                                  (with-current-buffer buffer
                                    (save-excursion
                                      ;; Widening already happens in `el-search-continue-search'
                                      (goto-char (el-search-head-position head))
                                      (message "Searching in %s"
                                               (or (el-search-head-file head)
                                                   (el-search-head-buffer head)))
                                      (if-let ((match (el-search--search-pattern-1
                                                       (el-search-head-matcher head) t)))
                                          (progn
                                            (setf (el-search-object-last-match search)
                                                  (copy-marker (point)))
                                            (el-search--skip-expression nil t)
                                            (setf (el-search-head-position head)
                                                  (copy-marker (point)))
                                            (cons
                                             (list (el-search-head-buffer head)
                                                   match
                                                   (el-search-head-file head))
                                             (funcall get-stream)))
                                        (setf (el-search-head-buffer head) nil
                                              (el-search-head-file   head) nil)
                                        (el-search--next-buffer search)
                                        ;; retry with the next buffer
                                        (cons nil (funcall get-stream)))))
                                ;; end of stream (no buffers left to search in)
                                nil)))))
                   get-stream))))))
      search)))

(defun el-search-setup-search (pattern stream &optional from-here)
  "Create and start a new search.
PATTERN is the search pattern.  STREAM is a stream of buffers
and/or files (i.e. file names) to search in.

With optional FROM-HERE non-nil, the first buffer in STREAM
should be the current buffer, and searching will start at the
current buffer's point instead of its beginning."
  (setq el-search--success nil)
  (let ((matcher (el-search--matcher (el-search--wrap-pattern pattern))))
    (setq el-search--current-search (el-search-make-search matcher stream))
    (setq el-search--current-matcher matcher))
  (setq el-search--current-pattern pattern)
  (ring-insert el-search-history (list el-search--current-search pattern))
  (when from-here (setq el-search--temp-buffer-flag nil))
  (el-search-continue-search from-here))

(defun el-search-stream-of-directory-files (&optional directory recurse)
  "Return a stream of emacs-lisp files in DIRECTORY.
DIRECTORY defaults to `default-directory'.  The returned stream
will recurse into DIRECTORY's subdirectories when RECURSE is
non-nil unless their name is matched by one of the
`el-search-ignored-directory-regexps'."
  (stream-of-directory-files
   (or directory default-directory)
   t nil
   (and recurse
        (lambda (dir-name)
          (not (cl-some (lambda (regexp) (string-match-p regexp dir-name))
                        el-search-ignored-directory-regexps))))
   t #'el-search--elisp-file-name-p))


;;;; Additional pattern type definitions

(defun el-search--eregexp-p (thing)
  ;; Return non-nil when THING is an "extended regexp" in the sense of
  ;; the "string" pattern type
  (pcase thing
    ((pred stringp) t)
    (`(,(and (pred listp) bindings)
       ,(pred stringp))
     (cl-every
      (lambda (binding) (pcase binding ((or (pred symbolp) `(,(pred symbolp)) `(,(pred symbolp) ,_)) t)))
      bindings))))

(el-search-defpattern string (&rest regexps)
  "Matches any string that is matched by all REGEXPS.

Any of the REGEXPS can also be an \"extended\" regexp of the form

  \(bindings regexp\)

where REGEXP is the actual regexp to match and BINDINGS is a
let-style list of variable bindings.

Example: (((case-fold-search nil)) \"foo\") is an extended regexp
matching \"foo\", but not \"Foo\" even when `case-fold-search' is
currently enabled."
  (el-search-defpattern--check-args "string" regexps #'el-search--eregexp-p
                                    "argument not a regexp")
  `(and (pred stringp)
        ,@(mapcar (lambda (thing) `(pred (el-search--string-match-p ',thing)))
                  regexps)))

(el-search-defpattern symbol (&rest regexps)
  "Matches any symbol whose name is matched by all REGEXPS.
Any of the REGEXPS can be an extended regexp of the form
\(bindings regexp\) like in the \"string\" pattern."
  (el-search-defpattern--check-args "symbol" regexps #'el-search--eregexp-p
                                    "argument not a regexp")
  `(and (pred symbolp) (app symbol-name (string ,@regexps))))

(defun el-search--contains-p (matcher exp)
  "Return non-nil when expression tree EXP contains a match for MATCHER.
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
  "Matches expressions that contain a match for all PATTERNs.

Example:

  \(contains (string \"H\") 17)

matches

  \((\"Hallo\") x (5 [1 17])).

The expression itself is included, so for example `1' is matched
by \(contains 1\)."
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

(defun el-search--match-symbol-file (eregexp symbol)
  (when-let ((symbol-file (and (symbolp symbol) (symbol-file symbol))))
    (el-search--string-match-p
     eregexp
     (file-name-sans-extension (file-name-nondirectory symbol-file)))))

(el-search-defpattern symbol-file (regexp)
  "Matches any symbol whose `symbol-file' is matched by REGEXP.

This pattern matches when the object is a symbol for that
`symbol-file' returns a (non-nil) FILE-NAME so that

   (file-name-sans-extension (file-name-nondirectory FILENAME)))

is matched by the REGEXP."
  (el-search-defpattern--check-args "symbol-file" (list regexp) #'el-search--eregexp-p
                                    "argument not a regexp")
  `(pred (el-search--match-symbol-file ',regexp)))

(el-search-defpattern char-prop (property)
  "Matches the object if completely covered with PROPERTY.
This pattern matches the object if its representation in the
search buffer is completely covered with the character property
PROPERTY.

This pattern should only be used to match against the current
object (so it can't be used inside an `app' pattern for
example)."
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

This pattern should only be used to match against the current
object (so it can't be used inside an `app' pattern for
example)."
  `(guard (or (get-char-property (point) ',property)
              ,(macroexp-let2 nil limit '(scan-sexps (point) 1)
                 `(not (= (next-single-char-property-change
                           (point) ',property nil ,limit)
                          ,limit))))))


;;;; Highlighting

(defvar-local el-search-hl-overlay nil)

(defvar-local el-search-hl-other-overlays '())

(defvar el-search-keep-hl nil
  "Non-nil indicates we should not remove any highlighting.")

(defun el-search-hl-sexp (&optional bounds)
  (let ((bounds (or bounds (list (point) (el-search--end-of-sexp)))))
    (if (overlayp el-search-hl-overlay)
        (apply #'move-overlay el-search-hl-overlay bounds)
      (overlay-put (setq el-search-hl-overlay (apply #'make-overlay bounds))
                   'face 'el-search-match))
    (overlay-put el-search-hl-overlay 'priority 1002)

    ;; Vertically scroll the current sexp into view when appropriate -- we
    ;; must redisplay to get updated window bounds.  The selected frame
    ;; must apparently be displayed for this to work.
    (while (not (eq t (frame-visible-p (selected-frame))))
      (sleep-for .1))
    (redisplay)
    (let ((wheight (window-height)))
      ;; FIXME: make the following integer constants customizable,
      ;; presumably, named in analogy to the scroll- options?
      (unless (pos-visible-in-window-p
               (save-excursion (goto-char (cadr bounds))
                               (line-end-position (max +3 (/ wheight 25)))))
        (scroll-up (min
                    (max
                     ;; make at least sexp end + a small margin visible
                     (- (line-number-at-pos (cadr bounds))
                        (line-number-at-pos (window-end))
                        (- (max 2 (/ wheight 4))))
                     ;; also try to center current sexp
                     (- (/ ( + (line-number-at-pos (car bounds))
                               (line-number-at-pos (cadr bounds)))
                           2)
                        (/ (+ (line-number-at-pos (window-start))
                              (line-number-at-pos (window-end)))
                           2)))
                    ;; but also ensure at least a small margin is left between point and window start
                    (- (line-number-at-pos (car  bounds))
                       (line-number-at-pos (window-start))
                       3))))))

  (add-hook 'post-command-hook #'el-search-hl-post-command-fun t t))

(defun el-search--hl-other-matches-1 (matcher from to)
  ;; Highlight all matches between FROM and TO with face
  ;; `el-search-other-match'.
  (mapc #'delete-overlay el-search-hl-other-overlays)
  (setq el-search-hl-other-overlays '())
  (let (this-match-beg this-match-end (done nil))
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

(defun el-search-hl-other-matches (matcher)
  "Highlight all visible matches.

Highlight all matches visible in the selected window with face
`el-search-other-match' and add `el-search--after-scroll' to the
local binding of `window-scroll-functions'."
  (el-search--hl-other-matches-1 matcher
                                 (save-excursion
                                   (goto-char (window-start))
                                   (beginning-of-defun-raw)
                                   (point))
                                 (window-end))
  (add-hook 'window-scroll-functions #'el-search--after-scroll t t))

(defun el-search--after-scroll (_win start)
  (el-search--hl-other-matches-1 el-search--current-matcher
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
    (remove-hook 'post-command-hook 'el-search-hl-post-command-fun t)
    (setq el-search--temp-buffer-flag nil)))


;;;; Core functions

(defun el-search-continue-in-next-buffer ()
  "Skip current search buffer and continue with the next."
  (interactive)
  (el-search--skip-to-next-buffer
   (lambda (buffer-or-file)
     (not (if (bufferp buffer-or-file)
              (eq buffer-or-file (current-buffer))
            (file-equal-p buffer-or-file buffer-file-name))))))

(defun el-search-all-matches (search)
  "Perform SEARCH non-interactively and return a list of all matches.

SEARCH is an `el-search-object'.  Execute SEARCH
non-interactively until finished and return a list of matches in
the form \(file-name-or-buffer . match-position)."
  (mapcar
   (pcase-lambda (`(,buffer ,position ,file))
     (cons (if (buffer-live-p buffer) buffer file) position))
   (seq-into-sequence (el-search-object-matches search))))

(defun el-search-jump-to-search-head (&optional previous-search)
  (interactive "P")
  "Switch to current search buffer and go to the last match.
With prefix arg, prompt for a prior search to resume, and make
that the current search."
  (when previous-search
    (let ((entry (ring-ref
                  el-search-history
                  (string-to-number
                   (let ((completion-extra-properties
                          `(:annotation-function
                            ,(lambda (elt)
                               (format "  Search in %S for %S"
                                       (el-search-head-buffer
                                        (el-search-object-head
                                         (car (ring-ref el-search-history (string-to-number elt)))))
                                       (cadr (ring-ref el-search-history (string-to-number elt))))))))
                     (completing-read
                      "Resume previous search: "
                      (mapcar #'prin1-to-string
                              (number-sequence 0 (1- (ring-length el-search-history))))))))))
      (setq el-search--current-search  (car entry))
      (setq el-search--current-pattern (cadr entry)))
    (setq el-search--current-matcher
          (el-search-head-matcher (el-search-object-head el-search--current-search)))
    (el-search--pushnew-to-history
     (el-search--pp-to-string el-search--current-pattern)
     'el-search-pattern-history))
  (if-let ((search el-search--current-search)
           (current-head (el-search-object-head search))
           (current-search-buffer (el-search-head-buffer current-head)))
      (progn
        (setq this-command 'el-search-pattern)
        (let ((win (display-buffer current-search-buffer el-search-display-buffer-action)))
          (select-frame-set-input-focus (window-frame win))
          (select-window win))
        (let ((last-match (el-search-object-last-match search)))
          (if (not (and last-match
                        (eq (marker-buffer last-match) (current-buffer))))
              ;; this should only happen for bad search patterns
              (goto-char (el-search-head-position current-head))
            (goto-char last-match)
            (el-search-hl-sexp)
            (el-search-hl-other-matches el-search--current-matcher)
            (set-transient-map el-search-map))))
    (error "Last search finished")))

(defun el-search-continue-search (&optional from-here)
  "Continue or resume the current search.

With prefix arg FROM-HERE given, the current search buffer should
be the current buffer, and the search will be resumed from point
instead of the position where the search would normally be
continued."
  (interactive "P")
  (setq this-command 'el-search-pattern)
  (setq el-search--current-matcher
        (el-search-head-matcher (el-search-object-head el-search--current-search)))
  (let ((old-current-buffer (current-buffer)))
    (when from-here
      (let* ((head (el-search-object-head el-search--current-search))
             (current-search-buffer
              (or (el-search-head-buffer head)
                  (el-search--next-buffer el-search--current-search))))
        (cond
         ((eq (current-buffer) current-search-buffer)
          (setf (el-search-head-position head) (copy-marker (point))))
         ((and current-search-buffer (buffer-live-p current-search-buffer))
          (error "Please resume from buffer %s" (buffer-name current-search-buffer)))
         (t (error "Invalid search head: buffer killed")))))
    (unwind-protect
        (let ((stream-of-matches (el-search-object-matches el-search--current-search)))
          (if (not (stream-empty-p stream-of-matches))
              (let ((match (stream-first stream-of-matches)))
                (switch-to-buffer (car match))
                (goto-char (cadr match))
                (cl-callf stream-rest (el-search-object-matches el-search--current-search))
                (el-search-hl-sexp)
                (unless (and (eq this-command last-command)
                             el-search--success
                             (eq (current-buffer) old-current-buffer))
                  (el-search-hl-other-matches el-search--current-matcher))
                (set-transient-map el-search-map)
                (setq el-search--success t))
            (setq el-search--success nil)
            (set-transient-map nil)
            (el-search-kill-left-over-search-buffers)
            (message "Done!")))
      (unless el-search--success (setq el-search--current-pattern nil)))))

(defun el-search-skip-directory (directory)
  "Skip all subsequent matches in files located under DIRECTORY."
  (interactive
   (list (expand-file-name
          (read-directory-name "Skip all files under directory: " nil
                               (if-let ((search el-search--current-search)
                                        (current-head (el-search-object-head search))
                                        (current-file (el-search-head-file current-head)))
                                   (file-name-directory current-file)
                                 default-directory)
                               t))))
  (el-search--skip-to-next-buffer
   (lambda (buffer-or-file-name)
     (or (bufferp buffer-or-file-name)
         ;; `file-in-directory-p' would be perfect here, but it calls
         ;; file-truename on both args what we don't want, so we use this:
         (string-match-p "\\`\\.\\." (file-relative-name buffer-or-file-name directory))))))

;;;###autoload
(defun el-search-pattern (pattern)
  "Start new or resume last elisp buffer search.

Search current buffer for expressions that are matched by `pcase'
PATTERN.  Use `read' to transform buffer contents into
expressions.

When called from the current search's current search buffer,
continue that search from point.  Otherwise or when a new PATTERN
is given, start a new single-buffer search from point.

Use `emacs-lisp-mode' for reading the input pattern.  Some keys
in the minibuffer have a special binding: to make it possible to
edit multi line input, C-j inserts a newline, and up and down
move the cursor vertically - see `el-search-read-expression-map'
for details.


Additional `pcase' pattern types to be used with this command can
be defined with `el-search-defpattern'.

The following additional pattern types are currently defined:"
  (interactive (list (if (and (eq this-command last-command)
                              el-search--success)
                         el-search--current-pattern
                       (el-search--read-pattern-for-interactive))))
  (cond
   ((and (eq this-command last-command)
         (eq pattern el-search--current-pattern))
    (progn
      (el-search--skip-expression nil t)
      (el-search-continue-search 'from-here)))
   ((and (equal pattern el-search--current-pattern)
         (eq (current-buffer)
             (el-search-head-buffer (el-search-object-head el-search--current-search))))
    (el-search-continue-search 'from-here))
   (t ;; create a new search single-buffer search
    (el-search-setup-search pattern (stream (list (current-buffer))) 'from-here))))

(put 'el-search-pattern 'function-documentation '(el-search--make-docstring 'el-search-pattern))

(defun el-search-previous-match ()
  "Revisit found matches in the current buffer in reverse order."
  ;; Implementing backward el-searching is very hard (or very slow,
  ;; alternatively), so let's use this for now...
  (interactive)
  (if (not (el-search-head-buffer (el-search-object-head el-search--current-search)))
      ;; FIXME: This case is tricky; the user would expect that when he hits
      ;; C-S afterwards, the search is restored with the old matches
      ;; "merged".  So for now, we raise this:
      (error "Last search completed, please start a new search")
    (setq this-command 'el-search-pattern)
    (let ((last-match-beg (el-search-object-last-match el-search--current-search)))
      (if (< last-match-beg (point))
          (el-search-jump-to-search-head)
        (let ((go-there nil) (matches el-search--this-buffer-matches))
          (catch 'done
            (while (and (not (stream-empty-p matches))
                        (< (cadr (stream-first matches)) (point)))
              (setq go-there (cadr (stream-first matches)))
              (stream-pop matches)))
          (if (not go-there)
              (message "[Beginning of this buffer's recorded matches]")
            (goto-char go-there)
            (el-search-hl-sexp)
            (set-transient-map el-search-map)))))))

;;;###autoload
(defun el-search-buffers (pattern)
  "Search all live elisp buffers for PATTERN."
  (interactive (list (el-search--read-pattern-for-interactive)))
  (el-search-setup-search
   pattern
   (seq-filter
    (lambda (buffer) (with-current-buffer buffer (derived-mode-p 'emacs-lisp-mode)))
    (stream (buffer-list)))))

;;;###autoload
(defun el-search-directory (pattern directory &optional recursively)
  "Search all elisp files in DIRECTORY for PATTERN.
With prefix arg RECURSIVELY non-nil, search subdirectories recursively."
  (interactive (list (el-search--read-pattern-for-interactive)
                     (expand-file-name
                      (read-directory-name (format "el-search directory%s: "
                                                   (if current-prefix-arg " recursively" ""))
                                           nil default-directory t))
                     current-prefix-arg))
  (el-search-setup-search
   pattern
   (el-search-stream-of-directory-files directory recursively)))

;;;###autoload
(defun el-search-emacs-elisp-sources (pattern)
  "Search Emacs elisp sources for PATTERN.
This command recursively searches all elisp files under
\(expand-file-name \"lisp/\" source-directory\)."
  (interactive (list (el-search--read-pattern-for-interactive)))
  (el-search-setup-search
   pattern
   (el-search-stream-of-directory-files
    (expand-file-name "lisp/" source-directory)
    t)))

(declare-function dired-get-marked-files "dired")

;;;###autoload
(defun el-search-dired-marked-files (pattern &optional recursively)
  "el-search marked files and directories in dired.
With RECURSIVELY given (the prefix arg in an interactive call),
search directories recursively."
  (interactive (list (el-search--read-pattern-for-interactive) current-prefix-arg))
  (el-search-setup-search
   pattern
   (stream-concatenate
    (seq-map
     (lambda (file)
       (if (file-directory-p file)
           (el-search-stream-of-directory-files file recursively)
         (stream (list file))))
     (stream (dired-get-marked-files))))))


;;;; Query-replace

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
      (save-excursion
        ;; the whole enclosing sexp might need re-indenting
        (condition-case nil (up-list)  (scan-error))
        (indent-region opoint (1+ (point)))))))

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
                  (if (el-search-forward `',this-sexp t)
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
        (skip-matches-in-replacement 'ask)
        (matcher (setq el-search--current-matcher
                       (el-search--matcher (el-search--wrap-pattern pattern)))))
    (unwind-protect
        (progn

          ;; Try to avoid to call time consuming `el-search-hl-other-matches' in the loop
          (el-search-hl-other-matches matcher)
          (add-hook 'window-scroll-functions #'el-search--after-scroll t t)

          (while (and (not done) (el-search--search-pattern-1 matcher t))
            (setq opoint (point))
            (unless replace-all
              (el-search-hl-sexp))
            (let* ((region (list (point) (el-search--end-of-sexp)))
                   (original-text (apply #'buffer-substring-no-properties region))
                   (expr      (read original-text))
                   (replaced-this nil)
                   (new-expr  (funcall get-replacement expr))
                   (get-replacement-string
                    (lambda () (el-search--format-replacement
                           new-expr original-text to-input-string splice)))
                   (to-insert (funcall get-replacement-string))
                   (replacement-contains-another-match
                    (with-temp-buffer
                      (emacs-lisp-mode)
                      (insert to-insert)
                      (goto-char 1)
                      (el-search--skip-expression new-expr)
                      (condition-case nil
                          (progn (el-search--ensure-sexp-start)
                                 (el-search-forward pattern t))
                        (end-of-buffer nil))))
                   (do-replace
                    (lambda ()
                      (save-excursion
                        (save-restriction
                          (widen)
                          (el-search--replace-hunk (list (point) (el-search--end-of-sexp)) to-insert)))
                      (el-search--ensure-sexp-start) ;skip potentially newly added whitespace
                      (unless replace-all (el-search-hl-sexp (list opoint (point))))
                      (cl-incf nbr-replaced)
                      (setq replaced-this t))))
              (if replace-all
                  (funcall do-replace)
                (redisplay) ;FIXME: why is this necessary?  Without this, read-char-choice recenters!?!
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
              (when replacement-contains-another-match
                (el-search-hl-other-matches matcher))
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
                  (sit-for 2.))))))))
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
                                    (if (equal (read (car el-search-pattern-history))
                                               (read (car el-search-query-replace-history)))
                                        (car el-search-query-replace-history)
                                      (car el-search-pattern-history))))))
                      (el-search--read-pattern "Query replace pattern: " nil
                                               'el-search-query-replace-history)))
        from to read-from read-to)
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert from-input)
      (goto-char 1)
      (forward-sexp)
      (skip-chars-forward " \t\n")
      ;; FIXME: maybe more sanity tests here...
      (if (not (looking-at "->\\|=>\\|>"))
          (setq from from-input
                to (let ((el-search--initial-mb-contents nil))
                     (el-search--read-pattern "Replace with result of evaluation of: " from)))
        (delete-region (point) (match-end 0))
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
    (el-search--pushnew-to-history from 'el-search-pattern-history)
    (setq read-from (read from))
    (setq read-to   (read to))
    (el-search--maybe-warn-about-unquoted-symbol read-from)
    (when (and (symbolp read-to)
               (not (el-search--contains-p (el-search--matcher `',read-to) read-from)))
      (el-search--maybe-warn-about-unquoted-symbol read-to))
    (list read-from read-to to)))

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

\(\">\" and \"=>\" are also allowed as a separator) to the first
prompt and specify both expressions at once.  This format is also
used for history entries."
  (interactive (el-search-query-replace--read-args))
  (setq this-command 'el-search-query-replace) ;in case we come from isearch
  (setq el-search--current-pattern from-pattern)
  (barf-if-buffer-read-only)
  (el-search--search-and-replace-pattern from-pattern to-expr nil textual-to))

(defun el-search--take-over-from-isearch (&optional goto-left-end)
  (let ((other-end (and goto-left-end isearch-other-end))
        (input isearch-string))
    (isearch-exit)
    (when (and other-end (< other-end (point)))
      (goto-char other-end))
    input))


;;;; Invoking from Isearch

;;;###autoload
(defun el-search-search-from-isearch ()
  "Switch to an el-search session from isearch.
Reuse already given input."
  (interactive)
  (let ((el-search--initial-mb-contents (concat "'" (el-search--take-over-from-isearch))))
    ;; use `call-interactively' so we get recorded in `extended-command-history'
    (call-interactively #'el-search-pattern)))

;; Is this useful at all?
;; ;;;###autoload
;; (defun el-search-replace-from-isearch ()
;;   (interactive)
;;   (let ((el-search--initial-mb-contents (concat "'" (el-search--take-over-from-isearch t))))
;;     (call-interactively #'el-search-query-replace)))


(provide 'el-search)
;;; el-search.el ends here
