;;; el-search.el --- Expression based interactive search for Emacs Lisp   -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 29 Jul 2015
;; Keywords: lisp
;; Compatibility: GNU Emacs 25
;; Version: 1.3.2
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


;; Dedicated to my Grandfather Fritz


;;; Commentary:

;; This package implements an expression based interactive search tool
;; for Emacs Lisp files and buffers using pcase-style search patterns.
;; It is multi file/buffer search capable.  It is designed to be fast
;; and easy to use.  It offers an occur-like overview of matches and
;; can do query-replace based on the same set of patterns.  All
;; searches are added to a history and can be resumed or restarted
;; later.  Finally, it allows you to define your own kinds of search
;; patterns and your own multi-search commands.
;;
;;
;; Suggested key bindings
;; ======================
;;
;; After loading this file, you can eval
;; (el-search-install-shift-bindings) to install a set of key bindings
;; to try things out (no other setup is needed).  Here is an overview
;; of the most important bindings that this function will establish -
;; most are of the form Control-Shift-Letter:
;;
;;   C-S (el-search-pattern)
;;     Start a search in the current buffer/go to the next match.
;;
;;   C-R (el-search-pattern-backwards)
;;     Search backwards.
;;
;;   C-% (el-search-query-replace)
;;     Do a query-replace.
;;
;;   M-x el-search-directory
;;     Prompt for a directory name and start a multi search for all
;;     Emacs-Lisp files in that directory.  With prefix arg,
;;     recursively search files in subdirectories.
;;
;;   C-S in Dired (el-search-dired-marked-files)
;;     Like above but uses the marked files and directories.
;;
;;   C-O (el-search-occur)
;;     Pop up an occur buffer for the current search.
;;
;;   C-O (from a search pattern prompt)
;;     Execute this search command as occur.
;;
;;   C-N (el-search-continue-in-next-buffer)
;;     Skip over current buffer or file.
;;
;;   C-D (el-search-skip-directory)
;;     Prompt for a directory name and skip all subsequent files
;;     located under this directory.
;;
;;   C-A (el-search-from-beginning) Go back to the first match in this
;;     buffer or (with prefix arg) completely restart the current
;;     search from the first file or buffer.
;;
;;   C-J (el-search-jump-to-search-head)
;;     Resume the last search from the position of the last visited
;;     match, or (with prefix arg) prompt for an old search to resume.
;;
;;
;; These bindings may not work in a console (if you have a good idea
;; for nice alternative bindings please mail me).
;;
;; The setup you'll need for your init file is trivial: just define
;; the key bindings you want to use (all important commands are
;; autoloaded) and you are done.  You can either just copy
;; (el-search-install-shift-bindings) to your init file to use the
;; above bindings or use its definition as a template for your own key
;; binding definitions.
;;
;;
;; Usage
;; =====
;;
;; The main user entry point `el-search-pattern' (C-S) is analogue to
;; `isearch-forward'.  You are prompted for a `pcase'-style search
;; pattern using an `emacs-lisp-mode' minibuffer.  After hitting RET
;; it searches the current buffer from point for matching expressions.
;; For any match, point is put at the beginning of the expression
;; found (unlike isearch which puts point at the end of matches).  Hit
;; C-S again to go to the next match etc.
;;
;; Syntax and semantics of search patterns are identical to that of
;; `pcase' patterns, plus additionally defined pattern types
;; especially useful for matching parts of programs.  The (only)
;; difference to the `pcase' macro is that the expressions found in
;; buffers and files are tried to be matched instead of a given
;; expression.
;;
;; It doesn't matter how code is actually formatted.  Comments are
;; ignored, and strings are treated as atomic objects (their contents
;; are not being searched).
;;
;;
;; Example 1: if you enter
;;
;;    97
;;
;; at the prompt, el-search will find any occurrence of the integer 97
;; in the code, but not 97.0 or 977 or (+ 90 7) or "My string
;; containing 97" or symbol_97.  OTOH it will find any printed
;; representation of 97, e.g. #x61 or ?a.
;;
;;
;; Example 2: If you enter the pattern
;;
;;   `(defvar ,_)
;;
;; you search for all `defvar' forms that don't specify an init value.
;;
;; The following pattern will search for `defvar's with a docstring
;; whose first line is longer than 70 characters:
;;
;;   `(defvar ,_ ,_
;;      ,(and (pred stringp)
;;            s
;;            (guard (< 70 (length (car (split-string s "\n")))))))
;;
;;
;; You can define your own pattern types with `el-search-defpattern'
;; which is analogue to `defmacro'.  See C-h f `el-search-pattern' for
;; a list of predefined additional pattern types, and C-h f pcase for
;; the basic pcase patterns.
;;
;; Some more pattern definitions can be found in the file
;; "el-search-x" which is part of this package but not automatically
;; loaded.
;;
;;
;; Multi Searching
;; ===============
;;
;; "el-search" is capable of performing "multi searches" - searches
;; spanning multiple files or buffers.  When no more matches can be
;; found in the current file or buffer, the search automatically
;; switches to the next.  Examples for search commands starting a
;; multi search are `el-search-buffers' (search all living elisp mode
;; buffers), `el-search-directory' (search all elisp files in a
;; specified directory), `el-search-emacs-elisp-sources' (search all
;; Emacs elisp sources) and `el-search-dired-marked-files'.  Actually,
;; every search is internally a multi search.
;;
;; You can pause any (multi) search by just doing something different
;; (no quitting is needed), the state of the search is automatically
;; saved.  You can later continue searching by calling
;; `el-search-jump-to-search-head' (C-J): this command jumps to the
;; last match and re-activates the search.

;; `el-search-continue-in-next-buffer' (C-N) skips all remaining
;; matches in the current buffer and continues searching in the next
;; buffer.  `el-search-skip-directory' even skips all subsequent files
;; under a specified directory.
;;
;;
;; El-Occur
;; ========
;;
;; To get an occur-like overview buffer, you can use the usual
;; commands to initiate a search.  You can either hit C-O from a
;; pattern prompt instead of RET to confirm your input and start the
;; search as noninteractive occur search.  Alternatively, you can
;; always call `el-search-occur' (C-O) to start an occur for the
;; latest started search.
;;
;; The *El Occur* buffer uses an adjusted emacs-lisp-mode.  RET on a
;; match gives you a pop-up window displaying the position of the
;; match in that buffer or file.  With S-tab you can (un)collapse all
;; file sections like in `org-mode' to see only file names and the
;; number of matches, or everything.  Tab folds and unfolds
;; expressions (this uses hideshow; initially, all expressions are
;; folded to one line) and sections at the beginning of headlines.
;;
;;
;; Multiple multi searches
;; =======================
;;
;; Every search is collected in a history.  You can resume older
;; searches from the position of the last match by calling
;; `el-search-jump-to-search-head' (C-J) with a prefix argument.  That
;; let's you select an older search to resume and switches to the
;; buffer and position where this search had been suspended.
;;
;;
;; Query-replace
;; =============
;;
;; You can replace expressions with command `el-search-query-replace'.
;; You are queried for a pattern and a replacement expression.  For
;; each match of the pattern, the replacement expression is evaluated
;; with the bindings created by pattern matching in effect, and
;; printed to a string to produce the replacement.
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
;; without moving, SPC or n to go to the next match and ! to replace
;; all remaining matches automatically.  q quits.
;;
;; It is possible to replace a match with multiple expressions using
;; "splicing mode".  When it is active, the replacement expression
;; must evaluate to a list, and is spliced into the buffer for any
;; match.  Use s from the prompt to toggle splicing mode in an
;; `el-search-query-replace' session.
;;
;; There are no special multi-file query-replace commands currently
;; implemented; I don't know if it would be that useful anyway.  If
;; you want to query-replace in multiple buffers or files, just call
;; an appropriate multi-search command, and every time a first match
;; is found in any buffer, start an ordinary
;; `el-search-query-replace'; after finishing, check that everything
;; is ok, save etc, and resume the multi search.
;;
;;
;; Advanced usage: Replacement rules for semi-automatic code rewriting
;; ===================================================================
;;
;; When you want to rewrite larger code parts programmatically, it can
;; often be useful to define a dedicated pattern type to perform the
;; replacement.  Here is an example:
;;
;; You heard that in many situations, `dolist' is faster than an
;; equivalent `mapc'.  You use `mapc' quite often in your code and
;; want to query-replace many occurrences in your stuff.  Instead of
;; using an ad hoc replacing rule, it's cleaner to define a dedicated
;; named pattern type using `el-search-defpattern'.  Make this pattern
;; accept an argument and use it to bind a replacement expression to a
;; variable you specify.  In query-replace, specify that variable as
;; replacement expression.
;;
;; In our case, the pattern could look like this:
;;
;;   (el-search-defpattern el-search-mapc->dolist (new)
;;     (let ((var  (make-symbol "var"))
;;           (body (make-symbol "body"))
;;           (list (make-symbol "list")))
;;       `(and `(mapc (lambda (,,var) . ,,body) ,,list)
;;             (let ,new `(dolist (,,var ,,list) . ,,body)))))
;;
;; The first condition in the `and' performs the matching and binds
;; the essential parts of the `mapc' form to helper variables.  The
;; second, the `let', part, binds the specified variable NEW to the
;; rewritten expression - in our case, a `dolist' form is constructed
;; with the remembered code parts filled in.
;;
;; Now after this preparatory work, for `el-search-query-replace' you
;; can simply specify (literally!) the following rule:
;;
;;   (el-search-mapc->dolist repl) -> repl
;;
;;
;;
;; Bugs, Known Limitations
;; =======================
;;
;; - Replacing: in some cases the read syntax of forms is changing due
;; to reading-printing.  "Some" because we can handle this problem in
;; most cases.
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
;; TODO:
;;
;; - The default keys are not available in the terminal
;;
;; - Handle buffers killed/files closed when resuming a search
;;
;; - Make searching work in comments, too? (->
;;   `parse-sexp-ignore-comments').  Related: should the pattern
;;   `symbol' also match strings that contain matches for a symbol so
;;   that it's possible to replace occurrences of a symbol in
;;   docstrings?
;;
;; - Port this package to non Emacs Lisp modes?  How?  Would it
;;   already work using only syntax tables, sexp scanning and
;;   font-lock?
;;
;; - For query-replace, maybe we should save the original buffer
;;   string in a buffer-local variable, and make that ediff'able
;;   against the new version.
;;
;; - Replace: pause and warn when replacement might be wrong
;;   (ambiguous reader syntaxes; lost comments, comments that can't
;;   non-ambiguously be assigned to rewritten code)




;;; Code:

;;;; Requirements

(eval-when-compile
  (require 'subr-x))

(require 'cl-lib)
(require 'pcase)
(require 'elisp-mode)
(require 'thingatpt)
(require 'thunk)
(require 'stream)
(require 'help-fns) ;el-search--make-docstring
(require 'ring)     ;el-search-history
(require 'hideshow) ;folding in *El Occur*


;;;; Configuration stuff

(defgroup el-search nil
  "Expression based search and replace for Emacs Lisp."
  :group 'lisp)

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

(defcustom el-search-display-buffer-popup-action
  '((display-buffer-reuse-window display-buffer-pop-up-frame)
    (reusable-frames . visible))
  "`display-buffer' action used to display pop-up windows."
  :type display-buffer--action-custom-type)

(defcustom el-search-display-next-buffer-action
  '(display-buffer-same-window (inhibit-same-window . nil))
  "Action used to display the next buffer in multi searches."
  :type display-buffer--action-custom-type)

(defcustom el-search-ignored-directory-regexps
  (mapcar
   (lambda (name) (format "\\`%s\\'" (regexp-quote name)))
   ;; this is just the default value of `grep-find-ignored-directories':
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}"))
  "List of regexps defining directories that el-search should ignore.

The value influences the behavior of the commands that
perform directory searches like `el-search-directory' or
`el-search-dired-marked-files'.  It is consulted by all streams
`el-search-stream-of-directory-files' returns.

The `file-name-nondirectory' of the directory file names is
tested.  "
  :type '(choice (repeat :tag "Regexps for ignored directories" regexp)
		 (const  :tag "No ignored directories" nil)))

(defvar el-search-read-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map read-expression-map)
    (define-key map [(control ?g)] #'abort-recursive-edit)
    (define-key map [up]   nil)
    (define-key map [down] nil)
    (define-key map [(control ?j)] #'newline)
    map)
  "Keymap for reading input with `el-search-read-expression'.")

(defcustom el-search-respect-nosearch t
  "Whether to disregard directories containing a .nosearch file.

When turned on, directory searches skip directories containing a
file named \".nosearch\".

Setting this has an effect on commands that perform searches in
directories, like `el-search-directory' or
`el-search-dired-marked-files'.  The value of this variable is
consulted by all streams `el-search-stream-of-directory-files'
returns."
  :type 'boolean)


;;;; Helpers and Definitions

(defvar el-search-optimized-search t
  "Whether to use optimized searching.
When turned on, use a fast pre-processing algorithm to sort out
buffers that can be proved to not contain a match.

Setting this to nil should not have any effect apart from making
multi-buffer searching much slower in most cases, so this is only
useful for debugging.")

(defvar el-search--current-search nil
  "The currently active search, an `el-search-object', or nil.")

(defvar-local el-search--temp-buffer-flag nil
  "Non-nil tags file visiting buffers as temporarily opened for searching."
  ;; FIXME: maintaining a list of buffers to close would make
  ;; `el-search-kill-left-over-search-buffers' more efficient.  And
  ;; could we merge this with `el-search--temp-file-buffer-flag'?
  )

(defvar-local el-search--temp-file-buffer-flag nil
  "Non-nil tags (file) buffers that should not be presented to the user.
Buffers flagged this way contain the contents of a file but were
not created with `find-file-noselect'.  The name of this file is
used as non-nil value.")

(defvar el-search--success nil
  "Non-nil when last search command was successful.")

(defvar el-search--wrap-flag nil
  "Non-nil when next search command should wrap the search.
The non-nil value should be one of the symbols `forward' and
`backward'.")

(defvar el-search-occur-flag nil
  "Non-nil when next search should be performed as occur.")

(defun el-search-true (&rest _args)
  "Ignore the arguments and return t."
  t)

(defun el-search-with-short-term-memory (function)
  "Wrap FUNCTION to cache the last arguments/result pair."
  (let ((cached nil))
    (lambda (&rest args)
      (pcase cached
        (`(,(pred (equal args)) . ,result) result)
        (_ (cdr (setq cached (cons args (apply function args)))))))))

;; FIXME: the next two should go to stream.el
(defun el-search--stream-divide (stream fun)
  "Divide STREAM after the first ELT for that (FUN ELT REST) returns nil.
The return value is a list of two streams.
Example:

  (mapcar #'seq-into-sequence
          (stream-divide
           (stream (list 1 2 3 5 6 7 9 10 11 23))
           (lambda (this rest) (< (- (stream-first rest) this) 2))))
==> ((1 2 3)
     (5 6 7 9 10 11 23))"
  (if (stream-empty-p stream)
      (list stream (stream-empty))
    (let ((this (stream-pop stream)))
      (letrec ((take-while
                (lambda () (stream-make
                       (cons this
                             (if (or (stream-empty-p stream) (not (funcall fun this stream)))
                                 nil
                               (setq this (stream-pop stream))
                               (funcall take-while)))))))
        (let ((first-part (funcall take-while)))
          (list first-part (stream-delay (progn (stream-flush first-part) stream))))))))

(defun el-search--stream-partition (stream fun)
  "Partition STREAM into bunches where FUN returns true for subsequent elements.
The return value is a stream of streams.

Example:

  (seq-into-sequence
   (seq-map #'seq-into-sequence
            (stream-partition
             (stream (list 1 2 3 5 6 7 9 10 15 23))
                (lambda (x y) (< (- y x) 2)))))
   ==> ((1 2 3)
        (5 6 7)
        (9 10)
        (15)
        (23))"
  (stream-make
   (if (stream-empty-p stream)
       nil
     (let ((divided (el-search--stream-divide stream
                                              (lambda (x rest) (and (not (stream-empty-p rest))
                                                               (funcall fun x (stream-first rest)))))))
       (cons (car divided)
             (el-search--stream-partition (cadr divided) fun))))))

(defun el-search--message-no-log (format-string &rest args)
  "Like `message' but with `message-log-max' bound to nil."
  (let ((message-log-max nil))
    (apply #'message format-string args)))

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
  "Read an expression from the minibuffer."
  (minibuffer-with-setup-hook #'el-search--setup-minibuffer
    (read-from-minibuffer prompt initial-contents el-search-read-expression-map read
                          (or hist 'read-expression-history) default)))

(defvar el-search-pattern-history ()
  "History of search pattern input strings.")

(defvar el-search-history (make-ring 10) ;FIXME: Make `10' customizable?
  "History of previous searches.")

(defvar el-search-query-replace-history ()
  "History of input strings from `el-search-query-replace'.")

(defvar el-search--initial-mb-contents nil)

(defun el-search--pushnew-to-history (input histvar)
  ;; Push string INPUT to HISTVAR unless empty or equal to the head
  ;; element modulo `read'.  Reindent INPUT when multiline.
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
             (not (keywordp pattern)))
    (user-error "Error: free variable `%S' (missing a quote?)" pattern)))

(defun el-search--read-pattern (prompt &optional default histvar)
  (cl-callf or histvar 'el-search-pattern-history)
  (let ((input (el-search-read-expression prompt el-search--initial-mb-contents histvar default)))
    (el-search--pushnew-to-history input histvar)
    (if (not (string= input "")) input (car (symbol-value histvar)))))

(defun el-search--read-pattern-for-interactive ()
  (let* ((input (el-search--read-pattern "El-search pattern: " (car el-search-pattern-history)))
         (pattern (read input)))
    ;; A very common mistake: input "foo" instead of "'foo"
    (el-search--maybe-warn-about-unquoted-symbol pattern)
    (setq this-command 'el-search-pattern) ;in case we come from isearch
    ;; Make input available also in query-replace history
    (el-search--pushnew-to-history input 'el-search-query-replace-history)
    pattern))


(defun el-search--end-of-sexp ()
  "Return the value of point at the end of this sexp.
Point should be at a sexp beginning."
  (if (eql (char-after) ?@) ;bug#24542
      (save-excursion
        (ignore (read (current-buffer)))
        (point))
    (or (scan-sexps (point) 1) (point-max))))

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
expression starting at that position and return it.  Point must
not be inside a string or comment."
  ;; We don't catch end-of-buffer to keep the return value
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
  "Alist of additional \"el-search\" pcase macros.
Keys are pattern names (i.e. symbols) and values the associated
expander functions.")

(defun el-search--make-docstring (name)
  ;; Code mainly from `pcase--make-docstring'
  (let* ((main (documentation (symbol-function name) 'raw))
         (ud (help-split-fundoc main name)))
    (with-temp-buffer
      (insert (or (cdr ud) main))
      (mapc
       (pcase-lambda (`(,symbol . ,fun))
         (unless (string-match-p "\\`[-_]\\|--" (symbol-name symbol)) ;Let's consider these "internal"
           (when-let ((doc (documentation fun)))
             (insert "\n\n\n-- ")
             (setq doc (help-fns--signature symbol doc fun fun nil))
             (insert "\n" (or doc "Not documented.")))))
       (reverse el-search--pcase-macros))
      (let ((combined-doc (buffer-string)))
        (if ud (help-add-fundoc-usage combined-doc (car ud)) combined-doc)))))

(defvar el-search--heuristic-matchers ()
  "Alist of heuristic matchers.
Keys are pattern names (i.e. symbols), and values the associated
heuristic matcher functions.")

(defvar el-search--inverse-heuristic-matchers ())

(defmacro el-search-defpattern (name args &rest body)
  "Like `pcase-defmacro', but for defining el-search patterns.

The semantics is very similar to that of `pcase-defmacro' but the
scope of the definitions is limited to \"el-search\", using a
separate name space.  The expansion is allowed to use any defined
`pcase' pattern as well as any defined el-search pattern.

The docstring may be followed by a `defun' style declaration list
DECL.  There is currently only one respected specification, it
has the form

  \(heuristic-matcher MATCHER-FUNCTION\)

and specifies a heuristic MATCHER-FUNCTION to be associated with
the defined pattern type NAME.  See `el-search-heuristic-matcher'
for details.

\(fn NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"
  (declare (indent 2) (debug defun))
  (let ((doc nil) (declaration-list ()))
    (when (stringp (car body))
      (setq doc  (car body)
            body (cdr body)))
    (pcase (car body)
      (`(declare . ,declarations)
       (setq body (cdr body)
             declaration-list declarations)))
    `(progn
       (setf (alist-get ',name el-search--heuristic-matchers)
             ,(car (alist-get 'heuristic-matcher declaration-list)))
       (setf (alist-get ',name el-search--inverse-heuristic-matchers) ;not official
             ,(car (alist-get 'inverse-heuristic-matcher declaration-list)))
       (setf (alist-get ',name el-search--pcase-macros)
             (lambda ,args ,@(and doc `(,doc)) ,@body)))))

(defmacro el-search--with-additional-pcase-macros (&rest body)
  `(cl-letf ,(mapcar (pcase-lambda (`(,symbol . ,fun)) `((get ',symbol 'pcase-macroexpander) #',fun))
                     el-search--pcase-macros)
     ,@body))

(defun el-search--macroexpand-1 (pattern)
  "Expand el-search PATTERN.
This is like `pcase--macroexpand' but expands only patterns
defined with `el-search-defpattern' and performs only one
expansion step.  If no entry for this pattern type exists in
`el-search--pcase-macros', PATTERN is returned."
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


(defun el-search--search-pattern-1 (matcher &optional noerror bound)
  "Like `el-search-forward' but accepts a matcher as first argument."
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
            (while (and (not match-beg) (or (not bound) (<= (point) bound)))
              (condition-case nil
                  (setq current-expr (el-search--ensure-sexp-start))
                (end-of-buffer
                 (goto-char opoint)
                 (throw 'no-match t)))
              (if (el-search--match-p matcher current-expr)
                  (setq match-beg (point)
                        opoint (point))
                (el-search--skip-expression current-expr))))
          (if noerror nil (signal 'search-failed nil))
        (if (and bound match-beg)
            (and (<= (scan-sexps match-beg 1) bound)
                 match-beg)
          match-beg)))))

(defun el-search-forward (pattern &optional bound noerror)
  "Search for el-search PATTERN in current buffer from point.
Set point to the beginning of the occurrence found and return point.

An optional second argument bounds the search; it is a buffer
position.  The match found must not end after that position.  A
value of nil means search to the end of the accessible portion of
the buffer.

Optional third argument NOERROR, if non-nil, means if fail just
return nil (no error)."
  (el-search--search-pattern-1 (el-search--matcher pattern) noerror bound))


;; FIXME: make this also a declaration spec?
(defun el-search-defpattern--check-args (type args predicate &optional message)
  "Check whether all ARGS fulfill PREDICATE.
Raise a `user-error' if not.  The string arguments TYPE and
optional MESSAGE are used to construct the error message."
  (dolist (arg args)
    (unless (funcall predicate arg)
      (user-error (concat "Pattern `%s': "
                          (or message (format "argument doesn't fulfill %S" predicate))
                          ": %S")
                  type arg))))

(defun el-search--elisp-file-p (file)
  (and (string-match-p (concat "\\.el" (regexp-opt jka-compr-load-suffixes) "?\\'") file)
       (file-exists-p file)
       (not (file-directory-p file))))


(cl-defstruct (el-search-object (:copier copy-el-search-object--1))
  pattern     ;the search pattern
  head        ;an `el-search-head' instance, modified ("moved") while searching
  last-match  ;position of last match found
  command     ;nil or invoking command + args
  get-matches ;method returning a stream of all matches
  )

(defun copy-el-search-object (search)
  (let ((copy (copy-el-search-object--1 search)))
    (cl-callf copy-el-search-head (el-search-object-head copy))
    copy))

(defun el-search--current-pattern ()
  (and el-search--current-search
       (el-search-object-pattern el-search--current-search)))

(defun el-search--current-matcher ()
  (and el-search--current-search
       (el-search-head-matcher (el-search-object-head el-search--current-search))))

(cl-defstruct el-search-head
  get-buffer-stream        ;a function of zero args returning a stream of files and/or buffers to search
  matcher                  ;for the search pattern
  heuristic-buffer-matcher ;for the search pattern
  buffer                   ;currently searched buffer, or nil meaning "continue in next buffer"
  position                 ;where to continue searching this buffer
  file                     ;name of currently searched file, or nil
  buffers                  ;stream of buffers and/or files yet to search
  )


(defun el-search-kill-left-over-search-buffers (&optional not-current-buffer)
  "Kill all buffers that were opened for searching."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer el-search--temp-buffer-flag)
      (unless (or (buffer-modified-p buffer)
                  (and not-current-buffer (eq buffer (current-buffer)))
                  (cl-some (lambda (search) (eq buffer (el-search-head-buffer
                                                   (el-search-object-head search))))
                           (ring-elements el-search-history)))
        (kill-buffer buffer)))))


(defun el-search-heuristic-matcher (pattern)
  "Return a heuristic matcher for PATTERN.

This is a predicate accepting two arguments.  The first argument
is a file name or buffer.  The second argument is a thunk (see
\"thunk.el\") of a list of all of this file's or buffer's atoms.
The predicate returns nil when we can be sure that this file or
buffer can't contain a match for the PATTERN, and must return
non-nil else.

The idea behind heuristic matching is to speed up multi buffer
searching without altering the matching behavior by discarding
files and buffers that can't contain a match.  Most search
patterns contain non-ambiguous information about properties of
atoms that must be present in a buffer containing matches, and
getting a list of atoms in a buffer is negligibly fast compared
to searching that buffer directly.  Thus we spare expensively
searching all buffers we can sort out, which is a majority of all
buffers to search in most cases.

When specified in an `el-search-defpattern' declaration, a
MATCHER-FUNCTION should be a function accepting the same
arguments as the defined pattern.  When called with arguments
ARGS, this function should return either nil (meaning that for
these specific arguments no heuristic matching should be
performed and normal matching should be used) or a (fast!)
function that accepts two arguments: a file-name or buffer, and a
thunk of a complete list of atoms in that file or buffer, that
returns non-nil when this file or buffer could contain a match
for the pattern (NAME . ARGS), and nil when we can be sure that
it doesn't contain a match.  \"Atom\" here means anything whose
parts aren't searched by el-searching, like integers or strings,
but unlike arrays.  When in doubt, this returned function must
return non-nil.

When el-searching is started with a certain PATTERN, a heuristic
matcher function is constructed by recursively destructuring the
PATTERN and combining the heuristic matchers of the subpatterns.
The resulting function is then used to dismiss any buffer that
can't contain any match."
  (pcase pattern
    ((pred symbolp) #'el-search-true)
    ((pred pcase--self-quoting-p) (lambda (_ atoms-thunk) (member pattern (thunk-force atoms-thunk))))
    (`',tree
     (pcase (el-search--flatten-tree tree)
       (`(,tree)  (lambda (_ atoms-thunk) (member tree (thunk-force atoms-thunk))))
       (flattened (let ((matchers (mapcar (lambda (atom) (el-search-heuristic-matcher `',atom))
                                          flattened)))
                    (lambda (file-name-or-buffer atoms-thunk)
                      (cl-every (lambda (matcher) (funcall matcher file-name-or-buffer atoms-thunk))
                                matchers))))))
    (``,qpat
     (cond
      ((eq (car-safe qpat) '\,) (el-search-heuristic-matcher (cadr qpat)))
      ((vectorp qpat)
       (let ((matchers (mapcar (lambda (inner-qpat) (el-search-heuristic-matcher (list '\` inner-qpat)))
                               qpat)))
         (lambda (file-name-or-buffer atoms-thunk)
           (cl-every (lambda (matcher) (funcall matcher file-name-or-buffer atoms-thunk))
                     matchers))))
      ((consp qpat)
       (el-search-heuristic-matcher
        `(and
          ,(list '\` (car qpat))
          ,(if (cdr qpat) (list '\` (cdr qpat)) '_))))
      ((or (stringp qpat) (integerp qpat) (symbolp qpat))
       (lambda (_ atoms-thunk) (member qpat (thunk-force atoms-thunk))))
      (t #'el-search-true)))
    (`(and . ,patterns)
     (let ((matchers (mapcar #'el-search-heuristic-matcher patterns)))
       (lambda (file-name-or-buffer atoms-thunk)
         (cl-every (lambda (matcher) (funcall matcher file-name-or-buffer atoms-thunk))
                   matchers))))
    (`(or . ,patterns)
     (let ((matchers (mapcar #'el-search-heuristic-matcher patterns)))
       (lambda (file-name-or-buffer atoms-thunk)
         (cl-some (lambda (matcher) (funcall matcher file-name-or-buffer atoms-thunk))
                  matchers))))
    (`(,(or 'app 'let 'pred 'guard) . ,_) #'el-search-true)
    ((and `(,name . ,args)
          (let heuristic-matcher (alist-get name el-search--heuristic-matchers))
          (guard heuristic-matcher)
          (let this-heuristic-matcher (apply heuristic-matcher args))
          (guard this-heuristic-matcher))
     (ignore name args heuristic-matcher) ;quite byte compiler
     this-heuristic-matcher)
    ((and (app el-search--macroexpand-1 expanded)
          (guard (not (eq expanded pattern))))
     (el-search-heuristic-matcher expanded))
    (_ #'el-search-true)))

(defvar el-search--atom-list-cache (make-hash-table :test #'equal :size 1000))

(defun el-search-atom-list (file-name-or-buffer)
  "Return a list of el-search-atomic expressions in FILE-NAME-OR-BUFFER."
  (let ((get-buffer-atoms
         (lambda () (apply #'append
                      (mapcar #'el-search--flatten-tree
                              (save-excursion
                                (goto-char (point-min))
                                (let ((forms ()))
                                  (condition-case err
                                      (while t (push (read (current-buffer)) forms))
                                    (end-of-file forms)
                                    (error
                                     (message "%s in %S\nat position %d - skipping"
                                              (error-message-string err)
                                              file-name-or-buffer
                                              (point))
                                     (sit-for 3.)
                                     ;; FIXME: we could also `throw' a tag that would force the
                                     ;; buffer/file to be searched regularly
                                     nil))))))))
        (buffer (if (bufferp file-name-or-buffer)
                    file-name-or-buffer
                  (get-file-buffer file-name-or-buffer))))
    (if buffer
        (if (buffer-live-p buffer)
            (with-current-buffer buffer (funcall get-buffer-atoms))
          ())
      (let ((file-name file-name-or-buffer))
        (if-let ((hash-entry (gethash file-name el-search--atom-list-cache))
                 (its-usable (equal (nth 5 (file-attributes file-name)) (car hash-entry))))
            (cdr hash-entry)
          (let ((atom-list (with-temp-buffer
                             (let ((inhibit-message t))
                               (insert-file-contents file-name-or-buffer))
                             (set-syntax-table emacs-lisp-mode-syntax-table)
                             (funcall get-buffer-atoms))))
            (when atom-list ;empty in case of error
              (puthash file-name
                       (cons (nth 5 (file-attributes file-name)) atom-list)
                       el-search--atom-list-cache))
            atom-list))))))

(defun el-search--flatten-tree (tree)
  (let ((elements ())
        (walked-objects ;to avoid infinite recursion for circular TREEs
         (make-hash-table :test #'eq))
        (gc-cons-percentage 0.8)) ;Why is binding it here more effective than binding it more top-level?
    (cl-labels ((walker (object)
                        (if (or (not (sequencep object)) (stringp object) (null object)
                                (char-table-p object) (bool-vector-p object))
                            (push object elements)
                          (unless (gethash object walked-objects)
                            (puthash object t walked-objects)
                            (if (consp object)
                                (progn
                                  (while (consp object)
                                    (walker (car object))
                                    (setq object (cdr object))
                                    (when (gethash object walked-objects) (setq object nil)))
                                  (when object ;dotted list
                                    (walker object)))
                              (cl-loop for elt being the elements of object do (walker elt)))))))
      (walker tree)
      elements)))

(defun el-search-heuristic-buffer-matcher (pattern)
  (let ((heuristic-matcher (el-search-heuristic-matcher pattern)))
    (lambda (file-name-or-buffer)
      (el-search--message-no-log "%s"
                                 (if (stringp file-name-or-buffer)
                                     file-name-or-buffer
                                   (buffer-name file-name-or-buffer)))
      (funcall heuristic-matcher
               file-name-or-buffer
               (thunk-delay (el-search-atom-list file-name-or-buffer))))))

(defvar warning-minimum-level)
(defun el-search--next-buffer (search &optional predicate keep-highlighting)
  ;; Prepare to continue SEARCH in the next buffer in line.  Move
  ;; SEARCH's head accordingly.  When specified, PREDICATE should accept
  ;; a file name or buffer, and we skip all buffers and files not
  ;; fulfilling it.  Return the new buffer to search in or nil if done.
  (unless keep-highlighting (el-search-hl-remove))
  (let ((original-predicate (or predicate #'el-search-true))
        (heuristic-buffer-matcher
         (el-search-head-heuristic-buffer-matcher (el-search-object-head search))))
    (setq predicate
          (lambda (file-name-or-buffer)
            (and (funcall original-predicate file-name-or-buffer)
                 (or (not el-search-optimized-search)
                     (funcall heuristic-buffer-matcher file-name-or-buffer))))))
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
          (setq buffer (let ((warning-minimum-level :error)
                             (inhibit-message t))
                         (let ((fresh-buffer (generate-new-buffer " el-search-helper-buffer"))
                               (inhibit-message t))
                           (with-current-buffer fresh-buffer
                             (insert-file-contents next)
                             (emacs-lisp-mode)
                             (setq-local el-search--temp-file-buffer-flag next)
                             (setq-local buffer-file-name next) ;make `file' pattern work as expected
                             (set-visited-file-modtime)
                             (set-buffer-modified-p nil))
                           fresh-buffer))))
        (unless (memq buffer buffer-list-before)
          (with-current-buffer buffer
            (setq-local el-search--temp-buffer-flag t)))
        (setf (el-search-head-buffer head) buffer)
        (when (and (bufferp buffer) (not (eq buffer (current-buffer))));FIXME: Is this a good condition?
          (with-current-buffer buffer
            (when (and (buffer-narrowed-p)
                       (y-or-n-p (format "Widen buffer \"%s\"? "
                                         (buffer-name))))
              (widen))))
        buffer))))

(defun el-search--skip-to-next-buffer (&optional predicate)
  ;; Find next buffer fulfilling the PREDICATE and continue search there
  (el-search--next-buffer el-search--current-search predicate)
  (el-search-continue-search))

(defun el-search--all-matches (search)
  "Return a stream of all matches of SEARCH.
The returned stream will always start searching from the
beginning anew even when SEARCH has been used interactively or
elements of another stream returned by this function have already
been requested.

The elements of the returned stream will have the form

  \(buffer match-beg file\)

where BUFFER or FILE is the buffer or file where a match has been
found (exactly one of the two will be nil), and MATCH-BEG is the
position of the beginning of the match."
  (let* ((search (el-search-reset-search search))
         (head (el-search-object-head search)))
    (seq-filter
     #'identity ;we use `nil' as a "skip" tag
     (funcall
      (letrec ((get-stream
                (lambda ()
                  (stream-make
                   (if-let ((buffer (or (el-search-head-buffer head)
                                        (el-search--next-buffer search nil t))))
                       (with-current-buffer buffer
                         (save-excursion
                           (goto-char (el-search-head-position head))
                           (el-search--message-no-log "%s"
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
                                 (cons ;Return the cons defining the build recipe of the stream
                                  (list (el-search-head-buffer head)
                                        match
                                        (el-search-head-file head))
                                  (funcall get-stream)))
                             (setf (el-search-head-buffer head) nil
                                   (el-search-head-file   head) nil)
                             (el-search--next-buffer search nil t)
                             ;; retry with the next buffer
                             (cons nil (funcall get-stream)))))
                     ;; end of stream (no buffers left to search in)
                     nil)))))
        get-stream)))))

(defun el-search-make-search (pattern get-buffer-stream)
  "Create and return a new `el-search-object' instance.
PATTERN is the pattern to search, and GET-BUFFER-STREAM a
function that returns a stream of buffers and/or files to search
in, in order, when called with no arguments."
  (let (self)
    (setq self
          (make-el-search-object
           :pattern pattern
           :head (make-el-search-head
                  :get-buffer-stream get-buffer-stream
                  :matcher (el-search--matcher pattern)
                  :buffers (funcall get-buffer-stream)
                  :heuristic-buffer-matcher (el-search-heuristic-buffer-matcher pattern))
           :get-matches (lambda () (el-search--all-matches self))))))

(defun el-search-reset-search (search)
  "Return a reset copy of SEARCH."
  (let* ((copy (copy-el-search-object search))
         (head (el-search-object-head copy)))
    (setf (el-search-head-buffers head)
          (funcall (el-search-head-get-buffer-stream head)))
    (setf (el-search-head-buffer head)   nil)
    (setf (el-search-head-file head)     nil)
    (setf (el-search-head-position head) nil)
    (setf (el-search-object-last-match copy) nil)
    copy))

(defun el-search-setup-search-1 (pattern get-buffer-stream  &optional from-here)
  (setq el-search--success nil)
  (setq el-search--current-search
        (el-search-make-search pattern get-buffer-stream))
  (let ((call
         ;; (car command-history) ;FIXME: this is not always what we want!
         nil))
    (ring-insert el-search-history el-search--current-search)
    (setf (el-search-object-command el-search--current-search) call))
  (when from-here (setq el-search--temp-buffer-flag nil)))

(defun el-search-setup-search (pattern get-buffer-stream &optional from-here)
  "Create and start a new el-search.
PATTERN is the search pattern.  GET-BUFFER-STREAM is a function
of no arguments that should return a stream of buffers and/or
files (i.e. file names) to search in.

With optional FROM-HERE non-nil, the first buffer in this stream
should be the current buffer, and searching will start at the
current buffer's point instead of its beginning."
  (el-search-setup-search-1 pattern get-buffer-stream)
  (if (not el-search-occur-flag)
      (el-search-continue-search from-here)
    (setq el-search-occur-flag nil)
    (el-search-occur)))

(defun el-search-stream-of-directory-files (&optional directory recurse)
  "Return a stream of emacs-lisp files in DIRECTORY.
DIRECTORY defaults to `default-directory'.  The returned stream
will recurse into DIRECTORY's subdirectories when RECURSE is
non-nil. Subdirectories whose name is matched by one of the
`el-search-ignored-directory-regexps' are excluded.  When
`el-search-respect-nosearch' has a non-nil value, subdirectories
that contain a file named \".nosearch\" are excluded as well."
  (stream-of-directory-files
   (or directory default-directory)
   t nil
   (and recurse
        (lambda (dir-name)
          (not (or (cl-some (lambda (regexp) (string-match-p regexp (file-name-nondirectory dir-name)))
                            el-search-ignored-directory-regexps)
                   (and
                    el-search-respect-nosearch
                    (directory-files dir-name nil "\\`\\.nosearch\\'" t))))))
   t #'el-search--elisp-file-p))

;;;###autoload
(defun el-search-install-shift-bindings ()
  (interactive)

  (define-key emacs-lisp-mode-map [(control ?S)] #'el-search-pattern)
  (define-key emacs-lisp-mode-map [(control ?R)] #'el-search-pattern-backwards)
  (define-key emacs-lisp-mode-map [(control ?%)] #'el-search-query-replace)
  (define-key global-map          [(control ?J)] #'el-search-jump-to-search-head)
  (define-key global-map          [(control ?A)] #'el-search-from-beginning)
  (define-key global-map          [(control ?D)] #'el-search-skip-directory)
  (define-key global-map          [(control ?N)] #'el-search-continue-in-next-buffer)

  (define-key global-map          [(control ?O)] #'el-search-occur)

  (define-key el-search-read-expression-map [(control ?S)] #'exit-minibuffer)
  (define-key el-search-read-expression-map [(control ?R)] #'exit-minibuffer)
  (define-key el-search-read-expression-map [(control ?O)]
    #'el-search-set-occur-flag-exit-minibuffer)

  (define-key isearch-mode-map [(control ?S)] #'el-search-search-from-isearch)
  (define-key isearch-mode-map [(control ?R)] #'el-search-search-backwards-from-isearch)
  (define-key isearch-mode-map [(control ?%)] #'el-search-replace-from-isearch)
  (define-key isearch-mode-map [(control ?O)] #'el-search-occur-from-isearch)

  (define-key global-map [(control ?E)] #'el-search-emacs-elisp-sources)
  (define-key global-map [(control ?L)] #'el-search-load-path)
  (define-key global-map [(control ?B)] #'el-search-buffers)

  (defvar dired-mode-map)

  (with-eval-after-load 'dired
    (define-key dired-mode-map [(control ?S)] #'el-search-dired-marked-files)))


;;;; Additional pattern type definitions

(defun el-search-regexp-like (thing)
  "Return non-nil when THING is regexp like.

In el-search, a regexp-like is either a normal regexp (i.e. a
string), or a predicate accepting a string argument, or a list of
the form

  \(bindings regexp\)

where REGEXP is the actual regexp to match and BINDINGS is a
let-style list of variable bindings.

Example: (((case-fold-search nil)) \"foo\") is a regexp like
matching \"foo\", but not \"Foo\" even when `case-fold-search' is
currently enabled."
  (pcase thing
    ((or (pred stringp) (pred functionp)) t)
    (`(,(and (pred listp) bindings)
       ,(pred stringp))
     (cl-every
      (lambda (binding) (pcase binding ((or (pred symbolp) `(,(pred symbolp)) `(,(pred symbolp) ,_)) t)))
      bindings))))

(defun el-search--string-matcher (regexp-like)
  "Return a compiled match predicate for REGEXP-LIKE.
That's a predicate returning non-nil when the
`el-search-regexp-like' REGEXP-LIKE matches the (only)
argument (that should be a string)."
  (let ((match-bindings ()) regexp)
    (pcase regexp-like
      ((pred stringp) (setq regexp regexp-like))
      (`(,binds ,real-regexp)
       (setq regexp real-regexp)
       (setq match-bindings binds)))
    (if (functionp regexp-like)
        (if (or (symbolp regexp-like) (byte-code-function-p regexp-like))
            regexp-like
          (byte-compile regexp-like))
      (byte-compile
       (let ((string (make-symbol "string")))
         `(lambda (,string) (let ,match-bindings (string-match-p ,regexp ,string))))))))

(el-search-defpattern string (&rest regexps)
  "Matches any string that is matched by all REGEXPS.
Any of the REGEXPS is an `el-search-regexp-like'."
  (declare (heuristic-matcher
            (lambda (&rest regexps)
              (let ((matchers (mapcar #'el-search--string-matcher regexps)))
                (lambda (_ atoms-thunk)
                  (cl-some
                   (lambda (atom)
                     (and (stringp atom)
                          (cl-every (lambda (matcher) (funcall matcher atom)) matchers)))
                   (thunk-force atoms-thunk)))))))
  (el-search-defpattern--check-args "string" regexps #'el-search-regexp-like)
  `(and (pred stringp)
        ,@(mapcar (lambda (regexp) `(pred ,(el-search--string-matcher regexp)))
                  regexps)))

(el-search-defpattern symbol (&rest regexps)
  "Matches any symbol whose name is matched by all REGEXPS.
Any of the REGEXPS is an `el-search-regexp-like'."
  (declare (heuristic-matcher
            (lambda (&rest regexps)
              (let ((matchers (mapcar #'el-search--string-matcher regexps)))
                (lambda (_ atoms-thunk)
                  (cl-some
                   (lambda (atom)
                     (when-let ((symbol-name (and (symbolp atom) (symbol-name atom))))
                       (cl-every (lambda (matcher) (funcall matcher symbol-name)) matchers)))
                   (thunk-force atoms-thunk)))))))
  (el-search-defpattern--check-args "symbol" regexps #'el-search-regexp-like)
  `(and (pred symbolp) (app symbol-name (string ,@regexps))))

(defun el-search--contains-p (matcher expr)
  "Return non-nil when expression tree EXPR contains a match for MATCHER.
MATCHER is a matcher for the el-search pattern to match.  Recurse
on all types of sequences el-search does not treat as atomic.
Matches are not restricted to atoms; for example

  (el-search--contains-p (el-search--matcher ''(2 3)) '(1 (2 3)))

succeeds.

In the positive case the return value is (t elt), where ELT is a
matching element found in EXPR."
  (if (el-search--match-p matcher expr)
      (list t expr)
    (and (sequencep expr)
         (let ((try-match (apply-partially #'el-search--contains-p matcher)))
           (if (consp expr)
               (or (funcall try-match (car expr))
                   (funcall try-match (cdr expr))) ;(1)
             (cl-some try-match expr))))))
;; (1) This means we consider (a b c) to "contain" (b c).  Because we
;; want (a . (b c)) [a syntax common e.g. for notation of alists] to
;; "contain" (b c), and we don't want the matching behavior to depend
;; on actual reader syntax.

(el-search-defpattern contains (&rest patterns)
  "Matches expressions that contain a match for all PATTERNs.

Example:

  \(contains (string \"H\") 17)

matches

  \((\"Hallo\") x (5 [1 17])).

The tested expression itself is included, so for example `1' is
matched by \(contains 1\)."
  (declare (heuristic-matcher
            (lambda (&rest patterns)
              (let ((matchers (mapcar #'el-search-heuristic-matcher patterns)))
                (lambda (file-name-or-buffer atoms-thunk)
                  (cl-every (lambda (matcher) (funcall matcher file-name-or-buffer atoms-thunk))
                            matchers))))))
  (cond
   ((null patterns) '_)
   ((null (cdr patterns))
    (let ((pattern (car patterns)))
      `(app ,(apply-partially #'el-search--contains-p (el-search--matcher pattern))
            `(t ,,pattern)))) ; Match again to establish bindings PATTERN should create
   (t `(and ,@(mapcar (lambda (pattern) `(contains ,pattern)) patterns)))))

(el-search-defpattern in-buffer (&rest atoms)
  "Matches anything in buffers containing all ATOMS.

This pattern matches anything, but only in files or buffers that
contain all of the ATOMS.  In all other files and buffers it
never matches."
  (declare (heuristic-matcher (alist-get 'contains el-search--heuristic-matchers)))
  (el-search-defpattern--check-args
   "in-buffer" atoms
   (lambda (arg)
     (cl-flet ((atom-or-string-p (arg) (or (atom arg) (stringp arg))))
       (pcase arg
         ((or (pred atom-or-string-p) `',(pred atom-or-string-p) ``,(pred atom-or-string-p)) t))))
   "argument not an atom or string")
  (ignore atoms)
  (unless el-search-optimized-search
    (user-error "Pattern `in-buffer' can't be used with `el-search-optimized-search' turned off"))
  '_)

(el-search-defpattern in-file (&rest atoms)
  "This is synonymous with `in-buffer'."
  `(in-buffer ,@atoms))

(el-search-defpattern not (pattern)
  "Matches anything that is not matched by PATTERN."
  (declare
   (heuristic-matcher ;We can't just negate the hm of the PATTERN...
    (lambda (pattern)
      (pcase pattern
        ;; We currently only handle (not (PNAME . ARGS)) where PNAME is the
        ;; name of a pattern type with an inverse heuristic matcher definition
        ((and `(,name . ,args)
              (let inverse-heuristic-matcher (alist-get name el-search--inverse-heuristic-matchers))
              (guard inverse-heuristic-matcher))
         (if (eq t inverse-heuristic-matcher)
             (when-let ((heuristic-matcher
                         (apply (alist-get name el-search--heuristic-matchers) args)))
               (lambda (file-name-or-buffer atoms-thunk)
                 (not (funcall heuristic-matcher file-name-or-buffer atoms-thunk))))
           (apply inverse-heuristic-matcher args)))))))
  `(app ,(apply-partially #'el-search--match-p (el-search--matcher pattern))
        (pred not)))

(defalias 'el-search--symbol-file-matcher
  (el-search-with-short-term-memory
   (lambda (_current-load-history regexp-like)
     ;; We enclosure a prepared hash table containing all the symbols "in"
     (let ((table (make-hash-table))
           (file-name-matches-p
            (let ((string-matcher (el-search--string-matcher regexp-like)))
              (lambda (file-name) (funcall string-matcher (file-name-sans-extension
                                                      (file-name-nondirectory file-name)))))))
       (pcase-dolist (`(,file-name . ,definitions) load-history)
         (when (and (stringp file-name)
                    (funcall file-name-matches-p file-name))
           (dolist (definition definitions)
             (pcase definition
               ((or (and (pred symbolp) symbol)
                    (and `(,type . ,symbol)
                         (guard (not (memq type '(autoload require)))))
                    `(cl-defmethod ,symbol . ,_))
                (ignore type)
                (puthash symbol t table))))))
       (lambda (symbol) (and (symbolp symbol) (gethash symbol table)))))))

(el-search-defpattern symbol-file (regexp)
  "Matches any symbol whose `symbol-file' is matched by REGEXP.

This pattern matches when the object is a symbol for that
`symbol-file' returns a (non-nil) FILE-NAME so that

   (file-name-sans-extension (file-name-nondirectory FILENAME)))

is matched by the `el-search-regexp-like' REGEXP."
  (declare
   (heuristic-matcher
    (lambda (regexp)
      (lambda (_ atoms-thunk)
        (cl-some (el-search--symbol-file-matcher
                  (copy-sequence load-history) ;FIXME: would the car of the load-history suffice?
                  regexp)
                 (thunk-force atoms-thunk))))))
  (el-search-defpattern--check-args "symbol-file" (list regexp) #'el-search-regexp-like)
  (let ((this (make-symbol "this")))
    `(and ,this
          (guard (funcall (el-search--symbol-file-matcher (copy-sequence load-history)
                                                          ',regexp)
                          ,this)))))

(defun el-search--filename-matcher (&rest regexps)
  ;; Return a file name matcher for the REGEXPS.  This is a predicate
  ;; accepting two arguments that returns non-nil when the first
  ;; argument is a file name (i.e. a string) that is matched by all
  ;; REGEXPS, or a buffer whose associated file name matches
  ;; accordingly.  It ignores the second argument.
  ;; Any of the REGEXPS can also be an generalized regexp.
  (let ((get-file-name (lambda (file-name-or-buffer)
                         (if (bufferp file-name-or-buffer)
                             (buffer-file-name file-name-or-buffer)
                           file-name-or-buffer))))
    (if (not regexps)
        (lambda (file-name-or-buffer _) (funcall get-file-name file-name-or-buffer))
      (let* ((regexp-matchers (mapcar #'el-search--string-matcher regexps))
             (test-file-name-or-buffer
              (el-search-with-short-term-memory
               (lambda (file-name-or-buffer)
                 (when-let ((file-name (funcall get-file-name file-name-or-buffer)))
                   (cl-every (lambda (matcher) (funcall matcher file-name)) regexp-matchers))))))
        (lambda (file-name-or-buffer _) (funcall test-file-name-or-buffer file-name-or-buffer))))))

(el-search-defpattern filename (&rest regexps)
  "Matches anything when the searched buffer has an associated file.

With any REGEXPS given, the file's absolute name must be matched
by all of them.  The REGEXPS are `el-search-regexp-like's."
  ;;FIXME: should we also allow to match the f-n-nondirectory and
  ;;f-n-sans-extension?  Maybe it could become a new pattern type named `feature'?
  (declare (heuristic-matcher #'el-search--filename-matcher)
           (inverse-heuristic-matcher t))
  (el-search-defpattern--check-args "filename" regexps #'el-search-regexp-like)
  (let ((file-name-matcher (apply #'el-search--filename-matcher regexps)))
    ;; We can't expand to just t because this would not work with `not'.
    ;; `el-search--filename-matcher' caches the result, so this is still a
    ;; pseudo constant
    `(guard (funcall ',file-name-matcher (current-buffer) nil))))


;;;; Highlighting

(defvar-local el-search-hl-overlay nil)

(defvar-local el-search-hl-other-overlays '())

(defvar el-search-keep-hl nil
  "Non-nil indicates we should not remove any highlighting.

If the non-nil value is the symbol `once', inhibit highlight
removal only once.")

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
  (el-search--hl-other-matches-1
   (el-search--current-matcher)
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
  (unless (or (eq this-command 'el-search-query-replace)
              (eq this-command 'el-search-pattern))
    (setq el-search--wrap-flag nil)
    (if el-search-keep-hl
        (when (eq el-search-keep-hl 'once)
          (setq el-search-keep-hl nil))
      (el-search-hl-remove)
      (remove-hook 'post-command-hook 'el-search-hl-post-command-fun t)
      (setq el-search--temp-buffer-flag nil)
      (el-search-kill-left-over-search-buffers))))


;;;; Core functions

(defun el-search-continue-in-next-buffer ()
  "Skip current search buffer and continue with the next."
  (interactive)
  (el-search--skip-to-next-buffer))

(defun el-search-jump-to-search-head (&optional previous-search)
  "Switch to current search buffer and go to the last match.
With prefix arg, prompt for a prior search to resume, and make
that the current search."
  (interactive "P")
  (when previous-search
    ;; FIXME: would it be better to include some context around the search
    ;; head - or to even use an overview buffer for selection?
    (let ((entry (ring-ref
                  el-search-history
                  (string-to-number
                   (let ((completion-extra-properties
                          `(:annotation-function
                            ,(lambda (elt)
                               (let ((search (ring-ref el-search-history (string-to-number elt))))
                                 (concat
                                  " "
                                  (if-let ((command (el-search-object-command search)))
                                      (concat "`" (prin1-to-string (car command)) "'")
                                    "Search")
                                  (if-let ((buffer
                                            (el-search-head-buffer
                                             (el-search-object-head
                                              search))))
                                      (format " [paused in %s]"
                                              (if (buffer-live-p buffer)
                                                  (concat "buffer " (buffer-name buffer))
                                                "a killed buffer"))
                                    " [completed]")
                                  "\n" (pp-to-string (el-search-object-pattern search))))))))
                     (completing-read
                      "Resume previous search: "
                      (mapcar #'prin1-to-string
                              (number-sequence 0 (1- (ring-length el-search-history))))))))))
      (setq el-search--current-search entry)
      (setq el-search--success t)
      (setq el-search--wrap-flag nil)))
  (if-let ((search el-search--current-search)
           (current-head (el-search-object-head search))
           (current-search-buffer (el-search-head-buffer current-head)))
      (if (not (buffer-live-p current-search-buffer))
          (user-error "Search head points to a killed buffer")
        (setq this-command 'el-search-pattern)
        (pop-to-buffer current-search-buffer el-search-display-buffer-popup-action)
        (let ((last-match (el-search-object-last-match search)))
          (if (not (and last-match
                        (eq (marker-buffer last-match) (current-buffer))))
              ;; this should only happen for bad search patterns
              (goto-char (el-search-head-position current-head))
            (goto-char last-match)
            (el-search-hl-sexp)
            (el-search-hl-other-matches (el-search--current-matcher)))))
    (el-search--message-no-log "[Search completed - restarting]")
    (sit-for 1.5)
    (cl-callf el-search-reset-search el-search--current-search)
    (el-search-continue-search)))

(defun el-search-continue-search (&optional from-here)
  "Continue or resume the current search.

With prefix arg FROM-HERE given, the current search buffer should
be the current buffer, and the search will be resumed from point
instead of the position where the search would normally be
continued."
  (interactive "P")
  (setq this-command 'el-search-pattern)
  (unwind-protect
      (let* ((old-current-buffer (current-buffer))
             (head (el-search-object-head el-search--current-search))
             (current-search-buffer
              (or (el-search-head-buffer head)
                  (el-search--next-buffer el-search--current-search))))
        (when from-here
          (cond
           ((eq (current-buffer) current-search-buffer)
            (setf (el-search-head-position head) (copy-marker (point))))
           ((and current-search-buffer (buffer-live-p current-search-buffer))
            (user-error "Please resume from buffer %s" (buffer-name current-search-buffer)))
           (current-search-buffer
            (user-error "Search head points to a killed buffer"))))
        (let ((match nil)
              (matcher (el-search--current-matcher)))
          (while (and (el-search-head-buffer head)
                      (not (setq match (with-current-buffer (el-search-head-buffer head)
                                         (save-excursion
                                           (goto-char (el-search-head-position head))
                                           (el-search--search-pattern-1 matcher t))))))
            (el-search--next-buffer el-search--current-search))
          (if (not match)
              (progn
                (if (not (or el-search--success
                             (and from-here
                                  (save-excursion
                                    (goto-char (point-min))
                                    (el-search--search-pattern-1 matcher t)))))
                    (progn
                      (el-search--message-no-log "No matches")
                      (sit-for .7))
                  (setq el-search--wrap-flag 'forward)
                  (let ((keys (car (where-is-internal 'el-search-pattern))))
                    (el-search--message-no-log
                     (if keys
                         (format "No (more) matches - Hit %s to wrap search"
                                 (key-description keys)))
                     "No (more) matches"))))
            (let (match-start)
              ;; If (el-search-head-buffer head) is only a worker buffer, replace it
              ;; with a buffer created with `find-file-noselect'
              (with-current-buffer (el-search-head-buffer head)
                (goto-char match)
                (setq match-start (point))
                (when el-search--temp-file-buffer-flag
                  (let ((file-name buffer-file-name))
                    (setq buffer-file-name nil) ;prevent f-f-ns to find this buffer
                    (let ((buffer-list-before (buffer-list))
                          (new-buffer (find-file-noselect file-name)))
                      (setf (el-search-head-buffer head) new-buffer)
                      (unless (memq new-buffer buffer-list-before)
                        (with-current-buffer new-buffer
                          (setq-local el-search--temp-buffer-flag t)))))))
              (pop-to-buffer (el-search-head-buffer head) el-search-display-next-buffer-action)
              (goto-char match-start))
            (setf (el-search-object-last-match el-search--current-search)
                  (copy-marker (point)))
            (setf (el-search-head-position head)
                  (copy-marker (point)))
            (el-search-hl-sexp)
            (unless (and (eq this-command last-command)
                         el-search--success
                         (eq (current-buffer) old-current-buffer))
              (el-search-hl-other-matches matcher))
            (setq el-search--success t))))
    (el-search-kill-left-over-search-buffers)))

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

Search current buffer for expressions that are matched by
PATTERN.  When called from the current search's current search
buffer, continue that search from point.  Otherwise or when a new
PATTERN is given, start a new single-buffer search from point.

The minibuffer is put into `emacs-lisp-mode' for reading the
input pattern, and there are some special key bindings:
\\<el-search-read-expression-map>\\[newline] inserts a newline,
and <up> and <down> are unbound in the local map to let you move
the cursor vertically - see `el-search-read-expression-map' for
details.

PATTERN is an \"el-search\" pattern - which means, either a
`pcase' pattern or complying with one of the additional pattern
types defined with `el-search-defpattern'.  The following
additional pattern types are currently defined:"
  (declare (interactive-only el-search-forward))
  (interactive (list (if (or (memq #'el-search-hl-post-command-fun post-command-hook) ;FIXME: ugh!
                             (and (eq this-command last-command)
                                  (or el-search--success el-search--wrap-flag)))
                         (el-search--current-pattern)
                       (el-search--read-pattern-for-interactive))))
  (cond
   ((eq el-search--wrap-flag 'forward)
    (progn
      (setq el-search--wrap-flag nil)
      (el-search--message-no-log "[Wrapped search]")
      (sit-for .7)
      (el-search-from-beginning 'restart)))
   ((and (eq this-command last-command)
         (eq pattern (el-search--current-pattern)))
    (progn
      (el-search--skip-expression nil t)
      (el-search-continue-search 'from-here)))
   ((and (equal pattern (el-search--current-pattern))
         (eq (current-buffer)
             (el-search-head-buffer (el-search-object-head el-search--current-search))))
    ;; FIXME: We don't want to create a new search here, but when a
    ;; pattern definition has changed, this uses the old definition.
     (el-search-continue-search 'from-here))
   (t ;create a new search single-buffer search
    (el-search-setup-search
     pattern
     (let ((current-buffer (current-buffer)))
       (lambda () (stream (list current-buffer))))
     'from-here))))

(put 'el-search-pattern 'function-documentation '(el-search--make-docstring 'el-search-pattern))

(defun el-search-from-beginning (&optional restart-search)
  "Go to the first of this buffer's matches.
With prefix arg, restart the current search."
  (interactive "P")
  (if (not restart-search)
      (setf (el-search-head-position (el-search-object-head el-search--current-search))
            (point-min))
    (cl-callf el-search-reset-search el-search--current-search)
    (setq el-search--success nil))
  (el-search-continue-search))

(defun el-search-pattern-backwards (pattern)
  "Search the current buffer backwards for matches of PATTERN."
  (declare (interactive-only t))
  (interactive (list (if (and (eq last-command 'el-search-pattern)
                              (or el-search--success el-search--wrap-flag))
                         (el-search--current-pattern)
                       (el-search--read-pattern-for-interactive))))
  (unless (eq pattern (el-search--current-pattern))
    (el-search-setup-search-1
     pattern
     (let ((current-buffer (current-buffer)))
       (lambda () (stream (list current-buffer))))
     'from-here)
    ;; Make this buffer the current search buffer so that a following C-S
    ;; doesn't delete highlighting
    (el-search--next-buffer el-search--current-search))
  (setq this-command 'el-search-pattern)
  (when (eq el-search--wrap-flag 'backward)
    (setq el-search--wrap-flag nil)
    (el-search--message-no-log "[Wrapped backward search]")
    (sit-for .7)
    (goto-char (point-max)))
  (let ((outer-loop-done nil)
        (original-point (point))
        (matcher (el-search--current-matcher)))
    ;; Strategy: search forwards (inner loop) for PATTERN, starting from
    ;; this toplevel expression's beginning up to point, then if no match
    ;; is found, search the top level expression before this one up to its
    ;; end, etc (outer loop).
    (while (not outer-loop-done)
      (let ((last-match nil)
            (limit (point))
            (defun-start (or (syntax-ppss-toplevel-pos (syntax-ppss))
                             (scan-sexps (point) -1)))
            (done nil) defun-end)
        (when defun-start
          (goto-char defun-start)
          (setq defun-end (scan-sexps defun-start 1))
          (while (and (not done)
                      (el-search--search-pattern-1 matcher 'no-error defun-end))
            (if (>= (point) limit)
                (setq done t)
              (setq last-match (point))
              (el-search--skip-expression nil t))))
        (if (not last-match)
            (if defun-start
                (goto-char defun-start)
              ;; reached bob
              (goto-char original-point)
              (setq outer-loop-done t)
              (if (not (or el-search--success
                           (save-excursion
                             (goto-char (point-min))
                             (el-search--search-pattern-1 matcher t))))
                  (progn
                    (ding)
                    (el-search--message-no-log "No matches")
                    (sit-for .7))
                (let ((keys (car (where-is-internal 'el-search-pattern-backwards))))
                  (el-search--message-no-log
                   (if keys
                       (format "No (more) match; hit %s to wrap search" (key-description keys))
                     "No (more) match")))
                (sit-for .7)
                (goto-char original-point)
                (setq el-search--wrap-flag 'backward)))
          (setq outer-loop-done t)
          (goto-char last-match)
          (setf (el-search-head-position (el-search-object-head el-search--current-search))
                (copy-marker (point)))
          (setf (el-search-object-last-match el-search--current-search)
                (copy-marker (point)))
          (el-search-hl-sexp)
          (unless (eq last-command 'el-search-pattern)
            (el-search-hl-other-matches (el-search--current-matcher)))
          (setq el-search--success t))))))

(define-obsolete-function-alias 'el-search-previous-match 'el-search-pattern-backwards)


;;;; El-Occur

(defvar-local el-search-occur-search-object nil)


(defvar orgstruct-heading-prefix-regexp)
(declare-function org-back-to-heading 'org)
(declare-function org-global-cycle    'org)
(declare-function orgstruct-mode      'org)

(defun el-search-occur-revert-function (&rest _)
  (el-search--occur el-search-occur-search-object t))

(defun el-search-occur-jump-to-match ()
  (interactive)
  (if (button-at (point))
      (push-button)
    (when-let ((data (get-text-property (point) 'match-data))
               (pattern (el-search-object-pattern el-search-occur-search-object)))
      (pcase-let ((`(,buffer ,position ,file) data))
        (with-selected-window
            (display-buffer (if file (find-file-noselect file) buffer)
                            '((display-buffer-pop-up-window)))
          (when (and (buffer-narrowed-p)
                     (or (< position (point-min))
                         (> position (point-max)))
                     (not (and (y-or-n-p "Widen buffer? ")
                               (progn (widen) t))))
            (user-error "Can't jump to match"))
          (goto-char position)
          (el-search-setup-search
           pattern
           (let ((current-buffer (current-buffer)))
             (lambda () (stream (list current-buffer))))
           'from-here)
          (setq-local el-search-keep-hl 'once))))))

(declare-function orgstruct-hijacker-org-shifttab-2 'org)
(defun el-search-occur-cycle ()
  (interactive)
  (cl-letf (((symbol-function 'org-context-p) #'el-search-true))
    (call-interactively #'orgstruct-hijacker-org-shifttab-2)))

(defvar el-search-occur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]           #'hs-toggle-hiding)
    (define-key map "\t"            #'hs-toggle-hiding)
    (define-key map [return]        #'el-search-occur-jump-to-match)
    (define-key map "\r"            #'el-search-occur-jump-to-match)
    (define-key map [S-iso-lefttab] #'el-search-occur-cycle)
    (define-key map [(shift tab)]   #'el-search-occur-cycle)
    (set-keymap-parent map (make-composed-keymap special-mode-map emacs-lisp-mode-map))
    map))

(define-derived-mode el-search-occur-mode emacs-lisp-mode "El-Occur"
  (setq-local revert-buffer-function #'el-search-occur-revert-function)
  (setq buffer-read-only t)
  (hs-minor-mode +1)
  (hs-hide-all)
  (setq orgstruct-heading-prefix-regexp ";;; ")
  (setq outline-regexp "^;;;\\ \\*+")
  (orgstruct-mode +1))

(put 'el-search-occur-mode 'mode-class 'special)


(defun el-search--occur (search &optional buffer)
  (let ((occur-buffer (or buffer (generate-new-buffer "*El Occur*"))))
    (setq this-command 'el-search-pattern)
    (setq-local el-search--temp-buffer-flag nil)
    (with-selected-window (if buffer (selected-window)
                            (display-buffer
                             occur-buffer
                             '((display-buffer-pop-up-window display-buffer-use-some-window))))
      (let ((inhibit-read-only t))
        (if el-search-occur-search-object
            (erase-buffer)
          (el-search-occur-mode)
          (setq el-search-occur-search-object search))
        (insert (format ";;; * %s   -*- mode: el-search-occur -*-\n\n%s\n\n"
                        (current-time-string)
                        (if-let ((command-or-pattern
                                  (or (el-search-object-command el-search-occur-search-object)
                                      (el-search-object-pattern el-search-occur-search-object))))
                            (el-search--pp-to-string command-or-pattern)
                          "")))
        (condition-case-unless-debug err
            (let ((stream-of-matches
                   (el-search--stream-partition
                    (funcall (el-search-object-get-matches search))
                    (lambda (this prev) (and (eq (car this) (car prev)) (equal (nth 2 this) (nth 2 prev))))))
                  stream-of-buffer-matches  buffer-matches
                  (matching-files 0) (matching-buffers 0) (overall-matches 0))
              (while (setq stream-of-buffer-matches (stream-pop stream-of-matches))
                (setq buffer-matches (seq-length stream-of-buffer-matches))
                (cl-incf overall-matches buffer-matches)
                (pcase-let ((`(,buffer ,_ ,file) (stream-first stream-of-buffer-matches)))
                  (if file (cl-incf matching-files) (cl-incf matching-buffers))
                  (insert "\n;;; ** ")
                  (insert-button
                   (or file (format "%S" buffer))
                   'action
                   (let ((pattern (el-search--current-pattern)))
                     (lambda (_)
                       (pop-to-buffer
                        (if file (find-file-noselect file) buffer)
                        el-search-display-buffer-popup-action)
                       (widen)
                       (goto-char (point-min))
                       (let ((el-search-history (ring-copy el-search-history)))
                         (funcall-interactively #'el-search-pattern pattern))
                       (el-search--message-no-log "This is the first match in %S" (or file buffer)))))
                  (insert (format "  (%d matches)\n" buffer-matches))
                  (let* ((get-context
                          (lambda (match-beg)
                            (let ((context-beg nil)
                                  (need-more-context-p
                                   (lambda (start)
                                     (let (end)
                                       (pcase (save-excursion
                                                (goto-char start)
                                                (prog1 (read (current-buffer))
                                                  (setq end (point))))
                                         ((or (pred atom) `(,(pred atom))) t)
                                         ((guard (< (- end start) 100))     t)))))
                                  (try-go-upwards (lambda (pos) (condition-case nil (scan-lists pos -1 1)
                                                             (scan-error)))))
                              (with-current-buffer buffer
                                (when (funcall need-more-context-p match-beg)
                                  (setq context-beg (funcall try-go-upwards match-beg))
                                  (when (and context-beg (funcall need-more-context-p context-beg))
                                    (setq context-beg (or (funcall try-go-upwards context-beg)
                                                          context-beg))))
                                (cons (or context-beg match-beg)
                                      (if context-beg (scan-lists context-beg 1 0)
                                        (scan-sexps match-beg 1)))))))
                         (buffer-matches+contexts
                          (seq-map (pcase-lambda ((and match `(,_ ,match-beg ,_)))
                                     (cons match (funcall get-context match-beg)))
                                   stream-of-buffer-matches)))
                    (while (not (stream-empty-p buffer-matches+contexts))
                      (pcase-let ((`((,_ ,match-beg ,_) . (,context-beg . ,context-end))
                                   (stream-first buffer-matches+contexts)))
                        (let ((insertion-point (point)) matches
                              (end-of-defun (with-current-buffer buffer
                                              (goto-char match-beg)
                                              (let ((paren-depth (car (syntax-ppss))))
                                                (if (< 0 paren-depth)
                                                    (scan-lists match-beg 1 paren-depth)
                                                  (el-search--end-of-sexp))))))
                          (let ((rest buffer-matches+contexts)
                                (remaining-buffer-matches-+contexts buffer-matches+contexts))
                            (with-current-buffer buffer
                              (while (pcase (stream-first rest)
                                       (`(,_ . (,(and cbeg (pred (> end-of-defun))) . ,_))
                                        (prog1 t
                                          (stream-pop rest)
                                          (when (< cbeg context-end)
                                            (setq remaining-buffer-matches-+contexts rest)
                                            (when (< cbeg context-beg)
                                              (setq context-beg cbeg)
                                              (setq context-end
                                                    (or (scan-sexps cbeg 1) context-end)))))))))
                            (setq matches
                                  (car (el-search--stream-divide
                                        buffer-matches+contexts
                                        (lambda (_ rest)
                                          (not (eq rest remaining-buffer-matches-+contexts))))))
                            (setq buffer-matches+contexts remaining-buffer-matches-+contexts))
                          (cl-flet ((insert-match-and-advance
                                     (match-beg)
                                     (let ((insertion-point (point)))
                                       (insert (propertize
                                                (with-current-buffer buffer
                                                  (buffer-substring-no-properties
                                                   (goto-char match-beg)
                                                   (goto-char (scan-sexps (point) 1))))
                                                'match-data `(,buffer ,match-beg ,file)))
                                       (let ((ov (make-overlay insertion-point (point) nil t)))
                                         (overlay-put ov 'face 'el-search-match))
                                       (with-current-buffer buffer (point)))))
                            (let ((working-position context-beg))
                              (while (not (stream-empty-p matches))
                                (pcase-let ((`((,_ ,match-beg ,_) . ,_) (stream-pop matches)))
                                  (insert-buffer-substring buffer working-position match-beg)
                                  (setq working-position (insert-match-and-advance match-beg))
                                  ;; Drop any matches inside the printed area.
                                  ;; FIXME: Should we highlight matches inside matches specially?
                                  ;; Should we display the number of matches included in a context?
                                  (while (pcase (stream-first matches)
                                           (`((,_ ,(pred (> working-position)) ,_) . ,_) t))
                                    (stream-pop matches))))
                              (insert
                               (with-current-buffer buffer
                                 (buffer-substring-no-properties (point) (scan-sexps context-beg 1))))))

                          (let ((inhibit-message t) (message-log-max nil))
                            (indent-region insertion-point (point))
                            (save-excursion
                              (goto-char insertion-point)
                              (ignore-errors
                                ;; This can error...
                                (if nil ;if need-context
                                    (hs-hide-level 1)
                                  (hs-hide-block)))))
                          (insert "\n")))))))

              (insert
               (if (zerop overall-matches)
                   ";;; * No matches"
                 (concat
                  (format "\n\n;;; * %d matches in " overall-matches)
                  (unless (zerop matching-files) (format "%d files" matching-files))
                  (unless (or (zerop matching-files) (zerop matching-buffers)) " and ")
                  (unless (zerop matching-buffers)  (format "%d buffers" matching-buffers))
                  ".")))
              (goto-char (point-min)))
          (quit  (insert "\n\n;;; * Aborted"))
          (error (insert "\n\n;;; * Error: " (error-message-string err)
                         "\n;;; Please make a bug report to the maintainer.  Yes, really.
;;; Thanks in advance!")))
        (el-search--message-no-log "")
        (set-buffer-modified-p nil))))
  (el-search-kill-left-over-search-buffers))

(defun el-search-occur ()
  "Display an occur-like overview of matches of the current search.

Buffers and files are separated by headlines.  Matches are
highlighted with face `el-search-match'.  For short matches, some
context is shown, and nearby matches are grouped.  Initially all
expressions are folded to one line.
\\<el-search-occur-mode-map>
Clicking on a file or buffer name displays it in a different
window and goes to the first match.
\\[el-search-occur-jump-to-match] on a match displays that match
in its buffer.  \\[revert-buffer] refreshes the buffer and
restarts the search.  \\[hs-toggle-hiding] folds and unfolds
expressions, while \\[el-search-occur-cycle] folds and unfolds
buffer and file sections like in `org-mode'.  At the beginning of
a headline, <tab> toggles folding the following section.

Hit \\[quit-window] to leave without killing the buffer.

The occur buffer is in `el-search-occur-mode' that is derived
from `emacs-lisp-mode' and `special-mode'.  In addition it makes
use of `hs-minor-mode' and `orgstruct-mode'."
  (interactive)
  (el-search--message-no-log "Preparing occur buffer")
  (if el-search--current-search
      (el-search--occur el-search--current-search)
    (user-error "No active search"))
  (setq this-command 'el-search-pattern))

(defun el-search-set-occur-flag-exit-minibuffer ()
  (interactive)
  (setq el-search-occur-flag t)
  (exit-minibuffer))

;;;###autoload
(defun el-search-buffers (pattern)
  "Search all live elisp buffers for PATTERN."
  (interactive (list (el-search--read-pattern-for-interactive)))
  (el-search-setup-search
   pattern
   (lambda ()
     (seq-filter
      (lambda (buffer) (with-current-buffer buffer (and (derived-mode-p 'emacs-lisp-mode)
                                                   (not (eq major-mode 'el-search-occur-mode)))))
      (stream (buffer-list))))))

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
   (lambda () (el-search-stream-of-directory-files directory recursively))))

;;;###autoload
(defun el-search-emacs-elisp-sources (pattern)
  "Search Emacs elisp sources for PATTERN.
This command recursively searches all elisp files under
\(expand-file-name \"lisp/\" source-directory\)."
  (interactive (list (el-search--read-pattern-for-interactive)))
  (el-search-setup-search
   pattern
   (lambda ()
     (el-search-stream-of-directory-files
      (expand-file-name "lisp/" source-directory)
      t))))

;;;###autoload
(defun el-search-load-path (pattern)
  "Search PATTERN in all elisp files in all directories in `load-path'.
nil elements in `load-path' (standing for `default-directory')
are ignored."
  (interactive (list (el-search--read-pattern-for-interactive)))
  (el-search-setup-search
   pattern
   (lambda ()
     (stream-concatenate
      (seq-map (lambda (path) (el-search-stream-of-directory-files path nil))
               (stream (delq nil load-path)))))))

(declare-function dired-get-marked-files "dired")

;;;###autoload
(defun el-search-dired-marked-files (pattern files &optional recursively)
  "El-search files and directories marked in dired.
With RECURSIVELY given (the prefix arg in an interactive call),
search directories recursively.

This function uses `el-search-stream-of-directory-files' to
compute a the file stream - see there for a description of
related user options."
  (interactive (list (el-search--read-pattern-for-interactive)
                     (dired-get-marked-files)
                     current-prefix-arg))
  (el-search-setup-search
   pattern
   (lambda ()
     (stream-concatenate
      (seq-map
       (lambda (file)
         (if (file-directory-p file)
             (el-search-stream-of-directory-files file recursively)
           (stream (list file))))
       (stream files))))))


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
      (error "Expression to splice in is not a list")
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
                  (if (el-search-forward `',this-sexp nil t)
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
                ;; FIXME: there could be another occurrence of THIS-SEXP in
                ;; ORIG-BUFFER with more subsequent equal expressions after it
                ;; FIXME: include any trailing comment?
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
  (el-search-setup-search-1 pattern (lambda () (current-buffer)) t) ;for side effect only
  (let ((replace-all nil) (nbr-replaced 0) (nbr-skipped 0) (done nil)
        (el-search-keep-hl t) (opoint (point))
        (get-replacement (el-search--matcher pattern replacement))
        (skip-matches-in-replacement 'ask)
        (matcher (el-search--matcher pattern)))
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
                                 (el-search-forward pattern nil t))
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
  (interactive (el-search-query-replace--read-args)) ;this binds the optional argument
  (setq this-command 'el-search-query-replace) ;in case we come from isearch
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

;;;###autoload
(defun el-search-search-backwards-from-isearch ()
  "Switch to `el-search-pattern-backwards' from isearch.
Reuse already given input."
  (interactive)
  (let ((el-search--initial-mb-contents (concat "'" (el-search--take-over-from-isearch))))
    (call-interactively #'el-search-pattern-backwards)))

;;;###autoload
(defun el-search-replace-from-isearch ()
  "Switch to `el-search-query-replace' from isearch.
Reuse already given input."
  (interactive)
  (let ((el-search--initial-mb-contents (concat "'" (el-search--take-over-from-isearch t))))
    (call-interactively #'el-search-query-replace)))

;;;###autoload
(defun el-search-occur-from-isearch ()
  "Switch to `el-search-occur' from isearch.
Reuse already given input."
  (interactive)
  (setq el-search-occur-flag t)
  (call-interactively #'el-search-search-from-isearch))


(provide 'el-search)
;;; el-search.el ends here
