;;; el-search.el --- Expression based interactive search for Emacs Lisp   -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 29 Jul 2015
;; Keywords: lisp
;; Compatibility: GNU Emacs 25
;; Version: 1.6.5
;; Package-Requires: ((emacs "25") (stream "2.2.4") (cl-print "1.0"))


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
;; for Emacs Lisp files and buffers.  The pattern language used is a
;; superset of `pcase' patterns.
;;
;; "el-search" is multi file/buffer search capable.  It is designed to
;; be fast and easy to use.  It offers an occur-like overview of
;; matches and can do query-replace based on the same set of patterns.
;; All searches are added to a history and can be resumed or restarted
;; later.  Finally, it allows you to define your own kinds of search
;; patterns and your own multi-search commands.
;;
;;
;; Key bindings
;; ============
;;
;; Loading this file doesn't install any key bindings - but you
;; probably want some.  There are two predefined sets of key bindings.
;; The first set installs bindings mostly of the form
;; "Control-Shift-Letter", e.g. C-S, C-R, C-% etc.  These can be
;; installed by calling (el-search-install-shift-bindings) - typically
;; in your init file.  For console users (and others), the function
;; `el-search-install-bindings-under-prefix' installs bindings of the
;; form PREFIX LETTER.  If you e.g. call
;;
;;   (el-search-install-bindings-under-prefix [(meta ?s) ?e])
;;
;; you install bindings M-s e s, M-s e r, M-s e % etc.  When using
;; this function to install key bindings, the bindings are
;; "repeatable" where it makes sense, so that you can for example hit
;; M-s e j s s s a % to reactive the last search, go to the next match
;; three times, then go back to the first match in the current buffer,
;; and finally invoke query-replace.
;;
;; Here is a complete list of key bindings installed when
;; you call
;;   (el-search-install-shift-bindings)
;; or
;;   (el-search-install-bindings-under-prefix [(meta ?s) ?e])
;;
;; respectively:
;;
;;   C-S, M-s e s (el-search-pattern)
;;     Start a search in the current buffer/go to the next match.
;;
;;   C-R, M-s e r (el-search-pattern-backwards)
;;     Search backwards.
;;
;;   C-%, M-s e % (el-search-query-replace)
;;     Do a query-replace.
;;
;;   M-x el-search-directory
;;     Prompt for a directory name and start a multi search for all
;;     Emacs-Lisp files in that directory.  With prefix arg,
;;     recursively search files in subdirectories.
;;
;;   C-S, M-s e s in Dired (el-search-dired-marked-files)
;;     Like above but uses the marked files and directories.
;;
;;   C-S, M-s e s in Ibuffer (el-search-ibuffer-marked-buffers)
;;     Search marked buffers in *Ibuffer*.
;;
;;   C-O, M-s e o (el-search-occur)
;;     Pop up an occur buffer for the current search.
;;
;;   C-O or M-RET (from a search pattern prompt)
;;     Execute this search command as occur.
;;
;;   C-N, M-s e n (el-search-continue-in-next-buffer)
;;     Skip over current buffer or file.
;;
;;   C-D, M-s e d (el-search-skip-directory)
;;     Prompt for a directory name and skip all subsequent files
;;     located under this directory.
;;
;;   C-A, M-s e a (el-search-from-beginning)
;;     Go back to the first match in this buffer or (with prefix arg)
;;     completely restart the current search from the first file or
;;     buffer.
;;
;;   C-J, M-s e j (el-search-jump-to-search-head)
;;     Resume the last search from the position of the last visited
;;     match, or (with prefix arg) prompt for an old search to resume.
;;
;;   C-H, M-s e h (el-search-this-sexp)
;;     Grab the symbol or sexp under point and initiate an el-search
;;     for other occurrences.
;;
;;   M-x el-search-to-register
;;     Save the current search to an Emacs register.  Use C-x r j
;;     (`jump-to-register') to make that search current and jump to
;;     the latest position.
;;
;;
;; The setup you'll need for your init file is trivial: just define
;; the key bindings you want to use (all important commands are
;; autoloaded) and you are done.
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
;; Put simply, el-search is a tool to match representations of
;; symbolic expressions written in a buffer or file.  Most of the
;; time, but not necessarily, this is Elisp code.  El-search has no
;; semantic understanding of the meaning of these s-exps as a program.
;; For example, if you define a macro `my-defvar' that expands to
;; `defvar' forms, the pattern `(defvar ,_) will not match any
;; equivalent `my-defvar' form, it just matches any lists of two
;; elements with the first element being the symbol `defvar'.
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
;; all remaining matches automatically.  q quits.  And ? shows a quick
;; help summarizing all of these keys.
;;
;; It is possible to replace a match with more than one expression
;; using "splicing mode".  When it is active, the replacement
;; expression must evaluate to a list, and is spliced into the buffer
;; for any match.  Use s from the prompt to toggle splicing mode in an
;; `el-search-query-replace' session.
;;
;;
;; Multi query-replace
;; ===================
;;
;; To query-replace in multiple files or buffers at once, call
;; `el-search-query-replace' directly after starting a search whose
;; search domain is the set of files and buffers you want to treat.
;; Answer "yes" to the prompt asking whether you want the started
;; search drive the query-replace.  The user interface is
;; self-explanatory.
;;
;; It is always possible to resume an aborted query-replace session
;; even if you did other stuff in the meantime (including other
;; `el-search-query-replace' invocations).  Since internally every
;; query-replace is driven by a search, call
;; `el-search-jump-to-search-head' (maybe with a prefix arg) to make
;; that search current, and invoke `el-search-query-replace' (with the
;; default bindings, this would be C-J C-% or C-x o j %).  This will
;; continue the query-replace session from where you left.
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
;; Acknowledgments
;; ===============
;;
;; Thanks to Stefan Monnier for corrections and advice.
;;
;;
;; Known Limitations
;; =================
;;
;; - Replacing: in some cases the read syntax of forms is changing due
;;   to reading-printing.  "Some" because we can handle this problem
;;   in most cases.
;;
;; - Similar: comments are normally preserved (where it makes sense).
;;   But when replacing like `(foo ,a ,b) -> `(foo ,b ,a)
;;
;;   in a content like
;;
;;     (foo
;;       a
;;       ;; comment
;;       b)
;;
;;   the comment will be lost.
;;
;; - Something like '(1 #1#) is unmatchable (because it is
;;   un`read'able without context).  For a similar reason it is
;;   currently not possible to allow a replacement to contain
;;   uninterned symbols or repeated/circular parts.
;;
;;
;;
;; BUGS
;; ====
;;
;; - l is very slow for very long lists.  E.g. C-S-e (l "test")
;;
;; - Emacs bug#30132: 27.0.50; "scan-sexps and ##": Occurrences of the
;;   syntax "##" (a syntax for an interned symbol whose name is the
;;   empty string) can lead to errors while searching.
;;
;; - In *El Occur* buffers, when there are adjacent or nested matches,
;;   the movement commands (el-search-occur-previous-match,
;;   el-search-occur-next-match aka n and p) may skip matches, and the
;;   shown match count can be inaccurate.
;;
;;
;; TODO:
;;
;; - There should be a way to go back to the starting position, like
;;   in Isearch, which does this with (push-mark isearch-opoint t) in
;;   `isearch-done'.
;;
;; - Add a help command that can be called while searching.
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
;; - Replace: pause and warn when replacement might be wrong
;;   (ambiguous reader syntaxes; lost comments, comments that can't
;;   non-ambiguously be assigned to rewritten code)
;;
;; - There could be something much better than pp to format the
;;   replacement, or pp should be improved.
;;
;;
;; NEWS:
;;
;; Please see the NEWS file in this directory.



;;; Code:

;;;; Requirements

(eval-when-compile (require 'subr-x))
(unless (require 'rmc nil t) ;read-multiple-choice
  (require 'subr-x))

(require 'cl-lib)
(require 'pcase)    ;we want to bind `pcase--dontwarn-upats' before pcase is autoloaded
(require 'cl-print)
(require 'elisp-mode)
(require 'thingatpt)
(require 'thunk)
(require 'seq)
(require 'stream)
(require 'stream-x)
(require 'help-fns) ;el-search--make-docstring
(require 'ring)     ;el-search-history
(require 'hideshow) ;folding in *El Occur*
(require 'outline)  ;folding in *El Occur*


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

(defface el-search-highlight-in-prompt-face '((t (:inherit font-lock-variable-name-face)))
  "Face for highlighting important parts in prompts.")

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

(defcustom el-search-auto-save-buffers 'ask-multi
  "Whether to automatically save modified buffers.
When non-nil, save modified file buffers when query-replace is
finished there.

If the non-nil value is the symbol ask, ask for confirmation for
each modified file buffer.  You can still let all following
buffers automatically be saved or left unsaved from the prompt.

ask-multi is like ask, but don't ask and don't save for
single-buffer sessions.

Save automatically for any other non-nil value.

The default value is ask-multi."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On"  t)
                 (const :tag "Ask" ask)
                 (const :tag "Ask when multibuffer" ask-multi)))

(defvar el-search-use-transient-map nil
  "Whether el-search should make commands repeatable."
  ;; I originally wanted to make commands repeatable by looking at the
  ;; command keys.  But that got overly complicated: It interfered with
  ;; user interaction: we must remember in a flag if the current command
  ;; invocation was repeatable.  Obviously, we must reset that flag in
  ;; post-command-hook.  But we must avoid resetting in
  ;; post-command-hook when the command itself required user input, etc.
  ;; And it can't even work when we use a button or a register to resume
  ;; a search.  So let's simply use this flag.
  )

(defvar el-search-keep-transient-map-commands
  '(el-search-pattern
    el-search-pattern-backwards
    el-search-jump-to-search-head
    el-search-from-beginning
    el-search-skip-directory
    el-search-continue-in-next-buffer
    universal-argument universal-argument-more
    digit-argument negative-argument)
  "List of commands that don't end repeatability of el-search commands.

When `el-search-use-transient-map' is non-nil, when any
\"repeatable\" el-search command had been invoked, executing any
of these commands will keep the
`el-search-prefix-key-transient-map' further in effect.")

(defcustom el-search-allow-scroll t
  "Whether scrolling is allowed during el-search.
When non-nil, scrolling commands don't deactivate the current
search.  Unlike isearch, it's possible to scroll the current
match offscreen.  Use `el-search-jump-to-search-head' (\\[el-search-jump-to-search-head])
to go back to the position of the current match."
  :type 'boolean)

(defvar el-search-read-expression-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map read-expression-map)
    (define-key map [(control ?g)] #'abort-recursive-edit)
    (define-key map [up]   nil)
    (define-key map [down] nil)
    (define-key map [(control ?j)] #'newline)
    (define-key map [(meta return)] #'el-search-set-occur-flag-exit-minibuffer)
    (define-key map (kbd "M-RET")   #'el-search-set-occur-flag-exit-minibuffer)
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
  "Non-nil tags file visiting buffers as temporarily opened for searching.")

(defvar-local el-search--temp-file-buffer-flag nil
  "Non-nil tags (file) buffers that should not be presented to the user.
Buffers flagged this way contain the contents of a file but were
not created with `find-file-noselect'.")

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

(defun el-search--bounds-of-defun (&optional pos)
  "Return (BEG . END) of the top level s-exp covering POS.
POS defaults to point.  If no sexp is covering POS, return
nil."
  (cl-callf or pos (point))
  (save-restriction
    (widen)
    (let (defun-beg defun-end)
      (cl-flet ((top-level-paren-start
                 (pos)
                 (save-excursion
                   (let ((syntax-at-pos (syntax-ppss pos)))
                     (and (not (zerop (nth 0 syntax-at-pos)))
                          (syntax-ppss-toplevel-pos syntax-at-pos))))))
        (if (setq defun-beg
                  (or
                   ;; Iff inside a top-level paren group, this returns the defun beginning
                   (top-level-paren-start pos)
                   ;; Iff at the beginning top-level paren group, this succeeds and returns point
                   (and (not (eobp)) (top-level-paren-start (1+ pos)))))
            (cons defun-beg (scan-sexps defun-beg 1))
          ;; This corner case (not inside any s-exp or current top level s-exp
          ;; not a list) is a bit hairy to do with syntax stuff, so let's just
          ;; use el-search:
          (save-excursion
            (goto-char (point-min))
            (setq defun-beg (point-min))
            (setq defun-end (point-min))
            (while (and (<= defun-end pos)
                        (el-search-forward '_ nil t))
              (setq defun-beg (point))
              (goto-char (setq defun-end (el-search--end-of-sexp))))
            (if (<= defun-beg pos defun-end)
                (cons defun-beg defun-end)
              nil)))))))

(defun el-search-with-short-term-memory (function)
  "Wrap FUNCTION to cache the last arguments/result pair."
  (let ((cached nil))
    (lambda (&rest args)
      (pcase cached
        (`(,(pred (equal args)) . ,result) result)
        (_ (cdr (setq cached (cons args (apply function args)))))))))

(defmacro el-search-when-unwind (body-form &rest unwindforms)
  "Like `unwind-protect' but eval the UNWINDFORMS only if unwinding."
  (declare (indent 1))
  (let ((done (make-symbol "done")))
    `(let ((,done nil))
       (unwind-protect
           (prog1 ,body-form
             (setq ,done t))
         (unless ,done
           ,@unwindforms)))))

(defun el-search--message-no-log (format-string &rest args)
  "Like `message' but with `message-log-max' bound to nil."
  (let ((message-log-max nil))
    (apply #'message format-string args)))

(defalias 'el-search-read
  (if (boundp 'force-new-style-backquotes)
      (lambda (&optional stream)
        "Like `read' but bind `force-new-style-backquotes' to t."
        (let ((force-new-style-backquotes t))
          (read stream)))
    #'read))

(defun el-search--pp-to-string (expr)
  (let ((print-length nil)
        (print-level nil)
        (print-circle nil))
    (string-trim-right (pp-to-string expr))))

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
                         (ignore-errors (equal (el-search-read input) (el-search-read hist-head))))))
      (push (if (string-match-p "\\`.+\n" input)
                (with-temp-buffer
                  (emacs-lisp-mode)
                  (unless (string-match-p "\\`\n" input)
                    (insert "\n"))
                  (insert input)
                  (indent-region 1 (point))
                  (buffer-string))
              input)
            (symbol-value histvar)))))

(defun el-search--maybe-warn-about-unquoted-symbol (pattern)
  (when (and (symbolp pattern)
             (not (eq pattern '_))
             (not (keywordp pattern)))
    (message "Free variable `%S' (missing a quote?)" pattern)
    (sit-for 2.)))

(defun el-search--read-pattern (prompt &optional default histvar)
  (cl-callf or histvar 'el-search-pattern-history)
  (let ((input (el-search-read-expression prompt el-search--initial-mb-contents histvar default)))
    (el-search--pushnew-to-history input histvar)
    (if (not (string= input "")) input (car (symbol-value histvar)))))

(defun el-search-read-pattern-for-interactive (&optional prompt)
  "Read an \"el-search\" pattern from the minibuffer, prompting with PROMPT.

This function is designed to be used in the interactive form of
\"el-search\" commands that need to prompt for a pattern.  Apart
from reading the pattern it also sets `this-command' to
`el-search-pattern' and adds the given input to
`el-search-pattern-history' and `el-search-query-replace-history'.

PROMPT defaults to \"El-search pattern: \".  The return value is the
`read' input pattern."
  (let* ((input (el-search--read-pattern (or prompt "El-search pattern: ")
                                         (car el-search-pattern-history)))
         (pattern (el-search-read input)))
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
        (ignore (el-search-read (current-buffer)))
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
  (when read (setq expression (save-excursion (el-search-read (current-buffer)))))
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
            (setq res (save-excursion (el-search-read (current-buffer))))
            (setq not-done nil))
        (error (forward-char))))
    res))


(defvar el-search--pcase-macros '()
  "Alist of additional \"el-search\" pcase macros.
Keys are pattern names (i.e. symbols) and values the associated
expander functions.")

(defun el-search-defined-patterns ()
  "Return a list of defined el-search patterns."
  (mapcar #'car el-search--pcase-macros))

(put 'el-search-defined-patterns 'function-documentation
     '(el-search--make-docstring 'el-search-defined-patterns))

(defun el-search--make-docstring (name)
  ;; Code mainly from `pcase--make-docstring'
  (let* ((main (documentation (symbol-function name) 'raw))
         (ud (help-split-fundoc main name)))
    (with-temp-buffer
      (insert (or (cdr ud) main)
              "\n\nThe following additional pattern types are currently defined:")
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
  (declare (indent 2) (debug defun) (doc-string 3))
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

(defun el-search--macroexpand-1 (pattern &optional n)
  "Expand el-search PATTERN.
This is like `pcase--macroexpand' but expands only patterns
defined with `el-search-defpattern' and performs only one
expansion step.  If no entry for this pattern type exists in
`el-search--pcase-macros', PATTERN is returned.

With optional integer argument N given, successively macroexpand
N times."
  (cl-callf or n 1)
  (if-let ((expander (alist-get (car-safe pattern) el-search--pcase-macros)))
      (let ((expanded (apply expander (cdr pattern))))
        (if (<= n 1) expanded
          (el-search--macroexpand-1 expanded (1- n))))
    pattern))

(defun el-search--macroexpand (pattern)
  "Like `pcase--macroexpand' but also expanding \"el-search\" patterns."
  (eval `(el-search--with-additional-pcase-macros (pcase--macroexpand ',pattern))))

(cl-defun el-search-make-matcher (pattern &optional (result nil result-specified))
  (eval ;use `eval' to allow for user defined pattern types at run time
   (let ((expression (make-symbol "expression")))
     `(el-search--with-additional-pcase-macros
       (let ((byte-compile-debug t) ;make undefined pattern types raise an error
             (warning-suppress-log-types '((bytecomp)))
             (pcase--dontwarn-upats (cons '_ pcase--dontwarn-upats)))
         (byte-compile (lambda (,expression)
                         (pcase ,expression
                           (,pattern ,(if result-specified result t))
                           (_        nil)))))))))

(defun el-search--match-p (matcher expression)
  (funcall matcher expression))


(defun el-search--search-pattern-1 (matcher &optional noerror bound heuristic-matcher)
  "Like `el-search-forward' but accepts a matcher as first argument.
In addition, a HEURISTIC-MATCHER corresponding to the MATCHER can
be specified as third optional argument."
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

      (cond
       ((catch 'no-match
          (while (and (not match-beg) (or (not bound) (<= (point) bound)))
            (condition-case nil
                (setq current-expr (el-search--ensure-sexp-start))
              (end-of-buffer
               (goto-char opoint)
               (throw 'no-match t)))
            (let ((end-of-defun nil))
              (cond
               ((and el-search-optimized-search
                     heuristic-matcher
                     (looking-at "^(")
                     (not (funcall heuristic-matcher
                                   (current-buffer)
                                   (thunk-delay
                                    (el-search--flatten-tree
                                     (save-excursion
                                       (prog1 (el-search-read (current-buffer))
                                         (setq end-of-defun (point)))))))))
                (goto-char (or end-of-defun
                               ;; The thunk hasn't been forced
                               (scan-lists (point) 1 0))))
               ((el-search--match-p matcher current-expr)
                (setq match-beg (point)
                      opoint (point)))
               (t (el-search--skip-expression current-expr))))))
        (if noerror nil (signal 'search-failed nil)))
       ((and bound match-beg)
        (and (<= (scan-sexps match-beg 1) bound)
             match-beg))
       (t match-beg)))))

(defun el-search-forward (pattern &optional bound noerror)
  "Search for el-search PATTERN in current buffer from point.
Set point to the beginning of the occurrence found and return point.

An optional second argument bounds the search; it is a buffer
position.  The match found must not end after that position.  A
value of nil means search to the end of the accessible portion of
the buffer.

Optional third argument NOERROR, if non-nil, means if fail just
return nil (no error)."
  (el-search--search-pattern-1 (el-search-make-matcher pattern) noerror bound
                               (el-search-heuristic-matcher pattern)))


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
  get-matches ;method returning a stream of all matches
  properties  ;An alist of additional properties.  Meaningful properties
              ;are:
              ; - is-single-buffer   Indicates a single-buffer search
              ; - description        When specified, a string describing the search
  )

(defun copy-el-search-object (search)
  (let ((copy (copy-el-search-object--1 search)))
    (cl-callf copy-el-search-head (el-search-object-head copy))
    (cl-callf copy-alist (el-search-object-properties copy))
    copy))

(cl-defmethod cl-print-object ((object el-search-object) stream)
  ;; We use a syntax that looks nice with with pp.el
  (princ "#s(el-search-object " stream)
  (prin1 (el-search--get-search-description-string object 'verbose 'dont-propertize) stream)
  (princ ")" stream))

(defun el-search--current-pattern ()
  (and el-search--current-search
       (el-search-object-pattern el-search--current-search)))

(defun el-search--current-matcher ()
  (and el-search--current-search
       (el-search-head-matcher (el-search-object-head el-search--current-search))))

(defun el-search--current-heuristic-matcher ()
  (and el-search--current-search
       (el-search-head-heuristic-matcher (el-search-object-head el-search--current-search))))

(cl-defstruct el-search-head
  get-buffer-stream        ;a function of zero args returning a stream of files and/or buffers to search
  matcher                  ;for the search pattern
  heuristic-buffer-matcher ;for the search pattern
  heuristic-matcher        ;for the search pattern
  buffer                   ;currently searched buffer, or nil meaning "continue in next buffer"
  position                 ;where to continue searching this buffer
  file                     ;name of currently searched file, or nil
  buffers                  ;stream of buffers and/or files yet to search
  )

(defmacro el-search-protect-search-head (&rest body)
  "Reset current search's head when BODY exits non-locally."
  (macroexp-let2 nil head-copy '(copy-el-search-head (el-search-object-head el-search--current-search))
    `(el-search-when-unwind (progn ,@body)
                            (setf (el-search-object-head el-search--current-search) ,head-copy))))

(defun el-search--get-search-description-string (search &optional verbose dont-propertize)
  (concat
   (or (alist-get 'description (el-search-object-properties search))
       "Search")
   (when verbose
     (let ((search-head (el-search-object-head search)))
       (format " [%s %s]"
               (if (alist-get 'is-single-buffer (el-search-object-properties search))
                   "single-buffer" "paused")
               (if-let ((buffer (el-search-head-buffer search-head)))
                   (concat "-> "(if (buffer-live-p buffer)
                                    (buffer-name buffer)
                                  (if-let ((head-file (el-search-head-file search-head)))
                                      (file-name-nondirectory head-file)
                                    "killed buffer")))
                 "(completed)"))))
   " for"
   (let ((printed-pattern (el-search--pp-to-string (el-search-object-pattern search))))
     (format (if (string-match-p "\n" printed-pattern) "\n%s" " %s")
             (if dont-propertize printed-pattern
               (propertize printed-pattern 'face 'shadow))))))

(defun el-search-edit-search-description ()
  "Edit the description string of the current search.
That string appears in the printed representation of the search,
so this is mainly useful to add short notes."
  (interactive)
  (cl-callf (lambda (old-description) (read-string "New description: " old-description))
      (alist-get 'description (el-search-object-properties el-search--current-search))))


(defun el-search-kill-left-over-search-buffers (&optional not-current-buffer)
  "Kill all buffers that were opened just for searching.
Buffers where a search had been paused or aborted (e.g. by moving
the cursor) are not killed.

With NOT-CURRENT-BUFFER non-nil, inhibit the current buffer from
being killed."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer el-search--temp-buffer-flag)
      (unless (or (and not-current-buffer (eq buffer (current-buffer)))
                  (and el-search--current-search
                       (eq buffer (el-search-head-buffer
                                   (el-search-object-head el-search--current-search))))
                  (with-current-buffer buffer (el-search--pending-search-p)))
        (kill-buffer buffer)))))


(defun el-search-heuristic-matcher (pattern)
  "Return a heuristic matcher for PATTERN.

A heuristic matcher is a predicate accepting two arguments.  The
first argument is a file name or buffer.  The second argument is
a thunk (see \"thunk.el\") of a list of all of this file's or
buffer's atoms, or of the atoms of a defun (i.e. top-level
expression) in this file or buffer.  The predicate returns nil
when we can be sure that this file or buffer or defun can't
contain a match for the PATTERN, and must return non-nil else.

The idea behind heuristic matching is to speed up searching
without altering the matching behavior by discarding files or
buffers or defuns that can't contain a match.  Most search
patterns contain non-ambiguous information about properties of
atoms that must be present in a buffer or defun containing a
match, and computing a list of atoms is negligibly fast compared
to searching that buffer or defun directly.  Thus we spare
expensively searching all buffers and defuns we can sort out that
way.

When specified in an `el-search-defpattern' declaration, a
MATCHER-FUNCTION should be a function accepting the same
arguments ARGS as the defined pattern.  When called with ARGS,
this function should return either nil (meaning that for these
specific arguments no heuristic matching should be performed and
normal matching should be used) or a (fast!) function, the
\"heuristic matcher\" for this pattern, that accepts two
arguments: a file-name or buffer, and a thunk of a complete list
of atoms in that file or buffer or of a defun in it, that returns
non-nil when this file or buffer or defun could contain a match
for the pattern (NAME . ARGS), and nil when we can be sure that
it doesn't contain a match.  \"Atom\" here means anything whose
parts aren't searched by el-searching, like integers or strings,
but unlike arrays (see `el-search--atomic-p').  When in doubt,
the heuristic matcher function must return non-nil.

When el-searching is started with a certain PATTERN, a heuristic
matcher function is constructed by recursively destructuring the
PATTERN and combining the heuristic matchers of the subpatterns."
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
                                      (while t (push (el-search-read (current-buffer)) forms))
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

(defun el-search--atomic-p (object)
  (or (not (sequencep object)) (stringp object) (null object)
      (char-table-p object) (bool-vector-p object)))

(defun el-search--flatten-tree (tree)
  (let ((elements ())
        (walked-objects ;to avoid infinite recursion for circular TREEs
         (make-hash-table :test #'eq))
        (gc-cons-percentage 0.8)) ;Why is binding it here more effective than binding it more top-level?
    (cl-labels ((walker (object)
                        (if (el-search--atomic-p object)
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
  (unless keep-highlighting
    (el-search-hl-remove)
    ;; Ensure that `el-search--pending-search-p' returns nil in this
    ;; buffer even when `el-search-hl-post-command-fun' doesn't get a
    ;; chance to clean up before that call.
    (remove-hook 'post-command-hook 'el-search-hl-post-command-fun t))
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
      (while (not (or done (stream-empty-p buffer-stream)))
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
          (setq buffer (or (get-file-buffer next)
                           (let ((warning-minimum-level :error)
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
                               fresh-buffer)))))
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

;;;###autoload
(defun el-search-count-matches (pattern &optional rstart rend interactive)
  "Like `count-matches' but accepting an el-search PATTERN instead of a regexp.

Unlike `count-matches' matches \"inside\" other matches also count."
  (interactive (list (el-search-read-pattern-for-interactive "How many matches for pattern: ")
                     nil nil t))
  ;; Code is mainly adopted from `count-matches'
  (save-excursion
    (if rstart
        (if rend
            (progn
              (goto-char (min rstart rend))
              (setq rend (max rstart rend)))
          (goto-char rstart)
          (setq rend (point-max)))
      (if (and interactive (use-region-p))
	  (setq rstart (region-beginning)
		rend (region-end))
	(setq rstart (point)
	      rend (point-max)))
      (goto-char rstart))
    (let ((count 0)
          (matcher  (el-search-make-matcher      pattern))
          (hmatcher (el-search-heuristic-matcher pattern)))
      (while (and (< (point) rend)
		  (el-search--search-pattern-1 matcher t rend hmatcher))
	(cl-incf count)
	(el-search--skip-expression nil t))
      (when interactive (message "%d occurrence%s" count (if (= count 1) "" "s")))
      count)))

(defun el-search--looking-at-1 (matcher &optional allow-leading-whitespace)
  "Like `el-search-looking-at' but accepts a MATCHER as first argument."
  (if (not (derived-mode-p 'emacs-lisp-mode))
      (error "Buffer not in emacs-lisp-mode: %s" (buffer-name))
    (save-excursion
      (let ((syntax-here (syntax-ppss)) (here (point)) current-sexp)
        (and (not (or (nth 3 syntax-here) (nth 4 syntax-here)))
             (condition-case nil
                 (progn (setq current-sexp (el-search--ensure-sexp-start))
                        t)
               (end-of-buffer nil))
             (or (= here (point))
                 (and allow-leading-whitespace
                      (string-match-p "\\`[[:space:]]*\\'" (buffer-substring here (point)))))
             (el-search--match-p matcher current-sexp))))))

;;;###autoload
(defun el-search-looking-at (pattern &optional allow-leading-whitespace)
  "El-search version of `looking-at'.
Return non-nil when there is a match for PATTERN at point in the
current buffer.

With ALLOW-LEADING-WHITESPACE non-nil, the match may
be preceded by whitespace."
  (el-search--looking-at-1 (el-search-make-matcher pattern) allow-leading-whitespace))

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
  (let* ((search (el-search-reset-search (copy-el-search-object search)))
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
                                            (el-search-head-matcher head)
                                            t nil (el-search-head-heuristic-matcher head))))
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

(defun el-search--set-head-pattern (head pattern)
  (setf (el-search-head-matcher head)
        (el-search-make-matcher pattern))
  (setf (el-search-head-heuristic-matcher head)
        (el-search-heuristic-matcher pattern))
  (setf (el-search-head-heuristic-buffer-matcher head)
        (el-search-heuristic-buffer-matcher pattern))
  head)

(defun el-search-compile-pattern-in-search (search)
  (el-search--set-head-pattern (el-search-object-head    search)
                               (el-search-object-pattern search)))

(defun el-search-make-search (pattern get-buffer-stream)
  "Create and return a new `el-search-object' instance.
PATTERN is the pattern to search, and GET-BUFFER-STREAM a
function that returns a stream of buffers and/or files to search
in, in order, when called with no arguments."
  (letrec ((search
            (make-el-search-object
             :pattern pattern
             :head (make-el-search-head
                    :get-buffer-stream get-buffer-stream
                    :buffers (funcall get-buffer-stream))
             :get-matches (lambda () (el-search--all-matches search)))))
    (el-search-compile-pattern-in-search search)
    search))

(defun el-search-reset-search (search)
  "Reset SEARCH."
  (let ((head (el-search-object-head search)))
    (setf (el-search-head-buffers head)
          (funcall (el-search-head-get-buffer-stream head)))
    (setf (el-search-head-buffer head)   nil)
    (setf (el-search-head-file head)     nil)
    (setf (el-search-head-position head) nil)
    (setf (el-search-object-last-match search) nil)
    (el-search-compile-pattern-in-search search)
    search))


;;;###autoload
(defun el-search-loop-over-bindings (function)
  (cl-flet ((keybind (apply-partially #'funcall function)))

    (keybind emacs-lisp-mode-map           ?s #'el-search-pattern)
    (keybind emacs-lisp-mode-map           ?r #'el-search-pattern-backwards)
    (keybind emacs-lisp-mode-map           ?% #'el-search-query-replace)
    (keybind emacs-lisp-mode-map           ?h #'el-search-this-sexp) ;h like in "highlight" or "here"
    (keybind global-map                    ?j #'el-search-jump-to-search-head)
    (keybind global-map                    ?a #'el-search-from-beginning)
    (keybind global-map                    ?d #'el-search-skip-directory)
    (keybind global-map                    ?n #'el-search-continue-in-next-buffer)

    (keybind global-map                    ?o #'el-search-occur)

    (keybind el-search-read-expression-map ?s #'exit-minibuffer)
    (keybind el-search-read-expression-map ?r #'exit-minibuffer)
    (keybind el-search-read-expression-map ?o #'el-search-set-occur-flag-exit-minibuffer)

    (keybind isearch-mode-map              ?s #'el-search-search-from-isearch)
    (keybind isearch-mode-map              ?r #'el-search-search-backwards-from-isearch)
    (keybind isearch-mode-map              ?% #'el-search-replace-from-isearch)
    (keybind isearch-mode-map              ?o #'el-search-occur-from-isearch)

    (keybind global-map                    ?e #'el-search-emacs-elisp-sources)
    (keybind global-map                    ?l #'el-search-load-path)
    (keybind global-map                    ?b #'el-search-buffers)

    (defvar dired-mode-map)
    (defvar ibuffer-mode-map)

    (with-eval-after-load 'dired
      (keybind dired-mode-map   ?s #'el-search-dired-marked-files))
    (with-eval-after-load 'ibuffer
      (keybind ibuffer-mode-map ?s #'el-search-ibuffer-marked-buffers))))

(defvar el-search-prefix-key-transient-map
  (let ((transient-map (make-sparse-keymap)))
    (el-search-loop-over-bindings
     (lambda (_map key command)
       (when (memq command '(el-search-pattern
                             el-search-pattern-backwards
                             el-search-query-replace
                             el-search-from-beginning
                             el-search-skip-directory
                             el-search-continue-in-next-buffer
                             el-search-occur))
         (define-key transient-map (vector key) command))))
    transient-map))

(defun el-search-keep-session-command-p (command)
  "Non-nil when COMMAND should not deactivate the current search."
  (and
   el-search-allow-scroll
   (symbolp command)
   (or (get command 'isearch-scroll) ;isearch is preloaded
       (get command 'scroll-command))))

(defun el-search-prefix-key-maybe-set-transient-map ()
  (when el-search-use-transient-map
    (set-transient-map el-search-prefix-key-transient-map
                       (lambda () (or (memq this-command el-search-keep-transient-map-commands)
                                      (el-search-keep-session-command-p this-command))))))

(defun el-search-shift-bindings-bind-function (map key command)
  (define-key map `[(control ,@(if (<= ?a key ?z) `(shift ,key) `(,key)))] command))

;;;###autoload
(defun el-search-install-shift-bindings ()
  (interactive)
  (el-search-loop-over-bindings #'el-search-shift-bindings-bind-function))

(defun el-search-bind-under-prefix-key-function (prefix)
  (lambda (map key command)
    (unless (memq map (list el-search-read-expression-map isearch-mode-map))
      (define-key map `[,@(seq-into prefix 'list) ,key] command))))

;;;###autoload
(defun el-search-install-bindings-under-prefix (prefix-key)
  (el-search-loop-over-bindings
   (el-search-bind-under-prefix-key-function prefix-key))
  (setq el-search-use-transient-map t))

(defun el-search-setup-search-1 (pattern get-buffer-stream  &optional from-here setup-function)
  (setq el-search--success nil)
  (setq el-search--current-search
        (el-search-make-search pattern get-buffer-stream))
  (when setup-function (funcall setup-function el-search--current-search))
  (ring-insert el-search-history el-search--current-search)
  (when from-here (setq el-search--temp-buffer-flag nil))
  (el-search-prefix-key-maybe-set-transient-map))

(defun el-search-setup-search (pattern get-buffer-stream &optional setup-function from-here)
  "Create and start a new el-search.
PATTERN is the search pattern.  GET-BUFFER-STREAM is a function
of no arguments that should return a stream of buffers and/or
files (i.e. file names) to search in.

With optional FROM-HERE non-nil, the first buffer in this stream
should be the current buffer, and searching will start at the
current buffer's point instead of its beginning."
  (el-search-setup-search-1 pattern get-buffer-stream nil setup-function)
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


;;;; Additional pattern type definitions

(defun el-search-regexp-like-p (thing)
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
`el-search-regexp-like-p' REGEXP-LIKE matches the (only)
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
         `(lambda (,string) (let ,match-bindings (string-match ,regexp ,string))))))))

(el-search-defpattern string (&rest regexps)
  "Matches any string that is matched by all REGEXPS.
Any of the REGEXPS is `el-search-regexp-like-p'."
  (declare (heuristic-matcher
            (lambda (&rest regexps)
              (let ((matchers (mapcar #'el-search--string-matcher regexps)))
                (lambda (_ atoms-thunk)
                  (cl-some
                   (lambda (atom)
                     (and (stringp atom)
                          (cl-every (lambda (matcher) (funcall matcher atom)) matchers)))
                   (thunk-force atoms-thunk)))))))
  (el-search-defpattern--check-args "string" regexps #'el-search-regexp-like-p)
  `(and (pred stringp)
        ,@(mapcar (lambda (regexp) `(pred ,(el-search--string-matcher regexp)))
                  regexps)))

(el-search-defpattern symbol (&rest regexps)
  "Matches any symbol whose name is matched by all REGEXPS.
Any of the REGEXPS is `el-search-regexp-like-p'.

Example: to replace all symbols with names starting with \"foo-\"
to start with \"bar-\" instead, you would use
`el-search-query-replace' with a rule like this:

  (and (symbol \"\\\\`foo-\\\\(.*\\\\)\") s) >
  (intern (concat \"bar-\" (match-string 1 (symbol-name s))))"
  (declare (heuristic-matcher
            (lambda (&rest regexps)
              (let ((matchers (mapcar #'el-search--string-matcher regexps)))
                (lambda (_ atoms-thunk)
                  (cl-some
                   (lambda (atom)
                     (when-let ((symbol-name (and (symbolp atom) (symbol-name atom))))
                       (cl-every (lambda (matcher) (funcall matcher symbol-name)) matchers)))
                   (thunk-force atoms-thunk)))))))
  (el-search-defpattern--check-args "symbol" regexps #'el-search-regexp-like-p)
  `(and (pred symbolp) (app symbol-name (string ,@regexps))))

(defun el-search--contains-p (matcher expr)
  "Return non-nil when expression tree EXPR contains a match for MATCHER.
MATCHER is a matcher for the el-search pattern to match.  Recurse
on all types of sequences el-search does not treat as atomic.
Matches are not restricted to atoms; for example

  (el-search--contains-p (el-search-make-matcher ''(2 3)) '(1 (2 3)))

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
      `(app ,(apply-partially #'el-search--contains-p (el-search-make-matcher pattern))
            `(t ,,pattern)))) ; Match again to establish bindings PATTERN should create
   (t `(and ,@(mapcar (lambda (pattern) `(contains ,pattern)) patterns)))))

(defun el-search--in-buffer-matcher (&rest atoms)
  ;; We only allow atoms here because this works with heuristic matching
  ;; and allowing arbitrary patterns would produce false positives
  (let* ((hms (mapcar #'el-search-heuristic-buffer-matcher atoms))
         (test-buffer (el-search-with-short-term-memory
                       (lambda (file-name-or-buffer)
                         (let ((inhibit-message t))
                           (cl-every (lambda (hm) (funcall hm file-name-or-buffer)) hms))))))
    (lambda (file-name-or-buffer _) (funcall test-buffer file-name-or-buffer))))

(el-search-defpattern in-buffer (&rest atoms)
  "Matches anything in buffers containing all ATOMS.

This pattern type matches anything, but only in files or buffers
that contain all of the ATOMS.  In all other files and buffers it
never matches."
  (declare (heuristic-matcher #'el-search--in-buffer-matcher))
  (el-search-defpattern--check-args
   "in-buffer" atoms
   (lambda (arg)
     (pcase arg
       ((or (pred el-search--atomic-p) `',(pred el-search--atomic-p) ``,(pred el-search--atomic-p))
        t)))
   "argument not atomic")
  (let ((in-buffer-matcher (apply #'el-search--in-buffer-matcher atoms)))
    `(guard (funcall ',in-buffer-matcher (current-buffer) nil))))

(el-search-defpattern in-file (&rest atoms)
  "Synonymous with `in-buffer' for buffers with an associated file.

This is like `in-buffer' but only matches in buffers with an
associated `buffer-file-name'."
  `(and (filename) (in-buffer ,@atoms)))

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
  `(app ,(apply-partially #'el-search--match-p (el-search-make-matcher pattern))
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
                (ignore (bound-and-true-p type))
                (puthash symbol t table))))))
       (lambda (symbol) (and (symbolp symbol) (gethash symbol table)))))))

(el-search-defpattern symbol-file (regexp)
  "Matches any symbol whose `symbol-file' is matched by REGEXP.

This pattern type matches when the object is a symbol for that
`symbol-file' returns a (non-nil) FILE-NAME so that

   (file-name-sans-extension (file-name-nondirectory FILENAME)))

is matched by the `el-search-regexp-like-p' REGEXP."
  (declare
   (heuristic-matcher
    (lambda (regexp)
      (lambda (_ atoms-thunk)
        (cl-some (el-search--symbol-file-matcher
                  (copy-sequence load-history)
                  regexp)
                 (thunk-force atoms-thunk))))))
  (el-search-defpattern--check-args "symbol-file" (list regexp) #'el-search-regexp-like-p)
  (let ((this (make-symbol "this")))
    `(and ,this
          (guard (funcall (el-search--symbol-file-matcher (copy-sequence load-history)
                                                          ',regexp)
                          ,this)))))

(defun el-search--filename-matcher (&rest regexps)
  ;; Return a file name matcher for the REGEXPS.  This is a predicate
  ;; accepting two arguments that returns non-nil when the first
  ;; argument is a file name (i.e. a string) that is matched by all
  ;; `el-search-regexp-like-p' REGEXPS, or a buffer whose associated file
  ;; name matches accordingly.  It ignores the second argument.
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

With any `el-search-regexp-like-p' REGEXPS given, the file's
absolute name must be matched by all of them."
  ;;FIXME: should we also allow to match the f-n-nondirectory and
  ;;f-n-sans-extension?  Maybe it could become a new pattern type named `feature'?
  (declare (heuristic-matcher #'el-search--filename-matcher)
           (inverse-heuristic-matcher t))
  (el-search-defpattern--check-args "filename" regexps #'el-search-regexp-like-p)
  (let ((file-name-matcher (apply #'el-search--filename-matcher regexps)))
    ;; We can't expand to just t because this would not work with `not'.
    ;; `el-search--filename-matcher' caches the result, so this is still a
    ;; pseudo constant
    `(guard (funcall ',file-name-matcher (current-buffer) nil))))


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
        (condition-case nil
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
                           3)))
          ((beginning-of-buffer end-of-buffer) nil)))))

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

(defvar-local el-search--buffer-match-count-data nil
  "Holds information for displaying a match count.
The value is a list of elements

   \(SEARCH BUFFER-CHARS-MOD-TICK BUFFER-MATCHES\)

BUFFER-MATCHES is a stream of matches in this buffer.  SEARCH is
the active search and BUFFER-CHARS-MOD-TICK the return value of
`buffer-chars-modified-tick' from when this stream had been
created.")

(defun el-search-display-match-count ()
  "Display an x/y-style match count in the echo area."
  (when (and el-search--success (not el-search--wrap-flag))
    (while-no-input

      ;; Check whether cached stream of buffer matches is still valid
      (pcase el-search--buffer-match-count-data
        (`(,(pred (eq el-search--current-search))  ,(pred (eq (buffer-chars-modified-tick)))  . ,_))
        (_
         ;; (message "Refreshing match count data") (sit-for 1)
         (redisplay) ;don't delay highlighting
         (setq-local el-search--buffer-match-count-data
                     (let ((stream-of-buffer-matches
                            (seq-map #'cadr
                                     (el-search--all-matches
                                      (el-search-make-search
                                       (el-search--current-pattern)
                                       (let ((current-buffer (current-buffer)))
                                         (lambda () (stream (list current-buffer)))))))))
                       (list
                        el-search--current-search
                        (buffer-chars-modified-tick)
                        stream-of-buffer-matches)))))

      (let ((nbr-this-match 1) total-matches (pos-here (point))
            (defun-bounds (or (el-search--bounds-of-defun) (cons (point) (point))))
            (nbr-this-match-in-defun 1) (total-matches-in-defun 0)
            (largest-match-start-not-after-pos-here nil))
        (pcase-let ((`(,_ ,_ ,matches) el-search--buffer-match-count-data))
          (setq total-matches (let ((inhibit-message t)) (seq-length matches)))
          (while (and (not (stream-empty-p matches)) (< (stream-first matches) (cdr defun-bounds)))
            (when (<= (stream-first matches) pos-here)
              (setq largest-match-start-not-after-pos-here (stream-first matches))
              (unless (= (stream-first matches) pos-here)
                (cl-incf nbr-this-match)))
            (when (<= (car defun-bounds) (stream-first matches))
              (cl-incf total-matches-in-defun)
              (when (< (stream-first matches) pos-here)
                (cl-incf nbr-this-match-in-defun)))
            (stream-pop matches))
          (if (zerop total-matches) ;this can happen for el-search-this-sexp
              (el-search--message-no-log "No matches")
            (let* ((at-a-match-but-not-at-match-beginning
                    (and largest-match-start-not-after-pos-here
                         (and (< largest-match-start-not-after-pos-here pos-here)
                              (save-excursion
                                (goto-char largest-match-start-not-after-pos-here)
                                (<= pos-here (el-search--end-of-sexp))))))
                   (at-a-match
                    (and largest-match-start-not-after-pos-here
                         (or (= pos-here largest-match-start-not-after-pos-here)
                             at-a-match-but-not-at-match-beginning))))
              (when at-a-match-but-not-at-match-beginning
                (cl-decf nbr-this-match)
                (cl-decf nbr-this-match-in-defun))
              (if at-a-match
                  (el-search--message-no-log
                   "%s  %d/%d    %s"
                   (let ((head (el-search-object-head el-search--current-search)))
                     (or (el-search-head-file head)
                         (buffer-name (el-search-head-buffer head))))
                   nbr-this-match
                   total-matches
                   (if (<= total-matches-in-defun 1)
                       ""
                     (propertize (format "(%d/%d)" nbr-this-match-in-defun total-matches-in-defun)
                                 'face 'shadow)))
                (el-search--message-no-log "[Not at a match]")))))))))

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
  (pcase this-command
    ('el-search-query-replace)
    ('el-search-pattern (el-search-display-match-count))
    ((pred el-search-keep-session-command-p))
    (_ (unless el-search-keep-hl
         (el-search-hl-remove)
         (remove-hook 'post-command-hook 'el-search-hl-post-command-fun t)
         (setq el-search--temp-buffer-flag nil)
         (el-search-kill-left-over-search-buffers)))))

(defun el-search--pending-search-p ()
  (memq #'el-search-hl-post-command-fun post-command-hook))

(defun el-search--reset-wrap-flag ()
  (unless (or (eq this-command 'el-search-query-replace)
              (eq this-command 'el-search-pattern))
    (remove-hook 'post-command-hook 'el-search--reset-wrap-flag)
    (setq el-search--wrap-flag nil)))

(defun el-search--set-wrap-flag (value)
  (when value
    (add-hook 'post-command-hook #'el-search--reset-wrap-flag t))
  (setq el-search--wrap-flag value))


;;;; Core functions

(defun el-search-continue-in-next-buffer ()
  "Skip current search buffer and continue with the next."
  (interactive)
  (el-search--skip-to-next-buffer))

(defun el-search-jump-to-search-head (&optional previous-search)
  "Switch to current search buffer and go to the last match.
With argument PREVIOUS-SEARCH non-nil (the prefix argument in an
interactive call), prompt for a prior search to resume, and make
that the current search.  In a non-interactive call,
PREVIOUS-SEARCH can directly specify an el-search-object to make
current."
  (interactive "P")
  (when previous-search
    ;; FIXME: would it be better to include some context around the search
    ;; head - or to even use an overview buffer for selection?
    (setq el-search--current-search
          (if (el-search-object-p previous-search) previous-search
            (ring-ref
             el-search-history
             (let ((input
                    (completing-read
                     "Resume previous search: "
                     (mapcar
                      (lambda (n) (format "%d - %s"
                                     n
                                     (el-search--get-search-description-string
                                      (ring-ref el-search-history n)
                                      t)))
                      (number-sequence 0 (1- (ring-length el-search-history)))))))
               (string-match "\\`\\([0-9]+\\) - " input)
               (string-to-number (match-string 1 input))))))
    (setq el-search--success t)
    (el-search--set-wrap-flag nil))
  (el-search-compile-pattern-in-search el-search--current-search)
  (if-let ((search el-search--current-search)
           (current-head (el-search-object-head search))
           (current-search-buffer (el-search-head-buffer current-head)))
      (if (not (buffer-live-p current-search-buffer))
          (let* ((head-file-name (el-search-head-file current-head))
                 (search (el-search-reset-search search))
                 (buffer-stream (el-search-head-buffers (el-search-object-head search)))
                 (buffer-stream-from-head-file
                  (let ((inhibit-message t))
                    (and head-file-name
                         (cadr (stream-divide-with-get-rest-fun
                                buffer-stream
                                (lambda (s)
                                  (while (and (not (stream-empty-p s))
                                              (or (not (stringp (stream-first s)))
                                                  (not (file-equal-p (stream-first s) head-file-name))))
                                    (stream-pop s))
                                  s)))))))
            (message "Search head points to a killed buffer...")
            (sit-for 1)
            (if (or (not head-file-name)
                    (stream-empty-p buffer-stream-from-head-file))
                (el-search--message-no-log "Restarting search...")
              (setf (el-search-head-buffers (el-search-object-head search))
                    buffer-stream-from-head-file)
              (message "Restarting from %s..." (file-name-nondirectory head-file-name)))
            (sit-for 2)
            (el-search-continue-search))
        (setq this-command 'el-search-pattern)
        (pop-to-buffer current-search-buffer el-search-display-buffer-popup-action)
        (let ((last-match (el-search-object-last-match search)))
          (if (not (and last-match
                        (eq (marker-buffer last-match) (current-buffer))))
              ;; this should only happen for bad search patterns
              (goto-char (el-search-head-position current-head))
            (goto-char last-match))
          (let ((match-pos
                 (save-excursion
                   (el-search--search-pattern-1
                    (el-search--current-matcher) t nil (el-search--current-heuristic-matcher)))))
            (unless (eq (point) match-pos)
              (message "No match at search head any more - going to the next match")
              (redisplay)
              ;; Don't just `sit-for' here: `pop-to-buffer' may have generated frame
              ;; focus events
              (sleep-for 1.5))
            (if (not match-pos)
                (el-search-continue-search)
              (goto-char match-pos)
              (setf (el-search-head-position current-head)
                    (copy-marker (point)))
              (setf (el-search-object-last-match el-search--current-search)
                    (copy-marker (point)))
              (el-search-hl-sexp)
              (el-search-hl-other-matches (el-search--current-matcher))
              (el-search-prefix-key-maybe-set-transient-map)))))
    (el-search--message-no-log "[Search completed - restarting]")
    (sit-for 1.5)
    (el-search-reset-search el-search--current-search)
    (el-search-continue-search)))

(defun el-search-continue-search (&optional from-here)
  "Continue or resume the current search.

With prefix arg FROM-HERE given, the current search buffer should
be the current buffer, and the search will be resumed from point
instead of the position where the search would normally be
continued."
  (interactive "P")
  (setq this-command 'el-search-pattern)
  (el-search-compile-pattern-in-search el-search--current-search)
  (el-search-protect-search-head
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
               (matcher (el-search--current-matcher))
               (heuristic-matcher (el-search--current-heuristic-matcher)))
           (while (and (el-search-head-buffer head)
                       (not (setq match (with-current-buffer (el-search-head-buffer head)
                                          (save-excursion
                                            (goto-char (el-search-head-position head))
                                            (el-search--search-pattern-1
                                             matcher t nil heuristic-matcher))))))
             (el-search--next-buffer el-search--current-search))
           (if (not match)
               (progn
                 (if (not (or el-search--success
                              (and from-here
                                   (save-excursion
                                     (goto-char (point-min))
                                     (el-search--search-pattern-1 matcher t nil heuristic-matcher)))))
                     (progn
                       (el-search--message-no-log "No matches")
                       (sit-for .7))
                   (el-search--set-wrap-flag 'forward)
                   (let ((keys (car (where-is-internal 'el-search-pattern))))
                     (el-search--message-no-log
                      (if keys
                          (format "No (more) matches - Hit %s to wrap search"
                                  (key-description keys))
                        "No (more) matches")))))
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
             (setq el-search--success t)))
         (el-search-prefix-key-maybe-set-transient-map))
     (el-search-kill-left-over-search-buffers))))

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

(defun el-search-pattern--interactive (&optional prompt)
  (list (if (or
             ;;Hack to make a pop-up buffer search from occur "stay active"
             (el-search--pending-search-p)
             (and (eq this-command last-command)
                  (or el-search--success el-search--wrap-flag)))
            (el-search--current-pattern)
          (el-search-read-pattern-for-interactive prompt))))

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
types defined with `el-search-defpattern'.

See `el-search-defined-patterns' for a list of defined patterns."
  (declare (interactive-only el-search-forward))
  (interactive (el-search-pattern--interactive))
  (cond
   ((eq el-search--wrap-flag 'forward)
    (progn
      (el-search--set-wrap-flag nil)
      (el-search--message-no-log "[Wrapped search]")
      (sit-for .7)
      (el-search-from-beginning 'restart)))
   ((or
     (el-search--pending-search-p)
     (and (eq this-command last-command)
          (eq pattern (el-search--current-pattern))))
    (progn
      (el-search--skip-expression nil t)
      (el-search-continue-search 'from-here)))
   (t ;create a new search single-buffer search
    (el-search-setup-search
     pattern
     (let ((current-buffer (current-buffer)))
       (lambda () (stream (list current-buffer))))
     (lambda (search) (setf (alist-get 'is-single-buffer (el-search-object-properties search)) t))
     'from-here))))

(defun el-search-from-beginning (&optional restart-search)
  "Go to the first of this buffer's matches.
With prefix arg, restart the current search."
  (interactive "P")
  (if (not restart-search)
      (setf (el-search-head-position (el-search-object-head el-search--current-search))
            (point-min))
    (el-search-reset-search el-search--current-search)
    (setq el-search--success nil))
  (el-search-continue-search))

;;;###autoload
(defun el-search-pattern-backwards (pattern)
  "Search the current buffer backwards for matches of PATTERN."
  (declare (interactive-only t));; FIXME: define noninteractive version - and -1 with hms like
                                ;; `el-search--search-pattern-1'
  (interactive (el-search-pattern--interactive))
  (if (eq pattern (el-search--current-pattern))
      (progn
        (el-search-compile-pattern-in-search el-search--current-search)
        (el-search-prefix-key-maybe-set-transient-map))
    (el-search-setup-search-1
     pattern
     (let ((current-buffer (current-buffer)))
       (lambda () (stream (list current-buffer))))
     'from-here
     (lambda (search) (setf (alist-get 'is-single-buffer (el-search-object-properties search)) t)))
    ;; Make this buffer the current search buffer so that a following C-S
    ;; doesn't delete highlighting
    (el-search--next-buffer el-search--current-search))
  (setq this-command 'el-search-pattern)
  (when (eq el-search--wrap-flag 'backward)
    (el-search--set-wrap-flag nil)
    (el-search--message-no-log "[Wrapped backward search]")
    (sit-for .7)
    (goto-char (point-max)))
  (let ((outer-loop-done nil)
        (original-point (point))
        (matcher (el-search--current-matcher))
        (heuristic-matcher (el-search--current-heuristic-matcher)))
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
                             (el-search--search-pattern-1 matcher t nil heuristic-matcher))))
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
                (el-search--set-wrap-flag 'backward)))
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

(define-obsolete-function-alias 'el-search-previous-match
  'el-search-pattern-backwards "since el-search-1.3")

;;;###autoload
(defun el-search-this-sexp (sexp)
  "Prepare to el-search the `sexp-at-point'.

Grab the `sexp-at-point' SEXP and prepare to el-search the
current buffer for other matches of 'SEXP.

Use the normal search commands to seize the search."
  (interactive (list (if (not (derived-mode-p 'emacs-lisp-mode))
                         (user-error "Current buffer not in `emacs-lisp-mode'")
                       (let ((symbol-at-point-text (thing-at-point 'symbol))
                             symbol-at-point)
                         (if (and symbol-at-point-text
                                  ;; That should ideally be always true but isn't
                                  (condition-case nil
                                      (symbolp (setq symbol-at-point (el-search-read symbol-at-point-text)))
                                    (invalid-read-syntax nil)))
                             symbol-at-point
                           (if (thing-at-point 'sexp)
                               (sexp-at-point)
                             (user-error "No sexp at point")))))))
  (let ((printed-sexp (el-search--pp-to-string sexp)))
    (el-search--pushnew-to-history (concat "'" printed-sexp) 'el-search-pattern-history)
    (el-search-setup-search-1
     `',sexp
     (let ((current-buffer (current-buffer)))
       (lambda () (stream (list current-buffer))))
     'from-here
     (lambda (search) (setf (alist-get 'is-single-buffer (el-search-object-properties search)) t)))
    (el-search--next-buffer el-search--current-search)
    (setf (el-search-head-position (el-search-object-head el-search--current-search))
          (copy-marker (point)))
    (setq this-command 'el-search-pattern
          el-search--success t)
    (el-search-hl-other-matches (el-search--current-matcher))
    (add-hook 'post-command-hook #'el-search-hl-post-command-fun t t)
    (el-search--message-no-log "%s" printed-sexp)))


;;;; El-Occur

(defvar-local el-search-occur-search-object nil)


(defun el-search-occur-revert-function (&rest _)
  (el-search--occur el-search-occur-search-object t))

(defun el-search-edit-occur-pattern (new-pattern)
  "Change the search pattern associated with this *El Occur* buffer.
Prompt for a new pattern and revert."
  (interactive (list (let ((el-search--initial-mb-contents
                            (el-search--pp-to-string
                             (el-search-object-pattern el-search-occur-search-object))))
                       (el-search-read-pattern-for-interactive "New pattern: "))))
  (setf (el-search-object-pattern el-search-occur-search-object)
        new-pattern)
  (el-search-compile-pattern-in-search el-search-occur-search-object)
  (revert-buffer))

(defun el-search-occur-jump-to-match ()
  (interactive)
  (if (button-at (point))
      (push-button)
    (if-let ((params  (get-char-property (point) 'el-search-match)))
        (apply #'el-search--occur-button-action params)
      ;; User clicked not directly on a match
      (catch 'nothing-here
        (let ((clicked-pos (point)) (done nil) some-match-pos)
          (save-excursion
            (pcase (el-search--bounds-of-defun)
              ('nil
               (throw 'nothing-here t))
              (`(,defun-beg . ,defun-end)
               (unless (< defun-end (point)) (goto-char defun-beg))))
            ;; Try to find corresponding position in source buffer
            (setq some-match-pos (point))
            (while (and (not done) (setq some-match-pos (funcall #'next-single-char-property-change
                                                                 some-match-pos 'el-search-match)))
              (setq done (or (memq some-match-pos (list (point-min) (point-max)))
                             (cl-some (lambda (ov) (overlay-get ov 'el-search-match))
                                      (overlays-at some-match-pos))))))
          (let ((delta-lines (count-lines clicked-pos some-match-pos)))
            (when (save-excursion
                    (goto-char (max clicked-pos some-match-pos))
                    (not (bolp)))
              (cl-decf delta-lines))
            (when (< clicked-pos some-match-pos)
              (cl-callf - delta-lines))
            (pcase-let ((`(,file-name-or-buffer ,pos)
                         (get-char-property some-match-pos 'el-search-match)))
              (el-search--occur-button-action
               file-name-or-buffer nil
               (lambda ()
                 (goto-char pos)
                 (beginning-of-line)
                 (forward-line delta-lines))
               '()))))))))

(cl-defun el-search--occur-button-action
    (filename-or-buffer &optional match-pos do-fun (display-buffer-action nil action-specified))
  (let ((buffer (if (bufferp filename-or-buffer)
                    filename-or-buffer
                  (find-file-noselect filename-or-buffer)))
        (search-pattern (el-search-object-pattern el-search-occur-search-object)))
    (with-selected-window (display-buffer
                           buffer
                           (cond
                            (action-specified display-buffer-action)
                            (match-pos        '((display-buffer-pop-up-window)))
                            (t                el-search-display-buffer-popup-action)))
      (when match-pos
        (when (and (buffer-narrowed-p)
                   (or (< match-pos (point-min))
                       (> match-pos (point-max)))
                   (not (and (y-or-n-p "Widen buffer? ")
                             (progn (widen) t))))
          (user-error "Can't jump to match"))
        (goto-char match-pos))
      (el-search-setup-search-1
       search-pattern
       (lambda () (stream (list buffer)))
       'from-here
       (lambda (search)
         (setf (alist-get 'is-single-buffer (el-search-object-properties search))
               t)))
      (el-search--next-buffer el-search--current-search)
      (setq this-command 'el-search-pattern
            el-search--success t)
      (when match-pos
        (el-search-hl-sexp)
        (el-search-display-match-count))
      (el-search-hl-other-matches (el-search--current-matcher))
      (add-hook 'post-command-hook #'el-search-hl-post-command-fun t t)
      (when do-fun (funcall do-fun)))))

(defun el-search-occur--next-match (&optional backwards)
  (let ((done nil) (pos (point)))
    (when-let ((this-ov (cl-some (lambda (ov) (and (overlay-get ov 'el-search-match) ov))
                                 (overlays-at pos))))
      (setq pos (funcall (if backwards #'overlay-start #'overlay-end) this-ov)))
    (while (and (not done) (setq pos (funcall (if backwards #'previous-single-char-property-change
                                                #'next-single-char-property-change)
                                              pos 'el-search-match)))
      (setq done (or (memq pos (list (point-min) (point-max)))
                     (cl-some (lambda (ov) (overlay-get ov 'el-search-match))
                              (overlays-at pos)))))
    (if (memq pos (list (point-min) (point-max)))
        (progn
          (el-search--message-no-log "No match %s this position" (if backwards "before" "after"))
          (sit-for 1.5))
      (goto-char pos)
      (save-excursion (hs-show-block))))
  (el-search-occur--show-match-count))

(defvar el-search-occur--total-matches nil)

(defun el-search-occur--show-match-count ()
  (while-no-input
    (let ((nbr-match 0)
          (pos (point))
          (match-here-p (lambda () (get-char-property (point) 'el-search-match))))
      (when (funcall match-here-p)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (< (point) pos)
              (goto-char (next-single-char-property-change (point) 'el-search-match))
              (when (funcall match-here-p)
                (cl-incf nbr-match)))
            (el-search--message-no-log
             "Match %d/%d" nbr-match el-search-occur--total-matches)))))))

(defun el-search-occur-next-match ()
  "Move point to the next match."
  (interactive)
  (el-search-occur--next-match))

(defun el-search-occur-previous-match ()
  "Move point to the previous match."
  (interactive)
  (el-search-occur--next-match 'backwards))


(defvar el-search-occur--outline-visible t)

(defun el-search-occur-cycle ()
  "Cycle between showing an outline and everything."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if el-search-occur--outline-visible
        (outline-hide-leaves)
      (outline-show-all)))
  (cl-callf not el-search-occur--outline-visible))

(defun el-search-occur-tab-command ()
  "Hide or unhide heading or sexp at point."
  (interactive)
  (call-interactively
   (if (not (outline-on-heading-p))
       #'hs-toggle-hiding
     #'outline-toggle-children)))

(defvar el-search-occur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]           #'el-search-occur-tab-command)
    (define-key map "\t"            #'el-search-occur-tab-command)
    (define-key map [return]        #'el-search-occur-jump-to-match)
    (define-key map "\r"            #'el-search-occur-jump-to-match)
    (define-key map [S-iso-lefttab] #'el-search-occur-cycle)
    (define-key map [(shift tab)]   #'el-search-occur-cycle)
    (define-key map [?p]            #'el-search-occur-previous-match)
    (define-key map [?n]            #'el-search-occur-next-match)
    (define-key map [?e]            #'el-search-edit-occur-pattern)
    (define-key map [?c ?n]         #'el-search-occur-no-context)
    (define-key map [?c ?d]         #'el-search-occur-defun-context)
    (define-key map [?c ?a]         #'el-search-occur-defun-context)
    (define-key map [?c ?s]         #'el-search-occur-some-context)
    (set-keymap-parent map (make-composed-keymap special-mode-map emacs-lisp-mode-map))
    map))

(define-derived-mode el-search-occur-mode emacs-lisp-mode "El-Occur"
  (setq-local revert-buffer-function #'el-search-occur-revert-function)
  (setq buffer-read-only t)
  (setq-local hs-hide-comments-when-hiding-all nil)
  (hs-minor-mode +1)
  (hs-hide-all)
  (setq outline-regexp "^;;;\\ \\*+")
  (outline-minor-mode +1))

(put 'el-search-occur-mode 'mode-class 'special)

(defun el-search-occur-get-some-context (match-beg)
  (let ((context-beg nil)
        (need-more-context-p
         (lambda (start)
           (let (end)
             (pcase (save-excursion
                      (goto-char start)
                      (prog1 (el-search-read (current-buffer))
                        (setq end (point))))
               ((or (pred atom) `(,(pred atom))) t)
               ((guard (< (- end start) 100))    t)))))
        (try-go-upwards (lambda (pos) (condition-case nil
                                     (when-let ((pos (scan-lists pos -1 1)))
                                       (if (eq (char-before pos) ?`) (1- pos) pos))
                                   (scan-error nil)))))
    (when (funcall need-more-context-p match-beg)
      (setq context-beg (funcall try-go-upwards match-beg))
      (when (and context-beg (funcall need-more-context-p context-beg))
        (setq context-beg (or (funcall try-go-upwards context-beg)
                              context-beg))))
    (cons (or context-beg match-beg)
          (if context-beg (scan-lists context-beg 1 0)
            (scan-sexps match-beg 1)))))

(defun el-search-occur-get-defun-context (match-beg)
  (el-search--bounds-of-defun match-beg))

(defun el-search-occur-get-null-context (match-beg)
  (cons match-beg (scan-sexps match-beg 1)))

(defvar el-search-get-occur-context-function #'el-search-occur-get-some-context
  "Function determining amount of context shown in *El Occur* buffers.")

(defun el-search-occur-defun-context ()
  "Show complete top-level expressions in *El Occur*."
  (interactive)
  (setq el-search-get-occur-context-function #'el-search-occur-get-defun-context)
  (revert-buffer))

(defun el-search-occur-no-context ()
  "Show no context around matches in *El Occur*."
  (interactive)
  (setq el-search-get-occur-context-function #'el-search-occur-get-null-context)
  (revert-buffer))

(defun el-search-occur-some-context ()
  "Show some context around matches in *El Occur*."
  (interactive)
  (setq el-search-get-occur-context-function #'el-search-occur-get-some-context)
  (revert-buffer))

(declare-function which-func-ff-hook which-func)

(defun el-search--occur (search &optional buffer)
  (unwind-protect
      (let ((occur-buffer (or buffer (generate-new-buffer "*El Occur*"))))
        (setq this-command 'el-search-pattern)
        (setq-local el-search--temp-buffer-flag nil)
        (with-selected-window (if buffer (selected-window) (display-buffer occur-buffer))
          (let ((inhibit-read-only t))
            (if el-search-occur-search-object
                (progn
                  (erase-buffer)
                  (delete-all-overlays))
              (el-search-occur-mode)
              (setq el-search-occur-search-object search))
            (insert (format ";;; * %s   -*- mode: el-search-occur -*-\n\n;;; ** %s\n\n"
                            (current-time-string)
                            (el-search--get-search-description-string search)))
            (condition-case-unless-debug err
                (let ((insert-summary-position (point))
                      (stream-of-matches
                       (stream-partition
                        (funcall (el-search-object-get-matches search))
                        (lambda (this prev)
                          (and (eq (car this) (car prev)) (equal (nth 2 this) (nth 2 prev))))))
                      stream-of-buffer-matches  buffer-matches
                      (matching-files 0) (matching-buffers 0) (overall-matches 0))
                  (while (setq stream-of-buffer-matches (stream-pop stream-of-matches))
                    (setq buffer-matches (seq-length stream-of-buffer-matches))
                    (cl-incf overall-matches buffer-matches)
                    (pcase-let ((`(,buffer ,_ ,file) (stream-first stream-of-buffer-matches)))
                      (if file (cl-incf matching-files) (cl-incf matching-buffers))
                      (insert "\n\n;;; *** ")
                      (insert-button
                       (or file (format "%S" buffer))
                       'action (lambda (_) (el-search--occur-button-action (or file buffer))))
                      (insert (format "  (%d match%s)\n"
                                      buffer-matches
                                      (if (> buffer-matches 1) "es" "")))
                      (let ((buffer-matches+contexts
                             (seq-map (pcase-lambda ((and match `(,_ ,match-beg ,_)))
                                        (with-current-buffer buffer
                                          (cons match
                                                (let ((open-paren-in-column-0-is-defun-start nil))
                                                  (save-excursion
                                                    (funcall el-search-get-occur-context-function
                                                             match-beg))))))
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
                                      (car (stream-divide-with-get-rest-fun
                                            buffer-matches+contexts
                                            (lambda (_) remaining-buffer-matches-+contexts))))
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
                                             (overlay-put ov 'face 'el-search-match)
                                             (overlay-put
                                              ov 'el-search-match (list (or file buffer) match-beg)))
                                           (with-current-buffer buffer (point)))))
                                (insert (format "\n;;;; Line %d\n"
                                                (with-current-buffer buffer
                                                  (line-number-at-pos context-beg))))
                                (setq insertion-point (point))
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

                  (save-excursion
                    (goto-char insert-summary-position)
                    (setq el-search-occur--total-matches overall-matches)
                    (insert
                     (if (zerop overall-matches)
                         ";;; * No matches"
                       (concat
                        (format ";;; ** %d matches in " overall-matches)
                        (unless (zerop matching-files) (format "%d files" matching-files))
                        (unless (or (zerop matching-files) (zerop matching-buffers)) " and ")
                        (unless (zerop matching-buffers)  (format "%d buffers" matching-buffers))
                        (unless (zerop overall-matches) ":\n\n")))))
                  (goto-char (point-min))
                  (when (and (bound-and-true-p which-function-mode)
                             (eq el-search-get-occur-context-function
                                 #'el-search-occur-get-defun-context))
                    (which-func-ff-hook)))
              (quit  (insert "\n\n;;; * Aborted"))
              (error (insert "\n\n;;; * Error: " (error-message-string err)
                             "\n\
;;; If you think this error could be caused by a bug in
;;; el-search, please make a bug report to the maintainer.
;;; Thanks!")))
            (el-search--message-no-log "")
            (set-buffer-modified-p nil))))
    (el-search-kill-left-over-search-buffers)))

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
  (el-search--message-no-log "Preparing occur...")
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
  (interactive
   (list (el-search-read-pattern-for-interactive "Search elisp buffers for pattern: ")))
  (el-search-setup-search
   pattern
   (lambda ()
     (seq-filter
      (lambda (buffer) (with-current-buffer buffer (and (derived-mode-p 'emacs-lisp-mode)
                                                   (not (eq major-mode 'el-search-occur-mode)))))
      (stream (buffer-list))))
   (lambda (search) (setf (alist-get 'description (el-search-object-properties search))
                     "el-search-buffers"))))

;;;###autoload
(defun el-search-directory (pattern directory &optional recursively)
  "Search all elisp files in DIRECTORY for PATTERN.
With prefix arg RECURSIVELY non-nil, search subdirectories recursively."
  (interactive (list (el-search-read-pattern-for-interactive "Search dir for pattern: ")
                     (expand-file-name
                      (read-directory-name (format "el-search directory%s: "
                                                   (if current-prefix-arg " recursively" ""))
                                           nil default-directory t))
                     current-prefix-arg))
  (el-search-setup-search
   pattern
   (lambda () (el-search-stream-of-directory-files directory recursively))
   (lambda (search) (setf (alist-get 'description (el-search-object-properties search))
                     (concat (if recursively "Recursive directory search in "
                               "Directory search in ")
                             directory)))))

;;;###autoload
(defun el-search-emacs-elisp-sources (pattern)
  "Search Emacs elisp sources for PATTERN.
This command recursively searches all elisp files under
`source-directory'."
  (interactive (list (el-search-read-pattern-for-interactive
                      "Search Elisp sources for pattern: ")))
  (el-search-setup-search
   pattern
   (lambda ()
     (seq-filter
      #'el-search--elisp-file-p
      (el-search-stream-of-directory-files source-directory t)))
   (lambda (search) (setf (alist-get 'description (el-search-object-properties search))
                     "Search the Emacs Elisp sources"))))

;;;###autoload
(defun el-search-load-path (pattern)
  "Search PATTERN in all elisp files in all directories in `load-path'.
nil elements in `load-path' (standing for `default-directory')
are ignored."
  (interactive (list (el-search-read-pattern-for-interactive
                      "Search load path for pattern: ")))
  (el-search-setup-search
   pattern
   (lambda ()
     (stream-concatenate
      (seq-map (lambda (path) (el-search-stream-of-directory-files path nil))
               (stream (remq nil load-path)))))
   (lambda (search) (setf (alist-get 'description (el-search-object-properties search))
                     "Search `load-path'"))))

(declare-function dired-get-marked-files "dired")

;;;###autoload
(defun el-search-dired-marked-files (pattern files &optional recursively)
  "El-search files and directories marked in dired.
With RECURSIVELY given (the prefix arg in an interactive call),
search directories recursively.

This function uses `el-search-stream-of-directory-files' to
compute a the file stream - see there for a description of
related user options."
  (interactive (list (el-search-read-pattern-for-interactive
                      "Search marked files for pattern: ")
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
       (stream files))))
   (lambda (search) (setf (alist-get 'description (el-search-object-properties search))
                     "el-search-dired-marked-files"))))

(declare-function ibuffer-get-marked-buffers 'ibuffer)

;;;###autoload
(defun el-search-ibuffer-marked-buffers (pattern buffer-names)
  "El-search the buffers marked in *Ibuffer*."
  (interactive
   (list (el-search-read-pattern-for-interactive
          "Search marked files for pattern: ")
         ;; Return a list of buffer names here so that `repeat-complex-command'
         ;; works ok
         (mapcar #'buffer-name (ibuffer-get-marked-buffers))))
  (el-search-setup-search
   pattern
   (lambda () (stream (delq nil (mapcar #'get-buffer buffer-names))))
   (lambda (search) (setf (alist-get 'description (el-search-object-properties search))
                     "el-search-ibuffer-marked-files"))))

;;;; Register usage

(defun el-search-to-register (register &optional el-search-object)
  "Prompt for a register and save the EL-SEARCH-OBJECT to it.
In an interactive call or when EL-SEARCH-OBJECT is nil, the
current search is used."
  (interactive (list (if el-search--current-search
                         (register-read-with-preview "Save current search to register: ")
                       (user-error "No search has been started yet"))))
  (set-register register (or el-search-object el-search--current-search)))

(defun el-search-clone-to-register (register &optional el-search-object)
  "Prompt for a register and save a clone of the EL-SEARCH-OBJECT to it.
In an interactive call or when EL-SEARCH-OBJECT is nil, the
current search is used.

This is similar to `el-search-to-register' but what is saved is a
clone with an individual state."
  (interactive (list (if el-search--current-search
                         (register-read-with-preview "Save clone of current search to register: ")
                       (user-error "No search has been started yet"))))
  (set-register register (copy-el-search-object (or el-search-object el-search--current-search))))

(cl-defmethod register-val-jump-to ((val el-search-object) _arg)
  (el-search-jump-to-search-head val))

(cl-defmethod register-val-describe ((val el-search-object) _verbose) ;VERBOSE is only used by C-x r v
  (let ((print-circle nil)) ;bug#30070
    (cl-prin1 val)))


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
      (if (string= to-insert "")
          ;; We deleted the match.  Clean up.
          (if (save-excursion (goto-char (line-beginning-position))
                              (looking-at (rx bol (* space) eol)))
              (delete-region (match-beginning 0) (min (1+ (match-end 0)) (point-max)))
            (save-excursion
              (skip-chars-backward " \t")
              (when (looking-at (rx (+ space) eol))
                (delete-region (match-beginning 0) (match-end 0))))
            (when (and (looking-back (rx space) (1- (point)))
                       (looking-at (rx (+ space))))
              (delete-region (match-beginning 0) (match-end 0)))
            (indent-according-to-mode))
        (save-excursion
          ;; the whole enclosing sexp might need re-indenting
          (condition-case nil (up-list)  (scan-error))
          (indent-region opoint (1+ (point))))))))

(defun el-search--format-replacement (replacement original replace-expr-input splice)
  ;; Return a printed representation of REPLACEMENT.  Try to reuse the
  ;; layout of subexpressions shared with the original (replaced)
  ;; expression and the replace expression.
  (if (and splice (not (listp replacement)))
      (error "Expression to splice in is not a list: %S" replacement)
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
                                      this-sexp (el-search-read (current-buffer))
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
                                (progn (setq this-sexp (el-search-read (current-buffer)))
                                       t)
                              ((invalid-read-syntax end-of-buffer end-of-file) nil)))
                  (if (with-current-buffer orig-buffer
                        (condition-case nil
                            (if (not (equal this-sexp (el-search-read (current-buffer))))
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
              (if (condition-case nil
                      (equal replacement (el-search-read (if splice (format "(%s)" result) result)))
                    ((debug error) nil))
                  result
                (error "Apparent error in `el-search--format-replacement'
Can please make a bug report including a recipe of what exactly you did?
Thanks!"))))
        (kill-buffer orig-buffer)))))

(defun el-search--search-and-replace-pattern
    (pattern replacement &optional splice to-input-string use-current-search)
  (unless use-current-search
    (el-search-setup-search-1 pattern
                              (let ((current-buffer (current-buffer)))
                                (lambda () (stream (list current-buffer))))
                              t
                              (let ((here (copy-marker (point))))
                                (lambda (search)
                                  (setf (alist-get 'is-single-buffer
                                                   (el-search-object-properties search))
                                        t)
                                  (setf (alist-get 'description (el-search-object-properties search))
                                        "Search created by `el-search-query-replace'")
                                  (let ((inhibit-message t))
                                    (el-search--next-buffer search)
                                    (setf (el-search-head-position (el-search-object-head search))
                                          here))))))
  (catch 'done
    (let ((replace-all nil) (replace-all-and-following nil)
          nbr-replaced nbr-skipped (nbr-replaced-total 0) (nbr-changed-buffers 0)
          (el-search-keep-hl t) (opoint (point))
          (get-replacement (el-search-make-matcher pattern replacement))
          (skip-matches-in-replacement 'ask)
          (matcher (el-search-make-matcher pattern))
          (heuristic-matcher (el-search--current-heuristic-matcher))
          (save-all-answered nil)
          (should-quit nil))
      (let ((replace-in-current-buffer
             (lambda ()
               (setq nbr-replaced 0)
               (setq nbr-skipped  0)
               (condition-case err
                   (let ((start-point (point)))

                     (unless replace-all
                       (el-search-hl-other-matches matcher)
                       (add-hook 'window-scroll-functions #'el-search--after-scroll t t)
                       (when use-current-search
                         (let ((head (el-search-object-head el-search--current-search)))
                           (el-search--message-no-log "%s..."
                                                      (or (el-search-head-file head)
                                                          (el-search-head-buffer head)))
                           (sit-for 1.))))

                     (while (el-search--search-pattern-1 matcher t nil heuristic-matcher)
                       (setq opoint (point))
                       (setf (el-search-head-position
                              (el-search-object-head el-search--current-search))
                             (copy-marker (point)))
                       (setf (el-search-object-last-match el-search--current-search)
                             (copy-marker (point)))
                       (unless replace-all
                         (el-search-hl-sexp))
                       (let* ((region (list (point) (el-search--end-of-sexp)))
                              (original-text (apply #'buffer-substring-no-properties region))
                              (expr      (el-search-read original-text))
                              (replaced-this nil)
                              (new-expr  (funcall get-replacement expr))
                              (get-replacement-string
                               (lambda () (el-search--format-replacement
                                      new-expr original-text to-input-string splice)))
                              (to-insert (funcall get-replacement-string))
                              (replacement-contains-another-match-p
                               (lambda ()
                                 ;; This intentionally includes the replacement itself
                                 (with-temp-buffer
                                   (emacs-lisp-mode)
                                   (insert to-insert)
                                   (goto-char 1)
                                   (condition-case nil
                                       (el-search--search-pattern-1 matcher 'noerror)
                                     (end-of-buffer nil)))))
                              (replacement-contains-another-match
                               (funcall replacement-contains-another-match-p))
                              (void-replacement-p (lambda () (and splice (null new-expr))))
                              (do-replace
                               (lambda ()
                                 (save-excursion
                                   (save-restriction
                                     (widen)
                                     (el-search--replace-hunk
                                      (list (point) (el-search--end-of-sexp))
                                      to-insert)))
                                 (unless (funcall void-replacement-p)
                                   ;;skip potentially newly added whitespace
                                   (el-search--ensure-sexp-start))
                                 (cl-incf nbr-replaced)
                                 (cl-incf nbr-replaced-total)
                                 (setq replaced-this t)
                                 (when replace-all
                                   (let ((head (el-search-object-head el-search--current-search)))
                                     (el-search--message-no-log
                                      "%s (%d%%)"
                                      (or (el-search-head-file head)
                                          (el-search-head-buffer head))
                                      (/ (* 100 (- (point) start-point -1))
                                         (- (point-max) start-point -1)))))))
                              (query
                               (lambda ()
                                 (car
                                  (read-multiple-choice
                                   (let ((nbr-done  (+ nbr-replaced nbr-skipped))
                                         (nbr-to-do (el-search-count-matches pattern)))
                                     (format "[%d/%d]"
                                             (if replaced-this nbr-done (1+ nbr-done))
                                             (+ nbr-done nbr-to-do)))
                                   (delq nil
                                         (list
                                          (and (not replaced-this)
                                               '(?y "y" "Replace this match and move to the next"))
                                          (list ?n
                                                (if replaced-this "next" "n")
                                                "Go to the next match")
                                          (and (not replaced-this)
                                               '(?r "r" "Replace this match but don't move"))
                                          '(?! "all" "Replace all remaining matches in this buffer")
                                          '(?b "skip buf"
                                               "Skip this buffer and any remaining matches in it")
                                          (and buffer-file-name
                                               '(?d "skip dir"
                                                    "Skip a parent directory of current file"))
                                          (and (not replaced-this)
                                               (list ?s (concat (if splice "disable" "enable")
                                                                " splice")
                                                     (substitute-command-keys "\
Toggle splicing mode (\\[describe-function] el-search-query-replace for details).")))
                                          '(?o "show" "Show replacement in a buffer")
                                          '(?q "quit"))))))))
                         (if replace-all
                             (funcall do-replace)
                           (while (not (pcase (funcall query)
                                         (?r (funcall do-replace)
                                             nil)
                                         (?y (funcall do-replace)
                                             t)
                                         (?n
                                          (unless replaced-this (cl-incf nbr-skipped))
                                          t)
                                         (?!
                                          (when (and use-current-search
                                                     (not (alist-get 'is-single-buffer
                                                                     (el-search-object-properties
                                                                      el-search--current-search)))
                                                     (eq (car (read-multiple-choice
                                                               "Replace in all following buffers?"
                                                               '((?! "Only this"
                                                                     "\
Replace only remaining matches in this buffer")
                                                                 (?A "All buffers"
                                                                     "\
Replace all matches in all buffers"))))
                                                         ?A))
                                            (setq replace-all-and-following t))
                                          (setq replace-all t)
                                          (unless replaced-this (funcall do-replace))
                                          t)
                                         (?b (goto-char (point-max))
                                             (message "Skipping this buffer")
                                             (sit-for 1)
                                             ;; FIXME: add #skipped matches to nbr-skipped?
                                             t)
                                         (?d (call-interactively #'el-search-skip-directory)
                                             t)
                                         (?s
                                          (setq splice    (not splice)
                                                to-insert (funcall get-replacement-string)
                                                replacement-contains-another-match
                                                (funcall replacement-contains-another-match-p))
                                          nil)
                                         (?o
                                          ;; FIXME: Should we allow to edit the replacement?
                                          (let* ((buffer (get-buffer-create
                                                          (generate-new-buffer-name "*Replacement*")))
                                                 (window (display-buffer buffer)))
                                            (with-selected-window window
                                              (emacs-lisp-mode)
                                              (save-excursion
                                                (insert
                                                 "\
;; This buffer shows the replacement for the current match.
;; Please hit any key to proceed.\n\n"
                                                 (funcall get-replacement-string)))
                                              (read-char " "))
                                            (delete-window window)
                                            (kill-buffer buffer)
                                            (el-search--after-scroll (selected-window) (window-start))
                                            nil))
                                         ((or ?q ?\C-g) (signal 'quit t))))))
                         (when replacement-contains-another-match
                           (el-search-hl-other-matches matcher))
                         (unless (eobp)
                           (let ((skip-replacement
                                  (lambda () (forward-sexp (if splice (length replacement) 1)))))
                             (cond
                              ((not (and replaced-this
                                         replacement-contains-another-match
                                         skip-matches-in-replacement))
                               (unless (or replaced-this (eobp))
                                 (el-search--skip-expression nil t)))
                              ((eq skip-matches-in-replacement 'ask)
                               (pcase (car (read-multiple-choice
                                            (propertize
                                             "There are matches in this replacement - skip them? "
                                             'face 'el-search-highlight-in-prompt-face)
                                            '((?y "yes")
                                              (?n "no")
                                              (?Y "always Yes")
                                              (?N "always No")
                                              (?q "quit"))))
                                 ((and (or ?y ?Y) answer)
                                  (when (= answer ?Y) (setq skip-matches-in-replacement t))
                                  (funcall skip-replacement))
                                 (?q (signal 'quit t))
                                 (answer
                                  (when (= answer ?N) (setq skip-matches-in-replacement nil))
                                  (when replace-all
                                    (setq replace-all nil) ;FIXME: can this be annoying?  Problem:
                                                           ;we need to catch possibly infinite loops
                                    (message "Falling back to interactive mode")
                                    (sit-for 2.)))))
                              (t (funcall skip-replacement))))))))
                 (quit  (setq should-quit t))
                 ((error debug) (setq should-quit (lambda () (error "%s" (error-message-string err))))))
               (el-search-hl-remove)
               (when should-quit
                 (remove-hook 'post-command-hook 'el-search-hl-post-command-fun t)
                 (if (functionp should-quit) (funcall should-quit) (throw 'done t)))
               (setf (el-search-head-position (el-search-object-head el-search--current-search))
                     (point-max))
               (goto-char opoint)
               (if (> nbr-replaced 0)
                   (progn
                     (cl-incf nbr-changed-buffers)
                     (when (pcase el-search-auto-save-buffers
                             ((or 'nil
                                  (guard (not buffer-file-name)))
                              nil)
                             ((and 'ask-multi
                                   (guard (alist-get 'is-single-buffer
                                                     (el-search-object-properties
                                                      el-search--current-search))))
                              nil)
                             ((or 'ask 'ask-multi)
                              (if save-all-answered
                                  (cdr save-all-answered)
                                (pcase (car (read-multiple-choice
                                             (format
                                              "Replaced %d matches%s - save this buffer? "
                                              nbr-replaced
                                              (if (zerop nbr-skipped)  ""
                                                (format "   (%d skipped)" nbr-skipped)))
                                             '((?y "yes")
                                               (?n "no")
                                               (?Y "Yes to all"
                                                   "\
Save this buffer and all following buffers without asking again")
                                               (?N "No to all"
                                                   "\
Don't save this buffer and all following buffers; don't ask again"))))
                                  (?y t)
                                  (?n nil)
                                  (?Y (cdr (setq save-all-answered (cons t t))))
                                  (?N (cdr (setq save-all-answered (cons t nil)))))))
                             (_ t))
                       (save-buffer)))
                 (unless use-current-search
                   (message "Replaced %d matches%s"
                            nbr-replaced
                            (if (zerop nbr-skipped)  ""
                              (format "   (%d skipped)" nbr-skipped))))))))
        (while (progn (el-search-continue-search)
                      (and el-search--success (not el-search--wrap-flag)))
          (funcall replace-in-current-buffer)
          (unless replace-all-and-following (setq replace-all nil)))
        (message "Replaced %d matches in %d buffers" nbr-replaced-total nbr-changed-buffers)))))

(defun el-search-query-replace--read-args ()
  (barf-if-buffer-read-only)
  (let ((from-input (let ((el-search--initial-mb-contents
                           (or el-search--initial-mb-contents
                               (and (or (eq last-command 'el-search-pattern)
                                        (el-search--pending-search-p))
                                    (if (equal (el-search-read (car el-search-pattern-history))
                                               (el-search-read (car el-search-query-replace-history)))
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
    (setq read-from (el-search-read from))
    (setq read-to   (el-search-read to))
    (el-search--maybe-warn-about-unquoted-symbol read-from)
    (when (and (symbolp read-to)
               (not (el-search--contains-p (el-search-make-matcher `',read-to) read-from))
               (not (eq read-to t))
               (not (eq read-to nil)))
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
used for history entries.

When called directly after a search command, use the current
search to drive query-replace (like in isearch).  You get a
multi-buffer query-replace this way when the current search is
multi-buffer.  When not called after a search command,
query-replace all matches following point in the current buffer.

It is also possible to replace matches with an arbitrary number
of expressions (even with zero expressions, effectively deleting
matches) by using the \"splicing\" submode that can be toggled
from the prompt with \"s\".  When splicing mode is on (default
off), the replacement expression must evaluate to a list, and all
of the list's elements are inserted in order.

The optional argument TEXTUAL-TO is bound by the interactive form
to the text form of the replacement expression specified.  It is
consulted to construct the text form of each replacement."
  (interactive (el-search-query-replace--read-args)) ;this binds the optional argument
  (setq this-command 'el-search-query-replace) ;in case we come from isearch
  (barf-if-buffer-read-only)
  (el-search--search-and-replace-pattern
   from-pattern to-expr nil textual-to
   (let ((search-head (and el-search--current-search
                           (el-search-object-head el-search--current-search))))
     (and
      search-head
      (eq (el-search-head-buffer search-head) (current-buffer))
      (equal from-pattern (el-search-object-pattern el-search--current-search))
      (or (eq last-command 'el-search-pattern)
          (el-search--pending-search-p))
      (prog1 t
        (el-search--message-no-log "Using the current search to drive query-replace...")
        (sit-for 1.))))))

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
