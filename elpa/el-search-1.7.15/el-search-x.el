;;; el-search-x.el --- Additional pattern definitions for el-search    -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 2016_08_03
;; Keywords: lisp


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

;; This file contains additional definitions of el-search patterns.
;; You can just `require' this file, but doing so is not mandatory for
;; using el-search.



;;; Code:

(eval-when-compile
  (require 'subr-x))
(require 'thunk)
(require 'el-search)


(el-search-defpattern string-lines (pattern)
  "Matches any string whose line count is matched by PATTERN.

Examples: (string-lines 1) matches one-line strings.
\(string-lines (pred (>= 5))\) matches strings consisting of not
more than 5 lines."
  (let ((string (make-symbol "string")))
    `(and (string)
          ,string
          (let ,pattern
            (with-temp-buffer
              (insert ,string)
              (count-lines (point-min) (point-max)))))))

;;;; `append and `l'

(defun el-search--split (matcher1 matcher2 list)
  "Helper for the \"append\" pattern type.

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

matches the list (1 2 3 4 5 6 7 8 9), binding `x' to (4 5 6)."
  (if (null patterns)
      '(pred null)
    (pcase-let ((`(,pattern . ,more-patterns) patterns))
      (cond
       ((null more-patterns) pattern)
       ((null (cdr more-patterns))
        `(and (pred listp)
              (app ,(apply-partially #'el-search--split
                                     (el-search-make-matcher pattern)
                                     (el-search-make-matcher (car more-patterns)))
                   `(,,pattern ,,(car more-patterns)))))
       (t `(append ,pattern (append ,@more-patterns)))))))

(defcustom el-search-lazy-l t
  "Whether to interpret symbols and strings specially in `l'.

When non-nil, the default, `l' based pattern types interpret
symbols and strings as special LPATS: a SYMBOL matches any symbol
S matched by SYMBOL's name interpreted as a regexp, and a STRING
matches any string matched by the STRING interpreted as a regexp.

When nil, symbols and strings act as standard `pcase' patterns."
  :group 'el-search :type 'boolean)

(defun el-search--transform-nontrivial-lpat (expr)
  (if el-search-lazy-l
      (pcase expr
        ((and (pred symbolp) (let symbol-name (symbol-name expr)))
         `(symbol ,symbol-name))
        ((pred stringp) `(string ,expr))
        (_ expr))
    expr))

(el-search-defpattern l (&rest lpats)
  "Alternative pattern type for matching lists.
Match any list with subsequent elements matched by all LPATS in
order.

The idea is to be able to search for pieces of code (i.e. lists)
with very brief input by using a specialized syntax.

An LPAT can take the following forms (the special interpretation
of symbols and strings can be turned off by binding or
customizing `el-search-lazy-l' to nil):

SYMBOL  Matches any symbol S matched by SYMBOL's name interpreted
        as a regexp.
'SYMBOL Matches the SYMBOL.
STRING  Matches any string matched by STRING interpreted as a
        regexp.
_       Matches any list element.
__      Matches any number (including zero) of list elements.
^       Matches zero elements, but only at the beginning of a list.
        Only allowed as the first of the LPATS.
$       Matches zero elements, but only at the end of a list.
        Only allowed as the last of the LPATS.
PAT     Anything else is interpreted as a standard pattern and
        matches one list element matched by it.  Note: If
        matching PAT binds any symbols, occurrences in any
        following patterns are not turned into equivalence tests;
        the scope of symbol bindings is limited to the PAT
        itself.

Example: To match defuns that contain \"hl\" in the defined name
and have at least one mandatory, but also optional arguments, you
could use this pattern:

    (l ^ 'defun hl (l _ &optional))"
  ;; We don't allow PATs in `l' to create bindings because to make this
  ;; work as expected we would need some kind of backtracking
  (declare
   (heuristic-matcher
    (lambda (&rest lpats)
      (lambda (file-name-or-buffer atoms-thunk)
        (cl-every
         (lambda (lpat)
           (pcase lpat
             ((or '__ '_ '_? '^ '$) t)
             (_ (funcall (el-search-heuristic-matcher (el-search--transform-nontrivial-lpat lpat))
                         file-name-or-buffer atoms-thunk))))
         lpats)))))
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


;;;; `change', `changed'

(defvar diff-hl-reference-revision)
(declare-function diff-hl-changes "diff-hl")
(defvar-local el-search--cached-changes nil)


(defcustom el-search-change-revision-transformer-function nil
  "Transformer function for the REVISION argument of `change' and `changed'.

When specified, this function is called with two arguments - the
REVISION argument passed to `change' or `changed' and the current
file name - and the return value is used as REVISION argument for
these patterns.

The default value is nil."
  :group 'el-search
  :type '(choice (const :tag "No transformer" nil)
                 (function :tag "User specified function")))

(defalias 'el-search--file-truename-wstm
  ;; We call `file-truename' very often and it's quite slow
  (el-search-with-short-term-memory #'file-truename))

(defun el-search--changed-files-in-repo (repo-root-dir &optional commit)
  "Return a list of files that changed relative to COMMIT.
COMMIT defaults to HEAD."
  (cl-callf or commit "HEAD")
  (let ((default-directory repo-root-dir))
    (mapcar #'expand-file-name
            (split-string
             (shell-command-to-string
              (format "git diff -z --name-only %s --" (shell-quote-argument commit)))
             "\0" t))))

(defvar vc-git-diff-switches)
(defun el-search--file-changed-p (file revision)
  "Return non-nil when FILE has changed relative to REVISION."
  (cl-callf el-search--file-truename-wstm file)
  (when-let ((backend (vc-backend file)))
    (let ((default-directory (file-name-directory file)))
      (and
       (with-temp-buffer
         (= 1 (vc-call-backend backend 'diff (list file) nil revision (current-buffer))))
       (with-temp-buffer
         (= 1 (vc-call-backend backend 'diff (list file) revision nil (current-buffer))))))))

(defun el-search--changes-from-diff-hl (revision)
  "Return the changed regions in the current buffer's file.
The return value is a list of conses (START . END) of all changes
relative to REVISION.

Uses variable `el-search--cached-changes' for caching."
  (let ((buffer-file-name (el-search--file-truename-wstm buffer-file-name))) ;shouldn't be necessary, but it is...
    (if (and (consp el-search--cached-changes)
             (equal (car el-search--cached-changes)
                    (list revision (visited-file-modtime))))
        (cdr el-search--cached-changes)
      (when (buffer-modified-p)
        (user-error "Buffer is modified - please save"))
      (require 'vc)
      (require 'diff-hl)
      ;; `diff-hl-changes' returns line numbers.  We must convert them into positions.
      (save-restriction
        (widen)
        (save-excursion
          (let ((diff-hl-reference-revision
                 (if el-search-change-revision-transformer-function
                     (funcall el-search-change-revision-transformer-function
                              revision
                              buffer-file-name)
                   revision))
                (current-line-nbr 1) change-beg)
            (goto-char 1)
            (cdr (setq el-search--cached-changes
                       (cons (list revision (visited-file-modtime))
                             (and (el-search--file-changed-p buffer-file-name diff-hl-reference-revision)
                                  (delq nil (mapcar (pcase-lambda (`(,start-line ,nbr-lines ,kind))
                                                      (if (eq kind 'delete) nil
                                                        (forward-line (- start-line current-line-nbr))
                                                        (setq change-beg (point))
                                                        (forward-line (1- nbr-lines))
                                                        (setq current-line-nbr (+ start-line nbr-lines -1))
                                                        (cons (copy-marker change-beg)
                                                              (copy-marker (line-end-position)))))
                                                    (ignore-errors
                                                      (let ((default-directory (file-name-directory buffer-file-name)))
                                                        (diff-hl-changes)))))))))))))))

(defun el-search--change-p (posn revision)
  ;; Non-nil when sexp after POSN is part of a change
  (when (buffer-modified-p)
    (user-error "Buffer is modified - please save"))
  (save-restriction
    (widen)
    (let ((changes (el-search--changes-from-diff-hl revision))
          (sexp-end (el-search--end-of-sexp posn))
          (atomic? (thunk-delay (el-search--atomic-p
                                 (save-excursion (goto-char posn)
                                                 (el-search-read (current-buffer)))))))
      (while (and changes (or (< (cdar changes) posn)
                              (and
                               ;; a string spanning multiple lines is a change even when not all
                               ;; lines are changed
                               (< (cdar changes) sexp-end)
                               (not (thunk-force atomic?)))))
        (pop changes))
      (and changes (or (<= (caar changes) posn)
                       (and (thunk-force atomic?)
                            (<= (caar changes) sexp-end)))))))

(defun el-search--changed-p (posn revision)
  ;; Non-nil when sexp after POSN contains a change
  (when (buffer-modified-p)
    (user-error "Buffer is modified - please save"))
  (save-restriction
    (widen)
    (let ((changes (el-search--changes-from-diff-hl revision)))
      (while (and changes (<= (cdar changes) posn))
        (pop changes))
      (and changes
           (< (caar changes) (el-search--end-of-sexp posn))))))

(defun el-search-change--heuristic-matcher (&optional revision)
  (let* ((revision (or revision "HEAD"))
         (get-changed-files-in-repo
          (el-search-with-short-term-memory #'el-search--changed-files-in-repo))
         (file-changed-p (el-search-with-short-term-memory
                          (lambda (file-name-or-buffer)
                            (require 'vc)
                            (when-let ((file (if (stringp file-name-or-buffer)
                                                 file-name-or-buffer
                                               (buffer-file-name file-name-or-buffer))))
                              (cl-callf el-search--file-truename-wstm file)
                              (let ((default-directory (file-name-directory file)))
                                (when-let ((backend (vc-backend file))
                                           (root-dir
                                            (condition-case err
                                                (vc-call-backend backend 'root default-directory)
                                              ;; Same handler as in `vc-root-dir'
                                              (vc-not-supported
                                               (unless (eq (cadr err) 'root)
                                                 (signal (car err) (cdr err)))
                                               nil))))
                                  (cl-some
                                   (apply-partially #'file-equal-p file)
                                   (funcall get-changed-files-in-repo
                                            root-dir
                                            (funcall (or el-search-change-revision-transformer-function
                                                         (lambda (rev _) rev))
                                                     revision file))))))))))
    (lambda (file-name-or-buffer _) (funcall file-changed-p file-name-or-buffer))))

(el-search-defpattern change (&optional revision)
  "Matches the object if its text is part of a file change.

Requires library \"diff-hl\".  REVISION defaults to the file's
repository's HEAD commit and is a revision string.  Customize
`el-search-change-revision-transformer-function' to control how
REVISION is interpreted.

This pattern-type does currently only work for git versioned
files."
  (declare (heuristic-matcher #'el-search-change--heuristic-matcher))
  `(guard (el-search--change-p (point) ,(or revision "HEAD"))))

(el-search-defpattern changed (&optional revision)
  "Matches the object if its text contains a file change.

Requires library \"diff-hl\".  REVISION defaults to the file's
repository's HEAD commit and is a revision string.  Customize
`el-search-change-revision-transformer-function' to control how
REVISION is interpreted.

This pattern-type does currently only work for git versioned
files."
  (declare (heuristic-matcher #'el-search-change--heuristic-matcher))
  `(guard (el-search--changed-p (point) ,(or revision "HEAD"))))


;;;; `outermost' and `top-level'

(el-search-defpattern outermost (pattern &optional not-pattern)
    "Matches when PATTERN matches but the parent sexp does not.
For toplevel expressions, this is equivalent to PATTERN.

Optional NOT-PATTERN defaults to PATTERN; when given, match when
PATTERN matches but the parent sexp is not matched by
NOT-PATTERN.


This pattern is useful to match only the outermost expression
when subexpressions would match recursively.  For
example, (outermost _) matches only top-level expressions.
Another example: For the `change' pattern, any subexpression of a
match is typically also an according change.  Wrapping the
`change' pattern into `outermost' prevents el-search from
descending into any found expression - only the outermost
expression matching the `change' pattern will be matched."
    `(and ,pattern
          (not (guard (save-excursion
                        (condition-case nil
                            (progn
                              (backward-up-list)
                              (el-search--match-p
                               ',(el-search-make-matcher (or not-pattern pattern))
                               (save-excursion (el-search-read (current-buffer)))))
                          (scan-error)))))))

(el-search-defpattern top-level ()
  "Matches any toplevel expression."
  '(outermost _))


;;; Sloppy pattern types for quick navigation

;;;; `keys'

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
  (el-search-defpattern--check-args
   "keys" (list key-sequence) (lambda (x) (or (stringp x) (vectorp x))) "argument not a string or vector")
  `(pred (el-search--match-key-sequence ,key-sequence)))



;;; Patterns for stylistic rewriting and syntactical simplification

;;; de Morgan

(el-search-defpattern de-morgan (&optional replacement)
  "Matches forms that can be simplified by applying de Morgan.
Matched are all expressions of the form

  (or (not A1) (not A2) ...)

and

  (and (not B1) (not B2) ...)

where at least two `not' expressions are present.

REPLACEMENT, when specified, should be a variable, and will be
bound to a semantically equivalent expression with de Morgan's
law been applied, namely

  (not (and A1 A2 ...))

or

  (not (or B1 B2 ...))

respectively.

Note that when `el-search-query-replace'ing with this pattern
type, it's possible that de Morgan can be applied again, so you
may want to check that."
  (let ((functor (make-symbol "functor"))
        (nots    (make-symbol "nots"))
        (arg     (make-symbol "arg")))
    `(and `(,(and (or 'or 'and) ,functor) . ,,nots)
          (guard (and (consp ,nots) (not (cdr (last ,nots))))) ;check for a proper non-empty list
          (guard (cl-every (lambda (,arg) (pcase ,arg (`(not ,_) t))) ,nots))
          (let (pred identity) (cdr ,nots))
          ,@(and replacement
                 (not (eq '_ replacement))
                 `((let ,replacement `(not (,(if (eq ,functor 'or) 'and 'or)
                                            ,@(mapcar #'cadr ,nots)))))))))

;;;; Iffy `if's

(defun el-search--nested-if-1 (expr)
  ;; EXPR is a (potentially nested) `if' expression.  Return a list L so
  ;; that (cond . L) is semantically equivalent to EXPR.  For example,
  ;; when EXPR == (if x 1 (if y 2 3)), return ((x 1) (y 2) (t 3))
  (pcase-exhaustive expr
    (`(if ,condition ,then ,(and `(if . ,_) inner-if))
     `((,condition ,then)  ,@(el-search--nested-if-1 inner-if)))
    (`(if ,condition ,then)
     `((,condition ,then)))
    (`(if ,condition ,then . ,else)
     `((,condition ,then)
       (t . ,else)))))

(el-search-defpattern -nested-if (&optional var)
  (let ((test-pattern '`(if ,_ ,_ (if ,_ ,_ ,_ . ,_))))
    (if (not var)  test-pattern
      (let ((cases (make-symbol "cases")))
        `(and ,test-pattern
              (app el-search--nested-if-1 ,cases)
              (let ,var `(cond . ,,cases)))))))

(el-search-defpattern iffy-if (&optional var)
  "Matches `if'-clauses that could be replaced with a more suitable form.

Match `if' clauses that would fit better into either `cond',
`when' or `unless'.  With symbol VAR given, bind that to such a
semantically equivalent expression suitable to replace the
current match."
  (cl-callf or var '_)
  (let ((condition (make-symbol "condition"))
        (then      (make-symbol "then"))
        (clauses   (make-symbol "clauses")))
    `(or (-nested-if ,var)
         (and `(if (not ,,condition) ,,then)
              (let ,var `(unless ,,condition ,,then)))
         (and `(if ,,condition ,,then)
              (let ,var `(when   ,,condition ,,then)))
         (and `(if ,,condition ,,then (cond . ,,clauses))
              (let ,var `(cond (,,condition ,,then) . ,,clauses))))))


(provide 'el-search-x)

;;; el-search-x.el ends here
