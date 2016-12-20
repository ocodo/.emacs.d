;;; el-search-x.el --- Additional pattern definitions for el-search    -*- lexical-binding: t -*-

;; Copyright (C) 2016 Free Software Foundation, Inc

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
  (require 'subr-x)
  (require 'thunk))
(require 'el-search)


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

(defun el-search--transform-nontrivial-lpat (expr)
  (pcase expr
    ((and (pred symbolp) (let symbol-name (symbol-name expr)))
     `(or (symbol ,symbol-name)
          `',(symbol  ,symbol-name)
          `#',(symbol ,symbol-name)))
    (`',(and (pred symbolp) symbol)
     `(or ',symbol '',symbol '#',symbol))
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
'SYMBOL Matches SYMBOL, 'SYMBOL and #'SYMBOL (so it's like the above
        without regexp matching).
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
  (declare
   (heuristic-matcher
    (lambda (&rest lpats)
      (lambda (file-name-or-buffer atom-thunk)
        (cl-every
         (lambda (lpat)
           (pcase lpat
             ((or '__ '_ '_? '^ '$) t)
             ((pred symbolp)
              (funcall (el-search-heuristic-matcher `(symbol ,(symbol-name lpat)))
                       file-name-or-buffer atom-thunk))
             (_ (funcall (el-search-heuristic-matcher (el-search--transform-nontrivial-lpat lpat))
                         file-name-or-buffer atom-thunk))))
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

When specified, this function is called with two arguments: the
REVISION argument passed to `change' or `changed', and the
current file name, and the returned value is used instead of
REVISION.

The default value is nil."
  :group 'el-search
  :type '(choice (const :tag "No transformer" nil)
                 (function :tag "User specified function")))

(defun el-search--changes-from-diff-hl (revision)
  "Return a list of changed regions (as conses of positions) since REVISION.
Use variable `el-search--cached-changes' for caching."
  (if (and (consp el-search--cached-changes)
           (equal (car el-search--cached-changes)
                  (list revision (visited-file-modtime))))
      (cdr el-search--cached-changes)
    (when (buffer-modified-p)
      (error "Buffer is modified - please save"))
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
                                                  (ignore-errors (diff-hl-changes)))))))))))))

(defun el-search--change-p (posn &optional revision)
  ;; Non-nil when sexp after POSN is part of a change
  (save-restriction
    (widen)
    (let ((changes (el-search--changes-from-diff-hl revision))
          (sexp-end (scan-sexps posn 1)))
      (while (and changes (< (cdar changes) sexp-end))
        (pop changes))
      (and changes
           (<= (caar changes) posn)))))

(defun el-search--changed-p (posn &optional revision)
  ;; Non-nil when sexp after POSN contains a change
  (when (buffer-modified-p)
    (error "Buffer is modified - please save"))
  (save-restriction
    (widen)
    (let ((changes (el-search--changes-from-diff-hl revision)))
      (while (and changes (<= (cdar changes) posn))
        (pop changes))
      (and changes
           (< (caar changes) (scan-sexps posn 1))))))

(defun el-search--file-changed-p (file rev)
  (cl-callf file-truename file)
  (when-let ((backend (vc-backend file)))
    (ignore-errors
      (let ((default-directory (file-name-directory file)))
        (and
         (with-temp-buffer
           (= 1 (vc-call-backend backend 'diff (list file) nil rev (current-buffer))))
         (with-temp-buffer
           (= 1 (vc-call-backend backend 'diff (list file) rev nil (current-buffer)))))))))

(defun el-search-change--heuristic-matcher (&optional revision)
  (lambda (file-name-or-buffer _)
    (require 'vc)
    (when-let ((file (if (stringp file-name-or-buffer)
                         file-name-or-buffer
                       (buffer-file-name file-name-or-buffer))))
      (let ((default-directory (file-name-directory file)))
        (el-search--file-changed-p
         file
         (funcall el-search-change-revision-transformer-function
                  (or revision "HEAD") file))))))

(el-search-defpattern change (&optional revision)
  "Matches the object if its text is part of a file change.

Requires library \"diff-hl\".  REVISION defaults to the file's
repository's HEAD commit and is a revision string.  Customize
`el-search-change-revision-transformer-function' to control how
REVISION is interpreted."
  (declare (heuristic-matcher #'el-search-change--heuristic-matcher))
  `(guard (el-search--change-p (point) ,(or revision "HEAD"))))

(el-search-defpattern changed (&optional revision)
  "Matches the object if its text contains a file change.

Requires library \"diff-hl\".  REVISION defaults to the file's
repository's HEAD commit and is a revision string.  Customize
`el-search-change-revision-transformer-function' to control how
REVISION is interpreted."
  (declare (heuristic-matcher #'el-search-change--heuristic-matcher))
  `(guard (el-search--changed-p (point) ,(or revision "HEAD"))))


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
                               ',(el-search--matcher (or not-pattern pattern))
                               (save-excursion (read (current-buffer)))))
                          (scan-error)))))))

(el-search-defpattern top-level ()
  "Matches any toplevel expression."
  '(outermost _))


;;; Patterns for stylistic rewriting

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
