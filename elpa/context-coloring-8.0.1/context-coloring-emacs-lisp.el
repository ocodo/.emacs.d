;;; context-coloring-emacs-lisp.el --- Emacs Lisp support  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;; Add Emacs Lisp context coloring support.

;;; Code:

(require 'context-coloring)


;;; Emacs Lisp colorization

(defconst context-coloring-WORD-CODE 2)
(defconst context-coloring-SYMBOL-CODE 3)
(defconst context-coloring-OPEN-PARENTHESIS-CODE 4)
(defconst context-coloring-CLOSE-PARENTHESIS-CODE 5)
(defconst context-coloring-EXPRESSION-PREFIX-CODE 6)
(defconst context-coloring-STRING-QUOTE-CODE 7)
(defconst context-coloring-ESCAPE-CODE 9)
(defconst context-coloring-COMMENT-START-CODE 11)
(defconst context-coloring-COMMENT-END-CODE 12)

(defconst context-coloring-OCTOTHORPE-CHAR (string-to-char "#"))
(defconst context-coloring-APOSTROPHE-CHAR (string-to-char "'"))
(defconst context-coloring-OPEN-PARENTHESIS-CHAR (string-to-char "("))
(defconst context-coloring-COMMA-CHAR (string-to-char ","))
(defconst context-coloring-AT-CHAR (string-to-char "@"))
(defconst context-coloring-BACKTICK-CHAR (string-to-char "`"))

(defsubst context-coloring-get-syntax-code ()
  "Get the syntax code at point."
  (syntax-class
   ;; Faster version of `syntax-after':
   (aref (syntax-table) (char-after (point)))))

(defsubst context-coloring-forward-sws ()
  "Move forward through whitespace and comments."
  (while (forward-comment 1)))

(defsubst context-coloring-elisp-colorize-comments-and-strings
  (&optional min max)
  "Color comments and strings from MIN to MAX."
  (context-coloring-colorize-comments-and-strings min max t))

(defsubst context-coloring-elisp-forward-sws ()
  "Move through whitespace and comments, coloring comments."
  (let ((start (point)))
    (context-coloring-forward-sws)
    (context-coloring-elisp-colorize-comments-and-strings start (point))))

(defsubst context-coloring-elisp-forward-sexp ()
  "Skip/ignore missing sexps, coloring comments and strings."
  (let ((start (point)))
    (when (= (context-coloring-get-syntax-code)
             context-coloring-EXPRESSION-PREFIX-CODE)
      ;; `forward-sexp' does not skip an unfinished expression (e.g. when the
      ;; name of a symbol or the parentheses of a list do not follow a single
      ;; quote).
      (forward-char))
    (condition-case nil
        (forward-sexp)
      (scan-error (context-coloring-forward-sws)))
    (context-coloring-elisp-colorize-comments-and-strings-in-region
     start (point))))

(defsubst context-coloring-exact-regexp (word)
  "Create a regexp matching exactly WORD."
  (concat "\\`" (regexp-quote word) "\\'"))

(defsubst context-coloring-exact-or-regexp (words)
  "Create a regexp matching any exact word in WORDS."
  (context-coloring-join
   (mapcar #'context-coloring-exact-regexp words) "\\|"))

(defconst context-coloring-elisp-ignored-word-regexp
  (context-coloring-join (list "\\`[-+]?[0-9]"
                               "\\`[&:].+"
                               (context-coloring-exact-or-regexp
                                '("t" "nil" "." "?")))
                         "\\|")
  "Match symbols that can't be bound as variables.")

(defsubst context-coloring-elisp-identifier-p (syntax-code)
  "Check if SYNTAX-CODE is an elisp identifier constituent."
  (or (= syntax-code context-coloring-WORD-CODE)
      (= syntax-code context-coloring-SYMBOL-CODE)))

(defconst context-coloring-elisp-sexps-per-pause 350
  "Pause after this many iterations to check for user input.
If user input is pending, stop the parse.  This makes for a
smoother user experience for large files.

This number should trigger pausing at about 60 frames per
second.")

(defvar context-coloring-elisp-sexp-count 0
  "Current number of sexps leading up to the next pause.")

(defsubst context-coloring-elisp-increment-sexp-count ()
  "Maybe check if the user interrupted the current parse."
  (setq context-coloring-elisp-sexp-count
        (1+ context-coloring-elisp-sexp-count))
  (when (and (zerop (% context-coloring-elisp-sexp-count
                       context-coloring-elisp-sexps-per-pause))
             context-coloring-interruptable-p
             (input-pending-p))
    (throw 'interrupted t)))

(defvar context-coloring-elisp-scope-stack '()
  "List of scopes in the current parse.")

(defsubst context-coloring-elisp-make-scope (level)
  "Make a scope object for LEVEL."
  (list
   :level level
   :variables '()))

(defsubst context-coloring-elisp-scope-get-level (scope)
  "Get the level of SCOPE object."
  (plist-get scope :level))

(defsubst context-coloring-elisp-scope-add-variable (scope variable)
  "Add to SCOPE a VARIABLE."
  (plist-put scope :variables (cons variable (plist-get scope :variables))))

(defsubst context-coloring-elisp-scope-has-variable (scope variable)
  "Check if SCOPE has VARIABLE."
  (member variable (plist-get scope :variables)))

(defsubst context-coloring-elisp-get-variable-level (variable)
  "Return the level of VARIABLE, or 0 if it isn't found."
  (let* ((scope-stack context-coloring-elisp-scope-stack)
         scope
         level)
    (while (and scope-stack (not level))
      (setq scope (car scope-stack))
      (cond
       ((context-coloring-elisp-scope-has-variable scope variable)
        (setq level (context-coloring-elisp-scope-get-level scope)))
       (t
        (setq scope-stack (cdr scope-stack)))))
    ;; Assume a global variable.
    (or level 0)))

(defsubst context-coloring-elisp-get-current-scope-level ()
  "Get the nesting level of the current scope."
  (cond
   ((car context-coloring-elisp-scope-stack)
    (context-coloring-elisp-scope-get-level (car context-coloring-elisp-scope-stack)))
   (t
    0)))

(defsubst context-coloring-elisp-push-scope ()
  "Add a new scope to the bottom of the scope chain."
  (push (context-coloring-elisp-make-scope
         (1+ (context-coloring-elisp-get-current-scope-level)))
        context-coloring-elisp-scope-stack))

(defsubst context-coloring-elisp-pop-scope ()
  "Remove the scope on the bottom of the scope chain."
  (pop context-coloring-elisp-scope-stack))

(defsubst context-coloring-elisp-add-variable (variable)
  "Add VARIABLE to the current scope."
  (context-coloring-elisp-scope-add-variable
   (car context-coloring-elisp-scope-stack)
   variable))

(defsubst context-coloring-elisp-parse-bindable (callback)
  "Parse the symbol at point.
If the symbol can be bound, invoke CALLBACK with it."
  (let* ((arg-string (buffer-substring-no-properties
                      (point)
                      (progn (context-coloring-elisp-forward-sexp)
                             (point)))))
    (when (not (string-match-p
                context-coloring-elisp-ignored-word-regexp
                arg-string))
      (funcall callback arg-string))))

(defun context-coloring-elisp-parse-let-varlist (type)
  "Parse the list of variable initializers at point.
If TYPE is `let', all the variables are bound after all their
initializers are parsed; if TYPE is `let*', each variable is
bound immediately after its own initializer is parsed."
  (let ((varlist '())
        syntax-code)
    ;; Enter.
    (forward-char)
    (context-coloring-elisp-forward-sws)
    (while (/= (setq syntax-code (context-coloring-get-syntax-code))
               context-coloring-CLOSE-PARENTHESIS-CODE)
      (cond
       ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
        (forward-char)
        (context-coloring-elisp-forward-sws)
        (setq syntax-code (context-coloring-get-syntax-code))
        (when (context-coloring-elisp-identifier-p syntax-code)
          (context-coloring-elisp-parse-bindable
           (lambda (var)
             (push var varlist)))
          (context-coloring-elisp-forward-sws)
          (setq syntax-code (context-coloring-get-syntax-code))
          (when (/= syntax-code context-coloring-CLOSE-PARENTHESIS-CODE)
            (context-coloring-elisp-colorize-sexp)))
        (context-coloring-elisp-forward-sws)
        ;; Skip past the closing parenthesis.
        (forward-char))
       ((context-coloring-elisp-identifier-p syntax-code)
        (context-coloring-elisp-parse-bindable
         (lambda (var)
           (push var varlist))))
       (t
        ;; Ignore artifacts.
        (context-coloring-elisp-forward-sexp)))
      (when (eq type 'let*)
        (context-coloring-elisp-add-variable (pop varlist)))
      (context-coloring-elisp-forward-sws))
    (when (eq type 'let)
      (while varlist
        (context-coloring-elisp-add-variable (pop varlist))))
    ;; Exit.
    (forward-char)))

(defun context-coloring-elisp-parse-arglist ()
  "Parse the list of function arguments at point."
  (let (syntax-code)
    ;; Enter.
    (forward-char)
    (context-coloring-elisp-forward-sws)
    (while (/= (setq syntax-code (context-coloring-get-syntax-code))
               context-coloring-CLOSE-PARENTHESIS-CODE)
      (cond
       ((context-coloring-elisp-identifier-p syntax-code)
        (context-coloring-elisp-parse-bindable
         (lambda (arg)
           (context-coloring-elisp-add-variable arg))))
       (t
        ;; Ignore artifacts.
        (context-coloring-elisp-forward-sexp)))
      (context-coloring-elisp-forward-sws))
    ;; Exit.
    (forward-char)))

(defun context-coloring-elisp-skip-callee-name ()
  "Skip past the opening parenthesis and name of a function."
  ;; Enter.
  (forward-char)
  (context-coloring-elisp-forward-sws)
  ;; Skip past the function name.
  (forward-sexp)
  (context-coloring-elisp-forward-sws))

(defun context-coloring-elisp-colorize-scope (callback)
  "Color the whole scope at point with its one color.
Handle a header in CALLBACK."
  (let ((start (point))
        (end (progn (forward-sexp)
                    (point))))
    (context-coloring-elisp-push-scope)
    ;; Splash the whole thing in one color.
    (context-coloring-colorize-region
     start
     end
     (context-coloring-elisp-get-current-scope-level))
    ;; Even if the parse is interrupted, this region should still be colored
    ;; syntactically.
    (context-coloring-elisp-colorize-comments-and-strings-in-region
     start
     end)
    (goto-char start)
    (context-coloring-elisp-skip-callee-name)
    (funcall callback)
    (context-coloring-elisp-colorize-region (point) (1- end))
    ;; Exit.
    (forward-char)
    (context-coloring-elisp-pop-scope)))

(defun context-coloring-elisp-parse-header (callback)
  "Parse a function header at point with CALLBACK."
  (when (= (context-coloring-get-syntax-code) context-coloring-OPEN-PARENTHESIS-CODE)
    (funcall callback)))

(defun context-coloring-elisp-colorize-defun-like (callback)
  "Color the defun-like function at point.
Parse the header with CALLBACK."
  (context-coloring-elisp-colorize-scope
   (lambda ()
     (when (context-coloring-elisp-identifier-p (context-coloring-get-syntax-code))
       ;; Color the defun's name with the top-level color.
       (context-coloring-colorize-region
        (point)
        (progn (forward-sexp)
               (point))
        0)
       (context-coloring-elisp-forward-sws)
       (context-coloring-elisp-parse-header callback)))))

(defun context-coloring-elisp-colorize-defun ()
  "Color the `defun' at point."
  (context-coloring-elisp-colorize-defun-like
   'context-coloring-elisp-parse-arglist))

(defun context-coloring-elisp-colorize-defadvice ()
  "Color the `defadvice' at point."
  (context-coloring-elisp-colorize-defun-like
   (lambda ()
     (let (syntax-code)
       ;; Enter.
       (forward-char)
       (context-coloring-elisp-forward-sws)
       (while (/= (setq syntax-code (context-coloring-get-syntax-code))
                  context-coloring-CLOSE-PARENTHESIS-CODE)
         (cond
          ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
           (context-coloring-elisp-parse-arglist))
          (t
           ;; Ignore artifacts.
           (context-coloring-elisp-forward-sexp)))
         (context-coloring-elisp-forward-sws))))))

(defun context-coloring-elisp-colorize-lambda-like (callback)
  "Color the lambda-like function at point.
Parsing the header with CALLBACK."
  (context-coloring-elisp-colorize-scope
   (lambda ()
     (context-coloring-elisp-parse-header callback))))

(defun context-coloring-elisp-colorize-lambda ()
  "Color the `lambda' at point."
  (context-coloring-elisp-colorize-lambda-like
   'context-coloring-elisp-parse-arglist))

(defun context-coloring-elisp-colorize-let ()
  "Color the `let' at point."
  (context-coloring-elisp-colorize-lambda-like
   (lambda ()
     (context-coloring-elisp-parse-let-varlist 'let))))

(defun context-coloring-elisp-colorize-let* ()
  "Color the `let*' at point."
  (context-coloring-elisp-colorize-lambda-like
   (lambda ()
     (context-coloring-elisp-parse-let-varlist 'let*))))

(defun context-coloring-elisp-colorize-macroexp-let2 ()
  "Color the `macroexp-let2' at point."
  (let (syntax-code
        variable)
    (context-coloring-elisp-colorize-scope
     (lambda ()
       (and
        (progn
          (setq syntax-code (context-coloring-get-syntax-code))
          (context-coloring-elisp-identifier-p syntax-code))
        (progn
          (context-coloring-elisp-colorize-sexp)
          (context-coloring-elisp-forward-sws)
          (setq syntax-code (context-coloring-get-syntax-code))
          (context-coloring-elisp-identifier-p syntax-code))
        (progn
          (context-coloring-elisp-parse-bindable
           (lambda (parsed-variable)
             (setq variable parsed-variable)))
          (context-coloring-elisp-forward-sws)
          (when variable
            (context-coloring-elisp-add-variable variable))))))))

(defun context-coloring-elisp-colorize-cond ()
  "Color the `cond' at point."
  (let (syntax-code)
    (context-coloring-elisp-skip-callee-name)
    (while (/= (setq syntax-code (context-coloring-get-syntax-code))
               context-coloring-CLOSE-PARENTHESIS-CODE)
      (cond
       ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
        ;; Colorize inside the parens.
        (let ((start (point)))
          (forward-sexp)
          (context-coloring-elisp-colorize-region
           (1+ start) (1- (point)))
          ;; Exit.
          (forward-char)))
       (t
        ;; Ignore artifacts.
        (context-coloring-elisp-forward-sexp)))
      (context-coloring-elisp-forward-sws))
    ;; Exit.
    (forward-char)))

(defun context-coloring-elisp-colorize-condition-case ()
  "Color the `condition-case' at point."
  (let (syntax-code
        variable
        case-pos
        case-end)
    (context-coloring-elisp-colorize-scope
     (lambda ()
       (setq syntax-code (context-coloring-get-syntax-code))
       ;; Gracefully ignore missing variables.
       (when (context-coloring-elisp-identifier-p syntax-code)
         (context-coloring-elisp-parse-bindable
          (lambda (parsed-variable)
            (setq variable parsed-variable)))
         (context-coloring-elisp-forward-sws))
       (context-coloring-elisp-colorize-sexp)
       (context-coloring-elisp-forward-sws)
       ;; Parse the handlers with the error variable in scope.
       (when variable
         (context-coloring-elisp-add-variable variable))
       (while (/= (setq syntax-code (context-coloring-get-syntax-code))
                  context-coloring-CLOSE-PARENTHESIS-CODE)
         (cond
          ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
           (setq case-pos (point))
           (context-coloring-elisp-forward-sexp)
           (setq case-end (point))
           (goto-char case-pos)
           ;; Enter.
           (forward-char)
           (context-coloring-elisp-forward-sws)
           (setq syntax-code (context-coloring-get-syntax-code))
           (when (/= syntax-code context-coloring-CLOSE-PARENTHESIS-CODE)
             ;; Skip the condition name(s).
             (context-coloring-elisp-forward-sexp)
             ;; Color the remaining portion of the handler.
             (context-coloring-elisp-colorize-region
              (point)
              (1- case-end)))
           ;; Exit.
           (forward-char))
          (t
           ;; Ignore artifacts.
           (context-coloring-elisp-forward-sexp)))
         (context-coloring-elisp-forward-sws))))))

(defun context-coloring-elisp-colorize-dolist ()
  "Color the `dolist' at point."
  (let (syntax-code
        (index 0))
    (context-coloring-elisp-colorize-scope
     (lambda ()
       (setq syntax-code (context-coloring-get-syntax-code))
       (when (= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
         (forward-char)
         (context-coloring-elisp-forward-sws)
         (while (/= (setq syntax-code (context-coloring-get-syntax-code))
                    context-coloring-CLOSE-PARENTHESIS-CODE)
           (cond
            ((and
              (or (= index 0) (= index 2))
              (context-coloring-elisp-identifier-p syntax-code))
             ;; Add the first or third name to the scope.
             (context-coloring-elisp-parse-bindable
              (lambda (variable)
                (context-coloring-elisp-add-variable variable))))
            (t
             ;; Color artifacts.
             (context-coloring-elisp-colorize-sexp)))
           (context-coloring-elisp-forward-sws)
           (setq index (1+ index)))
         ;; Exit.
         (forward-char))))))

(defun context-coloring-elisp-colorize-quote ()
  "Color the `quote' at point."
  (let* ((start (point))
         (end (progn (forward-sexp)
                     (point))))
    (context-coloring-colorize-region
     start
     end
     (context-coloring-elisp-get-current-scope-level))
    (context-coloring-elisp-colorize-comments-and-strings-in-region start end)))

(defvar context-coloring-elisp-callee-dispatch-hash-table
  (let ((table (make-hash-table :test 'equal)))
    (dolist (callee '("defun" "defun*" "defsubst" "defmacro" "cl-defun" "cl-defsubst" "cl-defmacro"))
      (puthash callee #'context-coloring-elisp-colorize-defun table))
    (dolist (callee '("condition-case" "condition-case-unless-debug"))
      (puthash callee #'context-coloring-elisp-colorize-condition-case table))
    (dolist (callee '("dolist" "dotimes"))
      (puthash callee #'context-coloring-elisp-colorize-dolist table))
    (dolist (callee '("let" "gv-letplace"))
      (puthash callee #'context-coloring-elisp-colorize-let table))
    (puthash "let*" #'context-coloring-elisp-colorize-let* table)
    (puthash "macroexp-let2" #'context-coloring-elisp-colorize-macroexp-let2 table)
    (puthash "lambda" #'context-coloring-elisp-colorize-lambda table)
    (puthash "cond" #'context-coloring-elisp-colorize-cond table)
    (puthash "defadvice" #'context-coloring-elisp-colorize-defadvice table)
    (puthash "quote" #'context-coloring-elisp-colorize-quote table)
    (puthash "backquote" #'context-coloring-elisp-colorize-backquote table)
    table)
  "Map function names to their coloring functions.")

(defun context-coloring-elisp-colorize-parenthesized-sexp ()
  "Color the sexp enclosed by parenthesis at point."
  (context-coloring-elisp-increment-sexp-count)
  (let* ((start (point))
         (end (progn (forward-sexp)
                     (point)))
         (syntax-code (progn (goto-char start)
                             (forward-char)
                             ;; Coloring is unnecessary here, it'll happen
                             ;; presently.
                             (context-coloring-forward-sws)
                             (context-coloring-get-syntax-code)))
         dispatch-function)
    ;; Figure out if the sexp is a special form.
    (cond
     ((and (context-coloring-elisp-identifier-p syntax-code)
           (setq dispatch-function (gethash
                                    (buffer-substring-no-properties
                                     (point)
                                     (progn (forward-sexp)
                                            (point)))
                                    context-coloring-elisp-callee-dispatch-hash-table)))
      (goto-char start)
      (funcall dispatch-function))
     ;; Not a special form; just colorize the remaining region.
     (t
      (context-coloring-colorize-region
       start
       end
       (context-coloring-elisp-get-current-scope-level))
      (context-coloring-elisp-colorize-region (point) (1- end))
      (forward-char)))))

(defun context-coloring-elisp-colorize-symbol ()
  "Color the symbol at point."
  (context-coloring-elisp-increment-sexp-count)
  (let* ((symbol-pos (point))
         (symbol-end (progn (forward-sexp)
                            (point)))
         (symbol-string (buffer-substring-no-properties
                         symbol-pos
                         symbol-end)))
    (cond
     ((string-match-p context-coloring-elisp-ignored-word-regexp symbol-string))
     (t
      (context-coloring-colorize-region
       symbol-pos
       symbol-end
       (context-coloring-elisp-get-variable-level
        symbol-string))))))

(defun context-coloring-elisp-colorize-backquote-form ()
  "Color the backquote form at point."
  (let ((start (point))
        (end (progn (forward-sexp)
                    (point)))
        char)
    (goto-char start)
    (while (> end (progn (forward-char)
                         (point)))
      (setq char (char-after))
      (when (= char context-coloring-COMMA-CHAR)
        (forward-char)
        (when (= (char-after) context-coloring-AT-CHAR)
          ;; If we don't do this "@" could be interpreted as a symbol.
          (forward-char))
        (context-coloring-elisp-forward-sws)
        (context-coloring-elisp-colorize-sexp)))
    ;; We could probably do this as part of the above loop but it'd be
    ;; repetitive.
    (context-coloring-elisp-colorize-comments-and-strings-in-region
     start end)))

(defun context-coloring-elisp-colorize-backquote ()
  "Color the `backquote' at point."
  (context-coloring-elisp-skip-callee-name)
  (context-coloring-elisp-colorize-backquote-form)
  ;; Exit.
  (forward-char))

(defun context-coloring-elisp-colorize-expression-prefix ()
  "Color the expression prefix and expression at point.
It could be a quoted or backquoted expression."
  (context-coloring-elisp-increment-sexp-count)
  (cond
   ((/= (char-after) context-coloring-BACKTICK-CHAR)
    (context-coloring-elisp-forward-sexp))
   (t
    (context-coloring-elisp-colorize-backquote-form))))

(defun context-coloring-elisp-colorize-comment ()
  "Color the comment at point."
  (context-coloring-elisp-increment-sexp-count)
  (context-coloring-elisp-forward-sws))

(defun context-coloring-elisp-colorize-string ()
  "Color the string at point."
  (context-coloring-elisp-increment-sexp-count)
  (let ((start (point)))
    (forward-sexp)
    (context-coloring-elisp-colorize-comments-and-strings start (point))))

;; Elisp has whitespace, words, symbols, open/close parenthesis, expression
;; prefix, string quote, comment starters/enders and escape syntax classes only.

(defun context-coloring-elisp-colorize-sexp ()
  "Color the sexp at point."
  (let ((syntax-code (context-coloring-get-syntax-code)))
    (cond
     ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
      (context-coloring-elisp-colorize-parenthesized-sexp))
     ((context-coloring-elisp-identifier-p syntax-code)
      (context-coloring-elisp-colorize-symbol))
     ((= syntax-code context-coloring-EXPRESSION-PREFIX-CODE)
      (context-coloring-elisp-colorize-expression-prefix))
     ((= syntax-code context-coloring-STRING-QUOTE-CODE)
      (context-coloring-elisp-colorize-string))
     ((= syntax-code context-coloring-ESCAPE-CODE)
      (forward-char 2)))))

(defun context-coloring-elisp-colorize-comments-and-strings-in-region (start end)
  "Color comments and strings between START and END."
  (let (syntax-code)
    (goto-char start)
    (while (> end (progn (skip-syntax-forward "^\"<\\" end)
                         (point)))
      (setq syntax-code (context-coloring-get-syntax-code))
      (cond
       ((= syntax-code context-coloring-STRING-QUOTE-CODE)
        (context-coloring-elisp-colorize-string))
       ((= syntax-code context-coloring-COMMENT-START-CODE)
        (context-coloring-elisp-colorize-comment))
       ((= syntax-code context-coloring-ESCAPE-CODE)
        (forward-char 2))))))

(defun context-coloring-elisp-colorize-region (start end)
  "Color everything between START and END."
  (let (syntax-code)
    (goto-char start)
    (while (> end (progn (skip-syntax-forward "^w_('\"<\\" end)
                         (point)))
      (setq syntax-code (context-coloring-get-syntax-code))
      (cond
       ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
        (context-coloring-elisp-colorize-parenthesized-sexp))
       ((context-coloring-elisp-identifier-p syntax-code)
        (context-coloring-elisp-colorize-symbol))
       ((= syntax-code context-coloring-EXPRESSION-PREFIX-CODE)
        (context-coloring-elisp-colorize-expression-prefix))
       ((= syntax-code context-coloring-STRING-QUOTE-CODE)
        (context-coloring-elisp-colorize-string))
       ((= syntax-code context-coloring-COMMENT-START-CODE)
        (context-coloring-elisp-colorize-comment))
       ((= syntax-code context-coloring-ESCAPE-CODE)
        (forward-char 2))))))

(defun context-coloring-elisp-colorize-region-initially (start end)
  "Begin coloring everything between START and END."
  (setq context-coloring-elisp-sexp-count 0)
  (setq context-coloring-elisp-scope-stack '())
  (let ((inhibit-point-motion-hooks t)
        (case-fold-search nil)
        ;; This is a recursive-descent parser, so give it a big stack.
        (max-lisp-eval-depth (max max-lisp-eval-depth 3000))
        (max-specpdl-size (max max-specpdl-size 3000)))
    (context-coloring-elisp-colorize-region start end)))

(defun context-coloring-elisp-colorize-guard (callback)
  "Silently color in CALLBACK."
  (with-silent-modifications
    (save-excursion
      (condition-case nil
          (funcall callback)
        ;; Scan errors can happen virtually anywhere if parenthesis are
        ;; unbalanced.  Just swallow them.  (`progn' for test coverage.)
        (scan-error (progn))))))

;;;###autoload
(defun context-coloring-elisp-colorize ()
  "Color the current Emacs Lisp buffer."
  (interactive)
  (context-coloring-elisp-colorize-guard
   (lambda ()
     (cond
      ;; Just colorize the changed region.
      (context-coloring-changed-p
       (let* (;; Prevent `beginning-of-defun' from making poor assumptions.
              (open-paren-in-column-0-is-defun-start nil)
              ;; Seek the beginning and end of the previous and next
              ;; offscreen defuns, so just enough is colored.
              (start (progn (goto-char context-coloring-changed-start)
                            (while (and (< (point-min) (point))
                                        (pos-visible-in-window-p))
                              (end-of-line 0))
                            (beginning-of-defun)
                            (point)))
              (end (progn (goto-char context-coloring-changed-end)
                          (while (and (> (point-max) (point))
                                      (pos-visible-in-window-p))
                            (forward-line 1))
                          (end-of-defun)
                          (point))))
         (context-coloring-elisp-colorize-region-initially start end)
         ;; Fast coloring is nice, but if the code is not well-formed
         ;; (e.g. an unclosed string literal is parsed at any time) then
         ;; there could be leftover incorrectly-colored code offscreen.  So
         ;; do a clean sweep as soon as appropriate.
         (context-coloring-schedule-coloring context-coloring-default-delay)))
      (t
       (context-coloring-elisp-colorize-region-initially (point-min) (point-max)))))))

;;;###autoload
(puthash
 'emacs-lisp
 (list :modes '(emacs-lisp-mode lisp-interaction-mode)
       :colorizer #'context-coloring-elisp-colorize
       :setup #'context-coloring-setup-idle-change-detection
       :teardown #'context-coloring-teardown-idle-change-detection)
 context-coloring-dispatch-hash-table)


;;; eval-expression colorization

(defun context-coloring-eval-expression-match ()
  "Determine expression start in `eval-expression'."
  (string-match "\\`Eval: " (buffer-string)))

;;;###autoload
(defun context-coloring-eval-expression-colorize ()
  "Color the `eval-expression' minibuffer prompt as elisp."
  (interactive)
  (context-coloring-elisp-colorize-guard
   (lambda ()
     (context-coloring-elisp-colorize-region-initially
      (progn
        (context-coloring-eval-expression-match)
        (1+ (match-end 0)))
      (point-max)))))

;; `eval-expression-minibuffer-setup-hook' is not available in Emacs 24.3, so
;; the backwards-compatible recommendation is to use `minibuffer-setup-hook' and
;; rely on this predicate instead.
;;;###autoload
(defun context-coloring-eval-expression-predicate ()
  "Non-nil if the minibuffer is for `eval-expression'."
  ;; Kinda better than checking `this-command', because `this-command' changes.
  (context-coloring-eval-expression-match))

;;;###autoload
(puthash
 'eval-expression
 (list :predicate #'context-coloring-eval-expression-predicate
       :colorizer #'context-coloring-eval-expression-colorize
       :setup #'context-coloring-setup-idle-change-detection
       :teardown #'context-coloring-teardown-idle-change-detection)
 context-coloring-dispatch-hash-table)

(provide 'context-coloring-emacs-lisp)

;;; context-coloring-emacs-lisp.el ends here
