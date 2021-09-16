;;; buttons.el --- Define and visualize hierarchies of keymaps   -*- lexical-binding: t; -*-

;; Copyright (C) 2018,  Ernesto Alfonso, all rights reserved.

;; Author: Ernesto Alfonso
;; Maintainer: (concat "erjoalgo" "@" "gmail" ".com")
;; Keywords: keymap, template, snippet
;; Package-Version: 20201123.2333
;; Package-Commit: de41b48244574a13000c4289fdb4216a2b0490ff
;; Created: 16 Sep 2018
;; Package-Requires: ((cl-lib "0.3"))
;; URL: http://github.com/erjoalgo/emacs-buttons
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A library and template language to define and visualize keymap hierarchies.

;;; Code:

(require 'cl-lib)

(defvar buttons-make-key-mapper #'identity
  "A function used to map key definitions within a ‘buttons-make’ form.
It should be bound at compile-time via ‘let-when'")

(defvar buttons-make-self-help-binding (kbd "s-?")
  "Key where to install the help visualizer in a buttons-make-defined keymap.")

(defmacro buttons-make (&rest bindings)
  "Create a sparse keymap.

   BINDINGS... is a list of (KEY TARGET) pairs, where KEY
   should be suitable for use as the KEY argument in DEFINE-KEY,
   for example \"<s-f1>\".

   TARGET may be any value that could be passed to the DEF
   argument of DEFINE-KEY, including a command and a keymap,
   including an anonymous keymap created with BUTTONS-MAKE.

   BUTTONS-MAKE-KEY-MAPPER, if non-nil, specifies
   a function to apply to the KEY of each binding
   before it is passed to DEFINE-KEY.
   As an example, it may be used to add a modifier to
   its input key to make the BINDINGS list more concise."

  (let ((kmap-sym (cl-gentemp "kmap-")))
    `(let ((,kmap-sym (make-sparse-keymap)))
       (when buttons-make-self-help-binding
         (define-key ,kmap-sym buttons-make-self-help-binding
           ((lambda (kmap-sym)
              (defalias (make-symbol "keymap-help")
                `(lambda () (interactive)
                   (buttons-display
                    (unless current-prefix-arg ',kmap-sym)))
                "Keymap self-help."))
            ,kmap-sym)))
       ,@(cl-loop
          for (key-spec value . rest) in bindings
          when rest do (error "Malformed key definition: %s %s" key-spec value)
          as key = (funcall buttons-make-key-mapper key-spec)
          collect `(define-key ,kmap-sym ,key ,value))
       ,kmap-sym)))

(defun buttons-modifier-add-super (key-spec)
  "Add the supper modifier to KEY-SPEC, if it is a string.

  If KEY-SPEC is a string, then prefix it with the super modifier,
  otherwise leave it intact.
  Suitable as the value of BUTTONS-MAKE-KEY-MAPPER in ‘buttons-make'"
  (cl-typecase key-spec
    (string (kbd (format
                  (if (= (length key-spec) 1)
                      "s-%s"
                    "<s-%s>")
                  key-spec)))
    (t key-spec)))

(defmacro defbuttons (kmap-sym ancestor-kmap target-keymap-syms keymap)
  "Define a keymap KMAP-SYM.

   ANCESTOR-KMAP, if non-nil,is merged recursively onto
   KMAP-SYM via BUTTONS-DEFINE-KEYMAP-ONTO-KEYMAP.

   TARGET-KEYMAP-SYMS is a list of keymap symbols, bound or unbound,
   onto which to define KMAP-SYM via BUTTONS-AFTER-SYMBOL-LOADED-FUNCTION-ALIST.

   KEYMAP is the keymap, for example, one defined via BUTTONS-MAKE."
  (declare (indent 3))
  (let* ((sym-name (symbol-name kmap-sym)))
    `(progn
       (defvar ,kmap-sym nil ,(format "%s buttons map" sym-name))
       (setf ,kmap-sym ,keymap)
       ,@(when ancestor-kmap
           `((buttons-define-keymap-onto-keymap ,ancestor-kmap ,kmap-sym ',kmap-sym t)))
       ,@(cl-loop for orig in (if (and target-keymap-syms
                                       (atom target-keymap-syms))
                                  (list target-keymap-syms)
                                target-keymap-syms)
                  as form = `(buttons-define-keymap-onto-keymap ,kmap-sym ,orig ',orig)
                  append
                  (if (boundp orig)
                      `(,form)
                    `((push (cons ',orig (lambda () ,form))
                            buttons-after-symbol-loaded-function-alist)))))))

(defun keymap-symbol (keymaps)
  "Return the symbol to which any keymap in KEYMAPS is bound."
  (let (syms)
    (mapatoms (lambda (sym)
                (and (not (eq sym 'keymap))
                     (boundp sym)
                     (find (symbol-value sym) keymaps)
                     (push sym syms))))
    syms))

(defun buttons-define-keymap-onto-keymap (from-map to-map &optional from-sym no-overwrite-p)
  "Define bindings FROM-MAP onto TO-MAP, recursively.

   If a binding A in FROM-MAP doesn't exist on TO-MAP, define A onto TO-MAP.
   Otherwise, if a binding is a prefix key on both maps, merge recursively.
   Otherwise FROM-MAP's binding overwrites TO-MAP's binding
   only when NO-OVERWRITE-P is non-nil.

   The optional argument FROM-SYM is used for visualization."
  (cl-labels
      ((merge
        (from-map to-map &optional path)
        (map-keymap
         (lambda (key cmd)
           (let* ((keyvec (vector key))
                  (existing (lookup-key to-map keyvec)))
             (cond
              ((and (keymapp cmd) (keymapp existing))
               (merge cmd existing (cons (key-description keyvec) path)))
              ((or (not no-overwrite-p) (not existing))
               (when (and existing (keymapp existing))
                 (warn
                  (concat "%s overwrites nested keymap with plain command "
                          "on %s %s in map %s: %s overwrites %s")
                  (or (symbol-name from-sym) "child")
                  (key-description keyvec)
                  (or (reverse path) "")
                  (keymap-symbol (list to-map))
                  cmd
                  existing))
               (define-key to-map keyvec cmd)))))
         from-map)
        to-map))
    (merge from-map to-map)))

(defvar buttons-after-symbol-loaded-function-alist nil
  "An alist where each element has the form (SYMBOL . FUNCTION).

   FUNCTION takes no arguments and is evaluated after SYMBOL has been bound.
   If SYMBOL is currently bound, FUNCTION is called immediately.")

(defun buttons-after-symbol-loaded (_file-loaded)
  "Function invoked after new symbols may have been defined in FILE-LOADED.

   Iterates over list of pending items in
   ‘buttons-after-symbol-loaded-function-alist',
   evaluating and removing entries for symbols that have become bound."
  (setf buttons-after-symbol-loaded-function-alist
        (cl-loop for (sym . fun) in buttons-after-symbol-loaded-function-alist
                 if (boundp sym) do
                 (progn
                   (condition-case err (funcall fun)
                     ('error
                      (warn "WARNING: unable to load action %s for symbol %s: %s"
                            sym fun err))))
                 else collect (cons sym fun))))

(add-hook 'after-load-functions #'buttons-after-symbol-loaded)

(defun buttons-read-keymap ()
  "Interactively read a keymap symbol.  Based on ‘help-fns+'."
  (intern
   (completing-read "Keymap: " obarray
                    (lambda (m) (and (boundp m)))
                    t
                    (when (symbol-at-point)
                      (symbol-name (symbol-at-point)))
                    'variable-name-history)))

(defun buttons-display (&optional keymap hide-command-names hide-command-use-count)
  "Visualize a keymap KEYMAP in a help buffer.

   Unlike the standard keymap bindings help, nested keymaps
   are visualized recursively.  This is suitable for visualizing
   BUTTONS-MAKE-defined nested keymaps.

   If HIDE-COMMAND-NAMES is non-nil, command names are not displayed.

   If HIDE-COMMAND-USE-COUNT is non-nil, no attempt is made to display
   recorded command use-counts.

   When called with a nil keymap, or interactively with a prefix argument,
   all currently active keymaps are displayed."
  (interactive (unless current-prefix-arg
                 (list (buttons-read-keymap))))
  (let ((min-sep 2) (max-command-name-length 30) (use-count-padding 6))
    (cl-labels ((event-to-string (event)
                                 (key-description (vector event)))
                (print-key (event)
                           (princ (event-to-string event)))
                (spaces (len) (make-string len 32))
                (maybe-truncate (string max)
                                (if (>= max (length string))
                                    string
                                  (cl-assert (>= max 3))
                                  (concat (cl-subseq string 0 (- max 3)) "...")))
                (remove-newlines (string)
                                 (replace-regexp-in-string "\n" "\\\\n" string))
                (print-command (binding)
                               (unless hide-command-names
                                 (if (and (commandp binding);;not a keymap
                                          (symbolp binding);;not an anonymous lambda
                                          binding)
                                     (insert-text-button
                                      (maybe-truncate (remove-newlines (prin1-to-string binding))
                                                      max-command-name-length)
                                      :type 'help-function
                                      'help-args (list binding)
                                      'button '(t))
                                   (princ (remove-newlines (prin1-to-string binding)))))
                               (unless hide-command-use-count
                                 (let ((use-count-width
                                        (and (symbolp binding)
                                             (< 0 (or (get binding 'use-count) 0))
                                             (length (princ
                                                      (format "<%d>" (get binding 'use-count)))))))
                                   (princ (spaces (- use-count-padding (or use-count-width 0))))))
                               (when (and (commandp binding)
                                          (documentation binding))
                                 (princ (spaces min-sep))
                                 (princ (remove-newlines (documentation binding)))))
                (print-keymap (keymap level sep)
                              (map-keymap (lambda (event binding)
                                            (princ (spaces (* level sep)))
                                            (let ((event-desc (print-key event)))
                                              (cl-assert (> sep (length event-desc)))
                                              (princ (spaces (- sep (length event-desc)))))
                                            (if (keymapp binding)
                                                (progn
                                                  (princ "\n")
                                                  (print-keymap binding (1+ level) sep))
                                              (print-command binding)
                                              (princ "\n")))
                                          keymap))
                (find-keymap-descriptor (keymap)
                                        (or
                                         (cl-block nil
                                           (mapatoms (lambda (sym)
                                                       (when (and
                                                              (not (eq sym 'keymap))
                                                              (boundp sym)
                                                              (eq (symbol-value sym) keymap))
                                                         (cl-return sym)))))
                                         (cl-loop for (minor-mode-sym . kmap)
                                                  in minor-mode-map-alist
                                                  thereis
                                                  (when (equal kmap keymap)
                                                    (format "%s (minor-mode-map-alist)"
                                                            minor-mode-sym)))))
                (max-event-length (keymap)
                                  (let ((max 0))
                                    (map-keymap
                                     (lambda (event binding)
                                       (setf max
                                             (max max (length (event-to-string event))
                                                  (if (keymapp binding)
                                                      (max-event-length binding) 0))))
                                     keymap)
                                    max)))
      (cl-destructuring-bind (name kmaps)
          (cond
           ((null keymap) (list "(current-active-maps)" (current-active-maps)))
           ((symbolp keymap) (list (symbol-name keymap) (list (symbol-value keymap))))
           (t (list (find-keymap-descriptor keymap) (list keymap))))
        (let ((max-event-length (cl-loop for kmap in kmaps
                                         maximize (max-event-length kmap)))
              (buffer-name (format "*%s help*" (or name "keymap")))
              (help-window-select t))
          (with-help-window buffer-name
            (with-current-buffer
                buffer-name
              (dolist (kmap kmaps)
                (princ (or (find-keymap-descriptor kmap) "(anonymous keymap)"))
                (add-text-properties (line-beginning-position)
                                     (line-end-position)
                                     '(face bold))
                (princ ":\n")
                (print-keymap kmap 0 (+ max-event-length min-sep))
                (princ "\n\n\n"))
              (toggle-truncate-lines t))))))))

(unless (lookup-key help-map "M")
  (define-key help-map "M" #'buttons-display))

(defvar buttons-template-insert-directive-regexp
  "{\\(.*?\\)}"
  "Determines what buttons-template-insert interprets as a directive.

   BUTTONS-TEMPLATE-INSERT-DIRECTIVE-REGEXP may be used to set the regexp
   that defines directives to interpret.  The first capture group is used
   as the directive contents.  Note that this variable should be bound
   via ‘let-when-compile' instead of ‘let' to make this binding available
   at macro-expansion time.")

(defmacro buttons-template-insert (&rest templates)
  "Compile a string template into a progression of LISP commands.

   The template may be split into several arguments TEMPLATES,
   each of which is compiled.  If an argument is not a string,
   it is used as a raw LISP expression.  Otherwise,

   Any directive {DIRECTIVE} within curly braces is interpreted:

       If DIRECTIVE is the empty string, the function
       ‘buttons-record-template-var'is invoked to allow the user to enter text.

       If DIRECTIVE is a number K, the function ‘buttons-record-template-var'
       is invoked to allow the user to enter text on the first occurrence
       of the directive K in the template, and on subsequent occurrences
       the recorded text is entered without prompt.

       Otherwise, DIRECTIVE is interpreted as a LISP expression.
       If the expression evaluates to a string, it is inserted.

    Any text outside directives is inserted literally.

    BUTTONS-TEMPLATE-INSERT-DIRECTIVE-REGEXP may be used to change the regexp
    that defines directives to interpret.  The first capture group is used
    as the directive contents.  Note that this variable should be bound
    via ‘let-when-compile' instead of ‘let' to make this binding available
    at macro-expansion time.  Also note that a substring is not considered
    a directive if it does not match the directive regexp within a single
    string.

    Example:

    for ( int {0} = 0; {0} < {}; {0}++ ){(cbd)}

    Expands into:

        - insert 'for ( int '
        - enter recursive edit and record entered text as a string labeled '0'
        - insert ' = ; '
        - insert the already-recorded string 0
        - insert ' < '
        - enter recursive edit, no recording is done
        - enter '; '
        - insert the already-recorded string 0
        - insert '++ )
        - expand into the form: (cbd), which should be a valid LISP expression"

  (cl-loop for tmpl in templates
           with forms = nil
           with rec-sym-alist = nil
           with directive-regexp = buttons-template-insert-directive-regexp
           with insert-if-string =
           (lambda (form)
             (let ((expr-val-sym (gensym "expr-val-")))
               `(let* ((,expr-val-sym ,form))
                  (when (stringp ,expr-val-sym)
                    (insert ,expr-val-sym)))))
           do
           (if (not (stringp tmpl))
               (push (funcall insert-if-string tmpl) forms)
             (cl-loop with start = 0
                      as rec-capture-start = (string-match directive-regexp tmpl start)
                      do (if rec-capture-start
                             (progn
                               (unless (= start rec-capture-start)
                                 (push `(insert ,(cl-subseq tmpl start rec-capture-start)) forms))
                               (let ((group-no-str (match-string 1 tmpl))
                                     (match-data (match-data)))
                                 (cond
                                  ((zerop (length group-no-str))
                                   (push `(buttons-record-template-var) forms))
                                  ((string-match "^[0-9]+$" group-no-str)
                                   (let* ((group-no (string-to-number group-no-str))
                                          (sym (cdr (assoc group-no rec-sym-alist))))
                                     (if sym
                                         (push `(insert ,sym) forms)
                                       (setf sym (gensym (format "rec-capture-%d-" group-no)))
                                       (push (cons group-no sym) rec-sym-alist)
                                       (push `(setf ,sym (buttons-record-template-var)) forms))))
                                  (t (push (funcall insert-if-string (read group-no-str))
                                           forms)))
                                 (set-match-data match-data)
                                 (setf start (match-end 0))))
                           (progn (when (< start (length tmpl))
                                    (push `(insert ,(cl-subseq tmpl start)) forms))
                                  (setf start (length tmpl))))
                      while rec-capture-start))
           finally (return `(let ,(mapcar 'cdr rec-sym-alist)
                              ;; (doc ,tmpl)
                              ,@(reverse forms)))))

(defcustom buttons-record-template-var-method
  'recedit
  "Specifies how ‘buttons-record-template-var' should prompt for template variables."
  :type 'symbol
  :group 'emacs-buttons)

(defun buttons-record-template-var ()
  "Insert and record some text from the user.

   If the value of ‘buttons-record-template-var' is

   - 'recedit: enter a recursive edit and record any text entered there
   - 'prompt: use a minibuffer prompt."

  (cl-case buttons-record-template-var-method
    ('recedit (let ((old-point (point)))
                (recursive-edit)
                (buffer-substring-no-properties old-point (point))))
    ('prompt (insert (read-string "enter template variable: ")))
    ((t) (error "Invalid value: %s" buttons-record-template-var-method))))

(defmacro buttons-defcmd (&rest body)
  "Define an anonymous command with body BODY.

   The number of times the command is invoked is recorded
   as the USE-COUNT property of the function symbol.
   This may be useful for analysis and for making
   decisions about which bindings' key-sequence
   lengths are worth shortening."
  (cl-loop for form in body
           with forms = nil
           with doc = nil
           with cmd-name = (cl-gentemp "autogen-cmd-")
           with point-original-sym = (gensym "point-original-")
           do (if (and (consp form)
                       (eq (car form) 'doc))
                  (push (cadr form) doc)
                (push form forms))
           finally
           (return
            `(progn
               (put ',cmd-name 'use-count (or (get ',cmd-name 'use-count) 0))
               (defun ,cmd-name ()
                 ,(apply 'concat (reverse (mapcar 'prin1-to-string forms)))
                 (interactive)
                 (cl-incf (get ',cmd-name 'use-count))
                 (cl-block ,cmd-name
                   (let ((,point-original-sym (point)))
                     (catch 'buttons-abort
                       ,@(reverse forms)
                       (cl-return-from ,cmd-name))
                     ;; aborted. undoing...
                     (undo-boundary)
                     (delete-region ,point-original-sym (point)))))))))

(defun buttons-abort-cmd ()
  "Throw the tag required to abort the current buttons-defined command."
  (interactive)
  (message "aborting buttons command...")
  (throw 'buttons-abort nil))

(defun buttons-insert-c-style-code-block ()
  "Insert a c-style code block with curly braces."
  (interactive)
  (insert " {")
  (newline-and-indent)
  (recursive-edit)
  (newline)
  (insert "}")
  (indent-for-tab-command))

(defmacro buttons-macrolet (more-macrolet-defs &rest body)
  "Make short aliases of useful button-related forms available within BODY.

   Provides a compact DSL for defining buttons.
   MORE-MACROLET-DEFS specifies additional user-defined cl-macrolet forms."
  `(cl-macrolet
       ((but (&rest rest) `(buttons-make ,@rest))
        (nli () `(newline-and-indent))
        (ins (&rest text) `(buttons-template-insert ,@text))
        (cmd (&rest rest) `(buttons-defcmd ,@rest))
        (cbd () `(buttons-insert-c-style-code-block))
        (rec () `(recursive-edit))
        (idt () `(indent-for-tab-command))
        (cmt (&rest rest) `(comint-send-input ,@rest))
        (cmd-ins (&rest rest) `(cmd (ins ,@rest)))
        ,@more-macrolet-defs)
     ,@body))

(provide 'buttons)
;;; buttons.el ends here
