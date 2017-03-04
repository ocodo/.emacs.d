;;; php-funcs.el --- Utility functions for php and zf modes.

;; Version: 2.0
;; Created: 10-1-2009
;; Copyright © 2009 Brian Zwahr
;; Author(s): 
;; Brian Zwahr <echosa@gmail.com>

;;; *****
;;; About
;;; *****

;; php-funcs.el is a part of the php+-mode suite and contains
;; convenience functions for php programming, such as moving between
;; methods and quick insertion of common code.

;; The functions below work in the standard that places the opening
;; brace for classes, functions, etc. on the next line and for ifs,
;; fors, etc. on the same line.

;; ********************************************************************

;; ************
;; REQUIREMENTS
;; ************
(require 'cl)
(require 'php-parse)
(require 'thingatpt)

;; *********
;; VARIABLES
;; *********
(defcustom php-hide-show-ignore-extensions '()
  "File extensions to ignore when running hide-show
automatically."
  :type '(repeat :tag "File extension" (string))
  :group 'php)

(defcustom php-hide-show-hide-doc-blocks nil
  "Whether `php-hide-innards' should hide the initial
docblocks.  This will currently only work on
constant/property/method docblocks.  The ability to choose
file/class/interface docblocks may be added in the future."
  :type 'boolean
  :group 'php)

(defcustom php-jump-show-hidden t
  "Whether jump commands should show things hidden by hideshow."
  :type 'boolean
  :group 'php)

(defcustom php-doc-tag "EOT"
  "The default tag to use for PHP {here,now}docs."
  :type 'string
  :group 'php)

; declared for compiler
(defvar php+-mode-protected-underscore)

;; *********
;; FUNCTIONS
;; *********
; declarations for compiler
(declare-function hs-show-all "hideshow")
(declare-function hs-show-block "hideshow")
(declare-function hs-hide-block "hideshow")
(declare-function hs-overlay-at "hideshow")
(declare-function php-single-quote-string "php-string")
(declare-function php-format-region "php-format")
(declare-function php-format-spacing "php-format")
(declare-function php-doc-class-insert "php-doc")
(declare-function php-doc-method-insert "php-doc")

(defun php-jump-to-first-struct (type)
  "Jumps to the first TYPE structure in the file.  TYPE may be a
list."
  (let ((types (if (listp type) type `(,type)))
        (file (php-parse-current 'file))
        (structs '()))
    (when (php-parse-p file)
      (let* ((scripts (rest (assq 'scripts file)))
             (structs 
              (catch 'found
                (dolist (type types)
                  (let ((structs 
                         (apply 'append
                                (mapcar 
                                 (lambda (s) 
                                   (rest (assq (php-parse-pluralize-type type) 
                                               s))) scripts))))
                    (when structs
                      (throw 'found structs)))))))
        (when structs
          (let ((first-struct (catch 'found
                                (dolist (s structs)
                                  (when (php-parse-p s)
                                    (throw 'found s))))))
            (goto-char (rest (assq 'begin first-struct)))))))))

(defun php-jump-to-first-class/interface ()
  "Jumps to the first class/interface in the file."
  (php-jump-to-first-struct '(class interface)))

(defun php-hide-innards (&optional show just-doc-blocks force-doc-blocks)
  "This function goes through the buffer and hides (closes) all
methods in a class. If `php-hide-show-hide-doc-blocks' is set, it
will also hide any accompanying doc blocks.  Optionally, SHOW
instead of hide.  You may also hide JUST-DOC-BLOCKS and/or
FORCE-DOC-BLOCKS.  This requires hideshow."
  (interactive)
  (when (and hs-minor-mode
             (buffer-file-name)
             (not (member (file-name-extension (buffer-file-name) t) 
                          php-hide-show-ignore-extensions)))
    (save-excursion
      (goto-char (point-min))
      (let ((in-class (php-jump-to-first-class/interface)))
        (when in-class
          (php-jump-to-first-statement))
        (when (not just-doc-blocks)
          (if show
              (hs-show-all)
            (hs-hide-level (if in-class 1 0))))
        (when (or force-doc-blocks php-hide-show-hide-doc-blocks)
          (let* ((top-struct (php-parse-current (if in-class 
                                                    '(class interface)
                                                  'script)))
                 (structs (if in-class
                              (append (rest (assq 'constants top-struct))
                                      (rest (assq 'properties top-struct))
                                      (rest (assq 'methods top-struct)))
                            (rest (assq 'methods top-struct)))))
            (dolist (struct structs)
              (goto-char (rest (assq 'begin struct)))
              (when (looking-at-p "/")
                (if show
                    (hs-show-block)
                  (hs-hide-block))))))))))

(defun php-show-all ()
  "This is just a wrapper around hideshow's hs-show-all.
We have this wrapper to make sure that hide-show is loaded first."
  (when hs-minor-mode
    (hs-show-all)))

(defun php-get-named-things (&optional types)
  "Returns a list of parses of all named things in the current
buffer.  Optionally limit to TYPES"
  (let* ((types (or types '(interfaces classes methods properties constants)))
         (types (if (listp types) types `(,types)))
         (file (php-parse-current 'file)))
    (when (php-parse-p file)
      (apply 'append (mapcar (lambda (script)
                               (let (things)
                                 (dolist (type types things)
                                   (setf things 
                                         (append (rest (assq type script)) 
                                                 things)))))
                             (rest (assq 'scripts file)))))))

(defun php-get-thing-names (&optional types)
  "Returns a list of all the names of PHP structures in the
current buffer.  Optionally limit to TYPES."
  (mapcar (lambda (thing)
            (rest (assq 'name thing)))
          (php-get-named-things types)))

(defun php-get-method-arg-names (&optional with-dollar method-name)
  "Return a list of argument names for the method at point or
METHOD-NAME."
  (when (or (not method-name)
            (php-jump-to-thing method-name))
    (let ((parse (php-parse-current 'method)))
      (when (php-parse-p parse)
        (mapcar (lambda (x) 
                  (if with-dollar
                      (rest (assoc 'name x))
                    (replace-regexp-in-string "[$&]" "" 
                                              (rest (assoc 'name x)))))
                (rest (assoc 'arguments parse)))))))

(defun php-find-thing (name)
  "Finds the PHP structure named NAME in the
current buffer."
  (let* ((file (php-parse-current 'file)))
    (catch 'found
      (dolist (script (rest (assq 'scripts file)))
        (dolist (thing (append (rest (assq 'interfaces script))
                               (rest (assq 'classes script))
                               (rest (assq 'methods script))
                               (rest (assq 'properties script))
                               (rest (assq 'constants script))))
          (when (string= name (rest (assq 'name thing)))
            (throw 'found thing)))))))

(defvar php-jump-showed-hidden nil
  "Whether php-jump-first-statement ran hs-show-block.")

(defun php-jump-to-declaration (&optional type)
  "Jumps to the first non-documentation line in the current
struct.  Optionally restrict TYPE according to the semantics of
`php-parse-current'.  Returns (point) at the location."
  (let* ((type (or type '(constant property method class interface script)))
         (parse (php-parse-current type)))
    (when (php-parse-p parse)
      (goto-char (rest (assq 'begin parse)))
      (goto-char (+ 1 (point) (length (rest (assq 'documentation-text parse)))))
      (beginning-of-line-non-whitespace)
      (point))))

(defun php-jump-to-first-statement (&optional struct no-show)
  "Auxilliary function.  Will start at beginning of struct or
point.  Finds the beginning of the first statment in struct.
Will show any hide-show hidden blocks unless NO-SHOW.
Returns (point)."
  (unless (php-parse-p struct)
    (setq struct (php-parse-current '(constant property method class 
                                               interface))))
  (when (php-parse-p struct)
    (goto-char (rest (assq 'begin struct))))
  (when (looking-at-p "/\\*")
    (forward-char 3)
    (when (and hs-minor-mode (hs-overlay-at (point)) php-jump-show-hidden 
               (not no-show))
      (hs-show-block)
      (setq php-jump-showed-hidden t))
    (re-search-forward (concat "\\*/" ws-re "*") nil t))
  (let ((begin (point))
        (type (rest (assq 'type struct))))
    (when (or (member type '(constant property))
              (and (eq type 'method)
                   (not (rest (assq 'abstractp struct)))))
      (save-excursion
        (let ((end (save-excursion
                     (cond ((eq type 'method)
                            (re-search-forward "{" nil t)
                            (re-search-backward ")" nil t))
                           (t (re-search-forward ";" nil t)))
                     (point))))
          (when (re-search-forward "(" end t)
            (backward-char)
            (catch 'done
              (while (< (point) end)
                (forward-char)
                (when (and hs-minor-mode (hs-overlay-at (point)) 
                           php-jump-show-hidden (not no-show))
                  (hs-show-block)
                  (setq php-jump-showed-hidden t)
                  (throw 'done (point)))))))))
    (re-search-forward "[;{]" nil t)
    (when (and hs-minor-mode (hs-overlay-at (point)) php-jump-show-hidden 
               (not no-show))
      (hs-show-block)
      (setq php-jump-showed-hidden t))
    (if (= (char-before) ?\;)
        (goto-char begin)
      (let ((begin (point)))
        (re-search-forward non-ws-re nil t)
        (if (= (char-before) ?\})
            (goto-char begin)
          (backward-char)))))
  (point))

(defun php-jump-hide-if-last-command-was-jump ()
  (when (and (member last-command '(php-jump-to-next-thing
                                    php-jump-to-previous-thing))
             php-jump-showed-hidden)
    (let ((parse (php-parse-current 'method)))
      (when (and hs-minor-mode
                 (eq 'method (rest (assq 'type parse)))
                 (not (rest (assq 'abstractp parse))))
        (hs-hide-block)
        (re-search-backward ")" nil t)
        (hs-hide-block)
        (setq php-jump-showed-hidden nil))
      (when (and hs-minor-mode
                 php-hide-show-hide-doc-blocks
                 (rest (assq 'documentation parse)))
        (save-excursion
          (goto-char (rest (assq 'begin parse)))
          (hs-hide-block)
          (setq php-jump-showed-hidden nil))))))

(defun php-get-first/last-struct-in-class/interface (first-or-last)
  "Returns the parse of the FIRST-OR-LAST
constant/property/method in the first class/interface in the
file."
  (let* ((things (php-get-named-things))
         (parse 
          (first 
           (sort (remove-if-not (lambda (p)
                                  (let ((type 
                                         (rest (assq 'type p))))
                                    (member type 
                                            '(class interface)))) things)
                 (lambda (a b)
                   (cond ((eq first-or-last 'last)
                          'php-parse-pos>)
                         ((eq first-or-last 'first)
                          'php-parse-pos<))))))
         (parse 
          (first 
           (sort (append (rest (assq 'constants parse))
                         (rest (assq 'properties parse))
                         (rest (assq 'methods parse)))
                 (cond ((eq first-or-last 'last) 'php-parse-pos>)
                       ((eq first-or-last 'first) 'php-parse-pos<))))))
    parse))

(defun php-jump-to-next/previous-thing (direction)
  "This function moves the cursor to the first statement in the
next/previous PHP structure."
  (unless (member direction '(previous next))
    (error (format "Invalid direction %s!" direction)))
  (let ((parse (php-parse-current '(constant property method))))
    (unless (catch 'moved
              (when (php-parse-p parse)
                (let ((begin (rest (assq 'begin parse)))
                      (end (rest (assq 'end parse)))
                      (first-statement (save-excursion 
                                         (let ((php-jump-show-hidden nil))
                                           (php-jump-to-first-statement)))))
                  (if (or (and (eq direction 'previous) 
                               (and (> (point) first-statement) 
                                    (<= (point) end)))
                          (and (eq direction 'next)
                               (and (>= (point) begin) 
                                    (< (point) first-statement))))
                      (progn
                        (php-jump-to-first-statement)
                        (throw 'moved t))
                    (let ((first-or-last (cond ((eq direction 'previous) 'first)
                                               ((eq direction 'next) 'last))))
                      (when (equal 
                             parse 
                             (php-get-first/last-struct-in-class/interface
                              first-or-last))
                        (throw 'moved t)))
                    (goto-char (cond ((eq direction 'previous) begin)
                                     ((eq direction 'next) end)))
                    (when (and (eq direction 'next) (looking-at-p "//"))
                      (end-of-line))
                    (throw 'moved nil)))))
      (let* ((search-func (cond ((eq direction 'previous) 're-search-backward)
                                ((eq direction 'next) 're-search-forward)))
             (thing (save-excursion
                      (funcall search-func non-ws-re nil t)
                      (when (eq direction 'previous)
                        (let ((bound (point)))
                          (beginning-of-line)
                          (if (re-search-forward "//" bound t)
                              (backward-char 2)
                            (goto-char bound))))
                      (let* ((parse (php-parse-current '(constant property 
                                                                  method 
                                                                  class 
                                                                  interface
                                                                  script
                                                                  file))))
                        (when (php-parse-p parse)
                          (let ((parse-type (rest (assq 'type parse))))
                            (cond 
                             ((member parse-type 
                                      '(constant property method)) parse)
                             (t (php-get-first/last-struct-in-class/interface 
                                 (cond ((eq direction 'previous) 'last)
                                       ((eq direction 'next) 'first)))))))))))
        (if thing
            (progn
              (when (php-parse-p parse)
                (save-excursion
                  (cond ((eq direction 'previous)
                         (forward-char))
                        ((eq direction 'next)
                         (backward-char)))
                  (let ((php-jump-show-hidden nil))
                    (php-jump-to-first-statement)
                    (php-jump-hide-if-last-command-was-jump))))
              (goto-char (rest (assq (cond ((eq direction 'previous) 'end)
                                           ((eq direction 'next) 'begin))
                                     thing)))
              (php-jump-to-first-statement))
          (cond ((eq direction 'previous)
                 (backward-char))
                ((eq direction 'next)
                 (forward-char)))
          (php-jump-to-first-statement))))))

(defun php-jump-to-next-thing ()
  "This function moves the cursor to the first statement in the
next PHP structure."
  (interactive)
  (php-jump-to-next/previous-thing 'next))

(defun php-jump-to-previous-thing ()
  "This function moves the cursor to the first statement in the
previous PHP structure."
  (interactive)
  (php-jump-to-next/previous-thing 'previous))

(defun php-jump-to-thing (name)
  "Jumps to the beginning of the PHP structure named NAME in the
current buffer."
  (interactive `(,(completing-read "Jump to: " (php-get-thing-names) nil t)))
  (let ((thing (php-find-thing name)))
    (when (php-parse-p thing)
      (goto-char (rest (assq 'begin thing))))))

(defun* php-insert-get-interfaces (&key (verb "implements") 
                                        completion-filter default)
  "This function gathers interfaces for a PHP class definition."
  (let ((interface-list default))
    (catch 'done
      (while t
        (let ((answer 
               (completing-read 
                (format 
                 "Modify interfaces %s ([a]dd/[d]elete/[c]lear/e[X]it)? " 
                 (or interface-list "(*none*)")) 
                '("a" "A" "d" "D" "c" "C" "x" "X") nil t nil nil "x")))
          (cond ((member answer '("x" "X"))
                 (throw 'done t))
                ((member answer '("a" "A"))
                 (let ((interface 
                        (completing-read (concat (upcase-initials verb) ": ")
                                         (append (php-completion-get-etags)
                                                 (php-completion-candidates 
                                                  "" '("class")))
                                         completion-filter nil default)))
                   (unless (nil-or-blank interface)
                     (add-to-list 'interface-list interface t))))
                ((member answer '("c" "C"))
                 (setf interface-list nil))
                ((member answer '("d" "A"))
                 (let ((interface (completing-read "Delete: " interface-list)))
                   (unless (nil-or-blank interface)
                     (setf interface-list 
                           (remove interface interface-list)))))))))
    interface-list))

(defun* php-get-insert-interface-arguments (&optional name &key 
                                                      (extends nil extends-p)
                                                      extends-completion-filter
                                                      extends-default
                                                      (finalp nil finalp-p)
                                                      (abstractp nil 
                                                                 abstractp-p))
  "Read arguments necessary for an interface definition."
  (let ((class-completion-list (append (php-completion-get-etags)
                                       (php-completion-candidates 
                                        "" '("class")))))
    `(,(or name (completing-read "Interface name: " class-completion-list))
      ,(if extends-p 
           extends 
         (php-insert-get-interfaces :verb "extends"
                                    :completion-filter
                                    extends-completion-filter
                                    :default extends-default)))))

(defun* php-get-insert-class-arguments (&optional name &key 
                                                  (extends nil extends-p)
                                                  extends-completion-filter
                                                  extends-default
                                                  (implements nil implements-p)
                                                  implements-completion-filter
                                                  implements-default
                                                  (finalp nil finalp-p)
                                                  (abstractp nil abstractp-p))
  "Read arguments necessary for a class definition."
  (let ((class-completion-list (append (php-completion-get-etags)
                                       (php-completion-candidates 
                                        "" '("class")))))
    `(,(or name (completing-read "Class name: " class-completion-list))
      ,(if extends-p 
           extends 
         (let ((input (completing-read "Extends: " class-completion-list
                                       extends-completion-filter nil
                                       extends-default nil extends-default)))
           (unless (nil-or-blank input) input)))
      ,(if implements-p 
           implements
         (php-insert-get-interfaces :completion-filter
                                    implements-completion-filter
                                    :default implements-default))
      ,(if finalp-p
           finalp
         (y-or-n-p "Make final? "))
      ,(if abstractp-p 
           abstractp
         (y-or-n-p "Make abstract? ")))))

(defun php-insert-interface (name &optional extends)
  "Inserts an interface element named NAME in the buffer, with
the passed-in properties. EXTENDS may be lists."
  (interactive (php-get-insert-interface-arguments))
  (if (nil-or-blank name)
      (message "Must provide a name!")
    (call-interactively 'php-doc-interface-insert)
    (let* ((extends (if (listp extends) extends (list extends)))
           (extend-string (mapconcat 'identity extends ", "))
           (interface-def 
            (concat "interface " (upcase-initials name)
                    (if (and (stringp extend-string)
                             (not (nil-or-blank extend-string)))
                        (concat " extends " (upcase-initials extend-string))
                      ""))))
      (insert interface-def)
      (newline)
      (insert "{")
      (php-format-break-statement)
      (newline-and-indent)
      (save-excursion
        (newline)
        (insert "}")))))

(defun* php-insert-class (name &optional extends implements finalp abstractp
                               &key (doc-args nil doc-args-p))
  "Inserts a class named NAME in the buffer, with the
passed-in properties. Both EXTENDS and IMPLEMENTS may be lists."
  (interactive (php-get-insert-class-arguments))
  (if (nil-or-blank name)
      (message "Must provide a name!")
    (if doc-args-p
        (apply 'php-doc-class-insert doc-args)
      (call-interactively 'php-doc-class-insert))
    (php-insert-class-definition name extends implements finalp abstractp)
    (save-excursion
      (newline)
      (insert "}"))))

(defun php-insert-class-definition (name extends implements finalp abstractp)
    "Inserts a properly formatted class definition for NAME into
the buffer with the supplied parameters."
    (let* ((implements (if (listp implements) implements (list implements)))
           (implement-string (mapconcat 'identity implements ", "))
           (class-def 
            (concat (if finalp "final " "")
                    (if abstractp "abstract " "")
                    "class " (upcase-initials name)
                    (if (and (stringp extends)
                             (not (nil-or-blank extends)))
                        (concat " extends " (upcase-initials extends))
                      "")
                    (if (and (stringp implement-string)
                             (not (nil-or-blank implement-string)))
                        (concat " implements " 
                                (upcase-initials implement-string)) ""))))
      (insert class-def)
      (newline)
      (insert "{")
      (unless (looking-at-p "[[:space:]]*$")
        (save-excursion
          (newline-and-indent)))
      (php-format-break-statement)))

(defun* php-get-insert-constant-arguments (&optional name value)
  "Read arguments necessary for a constant definition."
  `(,(or name 
         (read-string "Constant name: "))
    ,(or value (read-string "Value: "))))

(defun* php-insert-constant (name &optional value 
                                  &key (doc-args nil doc-args-p))
  "Inserts a constant named NAME with value VALUE into the
buffer."
  (interactive (php-get-insert-constant-arguments))
  (if (nil-or-blank name)
      (message "Must provide a name!")
    (if (nil-or-blank value)
        (message "Must provide a value!")
      (let* ((pos (php-find-struct-position 'constant name)))
        (goto-char pos)
        (let ((doc-args (if doc-args-p
                            (apply 'php-doc-constant-insert doc-args)
                          (call-interactively 'php-doc-constant-insert))))
          (php-insert-constant-definition name value doc-args))))))

(defun php-insert-constant-definition (name value doc-args)
  "Inserts a properly formatted constant definition for NAME into
the buffer with the supplied parameters."
  (let ((pos (point))
        (str ""))
    (setf str (concat str "const " (upcase name)))
    (let* ((value-str (if (and (string= "string" (second doc-args))
                               (not (php-is-a-constant-p value)))
                          (php-single-quote-string value)
                        value))
           (value-str (if (stringp value-str) 
                          value-str 
                        (number-to-string value-str))))
      (setf str (concat str " = " value-str ";"))
      (insert str)
      (php-format-region pos (point) (looking-at-p "}"))
      (php-format-spacing))))

(defun* php-get-insert-property-arguments (&optional name scope value &key
                                                     (staticp nil staticp-p))
  "Read arguments necessary for a property definition."
  `(,(or name 
         (read-string 
          "Property name (without leading characters such as $ or _): "))
    ,(or scope (completing-read "Public, protected, or private (U/o/i)? "
                                '("u" "o" "i") nil t nil nil "u"))
    ,(or value (read-string "Value: "))
    ,(if staticp-p
         staticp
       (y-or-n-p "Make static? "))))

(defun* php-insert-property (name &optional scope value staticp
                                  &key (doc-args nil doc-args-p))
  "Inserts a property named NAME into the buffer.  SCOPE is
whether the method should be public (u, default), protected (o),
or private (i).  The symbols 'public, 'private and 'protected are
also accepted.  The default VALUE may be supplied.  The property
may also be STATICP."
  (interactive (php-get-insert-property-arguments))
  (if (nil-or-blank name)
      (message "Must provide a name!")
    (let* ((scope (if (stringp scope) (php-initial->scope scope) scope))
           (name (concat (if (or (eq scope 'private)
                                 (and php+-mode-protected-underscore
                                      (eq scope 'protected))) "_" "") name))
           (pos (php-find-struct-position 'property name scope)))
      (goto-char pos)
      (when (looking-at-p "[[:space:]]*}")
        (newline-and-indent)
        (save-excursion
          (newline-and-indent)))
      (indent-according-to-mode)
      (let ((doc-args (if doc-args-p
                          (apply 'php-doc-property-insert doc-args)
                        (call-interactively 'php-doc-property-insert))))
        (php-insert-property-definition name scope value staticp doc-args)))))

(defun php-insert-property-definition (name scope value staticp doc-args)
  "Insert a properly formatted property definition for NAME into
the buffer with the supplied parameters."
  (let ((str ""))
    (when staticp
      (setf str (concat str "static ")))
    (setf str (concat str (symbol-name scope) " $" name))
    (unless (nil-or-blank value)
      (let* ((value-str (if (and (string= "string" (second doc-args))
                                 (not (php-is-a-constant-p value)))
                            (php-single-quote-string value)
                          value))
             (value-str (if (stringp value-str) 
                            value-str 
                          (number-to-string value-str))))
        (setf str (concat str " = " value-str))))
    (setf str (concat str ";"))
    (let ((pos (point)))
      (insert str)
      (php-format-region pos (point) (looking-at-p "}"))
      (php-format-spacing t))))

(defun php-initial->scope (i)
  (let ((scope-assoc '(("u" . public)
                       ("o" . protected)
                       ("i" . private))))
    (rest (assoc i scope-assoc))))

(defun* php-get-insert-method-arguments (&optional name scope &key 
                                                   (staticp nil staticp-p)
                                                   (abstractp nil abstractp-p)
                                                   (finalp nil finalp-p)
                                                   (non-class nil non-class-p))
  "Read arguments necessary for a method definition."
  (let ((non-class (if non-class-p 
                       non-class 
                     (not (php-parse-current '(class interface))))))
    `(,(or name (read-string "Method name: "))
      ,(unless non-class
         (or scope (completing-read "Public, protected, or private (U/o/i)? "
                                    '("u" "o" "i") nil t nil nil "u")))
      ,(unless non-class (if staticp-p staticp (y-or-n-p "Make static? ")))
      ,(unless non-class (if abstractp-p 
                             abstractp (y-or-n-p "Make abstract? ")))
      ,(unless non-class (if finalp-p finalp (y-or-n-p "Make final? ")))
      ,non-class)))

(defun* php-insert-method (name &optional scope staticp abstractp finalp 
                                non-class &key (var-list nil var-list-p) 
                                (doc-args nil doc-args-p))
  "Inserts a method element named NAME in the buffer. SCOPE is
whether the method should be public (u, default), protected (o),
or private (i).  The symbols 'public, 'private and 'protected are
also accepted.  The method may also be STATICP, ABSTRACTP or
FINALP unless it is NON-CLASS.  You may pass in VAR-LIST and/or
DOC-ARGS if you do not wish to be prompted for them."
  (interactive (php-get-insert-method-arguments))
  (if (nil-or-blank name)
      (message "Must provide a name!")
    (if (save-match-data
          (or (not (string-match (substring (php-type-regexp 'identifier) 2) 
                                 name))
              (not (= (length name) (- (match-end 0) (match-beginning 0))))))
        (message "Must provide a valid name!")
      (let* ((scope (if (symbolp scope) scope (php-initial->scope scope)))
             (name (concat (if (or (eq scope 'private)
                                   (and php+-mode-protected-underscore
                                        (eq scope 'protected))) "_" "") 
                           name))
             (pos (if (not non-class)
                      (php-find-struct-position 'method name scope staticp 
                                                abstractp finalp)
                    (unless (looking-at-p "[[:space:]]*$")
                      (save-excursion
                        (newline-and-indent)))
                    (indent-according-to-mode)
                    (point))))
        (goto-char pos)
        (unless (looking-back-p "\\(\n[[:space:]]*\\|{\\)\n[[:space:]]*")
          (newline-and-indent))
        (unless (looking-at-p "[[:space:]]*$")
          (save-excursion
            (newline-and-indent)))
        (indent-according-to-mode)
        (let* ((confirm-constants (not var-list-p))
               (var-list (if var-list-p
                             (when doc-args-p
                               (apply 'php-doc-method-insert 
                                      (append doc-args `(:var-list ,var-list))))
                             var-list
                           (if doc-args-p
                               (apply 'php-doc-method-insert doc-args)
                             (call-interactively 'php-doc-method-insert)))))
          (php-insert-method-definition name var-list scope staticp abstractp
                                        finalp confirm-constants non-class)
          (unless abstractp
            (save-excursion
              (newline)
              (insert "}")
              (indent-according-to-mode)
              (newline-and-indent)
              (when (not (or (looking-at-p "}")
                             (looking-at-p (concat ws-re "*$"))))
                (newline-and-indent))))
          (php-format-spacing t)
          (indent-according-to-mode))))))

(defun php-insert-method-definition (name var-list scope staticp abstractp 
                                          finalp &optional confirm-constants 
                                          non-class)
  "Inserts a properly formatted method definition using the
supplied parameters.  You may optionally specify whether to
CONFIRM-CONSTANTS or whether the method is NON-CLASS."
  (beginning-of-line)
  (indent-according-to-mode)
  (let ((insert-string ""))
    (unless non-class
      (dolist (mod '("abstract" "final" "static"))
        (when (symbol-value (intern (concat mod "p")))
          (setf insert-string (concat insert-string mod " "))))
      (setf insert-string 
            (concat insert-string 
                    (when scope (concat (symbol-name scope) " ") ))))
    (setf insert-string (concat insert-string "function " name "("))
    (when var-list
      (dolist (var (php-sort-method-args var-list))
        (let* ((arg-text "")
               (name (rest (assq 'name var)))
               (type (rest (assq 'type var)))
               (type (or type "mixed"))
               (value (rest (assq 'value var)))
               (value (when value (format "%s" value))))
          (when (and type
                     (or (string= type "array")
                         (let ((c (substring type 0 1)))
                           (string= c (upcase c)))))
            (setq arg-text (concat arg-text type " ")))
          (setq arg-text (concat arg-text name))
          (when value
            (setq arg-text 
                  (concat arg-text " = " 
                          (if (or (not (member type '("callback" "string")))
                                  (php-is-a-constant-p value 
                                                       (not confirm-constants))
                                  (string= value "''")
                                  (string= value "\"\""))
                              value
                            (php-single-quote-string value)))))
          (setq arg-text (concat arg-text ", "))
          (setf insert-string (concat insert-string arg-text))))
      (setf insert-string (substring insert-string 0 -2)))
    (setf insert-string (concat insert-string ")"))
    (if abstractp
        (setf insert-string (concat insert-string ";"))
      (setf insert-string (concat insert-string " {")))
    (unless (looking-at-p "[[:space:]]*$")
      (save-excursion
        (newline-and-indent)))
    (insert insert-string)
    (when (looking-back-p "{")
      (backward-char))
    (php-format-break-statement)
    (php-skip-this-statement)
    (when (looking-back-p "{")
      (forward-line 1)
      (indent-according-to-mode))))

(defun php-sort-method-args (var-list)
  "Sorts a copy of VAR-LIST so that variables without default
arguments are placed after those with default arguments."
  (let ((var-list (sort (copy-list var-list) 
                         (lambda (x y)
                           (and (not (rest (assq 'value x)))
                                (rest (assq 'value y)))))))
    var-list))

(defun php-extract-var-list-from-parse (parse)
  "Given a method PHP parse, return a var-list that can be passed
to ``php-doc-method-insert'' or ``php-method-insert''."
  (when (and (php-parse-p parse)
             (eq (rest (assq 'type parse)) 'method))
    (let ((arguments (rest (assq 'arguments parse)))
          (params (rest (assq 'params (rest (assq 'documentation parse)))))
          var-list)
      (dolist (arg arguments var-list)
        (let ((param (first (member-if (lambda (x) 
                                         (string= (rest (assq 'name arg))
                                                  (rest (assq 'name x))))
                                       params)))) 
          (add-to-list 
           'var-list 
           `(,(assq 'name arg)
             (type . ,(or (rest (assq 'type arg)) (rest (assq 'type param))))
             (value . ,(let ((value (rest (assq 'value arg))))
                         (when (stringp value)
                           (when (string= (substring value 0 1) "'")
                             (setf value (substring value 1)))
                           (when (string= (substring value -1) "'")
                             (setf value (substring value 0 -1))))
                         value))
             ,(assq 'desc param)) t))))))
                                      
(defun php-edit-thing (&optional commands thing-name)
  "Slurp up the current
function/method/property/constant/class/interface definition,
apply COMMANDS and then re-insert the new definition.  Commands
are alists of the form '((verb . verb) (type . type) (name
.name) (value . value))).  Verbs currently supported are
'remove (for params only) and 'edit.  Editing a non-existant
entry results in adding a new one.  Types are currently
'short-desc 'long-desc 'author 'return, 'param and 'definition.
Values are single strings except in the case of 'param and
'definition. In the case of 'param, it is an alist '((name
. name) (type . type) (value . value) (desc . desc) (after
. after)).  For 'definition, it is '((name . name) (visibility
. visibility) (staticp . staticp) (abstractp . abstractp) (finalp
. finalp) (extends . extends) (implements . implements)).
Specifiying abstractp or finalp for a constant or property will
be ignored.  Specifying '(after . nil) in the value will result
in the param begin placed first.  Optional params will be pushed
backwards always before generation, which may affect the order.
In the case of the verb 'edit, only supply values that will be
changed.  Returns the change in text size.  Leaves point at the
beginning of thing."
  (when (or (not thing-name) (php-jump-to-thing thing-name))
    (let ((parse (php-parse-current '(constant property method class 
                                               interface))))
      (when (php-parse-p parse)
        (let* ((doc (rest (assq 'documentation parse)))
               (begin (rest (assq 'begin parse)))
               (parse-type (rest (assq 'type parse)))
               (parse-type 
                (if (and (eq parse-type 'method)
                         (not (php-parse-current '(class interface))))
                    'function
                  parse-type))
               (def-end (save-excursion (php-jump-to-first-statement parse)))
               (def-end (if (or (memq parse-type '(constant property))
                                (rest (assq 'abstractp parse))
                                (= def-end begin))
                            (rest (assq 'end parse))
                          def-end))
               (var-list (when (memq parse-type '(function method))
                           (php-extract-var-list-from-parse parse))))
          (dolist (command commands)
            (let* ((verb (rest (assq 'verb command)))
                   (type (rest (assq 'type command)))
                   (name (rest (assq 'name command)))
                   (value (rest (assq 'value command)))
                   (name (if (and (not name) (listp value)) 
                             (rest (assq 'name value))
                           name))
                   thing-order
                   (thing (cond ((eq type 'param)
                                 (let ((place (php-var-list-tail
                                               name var-list)))
                                   (setf thing-order
                                         (- (length var-list) (length place)))
                                   (first place)))
                                ((eq type 'definition) parse)
                                (t (assq type doc))))
                   (list-to-edit (cond ((eq type 'param) 'var-list)
                                       ((eq type 'definition) 'parse)
                                       (t 'doc)))
                   (list-value (symbol-value list-to-edit)))
              (cond ((not (eq type 'definition))
                     (setf list-value (delete thing list-value))))
              (cond ((eq verb 'edit)
                     (cond ((or (eq type 'param) (eq type 'definition))
                            (when (assq 'after value)
                              (let ((after (rest (assq 'after value))))
                                (setf thing-order 
                                      (if after
                                          (- (1+ (length list-value))
                                             (length (php-var-list-tail 
                                                      after 
                                                      list-value)))
                                        0))))
                            (dolist (field (cond
                                            ((eq type 'param)
                                             '(name type value desc))
                                            ((eq type 'definition)
                                             (cond ((eq parse-type 'class)
                                                    '(name extends implements
                                                           abstractp finalp))
                                                   ((eq parse-type 'interface)
                                                    '(name extends))
                                                   ((eq parse-type 'function)
                                                    '(name))
                                                   ((eq parse-type 'method)
                                                    '(name visibility staticp 
                                                           abstractp finalp))
                                                   ((eq parse-type 'constant)
                                                    '(name value))
                                                   ((eq parse-type 'property)
                                                    '(name value visibility 
                                                           staticp))))))
                              (when (assq field value)
                                (let* ((new-val (rest (assq field value))))
                                  (when (eq field 'name)
                                    (php-edit-thing-handle-name))
                                  (setf thing (assq-delete-all field thing))
                                  (setf thing 
                                        (append thing 
                                                `((,field . ,new-val))))))))
                           (t (setf thing `(,type . ,value))))
                     (cond ((eq type 'param)
                            (when (and (assoc 'name thing)
                                       (assoc 'type thing))
                              (setf list-value
                                    (append (butlast list-value 
                                                     (- (length list-value)
                                                        thing-order))
                                            `(,thing)
                                            (nthcdr thing-order 
                                                    list-value)))))
                           ((eq type 'definition)
                            (setf list-value thing))
                           (t (setf list-value (append list-value 
                                                       `(,thing)))))))
              (setf (symbol-value list-to-edit) list-value)))
          (goto-char begin)
          (delete-region begin def-end)
          (cond ((eq parse-type 'class)
                 (php-doc-class-insert (rest (assq 'short-desc doc)) 
                                       (rest (assq 'long-desc doc)) 
                                       (rest (assq 'category doc))
                                       (rest (assq 'package doc))
                                       (rest (assq 'subpackage doc))
                                       (rest (assq 'author doc)) 
                                       (rest (assq 'license doc))
                                       (rest (assq 'link doc)))
                 (php-insert-class-definition (rest (assq 'name parse))
                                              (rest (assq 'extends parse))
                                              (rest (assq 'implements parse))
                                              (rest (assq 'finalp parse))
                                              (rest (assq 'abstractp parse))))
                ((memq parse-type '(function method))
                 (php-doc-method-insert (rest (assq 'short-desc doc)) 
                                        (rest (assq 'long-desc doc)) 
                                        (rest (assq 'author doc)) 
                                        (rest (assq 'return doc)) 
                                        :var-list var-list)
                 (php-insert-method-definition (rest (assq 'name parse)) 
                                               var-list
                                               (rest (assq 'visibility parse))
                                               (rest (assq 'staticp parse))
                                               (rest (assq 'abstractp parse))
                                               (rest (assq 'finalp parse))
                                               (eq parse-type 'function)))
                ((memq parse-type '(constant property))
                 (let ((doc-args `(,(rest (assq 'short-desc doc))
                                   ,(rest (assq 'var doc))))
                       (name (rest (assq 'name parse))))
                   (apply 'php-doc-property-insert doc-args)
                   (cond ((eq parse-type 'constant)
                          (php-insert-constant-definition 
                           name (rest (assq 'value parse)) doc-args))
                         ((eq parse-type 'property)
                          (php-insert-property-definition 
                           name
                           (rest (assq 'visibility parse))
                           (rest (assq 'value parse)) 
                           (rest (assq 'staticp parse))
                           doc-args))))))
          (let* ((parse-type (if (eq parse-type 'function) 'method parse-type))
                 (parse (php-parse-current parse-type))
                 (begin (rest (assq 'begin parse)))
                 (new-def-end (save-excursion (php-jump-to-first-statement)))
                 (new-def-end (if (or (memq parse-type '(constant property))
                                      (rest (assq 'abstractp parse))
                                      (= def-end begin))
                                  (rest (assq 'end parse))
                                new-def-end)))
            (when (wholenump begin) (goto-char begin))
            (- new-def-end def-end)))))))

(defun php-edit-thing-handle-name ()
  (cond ((eq type 'param)
         (let ((ref (string-match-p "&" new-val)))
           (setf new-val 
                 (concat (when ref "&") "$"
                         (replace-regexp-in-string
                          "[$&]" "" new-val)))))
        ((eq type 'definition)
         (when (memq parse-type '(property method))
           (let* ((vis (rest (assq 'visibility value)))
                  (under (or (eq vis 'private)
                             (and (eq vis 'protected)
                                  php+-mode-protected-underscore))))
             (setf new-val 
                   (concat (when under "_") 
                           (replace-regexp-in-string "^_\\([^_]\\)" "\\1" 
                                                     new-val))))))))

(defun php-var-list-tail (name var-list)
  "Return the tail of VAR-LIST beginning with the var NAME."
  (when (and (stringp name)
             (listp var-list))
    (let ((name (replace-regexp-in-string "[$&]" "" name)))
      (member-if (lambda (x)
                   (let* ((x-name (replace-regexp-in-string 
                                   "[$&]" "" (rest (assq 'name x)))))
                     (string= x-name name))) var-list))))
  
(defun php-create-new-file (filename directory &optional keep-empty)
  "Creates a new php file named 'filename' in the given
directory."
  (let ((file (convert-standard-filename (concat directory filename))))
    (if (file-exists-p file)
        (progn
          (message "File already exists!")
          nil)
      (make-directory directory t)
      (find-file file)
      (unless keep-empty
        (insert "<?php")
        (newline)
        (call-interactively 'php-doc-file-insert)
        (newline))
      t)))

(defun php-tab-key (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and (or (bobp) (= ?w (char-syntax (char-before))))
           (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

(defun php-is-a-constant-p (value &optional do-not-ask)
  "This function tries to determine if the value is the name of a
constant.  Currently, it looks for files that match the php regex
for constants, with the exception that it will only match if the
value is all upper case.  You may also specify DO-NOT-ASK."
  (when (and
         (or (member value '("true" "false" "null"))
             (string= (upcase value) value))
         (string-match "\\<[A-Z_\x7f-\xff][A-Z0-9_\x7f-\xff]*\\>" value)
         (= (- (match-end 0) (match-beginning 0)) (length value)))
    (or do-not-ask 
        (y-or-n-p (concat "It looks like " value 
                          " is a constant. Is this so? ")))))

(defun php-parse-named-thing (name)
  "Jump to thing `name` and return its parse."
  (save-excursion
    (when (php-jump-to-thing name)
      (php-parse-current))))

(defun* php-get-parsed-thing-part (part &optional thing-name &key argument)
  "Returns the requested part of the parsed thing."
  (unless thing-name (setq thing-name (php-get-current-thing-name)))
  (let ((parsed-thing (php-parse-named-thing thing-name)))
    (cond
     (argument
      (let ((arg-parts '(name value))
            (doc-parts '(type desc)))
        (cond 
         ((member part arg-parts)
          (rest (assoc part
                       (car (member-if
                             (lambda (x)
                               (string= (replace-regexp-in-string
                                         "[$&]" ""
                                         (rest (assoc 'name x))) argument)) 
                             (rest (assoc 'arguments parsed-thing)))))))
         ((member part doc-parts)
          (rest (assoc part
                       (car (member-if
                             (lambda (x)
                               (string= (replace-regexp-in-string
                                         "[$&]" ""
                                         (rest (assoc 'name x))) argument)) 
                             (rest (assoc 'params 
                                          (rest (assoc 'documentation 
                                                       parsed-thing))))))))))))
     (t
      (let ((thing-parts '(name visibility extends implements abstractp finalp 
                                staticp value))
            (doc-parts '(short-desc long-desc author return var)))
        (cond 
         ((member part thing-parts)
          (rest (assoc part parsed-thing)))
         ((member part doc-parts)
          (rest (assoc part (rest (assoc 'documentation parsed-thing)))))))))))

(defun php-get-current-thing-name (&optional type)
  "This function returns the name for the thing under point.
Optionally specify TYPE according to the rules of
``php-parse-current''."
  (rest (assoc 'name (php-parse-current type))))

(defun php-modify-thing (arg)
  "This function facilitates the interactive modification of
constants, properties and methods."
  (interactive "P")
  (let ((thing (completing-read "Thing to modify: " (php-get-thing-names) 
                                nil t (php-get-current-thing-name))))
    (php-jump-to-thing thing)
    (let* ((parse (php-parse-named-thing thing))
           (thing-type (rest (assq 'type parse)))
           (thing-type (if (and (eq thing-type 'method)
                                (not (php-parse-current '(class interface))))
                           'function
                         thing-type)))
      (unless arg
        (let* ((name (read-string (concat "New " (symbol-name thing-type) 
                                          " name: ") thing))
               (visibility
                (when (memq thing-type '(property method))
                  (intern
                   (completing-read "New visibility: "
                                    '("public" "protected" "private")
                                    nil t
                                    (symbol-name
                                     (or (php-get-parsed-thing-part
                                          'visibility thing)
                                         'public))))))
               (short-desc
                (read-string "Short description: "
                             (php-get-parsed-thing-part 'short-desc thing)))
               (long-desc
                (when (memq thing-type '(function method class interface))
                  (read-string "Long description: "
                               (php-get-parsed-thing-part 'long-desc thing))))
               (author
                (when (memq thing-type '(function method class interface))
                  (read-string "Author: "
                               (php-get-parsed-thing-part 'author thing))))
               (return
                (when (memq thing-type '(function method))
                  (completing-read "Return type: "
                                   (php-completion-get-type-list t) nil nil
                                   (or (php-get-parsed-thing-part 'return thing)
                                       "void"))))
               (var
                (when (memq thing-type '(constant property))
                  (completing-read "Type: " (php-completion-get-type-list)
                                   nil nil
                                   (php-get-parsed-thing-part 'var thing))))
               (value (when (memq thing-type '(constant property))
                        (read-string "Value: "
                                     (let ((raw-val 
                                            (php-get-parsed-thing-part 'value 
                                                                       thing)))
                                       (if (string= var "string")
                                           (replace-regexp-in-string
                                            "^['\"]" ""
                                            (replace-regexp-in-string
                                             "['\"]$" "" raw-val))
                                         raw-val)))))
               (class-completion-list (append (php-completion-get-etags)
                                              (php-completion-candidates 
                                               "" '("class"))))
               (extends 
                (let ((default (php-get-parsed-thing-part 'extends thing)))
                  (cond ((eq thing-type 'class)
                         (completing-read "Extends: " class-completion-list nil 
                                          nil default))
                        ((eq thing-type 'interface)
                         (php-insert-get-interfaces :verb "extends" 
                                                    :default default)))))
               (implements 
                (let ((default (php-get-parsed-thing-part 'implements thing)))
                  (when (eq thing-type 'class)
                    (php-insert-get-interfaces :default default))))
               (staticp-str 
                (when (memq thing-type '(property method))
                  (if (php-get-parsed-thing-part 'staticp thing) 
                      "y" "n")))
               (staticp 
                (when staticp-str 
                  (completing-read (concat "Static (" staticp-str ")? ") 
                                   '("y" "Y" "n" "N") nil t nil 
                                   nil staticp-str)))
               (staticp (or (string= staticp "y") (string= staticp "Y")))
               (abstractp-str 
                (when (memq thing-type '(method class))
                  (if (php-get-parsed-thing-part 'abstractp thing) "y" "n")))
               (abstractp (when abstractp-str
                            (completing-read (concat "Abstract (" abstractp-str 
                                                     ")? ") 
                                             '("y" "Y" "n" "N") nil t nil 
                                             nil abstractp-str)))
               (abstractp (or (string= abstractp "y") (string= abstractp "Y")))
               (finalp-str 
                (when (memq thing-type '(method class))
                  (if (php-get-parsed-thing-part 'finalp thing) "y" "n")))
               (finalp (when finalp-str
                         (completing-read (concat "Final (" finalp-str ")? ") 
                                          '("y" "Y" "n" "N") nil t nil 
                                          nil finalp-str)))
               (finalp (or (string= finalp "y") (string= finalp "Y")))
               (edit-list 
                (append 
                 `(((verb . edit) (type . short-desc) (value . ,short-desc)))
                 (when (memq thing-type '(function method class interface))
                   `(((verb . edit) (type . long-desc) (value . ,long-desc))
                     ((verb . edit) (type . author) (value . ,author))))
                 (when (memq thing-type '(function method))
                   `(((verb . edit) (type . return) (value . ,return))))
                 (when (memq thing-type '(constant property))
                   `(((verb . edit) (type . var) (value . ,var))))
                 `(((verb . edit) (type . definition) (name . ,thing)
                    (value . ,(append 
                               `((name . ,name))
                               (when (memq thing-type '(class interface))
                                 `((extends . ,extends)))
                               (when (eq thing-type 'class)
                                 `((implements . ,implements)))
                               (when (memq thing-type 
                                           '(constant property method))
                                 `((visibility . ,visibility)))
                               (when (memq thing-type '(property method))
                                 `((staticp . ,staticp)))
                               (when (memq thing-type '(constant property))
                                 `((value . ,value)))
                               (when (memq thing-type '(method class))
                                 `((abstractp . ,abstractp)
                                   (finalp . ,finalp))))))))))
          (php-edit-thing edit-list)
          (when (memq thing-type '(constant property method))
            (php-rearrange-current))
          (php-jump-to-first-statement)
          (when (and (member thing-type 
                             '(function method property constant class 
                                        interface))
                     (not (string= name thing))
                     (y-or-n-p (format "Convert calls of %s to %s? " thing 
                                       name)) )
            (php-project-query-replace-regexp thing name))))
      (when (memq thing-type '(function method))
        (let ((name (php-get-current-thing-name)))
          (while (or arg (y-or-n-p "Modify arguments? "))
            (setf arg nil)
            (php-modify-method-argument name)))))))
  
(defun php-modify-method-argument (method)
  "This function facilitates the interactive modification of method arguments."
  (php-jump-to-thing method)
  (let* ((arguments (php-get-method-arg-names nil method))
         argument)
    (while (not (and argument
                     (or (member argument arguments)
                         (string-match-p 
                          (substring (php-type-regexp 'identifier) 
                                     2) argument))))
      (setf argument (completing-read "Argument name: " arguments nil 
                                      'confirm-after-completion
                                      (when (= (length arguments) 1) 
                                        (first arguments)))))
    (let* ((verb (if (member argument arguments)
                     (if (y-or-n-p (concat "Delete " argument "? "))
                         'delete
                       'edit)
                   'edit)))
      (if (eq verb 'delete)
          (php-edit-thing
           `(((verb . ,verb) (type . param) (name . ,argument))))
        (let ((name (if (member argument arguments)
                        (let ((argument 
                               (replace-regexp-in-string 
                                "\\$" ""
                                (find-if (lambda (x)
                                           (string= argument 
                                                    (replace-regexp-in-string 
                                                     "[$&]" "" x)))
                                         (php-get-method-arg-names t method)))))
                          (read-string "New argument name: " argument))
                      argument))
              (type (completing-read "Argument type: "
                                     (php-completion-get-type-list)
                                     nil nil
                                     (php-get-parsed-thing-part
                                      'type method :argument argument)))
              (value (read-string "Default value: "
                                  (php-get-parsed-thing-part
                                   'value method :argument argument)))
              (desc (read-string "Argument description: "
                                 (php-get-parsed-thing-part
                                  'desc method :argument argument)))
              (after (when (> (length (remove argument arguments)) 0)
                       (completing-read "Reposition after: "
                                        (append '("*first*")
                                                (php-get-method-arg-names nil))
                                        nil t))))
          (if (string= after "")
              (setq after nil)
            (if (string= after "*first*")
                (setq after '(after . nil))
              (setq after `(after . ,after))))
          (when (string= value "")
            (setq value nil))
          (setq value `(value . ,value))
          (php-edit-thing
           `(((verb . ,verb) (type . param) (name . ,argument)
              (value . ((name . ,name) (type . ,type) ,value (desc . ,desc)
                        ,after)))))
          (let ((replace-argument (replace-regexp-in-string "[$&]" "" argument))
                (replace-name (replace-regexp-in-string "[$&]" "" name)))
            (unless (string= replace-argument replace-name)
              (save-excursion
                (let ((parse (php-parse-current 'method)))
                  (when (and (php-parse-p parse)
                             (php-jump-to-first-statement))
                    (query-replace (concat "$" replace-argument) 
                                   (concat "$" replace-name) nil (point) 
                                   (rest (assoc 'end parse)))))))))))))

(defun php-modify-class/interface ()
  "This function modifies a php class or interface, renaming it and asking to
change calls to the class name throughout the project."
  (interactive)
  (let* ((class (php-get-class-name))
         (extension (php-get-class-extends))
         (name (upcase-initials
                (read-string (concat "New name for " class ": "))))
         (extends (upcase-initials
                   (completing-read "Extends: "
                                    (php-completion-get-type-list)
                                    nil nil
                                    extension)))
         (declaration (concat "class " name
                              (unless (string= "" (replace-regexp-in-string
                                                   ws-re "" extends))
                                (concat " extends " extends))
                              " {")))
    (php-jump-to-declaration 'class)
    (zap-to-char 1 (string-to-char "{"))
    (insert declaration)
    (php-format-break-statement)
    (let ((php+-mode-php-compile-on-save nil))
      (write-file (concat name ".php")))
    (when (y-or-n-p (concat "Convert calls to " class "?"))
      (php-project-query-replace-regexp class name))))

(defun php-get-class-info ()
  "This function returns the information for the first class in a buffer."
  (php-jump-to-class-name)
  (let ((begin (point))
        class
        extends)
    (re-search-forward "[ \n]")
    (backward-char)
    (setq class (buffer-substring-no-properties begin (point)))
    
    (php-jump-to-class-extends)
    (setq begin (point))
    (end-of-line)
    (setq extends (buffer-substring-no-properties begin (point)))
    `(,class . ,extends)))

(defun php-get-class-name ()
  "This function return the first class name."
  (car (php-get-class-info)))

(defun php-get-class-extends ()
  "This function returns the first class extends"
  (cdr (php-get-class-info)))

(defun php-jump-to-class-name ()
  "This function jumps to the first class name in a file."
  (if (and (php-jump-to-first-class/interface)
           (php-jump-to-declaration 'class))
      (progn
        (forward-word)
        (forward-char))
    nil))

(defun php-jump-to-class-extends ()
  "This function jumps to the first class extends in a file."
  (if (and (php-jump-to-first-class/interface)
           (php-jump-to-declaration 'class))
      (if (search-forward "extends" nil t)
          (forward-char)
        nil)
    nil))

(provide 'php-funcs)
