;;; php-parse.el --- Functions for parsing the structure of a PHP file.

;; Version: 1.0
;; Created: 2011-07-17
;; Copyright © 2011 Michael Dwyer
;; Author(s):
;; Michael Dwyer <mdwyer@ehtech.in>

;;; *****
;;; About
;;; *****

;; php-parse.el was created as part of the php+-mode suite and contains
;; a parsing library directed towards (but not necessarily limited to)
;; PHP files.  What started as fixing a bug in
;; php-find-method-position has led to the creation of a regex-based
;; PHP structure parser that could be extended to other languages in
;; the future.

;; The next goal will be to use the function provided to enable
;; context-sensitive code completion and code cleanup.

;; ********************************************************************

;; ************
;; REQUIREMENTS
;; ************
(require 'cl)
(require 'string-utils)

;; *********
;; CUSTOMIZE
;; *********

(defgroup php-parse nil
  "Customizations for php-parse."
  :group 'php)

(defcustom php-parse-send-to-front '()
  "List of structure names that are automatically sorted first."
  :group 'php
  :group 'php-parse
  :type '(alist :key-type (string :tag "Structure Name")
                :value-type
                (list
                 (set :tag "Flags" 
                      (const abstract)
                      (const final)
                      (const static))
                 (radio :tag "Type"
                        (const property)
                        (const method))
                 (radio :tag "Visibility"
                        (const public)
                        (const protected)
                        (const private)))))

(defcustom php-parse-send-to-back '()
  "List of structure names that are automatically sorted last."
  :group 'php
  :group 'php-parse
  :type '(alist :key-type (string :tag "Structure Name")
                :value-type
                (list
                 (set :tag "Flags" 
                      (const abstract)
                      (const final)
                      (const static))
                 (radio :tag "Type"
                        (const property)
                        (const method))
                 (radio :tag "Visibility"
                        (const public)
                        (const protected)
                        (const private)))))

;; *********
;; CONSTANTS
;; *********

(defconst php-type-regexps
  '((file . "\\`")
    (script . "<\\?\\(php\\|\\)?[[:space:]
]*\\(/\\*\\([^*]\\|\\*[^/]\\)*?\\*/\\)?")
    (comment . "\\(/\\*\\([^*]\\|\\*[^/]\\)*?\\*/\\|\\(?://\\|#\\).*$\\)")
    (interface . "\\(/\\*\\([^*]\\|\\*[^/]\\)*?\\*/\\)?[[:space:]
]*\\interface[[:space:]
]+\\([[:word:]_]+\\)\\([[:space:]
]+extends[[:space:]
]+\\(\\(\\([[:word:]_]+\\)\\(,[[:space:]
]*\\)?\\)+\\)\\)?[[:space:]
]*") 
    (class . "\\(/\\*\\([^*]\\|\\*[^/]\\)*?\\*/\\)?[[:space:]
]*\\(\\(abstract\\|final\\)[[:space:]
]+\\)?class[[:space:]
]+\\([[:word:]_]+\\)\\([[:space:]
]+extends[[:space:]
]+\\(\\(\\([[:word:]_]+\\)\\(,[[:space:]
]*\\)?\\)+\\)\\)?\\([[:space:]
]+implements[[:space:]
]+\\(\\(\\([[:word:]_]+\\)\\(,[[:space:]
]*\\)?\\)+\\)\\)?[[:space:]
]*") 
    (method . "\\(\\(?:/\\*\\(?:[^*]\\|\\*[^/]\\)*?\\*/\\)[[:space:]
]*?\\)?\\(\\(\\(?4:abstract\\|final\\)\\|\\(?6:static\\)\\|\\(?8:public\\|protected\\|private\\)\\)[[:space:]\n]+\\)\\{,3\\}function[[:space:]
]+\\([[:word:]_]+\\)([[:space:]
]*\\(\\(\\([[:word:]_]+[[:space:]
]*\\)?&?\\$[[:word:]_]+\\([[:space:]
]*=[[:space:]
]*\\(?:array()\\|[^,]+?\\)\\)?\\(,[[:space:]
]*\\)?\\)*\\)[[:space:]
]*)[[:space:]
]*;?") 
    (property . "\\(/\\*\\(?:[^*]\\|\\*[^/]\\)*?\\*/[[:space:]
]*\\)?\\(\\(\\(?4:static\\)\\|\\(?6:public\\|protected\\|private\\)\\)[[:space:]\n]+\\)\\{1,2\\}\\$\\([[:word:]_]+\\)\\([[:space:]
]*=[[:space:]
]*\\([^;]+?\\)\\)?[[:space:]
]*;")
    (constant . "\\(/\\*\\([^*]\\|\\*[^/]\\)*?\\*/[[:space:]
]*\\)?const[[:space:]
]+\\([[:word:]_]+\\)\\([[:space:]
]*=[[:space:]
]*\\([^;]+?\\)\\)?[[:space:]
]*;")
    (number . "[+-]?\\(\\([1-9][0-9]*\\|0\\|0?\\.[0-9]+\\)\\([eE][-+]?[0-9]+\\)?\\)\\|\\(0[xX][0-9a-fA-F]+\\)\\|\\(0[0-7]+\\)")
    (identifier . "$?[a-zA-Z_-ÿ][a-zA-Z0-9_-ÿ]*")
    (accessor . "\\(->\\|::\\)")
    (assignment-operator . "[^-.+*/%&|^<>!=]\\(?1:=\\)[^=>]\\|\\.=\\|\\+=\\|-=\\|\\*=\\|/=\\|%=\\|&=\\||=\\|\\^=\\|<<=\\|>>=\\|=>")
    (operator . "\\(?1:\\.\\)[^=]\\|[^+]\\(?1:\\+\\)[^+=]\\|[^-]\\(?1:-\\)[^->=]\\|\\(?1:\\*\\)[^=]\\|\\(?1:/\\)[^=]\\|\\(?1:%\\)[^=]\\|&&\\|||\\|[^&]\\(?1:&\\)[^&=]\\|[^|]\\(?1:|\\)[^|=]\\|\\(?1:\\^\\)[^=]\\|[[:space:]\n]+\\(?1:and\\)[[:space:]\n]+\\|[[:space:]\n]+\\(?1:or\\)[[:space:]\n]+\\|[[:space:]\n]+\\(?1:xor\\)[[:space:]\n]+\\|[[:space:]\n]+\\(?1:as\\)[[:space:]\n]+\\|[[:space:]\n]+\\(?1:instanceof\\)[[:space:]\n]+\\|[[:space:]\n]+\\(?1:extends\\)[[:space:]\n]+\\|[[:space:]\n]+\\(?1:implements\\)[[:space:]\n]+\\|[^!=]\\(?1:==\\)[^=]\\|\\(?1:!=\\)[^=]\\|===\\|!==\\|>>\\|<<\\|[^<]\\(?1:<\\)[^<=]\\|<=\\|[^-=>]\\(?1:>\\)[^>=]\\|>=\\|\\(?1:\\?\\)[^:]\\|[^?:]\\(?1::\\)[^:]\\|\\?:"))
  "This is an alist of PHP type recognition regexps.  Do not
  remove the carriage returns in these regexps unless you replace
  them with \\n!")

(defconst doc-begin-tag-re 
  "<<<\\([\"']?\\)\\([a-zA-Z_-ÿ][a-zA-Z0-9_-ÿ]*\\)\\(\\1\\)\n"
  "A regular expression that matches the beginning tag of a
  {here,now}doc.")

(defconst doc-end-tag-re 
  "^\\([a-zA-Z_-ÿ][a-zA-Z0-9_-ÿ]*\\)\\([;\n]\\)"
  "A regular expression that matches the end tag of a
  {here,now}doc.")

;; *********
;; FUNCTIONS
;; *********

(defun php-type-regexp (type)
  "Return the TYPE PHP struct regexp."
  (rest (assoc type php-type-regexps)))

(defun php-parse-pluralize-type (type)
  "Quick pluralization of structure type."
  (cond
   ((eq type 'operator) 'operators)
   ((eq type 'assignment-operator) 'assignment-operators)
   ((eq type 'accessor) 'accessors)
   ((eq type 'identifier) 'identifiers)
   ((eq type 'number) 'numbers)
   ((eq type 'constant) 'constants)
   ((eq type 'property) 'properties)
   ((eq type 'method) 'methods)
   ((eq type 'class) 'classes)
   ((eq type 'interface) 'interfaces)
   ((eq type 'script) 'scripts)
   ((eq type 'comment) 'comments)
   ((eq type 'file) 'files)))

(defun php-parse-type-file ()
  "Parses the currently matched PHP file structure."
  `((type . file)
    (name . ,(buffer-file-name))
    (scripts . ,(php-scan-current 'file 'script begin end))))

(defun php-parse-type-script ()
  "Parses the currently matched PHP script structure."
  `((type . script)
    (interfaces . ,(php-scan-current 'script 'interface begin end))
    (classes . ,(php-scan-current 'script 'class begin end))
    (methods . ,(php-scan-current 'script 'method begin end))
    (constants . ,(php-scan-current 'script 'constant begin end))
    (properties . ,(php-scan-current 'script 'property begin end))
    (comments . ,(php-scan-current 'script 'comment begin end))
    (documentation-text . ,(match-string-no-properties 2))
    (documentation . ,(php-parse-doc-block (match-string-no-properties 2)))))

(defun php-parse-type-comment ()
  "Parses the currently matched PHP comment structure."
  `((type . comment)))

(defun php-parse-type-interface ()
  "Parses the currently matched PHP interface structure."
  `((type . interface)
    (name . ,(match-string-no-properties 3))
    (extends . ,(when (match-string-no-properties 5)
                  (mapcar 'chomp 
                          (safe-split-string 
                           (match-string-no-properties 5)
                           ","))))
    (constants . ,(php-scan-current 'interface 'constant begin end))
    (methods . ,(php-scan-current 'interface 'method begin end))
    (documentation-text . ,(match-string-no-properties 1))
    (documentation . ,(php-parse-doc-block (match-string-no-properties 1)))))

(defun php-parse-type-class ()
  "Parses the currently matched PHP class structure."
  `((type . class)
    (name . ,(match-string-no-properties 5))
    (abstractp . ,(string= "abstract" (match-string-no-properties 4)))
    (finalp . ,(string= "final" (match-string-no-properties 4)))
    (extends . ,(match-string-no-properties 7))
    (implements . ,(when (match-string-no-properties 12)
                     (mapcar 'chomp 
                             (safe-split-string 
                              (match-string-no-properties 12)
                              ","))))
    (constants . ,(php-scan-current 'class 'constant begin end))
    (properties . ,(php-scan-current 'class 'property begin end))
    (methods . ,(php-scan-current 'class 'method begin end))
    (documentation-text . ,(match-string-no-properties 1))
    (documentation . ,(php-parse-doc-block (match-string-no-properties 1)))))

(defun php-parse-type-method ()
  "Parses the currently matched PHP method structure."
  `((type . method)
    (name . ,(match-string-no-properties 9))
    (abstractp . ,(string= "abstract" (match-string-no-properties 4)))
    (finalp . ,(string= "final" (match-string-no-properties 4)))
    (staticp . ,(string= "static" (match-string-no-properties 6)))
    (visibility . ,(when (match-string-no-properties 8)
                       (intern (match-string-no-properties 8))))
    (arguments 
     . ,(let ((arg-list (match-string-no-properties 10)))
          (when arg-list
            (if (string= arg-list "") 
                nil
              (mapcar (lambda (item)
                       (let* ((parts (safe-split-string item "="))
                              (type-name (chomp (first parts)))
                              (value (chomp (cadr parts)))
                              (type-name-parts (safe-split-string type-name))
                              (type (when (> (length type-name-parts) 1) 
                                      (first type-name-parts)))
                              (name (if type 
                                        (cadr type-name-parts) 
                                      (first type-name-parts))))
                         `((name . ,name)(type . ,type)(value . ,value))))
                      (mapcar 'chomp 
                              (safe-split-string 
                               (match-string-no-properties 10) ",")))))))
    (statements . ,(unless 
                       (string= ";" 
                                (substring (match-string-no-properties 0) -1))
                     (chomp (buffer-substring-no-properties (match-end 0) 
                                                            (1- end)))))
    (documentation-text . ,(match-string-no-properties 1))
    (documentation . ,(php-parse-doc-block (match-string-no-properties 1)))))

(defun php-parse-type-property ()
  "Parses the currently matched PHP property structure."
  `((type . property)
    (name . ,(match-string-no-properties 7))
    (staticp . ,(string= "static" (match-string-no-properties 4)))
    (visibility . ,(let ((v (match-string-no-properties 6)))
                   (if (or (not (stringp v)) (string= "var" v)) 
                       'public 
                     (intern v))))
    (value . ,(match-string-no-properties 9))
    (documentation-text . ,(match-string-no-properties 1))
    (documentation . ,(php-parse-doc-block (match-string-no-properties 1)))))

(defun php-parse-type-constant ()
  "Parses the currently matched PHP constant structure."
  `((type . constant)
    (name . ,(match-string-no-properties 3))
    (value . ,(match-string-no-properties 5))
    (documentation-text . ,(match-string-no-properties 1))
    (documentation . ,(php-parse-doc-block (match-string-no-properties 1)))))

(defun php-parse-type-number ()
  "Parses the currently matched PHP number."
  `((type . number)))

(defun php-parse-type-identifier ()
  "Parses the currently matched PHP identifier."
  `((type . identifier)))

(defun php-parse-type-accessor ()
  "Parses the currently matched PHP accessor."
  `((type . accessor)))

(defun php-parse-type-assignment-operator ()
  "Parses the currently matched PHP assignment operator."
  `((type . assignment-operator)))

(defun php-parse-type-operator ()
  "Parses the currently matched PHP {arithmetic,logical,boolean}
operator."
  `((type . operator)))

(defun php-parse-type (type)
  "Parse the currently matched PHP TYPE structure."
  (save-excursion
    (let* ((begin (match-beginning 0))
           (end (php-parse-true-end type)))
      (when (integerp end)
        (let ((parse-function (intern (concat "php-parse-type-" 
                                              (symbol-name type)))))
          (unless (functionp parse-function)
            (error "Type %s has no parser defined." type))
          (goto-char (1- end))
          (append 
           (funcall parse-function)
           `((begin . ,begin) 
             (end . ,end)
             (text . ,(buffer-substring-no-properties begin 
                                                      (min (point-max) 
                                                           (1+ end)))))))))))

(defun php-parse-doc-block (doc-block)
  "Parse DOC BLOCK and return a list of doc block structures."
  (when (stringp doc-block)
    (save-match-data
      (let ((lines (safe-split-string
                    (chomp 
                     (replace-regexp-in-string 
                      "\n\n+" "\n"
                      (replace-regexp-in-string
                       "\n[[:space:]]*\\([^@]\\)" " \\1"
                       (replace-regexp-in-string 
                        "^[[:space:]]*\\*[[:space:]]*" "" 
                        (replace-regexp-in-string 
                         ".*\\*/" "" 
                         (replace-regexp-in-string 
                          "/\\*.*" "" doc-block)))))) "\n"))
            parse params)
        (dotimes (i (length lines) parse)
          (let ((line (chomp (elt lines i))))
            (cond ((and (> (length line) 0) (string= (substring line 0 1) "@"))
                   (let* ((parts (safe-split-string (substring line 1)))
                          (line-type (first parts)))
                     (cond ((string= line-type "param")
                            (add-to-list 'params 
                                         `((name . ,(third parts))
                                           (type . ,(second parts))
                                           (desc . ,(mapconcat 'identity
                                                               (nthcdr 3 parts) 
                                                               " ")))))
                           (t (add-to-list 'parse 
                                           `(,(intern (first parts))
                                             . ,(mapconcat 'identity 
                                                           (rest parts) 
                                                           " ")))))))
                  (t (let ((label 
                            (cond ((= i 0) 'short-desc)
                                  ((and (= i 1)
                                        (or (< (length (chomp line)) 11)
                                            (not (string= (substring line 0 11)
                                                          "PHP Version"))))
                                   'long-desc)
                                  (t 'php-version))))
                       (add-to-list 'parse `(,label . ,line)))))))
        (when params (add-to-list 'parse `(params . ,params)))
        parse))))

(defun php-scan-current (current-type struct-type &optional begin end)
  "Scan the current PHP CURRENT-TYPE structure for STRUCT-TYPE
structures and return a list of parses.  You may optionally pass
in BEGIN and END if you already have them."
  (let ((structs '()))
    (save-excursion
      (save-match-data
        (let ((extents (or (when (and (integerp begin) (integerp end))
                             `((begin . ,begin) (end . ,end)))
                           (php-parse-search current-type 'backward nil t)
                           (php-parse-search current-type 'current))))
          (when extents
            (let ((ext-begin (rest (assoc 'begin extents)))
                  (ext-end (rest (assoc 'end extents))))
              (goto-char ext-begin)
              (reverse (catch 'end
                         (while t
                           (let ((extents 
                                  (php-parse-search struct-type 
                                                    'forward ext-end)))
                             (unless extents
                               (throw 'end structs))
                             (set-match-data 
                              (rest (assoc 'match-data extents)))
                             (let* ((struct (php-parse-type struct-type))
                                    (struct-end (rest (assoc 'end struct))))
                               (add-to-list 'structs struct)
                               (when (>= struct-end ext-end)
                                 (throw 'end structs))))))))))))))
  
(defun php-parse-current (&optional type)
  "Parses the current PHP structure, optionally forcing TYPE.
TYPE follows the semantics of `php-parse-search'."
  (save-excursion
    (save-match-data
      (let ((types (cond
                    ((not type) 
                     '(constant property method class interface script file))
                    ((listp type) type)
                    (t (list type)))))
        (catch 'found
          (dolist (type types)
            (let ((extents 
                   (or (php-parse-search type 'backward nil t)
                       (php-parse-search type 'current))))
              (when extents
                (set-match-data (rest (assoc 'match-data extents)))
                (throw 'found 
                       (php-parse-type (rest (assoc 'type extents))))))))))))
        
(defun php-parse-search (type &optional direction bound include-point)
  "Finds the extents of the next ('forward), last ('backward) or
current ('current) TYPE structure.  TYPE may be a symbol, or list
of symbols.  If TYPE is a list, they will be checked in order
until a match is found.  If TYPE is nil, the list of types will
be pulled from php-type-regexps.  The default is 'current.  Stops
at BOUND.  If INCLUDE-POINT is true, (point) must be inside the
returned extents.  INCLUDE-POINT may also be an integer or list
of integers.  BOUND currently has no effect on 'current.  Point
will be left at end of structure if matched."
  (let* ((current-point (point))
         (types (cond
                 ((not type) 
                  '(constant property method class interface script file))
                 ((listp type) type)
                 (t (list type))))
         (search-func (cond
                       ((eq direction 'forward) 're-search-forward)
                       ((eq direction 'backward) 're-search-backward-greedy)
                       ((or (eq direction 'current)
                            (not direction)) 're-search-current)
                       (t (error "Invalid direction specified!"))))
         (retval
          (catch 'found
            (dolist (type types)
              (let* ((type-regexp-assoc (assoc type php-type-regexps))
                     (type-regex (if type-regexp-assoc
                                     (rest type-regexp-assoc)
                                   (error "Invalid type %s specified." type))))
                (save-match-data
                  (let ((found (catch 'found-valid
                                 (while (funcall search-func type-regex bound t)
                                   (when (not (php-inside-text-structp))
                                     (throw 'found-valid t))))))
                    (when found
                      (let* ((ext-begin (match-beginning 0))
                             (ext-end (php-parse-true-end type))
                             (included-points (cond ((not include-point) nil)
                                                    ((integerp include-point)
                                                     `(,include-point))
                                                    ((listp include-point) 
                                                     include-point)
                                                    (t `(,current-point))))
                             (success (and ext-end
                                           (or 
                                            (not included-points)
                                            (catch 'not-included
                                              (dolist (p included-points t)
                                                (when (or (< p ext-begin) 
                                                          (> p ext-end))
                                                  (throw 'not-included 
                                                         nil))))))))
                        (when success 
                          (throw 'found 
                                 `((begin . ,ext-begin)
                                   (end . ,ext-end)
                                   (type . ,type)
                                   (match-data . ,(match-data)))))))))))))
         (new-point (rest (assoc 'end retval))))
    (goto-char (or new-point current-point))
    retval))

(defun php-parse-true-end (type)
  "Finds the true end of the structure currently matched."
  (let (bogus)
    (goto-char (1- (match-end 0)))
    (cond
     ((eq type 'comment)
      (php-skip-this-text-struct))
     ((eq type 'file)
      (goto-char (point-max)))
     ((eq type 'script)
      (or (save-match-data
            (catch 'found
              (while (re-search-forward "\\?>" nil t)
                (backward-char)
                (unless (php-in-text-structp)
                  (throw 'found t)))))
          (goto-char (point-max))))
     ((or (eq type 'method)
          (eq type 'class)
          (eq type 'interface))
      (let ((end-re (concat "[{" (when (eq type 'method) ";") "]")))
        (save-match-data
          (when (looking-at-p ")")
            (forward-char))
          (when (looking-at-p ws-re)
            (re-search-forward non-ws-re nil t)
            (backward-char))
          (if (looking-at-p end-re)
              (if (looking-at-p ";")
                  (forward-char)
                (condition-case nil
                    (forward-sexp)
                  (error (goto-char (point-max)))))
            (set 'bogus (not (catch 'found
                               (while (php-inside-text-structp)
                                 (php-skip-this-text-struct)
                                 (when (looking-at-p ws-re)
                                   (re-search-forward non-ws-re nil t)
                                   (backward-char))
                                 (when (looking-at-p end-re)
                                   (forward-sexp)
                                   (throw 'found (point)))))))))))
     (t
      (if (char-equal ?{ (char-after))
          (condition-case nil
              (forward-sexp)
            (error (goto-char (point-max))))
        (forward-char))))
    (unless bogus
      (point))))

(defun php-find-in-buffer (type name)
  "Find the TYPE structure named NAME in the current buffer."
  (interactive (list (intern (completing-read "Type: " php-type-regexps nil 
                                              t))
                     (read-string "Name: ")))
  (goto-char (point-min))
  (catch 'found
    (while t
      (let ((parse (php-parse-search type 'forwards)))
        (unless parse
          (throw 'found nil))
        (let (parse-name (rest (assoc 'name parse)))
          (when (string= name parse-name)
            (goto-char (rest (assoc 'begin parse)))
            (throw 'found t)))))))

(defun php-parse-p (a)
  "Determine whether A is a php-parse structure."
  (and (listp a)
       (assoc 'type a)
       (let ((begin (rest (assoc 'begin a))))
         (integerp begin))
       (let ((end (rest (assoc 'end a))))
         (integerp end))
       (assoc 'text a)))

(defun php-parse-send-to= (parse send-to)
  "This function decides whether a parse matches an element of
php-parse-send-to-{front,back}."
  (when (php-parse-p parse)
    (let* ((name (first send-to))
           (flags (second send-to))
           (abstractp (first flags))
           (finalp (second flags))
           (staticp (third flags))
           (type (third send-to))
           (visibility (fourth send-to)))
      (and (string= name (rest (assoc 'name parse)))
           (eq abstractp (rest (assoc 'abstractp parse)))
           (eq finalp (rest (assoc 'finalp parse)))
           (eq staticp (rest (assoc 'staticp parse)))
           (eq type (rest (assoc 'type parse)))
           (eq visibility (rest (assoc 'visibility parse)))))))

(defun php-parse< (a b)
  "Ordering function for two php-parse structures.  If a
structure's name is found in the list `php-parse-send-to-front'
then it will be evalualated automatically less than unless the
second argument is also in the list.  If the name is in
`php-parse-send-to-back' it will be handled in the opposite
manner."
  (dolist (v `(,a ,b))
    (unless (php-parse-p v)
      (error "Invalid php-parse parameter %s." v)))
  (let* ((a-abstractp (rest (assoc 'abstractp a)))
         (b-abstractp (rest (assoc 'abstractp b)))
         (a-finalp (rest (assoc 'finalp a)))
         (b-finalp (rest (assoc 'finalp b)))
         (a-staticp (rest (assoc 'staticp a)))
         (b-staticp (rest (assoc 'staticp b)))
         (a-type (rest (assoc 'type a)))
         (b-type (rest (assoc 'type b)))
         (a-visibility (rest (assoc 'visibility a)))
         (b-visibility (rest (assoc 'visibility b)))
         (a-name (rest (assoc 'name a)))
         (b-name (rest (assoc 'name b)))
         (a-front-struct (find a php-parse-send-to-front 
                               :test 'php-parse-send-to=))
         (a-front-count (length (member a-front-struct 
                                        php-parse-send-to-front)))
         (a-back-struct (find a php-parse-send-to-back 
                              :test 'php-parse-send-to=))
         (a-back-count (length (member a-back-struct 
                                       php-parse-send-to-back)))
         (b-front-struct (find b php-parse-send-to-front 
                               :test 'php-parse-send-to=))
         (b-front-count (length (member b-front-struct 
                                        php-parse-send-to-front)))
         (b-back-struct (find b php-parse-send-to-back 
                              :test 'php-parse-send-to=))
         (b-back-count (length (member b-back-struct 
                                       php-parse-send-to-back))))
    (when (and (> a-front-count 0) (> a-back-count 0))
      (message (concat "Structure %s was in both the send to front and send "
                       "to back lists, assuming you want it in the front.") 
               a-name))
    (when (and (> b-front-count 0) (> b-back-count 0))
      (message (concat "Structure %s was in both the send to front and send "
                       "to back lists, assuming you want it in the front.") 
               b-name))
    (or (and a-abstractp (not b-abstractp))
        (and 
         (eq a-abstractp b-abstractp)
         (or (and a-finalp (not b-finalp))
             (and 
              (eq a-finalp b-finalp)
              (or (and a-staticp (not b-staticp))
                  (and 
                   (eq a-staticp b-staticp)
                   (or 
                    (and (eq a-type 'constant)
                         (or (eq b-type 'property)
                             (eq b-type 'method)))
                    (and (eq a-type 'property)
                         (eq b-type 'method))
                    (and (eq a-type b-type)
                         (or (and (eq a-visibility 'public)
                                  (or (eq b-visibility 
                                          'protected)
                                      (eq b-visibility 
                                          'private)))
                             (and (eq a-visibility 
                                      'protected)
                                  (eq b-visibility 
                                      'private))
                             (and (eq a-visibility 
                                      b-visibility)
                                  (or (and (> a-front-count 0)
                                           (zerop b-front-count))
                                      (and (> a-front-count 0)
                                           (> b-front-count 0)
                                           (> a-front-count
                                              b-front-count))
                                      (and (zerop a-front-count) 
                                           (zerop a-back-count)
                                           (zerop b-front-count) 
                                           (> b-back-count 0))
                                      (and (zerop a-front-count) 
                                           (> a-back-count 0)
                                           (zerop b-front-count) 
                                           (> b-back-count 0)
                                           (> a-back-count b-back-count))
                                      (and (not (and (zerop a-front-count)
                                                     (> b-front-count 0)))
                                           (not (and (> a-front-count 0)
                                                     (> b-front-count 0)
                                                     (> b-front-count
                                                        a-front-count)))
                                           (not (and (zerop a-front-count)
                                                     (> a-back-count 0)
                                                     (zerop b-front-count)
                                                     (zerop b-back-count)))
                                           (or
                                            (string< a-name 
                                                     b-name)
                                            (string= a-name 
                                                     b-name))))))))))))))))

(defun php-parse> (a b)
  "Ordering function for two php-parse structures."
  (php-parse< b a))

(defun php-parse= (a b)
  "Ordering function for two php-parse structures."
  (and (php-parse< a b) (php-parse> a b)))

(defun php-parse-pos< (a b)
  "Ordering function for two php-parse structues by position in
buffer."
  (< (rest (assoc 'begin a))
     (rest (assoc 'begin b))))

(defun php-parse-pos> (a b)
  "Ordering function for two php-parse structues by position in
buffer."
  (> (rest (assoc 'begin a))
     (rest (assoc 'begin b))))

(defun php-parse-pos= (a b)
  "Ordering function for two php-parse structues by position in
buffer."
  (= (rest (assoc 'begin a))
     (rest (assoc 'begin b))))

(defun php-parse->def (type-struct)
  "Create a PHP type definition from TYPE-STRUCT."
  (unless (php-parse-p type-struct)
    (error "Invalid PHP parse struct %s" type-struct))
  (let ((type (rest (assoc 'type type-struct)))
        (name (rest (assoc 'name type-struct)))
        (documentation (rest (assoc 'documentation-text type-struct)))
        (abstractp (rest (assoc 'abstractp type-struct)))
        (finalp (rest (assoc 'finalp type-struct)))
        (staticp (rest (assoc 'staticp type-struct)))
        (visibility (rest (assoc 'visibility type-struct)))
        (extends (rest (assoc 'extends type-struct)))
        (constants (rest (assoc 'constants type-struct)))
        (methods (rest (assoc 'methods type-struct)))
        (text (rest (assoc 'text type-struct)))
        (parts '()))
    (when documentation (add-to-list 'parts (concat "\n" documentation "\n")))
    (cond
     ((or (eq type 'file)
          (eq type 'script)
          (eq type 'comment))
      (add-to-list 'parts text))
     ((eq type 'interface)
      (add-to-list 'parts "interface")
      (add-to-list 'parts name)
      (when extends
        (add-to-list 'parts (concat "extends "
                                    (mapconcat 'identity extends ", "))))
      (add-to-list 'parts "\n{")
      (dolist (p (sort (append constants methods) 'php-parse<))
        (let ((def (php-parse->def p)))
          (add-to-list 'parts (concat 
                               (unless (string= "\n" 
                                                (substring (first parts) 
                                                           -1)) "\n")
                               "    " def
                               (when (multilinep def) "\n")))))
      (add-to-list 'parts "\n}"))
     ((eq type 'class)
      (let ((implements (rest (assoc 'implements type-struct)))
            (properties (rest (assoc 'properties type-struct))))
        (when abstractp (add-to-list 'parts "abstract"))
        (when finalp (add-to-list 'parts "final"))
        (add-to-list 'parts "class")
        (add-to-list 'parts name)
        (when extends
          (add-to-list 'parts (concat "extends " extends)))
        (when implements
          (add-to-list 'parts (concat "implements " 
                                      (mapconcat 'identity implements ", "))))
        (add-to-list 'parts "\n{")
        (dolist (p (sort (append constants properties methods) 'php-parse<))
          (let ((def (php-parse->def p)))
            (add-to-list 'parts (concat 
                                 (unless (string= "\n" 
                                                  (substring (first parts) 
                                                             -1)) "\n")
                                 "    " def
                                 (when (multilinep def) "\n")))))
        (add-to-list 'parts "\n}")))
     ((eq type 'method)
      (let ((statements (rest (assoc 'statements type-struct))))
        (when abstractp (add-to-list 'parts "abstract"))
        (when finalp (add-to-list 'parts "final"))
        (when staticp (add-to-list 'parts "static"))
        (add-to-list 'parts (symbol-name visibility))
        (add-to-list 'parts "function")
        (add-to-list 'parts name)
        (add-to-list 'parts 
                     (concat "("
                             (mapconcat (lambda (a)
                                          (let ((type (rest (assoc 'type a))))
                                            (concat (if (stringp type) 
                                                        (concat type " ") "")
                                                    (rest (assoc 'name a))))) 
                                        (rest (assoc 'arguments type-struct))
                                        ", ") ")" 
                                        (if (not statements)
                                            ";" 
                                          (concat "\n{\n    " 
                                                  (rest (assoc 'statements 
                                                               type-struct))
                                                  "\n}"))))))
     ((eq type 'property)
      (let ((value (rest (assoc 'value type-struct))))
        (when staticp (add-to-list 'parts "static"))
        (add-to-list 'parts (symbol-name visibility))
        (add-to-list 'parts (concat name
                                    (when value
                                      (concat " = " value)) ";"))))
     ((eq type 'constant)
      (add-to-list 'parts "const")
      (let ((value (rest (assoc 'value type-struct))))
        (add-to-list 'parts (concat name
                                    (when value
                                      (concat " = " value)) ";")))))
    (mapconcat 'identity (reverse parts) " ")))
      
(defun php-parse-order-test ()
  (let* ((parse (php-parse-current 'class)) 
         (constants (rest (assoc 'constants parse))) 
         (properties (rest (assoc 'properties parse))) 
         (methods (rest (assoc 'methods parse)))) 
    (switch-to-buffer "*phptest*") 
    (insert (mapconcat 'identity 
                       (mapcar (lambda (x) 
                                 (rest (assoc 'text x))) 
                               (sort (append constants properties methods) 
                                     'php-parse<)) "\n\n"))))
(provide 'php-parse)
