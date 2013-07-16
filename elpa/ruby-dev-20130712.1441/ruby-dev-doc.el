;;; ruby-dev-doc.el — Module to display the documentation of Ruby symbols

(require 'ruby-dev-core)
(require 'ruby-dev-error)
(require 'ruby-dev-utils)

;;;###autoload
(defgroup ruby-dev-doc nil
  "Module to display the documentation of Ruby symbols."
  :group 'ruby-dev)

(defface ruby-dev-doc-header-face
  '((t (:inherit font-lock-function-name-face
                 :height 160 :weight bold)))
  "Face for the title of the documented object."
  :group 'ruby-dev-faces
  :group 'ruby-dev-doc)

(defface ruby-dev-doc-overload-header-face
  '((t (:inherit font-lock-function-name-face
                 :height 150 :weight bold)))
  "Face for the title of the documented object."
  :group 'ruby-dev-faces
  :group 'ruby-dev-doc)

(defface ruby-dev-doc-section-header-face
  '((t (:inhernit default
                 :height 140 :weight bold)))
  "Face for the title of a documentation section (parameters, etc.)."
  :group 'ruby-dev-faces
  :group 'ruby-dev-doc)

(defface ruby-dev-doc-arg-name-face
  '((t (:inherit default :weight bold)))
  "Face for argument names."
  :group 'ruby-dev-faces
  :group 'ruby-dev-doc)

(defface ruby-dev-doc-note-name-face
  '((t (:inherit default :weight bold)))
  "Face for special indicators before notes (deprecated, API, etc.)."
  :group 'ruby-dev-faces
  :group 'ruby-dev-doc)

(defface ruby-dev-doc-example-name-face
  '((t (:inherit default :weight bold)))
  "Face used for the names of examples"
  :group 'ruby-dev-faces
  :group 'ruby-dev-doc)

(defface ruby-dev-doc-ref-face
  '((t (:inherit font-lock-constant-face
                 :underline t)))
  "Face for links towards other objects."
  :group 'ruby-dev-faces
  :group 'ruby-dev-doc)

(defcustom ruby-dev-doc-completions-limit 1000
  "The maximal amount of elements retrieved when getting completions from the
ruby-dev process."
  :group 'ruby-dev-doc)

(defvar ruby-dev-doc-source-location nil)
(defvar ruby-dev-doc-instance-methods-line nil)
(defvar ruby-dev-doc-class-methods-line nil)
(defvar ruby-dev-doc-included-modules-line nil)
(defvar ruby-dev-doc-source-line nil)

(defvar ruby-dev-doc-buffer nil
  "Buffer used to show docs to the user.")

(defun ruby-dev-doc-ref (string)
  "Returns the passed string with properties that make it a button towards
another documentation symbol."
  (with-temp-buffer
    (lexical-let ((s string))
      (insert-text-button s 'action (lambda (b) (ruby-dev-show-doc s))
                             'face 'ruby-dev-doc-ref-face))
    (buffer-string)))

(defun ruby-dev-make-doc-references (string)
  "Returns the passed string, where each reference to another object
has been replaced with a link to that object.

References are assumed to be between curly braces."
  (let ((index 0))
    (loop while (string-match "{[^{}]+}" string index) do
      (let ((start (nth 0 (match-data)))
            (end (nth 1 (match-data))))
        (setq string (concat (substring string 0 (1+ start))
                             (ruby-dev-doc-ref
                              (substring string (1+ start) (1- end)))
                             (substring string (1- end))))
        (setq index end))))
  string)

(defun ruby-dev-format-types (types)
  "Returns a string to display an array of types.

If the array is empty, Object is assumed to be the type to use. Otherwise, each
type is replaced with a link and all those links are joined with commas. This
also supports YARD's syntax for collection types: Container<Contained>."
  (if (= (length types) 0) (ruby-dev-doc-ref "Object")
    (let ((refs (loop for type across types
                      collecting
                      (if (string-match "^\\([^<>]+\\)<\\([^<>]+\\)>$" type)
                          (let ((fst (match-string 1 type))
                                (snd (match-string 2 type)))
                            (concat (ruby-dev-doc-ref fst) "<" (ruby-dev-doc-ref snd) ">"))
                        (ruby-dev-doc-ref type)))))
      (ruby-dev-join-list refs))))

(defun ruby-dev-format-doc-text (text)
  "Format some documentation text.

If TEXT is nil, an empty string is still returned."
  (if text (ruby-dev-make-doc-references text) ""))

(defun ruby-dev-find-tags (tags type)
  "Searches for tags whose name match a certain pattern."
  (remove-if-not (lambda (tag)
                   (string-match type (cdr (assoc 'tag-name tag))))
                 tags))

(defun ruby-dev-format-typed-doc (types text &optional name)
  "Formats the documentation for something that has a type.
Also adds a name to the string if needed."
  (concat "(" (ruby-dev-format-types types) ") "
          (if name (concat (propertize name 'face 'ruby-dev-doc-arg-name-face)
                           ": "
            ""))
          (ruby-dev-format-doc-text text)))

(defmacro ruby-dev-handle-tags (header tag-var tag-name arguments &rest body)
  "Runs through each tag found with TAG-VAR, and display them using BODY,
which can use its properties using ARGUMENTS to destructure it.

HEADER is the name to use for the section."
  (declare (indent 4))
  (let ((tags (gensym))
        (tag  (gensym)))
    `(let ((,tags (ruby-dev-find-tags ,tag-var ,tag-name)))
       (when (> (length ,tags) 0)
         (insert (propertize ,header
                             'face 'ruby-dev-doc-section-header-face))
         (loop for ,tag across ,tags do
           (with-ruby-dev-data ,arguments ,tag ,@body))
         (insert "\n")))))

(defmacro ruby-dev-handle-single-tag (header tag-var tag-name arguments &rest body)
  "Like `ruby-dev-handle-tags', except only the first tag found is used."
  (declare (indent 4))
  (let ((label (gensym)))
    `(catch ',label
       (ruby-dev-handle-tags ,header ,tag-var ,tag-name ,arguments
         ,@body
         (throw ',label nil)))))

(defun ruby-dev-search-doc (input)
  "Searches Ruby for symbols starting with INPUT.

This returns a list of results, where each result is a string. If an error
occurs, it is shown to the user, and nil is returned (so that further
computations can go on as if there had been no matches found)."
  (ruby-dev-send-request "search-doc" :input input :limit ruby-dev-doc-completions-limit)
  (let ((response (ruby-dev-read-response)))
    (with-ruby-dev-data (success completions) response
      (if (eql success :json-false) (ruby-dev-show-error response)
        (loop for i across completions
              collecting i)))))

(defun ruby-dev-doc-completions (input predicate flag)
  "Completion function for `completing-read' that uses results from
`ruby-dev-search-doc' as a completion source."
  (unless predicate (setq predicate (lambda (s) t)))
  (cond
   ((eql nil flag)
    (let (c (ruby-dev-search-doc input))
      (cond
       ((null c) nil)
       ((null (rest c)) t)
       (t input))))
   ((eql t flag)
    (ruby-dev-search-doc input))
   ((eql 'lambda flag)
    (if (find input (ruby-dev-search-doc input)) t))
   ((eql 'metadata flag)
    nil)
   ((and (consp flag) (eql (car flag) 'boundaries))
    (let ((suffix (cdr flag)))
      (cons 0 (length suffix))))))

(defun ruby-dev-object-info (symbol)
  "Returns all the informations that can be found about an object.

The informations are returned in an alist.

It always contains:
  - SYMBOL (which is the argument that was passed to this function)
  - TYPE (class, module, or method, as a string)
  - SOURCE-LOCATION (nil or an array containing the filename and the line number
    where the symbol was defined)
  - SOURCE (nil or the source code that defines this symbols)
  - DOC (the documentation, as a docstring, see below)

For modules and classes there also is:
  - SUPERCLASS (the name of the superclass or nil)
  - INCLUDED-MODULES (array with the name of modules inculded in this one)
  - METHODS and INSTANCE-METHODS, two alists containing
    - OLD (inherited class and instance methods, respectively)
    - NEW (newly defined class and instance methods, respectively)

For methods:
  - LANGUAGE (either c or ruby)
  - VISIBILITY (public, private, protected)
  - SIGNATURE (nil or the arguments, wrapped in parentheses)

Docstrings are alists that contain:
  - TEXT (documentation for the symbol or nil)
  - TAGS (an array of tags adding information about that symbol)

Tags just copy the attributes of `YARD::Tags::Tag' objects (and their
subclasses if needed).

If an error occured, it is shown to the user and nil is returned."
  (ruby-dev-send-request "object-info" :symbol symbol)
  (let ((response (ruby-dev-read-response)))
    (with-ruby-dev-data (success) response
      (if (eql success :json-false) (ruby-dev-show-error response)
        response))))

;;;###autoload
(defun ruby-dev-show-doc (symbol)
  "Shows the documentation for a given symbol.

If the symbol is not found, an error message is shown instead."
  (interactive
   (progn
     (ruby-dev-ensure)
     (list (completing-read "ri " 'ruby-dev-doc-completions))))
  (ruby-dev-ensure)
  (let ((doc (ruby-dev-object-info symbol)))
    (unless (and ruby-dev-doc-buffer (buffer-live-p ruby-dev-doc-buffer))
      (ruby-dev-create-doc-buffer))
    (with-current-buffer ruby-dev-doc-buffer
      (save-excursion
        (toggle-read-only -1)
        (erase-buffer)
        (ruby-dev-write-doc doc)
        (toggle-read-only 1))))
  (unless (equal (current-buffer) ruby-dev-doc-buffer)
    (switch-to-buffer-other-window ruby-dev-doc-buffer)))

(defun ruby-dev-create-doc-buffer ()
  "Creates the buffer to show the documentation to the user."
  (setq ruby-dev-doc-buffer (generate-new-buffer "*ruby-dev ri*"))
  (with-current-buffer ruby-dev-doc-buffer
    (ruby-dev-doc-mode)))

(defun ruby-dev-write-doc (doc)
  "Writes the documentation to the current buffer."
  (with-ruby-dev-data (type source-location) doc
    (set 'ruby-dev-doc-source-location source-location)
    (cond
     ((string= "method" type) (ruby-dev-write-method doc))
     ((string= "module" type) (ruby-dev-write-module doc))
     ((string= "class"  type) (ruby-dev-write-module doc)))))

(defun ruby-dev-write-method (response)
  "Writes the documentation for a method to the current buffer."
  (with-ruby-dev-data (symbol visibility signature
                       source-location doc
                       language source) response
    (ruby-dev-write-doc-title (concat symbol signature))
    (ruby-dev-write-doc-summary
     (if visibility (concat (capitalize visibility) " method")
       "Method")
     source-location)
    (ruby-dev-write-docstring doc)
    (ruby-dev-write-source source language)))

(defun ruby-dev-write-module (response)
  (with-ruby-dev-data (symbol type
                       source-location source
                       superclass included-modules
                       methods instance-methods
                       doc) response
    (ruby-dev-write-doc-title symbol)
    (ruby-dev-write-doc-summary
     (cond
      ((string= type "module") "Module")
      (superclass (concat "Subclass of " (ruby-dev-doc-ref superclass)))
      (t "Class"))
     source-location)
    (ruby-dev-write-docstring doc)
    (ruby-dev-write-included-modules included-modules)
    (ruby-dev-write-method-list symbol methods instance-methods)
    (ruby-dev-write-source source "ruby")))

(defun ruby-dev-write-doc-title (title)
  "Writes the name of the symbol to document into the current buffer."
  (insert (propertize title 'face 'ruby-dev-doc-header-face) "\n")
  (dotimes (n (length title))
    (insert (propertize "=" 'face 'ruby-dev-doc-header-face)))
  (insert "\n\n"))

(defun ruby-dev-write-doc-summary (title source-location)
  "Writes a brief description of the described symbol.

TITLE describes what the symbol represents (a class, a public method, etc.)"
  (insert "(" title " ")
  (if (not source-location) (insert "defined at an unknown location")
    (lexical-let ((fname (aref source-location 0))
                  (line  (aref source-location 1)))
        (insert "defined in `")
        (insert-text-button (file-name-nondirectory fname)
                            'action (lambda (b)
                                      (ruby-dev-open-source fname line)))
        (insert "'")))
  (insert ".)\n\n"))

(defun ruby-dev-write-source (source language)
  "Writes source code to the current buffer.

LANGUAGE should be a string with the name of the language, either ruby or c."
  (when source
    (set 'ruby-dev-doc-source-line (line-number-at-pos))
    (insert (propertize "Source Code: "
                        'face 'ruby-dev-doc-section-header-face)
            "\n\n")
    (insert (ruby-dev-highlight-code source
                                     (if (string= language "ruby")
                                         'ruby-mode 'c-mode)))))

(defun ruby-dev-write-link-array (header elements &optional prefix line-var)
  "Writes an array of links towards other symbols.

HEADER is the name to use for the section.
Each element can be prefixed with PREFIX as needed.
If LINE-VAR is non-nil, it is set to the line number of the header."
  (unless prefix (setq prefix ""))
  (when (> (length elements) 0)
    (when line-var (set line-var (line-number-at-pos)))
    (insert (propertize (concat header ":")
                        'face 'ruby-dev-doc-section-header-face)
            "\n\n")
    (loop for el across elements do
      (insert "- " (ruby-dev-doc-ref (concat prefix el)) "\n"))
    (insert "\n")))

(defun ruby-dev-write-method-list (symbol methods instance-methods)
  "Writes a list of links towards instance and class methods "
  (ruby-dev-write-link-array "Instance Methods"
                             (cdr (assoc 'new instance-methods))
                             (concat symbol "#")
                             'ruby-dev-doc-instance-methods-line)
  (ruby-dev-write-link-array "Class Methods"
                             (cdr (assoc 'new methods))
                             (concat symbol ".")
                             'ruby-dev-doc-class-methods-line))

(defun ruby-dev-write-included-modules (modules)
  "Writes a links to the modules included in the current one to the current
buffer."
  (ruby-dev-write-link-array "Included Modules" modules ""
                             'ruby-dev-doc-included-modules-line))

(defun ruby-dev-write-docstring (doc)
  "Writes a docstring along with its tags to the current buffer."
  (if (not doc) (insert "No documentation found.\n\n")
    (with-ruby-dev-data (text tags) doc
      (when (> (length text) 0)
        (insert (ruby-dev-highlight-code-blocks-in
                 (ruby-dev-make-doc-references text))
                "\n\n"))
      (when tags
        (ruby-dev-write-tags tags)))))

(defun ruby-dev-write-tags (tags)
  "Writes all supported tags found in the passed array to the
current buffer."
  (ruby-dev-write-tag-params    tags)
  (ruby-dev-write-tag-yield     tags)
  (ruby-dev-write-tag-raise     tags)
  (ruby-dev-write-tag-return    tags)
  (ruby-dev-write-tag-examples  tags)
  (ruby-dev-write-tag-notes     tags)
  (ruby-dev-write-tag-see       tags)
  (ruby-dev-write-tag-todo      tags)
  (ruby-dev-write-tag-since     tags)
  (ruby-dev-write-tag-version   tags)
  (ruby-dev-write-tag-author    tags)
  (ruby-dev-write-tag-overloads tags))

(defun ruby-dev-write-tag-params (tags)
  (ruby-dev-handle-tags "Method parameters:\n" tags "param" (name types text)
    (insert "- " (ruby-dev-format-typed-doc types text name) "\n"))
  (let ((tags (ruby-dev-find-tags tags "option")))
    (when (> (length tags) 0)
      (let ((options (ruby-dev-group-by (lambda (tag) (cdr (assoc 'name tag))) tags)))
        (maphash
         (lambda (name option-tags)
           (insert (propertize (concat "Option hash: " name "\n")
                               'face 'ruby-dev-doc-section-header-face))
           (dolist (tag option-tags)
             (with-ruby-dev-data (pair) tag
               (with-ruby-dev-data (types name text defaults) pair
                 (insert "- " (ruby-dev-format-typed-doc types text name))
                 (when defaults (insert " (defaults to: " defaults ")"))
                 (insert "\n"))))
             (insert "\n"))
         options)))))

(defun ruby-dev-write-tag-yield (tags)
  (ruby-dev-handle-single-tag "Yields: " tags "yield" (types text)
    (when (> (length types) 0)
      (insert "(" (ruby-dev-join-list (loop for type across types
                                            collecting type))
              ") "))
    (insert (ruby-dev-format-doc-text text) "\n\n"))
  (ruby-dev-handle-tags "Yield parameters:\n" tags "yieldparam" (types text name)
    (insert "- " (ruby-dev-format-typed-doc types text name) "\n"))
  (ruby-dev-handle-single-tag "Yield returns: " tags "yieldreturn" (types text)
    (insert (ruby-dev-format-typed-doc types text) "\n\n")))

(defun ruby-dev-write-tag-raise (tags)
  (ruby-dev-handle-tags "Raises:\n" tags "raise" (types text)
    (insert "- " (ruby-dev-format-typed-doc types text) "\n")))

(defun ruby-dev-write-tag-return (tags)
  (ruby-dev-handle-single-tag "Returns: " tags "return" (types text)
    (insert (ruby-dev-format-typed-doc types text) "\n\n")))

(defun ruby-dev-write-tag-examples (tags)
  (ruby-dev-handle-tags "Examples:\n\n" tags "example" (name text)
    (when (and name (not (equal "" name)))
      (insert (propertize name
                          'face 'ruby-dev-doc-example-name-face)
              "\n\n"))
    (insert (ruby-dev-highlight-code text) "\n\n")))

(defun ruby-dev-write-tag-notes (tags)
  (ruby-dev-handle-tags "Notes:\n" tags "\\(abstract\\|api\\|deprecated\\|note\\)"
                        (tag-name text)
                        (message "tag name: %s" tag-name)
     (insert "- ")
     (if (string= tag-name "note") (insert (ruby-dev-format-doc-text text))
       (insert (propertize
                (cond
                 ((string= tag-name "abstract")   "Abstract method")
                 ((string= tag-name "api")        "API")
                 ((string= tag-name "deprecated") "Deprecated"))
                'face 'ruby-dev-doc-note-name-face))
       (when text
         (insert ": " (ruby-dev-format-doc-text text))))
     (insert "\n")))

(defun ruby-dev-write-tag-see (tags)
  (ruby-dev-handle-tags "See Also:\n" tags "see" (name text)
    (insert "- " (ruby-dev-doc-ref name))
    (when text
      (insert ": " (ruby-dev-format-doc-text text)))
    (insert "\n")))

(defun ruby-dev-write-tag-todo (tags)
  (ruby-dev-handle-tags "To Do:\n" tags "todo" (text)
    (insert "- " (ruby-dev-format-doc-text text) "\n")))

(defun ruby-dev-write-tag-overloads (tags)
  (let ((overloads (ruby-dev-find-tags tags "overload")))
    (loop for overload across overloads do
      (with-ruby-dev-data (docstring signature) overload
        (insert (propertize signature
                            'face 'ruby-dev-doc-overload-header-face)
                "\n")
        (dotimes (n (length signature))
          (insert (propertize "-" 'face 'ruby-dev-doc-overload-header-face)))
        (insert "\n\n")
        (ruby-dev-write-docstring docstring)
        (insert "\n")))))

(macrolet ((define-handler (name tag)
             `(defun ,name (tags)
                (ruby-dev-handle-single-tag ,(concat (capitalize tag) ": ")
                                            tags ,tag (text)
                  (insert (ruby-dev-format-doc-text text) "\n\n")))))
  (define-handler ruby-dev-write-tag-since   "since")
  (define-handler ruby-dev-write-tag-version "version")
  (define-handler ruby-dev-write-tag-author  "author"))

(macrolet ((define-goto (name)
             (let ((func-name (intern (concat "ruby-dev-doc-goto-" name)))
                   (var-name  (intern (concat "ruby-dev-doc-" name "-line"))))
               `(defun ,func-name ()
                  ,(format "Jump to the line containing the %s section in the buffer."
                           name)
                  (interactive)
                  (if ,var-name (goto-line ,var-name)
                    (message "No %s section in this buffer." name))))))
  (define-goto "instance-methods")
  (define-goto "class-methods")
  (define-goto "included-modules")
  (define-goto "source"))

(defun ruby-dev-doc-visit-source ()
  "Opens the file containing the definition of the current symbol,
at the right line."
  (interactive)
  (if ruby-dev-doc-source-location
      (ruby-dev-open-source (aref ruby-dev-doc-source-location 0)
                            (aref ruby-dev-doc-source-location 1))
    (message "Soure location unknown. Try `ruby-dev-goto-source'.")))

;;;###autoload
(defvar ruby-dev-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'forward-button)
    (define-key map (kbd "e") 'ruby-dev-doc-visit-source)
    (define-key map (kbd "i") 'ruby-dev-doc-goto-instance-methods)
    (define-key map (kbd "c") 'ruby-dev-doc-goto-class-methods)
    (define-key map (kbd "s") 'ruby-dev-doc-goto-source)
    (define-key map (kbd "m") 'ruby-dev-doc-goto-included-modules)
    (define-key map (kbd "/") 'ruby-dev-show-doc)
    map)
  "Key bindings for the `ruby-dev-doc-mode'.")

;;;###autoload
(define-derived-mode ruby-dev-doc-mode special-mode "ri"
  "Major mode for viewing Ruby documentation for classes, modules or methods,
as shown by `ruby-dev-show-doc'.

\\{ruby-dev-doc-mode-map}"
  (toggle-truncate-lines 1)
  (toggle-read-only 1)
  (set (make-local-variable 'ruby-dev-doc-source-location)       nil)
  (set (make-local-variable 'ruby-dev-doc-instance-methods-line) nil)
  (set (make-local-variable 'ruby-dev-doc-class-methods-line)    nil)
  (set (make-local-variable 'ruby-dev-doc-included-modules-line) nil)
  (set (make-local-variable 'ruby-dev-doc-source-line)           nil))

(provide 'ruby-dev-doc)
