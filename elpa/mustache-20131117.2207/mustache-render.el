(require 'ht)
(require 's)
(require 'dash)

(eval-when-compile '(require 'cl)) ;; return, dolist

(load "mustache-lex.el")
(load "mustache-parse.el")

(defun mst--render (template context)
  "Render a mustache TEMPLATE with hash table CONTEXT."
  (-> template
    mst--lex
    mst--clean-whitespace
    mst--parse
    (mst--render-section-list context)))

(defun mst--mapconcat (function sequence)
  "Apply FUNCTION to every element in SEQUENCE, and concat the results as strings."
  (mapconcat function sequence ""))

(defmacro mst--amapconcat (form sequence)
  "Anaphoric version of `mst--mapconcat'."
  `(mst--mapconcat (lambda (it) ,form) ,sequence))

;; todo: set flag to set tolerance of missing templates
(defun mst--get-partial (name)
  "Get the first partial whose file name is NAME.mustache, or nil otherwise.
Partials are searched for in `mustache-partial-paths'."
  (unless (listp mustache-partial-paths)
    (error "`mustache-partial-paths' must be a list of paths"))
  (let ((partial-name (format "%s.mustache" name)))
    (dolist (path mustache-partial-paths)
      (-when-let*
          ((partials (directory-files path nil "\\.mustache$"))
           (matching-partial (--first
                              (string-match-p (regexp-quote partial-name) it)
                              partials)))
        (return
         (with-temp-buffer
           (insert-file-contents-literally (expand-file-name matching-partial path))
           (buffer-substring-no-properties (point-min) (point-max))))))))

(defun mst--render-section-list (sections context)
  "Render a parsed list SECTIONS in CONTEXT."
  (mst--amapconcat (mst--render-section it context) sections))

(defun mst--context-get (context variable-name &optional default)
  "Lookup VARIABLE-NAME in CONTEXT, returning DEFAULT if not present."
  (when (eq mustache-key-type 'keyword)
    (setq variable-name (intern (concat ":" variable-name))))
  (ht-get context variable-name default))

(defun mst--render-tag (parsed-tag context)
  "Given PARSED-TAG, render it in hash table CONTEXT."
  (let ((inner-text (mst--tag-text parsed-tag)))
    (cond
     ((mst--comment-tag-p parsed-tag)
      "")
     ((mst--unescaped-tag-p parsed-tag)
      (let ((variable-value (mst--context-get context (s-trim (substring inner-text 1)) "")))
        (when (numberp variable-value)
          (setq variable-value (number-to-string variable-value)))
        variable-value))
     ((mst--partial-tag-p parsed-tag)
      (let ((partial (mst--get-partial (s-trim (substring inner-text 1)))))
        (if partial
            (mst--render partial context)
          "")))
     (t ;; normal variable
      (let ((variable-value (mst--context-get context inner-text "")))
        (when (numberp variable-value)
          (setq variable-value (number-to-string variable-value)))
        (mst--escape-html variable-value))))))

(defun mst--context-add (table from-table)
  "Return a copy of TABLE where all the key-value pairs in FROM-TABLE have been set."
  (let ((new-table (ht-copy table)))
    (ht-update new-table from-table)
    new-table))

(defun mst--listp (object)
  "Return t if OBJECT is a list.
Unlike `listp', does not return t if OBJECT is a function."
  (and (not (functionp object)) (listp object)))

(defun mst--section-name (section-tag)
  "Get the name of this SECCTION-TAG.
E.g. from {{#foo}} to \"foo\"."
  (-> section-tag ;; e.g (:tag "#foo")
    mst--tag-text ;; to "#foo"
    (substring 1) ;; to "foo"
    s-trim))

(defun mst--render-section (parsed-lexeme context)
  "Given PARSED-LEXEME -- a lexed tag, plain text, or a nested list,
render it in CONTEXT."
  (cond ((mst--section-p parsed-lexeme)
         ;; nested section
         (let* ((section-tag (first parsed-lexeme))
                (section-name (mst--section-name section-tag))
                (context-value (mst--context-get context section-name))
                ;; strip section open and close
                (section-contents (-slice parsed-lexeme 1 -1)))
           (cond
            ((mst--open-section-tag-p section-tag)
             (cond
              ;; if the context is a list of hash tables, render repeatedly
              ((or (mst--listp context-value) (vectorp context-value))
               (mst--amapconcat (mst--render-section-list section-contents (mst--context-add context it)) context-value))
              ;; if the context is a hash table, render in that context
              ((hash-table-p context-value)
               (mst--render-section-list section-contents (mst--context-add context context-value)))
              ;; if the context is a function, call it
              ((functionp context-value)
               (funcall context-value (mst--unlex section-contents) context))
              ;; if it's a truthy value, render in the current context
              (context-value
               (mst--render-section-list section-contents context))
              ;; otherwise, don't render anything
              (t "")))
            ;; render ^tags
            ((mst--inverted-section-tag-p section-tag)
             (if context-value
                 ""
               (mst--render-section-list section-contents context))))))
        ((mst--tag-p parsed-lexeme)
         (mst--render-tag parsed-lexeme context))
        ;; plain text
        (t
         (second parsed-lexeme))))

(defun mst--escape-html (string)
  "Escape HTML in STRING."
  (->> string
    (s-replace "&" "&amp;")
    (s-replace "<" "&lt;")
    (s-replace ">" "&gt;")
    (s-replace "'" "&#39;")
    (s-replace "\"" "&quot;")))
