;;; ox-blackfriday.el --- Blackfriday Markdown Back-End for Org Export Engine  -*- lexical-binding: t -*-

;; Authors: Matt Price <moptop99@gmail.com>
;;          Kaushal Modi <kaushal.modi@gmail.com>
;; URL: https://ox-hugo.scripter.co
;; Package-Requires: ((emacs "24.5"))
;; Version: 0.1

;;; Commentary:

;; This library implements a Markdown back-end (Blackfriday flavor
;; (https://github.com/russross/blackfriday)) for Org exporter, based
;; on the ox-md exporter.

;; It started off as a clone of Lars Tveito's GitHub Flavored Markdown
;; exporter (https://github.com/larstvei/ox-gfm).

;;; Code:

(require 'ox-md)
(require 'ox-publish)
(require 'table)         ;To support tables written in table.el format


;;; Variables

(defvar org-blackfriday-width-cookies nil)
(defvar org-blackfriday-width-cookies-table nil)

(defconst org-blackfriday-table-left-border "")
(defconst org-blackfriday-table-right-border " ")
(defconst org-blackfriday-table-separator "| ")

(defconst org-blackfriday-html5-inline-elements
  '("audio" "details" "command" "datalist" "mark" "meter"
    "nav" "source" "summary" "time")
  "New inline elements in html5.

http://itman.in/en/inline-elements-html5/.")

(defvar org-blackfriday--hrule-inserted nil
  "State variable to track if the horizontal rule was inserted.
This check is specifically track if that horizontal rule was
inserted after the first row of the table.")

(defvar org-blackfriday--code-block-num-backticks-default 3
  "Variable to store the default number of backticks for code block.

Note that this variable is *only* for internal use.")

(defvar org-blackfriday--code-block-num-backticks org-blackfriday--code-block-num-backticks-default
  "Variable to store the number of backticks for code block.
By default, it stays at 3.  This number is incremented for few
corner cases.

Note that this variable is *only* for internal use.")

(defvar org-blackfriday--org-element-string '((src-block . "Code Snippet")
                                              (table . "Table")
                                              (figure . "Figure")) ;Note that `figure' is not an actual Org element
  "Alist of strings used to represent various Org elements.")



;;; User-Configurable Variables

(defgroup org-export-blackfriday nil
  "Options for exporting Org mode files to Blackfriday Markdown."
  :tag "Org Export Blackfriday"
  :group 'org-export)



;;; Define Back-End

(org-export-define-derived-backend 'blackfriday 'md
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  ;; Do not clutter the *Org Exporter Dispatch* menu.
  ;; :menu-entry
  ;; '(?b "Export to Blackfriday Flavored Markdown"
  ;;      ((?B "To temporary buffer"
  ;;           (lambda (a s v b) (org-blackfriday-export-as-markdown a s v)))
  ;;       (?b "To file" (lambda (a s v b) (org-blackfriday-export-to-markdown a s v)))
  ;;       (?o "To file and open"
  ;;           (lambda (a s v b)
  ;;             (if a (org-blackfriday-export-to-markdown t s v)
  ;;               (org-open-file (org-blackfriday-export-to-markdown nil s v)))))))
  :translate-alist '((center-block . org-blackfriday-center-block)
                     (example-block . org-blackfriday-example-block)
                     (fixed-width . org-blackfriday-fixed-width) ;Org Babel Results
                     (footnote-reference . org-blackfriday-footnote-reference)
                     (inner-template . org-blackfriday-inner-template)
                     (italic . org-blackfriday-italic)
                     (item . org-blackfriday-item)
                     (latex-environment . org-blackfriday-latex-environment)
                     (latex-fragment . org-blackfriday-latex-fragment)
                     (line-break . org-html-line-break) ;"\\" at EOL forces a line break
                     (plain-list . org-blackfriday-plain-list)
                     (plain-text . org-blackfriday-plain-text)
                     (quote-block . org-blackfriday-quote-block)
                     (special-block . org-blackfriday-special-block)
                     (src-block . org-blackfriday-src-block)
                     (strike-through . org-blackfriday-strike-through)
                     (table-cell . org-blackfriday-table-cell)
                     (table-row . org-blackfriday-table-row)
                     (table . org-blackfriday-table)
                     (verse-block . org-blackfriday-verse-block)))


;;; Miscellaneous Helper Functions

;;;; Table of contents
(defun org-blackfriday-format-toc (headline info)
  "Return an appropriate table of contents entry for HEADLINE.

INFO is a plist used as a communication channel."
  (let* ((title (org-export-data (org-export-get-alt-title headline info) info))
         (level (1- (org-element-property :level headline)))
         (indent (concat (make-string (* level 2) ? )))
         (anchor (or (org-element-property :CUSTOM_ID headline)
                     (org-export-get-reference headline info))))
    (concat indent "- [" title "]" "(#" anchor ")")))

;;;; Footnote section
(defun org-blackfriday-footnote-section (info &optional is-cjk)
  "Format the footnote section.

INFO is a plist used as a communication channel.
IS-CJK should be set to non-nil if the language is Chinese,
Japanese or Korean."
  (let ((fn-alist (org-export-collect-footnote-definitions info))
        ;; Fri Jul 21 14:33:25 EDT 2017 - kmodi
        ;; TODO: Need to learn using cl-loop
        ;; Below form from ox-md did not work.
        ;; (fn-alist-stripped
        ;;  (cl-loop for (n raw) in fn-alist collect
        ;;           (cons n (org-trim (org-export-data raw info)))))
        fn-alist-stripped)
    (let ((n 1)
          def)
      (dolist (fn fn-alist)
        ;; (message "fn: %S" fn)
        ;; (message "fn: %s" (org-export-data fn info)) ;This gives error
        ;; (message "fn nth 2 car: %s" (org-export-data (nth 2 fn) info))
        (setq def (org-trim (org-export-data (nth 2 fn) info)))
        ;; Support multi-line footnote definitions by folding all
        ;; footnote definition lines into a single line as Blackfriday
        ;; does not support that.
        (setq def (if is-cjk
                      (replace-regexp-in-string
                       "\n" " " ;If the footnote still has newlines, replace them with spaces
                       (replace-regexp-in-string
                        ;; Do not insert spaces when joining newlines for
                        ;; CJK languages.
                        "\\([[:multibyte:]]\\)[[:blank:]]*\n[[:blank:]]*\\([[:multibyte:]]\\)" "\\1\\2"
                        def))
                    (replace-regexp-in-string "\n" " " def)))

        ;; Replace multiple consecutive spaces with a single space.
        (setq def (replace-regexp-in-string "[[:blank:]]+" " " def))
        (push (cons n def) fn-alist-stripped)
        (setq n (1+ n))))
    (when fn-alist-stripped
      (mapconcat (lambda (fn)
                   ;; (message "dbg: fn: %0d -- %s" (car fn) (cdr fn))
                   (format "[^fn:%d]: %s"
                           (car fn)     ;footnote number
                           (cdr fn)))   ;footnote definition
                 (nreverse fn-alist-stripped)
                 "\n"))))

;;;; Table-Common
(defun org-blackfriday-table-col-width (table column info)
  "Return width of TABLE at given COLUMN using INFO.

INFO is a plist used as communication channel.  Width of a column
is determined either by inquiring `org-blackfriday-width-cookies'
in the column, or by the maximum cell with in the column."
  (let ((cookie (when (hash-table-p org-blackfriday-width-cookies)
                  (gethash column org-blackfriday-width-cookies))))
    (if (and (eq table org-blackfriday-width-cookies-table)
             (not (eq nil cookie)))
        cookie
      (unless (and (eq table org-blackfriday-width-cookies-table)
                   (hash-table-p org-blackfriday-width-cookies))
        (setq org-blackfriday-width-cookies (make-hash-table))
        (setq org-blackfriday-width-cookies-table table))
      (let ((max-width 0)
            (specialp (org-export-table-has-special-column-p table)))
        (org-element-map
            table
            'table-row
          (lambda (row)
            (setq max-width
                  (max (length
                        (org-export-data
                         (org-element-contents
                          (elt (if specialp
                                   (car (org-element-contents row))
                                 (org-element-contents row))
                               column))
                         info))
                       max-width)))
          info)
        (puthash column max-width org-blackfriday-width-cookies)))))

;;;; Plain List Helper
(defun org-blackfriday--ordered-list-with-custom-counter-p (plain-list)
  "Return non-nil is PLAIN-LIST element has an item with custom counter.
Returns nil immediately if PLAIN-LIST is not an ordered list."
  (let ((type (org-element-property :type plain-list))
        has-custom-counter)
    (when (eq 'ordered type)
      (let* ((list-contents (org-element-contents plain-list)))
        (dolist (el list-contents)
          (when (eq 'item (car el))
            (let* ((item-plist (car (cdr el)))
                   (counter (plist-get item-plist :counter)))
              ;; (message "dbg: %S" counter)
              (when counter
                (setq has-custom-counter t)))))))
    ;; (message "has custom counter: %S" has-custom-counter)
    has-custom-counter))

;;;; Table Cell Alignment
;; Below function is heavily adapted from
;; `org-export-table-cell-alignment' from ox.el.  The main difference
;; is that the below variation can return a `default' value too.
(defun org-blackfriday-table-cell-alignment (table-cell info)
  "Return TABLE-CELL contents alignment.

INFO is a plist used as the communication channel.

Return alignment as specified by the last alignment cookie in the
same column as TABLE-CELL.  If no such cookie is found, return
`default'.  Possible values are `default', `left', `right' and
`center'."
  (let* ((row (org-export-get-parent table-cell))
         (table (org-export-get-parent row))
         (cells (org-element-contents row))
         (columns (length cells))
         (column (- columns (length (memq table-cell cells))))
         (cache (or (plist-get info :table-cell-alignment-cache)
                    (let ((table (make-hash-table :test #'eq)))
                      (plist-put info :table-cell-alignment-cache table)
                      table)))
         (align-vector (or (gethash table cache)
                           (puthash table (make-vector columns nil) cache))))
    (or (aref align-vector column)
        (let (cookie-align)
          (dolist (row (org-element-contents (org-export-get-parent row)))
            (cond
             ;; In a special row, try to find an alignment cookie at
             ;; COLUMN.
             ((org-export-table-row-is-special-p row info)
              (let ((value (org-element-contents
                            (elt (org-element-contents row) column))))
                ;; Since VALUE is a secondary string, the following
                ;; checks avoid useless expansion through
                ;; `org-export-data'.
                (when (and value
                           (not (cdr value))
                           (stringp (car value))
                           (string-match "\\`<\\([lrc]\\)?\\([0-9]+\\)?>\\'"
                                         (car value))
                           (match-string 1 (car value)))
                  (setq cookie-align (match-string 1 (car value))))))
             ;; Ignore table rules.
             ((eq (org-element-property :type row) 'rule))))
          ;; Return value.  Alignment specified by cookies has
          ;; precedence over alignment deduced from cell's contents.
          (aset align-vector
                column
                (cond ((equal cookie-align "l") 'left)
                      ((equal cookie-align "r") 'right)
                      ((equal cookie-align "c") 'center)
                      (t 'default)))))))

;;;; Escape certain characters inside equations (Blackfriday bug workaround)
(defun org-blackfriday-escape-chars-in-equation (str)
  "Escape few characters in STR so that Blackfriday doesn't parse them.

Do not interpret underscores and asterisks in equations as
Markdown formatting
characters (https://gohugo.io/content-management/formats#solution):

  \"_\" -> \"\\=\\_\"
  \"*\" -> \"\\=\\*\"

https://github.com/kaushalmodi/ox-hugo/issues/104

Blackfriday converts \"(r)\" to Registered Trademark symbol,
\"(c)\" to Copyright symbol, and \"(tm)\" to Trademark symbol if
the SmartyPants extension is enabled (and there is no way to
disable just this).  So insert an extra space after the opening
parentheses in those strings to trick Blackfriday/smartParens
from activating inside equations.  That extra space anyways
doesn't matter in equations.

  \"(c)\" -> \"( c)\"
  \"(r)\" -> \"( r)\"
  \"(tm)\" -> \"( tm)\"

https://gohugo.io/content-management/formats#solution
https://github.com/kaushalmodi/ox-hugo/issues/138

Need to escape the backslash in \"\\(\", \"\\)\", .. to make
Blackfriday happy.  So:

  \"\\(\" -> \"\\\\(\"
  \"\\)\" -> \"\\\\)\"
  \"\\\\=[\" -> \"\\\\\\=[\"
  \"\\\\=]\" -> \"\\\\\\=]\"
  \"\\\\={\" -> \"\\\\\\={\"
  \"\\\\=}\" -> \"\\\\\\=}\"
  \"\\|\" -> \"\\\\|\"

  \"](\" -> \"\\]\\(\"

and finally:

  \"\\\\\" -> \"\\\\\\\\\\\\\"."
  (let* (;; _ -> \_, * -> \*
         (escaped-str (replace-regexp-in-string "[_*]" "\\\\\\&" str))
         ;; (c) -> ( c), (r) -> ( r), (tm) -> ( tm)
         (escaped-str (replace-regexp-in-string "(\\(c\\|r\\|tm\\))" "( \\1)" escaped-str))
         ;; \( -> \\(, \) -> \\), \[ -> \\[, \] -> \\], \{ -> \\{, \} -> \\}, \| -> \\|
         (escaped-str (replace-regexp-in-string "\\(\\\\[](){}[|]\\)" "\\\\\\1" escaped-str))
         ;; ]( -> \]\(
         (escaped-str (replace-regexp-in-string "](" "\\\\]\\\\(" escaped-str))
         (escaped-str (replace-regexp-in-string
                       "\\([^\\]\\)\\\\\\{2\\}[[:blank:]]*$" ;Replace "\\" at EOL with:
                       "\\1\\\\\\\\\\\\\\\\\\\\\\\\"             ;"\\\\\\"
                       escaped-str)))
    escaped-str))

;;;; Reset org-blackfriday--code-block-num-backticks
(defun org-blackfriday--reset-org-blackfriday--code-block-num-backticks (_backend)
  "Reset `org-blackfriday--code-block-num-backticks' to its default value."
  (setq org-blackfriday--code-block-num-backticks org-blackfriday--code-block-num-backticks-default))
(add-hook 'org-export-before-processing-hook #'org-blackfriday--reset-org-blackfriday--code-block-num-backticks)

;;;; Make CSS property string
(defun org-blackfriday--make-css-property-string (props)
  "Return a list of CSS properties, as a string.
PROPS is a plist where values are either strings or nil.  A prop
with a nil value will be omitted from the result.

This function is adapted from `org-html--make-attribute-string'."
  (let (ret)
    (dolist (item props (mapconcat #'identity (nreverse ret) " "))
      (cond ((null item)
             (pop ret))
            ((symbolp item)
             (push (substring (symbol-name item) 1) ret))
            (t
             (let ((key (car ret))
                   (value (replace-regexp-in-string
                           "\"" "&quot;" (org-html-encode-plain-text item))))
               (setcar ret (format "%s: %s; " key value))))))))

;;;; Wrap with HTML attributes
(defun org-blackfriday--div-wrap-maybe (elem contents)
  "Wrap the CONTENTS with HTML div tags.

The div wrapping is done only if HTML attributes are set for the
ELEM Org element using #+attr_html.

If #+attr_css is also used, and if a class is specified in
#+attr_html, then an inline style is also inserted that applies
the specified CSS to that class.

If CONTENTS is nil, and #+attr_css is used, return only the HTML
style tag."
  (let* ((elem-type (org-element-type elem))
         (attr (let ((attr1 (org-export-read-attribute :attr_html elem)))
                 (when (equal elem-type 'paragraph)
                   ;; Remove "target" and "rel" attributes from the
                   ;; list of a paragraph's HTML attributes as they
                   ;; would be meant for links inside the paragraph
                   ;; instead of the paragraph itself.
                   (plist-put attr1 :target nil)
                   (plist-put attr1 :rel nil)
                   ;; Remove other attributes from the list of a
                   ;; paragraph's HTML attributes which would be meant
                   ;; for the inline images inside that paragraph.
                   (plist-put attr1 :src nil)
                   (plist-put attr1 :alt nil)
                   (plist-put attr1 :height nil)
                   (plist-put attr1 :width nil))
                 attr1))
         (attr-str (org-html--make-attribute-string attr))
         (ret contents))
    (when (org-string-nw-p attr-str)
      (let ((class (plist-get attr :class))
            (style-str ""))
        (when class
          (let* ((css-props (org-export-read-attribute :attr_css elem))
                 (css-props-str (org-blackfriday--make-css-property-string css-props)))
            (when (org-string-nw-p css-props-str)
              (setq style-str (format "<style>.%s { %s }</style>\n\n"
                                      class css-props-str)))))

        (setq ret (concat style-str
                          (if contents
                              (format "<div %s>\n  <div></div>\n\n%s\n</div>" ;See footnote 1
                                      attr-str contents)
                            "")))))
    ret))

;;;; Sanitize URL
(defun org-blackfriday--url-sanitize (url)
  "Sanitize the URL by replace certain characters with their hex encoding.

Replaces \"_\" with \"%5F\".

Workaround for Blackfriday bug https://github.com/russross/blackfriday/issues/278."
  (replace-regexp-in-string "_" "%5F" url))

;;;; Blackfriday Issue 239 Workaround
(defun org-blackfriday--issue-239-workaround (code parent-type)
  "Prefix Markdown list characters with zero width space.

CODE is the content of the source or example block.  PARENT-TYPE
is the type of the Org element wrapping that source or example
block.

Hack to avert the Blackfriday bug:
https://github.com/russross/blackfriday/issues/239.  Remove this
hack once that issue is resolved.

Prefix the ASTERISK (0x2a), PLUS SIGN (0x2b) and HYPHEN-MINUS
\(0x2d) characters with ZERO WIDTH SPACE (0x200b), if they
appear at BOL (following optional spaces).

Details: https://github.com/kaushalmodi/ox-hugo/issues/57."
  ;; (message "[ox-bf bfissue 239 DBG] parent type: %S" parent-type)
  (if (equal 'item parent-type)
      (setq code (replace-regexp-in-string "^\\s-*[-+*] " "​\\&" code))
    ;; There's a ZERO WIDTH SPACE char (0x200b) here     ^^,
    ;;                            (after «"», but before «\\&"» above)
    ;; It's not visible (because zero width), but it's there.
    code))

;;;; Get Reference
(defun org-blackfriday--get-reference (elem)
  "Return a reference for ELEM using its \"#+name\" if available.

If the ELEM has its `name' defined, the anchor is derived from it:

- If the `name' begins with \"code__\", \"tab__\", \"table__\",
  \"fig__\" or \"figure__\", that prefix is removed as this
  function adds its own appropriate prefix.
- Underscores and forward slashes in the `name' get replaced with
  hyphens.

This conditioned `name' is then appended to the
code/table/figure-appropriate prefix, and returned.

Else, return nil.

The return value, if non-nil, is a string."
  (let ((name (org-element-property :name elem))) ;Value of #+name
    ;; Reference cannot be created if #+name does not exist.
    ;; (message "[ox-bf ref DBG] name: %S" name)
    (when name
      (let* ((elem-type (org-element-type elem))
             (prefix (if (assoc elem-type org-blackfriday--org-element-string)
                         (let ((type-str (cdr (assoc elem-type org-blackfriday--org-element-string))))
                           (replace-regexp-in-string " " "-"
                                                     (downcase type-str)))
                       (format "org-%s" (symbol-name elem-type))))
             (name1 (let* ((tmp name)
                           ;; Remove commonly used code/table/figure
                           ;; prefixes in the #+name itself.
                           (tmp (replace-regexp-in-string "\\`\\(code\\|tab\\|table\\|fig\\|figure\\|\\)__" "" tmp))
                           ;; Prefer to use hyphens instead of
                           ;; underscores in anchors.  Also replace /
                           ;; chars with hyphens.
                           (tmp (replace-regexp-in-string "[_/]" "-" tmp)))
                      tmp)))
        (format "%s--%s" prefix name1)))))

;;;; Translate
(defun org-blackfriday--translate (type info &optional str)
  "Return translated string for element TYPE to the lang set by \"#+language\".

TYPE is the Org element type.

INFO is a plist holding contextual information.

If TYPE is `src-block' and if \"Listing\" translates to
\"Listing\", translate the string associated with `src-block'
from `org-blackfriday--org-element-string'.

Else if TYPE key exists in `org-blackfriday--org-element-string',
return the translated version of of the string associated in that
alist.

Else if TYPE key does not exist in
`org-blackfriday--org-element-string', or if TYPE is nil, but STR
is non-nil, return the translation of STR directly.

Else return an empty string."
  (let ((elem-str (cdr (assoc type org-blackfriday--org-element-string))))
    (if elem-str
        (cond
         ((equal 'src-block type)
          (let ((listing-tr (org-html--translate "Listing" info)))
            (if (string= "Listing" listing-tr)
                (org-html--translate elem-str info)
              listing-tr)))
         (t
          (org-html--translate elem-str info)))
      (if (stringp str)
          (org-html--translate str info)
        ""))))



;;; Transcode Functions

;;;; Center Block
(defun org-blackfriday-center-block (_center-block contents _info)
  "Center-align the text in CONTENTS using CSS."
  (let* ((class "org-center")
         (style (format ".%s { margin-left: auto; margin-right: auto; text-align: center; }" class)))
    (format "<style>%s</style>\n\n<div class=\"%s\">\n  <div></div>\n\n%s\n</div>" ;See footnote 1
            style class contents)))

;;;; Example Block
(defun org-blackfriday-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element into Blackfriday Markdown format.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let* ((parent-element (org-export-get-parent example-block))
         (parent-type (car parent-element))
         (backticks (make-string org-blackfriday--code-block-num-backticks ?`))
         (example (org-export-format-code-default example-block info))
         ret)
    ;; (message "[ox-bf example-block DBG]")
    ;; (message "[ox-bf example-block DBG] parent type: %S" parent-type)
    (setq ret (org-blackfriday--issue-239-workaround example parent-type))
    (setq ret (format "%stext\n%s%s" backticks ret backticks))
    (setq ret (org-blackfriday--div-wrap-maybe example-block ret))
    (when (equal 'quote-block parent-type)
      ;; If the current example block is inside a quote block, future
      ;; example/code blocks (especially the ones outside this quote
      ;; block) will require higher number of backticks.  Workaround
      ;; for https://github.com/russross/blackfriday/issues/407.
      (setq org-blackfriday--code-block-num-backticks
            (1+ org-blackfriday--code-block-num-backticks)))
    ret))

;;;; Fixed Width
(defun org-blackfriday-fixed-width (fixed-width _contents info)
  "Transcode a FIXED-WIDTH element into Blackfriday Markdown format.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let* ((parent-element (org-export-get-parent fixed-width))
         (parent-type (car parent-element))
         (backticks (make-string org-blackfriday--code-block-num-backticks ?`)))
    (prog1
        (org-blackfriday--div-wrap-maybe
         fixed-width
         (format "%stext\n%s%s"
                 backticks
                 (let ((org-src-preserve-indentation t))
                   ;; Preserve leading whitespace in the Org Babel Results
                   ;; blocks.
                   (org-export-format-code-default fixed-width info))
                 backticks))
      (when (equal 'quote-block parent-type)
        ;; If the current example block is inside a quote block,
        ;; future example/code blocks (especially the ones outside
        ;; this quote block) will require higher number of backticks.
        ;; Workaround for
        ;; https://github.com/russross/blackfriday/issues/407.
        (setq org-blackfriday--code-block-num-backticks
              (1+ org-blackfriday--code-block-num-backticks))))))

;;;; Footnote Reference
(defun org-blackfriday-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element into Blackfriday Markdown format.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; (message "footref: %s" footnote-reference)
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (and (eq (org-element-type prev) 'footnote-reference)
          (plist-get info :html-footnote-separator)))
   (format "[^fn:%d]" (org-export-get-footnote-number footnote-reference info))))

;;;; Inner Template
(defun org-blackfriday-inner-template (contents info)
  "Return body of document after converting it to Markdown syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((depth (plist-get info :with-toc))
         (headlines (and depth (org-export-collect-headlines info depth)))
         (toc-tail (if headlines "\n\n" ""))
         (toc-string ""))

    (when headlines
      (dolist (headline headlines)
        (setq toc-string (concat toc-string
                                 (org-blackfriday-format-toc headline info)
                                 "\n"))))
    (org-trim (concat toc-string toc-tail contents "\n" (org-blackfriday-footnote-section info)))))

;;;; Italic
(defun org-blackfriday-italic (_italic contents _info)
  "Transcode ITALIC object into Markdown format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  ;; (format "*%s*" contents)
  ;; While above also works in almost all cases, it fails in cases
  ;; like "*This is in italic, **and this is in bold-italics**, and
  ;; back to just italic.*".
  ;; As `org-md-bold' uses ** to mark bold text, switching to using
  ;; underscores only for italics.
  (format "_%s_" contents))

;;;; Item (list item)
(defun org-blackfriday-item (item contents info)
  "Transcode an ITEM element into Blackfriday Markdown format.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information.

Special note about descriptive lists:

Blackfriday style descriptive list syntax is used if that list is
not nested in another list.

    Term1
    : Description of term 1

If that list is nested, `ox-md' style descriptive list is
exported instead:

    -   **Term1:** Description of term 1."
  (let* ((parent-list (org-export-get-parent item)))
    ;; If this item is in an ordered list and if this or any other
    ;; item in this list is using a custom counter, export this list
    ;; item in HTML.
    (if (org-blackfriday--ordered-list-with-custom-counter-p parent-list)
        (org-html-format-list-item contents 'ordered nil info
                                   (org-element-property :counter item))
      (let* ((parent-list (org-export-get-parent item))
             (parent-list-type (org-element-property :type parent-list))
             (desc-list? (eq parent-list-type 'descriptive))
             (grandparent (when desc-list?
                            (org-export-get-parent parent-list)))
             (grandparent-type (when desc-list?
                                 (org-element-type grandparent)))
             (list-is-nested (eq 'item grandparent-type))
             ;; Export the descriptive list items like that in
             ;; ox-md.el if this descriptive list is nested in some
             ;; other list, because the Blackfriday style descriptive
             ;; list syntax seems to work only at top level (i.e. not
             ;; when that list is nested).
             (ox-md-style-desc-list (and desc-list? list-is-nested))
             (bf-style-desc-list (and desc-list? (not list-is-nested)))
             (struct (org-element-property :structure item))
             (item-num (car (last (org-list-get-item-number
                                   (org-element-property :begin item)
                                   struct
                                   (org-list-prevs-alist struct)
                                   (org-list-parents-alist struct)))))
             (bullet (cond
                      ((or (eq parent-list-type 'unordered)
                           ox-md-style-desc-list)
                       "-")
                      ((eq parent-list-type 'ordered)
                       (format "%d." item-num))
                      (t             ;Non-nested descriptive list item
                       (when (> item-num 1)
                         "\n")))) ;Newline between each descriptive list item
             (padding (unless bf-style-desc-list
                        (make-string (- 4 (length bullet)) ? )))
             (tag (when desc-list?
                    (let* ((tag1 (org-element-property :tag item))
                           (tag1-str (org-export-data tag1 info)))
                      (when tag1
                        (if ox-md-style-desc-list
                            (format "**%s:** " tag1-str)
                          (format "%s\n: " tag1-str)))))))
        (concat bullet
                padding
                (pcase (org-element-property :checkbox item)
                  (`on "[X] ")
                  (`trans "[-] ")
                  (`off "[ ] "))
                tag
                (and contents
                     (org-trim (replace-regexp-in-string "^" "    " contents))))))))

;;;; Latex Environment
(defun org-blackfriday-latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT object into Blackfriday Markdown format.
INFO is a plist holding contextual information."
  (let ((processing-type (plist-get info :with-latex)))
    ;; (message "[ox-bf-processing-type DBG] processing-type: %s" processing-type)
    (cond
     ((memq processing-type '(t mathjax))
      (let* ((latex-env (org-remove-indentation
                         (org-element-property :value latex-environment)))
             (env (org-html-format-latex latex-env 'mathjax info))
             (env (org-blackfriday-escape-chars-in-equation env)))
        ;; (message "[ox-bf-latex-env DBG] latex-env: %s" latex-env)
        ;; (message "[ox-bf-latex-env DBG] env: %s" env)
        env))
     (t
      (org-html-latex-environment latex-environment nil info)))))

;;;; Latex Fragment
(defun org-blackfriday-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object into Blackfriday Markdown format.
INFO is a plist holding contextual information."
  (let ((processing-type (plist-get info :with-latex)))
    (cond
     ((memq processing-type '(t mathjax))
      (let* ((latex-frag (org-element-property :value latex-fragment))
             (frag (org-html-format-latex latex-frag 'mathjax info))
             (frag (org-blackfriday-escape-chars-in-equation frag)))
        ;; (message "[ox-bf-latex-frag DBG] frag: %s" frag)
        frag))
     (t
      (org-html-latex-fragment latex-fragment nil info)))))

;;;; Plain List
(defun org-blackfriday-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element into Blackfriday Markdown format.
CONTENTS is the plain-list contents.  INFO is a plist used as a
communication channel."
  (let (ret)
    (if (org-blackfriday--ordered-list-with-custom-counter-p plain-list)
        ;; If this is an ordered list and if any item in this list is
        ;; using a custom counter, export this list in HTML.
        (setq ret (concat
                   (org-blackfriday--div-wrap-maybe plain-list nil)
                   (org-html-plain-list plain-list contents info)))
      (let* ((next (org-export-get-next-element plain-list info))
             (next-type (org-element-type next)))
        ;; (message "content: `%s', next type: %s" contents next-type)
        (setq ret (org-blackfriday--div-wrap-maybe plain-list contents))
        (when (member next-type '(plain-list
                                  src-block example-block)) ;https://github.com/russross/blackfriday/issues/556
          (setq ret (concat ret "\n<!--listend-->")))))
    ret))

;;;; Plain Text
(defun org-blackfriday-plain-text (text info)
  "Transcode TEXT element into Blackfriday Markdown format.
TEXT is the string to transcode.  INFO is a plist used as a
communication channel.

This function is almost same as `org-md-plain-text' except it
first escapes any existing \"\\\", and then escapes other string
matches with \"\\\" as needed."
  (when (plist-get info :with-smart-quotes)
    (setq text (org-export-activate-smart-quotes text :html info)))
  ;; The below series of replacements in `text' is order sensitive.
  ;; Protect `, *, _, and \
  (setq text (replace-regexp-in-string "[`*_\\]" "\\\\\\&" text))
  ;; Protect ambiguous #.  This will protect # at the beginning of
  ;; a line, but not at the beginning of a paragraph.  See
  ;; `org-md-paragraph'.
  (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
  ;; Protect ambiguous !
  (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
  ;; Handle special strings, if required.
  (when (plist-get info :with-special-strings)
    (setq text (org-html-convert-special-strings text)))
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  ;; Return value.
  text)

;;;; Quote Block
(defun org-blackfriday-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK element into Blackfriday Markdown format.
CONTENTS is the quote-block contents.  INFO is a plist used as a
communication channel."
  (let* ((next (org-export-get-next-element quote-block info))
         (next-type (org-element-type next))
         (next-is-quote (eq 'quote-block next-type))
         (contents (org-md-quote-block quote-block contents info))
         ret)
    ;; (message "[ox-bf quote-block DBG]")
    (setq ret (org-blackfriday--div-wrap-maybe quote-block contents))
    (setq ret (concat ret
                      ;; Two consecutive blockquotes in Markdown can be
                      ;; separated by a comment.
                      (when next-is-quote
                        "\n\n<!--quoteend-->")))
    ret))

;;;; Special Block
(defun org-blackfriday-special-block (special-block contents _info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.

This function is adapted from `org-html-special-block'."
  (let* ((block-type (org-element-property :type special-block))
         (html5-inline-fancy (member block-type org-blackfriday-html5-inline-elements))
         (html5-block-fancy (member block-type org-html-html5-elements))
         (html5-fancy (or html5-inline-fancy html5-block-fancy))
         (attributes (org-export-read-attribute :attr_html special-block)))
    (unless html5-fancy
      (let ((class (plist-get attributes :class)))
        (setq attributes (plist-put attributes :class
                                    (if class
                                        (concat class " " block-type)
                                      block-type)))))
    (let* ((contents (or contents ""))
           ;; If #+name is specified, use that for the HTML element
           ;; "id" attribute.
           (name (org-element-property :name special-block))
           (attr-str (org-html--make-attribute-string
                      (if (or (not name) (plist-member attributes :id))
                          attributes
                        (plist-put attributes :id name))))
           (attr-str (if (org-string-nw-p attr-str)
                         (concat " " attr-str)
                       "")))
      (cond
       (html5-inline-fancy
        (cond
         ((string= block-type "details")
          ;; Recognize Org Special blocks like:
          ;;   #+begin_details
          ;;   #+begin_summary
          ;;   This is summary.
          ;;   #+end_summary
          ;;   Here are the details.
          ;;   #+end_details
          (let ((p-open "<p class=\"details\">"))
            (setq contents
                  (concat
                   ;; Wrap the "details" portion in the <details> tag
                   ;; with '<p class="details"> .. </p>'.  With that,
                   ;; CSS rules can be set specific to that details
                   ;; portion using "details .details".
                   (if (string-match "\\(?1:<summary>\\(?:.\\|\n\\)*</summary>\\)" contents) ;If summary exists
                       (replace-match (format "\\1\n%s" p-open) nil nil contents 1)
                     (concat p-open contents))
                   ;; A newline is inserted before the closing </p>
                   ;; tag for the reason explained below using the
                   ;; emacs-lisp Markdown code block.
                   "\n</p>")))
          ;; Insert the "open" attribute only if user has ":open t" in
          ;; "#+attr_html".
          (when (org-string-nw-p attr-str)
            (when (string-match "\\(?1:open\\(?2:=\"\\(?3:t\\)\"\\)\\)" attr-str)
              (if (match-string 3 attr-str) ;if attr-str contains `open="t"'
                  (setq attr-str (replace-match "" nil nil attr-str 2))
                (setq attr-str (replace-match "" nil nil attr-str 1)))))))
        ;; Insert a newline before and after the `contents' to handle
        ;; the cases where that could begin or end with a Markdown
        ;; blocks like:
        ;;   ```emacs-lisp
        ;;   (message "foo")
        ;;   ```
        ;; An example scenario would be where such content could be
        ;; present in the "inline" <details> or <summary> Special
        ;; Blocks.
        ;; Without those newlines, the Markdown converted content will
        ;; look like below, and Blackfriday won't parse it correctly.
        ;;   <details>```emacs-lisp
        ;;   (message "foo")
        ;;   ```</details>
        (format "<%s%s>\n%s\n</%s>"
                block-type attr-str contents block-type))
       (html5-block-fancy
        (format "<%s%s>\n  <%s></%s>\n\n%s\n\n</%s>" ;See footnote 1
                block-type attr-str block-type block-type contents block-type))
       (t
        (format "<div%s>\n  <div></div>\n\n%s\n\n</div>" ;See footnote 1
                attr-str contents))))))

;;;; Src Block
(defun org-blackfriday-src-block (src-block _contents info)
  "Transcode SRC-BLOCK element into Blackfriday Markdown format.

INFO is a plist used as a communication channel."
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (parent-element (org-export-get-parent src-block))
         (parent-type (car parent-element))
         (num-backticks-in-code (when (string-match "^[[:blank:]]*\\(`\\{3,\\}\\)" code)
                                  (length (match-string-no-properties 1 code))))
         backticks)
    ;; In order to show the code-fence backticks in a code-fenced code
    ;; block, you need to have the wrapping code fence to have at
    ;; least 1 more backtick in the fence compared to those in the
    ;; being-wrapped code fence. This example will explain better:
    ;;
    ;;   ````md
    ;;   ```emacs-lisp
    ;;   (message "Hello")
    ;;   ```
    ;;   ````
    (when (and (numberp num-backticks-in-code)
               (<= org-blackfriday--code-block-num-backticks num-backticks-in-code))
      (setq org-blackfriday--code-block-num-backticks (1+ num-backticks-in-code)))
    (setq backticks (make-string org-blackfriday--code-block-num-backticks ?`))
    ;; (message "[ox-bf src-block DBG]")
    ;; (message "ox-bf [dbg] code: %s" code)
    ;; (message "[ox-bf src-block DBG] parent type: %S" parent-type)
    (setq code (org-blackfriday--issue-239-workaround code parent-type))
    (prog1
        (format "%s%s\n%s%s" backticks lang code backticks)
      (when (equal 'quote-block parent-type)
        ;; If the current code block is inside a quote block, future
        ;; example/code blocks (especially the ones outside this quote
        ;; block) will require higher number of backticks.  Workaround
        ;; for https://github.com/russross/blackfriday/issues/407.
        (setq org-blackfriday--code-block-num-backticks
              (1+ org-blackfriday--code-block-num-backticks))))))

;;;; Strike-Through
(defun org-blackfriday-strike-through (_strike-through contents _info)
  "Transcode strike-through text into Blackfriday Markdown format.
CONTENTS contains the text with strike-through markup."
  (format "~~%s~~" contents))

;;;; Table-Cell
(defun org-blackfriday-table-cell (table-cell contents info)
  "Transcode TABLE-CELL element into Blackfriday Markdown format.

CONTENTS is content of the cell.  INFO is a plist used as a
communication channel."
  ;; (message "[ox-bf-table-cell DBG]")
  ;; (message "[ox-bf-table-cell DBG] In contents: %s" contents)
  (let* ((table (org-export-get-parent-table table-cell))
         (column (cdr (org-export-table-cell-address table-cell info)))
         (width (org-blackfriday-table-col-width table column info))
         (left-border (if (org-export-table-cell-starts-colgroup-p table-cell info) "| " " "))
         (right-border " |")
         (data (or contents ""))
         (cell (concat left-border
                       data
                       (make-string (max 0 (- width (string-width data))) ?\s)
                       right-border))
         (cell-width (length cell)))
    ;; Just calling `org-blackfriday-table-cell-alignment' will save
    ;; the alignment info for the current cell/column to the INFO
    ;; channel.. magic!
    (org-blackfriday-table-cell-alignment table-cell info)
    ;; Each cell needs to be at least 3 characters wide (4 chars,
    ;; including the table border char "|"), otherwise
    ;; Hugo/Blackfriday does not render that as a table.
    (when (< cell-width 4)
      (setq cell (concat (make-string (- 4 cell-width) ? ) cell)))
    ;; (message "[ox-bf-table-cell DBG] Cell:\n%s" cell)
    cell))

;;;; Table-Row
(defun org-blackfriday-table-row (table-row contents info)
  "Transcode TABLE-ROW element into Blackfriday Markdown format.

CONTENTS is cell contents of TABLE-ROW.  INFO is a plist used as a
communication channel."
  ;; (message "[ox-bf-table-row DBG]")
  (let* ((table (org-export-get-parent-table table-row))
         (row-num (cl-position          ;Begins with 0
                   table-row
                   (org-element-map table 'table-row #'identity info)))
         (row contents)) ;If CONTENTS is `nil', row has to be returned as `nil' too
    ;; Reset the state variable when the first row of the table is
    ;; received.
    (when (eq 0 row-num)
      (setq org-blackfriday--hrule-inserted nil))

    ;; (message "[ox-bf-table-row DBG] Row # %0d In contents: %s,\ntable-row: %S" row-num contents table-row)
    (when (and row
               (eq 'rule (org-element-property :type table-row))
               ;; In Blackfriday, rule is valid only at second row.
               (eq 1 row-num))
      (let ((cols (cdr (org-export-table-dimensions table info))))
        (setq row (concat org-blackfriday-table-left-border
                          (mapconcat
                           (lambda (col)
                             (let ((max-width (max 3 (+ 1 (org-blackfriday-table-col-width table col info)))))
                               (make-string max-width ?-)))
                           (number-sequence 0 (- cols 1))
                           org-blackfriday-table-separator)
                          org-blackfriday-table-right-border))))

    ;; If the first table row is "abc | def", it needs to have a rule
    ;; under it for Blackfriday to detect the whole object as a table.
    (when (and (stringp row)
               (null org-blackfriday--hrule-inserted))
      ;; (message "[ox-bf-table-row DBG] row: %s" row)
      (let ((rule (replace-regexp-in-string "[^|]" "-" row))
            (pos 0)
            (new-rule "")
            matches)
        ;; (message "[ox-bf-table-row DBG] rule: %s" rule)
        ;; https://emacs.stackexchange.com/a/7150/115
        (while (string-match "|-+" rule pos)
          (push (match-string 0 rule) matches)
          (setq pos (match-end 0)))
        (setq matches (nreverse matches))
        ;; Get the align-vector that was saved in the INFO channel in
        ;; `org-blackfriday-table-cell-alignment'.
        (let* ((alignment-cache (plist-get info :table-cell-alignment-cache))
               (align-vector (gethash table alignment-cache))
               (col 0))
          ;; (message "[ox-bf-table-row DBG] align-vector: %S" align-vector)
          (dolist (match matches)
            (let ((align (aref align-vector col)))
              (when (member align '(left center))
                (setq match (replace-regexp-in-string "\\`|-" "|:" match)))
              (when (member align '(right center))
                (setq match (replace-regexp-in-string "-\\'" ":" match))))
            (setq new-rule (concat new-rule match))
            (setq col (1+ col))))
        (setq new-rule (concat new-rule "|"))
        ;; (message "[ox-bf-table-row DBG] new-rule: %s" new-rule)
        (setq row (concat row "\n" new-rule))
        (setq org-blackfriday--hrule-inserted t)))
    ;; (message "[ox-bf-table-row DBG] Row:\n%s" row)
    row))

;;;; Table
(defun org-blackfriday-table (table contents info)
  "Transcode TABLE element into Blackfriday Markdown format.

CONTENTS is contents of the table.  INFO is a plist holding
contextual information."
  ;; (message "[ox-bf-table DBG] In contents: %s" contents)
  (if (eq (org-element-property :type table) 'table.el)
      ;; "table.el" table.  Convert it using appropriate tools.
      (let ((tbl (org-html-table--table.el-table table info)))
        (replace-regexp-in-string
         "\\(<!-- This HTML table template is generated by emacs\\) .*\\( -->\n\\)" "\\1/table.el\\2" tbl))
    ;; Standard table.
    (let* ((rows (org-element-map table 'table-row 'identity info))
           (no-header (= (length rows) 1)) ;No header if table has just 1 row
           (table-ref (org-blackfriday--get-reference table))
           (table-anchor (if table-ref
                             (format "<a id=\"%s\"></a>\n" table-ref)
                           ""))
           (caption (org-export-get-caption table))
           table-num
           (blank-line-before-table "")
           (caption-html (if (not caption)
                             ""
                           (let ((caption-prefix (org-blackfriday--translate 'table info))
                                 (caption-str
                                  (org-html-convert-special-strings ;Interpret em-dash, en-dash, etc.
                                   (org-export-data-with-backend caption 'html info))))
                             (setq table-num (org-export-get-ordinal
                                              table info
                                              nil #'org-html--has-caption-p))
                             (format (concat "<div class=\"table-caption\">\n"
                                             "  <span class=\"table-number\">%s</span>:\n"
                                             "  %s\n"
                                             "</div>\n")
                                     (if table-ref ;Hyperlink the table prefix + number
                                         (format "<a href=\"#%s\">%s %s</a>"
                                                 table-ref caption-prefix table-num)
                                       (format "%s %s"
                                               caption-prefix table-num))
                                     caption-str))))
           (attr (org-export-read-attribute :attr_html table))
           ;; At the moment only the `class' attribute is supported in
           ;; #+attr_html above tables.
           (table-class-user (plist-get attr :class))
           (table-class-auto (concat "table-"
                                     (if table-num
                                         (format "%d" table-num)
                                       "nocaption")))
           (table-class (or table-class-user
                            table-class-auto))
           ;; If user has specified multiple classes for the table
           ;; (space-separated), use only the first class in that list
           ;; to specifying the styling in the <style> tag.
           (table-class-this (car (split-string table-class)))
           ;; https://www.w3schools.com/css/css_table.asp
           (css-props (org-export-read-attribute :attr_css table))
           (css-props-str (org-blackfriday--make-css-property-string css-props))
           (table-pre "")
           (table-post "")
           (tbl (replace-regexp-in-string "\n\n" "\n" contents)))

      (when (org-string-nw-p css-props-str)
        (setq table-pre (format "<style>.%s table { %s }</style>\n\n"
                                table-class-this css-props-str)))
      ;; Export user-specified table class explicitly.
      (when (or (org-string-nw-p table-class-user)
                (org-string-nw-p css-props-str))
        (setq table-pre (concat table-pre
                                (format "<div class=\"ox-hugo-table %s\">\n" table-class)
                                "<div></div>\n"))) ;See footnote 1
      (when (org-string-nw-p table-pre)
        (setq table-post (concat "\n"
                                 "</div>\n")))

      ;; If the table has only 1 row, do *not* make it a header row..
      ;; instead create an empty header row.
      ;; For 1-row, tbl would look like this at this point:
      ;;
      ;;   | a | b |
      ;;   |---|---|
      ;;
      ;; Below will convert that to:
      ;;
      ;;   |   |   |
      ;;   |---|---|
      ;;   | a | b |
      (when no-header
        (string-match "\\`\\(.*\\)\n\\(.*\\)\n\\'" tbl)
        (let* ((row-1 (match-string-no-properties 1 tbl))
               (hrule (match-string-no-properties 2 tbl))
               (dummy-header (replace-regexp-in-string "[-:]" " " hrule)))
          (setq tbl (concat dummy-header "\n" hrule "\n" row-1))))
      ;; (message "[ox-bf-table DBG] Tbl:\n%s" tbl)

      ;; A blank line is needed to separate the Markdown table and
      ;; the table anchor/caption HTML.
      (unless (string= (concat table-pre table-anchor caption-html) "")
        (setq blank-line-before-table "\n"))

      (concat table-pre table-anchor caption-html
              blank-line-before-table tbl table-post))))

;;;; Verse Block
(defun org-blackfriday-verse-block (_verse-block contents info)
  "Transcode a VERSE-BLOCK element from Org to partial HTML.
CONTENTS is verse block contents.  INFO is a plist holding
contextual information."
  (let* ((ret contents)
         ;; Org removes all the leading whitespace only from the first
         ;; line.  So the trick is to use the ">" character before any
         ;; intended indentation on the first non-blank line.
         (ret (replace-regexp-in-string "\\`\\([[:blank:]\n\r]*?\\)[[:blank:]]*>" "\\1" ret))
         (br (org-html-close-tag "br" nil info))
         (re (format "\\(?:%s\\)?[ \t]*\n" (regexp-quote br)))
         ;; Replace each newline character with line break.  Also
         ;; remove any trailing "br" close-tag so as to avoid
         ;; duplicates.
         (ret (replace-regexp-in-string re (concat br "\n") ret))
         ;; Replace leading white spaces with non-breaking spaces.
         (ret (replace-regexp-in-string
               "^[[:blank:]]+"
               (lambda (m)
                 (org-html--make-string (length m) "&nbsp;"))
               ret))
         (ret (format "<p class=\"verse\">\n%s</p>" ret)))
    ret))



;;; Interactive functions

;;;###autoload
(defun org-blackfriday-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Github Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org BLACKFRIDAY Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'blackfriday "*Org BLACKFRIDAY Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-blackfriday-convert-region-to-md ()
  "Convert text in the current region to Blackfriday Markdown.
The text is assumed to be in Org mode format.

This can be used in any buffer.  For example, you can write an
itemized list in Org mode syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'blackfriday))

;;;###autoload
(defun org-blackfriday-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Github Flavored Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'blackfriday outfile async subtreep visible-only)))

;;;###autoload
(defun org-blackfriday-publish-to-blackfriday (plist filename pub-dir)
  "Publish an Org file to Blackfriday Markdown file.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'blackfriday filename ".md" plist pub-dir))


(provide 'ox-blackfriday)



;;; Footnotes

;;;; Footnote 1
;; The empty HTML element tags like "<div></div>" is a hack to get
;; around a Blackfriday limitation.  Details:
;; https://github.com/kaushalmodi/ox-hugo/issues/93.

;;; ox-blackfriday.el ends here
