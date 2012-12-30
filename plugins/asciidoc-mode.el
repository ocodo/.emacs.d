;;; asciidoc-mode.el --- A major-mode for outlined AsciiDoc text

;; Author: Andreas Spindler <info@visualco.de>
;; Maintained at: <http://www.visualco.de>
;; Keywords: Emacs, Text, Outline, AsciiDoc

;; This file is  free software; you can redistribute it  and/or modify it under
;; the  terms of  the  GNU General  Public  License as  published  by the  Free
;; Software  Foundation;  either version  3,  or  (at  your option)  any  later
;; version. For license details C-h C-c in Emacs.

;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without  even the implied  warranty of MERCHANTABILITY  or FITNESS
;; FOR  A PARTICULAR  PURPOSE.  See the  GNU  General Public  License for  more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU  Emacs;  see the  file  COPYING.  If not,  write  to  the Free  Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Installation:
;;
;; Add following to your <~/.emacs> file:
;;
;;  (autoload 'asciidoc-mode "asciidoc-mode" nil t)
;;  (add-to-list 'auto-mode-alist '("\\.asciidoc$" . asciidoc-mode))
;;
;;; Commentary:
;;
;; Highlights titles, sections, subsections, paragraphs, footnotes and markups.
;; Uses the standard definitions for `font-lock-*-face' and extends them for
;; titles and markups.
;;
;;; Resources:
;;
;; http://www.emacswiki.org/emacs/AsciidocEl
;; 
;;;;;;;;
;; $Writestamp: 2011-03-16 11:33:53$

;;; -------- Fontlock faces
;; ----------------------------------------------------------------------------

;; Try `list-faces-display' and `list-colors-display'.

(defgroup asciidoc-faces nil
  "AsciiDoc highlighting"
  :group 'asciidoc)

;;;; Title/Section faces

;; Note that Asciidoc supports a document title plus four section levels plus
;; several paragraph-titles ("admonitionn block"). The nomenclature used here
;; borrows from (La)TeX.

(defface asciidoc-document-title-face
  `((((class color) (background dark))
     (:foreground "midnightblue" :bold t :underline t :height 1.3 :inherit variable-pitch))
    (((class color) (background light))
     (:foreground "midnightblue" :bold t :underline t :height 1.3 :inherit variable-pitch))
    (t (:weight bold :inherit variable-pitch)))
  "Face for AsciiDoc document titles (level 0)."
  :group 'asciidoc-faces)

(defface asciidoc-chapter-face
  `((((class color) (background dark))
     (:foreground "navyblue" :bold t :underline t :height 1.2 :inherit variable-pitch))
    (((class color) (background light))
     (:foreground "navyblue" :bold t :underline t :height 1.2 :inherit variable-pitch))
    (t (:weight bold :inherit variable-pitch)))
  "Face for AsciiDoc section titles (level 1)."
  :group 'asciidoc-faces)

(defface asciidoc-section-face
  `((((class color) (background dark))
     (:foreground "mediumblue" :bold t :underline t :height 1.1 :inherit variable-pitch))
    (((class color) (background light))
     (:foreground "mediumblue" :bold t :underline t :height 1.1 :inherit variable-pitch))
    (t (:weight bold :inherit variable-pitch)))
  "Face for AsciiDoc section titles (level 2)."
  :group 'asciidoc-faces)

(defface asciidoc-subsection-face
  `((((class color) (background dark))
     (:foreground "royalblue" :bold t :underline t :height 1.0 :inherit variable-pitch))
    (((class color) (background light))
     (:foreground "royalblue" :bold t :underline t :height 1.0 :inherit variable-pitch))
    (t (:weight bold)))
  "Face for AsciiDoc section titles (level 3)."
  :group 'asciidoc-faces)

(defface asciidoc-subsubsection-face
  `((((class color) (background dark))
     (:foreground "cornflowerblue" :underline t :height 1.0 :inherit variable-pitch))
    (((class color) (background light))
     (:foreground "cornflowerblue" :underline t :height 1.0 :inherit variable-pitch))
    (t (:weight bold)))
  "Face for AsciiDoc section titles (level 4)."
  :group 'asciidoc-faces)

(defface asciidoc-paragraph-face
  `((((class color) (background dark))
     (:foreground "Gray10" :bold t :inherit variable-pitch))
    (((class color) (background light))
     (:foreground "DarkRed" :bold t :inherit variable-pitch))
    (t (:inherit variable-pitch)))
  "Face for AsciiDoc paragraph titles and admonition blocks."
  :group 'asciidoc-faces)
;; CornflowerBlue

;;;; Markup faces
;; IndianRead

(defface asciidoc-block-face
  `((((class color) (background dark))
     (:foreground "DarkOrchid"))
    (((class color) (background light))
     (:foreground "DarkOrchid"))
    (t (:weight bold :inherit variable-pitch)))
  "Face for AsciiDoc paragraphs."
  :group 'asciidoc-faces)

(defface asciidoc-mono-face
  `((t (:foreground "DarkOrange4" :family "courier new")))
  "Face for AsciiDoc markup (monospaced text)."
  :group 'asciidoc-faces)

(defface asciidoc-list-number-face
  `((t (:foreground "Gray20" :bold t :family "courier new")))
  "Face for AsciiDoc list items."
  :group 'asciidoc-faces)

(defface asciidoc-emph-face
  `((t (:foreground "DarkOrange4" :slant italic :inherit variable-pitch)))
  "Face for AsciiDoc markup (emphasized text)."
  :group 'asciidoc-faces)

(defface asciidoc-bold-face
  `((t (:foreground "DarkOrange4" :weight bold)))
  "Face for AsciiDoc markup (bold text)."
  :group 'asciidoc-faces)

;;;; Extra faces

(defface asciidoc-idiosyncratic-face
  `((((class color) (background dark))
     (:foreground "Gray30"))
    (((class color) (background light))
     (:foreground "Gray30"))
    (t ()))
  "Face for AsciiDoc gibberish."
  :group 'asciidoc-faces)

(defface asciidoc-escape-face
  `((((class color) (background dark))
     (:foreground "Gray20" :background "Yellow"))
    (((class color) (background light))
     (:foreground "Gray20" :background "Yellow"))
    (t (:weight bold)))
  "Face for unquoted AsciiDoc text."
  :group 'asciidoc-faces)

;;; -------- Major mode setup
;; ----------------------------------------------------------------------------

(defvar asciidoc-mode-hook nil
  "Normal hook run when entering Doc Text mode.")

(defvar asciidoc-mode-abbrev-table nil
  "Abbrev table in use in Asciidoc-mode buffers.")
(define-abbrev-table 'asciidoc-mode-abbrev-table ())

;; To match newlines in Emacs use the negative clause "[^\r]+", which matches
;; anything that is not specified, including newlines. Here "\r" is just one
;; rarely used character under UN*X. To match single lines we use "\s-+" or "[^
;; \t]+" which matches non-whitespace characters.
;;
;; NOTE: "[^\r]" highlights multiple lines rather accidently. When you edit
;;       lines the highlight possibly disappears. The solution is to run
;;       `normal-mode' again to reset fontlocking (maybe timer-triggered). See
;;       also <http://www.emacswiki.org/cgi-bin/wiki/MultilineRegexp>

(require 'rx)
(defmacro asciidoc-rx-markup (&rest term) ; at word boundary
  `(rx (1+ (not white))
       ,@term
       (any alnum)
       (* (not (any "\r")))             ; alias [^\r]*?
       ,@term
       ))
;; (message (asciidoc-rx-markup ?'))

(defconst asciidoc-font-lock-keywords
  (eval-when-compile
    (list
     ;; Embedded HTML entities.

     (cons "&#[xX]?[0-9a-fA-F]+?;" `'font-lock-constant-face)
     (cons "#[xX]?[0-9a-fA-F]+?;" `'font-lock-constant-face)

     ;; Asciimath

     (cons "\\$\\$`.+?`\\$\\$"  `'font-lock-constant-face) ;; 

     ;; Unquoted text and trailing whitespace

     (cons "\\(?:##[^\r]*?##\\)" `'asciidoc-escape-face)
     (cons "\\(?:#.*?#\\)" `'asciidoc-escape-face)
     (cons "[ \t\v\r]+$"    `'asciidoc-escape-face)

     ;; Section titles

     (cons "^=\\s-+.*"      `'asciidoc-document-title-face)
     (cons "^==\\s-+.*"     `'asciidoc-chapter-face)
     (cons "^===\\s-+.*"    `'asciidoc-section-face)
     (cons "^====\\s-+.*"   `'asciidoc-subsection-face)
     (cons "^=====\\s-+.*"  `'asciidoc-subsubsection-face)
     (cons "^======\\s-+.*" `'asciidoc-paragraph-face)
     (cons "^\\.[A-Z].*$"   `'asciidoc-paragraph-face)
     (cons "^\\[.+?\\]"     `'asciidoc-idiosyncratic-face)

     ;; List item paragraphs with implicit numbering

     (cons "^\\s-*\\.\\{1,5\\}\\s-+"    `'asciidoc-list-number-face)

     ;; Bulleted listed item paragraphs

     (cons "^\\s-*-\\s-+"       `'asciidoc-list-number-face)
     (cons "^\\s-*\\*\\{1,5\\}\\s-+"    `'asciidoc-list-number-face)

     ;; List item paragraphs with explicit numbering

     (cons "^\\s-*[1-9]+\\.\\s-+"   `'asciidoc-list-number-face)
     (cons "^\\s-*[a-zA-Z]\\.\\s-+" `'asciidoc-list-number-face)
     (cons "^\\s-*[ixcvmIXCVM]+)\\s-+"  `'asciidoc-list-number-face)

     ;; Labeled list items

     (cons "^.*[:;][:;-]\\s-"       `'asciidoc-list-number-face)

     ;; Special lines that continue list items

     (cons "^\\(?:\\+\\|--\\)\\s-*$"    `'asciidoc-idiosyncratic-face)

     ;; Delimited blocks

     (cons "^[_=\\.\\*\\+\\-]\\{6,\\}\\s-*$" `'asciidoc-idiosyncratic-face)

     ;; Comment Lines

     (cons "^\\s-*//.*$" 'font-lock-comment-face)

     ;; Preprocessor commands

     (cons "^\\(image\\|include\\|sys\\|eval\\|ifn?def\\|endif\\|template\\)[0-9:]+\\S-*"
           'font-lock-keyword-face)
     (cons "\\(xref\\|anchor\\|link\\|image\\|asciimath\\|indexterm2?\\):+\\S-*"
           'font-lock-keyword-face)

     ;; Auxiliary directives

     (cons "^>>[{}=]+ .*" 'font-lock-type-face)
     (cons "^<< +.*" 'font-lock-builtin-face)

     ;; Text replacements

     (cons "\\(?:(R)\\|(TM)\\|(C)\\|---\\|--\\|\\.\\.\\.\\|[=-]+[<>]\\)" 
           `'font-lock-builtin-face)

     ;; Admonition blocks and annotations.

     (cons (concat "\\<\\(?:TODO\\|BUG\\|ERROR\\|DISCLAIMER\\|WARNING\\|NOTE"
           "\\|ERROR\\|TIP\\|CAUTION\\|IMPORTANT\\|EXAMPLE\\|BEISPIEL\\):")
           `'asciidoc-paragraph-face)
     (cons "\\*[A-Z]+\\*:" `'asciidoc-paragraph-face)

     ;; Normal markups (word boundaries) and unconstrained markups. Mono, bold,
     ;; emphasized. Quite a challenge, since the fontlock feature was build to
     ;; highlight single lines only, but markups can go over multiple lines.

     (cons (asciidoc-rx-markup ?+) `'asciidoc-mono-face)
     (cons (asciidoc-rx-markup ?*) `'asciidoc-bold-face)
     (cons (asciidoc-rx-markup ?') `'asciidoc-emph-face)

     ;; Super/subscript

     (cons (asciidoc-rx-markup ?^) `'asciidoc-emph-face)
     (cons (asciidoc-rx-markup ?~) `'asciidoc-emph-face)

     ;; Quoted text that can span multiple lines.

     (cons "\"[^\r]+?\"" `'font-lock-string-face)
     (cons "``[^\r]+?''" `'font-lock-string-face)
     (cons "`[^\r]+?'" `'font-lock-string-face)

     ;; Inline macros and URLs

     (cons "\\sw+://[^\\[\\t ]*" `'asciidoc-mono-face)
     (cons "\\[.+\\]" `'asciidoc-bold-face)
     )
    )
  "Syntax expressions in AsciiDoc editing mode.")

;;###autoload
(define-derived-mode asciidoc-mode text-mode "AsciiDoc"
  "Major mode for outlined AsciiDoc text files.

Calls the value of `text-mode-hook', `outline-mode-hook' then
`asciidoc-mode-hook'."
  (interactive)
  (turn-on-auto-fill)
  ;; (set-fill-column 100)
  (set-buffer-file-coding-system 'iso-latin-1-unix) ; required by our font-locking regexes
  (not-modified)

  (modify-syntax-entry ?\' ".")
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'font-lock-defaults)

  (setq comment-start "// "
        paragraph-start (concat "$\\|>" page-delimiter)
        paragraph-separate paragraph-start
        paragraph-ignore-fill-prefix t
        require-final-newline t
        case-fold-search t
        font-lock-defaults '(asciidoc-font-lock-keywords nil nil ((?_ . "w"))))

  ;; Insert, align, or delete end-of-line backslashes on the lines in the
  ;; region. See also `c-backslash-region'.

  (local-set-key [?\C-c ?\\] 'makefile-backslash-region)

  ;; Enable minor outline mode.

  (require 'outline)
  (outline-minor-mode)
  (set (make-local-variable 'outline-regexp) "^[=]+ ")
  (when nil
    (hide-body)
    (outline-next-visible-heading 1))

  ;; Run our mode hook
  (run-hooks 'asciidoc-mode-hook)
  (message "%s: asciidoc-mode" (buffer-name (current-buffer)))
  )

(provide 'asciidoc-mode)

;;; asciidoc-mode.el ends here
