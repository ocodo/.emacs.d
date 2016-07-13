;;; xah-css-mode.el --- Major mode for editing CSS code.

;; Copyright © 2013-2015 by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 2.3.4
;; Package-Version: 20160711.840
;; Created: 18 April 2013
;; Package-Requires: ((emacs "24.3"))
;; Keywords: languages, convenience, css, color
;; Homepage:  http://ergoemacs.org/emacs/xah-css-mode.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; Major mode for editing CSS code. Alternative to GNU emacs's builtin `css-mode'.

;; Features:

;; • Correct Syntax coloring ALL CSS words. Does not color typos.

;; • Color coded words by semantics. Each type of CSS words are colored distinctly. e.g. HTML tag names, CSS attribute names, predefined CSS value names, CSS unit names, pseudo selector names, media keywords, etc.

;; • ID selector string and class name in bold for easy identification.

;; • Keyword completion with `ido-mode' interface. Press Tab ↹ after a word to complete. All CSS words are supported: {html5 tags, property names, property value keywords, units, colors, pseudo selectors, “at keywords”, …}.

 ;; • Single Key Prettify Code Format. Press Tab ↹ before word to reformat current block of code. That is, all lines enclosed by curly brackets {}.

;; • Syntax coloring of hexadecimal color format #rrggbb , #rgb , and HSL Color format hsl(0,68%,42%).

;; • Call `xah-css-hex-to-hsl-color' to convert #rrggbb color format under cursor to HSL Color format.

;; • Call `xah-css-minify' to compact region.

;; • Call `xah-css-expand-to-multi-lines' to expand minified CSS code to multi-lines format.

;; • Call `describe-function' on `xah-css-mode' for detail.

;;; INSTALL:

;; To manual install,
;; Place the file at ~/.emacs.d/lisp/ . Create the dir if it doesn't exist.
;; Then put the following in ~/.emacs.d/init.el
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (autoload 'xah-css-mode "xah-css-mode" "css major mode." t)

;;; HISTORY

;; version history no longer kept here.
;; version 2015-01-30 fix a problem with emacs 24.3.1, Debugger entered--Lisp error: (file-error "Cannot open load file" "prog-mode")
;; version 0.3, 2013-05-02 added xah-css-hex-color-to-hsl, and other improvements.
;; version 0.2, 2013-04-22 added xah-css-compact-css-region
;; version 0.1, 2013-04-18 first version


;;; Code:

(require 'color) ; part of emacs 24.1
(require 'newcomment)
(require 'lisp-mode) ; for indent-sexp. todo possibly write own customized for css

(defvar xah-css-mode-hook nil "Standard hook for `xah-css-mode'")



(defface xah-css-id-selector
  '(
    (t :foreground "firebrick" :weight bold))
  "face for CSS ID selector “#…”."
  :group 'xah-css-mode )

(defface xah-css-class-selector
  '(
    (t :weight bold))
  "face for CSS class name selector “.…”."
  :group 'xah-css-mode )

;; temp for debugging
(face-spec-set
 'xah-css-id-selector
 '(
   (t :foreground "firebrick" :weight bold))
 'face-defface-spec
 )

(face-spec-set
 'xah-css-class-selector
 '(
   (t :weight bold))
 'face-defface-spec
 )

(defun xah-css-insert-random-color-hsl ()
  "Insert a random color string of CSS HSL format.
Sample output: hsl(100,24%,82%);
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2015-06-11"
  (interactive)
  (insert (format "hsl(%d,%d%%,%d%%);" (random 360) (random 100) (random 100))))

(defun xah-css-hex-color-to-hsl ()
  "Convert color spec under cursor from “#rrggbb” to CSS HSL format.
 ⁖ #ffefd5 ⇒ hsl(37,100%,91%)
URL `http://ergoemacs.org/emacs/elisp_convert_rgb_hsl_color.html'
Version 2015-06-11"
  (interactive)
  (let* (
         (-bds (bounds-of-thing-at-point 'word))
         (-p1 (car -bds))
         (-p2 (cdr -bds))
         (-currentWord (buffer-substring-no-properties -p1 -p2)))
    (if (string-match "[a-fA-F0-9]\\{6\\}" -currentWord)
        (progn
          (delete-region -p1 -p2 )
          (when (equal (char-before) 35) ; 35 is #
            (delete-char -1))
          (insert (xah-css-hex-to-hsl-color -currentWord )))
      (progn
        (user-error "The current word 「%s」 is not of the form #rrggbb." -currentWord)))))

(defun xah-css-hex-to-hsl-color (hex-str)
  "Convert hex-str color to CSS HSL format.
Return a string. Example:  \"ffefd5\" ⇒ \"hsl(37,100%,91%)\"
Note: The input string must NOT start with “#”.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2015-06-11"
  (let* (
         (colorVec (xah-css-convert-color-hex-to-vec hex-str))
         (-R (elt colorVec 0))
         (-G (elt colorVec 1))
         (-B (elt colorVec 2))
         (hsl (color-rgb-to-hsl -R -G -B))
         (-H (elt hsl 0))
         (-S (elt hsl 1))
         (-L (elt hsl 2)))
    (format "hsl(%d,%d%%,%d%%)" (* -H 360) (* -S 100) (* -L 100))))

(defun xah-css-convert-color-hex-to-vec (rrggbb)
  "Convert color rrggbb from “\"rrggbb\"” string to a elisp vector [r g b], where the values are from 0 to 1.
Example:
 (xah-css-convert-color-hex-to-vec \"00ffcc\") ⇒ [0.0 1.0 0.8]

Note: The input string must NOT start with “#”.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2015-06-11"
  (vector
   (xah-css-normalize-number-scale (string-to-number (substring rrggbb 0 2) 16) 255)
   (xah-css-normalize-number-scale (string-to-number (substring rrggbb 2 4) 16) 255)
   (xah-css-normalize-number-scale (string-to-number (substring rrggbb 4) 16) 255)))

(defun xah-css-normalize-number-scale (val range-max)
  "scale val from range [0, range-max] to [0, 1]
The arguments can be int or float.
Return value is float.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2015-06-11"
  (/ (float val) (float range-max)))


;;; functions

(defun xah-css--replace-regexp-pairs-region (begin end pairs &optional fixedcase-p literal-p)
  "Replace regex string find/replace PAIRS in region.

BEGIN END are the region boundaries.

PAIRS is
 [[regexStr1 replaceStr1] [regexStr2 replaceStr2] …]
It can be list or vector, for the elements or the entire argument.

The optional arguments FIXEDCASE-P and LITERAL-P is the same as in `replace-match'.

Find strings case sensitivity depends on `case-fold-search'. You can set it locally, like this: (let ((case-fold-search nil)) …)"
  (save-restriction
      (narrow-to-region begin end)
      (mapc
       (lambda (-x)
         (goto-char (point-min))
         (while (search-forward-regexp (elt -x 0) (point-max) t)
           (replace-match (elt -x 1) fixedcase-p literal-p)))
       pairs)))

(defun xah-css--replace-pairs-region (begin end pairs)
  "Replace multiple PAIRS of find/replace strings in region BEGIN END.

PAIRS is a sequence of pairs
 [[findStr1 replaceStr1] [findStr2 replaceStr2] …]
It can be list or vector, for the elements or the entire argument.

Find strings case sensitivity depends on `case-fold-search'. You can set it locally, like this: (let ((case-fold-search nil)) …)

The replacement are literal and case sensitive.

Once a subsring in the buffer is replaced, that part will not change again.  For example, if the buffer content is “abcd”, and the pairs are a → c and c → d, then, result is “cbdd”, not “dbdd”.

Note: the region's text or any string in PAIRS is assumed to NOT contain any character from Unicode Private Use Area A. That is, U+F0000 to U+FFFFD. And, there are no more than 65534 pairs."
  (let (
        (-unicodePriveUseA #xf0000)
        (-i 0)
        (-tempMapPoints '()))
    (progn
      ;; generate a list of Unicode chars for intermediate replacement. These chars are in  Private Use Area.
      (setq -i 0)
      (while (< -i (length pairs))
        (push (char-to-string (+ -unicodePriveUseA -i)) -tempMapPoints)
        (setq -i (1+ -i))))
    (save-excursion
      (save-restriction
        (narrow-to-region begin end)
        (progn
          ;; replace each find string by corresponding item in -tempMapPoints
          (setq -i 0)
          (while (< -i (length pairs))
            (goto-char (point-min))
            (while (search-forward (elt (elt pairs -i) 0) nil t)
              (replace-match (elt -tempMapPoints -i) t t))
            (setq -i (1+ -i))))
        (progn
          ;; replace each -tempMapPoints by corresponding replacement string
          (setq -i 0)
          (while (< -i (length pairs))
            (goto-char (point-min))
            (while (search-forward (elt -tempMapPoints -i) nil t)
              (replace-match (elt (elt pairs -i) 1) t t))
            (setq -i (1+ -i))))))))

(defun xah-css-compact-css-region (begin end)
  "Remove unnecessary whitespaces of CSS source code in region.
WARNING: not robust.
URL `http://ergoemacs.org/emacs/elisp_css_compressor.html'
Version 2015-04-29"
  (interactive "r")
  (save-restriction
    (narrow-to-region begin end)
    (xah-css--replace-regexp-pairs-region
     (point-min)
     (point-max)
     '(["  +" " "]))
    (xah-css--replace-pairs-region
     (point-min)
     (point-max)
     '(
       ["\n" " "]
       [" /* " "/*"]
       [" */ " "*/"]
       [" {" "{"]
       ["{ " "{"]
       ["; " ";"]
       [": " ":"]
       [";}" "}"]
       ["}" "}\n"]
       ))))

(defun xah-css-minify (&optional begin end)
  "Remove unnecessary whitespaces of CSS source code in region.
If there's text selection, work on that region.
Else, work on whole buffer.
Version 2016-07-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-restriction
    (narrow-to-region begin end)
    (xah-css--replace-regexp-pairs-region
     (point-min)
     (point-max)
     '(["  +" " "]))
    (xah-css--replace-pairs-region
     (point-min)
     (point-max)
     '(
       ["\n" " "]
       [" /* " "/*"]
       [" */ " "*/"]
       [" {" "{"]
       ["{ " "{"]
       ["; " ";"]
       [": " ":"]
       [" }" "}"]
       [";}" "}"]
       ["}" "}\n"]
       ))))

(defun xah-css-expand-to-multi-lines (&optional begin end)
  "Expand minified CSS code to multiple lines.
If there's text selection, work on that region.
Else, work on whole buffer.
Warning: if you have string and the string contains curly brackets {} semicolon ; and CSS comment delimitors, they may be changed with extra space added.
todo a proper solution is to check first if it's in string before transform. But may not worth it, since its rare to have string in css.
Version 2016-07-10"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-restriction
    (narrow-to-region begin end)
    (xah-css--replace-pairs-region
     (point-min)
     (point-max)
     '(
       [";" ";\n"]
       ["/* " "\n/*"]
       ["*/" "*/\n"]
       ["{" "\n{\n"]
       ["}" "\n}\n"]
       ))
    (xah-css--replace-regexp-pairs-region
     (point-min)
     (point-max)
     '(["\n\n+" "\n\n"]))))

(defun xah-css-compact-block ()
  "Compact current CSS code block.
A block is surrounded by blank lines.
This command basically replace newline char by space.
Version 2015-06-29"
  (interactive)
  (let (p1 p2)
    (save-excursion
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))
    (save-restriction
      (narrow-to-region p1 p2)

      (goto-char (point-min))
      (while (search-forward "\n" nil "NOERROR")
        (replace-match " "))

      (goto-char (point-min))
      (while (search-forward-regexp "  +" nil "NOERROR")
        (replace-match " ")))))


(defvar xah-css-html-tag-names nil "List of HTML5 tag names.")
(setq xah-css-html-tag-names

'(
"a" "abbr" "address" "applet" "area" "article" "aside" "audio" "b"
"base" "basefont" "bdi" "bdo" "blockquote" "body" "br" "button"
"canvas" "caption" "cite" "code" "col" "colgroup" "command" "datalist"
"dd" "del" "details" "dfn" "div" "dl" "doctype" "dt" "em" "embed"
"fieldset" "figcaption" "figure" "footer" "form" "h1" "h2" "h3" "h4"
"h5" "h6" "head" "header" "hgroup" "hr" "html" "i" "iframe" "img"
"input" "ins" "kbd" "keygen" "label" "legend" "li" "link"
"main"
 "map" "mark"
"menu" "meta" "meter" "nav" "noscript" "object" "ol" "optgroup"
"option" "output" "p" "param" "pre" "progress" "q" "rp" "rt" "ruby"
"s" "samp" "script" "section" "select" "small" "source" "span"
"strong" "style" "sub" "summary" "sup" "table" "tbody" "td" "textarea"
"tfoot" "th" "thead" "time" "title" "tr" "u" "ul" "var" "video" "wbr"

))

(defvar xah-css-property-names nil "List of CSS property names.")
(setq xah-css-property-names
'(

"align-content"
"align-items"
"align-self"
"animation"
"animation-delay"
"animation-direction"
"animation-duration"
"animation-fill-mode"
"animation-iteration-count"
"animation-name"
"animation-play-state"
"animation-timing-function"
"attr"
"backface-visibility"
"background"
"background-attachment"
"background-clip"
"background-color"
"background-image"
"background-origin"
"background-position"
"background-repeat"
"background-size"
"border"
"border-bottom"
"border-bottom-color"
"border-bottom-left-radius"
"border-bottom-right-radius"
"border-bottom-style"
"border-bottom-width"
"border-collapse"
"border-color"
"border-image"
"border-image-outset"
"border-image-repeat"
"border-image-slice"
"border-image-source"
"border-image-width"
"border-left"
"border-left-color"
"border-left-style"
"border-left-width"
"border-radius"
"border-right"
"border-right-color"
"border-right-style"
"border-right-width"
"border-spacing"
"border-style"
"border-top"
"border-top-color"
"border-top-left-radius"
"border-top-right-radius"
"border-top-style"
"border-top-width"
"border-width"
"bottom"
"bottom"
"box-decoration-break"
"box-shadow"
"box-sizing"
"break-after"
"break-before"
"break-inside"
"clear"
"color"
"content"
"counter-increment"
"counter-reset"
"cursor"
"direction"
"display"
"filter"
"float"
"font"
"font-family"
"font-size"
"font-style"
"font-weight"
"height"
"left"
"letter-spacing"
"line-height"
"list-style"
"list-style-image"
"list-style-type"
"margin"
"margin-bottom"
"margin-left"
"margin-right"
"margin-top"
"max-height"
"max-width"
"min-height"
"min-width"
"opacity"
"orphans"
"overflow"
"padding"
"padding-bottom"
"padding-left"
"padding-right"
"padding-top"
"page-break-after"
"page-break-inside"
"position"
"pre-wrap"
"right"
"tab-size"
"table-layout"
"text-align"
"text-align"
"text-align-last"
"text-combine-horizontal"
"text-decoration"
"text-decoration"
"text-decoration-color"
"text-decoration-line"
"text-decoration-style"
"text-indent"
"text-orientation"
"text-overflow"
"text-rendering"
"text-shadow"
"text-shadow"
"text-transform"
"text-underline-position"
"top"
"top"
"transform"
"transform-origin"
"transform-style"
"transition"
"transition-delay"
"transition-duration"
"transition-property"
"transition-timing-function"
"unicode-bidi"
"vertical-align"
"white-space"
"widows"
"width"
"word-spacing"
"word-wrap"
"z-index"

) )

(defvar xah-css-pseudo-selector-names nil "List of CSS pseudo selector names.")
(setq xah-css-pseudo-selector-names '(
":active"
":after"
":any"
":before"
":checked"
":default"
":dir"
":disabled"
":empty"
":enabled"
":first"
":first-child"
":first-letter"
":first-line"
":first-of-type"
":focus"
":fullscreen"
":hover"
":in-range"
":indeterminate"
":invalid"
":lang"
":last-child"
":last-of-type"
":left"
":link"
":not"
":nth-child"
":nth-last-child"
":nth-last-of-type"
":nth-of-type"
":only-child"
":only-of-type"
":optional"
":out-of-range"
":read-only"
":read-write"
":required"
":right"
":root"
":scope"
":target"
":valid"
":visited"
) )

(defvar xah-css-media-keywords nil "List of CSS xxxxx todo.")
(setq xah-css-media-keywords '(
"@charset"
"@document"
"@font-face"
"@import"
"@keyframes"
"@media"
"@namespace"
"@page"
"@supports"
"@viewport"
"print"
"screen"
"all"
"speech"
"and"
"not"
"only"
) ) ; todo

(defvar xah-css-unit-names nil "List of CSS unite names.")
(setq xah-css-unit-names
 '("px" "pt" "pc" "cm" "mm" "in" "em" "rem" "ex" "%" "deg") )

(defvar xah-css-value-kwds nil "List of CSS value names")
(setq
 xah-css-value-kwds
 '(

"initial"
"circle"
"ellipse"
"at"
"!important"
"absolute"
"alpha"
"auto"
"avoid"
"block"
"bold"
"both"
"bottom"
"break-word"
"center"
"collapse"
"dashed"
"dotted"
"embed"
"fixed"
"flex"
"flex-start"
"flex-wrap"
"grid"
"help"
"hidden"
"hsl"
"hsla"
"inherit"
"inline"
"inline-block"
"italic"
"large"
"left"
"linear-gradient"
"ltr"
"middle"
"monospace"
"no-repeat"
"none"
"normal"
"nowrap"
"pointer"
"radial-gradient"
"relative"
"rgb"
"rgba"
"right"
"rotate"
"rotate3d"
"rotateX"
"rotateY"
"rotateZ"
"rtl"
"sans-serif"
"scale"
"scale3d"
"scaleX"
"scaleY"
"scaleZ"
"serif"
"skew"
"skewX"
"skewY"
"small"
"smaller"
"solid"
"square"
"static"
"steps"
"table"
"table-caption"
"table-cell"
"table-column"
"table-column-group"
"table-footer-group"
"table-header-group"
"table-row"
"table-row-group"
"thin"
"top"
"translate"
"translate3d"
"translateX"
"translateY"
"translateZ"
"transparent"
"underline"
"overline"
"line-through"
"blink"
"url"
"wrap"
"x-large"
"xx-large"

   ) )

(defvar xah-css-color-names nil "List of CSS color names.")
(setq xah-css-color-names

'("aliceblue" "antiquewhite" "aqua" "aquamarine" "azure" "beige"
"bisque" "black" "blanchedalmond" "blue" "blueviolet" "brown"
"burlywood" "cadetblue" "chartreuse" "chocolate" "coral"
"cornflowerblue" "cornsilk" "crimson" "cyan" "darkblue" "darkcyan"
"darkgoldenrod" "darkgray" "darkgreen" "darkgrey" "darkkhaki"
"darkmagenta" "darkolivegreen" "darkorange" "darkorchid" "darkred"
"darksalmon" "darkseagreen" "darkslateblue" "darkslategray"
"darkslategrey" "darkturquoise" "darkviolet" "deeppink" "deepskyblue"
"dimgray" "dimgrey" "dodgerblue" "firebrick" "floralwhite"
"forestgreen" "fuchsia" "gainsboro" "ghostwhite" "gold" "goldenrod"
"gray" "green" "greenyellow" "grey" "honeydew" "hotpink" "indianred"
"indigo" "ivory" "khaki" "lavender" "lavenderblush" "lawngreen"
"lemonchiffon" "lightblue" "lightcoral" "lightcyan"
"lightgoldenrodyellow" "lightgray" "lightgreen" "lightgrey"
"lightpink" "lightsalmon" "lightseagreen" "lightskyblue"
"lightslategray" "lightslategrey" "lightsteelblue" "lightyellow"
"lime" "limegreen" "linen" "magenta" "maroon" "mediumaquamarine"
"mediumblue" "mediumorchid" "mediumpurple" "mediumseagreen"
"mediumslateblue" "mediumspringgreen" "mediumturquoise"
"mediumvioletred" "midnightblue" "mintcream" "mistyrose" "moccasin"
"navajowhite" "navy" "oldlace" "olive" "olivedrab" "orange"
"orangered" "orchid" "palegoldenrod" "palegreen" "paleturquoise"
"palevioletred" "papayawhip" "peachpuff" "peru" "pink" "plum"
"powderblue" "purple" "red" "rosybrown" "royalblue" "saddlebrown"
"salmon" "sandybrown" "seagreen" "seashell" "sienna" "silver"
"skyblue" "slateblue" "slategray" "slategrey" "snow" "springgreen"
"steelblue" "tan" "teal" "thistle" "tomato" "turquoise" "violet"
"wheat" "white" "whitesmoke" "yellow" "yellowgreen") )

(defvar xah-css-all-keywords nil "List of all CSS keywords")
(setq xah-css-all-keywords (append xah-css-html-tag-names
                                     xah-css-color-names
                                     xah-css-property-names
                                     xah-css-pseudo-selector-names
                                     xah-css-media-keywords
                                     xah-css-unit-names
                                     xah-css-value-kwds
                                     ))


;; completion

(defun xah-css-complete-symbol ()
  "Perform keyword completion on current word.
This uses `ido-mode' user interface for completion."
  (interactive)
  (let* (
         (-bds (bounds-of-thing-at-point 'symbol))
         (-p1 (car -bds))
         (-p2 (cdr -bds))
         (-current-sym
          (if  (or (null -p1) (null -p2) (equal -p1 -p2))
              ""
            (buffer-substring-no-properties -p1 -p2)))
         -result-sym)
    (when (not -current-sym) (setq -current-sym ""))
    (setq -result-sym
          (ido-completing-read "" xah-css-all-keywords nil nil -current-sym ))
    (delete-region -p1 -p2)
    (insert -result-sym)))


;; syntax table
(defvar xah-css-mode-syntax-table nil "Syntax table for `xah-css-mode'.")
(setq xah-css-mode-syntax-table
      (let ((synTable (make-syntax-table)))

;        (modify-syntax-entry ?0  "." synTable)
;        (modify-syntax-entry ?1  "." synTable)
;        (modify-syntax-entry ?2  "." synTable)
;        (modify-syntax-entry ?3  "." synTable)
;        (modify-syntax-entry ?4  "." synTable)
;        (modify-syntax-entry ?5  "." synTable)
;        (modify-syntax-entry ?6  "." synTable)
;        (modify-syntax-entry ?7  "." synTable)
;        (modify-syntax-entry ?8  "." synTable)
;        (modify-syntax-entry ?9  "." synTable)

        (modify-syntax-entry ?_ "_" synTable)
        (modify-syntax-entry ?: "." synTable)

        (modify-syntax-entry ?- "_" synTable)
        (modify-syntax-entry ?\/ ". 14" synTable) ; /* java style comment*/
        (modify-syntax-entry ?* ". 23" synTable)
        synTable))


;; syntax coloring related

(setq xah-css-font-lock-keywords
      (let (
            (cssPseudoSelectorNames (regexp-opt xah-css-pseudo-selector-names ))
            (htmlTagNames (regexp-opt xah-css-html-tag-names 'symbols))
            (cssPropertieNames (regexp-opt xah-css-property-names 'symbols ))
            (cssValueNames (regexp-opt xah-css-value-kwds 'symbols))
            (cssColorNames (regexp-opt xah-css-color-names 'symbols))
            (cssUnitNames (regexp-opt xah-css-unit-names ))
            (cssMedia (regexp-opt xah-css-media-keywords )))
        `(
          ("#[-_a-zA-z]+[-_a-zA-z0-9]*" . 'xah-css-id-selector)
          ("\\.[a-zA-z]+[-_a-zA-z0-9]*" . 'xah-css-class-selector)
          (,cssPseudoSelectorNames . font-lock-preprocessor-face)
          (,htmlTagNames . font-lock-function-name-face)
          (,cssPropertieNames . font-lock-variable-name-face )
          (,cssValueNames . font-lock-keyword-face)
          (,cssColorNames . font-lock-constant-face)
          (,cssUnitNames . font-lock-type-face)
          (,cssMedia . font-lock-builtin-face)

          ("#[abcdef[:digit:]]\\{6,6\\}" .
           (0 (put-text-property
               (match-beginning 0)
               (match-end 0)
               'face (list :background (match-string-no-properties 0)))))

          ("#[abcdef[:digit:]]\\{3,3\\};" .
           (0 (put-text-property
               (match-beginning 0)
               (match-end 0)
               'face
               (list
                :background
                (let* (
                       (ms (match-string-no-properties 0))
                       (r (substring ms 1 2))
                       (g (substring ms 2 3))
                       (b (substring ms 3 4)))
                  (concat "#" r r g g b b))))))

          ("hsl( *\\([0-9]\\{1,3\\}\\) *, *\\([0-9]\\{1,3\\}\\)% *, *\\([0-9]\\{1,3\\}\\)% *)" .
           (0 (put-text-property
               (+ (match-beginning 0) 3)
               (match-end 0)
               'face
               (list
                :background
                (concat "#"
                        (mapconcat
                         'identity
                         (mapcar
                          (lambda (x) (format "%02x" (round (* x 255))))
                          (color-hsl-to-rgb
                           (/ (string-to-number (match-string-no-properties 1)) 360.0)
                           (/ (string-to-number (match-string-no-properties 2)) 100.0)
                           (/ (string-to-number (match-string-no-properties 3)) 100.0)))
                         "" )) ;  "#00aa00"
                ))))

          ("'[^']+'" . font-lock-string-face))))


;; indent/reformat related

(defun xah-css-complete-or-indent ()
  "Do keyword completion or indent/prettify-format.

If char before point is letters and char after point is whitespace or punctuation, then do completion, except when in string or comment. In these cases, do `xah-css-prettify-root-sexp'."
  (interactive)
  ;; consider the char to the left or right of cursor. Each side is either empty or char.
  ;; there are 4 cases:
  ;; space▮space → do indent
  ;; space▮char → do indent
  ;; char▮space → do completion
  ;; char ▮char → do indent
  (let ( (-syntax-state (syntax-ppss)))
    (if (or (nth 3 -syntax-state) (nth 4 -syntax-state))
        (xah-css-prettify-root-sexp)
      (if
          (and (looking-back "[-_a-zA-Z]" 1)
               (or (eobp) (looking-at "[\n[:blank:][:punct:]]")))
          (xah-css-complete-symbol)
        (xah-css-prettify-root-sexp)))))

(defun xah-css-prettify-root-sexp ()
  "Prettify format current root sexp group.
Root sexp group is the outmost sexp unit."
  (interactive)
  (save-excursion
    (let (-p1 -p2)
      (xah-css-goto-outmost-bracket)
      (setq -p1 (point))
      (setq -p2 (scan-sexps (point) 1))
      (progn
        (goto-char -p1)
        (indent-sexp)
        ))))

(defun xah-css-goto-outmost-bracket (&optional pos)
  "Move cursor to the beginning of outer-most bracket, with respect to pos.
Returns true if point is moved, else false."
  (interactive)
  (let ((-i 0)
        (-p0 (if (number-or-marker-p pos)
                 pos
               (point))))
    (goto-char -p0)
    (while
        (and (< (setq -i (1+ -i)) 20)
             (not (eq (nth 0 (syntax-ppss (point))) 0)))
      (xah-css-up-list -1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING"))
    (if (equal -p0 (point))
        nil
      t
      )))

(defun xah-css-up-list (arg1 &optional arg2 arg3)
  "Backward compatibility fix for emacs 24.4's `up-list'.
emacs 25.x changed `up-list' to take up to 3 args. Before, only 1."
  (interactive)
  (if (>= emacs-major-version 25)
      (up-list arg1 arg2 arg3)
    (up-list arg1)))



(defun xah-css-abbrev-enable-function ()
  "Determine whether to expand abbrev.
This is called by emacs abbrev system."
  (let ((-syntax-state (syntax-ppss)))
    (if (or (nth 3 -syntax-state) (nth 4 -syntax-state))
        (progn nil)
      t)))

(setq xah-css-mode-abbrev-table nil)

(define-abbrev-table 'xah-css-mode-abbrev-table
  '(

    ("bgc" "background-color" nil :system t)
    ("rgb" "rgb(▮)" nil :system t)
    ("rgba" "rgba(▮)" nil :system t)
    ("rotate" "rotate(▮9deg)" nil :system t)
    ("rotate3d" "rotate3d(▮)" nil :system t)
    ("rotateX" "rotateX(▮)" nil :system t)
    ("rotateY" "rotateY(▮)" nil :system t)
    ("rotateZ" "rotateZ(▮)" nil :system t)
    ("scale" "scale(▮)" nil :system t)
    ("scale3d" "scale3d(▮)" nil :system t)
    ("scaleX" "scaleX(▮)" nil :system t)
    ("scaleY" "scaleY(▮)" nil :system t)
    ("scaleZ" "scaleZ(▮)" nil :system t)
    ("skew" "skew(▮9deg)" nil :system t)
    ("skewX" "skewX(▮)" nil :system t)
    ("skewY" "skewY(▮)" nil :system t)
    ("steps" "steps(▮)" nil :system t)

    ("translate" "translate(▮px,▮px)" nil :system t)
    ("translate3d" "translate3d(▮)" nil :system t)
    ("translateX" "translateX(▮)" nil :system t)
    ("translateY" "translateY(▮)" nil :system t)
    ("translateZ" "translateZ(▮)" nil :system t)

)

  "abbrev table for `xah-css-mode'"
  ;; :regexp "\\_<\\([_-0-9A-Za-z]+\\)"
  :regexp "\\([_-0-9A-Za-z]+\\)"
  :case-fixed t
  ;; :enable-function 'xah-css-abbrev-enable-function
  )


;; keybinding

(defvar xah-css-mode-map nil "Keybinding for `xah-css-mode'")

(progn
  (setq xah-css-mode-map (make-sparse-keymap))
  (define-key xah-css-mode-map (kbd "TAB") 'xah-css-complete-or-indent)

  (define-prefix-command 'xah-css-mode-no-chord-map)

  ;; todo need to set these to also emacs's conventional major mode keys
  (define-key xah-css-mode-no-chord-map (kbd "r") 'xah-css-insert-random-color-hsl)
  (define-key xah-css-mode-no-chord-map (kbd "c") 'xah-css-hex-color-to-hsl)
  (define-key xah-css-mode-no-chord-map (kbd "p") 'xah-css-compact-css-region)
  (define-key xah-css-mode-no-chord-map (kbd "m") 'xah-css-minify)
  (define-key xah-css-mode-no-chord-map (kbd "e") 'xah-css-expand-to-multi-lines)
  (define-key xah-css-mode-no-chord-map (kbd "u") 'xah-css-complete-symbol)

  ;  (define-key xah-css-mode-map [remap comment-dwim] 'xah-css-comment-dwim)

  ;; define separate, so that user can override the lead key
  (define-key xah-css-mode-map (kbd "C-c C-c") xah-css-mode-no-chord-map)

  )



;;;###autoload
(define-derived-mode xah-css-mode prog-mode "ξCSS"
  "A major mode for CSS.

URL `http://ergoemacs.org/emacs/xah-css-mode.html'

\\{xah-css-mode-map}"
  (setq font-lock-defaults '((xah-css-font-lock-keywords)))

  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")

  :group 'xah-css-mode
  )

(add-to-list 'auto-mode-alist '("\\.css\\'" . xah-css-mode))

(provide 'xah-css-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-css-mode.el ends here

