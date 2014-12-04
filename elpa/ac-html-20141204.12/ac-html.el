;;; ac-html.el --- auto complete source for html tags and attributes

;; Copyright (C) 2014 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Version: 0.1.0
;; Keywords: html, auto-complete, rails, ruby
;; Package-Requires: ((auto-complete "1.4"))
;; URL: https://github.com/cheunghy/ac-html

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

;; Configuration:
;; Add these lines
;; (add-to-list 'ac-sources 'ac-source-html-tag)
;; (add-to-list 'ac-sources 'ac-source-html-attribute)
;; If you are using web-mode:
;; Additionally you need to add these lines:
;; (add-to-list 'web-mode-ac-sources-alist
;;              '("html" . (ac-source-html-tag
;;                          ac-source-html-attribute)))
;; If you are using haml-mode:
;; use `ac-source-haml-tag' and `ac-source-haml-attribute'

;;; Code:

(require 'auto-complete)

(defvar ac-html-root-element-list
  (list
   "html" "!DOCTYPE html"))

(defvar ac-html-first-child-element-list
  (list
   "head" "body"))

(defvar ac-html-unique-element-list
  (list
   "html" "head" "body" "title"))

(defvar ac-html-block-level-element-list
  (list
   "address" "article" "aside" "audio" "blockquote" "canvas" "dd" "div" "dl"
   "fieldset" "figcaption" "figure" "footer" "form" "h1" "h2" "h3" "h4" "h5"
   "h6" "header" "hgroup" "hr" "noscript" "ol" "output" "p" "pre" "section"
   "table" "tfoot" "ul" "video"))

(defvar ac-html-inline-element-list
  (list
   "b" "big" "i" "small" "tt"
   "abbr" "acronym" "cite" "code" "dfn" "em" "kbd" "strong" "samp" "var"
   "a" "bdo" "br" "img" "map" "object" "q" "script" "span" "sub" "sup"
   "button" "input" "label" "select" "textarea"))

(defvar ac-html-all-element-list
  (list
   "a" "abbr" "acronym" "address" "applet" "area" "article" "aside" "audio"
   "b" "base" "basefont" "bdi" "bdo"
   "bgsound" "big" "blink" "blockquote" "body" "br" "button"
   "canvas" "caption" "center" "cite" "code" "col" "colgroup"
   "command" "content" "data" "datalist" "dd" "decorator" "del"
   "details" "dfn" "dialog" "dir" "div" "dl" "dt"
   "element" "em" "embed" "fieldset" "figcaption" "figure" "font"
   "footer" "form" "frame" "frameset" "head" "header" "h1" "h2" "h3" "h4"
   "h5" "h6" "hgroup"
   "hr" "href" "html" "i" "iframe" "image" "img"
   "input" "ins" "isindex" "kbd" "keygen" "label" "legend"
   "li" "link" "listing" "main" "map" "mark" "marquee"
   "menu" "menuitem" "meta" "meter" "multicol" "nav" "nextid"
   "nobr" "noembed" "noframes" "noscript" "object" "ol" "optgroup"
   "option" "output" "p" "param" "picture" "plaintext" "portfolio"
   "pre" "progress" "q" "rb" "rp" "rt" "ruby"
   "s" "samp" "script" "section" "select" "shadow" "small"
   "source" "spacer" "span" "strike" "strong" "style" "sub"
   "summary" "sup" "svg" "table" "tbody" "td" "template"
   "textarea" "tfoot" "th" "thead" "time" "title" "tr"
   "track" "tt" "u" "ul" "var" "video" "wbr" "xmp"
   ))

(defvar ac-html-user-defined-class-list
  '())

(defvar ac-html-user-defined-id-list
  '())

(defun ac-source-html-tag-candidates ()

  (let ((doctype-declared nil) (html-declared nil)
  	(head-declared nil) (body-declared nil)
  	(html-doctype-level nil) (head-body-level nil)
  	(inside-head-1-level nil) (inside-body-1-level nil))

    ;; inside head or not
    (and (save-excursion (re-search-backward "<head" nil t))
    	 (save-excursion (re-search-forward "</head>" nil t))
    	 (setq inside-head-1-level t))

    ;; inside body or not
    (and (save-excursion (search-backward-regexp "<body" nil t))
    	 (save-excursion (search-forward-regexp "</body>" nil t))
    	 (setq inside-body-1-level t))

    ;; inside html or not
    (or inside-head-1-level inside-body-1-level
    	(and (save-excursion (search-backward-regexp "<html" nil t))
    	     (save-excursion (search-forward-regexp "</html>" nil t))
    	     (setq head-body-level t)))

    ;; must be top level
    (or inside-head-1-level inside-body-1-level head-body-level
    	(setq html-doctype-level t))

    ;; if top level check if doctype and html declared
    (if html-doctype-level
    	(progn
    	  (and (save-excursion (goto-char (point-min))
                               (search-forward-regexp "<!DOCTYPE" nil t))
    	       (setq doctype-declared t))
    	  (and (save-excursion (goto-char (point-min))
    			       (search-forward-regexp "<html" nil t))
    	       (setq html-declared t))))

    ;; if head or body level
    (if head-body-level
    	(progn
    	  (and (save-excursion (goto-char (point-min))
    			       (search-forward-regexp "<head" nil t))
    	       (setq head-declared t))
    	  (and (save-excursion (goto-char (point-min))
    			       (search-forward-regexp "<body" nil t))
    	       (setq body-declared t))))

    ;; return value
    (cond (html-doctype-level ac-html-root-element-list)
    	  (head-body-level ac-html-first-child-element-list)
    	  (inside-head-1-level ac-html-all-element-list)
  	  (inside-body-1-level ac-html-all-element-list))
    ))

(defconst ac-html-package-dir (file-name-directory load-file-name))

(defun ac-source-html-tag-documentation (symbol)
  (let* ((where-to-find
          (expand-file-name "html-stuff/html-tag-short-docs"
                            ac-html-package-dir))
	 (doc-file (expand-file-name symbol where-to-find)))
    (if (file-exists-p doc-file)
	(progn
	  (with-temp-buffer
	    (insert-file-contents doc-file)
	    (buffer-string)))
      "Currently not documented.")))

(defun ac-html--load-list-from-file (filepath)
  "Return a list separated by \\n from FILEPATH."
  (with-current-buffer (find-file-noselect filepath)
    (split-string (save-restriction
		    (widen)
		    (buffer-substring-no-properties
		     (point-min) (point-max)))
		  "\n" t)))

(defun ac-html--current-html-tag ()
  "Return current html tag user is typing on."
  (let* ((tag-search (save-excursion
		       (re-search-backward "<\\(\\w+\\)[[:space:]]+" nil t)))
	 (tag-string (match-string 1)))
    tag-string))

(defun ac-html--attribute-candidates (source)
  (let* ((tag-string source)
	 (global-attributes-file
	  (expand-file-name "html-stuff/html-attributes-list/global"
			    ac-html-package-dir))
	 (this-attributes-file-name
	  (format "html-stuff/html-attributes-list/%s" tag-string))
	 (this-attributes-file
	  (expand-file-name this-attributes-file-name
			    ac-html-package-dir))
	 (list-to-return ()))

    (if (file-exists-p global-attributes-file)
	(setq list-to-return
	      (append list-to-return
		      (ac-html--load-list-from-file global-attributes-file))))

    (if (file-exists-p this-attributes-file)
	(setq list-to-return
	      (append list-to-return
		      (ac-html--load-list-from-file this-attributes-file))))
    list-to-return))

(defun ac-source-html-attribute-candidates ()
  (ac-html--attribute-candidates (ac-html--current-html-tag)))

(defmacro ac-html--attribute-documentation (attribute tag)
  `(let* ((where-to-find
           (expand-file-name "html-stuff/html-attributes-short-docs"
                             ac-html-package-dir))
          (tag-string ,tag)
          (tag-doc-file-name (format "%s-%s" tag-string ,attribute))
          (global-doc-file-name (format "%s-%s" "global" ,attribute))
          (tag-doc-file (expand-file-name tag-doc-file-name where-to-find))
          (global-doc-file
           (expand-file-name global-doc-file-name where-to-find))
          (doc-to-return ""))
     (if (file-exists-p tag-doc-file)
         (setq doc-to-return (with-temp-buffer
                               (insert-file-contents tag-doc-file)
                               (buffer-string))))
     (if (string-equal doc-to-return "")
         (if (file-exists-p global-doc-file)
             (setq doc-to-return (with-temp-buffer
                                   (insert-file-contents global-doc-file)
                                   (buffer-string)))))
     doc-to-return))

(defun ac-source-html-attribute-documentation (symbol)
  (ac-html--attribute-documentation symbol
                                    (ac-html--current-html-tag)))

(defvar ac-source-html-tag
  '((candidates . ac-source-html-tag-candidates)
    (prefix . "<\\(.*\\)")
    (symbol . "t")
    (document . ac-source-html-tag-documentation)))

(defvar ac-source-html-attribute
  '((candidates . ac-source-html-attribute-candidates)
    (prefix . "<\\w[^>]*[[:space:]]+\\(.*\\)")
    (symbol . "a")
    (document . ac-source-html-attribute-documentation)))

(provide 'ac-html)
;;; ac-html.el ends here
