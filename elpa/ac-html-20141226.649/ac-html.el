;;; ac-html.el --- auto complete source for html tags and attributes

;; Copyright (C) 2014 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Version: 0.22
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
;;
;; Add to hook `ac-html-enable'
;;
;; (add-hook 'html-mode-hook 'ac-html-enable)
;;
;; If you are using web-mode:
;;
;; (add-to-list 'web-mode-ac-sources-alist
;;              '("html" . (
;;                          ;; attribute-value better to be first
;;                          ac-source-html-attribute-value
;;                          ac-source-html-tag
;;                          ac-source-html-attribute)))
;;
;; `ac-html-enable' remove from list ac-disable-faces 'font-lock-string-face,
;; so if you wish manually add ac-source-html-attribute-value, etc, you may need
;; customize ac-disable-faces too.
;;; Code:

(require 'auto-complete)
(require 'auto-complete-config)
(require 'cl)

(defconst ac-html-package-dir (file-name-directory load-file-name)
  "The directory where `ac-html' package exists.")

(defconst ac-html-basic-source-dir
  (expand-file-name "html-stuff" ac-html-package-dir)
  "The directory where basic source of `ac-html' exists.")

;;; Customization

(defgroup auto-complete-html nil
  "HTML Auto Complete."
  :group 'auto-complete
  :prefix "ac-html-")

(defcustom ac-html-source-dirs '(ac-html-basic-source-dir)
  "Extend through this custom variable."
  :type '(repeat symbol)
  :group 'auto-complete-html)

(defcustom ac-html-style-css t
  "Enable style attribute CSS autocomplete.
If not nil no need 'ac-source-css-property in web-mode-ac-sources-alist for web-mode (\"html\" list dot pair)"
  :group 'auto-complete-html
  :type 'boolean)

;;; Variables

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

(defvar ac-html-user-defined-class-list
  '())

(defvar ac-html-user-defined-id-list
  '())

(defvar ac-html-string-check-faces '(font-lock-string-face web-mode-html-attr-value-face)
  "List of string faces")

;;; Functions

(defun ac-html--load-list-from-file (filepath)
  "Return a list separated by \\n from FILEPATH."
  (let (list)
    (with-current-buffer (find-file-noselect filepath)
      (setq list (split-string (save-restriction
                                 (widen)
                                 (buffer-substring-no-properties
                                  (point-min) (point-max)))
                               "\n" t))
      (kill-buffer)
      list)))

(defvar ac-html-all-element-list
  (ac-html--load-list-from-file (expand-file-name "html-tag-list"
                                                  ac-html-basic-source-dir)))

(defun ac-source--html-tag-documentation (symbol)
  (let* ((where-to-find
          (expand-file-name "html-tag-short-docs"
                            ac-html-basic-source-dir))
         (doc-file (expand-file-name symbol where-to-find)))
    (if (file-exists-p doc-file)
        (progn
          (with-temp-buffer
            (insert-file-contents doc-file)
            (buffer-string)))
      "Currently not documented.")))

(defmacro ac-html--attribute-documentation (attribute tag)
  `(let* ((where-to-find
           (expand-file-name "html-attributes-short-docs"
                             ac-html-basic-source-dir))
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

(defun ac-html--check-string-face ()
  "t if text's face(s) at point is in `ac-html-string-check-faces'."
  (let ((faces (get-text-property (point) 'face)))
    (if (listp faces)                   ; slim-mode define list of string-face (bug), so intersect
        (intersection faces ac-html-string-check-faces)
      (memq faces ac-html-string-check-faces) ;faces is atom
      )))

(defun ac-html--attribute-candidates (tag-string)
  (unless (ac-html--check-string-face)
    (let* ((global-attributes-file
            (expand-file-name "html-attributes-list/global"
                              ac-html-basic-source-dir))
           (this-attributes-file-name
            (format "html-attributes-list/%s" tag-string))
           (this-attributes-file
            (expand-file-name this-attributes-file-name
                              ac-html-basic-source-dir))
           (list-to-return ()))

      (if (file-exists-p global-attributes-file)
          (setq list-to-return
                (append list-to-return
                        (ac-html--load-list-from-file global-attributes-file))))

      (if (file-exists-p this-attributes-file)
          (setq list-to-return
                (append list-to-return
                        (ac-html--load-list-from-file this-attributes-file))))
      list-to-return)))

(defun ac-source--html-values-internal (tag-string attribute-string)
  "Read html-stuff/html-attributes-complete/global-<ATTRIBUTE>
and html-stuff/html-attributes-complete/<TAG>-<ATTRIBUTE> files

Those files may have documantation delimited by \" \" symbol."
  (let* (
         (this-global-attribute-file-name
          (format "html-attributes-complete/global-%s" attribute-string))
         (this-global-attribute-file
          (expand-file-name this-global-attribute-file-name
                            ac-html-basic-source-dir))

         (this-concrete-atribute-file-name
          (format "html-attributes-complete/%s-%s" tag-string attribute-string))
         (this-concrete-atribute-file
          (expand-file-name this-concrete-atribute-file-name
                            ac-html-basic-source-dir))
         (list-to-return ()))

    (if (file-exists-p this-concrete-atribute-file)
        (setq list-to-return
              (append list-to-return
                      (ac-html--load-list-from-file this-concrete-atribute-file))))
    (if (file-exists-p this-global-attribute-file)
        (setq list-to-return
              (append list-to-return
                      (ac-html--load-list-from-file this-global-attribute-file))))
    list-to-return))

(defun ac-source--html-attribute-values (tag-string attribute-string)
  (if (and ac-html-style-css
           (string= attribute-string "style")
           (<				; make sure that quote openned before ac-css-prefix
            (1+ (save-excursion (re-search-backward "\"" nil t)))
            (or (ac-css-prefix) 0)))	; TODO: how to compare numbers with possible nil?
      (mapcar '(lambda(css-item)	; if attribute is "style"
                 (popup-make-item css-item
                                  :summary "CSS"))
              (ac-css-property-candidates))
    (let ( (lines (ac-source--html-values-internal tag-string attribute-string)) )
      (mapcar '(lambda(line)
                 (replace-regexp-in-string "[ ].*" "" line))
              lines))))

(defun ac-source--html-attribute-value-document (symbol tag-string attribute-string)
  (let* ( (word (if (symbolp symbol)
                    (symbol-name symbol)
                  symbol))
          (len (length word)) )
    (let ( help )
      (mapc '(lambda(line)
               (when (>= (length line) len)
                 (when (string= word (substring line 0 (length word)))
                   (setq help (substring line (length word))))))
            (ac-source--html-values-internal tag-string attribute-string))
      (replace-regexp-in-string "\\\\n" "\n" (substring help 1)))))

;;; auto complete HTML for html-mode and web-mode

(defun ac-html--current-html-tag ()
  "Return current html tag user is typing on."
  (let* ((tag-search (save-excursion
                       (re-search-backward "<\\(\\w+\\)[[:space:]]+" nil t)))
         (tag-string (match-string 1)))
    tag-string))

(defun ac-html--current-html-attribute ()
  "Return current html tag's attribute user is typing on."
  (let* ((tag-search (save-excursion
                       (re-search-backward "[^a-z-]\\([a-z-]+\\)=" nil t)))
         (tag-string (match-string 1)))
    tag-string))

;; ac-source functions

(defun ac-source-html-tag-candidates ()
  ac-html-all-element-list)

(defun ac-source-html-attribute-candidates ()
  (ac-html--attribute-candidates (ac-html--current-html-tag)))

(defun ac-source-html-attribute-documentation (symbol)
  (ac-html--attribute-documentation symbol
                                    (ac-html--current-html-tag)))

(defun ac-source-html-attribute-value-candidates ()
  (ac-source--html-attribute-values
   (ac-html--current-html-tag) (ac-html--current-html-attribute)))

(defun ac-source-html-attribute-value-document (symbol)
  (ac-source--html-attribute-value-document symbol
                                            (ac-html--current-html-tag) (ac-html--current-html-attribute)))

(defun ac-html-value-prefix ()
  (if (re-search-backward "\\w=[\"]\\([^\"]+[ ]\\|\\)\\(.*\\)" nil t)
      (match-beginning 2)))

;;;###autoload
(defvar ac-source-html-tag
  '((candidates . ac-source-html-tag-candidates)
    (prefix . "<\\(.*\\)")
    (symbol . "t")
    (document . ac-source--html-tag-documentation)))

;;;###autoload
(defvar ac-source-html-attribute
  '((candidates . ac-source-html-attribute-candidates)
    (prefix . "<\\w[^>]*[[:space:]]+\\(.*\\)")
    (symbol . "a")
    (document . ac-source-html-attribute-documentation)))

;;;###autoload
(defvar ac-source-html-attribute-value
  '((candidates . ac-source-html-attribute-value-candidates)
    (prefix . ac-html-value-prefix)
    (document . ac-source-html-attribute-value-document)
    (symbol . "v")))

;;;###autoload
(defun ac-html-enable ()
  "Add ac-html sources into ac-sources and enable auto-comple-mode."
  (interactive)
  (mapc (lambda (source)
          (if (not (memq source ac-sources))
              (add-to-list 'ac-sources source)))
        '(ac-source-html-attribute-value ac-source-html-attribute ac-source-html-tag))

  ;; ac-source-jade-attribute-value complete in font-lock-string-face, must not be disabled
  (make-local-variable 'ac-disable-faces)
  (setq ac-disable-faces (remove 'font-lock-string-face ac-disable-faces))
  (auto-complete-mode t))

(provide 'ac-html)
;;; ac-html.el ends here
