;;; ac-html.el --- auto complete source for html tags and attributes

;; Copyright (C) 2014 Zhang Kai Yu

;; Author: Zhang Kai Yu <yeannylam@gmail.com>
;; Version: 0.3
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

(defconst ac-html-package-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "The directory where `ac-html' package exists.")

(defconst ac-html-basic-source-dir
  (expand-file-name "html-stuff" ac-html-package-dir)
  "The directory where basic completion source of `ac-html' exists.")

;;; Customization

(defgroup auto-complete-html nil
  "HTML Auto Complete."
  :group 'auto-complete
  :prefix "ac-html-")

(defcustom ac-html-source-dirs
  '(("html" . ac-html-basic-source-dir))
  "Alist support for multisource directories. 
car is source name, cdr is source location."
  :type 'alist
  :group 'auto-complete-html)

(defcustom ac-html-complete-css t
  "Enable style attribute CSS autocomplete."
  :group 'auto-complete-html
  :type 'boolean)

(defcustom ac-html-summary-truncate-length 10
  "Truncation length for type summary."
  :type 'integer
  :group 'auto-complete-html)

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
  "List of string faces to check.")

;;; Functions

(defun ac-html--load-list-from-file (filepath)
  "Return a list separated by \\n from FILEPATH."
  (with-current-buffer (find-file-noselect filepath)
    (unwind-protect
        (split-string (save-restriction
                        (widen)
                        (buffer-substring-no-properties
                         (point-min) (point-max)))
                      "\n" t)
      (kill-buffer))))

(defun ac-html--all-files-named (file-name)
  "Get a list of file named FILE-NAME in all directory specified by
 `ac-html-source-dirs'.

Returns an alist. car is source name, cdr is the file path."
  (let (return-files source-dir-path)
    (mapc (lambda (name-dir-cons-cell)
            (setq source-dir-path (cdr name-dir-cons-cell))
            (setq source-dir-path
                  (cond ((stringp source-dir-path) source-dir-path)
                        ((and (symbolp source-dir-path)
                              (boundp source-dir-path))
                         (symbol-value source-dir-path))
                        (t
                         (error "[ac-html] invalid element %s in\
 `ac-html-source-dirs'" source-dir-path))))
            (when source-dir-path
              (setq source-dir-path (expand-file-name file-name source-dir-path))
              (when (file-exists-p source-dir-path)
                (add-to-list 'return-files (cons (car name-dir-cons-cell) source-dir-path))
                )))
          ac-html-source-dirs)
    return-files))

(defun ac-html--flatten (wtf)
  "Flatten WTF, into a list."
  (cond ((null wtf) nil)
        ((atom wtf) (list wtf))
        (t (append (ac-html--flatten (car wtf))
                   (ac-html--flatten (cdr wtf))))))

(defun ac-html--make-popup-items (summary items documentation)
  "Make popup-item for each item with SUMMARY.

SUMMARY will be truncated to `ac-html-summary-truncate-length'.

ITEMS is a list of string where name and documentation are 
separated by one space.
Documentation newlines are escaped by \"\\n\".

If item have no inline documentation, DOCUMENTATION will be used.
DOCUMENTATION is string or function."
  (let ((truncated-summary
         (truncate-string-to-width
          summary ac-html-summary-truncate-length 0 nil nil)))
    (mapcar (lambda (item)
              (if (string-match "\\(.*?\\) \\(.*\\)" item)
                  (popup-make-item (match-string 1 item)
                                   :summary truncated-summary
                                   :document (replace-regexp-in-string
                                              "\\\\n" "\n"
                                              (match-string 2 item)))
                (popup-make-item item
                                 :summary truncated-summary
                                 :document documentation)))
            items)))

(defun ac-html--read-file (file-in-source-dir)
  "Return string content of FILE-IN-SOURCE-DIR from `ac-html-source-dirs'."
  (let ((file (cdr (nth 0 (ac-html--all-files-named file-in-source-dir)))))
    ;; Just read from the first file.
    (when file
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun ac-html--tags ()
  (ac-html--flatten
   (mapcar (lambda (source-name-and-file-path)
             (ac-html--make-popup-items
              (car source-name-and-file-path)
              (ac-html--load-list-from-file (cdr source-name-and-file-path))
              (lambda (tag-name)
                (let ((doc (ac-html--read-file
                            (concat "html-tag-short-docs/" tag-name))))
                  (if doc
                      doc
                    "Currently not documented.")))))
           (ac-html--all-files-named "html-tag-list"))))

(defun ac-html--attribute-documentation (attribute tag)
  (let* ((doc-file (format "html-attributes-short-docs/%s-%s" tag attribute))
         (doc (ac-html--read-file doc-file)))
    (if doc
        doc
      (progn
        (setq doc-file (format "html-attributes-short-docs/global-%s" attribute))
        (setq doc (ac-html--read-file doc-file))
        (if doc
            doc
          "Currently not documented.")))))

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

(defun ac-html--check-string-face ()
  "t if text's face(s) at point is in `ac-html-string-check-faces'."
  (let ((faces (get-text-property (point) 'face)))
    (if (listp faces)
        ;; slim-mode define list of string-face (bug), so intersect
        (intersection faces ac-html-string-check-faces)
      (memq faces ac-html-string-check-faces))))

(defun ac-html--attribute-candidates (tag-string document)
  "Attribute candidates for auto complete."
  (unless (ac-html--check-string-face)
    (let* ((items
            (mapcar (lambda (source-name-and-file-path)
                      (ac-html--make-popup-items
                       (concat (car source-name-and-file-path) ", G")
                       (ac-html--load-list-from-file
                        (cdr source-name-and-file-path))
                       document
                       ))
                    (ac-html--all-files-named "html-attributes-list/global"))))
      (add-to-list 'items
                   (mapcar (lambda (source-name-and-file-path)
                             (ac-html--make-popup-items
                              (car source-name-and-file-path)
                              (ac-html--load-list-from-file
                               (cdr source-name-and-file-path))
                              document
                              ))
                           (ac-html--all-files-named
                            (concat "html-attributes-list/" tag-string))))
      (ac-html--flatten items))))

(defun ac-source--html-values-internal (tag-string attribute-string)
  "Read html-stuff/html-attributes-complete/global-<ATTRIBUTE>
and html-stuff/html-attributes-complete/<TAG>-<ATTRIBUTE> files

Those files may have documantation delimited by \" \" symbol."

  (let* ((items (mapcar
                 (lambda (alist)
                   (ac-html--make-popup-items
                    (concat (car alist) ", G")
                    (ac-html--load-list-from-file (cdr alist))
                    nil))
                 (ac-html--all-files-named
                  (concat "html-attributes-complete/global-"
                          attribute-string)))))
    (add-to-list 'items
                 (mapcar (lambda (alist)
                           (ac-html--make-popup-items
                            (car alist)
                            (ac-html--load-list-from-file (cdr alist))
                            nil))
                         (ac-html--all-files-named
                          (format "html-attributes-complete/%s-%s" tag-string
                                  attribute-string))))
    (ac-html--flatten items)))

(defun ac-source--html-attribute-values (tag-string attribute-string)
  (if (and ac-html-complete-css
           (string= attribute-string "style")
           (< ;; make sure that quote openned before ac-css-prefix
            (1+ (save-excursion (re-search-backward "\"" nil t)))
            (or (ac-css-prefix) 0)))
      ;; TODO: how to compare numbers with possible nil?
      (ac-html--make-popup-items "CSS" (ac-css-property-candidates) nil)
    (ac-source--html-values-internal tag-string attribute-string)))

;;; auto complete HTML for html-mode and web-mode

(defun ac-html--current-html-tag ()
  "Return current html tag user is typing on."
  (save-excursion
    (re-search-backward "<\\(\\w+\\)[[:space:]]+" nil t)
    (match-string 1)))

(defun ac-html--current-html-attribute ()
  "Return current html tag's attribute user is typing on."
  (save-excursion
    (re-search-backward "[^a-z-]\\([a-z-]+\\)=" nil t)
    (match-string 1)))

;; ac-source functions

(defun ac-html-current-tag ()
  "Return current html tag user is typing on."
  (ac-html--current-html-tag))

(defun ac-html-current-attribute ()
  "Return current html tag's attribute user is typing on."
  (ac-html--current-html-attribute))

(defun ac-source-html-tag-candidates ()
  (ac-html--tags))

(defun ac-source-html-attribute-candidates ()
  (ac-html--attribute-candidates (ac-html-current-tag)
                                 (lambda (symbol)
                                   (ac-html--attribute-documentation
                                    symbol (ac-html-current-tag)))))

(defun ac-source-html-attribute-value-candidates ()
  (ac-source--html-attribute-values
   (ac-html-current-tag) (ac-html-current-attribute)))

(defun ac-html-value-prefix ()
  (if (re-search-backward "\\w=[\"]\\([^\"]+[ ]\\|\\)\\(.*\\)" nil t)
      (match-beginning 2)))

;;;###autoload
(defvar ac-source-html-tag
  '((candidates . ac-source-html-tag-candidates)
    (prefix . "<\\(.*\\)")
    (symbol . "t")))

;;;###autoload
(defvar ac-source-html-attribute
  '((candidates . ac-source-html-attribute-candidates)
    (prefix . "<\\w[^>]*[[:space:]]+\\(.*\\)")
    (symbol . "a")))

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
        '(ac-source-html-attribute-value ac-source-html-attribute
                                         ac-source-html-tag))

  ;; ac-source-jade-attribute-value complete in font-lock-string-face,
  ;; must not be disabled
  (make-local-variable 'ac-disable-faces)
  (setq ac-disable-faces (remove 'font-lock-string-face ac-disable-faces))
  (auto-complete-mode t))

(provide 'ac-html)
;;; ac-html.el ends here
