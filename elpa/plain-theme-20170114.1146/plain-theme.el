;;; plain-theme.el --- Black and white theme without syntax highlighting

;; Copyright (C) 2016 Yegor Timoshenko

;; Author: Yegor Timoshenko <yegortimoshenko@gmail.com>
;; URL: https://github.com/yegortimoshenko/plain-theme
;; Package-Version: 20170114.1146
;; Version: 2

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED AS IS AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
;; REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
;; FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
;; INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
;; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
;; OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; To install, enable MELPA (http://melpa.org/#/getting-started) and execute:
;; M-x package-install plain-theme

;;; Code:

(deftheme plain)

(defvar plain-faces
  `(default
    font-lock-builtin-face
    font-lock-comment-delimiter-face
    font-lock-comment-face
    font-lock-constant-face
    font-lock-doc-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-negation-char-face
    font-lock-function-name-face
    font-lock-keyword-face
    font-lock-negation-char-face
    font-lock-preprocessor-face
    font-lock-regexp-grouping-backslash
    font-lock-regexp-grouping-construct
    font-lock-string-face
    font-lock-type-face
    font-lock-variable-name-face
    font-lock-warning-face
    web-mode-error-face
    web-mode-warning-face
    web-mode-preprocessor-face
    web-mode-block-delimiter-face
    web-mode-block-control-face
    web-mode-builtin-face
    web-mode-symbol-face
    web-mode-doctype-face
    web-mode-html-tag-face
    web-mode-html-tag-custom-face
    web-mode-html-tag-namespaced-face
    web-mode-html-tag-bracket-face
    web-mode-html-attr-name-face
    web-mode-html-attr-custom-face
    web-mode-html-attr-engine-face
    web-mode-html-attr-equal-face
    web-mode-html-attr-value-face
    web-mode-block-attr-name-face
    web-mode-block-attr-value-face
    web-mode-variable-name-face
    web-mode-css-selector-face
    web-mode-css-pseudo-class-face
    web-mode-css-at-rule-face
    web-mode-css-property-name-face
    web-mode-css-color-face
    web-mode-css-priority-face
    web-mode-css-function-face
    web-mode-css-variable-face
    web-mode-function-name-face
    web-mode-filter-face
    web-mode-function-call-face
    web-mode-string-face
    web-mode-block-string-face
    web-mode-part-string-face
    web-mode-javascript-string-face
    web-mode-css-string-face
    web-mode-json-key-face
    web-mode-json-context-face
    web-mode-json-string-face
    web-mode-comment-face
    web-mode-block-comment-face
    web-mode-part-comment-face
    web-mode-json-comment-face
    web-mode-javascript-comment-face
    web-mode-css-comment-face
    web-mode-constant-face
    web-mode-type-face
    web-mode-keyword-face
    web-mode-param-name-face
    web-mode-whitespace-face
    web-mode-inlay-face
    web-mode-block-face
    web-mode-part-face
    web-mode-script-face
    web-mode-style-face
    web-mode-folded-face
    web-mode-bold-face
    web-mode-italic-face
    web-mode-underline-face
    web-mode-current-element-highlight-face
    web-mode-current-column-highlight-face
    web-mode-comment-keyword-face
    web-mode-sql-keyword-face
    web-mode-html-entity-face
    web-mode-jsx-depth-1-face
    web-mode-jsx-depth-2-face
    web-mode-jsx-depth-3-face
    web-mode-jsx-depth-4-face
    fringe
    mode-line))

(let ((black "#000")
      (white "#fff"))
  (apply 'custom-theme-set-faces 'plain
   `(mode-line ((t (:box (:line-width 1)))))
   `(cursor ((t (:background ,black :foreground ,white))))
    (mapcar (lambda (n) `(,n ((t (:background ,white :foreground ,black))))) plain-faces)))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'plain)

;;; plain-theme.el ends here
