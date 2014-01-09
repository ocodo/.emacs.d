;;; sea-before-storm-theme.el --- Sea Before Storm color theme for Emacs 24

;; Author:  Michael Lee <lee@soi2.org>
;; Version: 0.3

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(deftheme sea-before-storm
    "Dark, mostly blue and green hued theme created by Michael Lee.")

(custom-theme-set-faces
 'sea-before-storm

 ;;; basic faces

 '(default ((t (:foreground "grey60" :background "grey5"))))
 '(cursor ((t (:inverse-video t :background "gainsboro"))))
 '(bold ((t (:foreground "white" :weight bold))))
 '(italic ((t (:foreground "white" :slant italic))))
 '(bold-italic ((t (:foreground "white" :weight normal :slant normal))))
 '(escape-glyph ((t (:weight bold))))
 '(underline ((t (:underline t))))
 '(fringe ((t (:background "grey8"))))
 '(highlight ((t (:background "gold" :foreground "black"))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "turquoise"))))
 '(link-visited ((t (:foreground "steel blue" :inherit link))))
 '(region ((t (:background "grey15"))))
 '(secondary-selection ((t (:background "grey25"))))
 '(scroll-bar ((t (:background "grey30"))))
 '(border ((t (:background "black"))))
 '(vertical-border ((t (:background "white"))))
 '(button ((t (:foreground "turquoise"))))
 '(show-paren-match-face ((t (:background "dark orange" :foreground "black"))))
 '(show-paren-mismatch-face ((t (:background "red" :foreground "black"))))
 '(trailing-whitespace ((t (:background "firebrick"))))
 '(hl-line ((t (:background "grey8" :inherit nil))))
 '(isearch ((t (:background "orange" :foreground "black"))))
 '(lazy-highlight ((t (:background "yellow" :foreground "black"))))
 '(isearch-fail ((t (:background "firebrick" :foreground "black"))))
 '(query-replace ((t (:inherit (isearch)))))
 '(next-error ((t (:inherit (region)))))
 '(compilation-error ((t (:foreground "firebrick"))))
 '(Man-overstrike ((t (:foreground "goldenrod"))))

 ;;; fontlock

 '(font-lock-builtin-face ((t (:foreground "cyan4"))))
 '(font-lock-comment-face ((t (:foreground "medium sea green" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:foreground "sea green"))))
 '(font-lock-constant-face ((t (:foreground "pale turquoise"))))
 '(font-lock-doc-face ((t (:foreground "medium sea green"))))
 '(font-lock-function-name-face ((t (:foreground "cyan"))))
 '(font-lock-keyword-face ((t (:foreground "steel blue"))))
 '(font-lock-negation-char-face ((t (:inherit font-lock-constant-face))))
 '(font-lock-preprocessor-face ((t (:foreground "light slate grey"))))
 '(font-lock-reference-face ((t (:foreground "turquoise"))))
 '(font-lock-string-face ((t (:foreground "deepskyblue4" :slant italic))))
 '(font-lock-type-face ((t (:foreground "cyan4"))))
 '(font-lock-variable-name-face ((t (:foreground "cyan3"))))
 '(font-lock-warning-face ((t (:foreground "sienna4"))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold :inherit (bold)))))

 ;;; other mode specific faces

 ;; buffer-menu
 '(buffer-menu-buffer ((t (:foreground "steel blue" :weight normal))))

 ;; comint
 '(comint-highlight-input ((t (:foreground "ghost white"))))
 '(comint-highlight-prompt ((t (:foreground "sea green"))))

 ;; diff
 `(diff-added ((t (:foreground "green4" :background nil))))
 `(diff-changed ((t (:foreground "gold" :background nil))))
 `(diff-removed ((t (:foreground "firebrick" :background nil))))
 `(diff-header ((t (:foreground "khaki" :background nil))))
 `(diff-file-header ((t (:foreground "white" :background nil :weight bold))))

 ;; dired
 '(dired-marked ((t (:foreground "orange" :weight normal))))

 ;; eshell
 '(eshell-ls-directory ((t (:foreground "turquoise"))))
 '(eshell-ls-symlink ((t (:foreground "dark orange"))))
 '(eshell-ls-executable ((t (:foreground "lime green"))))
 '(eshell-ls-special ((t (:foreground "medium purple"))))
 '(eshell-prompt ((t (:foreground "goldenrod"))))

 ;; flymake
 '(flymake-errline-face ((t (:background "firebrick"))))
 '(flymake-warnline-face ((t (:background "dark orange"))))

 ;; flyspell
 '(flyspell-duplicate-face ((t (:foreground nil :underline t))))
 '(flyspell-incorrect-face ((t (:foreground nil :underline t))))

 ;; ido
 '(ido-subdir ((t (:inherit eshell-ls-directory))))
 '(ido-first-match ((t (:foreground "yellow"))))
 '(ido-only-match ((t (:foreground "spring green"))))

 ;; iedit
 '(iedit-occurrence ((t (:background "dark orange" :foreground "black"))))

 ;; infodoc
 '(info-xref ((t (:foreground "turquoise"))))
 '(info-xref-visited ((t (:foreground "steel blue"))))
 '(info-menu-star ((t (:foreground "grey78"))))

 ;; magit
 '(magit-item-highlight ((t (:background "grey10" :foreground nil :inherit nil))))

 ;; minibuffer
 '(minibuffer-prompt ((t (:foreground "medium purple"))))

 ;; modeline
 '(mode-line ((t (:foreground "white" :background "grey5"
                             :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t (:foreground "deep pink"))))
 '(mode-line-inactive ((t (:foreground "grey16" :background "grey8" :box nil))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t :foreground nil)))

 ;; markdown-mode
 '(markdown-bold-face ((t (:inherit bold))))
 '(markdown-italic-face ((t (:inherit italic))))
 '(markdown-header-face ((t (:inherit org-level-1))))
 '(markdown-link-face ((t (:inherit org-link))))
 '(markdown-reference-face ((t (:foreground "steel blue"))))
 '(markdown-url-face ((t (:inherit org-link))))
 '(markdown-pre-face ((t (:inherit org-verbatim))))
 '(markdown-inline-code-face ((t (:inherit org-verbatim))))
 '(markdown-list-face ((t (:inherit bold))))
 
 ;; org-mode
 '(org-document-title ((t (:foreground "cyan" :weight normal))))
 '(org-document-info-keyword ((t (:inherit org-meta-line :foreground nil))))
 '(org-code ((t (:foreground "goldenrod"))))
 '(org-verbatim ((t (:foreground "goldenrod"))))
 '(org-link ((t (:inherit link))))
 '(org-table ((t (:foreground "grey78"))))
 '(org-block-begin-line ((t (:foreground "dim grey"))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))))
 '(org-block ((t (:foreground "goldenrod"))))
 '(org-level-1 ((t (:foreground "dodger blue" :weight normal))))
 '(org-level-2 ((t (:inherit org-level-1))))
 '(org-level-3 ((t (:inherit org-level-1))))
 '(org-level-4 ((t (:inherit org-level-1))))
 '(org-level-5 ((t (:inherit org-level-1))))
 '(org-list-dt ((t (:foreground "white" :weight normal))))
 '(org-meta-line  ((t (:foreground "medium purple"))))
 '(org-tag ((t (:foreground "dark orange" :weight normal))))
 '(org-todo ((t (:foreground "firebrick" :weight normal))))
 '(org-done ((t (:foreground "forest green" :weight normal))))
 '(org-checkbox ((t (:foreground "azure" :weight normal))))
 '(org-checkbox-statistics-todo ((t (:foreground "lime green" :weight normal))))
 '(org-checkbox-statistics-done ((t (:inherit org-checkbox-statistics-todo))))

 ;; slime
 '(slime-repl-output-face ((t (:foreground "springgreen4"))))
 '(slime-repl-inputed-output-face ((t (:foreground "turquoise"))))
 '(slime-inspector-value-face ((t (:foreground "goldenrod")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'sea-before-storm)

;;; sea-before-storm-theme.el ends here
