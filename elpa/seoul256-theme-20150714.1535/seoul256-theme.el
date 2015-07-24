;;; seoul-theme.el --- Low-contrast dark emacs color scheme using Seoul colours

;; Copyright (C) 2015 by Chris Davison

;; Author: Chris Davison <c.jr.davison@gmail.com>
;; URL: 
;; Package-Version: 20150714.1535
;; Package-X-Original-Version: 20150714.0
;; Version: 0.01
;; License: MIT

;; Emacs-clone of the seoul256 theme (Dark version only)
;; Originally created by Junegunn Choi for Vim.
;; URL: github.com/junegunn/seoul256.vim

;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;;; Code:

(deftheme seoul256
  "seoul256 theme")

(custom-theme-set-faces
 'seoul256

 '(default ((t (:background "#4b4b4b" :foreground "#d9d9d9"))))

 '(bold ((t (:weight bold))))
 '(bold-italic ((t (:weight bold :slant italic))))
 '(custom-face-tag ((t (:foreground "#66D9EF" :weight bold))))
 '(custom-state ((t (:foreground "#FFBD98"))))
 '(italic ((t (:slant italic))))
 '(region ((t (:background "#007173"))))
 '(underline ((t (:underline t))))

 '(css-selector ((t (:foreground "#ffc0de"))))
 '(css-property ((t (:foreground "#66D9EF"))))

 '(diff-added ((t (:foreground "#FFBD98" :weight bold))))
 '(diff-context ((t (:foreground "#F8F8F2"))))
 '(diff-file-header ((t (:foreground "#66D9EF" :background nil))))
 '(diff-indicator-added ((t (:foreground "#FFBD98"))))
 '(diff-indicator-removed ((t (:foreground "#ffc0de"))))
 '(diff-header ((t (:foreground "#F8F8F2" :background "#232526"))))
 '(diff-hunk-header ((t (:foreground "#AE81FF" :background "#232526"))))
 '(diff-removed ((t (:foreground "#ffc0de" :weight bold))))

 '(escape-glyph ((t (:foreground "#E6DB74"))))
 '(minibuffer-prompt ((t (:foreground "#66D9EF"))))
 '(mode-line ((t (:foreground "#F8F8F2" :background "#000000"
                              :box (:line-width 1 :color "#000000" :style released-button)))))
 '(mode-line-buffer-id ((t (:foreground nil :background "#000000" :weight semi-bold))))
 '(mode-line-inactive ((t (:foreground "#BCBCBC" :background "#000000"
                                       ':box (:line-width 1 :color "#232526")))))
 '(mode-line-mousable ((t (:foreground "#BCBCBC" :background "#000000"))))
 '(mode-line-mousable-minor-mode ((t (:foreground "#BCBCBC" :background "#000000"))))

 '(font-lock-builtin-face ((t (:foreground "#FFBD98"))))
 '(font-lock-comment-face ((t (:foreground "#719872" :slant italic))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#719872" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#AE81FF"))))
 '(font-lock-doc-face ((t (:foreground "#E6DB74" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "#dedd99" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "#e17899"))))
 '(font-lock-negation-char-face ((t (:weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "#FFBD98"))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold))))
 '(font-lock-string-face ((t (:foreground "#98BCBD"))))
 '(font-lock-type-face ((t (:foreground "#e19772"))))
 '(font-lock-variable-name-face ((t (:foreground "#ffc0de"))))
 '(font-lock-warning-face ((t (:foreground "#FF0000" ':background "#333333"))))

 '(fringe ((t (:background "#3f3f3f"))))
 '(highlight ((t (:foreground "#000000" :background "#C4BE89"))))
 '(hl-line ((t (:background "#293739"))))

 '(icompletep-choices ((t (:foreground "#ffc0de"))))
 '(icompletep-determined ((t (:foreground "#FFBD98"))))
 '(icompletep-keys ((t (:foreground "#ffc0de"))))
 '(icompletep-nb-candidates ((t (:foreground "#AE81FF"))))

 '(isearch ((t (:foreground "#C4BE89" :background "#000000"))))
 '(isearch-fail ((t (:foreground "#FFFFFF" :background "#333333"))))

 '(lazy-highlight ((t (:foreground "#465457" :background "#000000"))))

 '(markdown-italic-face ((t (:slant italic))))
 '(markdown-bold-face ((t (:weight bold))))
 '(markdown-header-face ((t (:weight normal))))
 '(markdown-header-face-1 ((t (:foreground "#e17899"))))
 '(markdown-header-face-2 ((t (:foreground "#bcddbd"))))
 '(markdown-header-face-3 ((t (:foreground "#ffde99"))))
 '(markdown-header-face-4 ((t (:foreground "#009799"))))
 '(markdown-header-face-5 ((t (:foreground "#98bc99"))))
 '(markdown-header-face-6 ((t (:foreground "#ffdd00"))))
 '(markdown-inline-code-face ((t (:foreground "#66D9EF"))))
 '(markdown-list-face ((t (:foreground "#FFBD98"))))
 '(markdown-blockquote-face ((t (:slant italic))))
 '(markdown-pre-face ((t (:foreground "#AE81FF"))))
 '(markdown-link-face ((t (:foreground "#66D9EF"))))
 '(markdown-reference-face ((t (:foreground "#66D9EF"))))
 '(markdown-url-face ((t (:foreground "#E6DB74"))))
 '(markdown-link-title-face ((t (:foreground "#ffc0de"))))
 '(markdown-comment-face ((t (:foreground "#465457"))))
 '(markdown-math-face ((t (:foreground "#AE81FF" :slant italic))))

 '(outline-1 ((t (:foreground "#e17899"))))
 '(outline-2 ((t (:foreground "#bcddbd"))))
 '(outline-3 ((t (:foreground "#ffde99"))))
 '(outline-4 ((t (:foreground "#009799"))))
 '(outline-5 ((t (:foreground "#98bc99"))))
 '(outline-6 ((t (:foreground "#ffdd00"))))
 '(outline-7 ((t (:foreground "#bf2172"))))
 '(outline-8 ((t (:foreground "#FFBD98"))))
 
 '(helm-source-header ((t (:foreground "#000000" :background "#e17899" ))))
 '(helm-selection ((t (:background "#007173" :foreground "#d9d9d9"))))
 '(helm-visible-mark ((t (:background "#007173" :foreground "#d9d9d9"))))

 '(link ((t (:foreground  "#AE81FF" :underline t))))


 '(secondary-selection ((t (:background "#4b4b4b"))))
 '(show-paren-match-face ((t (:foreground "#000000" :background "#FD971F"))))
 '(show-paren-mismatch-face ((t (:foreground "#960050" :background "#1E0010"))))
 '(widget-inactive-face ((t (:background "#ff0000"))))
)

;;; autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'seoul256)

;;; seoul256-theme.el ends here
