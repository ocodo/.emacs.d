;;; goose-theme.el --- A gray color theme

;; Copyright (c) 2016 Stephen Whipple

;; Author: Stephen Whipple <shw@wicdmedia.org>
;; URL: https://github.com/thwg/goose-theme
;; Package-Version: 20160828.545
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.1"))

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

;; A low-contrast Emacs theme that reduces unnecessary coloring,
;; especially when editing source code.

;; This Emacs theme pairs well with the Greybird desktop theme.

;;; Code:

(deftheme goose "A gray color theme")

(custom-theme-set-variables
 'goose
 '(ansi-color-names-vector
   ["#111111" "#AA0000" "#00AA00" "#AA5500" "#0000AA" "#AA00AA" "#00AAAA" "#EDEDED"]))

(custom-theme-set-faces
 'goose
 '(default ((t (:inherit nil :stipple nil :background "#CECECE" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal))))
 '(error ((t (:foreground "#AA0000" :weight bold))))
 '(fixed-pitch ((t nil)))
 '(font-lock-builtin-face ((t (:weight normal))))
 '(font-lock-comment-face ((t (:foreground "#444444"))))
 '(font-lock-comment-face ((t (:foreground "#555555"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(font-lock-preprocessor-face ((t (:weight bold))))
 '(font-lock-string-face ((t (:slant italic))))
 '(font-lock-type-face ((t nil)))
 '(font-lock-warning-face ((t (:inherit error))))
 '(font-lock-variable-name-face ((t nil)))
 '(fringe ((t (:background "#CECECE"))))
 '(header-line ((t (:inherit mode-line))))
 '(highlight ((t (:background "#B0B0B0"))))
 '(link ((t (:foreground "#0000AA" :underline t))))
 '(link-visited ((t (:inherit link :foreground "#440044"))))
 '(minibuffer-prompt ((t (:weight bold))))
 '(mode-line ((t (:background "#A0A0A0"))))
 '(mode-line-inactive ((t (:inherit mode-line))))
 '(region ((t (:background "#C0C0C0"))))
 '(secondary-selection ((t (:background "#C0C0C0"))))
 '(underline ((t nil)))
 '(variable-pitch ((t nil)))
 '(warning ((t (:inherit error))))
 '(widget-button ((t (:inherit link))))
 '(widget-field ((t (:background "#DEDEDE"))))
 ;; avy
 '(avy-lead-face ((t (:background "#333333" :foreground "#FFFFFF"))))
 ;; clojure
 '(clojure-interop-method-face ((t nil)))
 ;; compilation
 '(compilation-column-number ((t nil)))
 '(compilation-line-number ((t nil)))
 ;; custom
 '(custom-button ((t (:inherit link))))
 '(custom-group-tag ((t (:inherit custom-variable-tag))))
 '(custom-state ((t nil)))
 '(custom-variable-tag ((t (:weight bold))))
 '(custom-visibility ((t (:inherit link))))
 ;; dired
 '(dired-directory ((t (:foreground "#0000AA" :weight bold))))
 ;; epa
 '(epa-validity-disabled ((t (:inherit error))))
 ;; erc
 '(erc-current-nick-face ((t (:weight bold))))
 '(erc-direct-msg-face ((t nil)))
 '(erc-error-face ((t (:inherit error))))
 '(erc-nick-msg-face ((t (:weight bold))))
 '(erc-notice-face ((t nil)))
 '(erc-prompt-face ((t (:weight bold))))
 '(erc-timestamp-face ((t (:weight bold))))
 ;; eshell
 '(eshell-ls-directory ((t (:foreground "#0000AA" :weight bold))))
 '(eshell-ls-executable ((t (:foreground "#007700" :weight bold))))
 '(eshell-ls-symlink ((t (:foreground "#0077AA" :weight bold))))
 '(eshell-prompt ((t (:weight bold))))
 ;; eww
 '(eww-form-submit ((t (:inherit widget-button))))
 '(eww-form-text ((t (:inherit widget-field))))
 ;; flyspell
 '(flyspell-duplicate ((t (:inherit flyspell-incorrect))))
 '(flyspell-incorrect ((t (:underline (:color "#AA0000" :style wave)))))
 ;; gnus
 '(gnus-button ((t (:inherit widget-button))))
 '(gnus-cite-1 ((t (:foreground "#222222"))))
 '(gnus-cite-2 ((t (:foreground "#444444"))))
 '(gnus-cite-3 ((t (:foreground "#666666"))))
 '(gnus-cite-4 ((t (:foreground "#888888"))))
 '(gnus-cite-5 ((t (:foreground "#999999"))))
 '(gnus-cite-6 ((t (:foreground "#999999"))))
 '(gnus-cite-7 ((t (:foreground "#999999"))))
 '(gnus-cite-8 ((t (:foreground "#999999"))))
 '(gnus-cite-9 ((t (:foreground "#999999"))))
 '(gnus-cite-10 ((t (:foreground "#999999"))))
 '(gnus-cite-11 ((t (:foreground "#999999"))))
 '(gnus-group-mail-3 ((t (:weight bold))))
 '(gnus-header-content ((t nil)))
 '(gnus-header-name ((t (:weight bold))))
 '(gnus-header-subject ((t nil)))
 '(gnus-summary-normal-read ((t nil)))
 '(gnus-summary-normal-unread ((t (:weight bold))))
 '(gnus-summary-selected ((t (:background "#DEDEDE"))))
 ;; info
 '(Info-quoted ((t (:inherit font-lock-string-face))))
 '(info-header-node ((t (:inherit font-lock-keyword-face))))
 '(info-menu-header ((t (:inherit font-lock-keyword-face))))
 '(info-menu-star ((t nil)))
 '(info-title-1 ((t (:inherit outline-1))))
 '(info-title-2 ((t (:inherit outline-2))))
 '(info-title-3 ((t (:inherit outline-3))))
 '(info-title-4 ((t (:inherit outline-4))))
 '(info-xref ((t (:inherit link))))
 '(info-xref-visited ((t (:inherit (link-visited info-xref)))))
 ;; ivy
 '(ivy-current-match ((t (:inherit highlight))))
 '(ivy-modified-buffer ((t (:weight bold))))
 ;; js2
 '(js2-error ((t (:inherit error))))
 '(js2-external-variable ((t nil)))
 '(js2-function-param ((t nil)))
 '(js2-instance-member ((t nil)))
 '(js2-jsdoc-html-tag-delimiter ((t nil)))
 '(js2-jsdoc-html-tag-name ((t nil)))
 '(js2-jsdoc-tag ((t (:inherit font-lock-preprocessor-face))))
 '(js2-jsdoc-type ((t (:inherit font-lock-doc-face))))
 '(js2-jsdoc-value ((t (:inherit font-lock-doc-face))))
 '(js2-private-function-call ((t nil)))
 '(js2-private-member ((t nil)))
 '(js2-warning ((t (:inherit warning))))
 ;; magit
 '(magit-section-highlight ((t (:background "#DEDEDE"))))
 ;; markdown
 '(markdown-markup-face ((t (:foreground "#333333"))))
 ;; mu4e
 '(mu4e-header-highlight-face ((t (:background "#B0B0B0"))))
 '(mu4e-header-key-face ((t (:weight bold))))
 ;; org
 '(org-block ((t (:inherit org-block-begin-line))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))) t)
 '(org-document-info ((t (:inherit org-meta-line))))
 '(org-document-info-keyword ((t (:inherit org-meta-line))))
 '(org-document-title ((t (:inherit org-meta-line :weight bold))))
 '(org-footnote ((t (:foreground "#772277"))))
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-level-2 ((t (:inherit outline-2))))
 '(org-level-3 ((t (:inherit outline-3))))
 '(org-level-4 ((t (:inherit outline-4))))
 '(org-level-5 ((t (:inherit outline-5))))
 '(org-level-6 ((t (:inherit outline-6))))
 '(org-level-7 ((t (:inherit outline-7))))
 '(org-level-8 ((t (:inherit outline-8))))
 '(org-table ((t nil)))
 '(org-verbatim ((t (:inherit font-lock-comment-face))))
 ;; outline
 '(outline-1 ((t (:weight bold))))
 '(outline-2 ((t (:weight bold))))
 '(outline-3 ((t (:weight bold))))
 '(outline-4 ((t (:weight bold))))
 '(outline-5 ((t (:weight bold))))
 '(outline-6 ((t (:weight bold))))
 '(outline-7 ((t (:weight bold))))
 '(outline-8 ((t (:weight bold))))
 ;; rcirc
 '(rcirc-bright-nick ((t (:inherit font-lock-keyword-face))))
 '(rcirc-dim-nick ((t (:inherit font-lock-comment-face))))
 '(rcirc-keyword ((t (:inherit font-lock-keyword-face))))
 '(rcirc-my-nick ((t (:inherit rcirc-other-nick))))
 '(rcirc-nick-in-message ((t (:inherit rcirc-keyword))))
 '(rcirc-nick-in-message-full-line ((t nil)))
 '(rcirc-other-nick ((t nil)))
 '(rcirc-prompt ((t (:inherit font-lock-keyword-face))))
 '(rcirc-server ((t (:foreground "#006600"))))
 '(rcirc-track-keyword ((t (:inherit rcirc-keyword))))
 '(rcirc-track-nick ((t (:inherit rcirc-track-keyword))))
 '(rcirc-url ((t (:inherit link))))
 ;; rst
 '(rst-level-1 ((t (:inherit outline-1))))
 '(rst-level-2 ((t (:inherit outline-2))))
 '(rst-level-3 ((t (:inherit outline-3))))
 ;; show-paren
 '(show-paren-match ((t (:background "#AAAAAA"))))
 '(show-paren-mismatch ((t (:background "#444444" :foreground "#FFFFFF"))))
 ;; sx
 '(sx-custom-button ((t (:inherit custom-button))))
 ;; term
 '(term-color-black ((t (:foreground "#111111"))))
 '(term-color-red ((t (:foreground "#AA0000"))))
 '(term-color-green ((t (:foreground "#00AA00"))))
 '(term-color-yellow ((t (:foreground "#AA5500"))))
 '(term-color-blue ((t (:foreground "#0000AA"))))
 '(term-color-magenta ((t (:foreground "#AA00AA"))))
 '(term-color-cyan ((t (:foreground "#00AAAA"))))
 '(term-color-white ((t (:foreground "#EDEDED"))))
 ;; web-mode
 '(web-mode-comment-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(web-mode-css-selector-face ((t nil)))
 '(web-mode-doctype-face ((t (:inherit font-lock-keyword-face))))
 '(web-mode-html-attr-name-face ((t nil)))
 '(web-mode-html-tag-face ((t nil)))
 '(web-mode-json-key-face ((t (:inherit font-lock-string-face))))
 ;; whitespace
 '(whitespace-empty ((t nil)))
 '(whitespace-indentation ((t (:foreground "#AAAAAA"))))
 '(whitespace-line ((t nil)))
 '(whitespace-newline ((t (:foreground "#AAAAAA"))))
 '(whitespace-space ((t (:foreground "#999999"))))
 '(whitespace-space-after-tab ((t (:foreground "#AAAAAA"))))
 '(whitespace-tab ((t (:foreground "#AAAAAA"))))
 '(whitespace-trailing ((t (:inherit whitespace-space))))
 ;; window-divider
 '(window-divider ((t (:foreground "#A0A0A0"))))
 '(window-divider-first-pixel ((t (:inherit window-divider))))
 '(window-divider-last-pixel ((t (:inherit window-divider)))))

;; Add theme directory to `custom-theme-load-path'
;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'goose)

;;; goose-theme.el ends here
