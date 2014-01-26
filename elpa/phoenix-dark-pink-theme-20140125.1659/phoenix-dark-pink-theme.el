;;; phoenix-dark-pink-theme.el --- Port of the Sublime Text 2 theme of the same name

;; Copyright 2013-2014 J Irving

;; Author: J Irving <j@lollyshouse.ca>
;; URL: http://github.com/j0ni/phoenix-dark-pink
;; Version: 20140125.1659
;; X-Original-Version: 1.1

;; Org-mode mods from Rikard Glans - https://github.com/darrik/phoenix-dark-pink

;; Code:

(unless (>= 24 emacs-major-version)
  (error "phoenix-dark-pink-theme requires Emacs 24 or later."))

(deftheme phoenix-dark-pink
  "Phoenix Dark Pink color theme")

(custom-theme-set-faces
 'phoenix-dark-pink

 '(default ((t (:inherit nil
                :stipple nil
                :background "#101010"
                :foreground "#cccccc"
                :inverse-video nil
                :box nil
                :strike-through nil
                :overline nil
                :underline nil
                :slant normal
                :weight normal
                :width normal))))

 '(cursor ((t (:background "#cccccc" :foreground "#101010" :underline nil :weight normal))))
 '(fixed-pitch ((t (:underline nil :weight normal :family "PragmataPro"))))
 '(variable-pitch ((t (:family "Sans Serif" :weight normal :underline nil))))
 '(escape-glyph ((t (:weight normal :underline nil :foreground "#d1afdd"))))
 '(minibuffer-prompt ((t (:weight normal :underline nil :foreground "#d1afdd"))))
 '(highlight ((t (:background "#31182d" :underline nil :weight normal))))
 '(region ((t (:weight normal :underline nil :background "#412b3f"))))
 '(shadow ((t (:weight normal :underline nil :foreground "grey70"))))
 '(secondary-selection ((t (:weight normal :underline nil :background "#3f3f3f"))))
 '(trailing-whitespace ((t (:background "#d1afdd" :underline nil :weight normal))))

 '(font-lock-builtin-face ((t (:weight normal :underline nil :foreground "#ddd"))))
 '(font-lock-comment-delimiter-face ((t (:weight normal :underline nil :foreground "#555" :inherit font-lock-comment-face))))
 '(font-lock-comment-face ((t (:foreground "#755273" :underline nil :weight normal))))
 '(font-lock-constant-face ((t (:foreground "#B294BB" :underline nil :weight normal))))
 '(font-lock-doc-face ((t (:weight normal :underline nil :foreground "#a582a3" :inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#EFEFEF" :underline nil :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "#d1afdd" :underline nil :weight normal))))
 '(font-lock-negation-char-face ((t (:weight normal :underline nil :foreground "#cccccc"))))
 '(font-lock-preprocessor-face ((t (:weight normal :underline nil :foreground "#d1afdd" :inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:weight normal :underline nil :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:weight normal :underline nil :inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#a582a3" :underline nil :weight normal))))
 '(font-lock-type-face ((t (:foreground "#e0e0e0" :underline nil :weight normal))))
 '(font-lock-variable-name-face ((t (:foreground "#787878" :underline nil :weight normal))))
 '(font-lock-warning-face ((t (:weight normal :underline nil :foreground "#c0af7f" :inherit (error)))))

 '(compilation-info ((t (:weight normal :foreground "#f0dfff"))))
 '(compilation-mode-line-exit ((t (:weight normal :foreground "#f0dfff"))))
 '(compilation-mode-line-fail ((t (:weight normal :foreground "#e0b0e0"))))
 '(compilation-mode-line-run ((t (:weight normal :foreground "#787878"))))

 '(warning ((t (:weight normal :foreground "#f0dfff"))))

 '(link ((t (:weight normal :underline nil :foreground "#f0dfff"))))
 '(link-visited ((t (:weight normal :underline nil :foreground "#d0bfdf" :inherit (link)))))
 '(button ((t (:foreground "#f0dfff" :underline nil :weight normal))))
 '(fringe ((t (:background "#191919" :foreground "#585858" :underline nil :weight normal))))
 '(header-line ((t (:weight normal :underline nil :inherit (mode-line)))))
 '(tooltip ((t (:weight normal :underline nil :foreground "#d1afdd" :background "#292929" :inherit (variable-pitch)))))

 '(mode-line ((t (:weight normal :underline nil :box nil :foreground "#d1afdd" :background "#2b2b2b"))))
 '(mode-line-buffer-id ((t (:weight normal :underline nil :foreground "#f0dfff"))))
 '(mode-line-emphasis ((t (:weight normal :underline nil))))
 '(mode-line-highlight ((t (:weight normal :underline nil :box nil))))
 '(mode-line-inactive ((t (:weight normal :underline nil :box nil :foreground "#a582a3" :background "#202020" :inherit (mode-line)))))

 '(isearch ((t (:weight normal :underline nil :foreground "#d1afdd" :background "#2b2b2b"))))
 '(isearch-fail ((t (:weight normal :underline nil :foreground "#d1afdd" :background "#856283"))))
 '(lazy-highlight ((t (:weight normal :underline nil :foreground "#dfafdf" :background "#2f2f2f"))))

 '(highlight-symbol-face ((t (:underline t :background "#2f2f2f"))))

 '(grep-context-face ((t (:foreground "#cccccc"))))
 '(grep-error-face ((t (:foreground "#dfafdf" :underline t))))
 '(grep-hit-face ((t (:foreground "#dfafdf"))))
 '(grep-match-face ((t (:foreground "#dfafdf"))))
 '(match ((t (:weight normal :underline nil :foreground "#dfafdf" :background "#2b2b2b"))))

 '(next-error ((t (:weight normal :underline nil :inherit (region)))))
 '(query-replace ((t (:weight normal :underline nil :inherit (isearch)))))

 '(ido-first-match ((t (:foreground "#dfafdf" :weight bold))))
 '(ido-only-match ((t (:foreground "#dfafdf" :weight bold))))
 '(ido-subdir ((t (:foreground "#a582a3"))))

 '(flx-highlight-face ((t (:foreground "#f0dfff" :weight bold))))

 '(linum ((t (:foreground "#787878"))))

 '(ac-candidate-face ((t (:background "#292929" :foreground "#a582a3"))))
 '(ac-candidate-mouse-face ((t (:background "#393939" :foreground "#d1afdd"))))
 '(ac-selection-face ((t (:background "#393939" :foreground "#d1afdd"))))
 '(ac-yasnippet-selection-face ((t (:background "#393939" :foreground "#d1afdd"))))
 '(ac-yasnippet-candidate-face ((t (:background "#292929" :foreground "#a582a3"))))
 '(popup-tip-face ((t (:background "#292929" :foreground "#a582a3"))))
 '(popup-scroll-bar-foreground-face ((t (:background "#393939"))))
 '(popup-scroll-bar-background-face ((t (:background "#101010"))))

 '(eval-sexp-fu-flash ((t (:background "#101010" :foreground "white"))))
 '(nrepl-eval-sexp-fu-flash ((t (:background "#101010" :foreground "white"))))

 '(magit-header ((t (:foreground "#dfafdf" :background "#292929" :box (:line-width 1 :color "grey20")))))
 '(magit-log-sha1 ((t (:foreground "#dfafdf" :background "#292929"))))
 '(magit-section-title ((t (:foreground "#dfafdf" :background "#101010"))))
 '(magit-branch ((t (:foreground "#d1afdd"))))
 '(magit-item-highlight ((t (:inherit (highlight-parentheses)))))
 '(magit-diff-add ((t (:foreground "#d1afdd" :background "#393939"))))
 '(magit-diff-del ((t (:foreground "#8f5f8f" :background "#202020"))))
 '(magit-diff-none ((t (:background "#101010"))))
 '(magit-diff-hunk-header ((t (:background "#292929"))))
 '(magit-diff-file-header ((t (:background "#393939"))))
 '(magit-log-author ((t (:foreground "#f0dfff"))))
 '(magit-log-head-label-remote ((t (:foreground "#f0dfff" :box t))))
 '(magit-log-head-label-local ((t (:foreground "#f0bfff" :box t))))

 '(highlight-parentheses ((t (:inherit highlight))))
 '(show-paren-match-face ((t (:inherit highlight :foreground "#101010" :background "#a582a3"))))

 '(erb-face ((t (:foreground "#cccccc" :background "#101010"))))
 '(erb-exec-face ((t (:inherit erb-face))))
 '(erb-out-face ((t (:inherit erb-face))))
 '(erb-delim-face ((t (:inherit erb-face :foreground "#efbfef" :background "#101010"))))
 '(erb-exec-delim-face ((t (:inherit erb-delim-face))))
 '(erb-out-delim-face ((t (:inherit erb-delim-face :foreground "#efbfef" :background "#101010"))))
 '(erb-comment-face ((t (:inherit erb-face :foreground "#755273" :background "#101010"))))
 '(erb-comment-delim-face ((t (:inherit erb-face :foreground "#555" :background "#101010"))))

 '(rainbow-delimiters-depth-9-face ((t (:foreground "#e1b1ed"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#dfafdf"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#d1a1dd"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#c19fcf"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#bf8fbf"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#af7faf"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#9f6f9f"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#8f5f8f"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#815f8d"))))

 '(js2-warning ((t (:foreground "#dfafdf"))))
 '(js2-error ((t (:foreground "#efbfef"))))
 '(js2-jsdoc-tag ((t (:foreground "#585858"))))
 '(js2-jsdoc-type ((t (:foreground "#787878"))))
 '(js2-jsdoc-value ((t (:foreground "#787878"))))
 '(js2-function-param ((t (:foreground "#b294bb"))))
 '(js2-external-variable ((t (:foreground "#ffbfff"))))

 '(erc-action-face ((t (:inherit erc-default-face))))
 '(erc-bold-face ((t (:weight bold))))
 '(erc-current-nick-face ((t (:foreground "#cccccc" :weight bold))))
 '(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
 '(erc-default-face ((t (:foreground "#cccccc"))))
 '(erc-direct-msg-face ((t (:inherit erc-default))))
 '(erc-error-face ((t (:inherit font-lock-warning))))
 '(erc-fool-face ((t (:inherit erc-default))))
 '(erc-highlight-face ((t (:inherit hover-highlight))))
 '(erc-input-face ((t (:foreground "#d1afdd"))))
 '(erc-keyword-face ((t (:foreground "#dfafdf" :weight bold))))
 '(erc-nick-default-face ((t (:foreground "#dfafdf" :weight bold))))
 '(erc-my-nick-face ((t (:foreground "#efefef" :weight bold))))
 '(erc-nick-msg-face ((t (:inherit erc-default))))
 '(erc-notice-face ((t (:foreground "#a883a3" :background "#101010"))))
 '(erc-pal-face ((t (:foreground "#efbfef" :weight bold))))
 '(erc-prompt-face ((t (:foreground "#d1afdd" :background "#101010" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "#787878"))))
 '(erc-underline-face ((t (:underline t))))

 '(w3m-anchor ((t (:inherit link))))
 '(w3m-arrived-anchor ((t (:foreground "#ffbfff"))))
 '(w3m-form ((t (:foreground "#a582a3" :underline t))))
 '(w3m-header-line-location-title ((t (:foreground "#efefef"
                                                   :underline t :weight bold))))
 '(w3m-history-current-url ((t (:inherit match))))
 '(w3m-lnum ((t (:foreground "#787878"))))
 '(w3m-lnum-match ((t (:background "#a582a3"
                                   :foreground "#101010"))))
 '(w3m-lnum-minibuffer-prompt ((t (:foreground "#efbfff"))))

 '(highlight-indentation-face ((t (:inherit highlight))))
 '(highlight-indentation-current-column-face ((t (:inherit highlight))))

 '(org-level-1 ((t (:foreground "#e1b1ed"))))
 '(org-level-2 ((t (:foreground "#dfafdf"))))
 '(org-level-3 ((t (:foreground "#d1a1dd"))))
 '(org-level-4 ((t (:foreground "#c19fcf"))))
 '(org-level-5 ((t (:foreground "#bf8fbf"))))
 '(org-level-6 ((t (:foreground "#af7faf"))))
 '(org-level-7 ((t (:foreground "#9f6f9f"))))
 '(org-level-8 ((t (:foreground "#8f5f8f"))))
 '(org-level-9 ((t (:foreground "#815f8d"))))
 '(org-meta-line ((t (:foreground "#714161"))))
 '(org-table ((t (:foreground "#c19fcf"))))
 '(org-document-info-keyword ((t (:foreground "#a582a3"))))
 '(org-document-title ((t (:foreground "#dfafdf"))))
 '(org-date ((t (:foreground "#a582a3")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'phoenix-dark-pink)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:

;;; phoenix-dark-pink-theme.el ends here
