;;; grandshell-theme.el --- Grand Shell color theme for Emacs > 24

;; Copyright 2013, Steckerhalter

;; Author: steckerhalter
;; Keywords: color theme grand shell faces
;; URL: https://github.com/steckerhalter/grandshell-theme

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Dark color theme for Emacs with intensive colors. The theme
;; structure has been borrowed from color-theme-sanityinc-solarized
;; URL: https://github.com/purcell/color-theme-sanityinc-solarized by
;; Steve Purcell.

;;; Requirements:

;; Emacs 24.

;;; Code:

(deftheme grandshell "Grand Shell, a dark theme for Emacs24+")

(let ((class '((class color) (min-colors 89)))
      (background "black")
      (alt-background "#222")
      (strong "#eee")
      (bright "#eee")
      (normal "gray")
      (faint "#888")
      (dark "#888")
      (faintest "#333")
      (very-dark "#333")
      (darkest "black")
      (contrast-background "#331133")
      (red-brightest "#ffbbbb")
      (red-bright "#f25a5a")
      (red "red")
      (red-dark "#5a0000")
      (red-darkest "#1a0000")
      (pink-brightest "#ffbfd7")
      (pink-brighter "#ff8fb7")
      (pink "#ff5f87")
      (pink-darker "#aa2255")
      (orange "#efc334")
      (yellow "#f6df92")
      (yellow-darker "#a86")
      (yellow-dark "#643")
      (green-bright "#dcf692")
      (green "#acfb5a")
      (green-darker "#77bb33")
      (cyan "#5af2ee")
      (malachite "#3affa3")
      (blue "#b2baf6")
      (blue-darker "#5555dd")
      (magenta-bright "#f09fff")
      (magenta "#c350ff")
      (magenta-dark "#34004A")
      (magenta-darkest "#1B0026")
      (violet "#78537A")
      (violet-darkest "#110011")
      )

  (custom-theme-set-faces
   'grandshell

   ;; Standard font lock faces
   `(default ((,class (:foreground ,normal :background ,background))))
   `(bold ((,class (:weight bold))))
   `(bold-italic ((,class (:slant italic :weight bold))))
   `(underline ((,class (:underline t))))
   `(italic ((,class (:slant italic))))
   `(font-lock-builtin-face ((,class (:foreground ,blue))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,yellow))))
   `(font-lock-comment-face ((,class (:foreground ,orange))))
   `(font-lock-constant-face ((,class (:foreground ,green))))
   `(font-lock-doc-face ((,class (:foreground ,magenta))))
   `(font-lock-doc-string-face ((,class (:foreground ,yellow))))
   `(font-lock-function-name-face ((,class (:foreground ,magenta-bright))))
   `(font-lock-keyword-face ((,class (:foreground ,cyan))))
   `(font-lock-negation-char-face ((,class (:foreground ,green))))
   `(font-lock-preprocessor-face ((,class (:foreground ,red-bright))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,cyan))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,magenta))))
   `(font-lock-string-face ((,class (:foreground ,pink))))
   `(font-lock-type-face ((,class (:foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:foreground ,yellow))))
   `(font-lock-warning-face ((,class (:weight bold :foreground ,red))))
   `(shadow ((,class (:foreground ,normal))))
   `(success ((,class (:foreground ,green))))
   `(error ((,class (:foreground ,red))))
   `(warning ((,class (:foreground ,orange))))
   `(outline-4 ((,class (:slant normal :foreground ,faint))))

   ;; Fic-mode
   `(font-lock-fic-face ((,class (:background ,red :foreground ,red-darkest :weight bold))))

   ;; eval-sexp-fu
   `(eval-sexp-fu-flash ((,class (:background ,magenta-dark))))

   ;; nrepl-eval-sexp-fu
   `(nrepl-eval-sexp-fu-flash ((,class (:background ,magenta-dark))))

   ;; auto-complete
   `(ac-completion-face ((,class (:foreground ,bright :underline t))))
   `(ac-candidate-face ((,class (:background ,magenta-darkest :foreground ,bright))))
   `(ac-selection-face ((,class (:background ,magenta :foreground ,darkest))))
   `(ac-yasnippet-candidate-face ((,class (:background ,pink-darker :foreground ,darkest))))
   `(ac-yasnippet-selection-face ((,class (:background ,pink :foreground ,darkest))))

   ;; highlight-symbol
   `(highlight-symbol-face ((,class (:background ,very-dark))))

   ;; helm
   `(helm-M-x-key ((,class (:foreground ,pink :underline t))))
   `(helm-buffer-size ((,class (:foreground ,orange))))
   `(helm-buffer-not-saved ((,class (:foreground ,orange))))
   `(helm-buffer-saved-out ((,class (:foreground ,red :background ,background :inverse-video t))))
   `(helm-candidate-number ((,class (:background ,background :foreground ,yellow :bold t))))
   `(helm-visible-mark ((,class (:background ,faintest :foreground ,magenta :bold t))))
   `(helm-header ((,class (:inherit header-line))))
   `(helm-selection ((,class (:background ,faintest :underline t))))
   `(helm-selection-line ((,class (:background ,normal :foreground ,yellow :underline nil))))
   `(helm-separator ((,class (:foreground ,red))))
   `(helm-source-header ((,class (:background ,background, :foreground ,pink, :underline t, :weight bold))))
   `(helm-ff-directory ((t (:foreground ,magenta))))
   `(helm-ff-symlink ((t (  :foreground ,yellow))))

   ;; Flymake
   `(flymake-warnline ((,class (:underline ,orange :background ,background))))
   `(flymake-errline ((,class (:underline ,red :background ,background))))

   ;; Flycheck
   `(flycheck-error-face ((t (:foreground ,red :background ,red-darkest :weight bold))))

   ;; Clojure errors
   `(clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
   `(clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
   `(clojure-test-success-face ((,class (:background nil :foreground nil :underline ,green))))

   ;; For Brian Carper's extended clojure syntax table
   `(clojure-keyword ((,class (:foreground ,yellow))))
   `(clojure-parens ((,class (:foreground ,strong))))
   `(clojure-braces ((,class (:foreground ,green))))
   `(clojure-brackets ((,class (:foreground ,yellow))))
   `(clojure-double-quote ((,class (:foreground ,cyan :background nil))))
   `(clojure-special ((,class (:foreground ,blue))))
   `(clojure-java-call ((,class (:foreground ,magenta))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,normal))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,blue))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,normal))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red))))

   ;; MMM-mode
   `(mmm-code-submode-face ((,class (:background ,alt-background))))
   `(mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))))
   `(mmm-output-submode-face ((,class (:background ,alt-background))))

   ;; Search
   `(match ((,class (:foreground ,blue :background ,background :inverse-video t))))
   `(isearch ((,class (:foreground ,yellow :background ,background :inverse-video t))))
   `(isearch-lazy-highlight-face ((,class (:foreground ,cyan :background ,background :inverse-video t))))
   `(isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

   ;; IDO
   `(ido-subdir ((,class (:foreground ,magenta))))
   `(ido-first-match ((,class (:foreground ,yellow))))
   `(ido-only-match ((,class (:foreground ,green))))
   `(ido-indicator ((,class (:foreground ,red :background ,background))))
   `(ido-virtual ((,class (:foreground ,faintest))))

   ;; which-function
   `(which-func ((,class (:foreground ,blue :background nil :weight bold))))

   ;; Emacs interface
   `(cursor ((,class (:background ,green))))
   `(fringe ((,class (:background ,alt-background))))
   `(linum ((,class (:background ,alt-background))))
   `(border ((,class (:background ,alt-background))))
   `(border-glyph ((,class (nil))))
   `(highlight ((,class (:inverse-video nil :background ,alt-background))))
   `(gui-element ((,class (:background ,alt-background :foreground ,normal))))
   `(mode-line ((t (:foreground ,strong :background ,contrast-background))))
   `(mode-line-inactive ((t (:foreground ,yellow-dark :background ,violet-darkest :weight light :box nil :inherit (mode-line )))))
   `(mode-line-buffer-id ((t (:foreground ,yellow))))
   `(mode-line-emphasis ((,class (:foreground ,magenta))))
   `(minibuffer-prompt ((,class (:foreground ,blue))))
   `(region ((,class (:background ,magenta-dark))))
   `(secondary-selection ((,class (:background ,alt-background))))

   `(header-line ((,class (:inherit mode-line :foreground ,magenta :background nil))))
   `(trailing-whitespace ((,class (:background ,red :underline nil))))

   ;; Parenthesis matching (built-in)
   `(show-paren-match ((,class (:background nil :foreground nil :inverse-video t))))
   `(show-paren-mismatch ((,class (:background ,magenta :foreground ,background))))

   ;; Parenthesis matching (mic-paren)
   `(paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis dimming (parenface)
   `(paren-face ((,class (:foreground ,faintest :background nil))))

   `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
   `(slime-highlight-edits-face ((,class (:foreground ,strong))))
   `(slime-repl-input-face ((,class (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,magenta))))
   `(slime-repl-result-face ((,class (:foreground ,green))))
   `(slime-repl-output-face ((,class (:foreground ,blue :background ,background))))

   `(csv-separator-face ((,class (:foreground ,yellow))))

   `(diff-added ((,class (:foreground ,green))))
   `(diff-changed ((,class (:foreground ,violet))))
   `(diff-removed ((,class (:foreground ,orange))))
   `(diff-header ((,class (:foreground ,cyan :background nil))))
   `(diff-file-header ((,class (:foreground ,blue :background nil))))
   `(diff-hunk-header ((,class (:foreground ,magenta))))

   `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((,class (:foreground ,faint :background nil :inverse-video t))))
   `(ediff-odd-diff-B  ((,class (:foreground ,faint :background nil :inverse-video t))))

   `(diff-hl-change ((,class (:foreground ,blue :background ,blue-darker))))
   `(diff-hl-delete ((,class (:foreground ,pink :background ,pink-darker))))
   `(diff-hl-insert ((,class (:foreground ,green :background ,green-darker))))

   `(eldoc-highlight-function-argument ((,class (:foreground ,green :weight bold))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,normal))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

   ;; dired+
   `(diredp-compressed-file-suffix ((,class (:foreground ,orange))))
   `(diredp-date-time ((,class (:foreground ,yellow))))
   `(diredp-deletion ((,class (:foreground ,red-bright :weight bold :slant italic))))
   `(diredp-deletion-file-name ((,class (:foreground ,red-bright :underline t))))
   `(diredp-dir-heading ((,class (:foreground ,pink :underline t :weight bold))))
   `(diredp-dir-priv ((,class (:foreground ,magenta :background nil))))
   `(diredp-exec-priv ((,class (:foreground ,green-bright :background nil))))
   `(diredp-executable-tag ((,class (:foreground ,green-bright :background nil))))
   `(diredp-file-name ((,class (:foreground ,normal))))
   `(diredp-file-suffix ((,class (:foreground ,cyan))))
   `(diredp-flag-mark ((,class (:foreground ,red-bright :weight bold))))
   `(diredp-flag-mark-line ((,class (:inherit highlight))))
   `(diredp-ignored-file-name ((,class (:foreground ,dark))))
   `(diredp-link-priv ((,class (:background nil :foreground ,pink))))
   `(diredp-mode-line-flagged ((,class (:foreground ,orange))))
   `(diredp-mode-line-marked ((,class (:foreground ,magenta-bright))))
   `(diredp-no-priv ((,class (:foreground ,dark :background nil))))
   `(diredp-number ((,class (:foreground ,orange))))
   `(diredp-other-priv ((,class (:background nil :foreground ,orange))))
   `(diredp-rare-priv ((,class (:foreground ,red :background nil))))
   `(diredp-read-priv ((,class (:foreground ,blue :background nil))))
   `(diredp-symlink ((,class (:foreground ,pink))))
   `(diredp-write-priv ((,class (:foreground ,magenta-bright :background nil))))

   ;; Magit (a patch is pending in magit to make these standard upstream)
   `(magit-branch ((,class (:foreground ,green))))
   `(magit-header ((,class (:inherit nil :weight bold))))
   `(magit-item-highlight ((,class (:inherit highlight :background nil))))
   `(magit-log-graph ((,class (:foreground ,faintest))))
   `(magit-log-sha1 ((,class (:foreground ,yellow))))
   `(magit-log-head-label-bisect-bad ((,class (:foreground ,red))))
   `(magit-log-head-label-bisect-good ((,class (:foreground ,green))))
   `(magit-log-head-label-default ((,class (:foreground ,yellow :box nil :weight bold))))
   `(magit-log-head-label-local ((,class (:foreground ,magenta :box nil :weight bold))))
   `(magit-log-head-label-remote ((,class (:foreground ,violet :box nil :weight bold))))
   `(magit-log-head-label-tags ((,class (:foreground ,cyan :box nil :weight bold))))
   `(magit-section-title ((,class (:foreground ,blue :weight bold))))

   ;; git-gutter
   `(git-gutter:modified ((,class (:foreground ,violet :weight bold))))
   `(git-gutter:added ((,class (:foreground ,green :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,red :weight bold))))
   `(git-gutter:unchanged ((,class (:background ,yellow))))

   ;; git-gutter-fringe
   `(git-gutter-fr:modified ((,class (:foreground ,violet :weight bold))))
   `(git-gutter-fr:added ((,class (:foreground ,green :weight bold))))
   `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))

   `(link ((,class (:foreground nil :underline t))))
   `(widget-button ((,class (:underline t))))
   `(widget-field ((,class (:background ,alt-background :box (:line-width 1 :color ,normal)))))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
   `(compilation-column-number ((,class (:foreground ,yellow))))
   `(compilation-line-number ((,class (:foreground ,yellow))))
   `(compilation-message-face ((,class (:foreground ,blue))))
   `(compilation-mode-line-exit ((,class (:foreground ,green))))
   `(compilation-mode-line-fail ((,class (:foreground ,red))))
   `(compilation-mode-line-run ((,class (:foreground ,blue))))
   `(compilation-info ((,class (:foreground ,malachite))))

   ;; Grep
   `(grep-context-face ((,class (:foreground ,faint))))
   `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,blue))))
   `(grep-match-face ((,class (:foreground nil :background nil :inherit match))))

   `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

   ;; mark-multiple
   `(mm/master-face ((,class (:inherit region :foreground nil :background nil))))
   `(mm/mirror-face ((,class (:inherit region :foreground nil :background nil))))

   ;; Term
   `(term-color-black ((,class (:background ,alt-background :foreground ,alt-background))))
   `(term-color-blue ((,class (:background ,blue :foreground ,blue))))
   `(term-color-cyan ((,class (:background ,cyan :foreground ,cyan))))
   `(term-color-green ((,class (:background ,green :foreground ,green))))
   `(term-color-magenta ((,class (:background ,magenta :foreground ,magenta))))
   `(term-color-red ((,class (:background ,red :foreground ,red))))
   `(term-color-white ((,class (:background ,contrast-background :foreground ,contrast-background))))
   `(term-color-yellow ((,class (:background ,yellow :foreground ,yellow))))

   ;; Eshell
   `(eshell-ls-archive ((,class (:foreground ,cyan :weight normal))))
   `(eshell-ls-backup ((,class (:foreground ,yellow))))
   `(eshell-ls-clutter ((,class (:foreground ,orange :weight normal))))
   `(eshell-ls-directory ((,class (:foreground ,blue :weight normal))))
   `(eshell-ls-executable ((,class (:foreground ,red :weight normal))))
   `(eshell-ls-missing ((,class (:foreground ,violet :weight normal))))
   `(eshell-ls-product ((,class (:foreground ,yellow))))
   `(eshell-ls-readonly ((,class (:foreground ,faintest))))
   `(eshell-ls-special ((,class (:foreground ,green :weight normal))))
   `(eshell-ls-symlink ((,class (:foreground ,magenta :weight normal))))
   `(eshell-ls-unreadable ((,class (:foreground ,normal))))
   `(eshell-prompt ((,class (:foreground ,green :weight normal))))

   `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

   ;; Stop outline-3 from inheriting font-lock-keyword-face, which we've made bold
   `(outline-3 ((,class (:inherit nil :foreground ,green))))

   `(org-agenda-structure ((,class (:foreground ,violet))))
   `(org-agenda-date ((,class (:foreground ,blue :underline nil))))
   `(org-agenda-done ((,class (:foreground ,green))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,faint))))
   `(org-block ((,class (:foreground ,orange))))
   `(org-code ((,class (:foreground ,yellow))))
   `(org-column ((,class (:background ,magenta))))
   `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
   `(org-date ((,class (:foreground ,blue :underline t))))
   `(org-document-info ((,class (:foreground ,pink))))
   `(org-document-info-keyword ((,class (:foreground ,pink-darker))))
   `(org-document-title ((,class (:weight bold :foreground ,yellow :height 1.44))))
   `(org-done ((,class (:foreground ,green))))
   `(org-ellipsis ((,class (:foreground ,faint))))
   `(org-footnote ((,class (:foreground ,cyan))))
   `(org-formula ((,class (:foreground ,orange))))
   `(org-hide ((,class (:foreground ,background :background ,background))))
   `(org-level-1 ((,class (:foreground ,yellow))))
   `(org-level-2 ((,class (:foreground ,blue))))
   `(org-level-3 ((,class (:foreground ,pink))))
   `(org-level-4 ((,class (:foreground ,cyan))))
   `(org-link ((,class (:foreground ,malachite :underline t))))
   `(org-scheduled ((,class (:foreground ,green))))
   `(org-scheduled-previously ((,class (:foreground ,yellow))))
   `(org-scheduled-today ((,class (:foreground ,green))))
   `(org-special-keyword ((,class (:foreground ,yellow-darker))))
   `(org-table ((,class (:foreground ,magenta))))
   `(org-tag ((,class (:foreground ,violet))))
   `(org-target ((,class (:foreground ,green))))
   `(org-todo ((,class (:foreground ,red-bright))))
   `(org-upcoming-deadline ((,class (:foreground ,yellow))))
   `(org-warning ((,class (:weight bold :foreground ,red))))

   `(markdown-url-face ((,class (:inherit link))))
   `(markdown-link-face ((,class (:foreground ,blue :underline t))))

   `(hl-sexp-face ((,class (:background ,alt-background))))
   `(highlight-80+ ((,class (:background ,alt-background))))

   ;; Python-specific overrides
   `(py-builtins-face ((,class (:foreground ,orange :weight normal))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,yellow))))
   `(js2-error-face ((,class (:foreground nil :underline ,red))))
   `(js2-external-variable-face ((,class (:foreground ,magenta))))
   `(js2-function-param-face ((,class (:foreground ,blue))))
   `(js2-instance-member-face ((,class (:foreground ,blue))))
   `(js2-private-function-call-face ((,class (:foreground ,red))))

   ;; js3-mode
   `(js3-warning-face ((,class (:underline ,yellow))))
   `(js3-error-face ((,class (:foreground nil :underline ,red))))
   `(js3-external-variable-face ((,class (:foreground ,magenta))))
   `(js3-function-param-face ((,class (:foreground ,blue))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,magenta))))
   `(js3-jsdoc-type-face ((,class (:foreground ,cyan))))
   `(js3-jsdoc-value-face ((,class (:foreground ,violet))))
   `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))))
   `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))))
   `(js3-instance-member-face ((,class (:foreground ,blue))))
   `(js3-private-function-call-face ((,class (:foreground ,red))))

   ;; nxml
   `(nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((,class (:underline ,red))))

   ;; RHTML
   `(erb-delim-face ((,class (:background ,alt-background))))
   `(erb-exec-face ((,class (:background ,alt-background :weight bold))))
   `(erb-exec-delim-face ((,class (:background ,alt-background))))
   `(erb-out-face ((,class (:background ,alt-background :weight bold))))
   `(erb-out-delim-face ((,class (:background ,alt-background))))
   `(erb-comment-face ((,class (:background ,alt-background :weight bold :slant italic))))
   `(erb-comment-delim-face ((,class (:background ,alt-background))))

   ;; Message-mode
   `(message-header-other ((,class (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,yellow))))
   `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))))
   `(message-header-cc ((,class (:inherit message-header-to :foreground nil))))
   `(message-header-name ((,class (:foreground ,green :background nil))))
   `(message-header-newsgroups ((,class (:foreground ,cyan :background nil :slant normal))))
   `(message-separator ((,class (:foreground ,magenta))))

   ;; Jabber
   `(jabber-chat-prompt-local ((,class (:foreground ,yellow))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,orange))))
   `(jabber-chat-prompt-system ((,class (:foreground ,yellow :weight bold))))
   `(jabber-chat-text-local ((,class (:foreground ,yellow))))
   `(jabber-chat-text-foreign ((,class (:foreground ,orange))))
   `(jabber-chat-text-error ((,class (:foreground ,red))))

   `(jabber-roster-user-online ((,class (:foreground ,green))))
   `(jabber-roster-user-xa ((,class :foreground ,faint)))
   `(jabber-roster-user-dnd ((,class :foreground ,yellow)))
   `(jabber-roster-user-away ((,class (:foreground ,orange))))
   `(jabber-roster-user-chatty ((,class (:foreground ,violet))))
   `(jabber-roster-user-error ((,class (:foreground ,red))))
   `(jabber-roster-user-offline ((,class (:foreground ,faint))))

   `(jabber-rare-time-face ((,class (:foreground ,faint))))
   `(jabber-activity-face ((,class (:foreground ,violet))))
   `(jabber-activity-personal-face ((,class (:foreground ,cyan))))

   ;; Powerline
   `(powerline-active1 ((t (:foreground ,normal :background ,contrast-background))))
   `(powerline-active2 ((t (:foreground ,normal :background ,alt-background))))

   ;; Gnus
   `(gnus-cite-1 ((,class (:inherit outline-1 :foreground nil))))
   `(gnus-cite-2 ((,class (:inherit outline-2 :foreground nil))))
   `(gnus-cite-3 ((,class (:inherit outline-3 :foreground nil))))
   `(gnus-cite-4 ((,class (:inherit outline-4 :foreground nil))))
   `(gnus-cite-5 ((,class (:inherit outline-5 :foreground nil))))
   `(gnus-cite-6 ((,class (:inherit outline-6 :foreground nil))))
   `(gnus-cite-7 ((,class (:inherit outline-7 :foreground nil))))
   `(gnus-cite-8 ((,class (:inherit outline-8 :foreground nil))))
   ;; there are several more -cite- faces...
   `(gnus-header-content ((,class (:inherit message-header-other))))
   `(gnus-header-subject ((,class (:inherit message-header-subject))))
   `(gnus-header-from ((,class (:inherit message-header-other-face :weight bold :foreground ,orange))))
   `(gnus-header-name ((,class (:inherit message-header-name))))
   `(gnus-button ((,class (:inherit link :foreground nil))))
   `(gnus-signature ((,class (:inherit font-lock-comment-face))))

   `(gnus-summary-normal-unread ((,class (:foreground ,strong :weight normal))))
   `(gnus-summary-normal-read ((,class (:foreground ,normal :weight normal))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,cyan :weight normal))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,orange :weight normal))))
   `(gnus-summary-low-unread ((,class (:foreground ,faint :weight normal))))
   `(gnus-summary-low-read ((,class (:foreground ,faintest :weight normal))))
   `(gnus-summary-low-ancient ((,class (:foreground ,faintest :weight normal))))
   `(gnus-summary-high-unread ((,class (:foreground ,yellow :weight normal))))
   `(gnus-summary-high-read ((,class (:foreground ,green :weight normal))))
   `(gnus-summary-high-ancient ((,class (:foreground ,green :weight normal))))
   `(gnus-summary-high-ticked ((,class (:foreground ,orange :weight normal))))
   `(gnus-summary-cancelled ((,class (:foreground ,red :background nil :weight normal))))

   `(gnus-group-mail-low ((,class (:foreground ,faintest))))
   `(gnus-group-mail-low-empty ((,class (:foreground ,faintest))))
   `(gnus-group-mail-1 ((,class (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-mail-2 ((,class (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-mail-3 ((,class (:foreground nil :weight normal :inherit outline-3))))
   `(gnus-group-mail-4 ((,class (:foreground nil :weight normal :inherit outline-4))))
   `(gnus-group-mail-5 ((,class (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-mail-6 ((,class (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :foreground ,faint))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :foreground ,faint))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :foreground ,faint))))
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-mail-4 :foreground ,faint))))
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-mail-5 :foreground ,faint))))
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-mail-6 :foreground ,faint))))
   `(gnus-group-news-1 ((,class (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-news-2 ((,class (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-news-3 ((,class (:foreground nil :weight normal :inherit outline-7))))
   `(gnus-group-news-4 ((,class (:foreground nil :weight normal :inherit outline-8))))
   `(gnus-group-news-5 ((,class (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-news-6 ((,class (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-news-1-empty ((,class (:inherit gnus-group-news-1 :foreground ,faint))))
   `(gnus-group-news-2-empty ((,class (:inherit gnus-group-news-2 :foreground ,faint))))
   `(gnus-group-news-3-empty ((,class (:inherit gnus-group-news-3 :foreground ,faint))))
   `(gnus-group-news-4-empty ((,class (:inherit gnus-group-news-4 :foreground ,faint))))
   `(gnus-group-news-5-empty ((,class (:inherit gnus-group-news-5 :foreground ,faint))))
   `(gnus-group-news-6-empty ((,class (:inherit gnus-group-news-6 :foreground ,faint))))

   `(erc-direct-msg-face ((,class (:foreground ,yellow))))
   `(erc-error-face ((,class (:foreground ,red))))
   `(erc-header-face ((,class (:foreground ,strong :background ,alt-background))))
   `(erc-input-face ((,class (:foreground ,green))))
   `(erc-current-nick-face ((,class (:foreground ,green))))
   `(erc-my-nick-face ((,class (:foreground ,green))))
   `(erc-nick-default-face ((,class (:weight normal :foreground ,violet))))
   `(erc-nick-msg-face ((,class (:weight normal :foreground ,yellow))))
   `(erc-notice-face ((,class (:foreground ,faintest))))
   `(erc-pal-face ((,class (:foreground ,orange))))
   `(erc-prompt-face ((,class (:foreground ,blue))))
   `(erc-timestamp-face ((,class (:foreground ,cyan))))
   `(erc-keyword-face ((,class (:foreground ,green))))

   `(custom-variable-tag ((,class (:foreground ,blue))))
   `(custom-group-tag ((,class (:foreground ,blue))))
   `(custom-state-tag ((,class (:foreground ,green))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'grandshell)

;;; grandshell-theme.el ends here


