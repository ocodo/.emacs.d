;;; hipster-theme.el --- A low contrast color theme for Emacs.

;; Author: Luis Angel <redshacker11@gmail.com>
;; URL: http://github.com/xzerocode/hispter-theme
;; Version: 20141205.2005
;; X-Original-Version: 0.1

;;; Commentary:

;; A port of the popular Vim theme Hipster for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Bozhidar Batsov adapt the vim theme on which i modified colors. Thanks!

;;; Code:

(deftheme hipster "The Hipster color theme")

;;; Color Palette

(defvar hipster-colors-alist
  '(("hipster-fg+1"     . "#FFFFEF")
    ("hipster-fg"       . "#DCDCCC")
    ("hipster-fg-1"     . "#656555")
    ("hipster-bg-2"     . "#000000")
    ("hipster-bg-1"     . "#2B2B2B")
    ("hipster-bg-05"    . "#383838")
    ("hipster-bg"       . "#000000")
    ("hipster-bg+05"    . "#494949")
    ("hipster-bg+1"     . "#4F4F4F")
    ("hipster-bg+2"     . "#5F5F5F")
    ("hipster-bg+3"     . "#6F6F6F")
    ("hipster-red+1"    . "#dc322f")
    ("hipster-red"      . "#d01A4E")
    ("hipster-red-1"    . "#dc322f")
    ("hipster-red-2"    . "#dc322f")
    ("hipster-red-3"    . "#dc322f")
    ("hipster-red-4"    . "#dc322f")
    ("hipster-orange"   . "#cb4b16")
    ("hipster-yellow"   . "#b58900")
    ("hipster-yellow-1" . "#b58900")
    ("hipster-yellow-2" . "#b58900")
    ("hipster-green-1"  . "#7E7D7E")
    ("hipster-green"    . "#7E7D7E")
    ("hipster-green+1"  . "#9FAA9B")
    ("hipster-green+2"  . "#9FC59F")
    ("hipster-green+3"  . "#859900")
    ("hipster-green+4"  . "#31be67")
    ("hipster-cyan"     . "#2aa198")
    ("hipster-blue+1"   . "#00a74e")
    ("hipster-blue"     . "#268bd2")
    ("hipster-blue-1"   . "#268bd2")
    ("hipster-blue-2"   . "#268bd2")
    ("hipster-blue-3"   . "#268bd2")
    ("hipster-blue-4"   . "#268bd2")
    ("hipster-blue-5"   . "#268bd2")
    ("hipster-magenta"  . "#d33682"))
  "List of Hipster colors.
Each element has the form (NAME . HEX).
`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro hipster-with-color-variables (&rest body)
  "`let' bind all colors defined in `hipster-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   hipster-colors-alist))
     ,@body))

;;; Theme Faces
(hipster-with-color-variables
  (custom-theme-set-faces
   'hipster
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,hipster-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,hipster-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,hipster-fg :background ,hipster-bg))))
   `(cursor ((t (:foreground ,hipster-fg :background ,hipster-fg+1))))
   `(escape-glyph ((t (:foreground ,hipster-yellow :bold t))))
   `(fringe ((t (:foreground ,hipster-fg :background ,hipster-bg+1))))
   `(header-line ((t (:foreground ,hipster-yellow
                                  :background ,hipster-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,hipster-bg-05))))
   `(success ((t (:foreground ,hipster-green :weight bold))))
   `(warning ((t (:foreground ,hipster-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,hipster-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,hipster-green))))
   `(compilation-error-face ((t (:foreground ,hipster-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,hipster-fg))))
   `(compilation-info-face ((t (:foreground ,hipster-blue))))
   `(compilation-info ((t (:foreground ,hipster-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,hipster-green))))
   `(compilation-line-face ((t (:foreground ,hipster-yellow))))
   `(compilation-line-number ((t (:foreground ,hipster-yellow))))
   `(compilation-message-face ((t (:foreground ,hipster-blue))))
   `(compilation-warning-face ((t (:foreground ,hipster-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,hipster-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,hipster-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,hipster-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,hipster-fg))))
   `(grep-error-face ((t (:foreground ,hipster-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,hipster-blue))))
   `(grep-match-face ((t (:foreground ,hipster-orange :weight bold))))
   `(match ((t (:background ,hipster-bg-1 :foreground ,hipster-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,hipster-yellow-2 :weight bold :background ,hipster-bg+2))))
   `(isearch-fail ((t (:foreground ,hipster-fg :background ,hipster-red-4))))
   `(lazy-highlight ((t (:foreground ,hipster-yellow-2 :weight bold :background ,hipster-bg-05))))

   `(menu ((t (:foreground ,hipster-fg :background ,hipster-bg))))
   `(minibuffer-prompt ((t (:foreground ,hipster-yellow))))
   `(mode-line
     ((,class (:foreground ,hipster-green+1
                           :background ,hipster-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,hipster-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,hipster-green-1
                      :background ,hipster-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,hipster-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,hipster-bg+2))))
   `(trailing-whitespace ((t (:background ,hipster-red))))
   `(vertical-border ((t (:foreground ,hipster-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,hipster-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,hipster-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,hipster-green-1))))
   `(font-lock-constant-face ((t (:foreground ,hipster-green+4))))
   `(font-lock-doc-face ((t (:foreground ,hipster-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,hipster-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,hipster-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,hipster-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,hipster-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,hipster-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,hipster-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,hipster-red))))
   `(font-lock-type-face ((t (:foreground ,hipster-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,hipster-orange))))
   `(font-lock-warning-face ((t (:foreground ,hipster-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,hipster-fg))))
   `(newsticker-default-face ((t (:foreground ,hipster-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,hipster-green+3))))
   `(newsticker-extra-face ((t (:foreground ,hipster-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,hipster-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,hipster-green))))
   `(newsticker-new-item-face ((t (:foreground ,hipster-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,hipster-red))))
   `(newsticker-old-item-face ((t (:foreground ,hipster-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,hipster-fg))))
   `(newsticker-treeview-face ((t (:foreground ,hipster-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,hipster-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,hipster-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,hipster-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,hipster-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,hipster-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,hipster-bg-1 :foreground ,hipster-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,hipster-fg-1 :background ,hipster-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,hipster-green+2 :background ,hipster-bg :inverse-video nil))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,hipster-green+1))))
   `(android-mode-error-face ((t (:foreground ,hipster-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,hipster-fg))))
   `(android-mode-verbose-face ((t (:foreground ,hipster-green))))
   `(android-mode-warning-face ((t (:foreground ,hipster-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,hipster-cyan :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,hipster-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,hipster-yellow))))
   `(font-latex-italic-face ((t (:foreground ,hipster-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,hipster-orange))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,hipster-bg+3 :foreground ,hipster-bg-2))))
   `(ac-selection-face ((t (:background ,hipster-blue-4 :foreground ,hipster-fg))))
   `(popup-tip-face ((t (:background ,hipster-yellow-2 :foreground ,hipster-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,hipster-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,hipster-bg-1))))
   `(popup-isearch-match ((t (:background ,hipster-bg :foreground ,hipster-fg))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,hipster-yellow :background ,hipster-bg+1))))
   `(company-tooltip-selection ((t (:foreground ,hipster-fg :background ,hipster-bg-1))))
   `(company-tooltip-mouse ((t (:background ,hipster-bg-1))))
   `(company-tooltip-common ((t (:foreground ,hipster-yellow :background ,hipster-bg-1))))
   `(company-tooltip-common-selection ((t (:background ,hipster-bg-1))))
   `(company-scrollbar-fg ((t (:background ,hipster-green+1))))
   `(company-scrollbar-bg ((t (:background ,hipster-bg-1))))
   `(company-preview ((t (:background ,hipster-green+1))))
   `(company-preview-common ((t (:background ,hipster-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,hipster-yellow-1 :foreground ,hipster-bg))))
   `(bm-fringe-face ((t (:background ,hipster-yellow-1 :foreground ,hipster-bg))))
   `(bm-fringe-persistent-face ((t (:background ,hipster-green-1 :foreground ,hipster-bg))))
   `(bm-persistent-face ((t (:background ,hipster-green-1 :foreground ,hipster-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,hipster-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,hipster-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,hipster-green+1 :weight bold :underline t))))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,hipster-blue :foreground ,hipster-bg))))
   `(ctbl:face-continue-bar ((t (:background ,hipster-bg-05 :foreground ,hipster-bg))))
   `(ctbl:face-row-select ((t (:background ,hipster-cyan :foreground ,hipster-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,hipster-green+4 :background nil))
                 (t (:foreground ,hipster-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,hipster-yellow))))
   `(diff-removed ((,class (:foreground ,hipster-red :background nil))
                   (t (:foreground ,hipster-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,hipster-bg+2))
                  (t (:background ,hipster-fg :foreground ,hipster-bg))))
   `(diff-file-header
     ((,class (:background ,hipster-bg+2 :foreground ,hipster-fg :bold t))
      (t (:background ,hipster-fg :foreground ,hipster-bg :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,hipster-blue-2 :background ,hipster-bg-05))))
   `(diff-hl-delete ((,class (:foreground ,hipster-red+1 :background ,hipster-bg-05))))
   `(diff-hl-insert ((,class (:foreground ,hipster-green+1 :background ,hipster-bg-05))))
   `(diff-hl-unknown ((,class (:foreground ,hipster-yellow :background ,hipster-bg-05))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,hipster-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,hipster-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,hipster-orange))))
   `(diredp-date-time ((t (:foreground ,hipster-magenta))))
   `(diredp-deletion ((t (:foreground ,hipster-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,hipster-red))))
   `(diredp-dir-heading ((t (:foreground ,hipster-blue :background ,hipster-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,hipster-cyan))))
   `(diredp-exec-priv ((t (:foreground ,hipster-red))))
   `(diredp-executable-tag ((t (:foreground ,hipster-green+1))))
   `(diredp-file-name ((t (:foreground ,hipster-blue))))
   `(diredp-file-suffix ((t (:foreground ,hipster-green))))
   `(diredp-flag-mark ((t (:foreground ,hipster-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,hipster-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,hipster-red))))
   `(diredp-link-priv ((t (:foreground ,hipster-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,hipster-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,hipster-orange))))
   `(diredp-no-priv ((t (:foreground ,hipster-fg))))
   `(diredp-number ((t (:foreground ,hipster-green+1))))
   `(diredp-other-priv ((t (:foreground ,hipster-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,hipster-red-1))))
   `(diredp-read-priv ((t (:foreground ,hipster-green-1))))
   `(diredp-symlink ((t (:foreground ,hipster-yellow))))
   `(diredp-write-priv ((t (:foreground ,hipster-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,hipster-fg :background ,hipster-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,hipster-fg :background ,hipster-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,hipster-fg :background ,hipster-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,hipster-fg :background ,hipster-blue-5))))
   `(ediff-even-diff-A ((t (:background ,hipster-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,hipster-bg+1))))
   `(ediff-even-diff-B ((t (:background ,hipster-bg+1))))
   `(ediff-even-diff-C ((t (:background ,hipster-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,hipster-fg :background ,hipster-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,hipster-fg :background ,hipster-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,hipster-fg :background ,hipster-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,hipster-fg :background ,hipster-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,hipster-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,hipster-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,hipster-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,hipster-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,hipster-fg))))
   `(egg-help-header-1 ((t (:foreground ,hipster-yellow))))
   `(egg-help-header-2 ((t (:foreground ,hipster-green+3))))
   `(egg-branch ((t (:foreground ,hipster-yellow))))
   `(egg-branch-mono ((t (:foreground ,hipster-yellow))))
   `(egg-term ((t (:foreground ,hipster-yellow))))
   `(egg-diff-add ((t (:foreground ,hipster-green+4))))
   `(egg-diff-del ((t (:foreground ,hipster-red+1))))
   `(egg-diff-file-header ((t (:foreground ,hipster-yellow-2))))
   `(egg-section-title ((t (:foreground ,hipster-yellow))))
   `(egg-stash-mono ((t (:foreground ,hipster-green+4))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,hipster-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,hipster-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,hipster-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,hipster-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,hipster-green+2 :background ,hipster-bg))))
   `(w3m-lnum-match ((t (:background ,hipster-bg-1
                                     :foreground ,hipster-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,hipster-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,hipster-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,hipster-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,hipster-yellow))))
   `(erc-keyword-face ((t (:foreground ,hipster-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,hipster-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,hipster-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,hipster-green))))
   `(erc-pal-face ((t (:foreground ,hipster-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,hipster-orange :background ,hipster-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,hipster-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,hipster-green+4 :background ,hipster-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,hipster-red :background ,hipster-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,hipster-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,hipster-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,hipster-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,hipster-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,hipster-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,hipster-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,hipster-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,hipster-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-red-1) :inherit unspecified))
      (t (:foreground ,hipster-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-yellow) :inherit unspecified))
      (t (:foreground ,hipster-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-cyan) :inherit unspecified))
      (t (:foreground ,hipster-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,hipster-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,hipster-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,hipster-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hipster-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hipster-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hipster-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-orange) :inherit unspecified))
      (t (:foreground ,hipster-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-red) :inherit unspecified))
      (t (:foreground ,hipster-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,hipster-fg))))
   `(ack-file ((t (:foreground ,hipster-blue))))
   `(ack-line ((t (:foreground ,hipster-yellow))))
   `(ack-match ((t (:foreground ,hipster-orange :background ,hipster-bg-1 :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,hipster-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,hipster-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,hipster-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,hipster-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,hipster-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,hipster-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,hipster-magenta :weight bold))))
;;;;; git-rebase-mode
   `(git-rebase-hash ((t (:foreground, hipster-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,hipster-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,hipster-blue))))
   `(gnus-summary-high-read ((t (:foreground ,hipster-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,hipster-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,hipster-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,hipster-blue))))
   `(gnus-summary-low-read ((t (:foreground ,hipster-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,hipster-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,hipster-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,hipster-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,hipster-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,hipster-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,hipster-fg))))
   `(gnus-summary-selected ((t (:foreground ,hipster-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,hipster-blue))))
   `(gnus-cite-10 ((t (:foreground ,hipster-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,hipster-yellow))))
   `(gnus-cite-2 ((t (:foreground ,hipster-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,hipster-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,hipster-green+2))))
   `(gnus-cite-5 ((t (:foreground ,hipster-green+1))))
   `(gnus-cite-6 ((t (:foreground ,hipster-green))))
   `(gnus-cite-7 ((t (:foreground ,hipster-red))))
   `(gnus-cite-8 ((t (:foreground ,hipster-red-1))))
   `(gnus-cite-9 ((t (:foreground ,hipster-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,hipster-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,hipster-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,hipster-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,hipster-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,hipster-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,hipster-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,hipster-bg+2))))
   `(gnus-signature ((t (:foreground ,hipster-yellow))))
   `(gnus-x ((t (:background ,hipster-fg :foreground ,hipster-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,hipster-blue))))
   `(guide-key/key-face ((t (:foreground ,hipster-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,hipster-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,hipster-green
                      :background ,hipster-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,hipster-yellow
                      :background ,hipster-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,hipster-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,hipster-bg+1))))
   `(helm-visible-mark ((t (:foreground ,hipster-bg :background ,hipster-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,hipster-green+4 :background ,hipster-bg-1))))
   `(helm-separator ((t (:foreground ,hipster-red :background ,hipster-bg))))
   `(helm-time-zone-current ((t (:foreground ,hipster-green+2 :background ,hipster-bg))))
   `(helm-time-zone-home ((t (:foreground ,hipster-red :background ,hipster-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,hipster-orange :background ,hipster-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,hipster-magenta :background ,hipster-bg))))
   `(helm-bookmark-info ((t (:foreground ,hipster-green+2 :background ,hipster-bg))))
   `(helm-bookmark-man ((t (:foreground ,hipster-yellow :background ,hipster-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,hipster-magenta :background ,hipster-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,hipster-red :background ,hipster-bg))))
   `(helm-buffer-process ((t (:foreground ,hipster-cyan :background ,hipster-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,hipster-fg :background ,hipster-bg))))
   `(helm-buffer-size ((t (:foreground ,hipster-fg-1 :background ,hipster-bg))))
   `(helm-ff-directory ((t (:foreground ,hipster-cyan :background ,hipster-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,hipster-fg :background ,hipster-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,hipster-green+2 :background ,hipster-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,hipster-red :background ,hipster-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,hipster-yellow :background ,hipster-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,hipster-bg :background ,hipster-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,hipster-cyan :background ,hipster-bg))))
   `(helm-grep-file ((t (:foreground ,hipster-fg :background ,hipster-bg))))
   `(helm-grep-finish ((t (:foreground ,hipster-green+2 :background ,hipster-bg))))
   `(helm-grep-lineno ((t (:foreground ,hipster-fg-1 :background ,hipster-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,hipster-red :background ,hipster-bg))))
   `(helm-moccur-buffer ((t (:foreground ,hipster-cyan :background ,hipster-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,hipster-fg-1 :background ,hipster-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,hipster-fg :background ,hipster-bg))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,hipster-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,hipster-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,hipster-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,hipster-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,hipster-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,hipster-yellow))))
   `(ido-indicator ((t (:foreground ,hipster-yellow :background ,hipster-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,hipster-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,hipster-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,hipster-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,hipster-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,hipster-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,hipster-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,hipster-red+1))))
   `(jabber-activity-face((t (:foreground ,hipster-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,hipster-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,hipster-orange))))
   `(js2-error ((t (:foreground ,hipster-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,hipster-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,hipster-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,hipster-green+3))))
   `(js2-function-param ((t (:foreground, hipster-green+3))))
   `(js2-external-variable ((t (:foreground ,hipster-orange))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,hipster-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,hipster-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,hipster-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,hipster-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,hipster-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,hipster-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,hipster-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,hipster-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,hipster-orange))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,hipster-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,hipster-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,hipster-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,hipster-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,hipster-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,hipster-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,hipster-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,hipster-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,hipster-green+2 :background ,hipster-bg))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,hipster-green+2 :background ,hipster-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,hipster-red+1 :background ,hipster-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,hipster-blue+1 :background ,hipster-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,hipster-magenta :background ,hipster-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,hipster-yellow :background ,hipster-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-item-highlight ((t (:background ,hipster-bg+1))))
   `(magit-section-title ((t (:foreground ,hipster-yellow :weight bold))))
   `(magit-process-ok ((t (:foreground ,hipster-green :weight bold))))
   `(magit-process-ng ((t (:foreground ,hipster-red :weight bold))))
   `(magit-branch ((t (:foreground ,hipster-blue :weight bold))))
   `(magit-log-author ((t (:foreground ,hipster-orange))))
   `(magit-log-sha1 ((t (:foreground, hipster-orange))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,hipster-green+1))))
   `(message-header-other ((t (:foreground ,hipster-green))))
   `(message-header-to ((t (:foreground ,hipster-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,hipster-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,hipster-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,hipster-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,hipster-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,hipster-green))))
   `(message-mml ((t (:foreground ,hipster-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,hipster-orange))))
   `(mew-face-header-from ((t (:foreground ,hipster-yellow))))
   `(mew-face-header-date ((t (:foreground ,hipster-green))))
   `(mew-face-header-to ((t (:foreground ,hipster-red))))
   `(mew-face-header-key ((t (:foreground ,hipster-green))))
   `(mew-face-header-private ((t (:foreground ,hipster-green))))
   `(mew-face-header-important ((t (:foreground ,hipster-blue))))
   `(mew-face-header-marginal ((t (:foreground ,hipster-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,hipster-red))))
   `(mew-face-header-xmew ((t (:foreground ,hipster-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,hipster-red))))
   `(mew-face-body-url ((t (:foreground ,hipster-orange))))
   `(mew-face-body-comment ((t (:foreground ,hipster-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,hipster-green))))
   `(mew-face-body-cite2 ((t (:foreground ,hipster-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,hipster-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,hipster-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,hipster-red))))
   `(mew-face-mark-review ((t (:foreground ,hipster-blue))))
   `(mew-face-mark-escape ((t (:foreground ,hipster-green))))
   `(mew-face-mark-delete ((t (:foreground ,hipster-red))))
   `(mew-face-mark-unlink ((t (:foreground ,hipster-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,hipster-green))))
   `(mew-face-mark-unread ((t (:foreground ,hipster-red-2))))
   `(mew-face-eof-message ((t (:foreground ,hipster-green))))
   `(mew-face-eof-part ((t (:foreground ,hipster-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,hipster-cyan :background ,hipster-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,hipster-bg :background ,hipster-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,hipster-bg :background ,hipster-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,hipster-blue))))
   `(mingus-pausing-face ((t (:foreground ,hipster-magenta))))
   `(mingus-playing-face ((t (:foreground ,hipster-cyan))))
   `(mingus-playlist-face ((t (:foreground ,hipster-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,hipster-yellow))))
   `(mingus-stopped-face ((t (:foreground ,hipster-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,hipster-yellow))))
   `(nav-face-button-num ((t (:foreground ,hipster-cyan))))
   `(nav-face-dir ((t (:foreground ,hipster-green))))
   `(nav-face-hdir ((t (:foreground ,hipster-red))))
   `(nav-face-file ((t (:foreground ,hipster-fg))))
   `(nav-face-hfile ((t (:foreground ,hipster-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,hipster-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,hipster-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,hipster-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,hipster-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,hipster-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,hipster-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,hipster-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,hipster-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,hipster-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,hipster-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,hipster-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,hipster-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,hipster-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,hipster-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,hipster-fg :weight bold))))
   `(org-checkbox ((t (:background ,hipster-bg+2 :foreground ,hipster-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,hipster-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,hipster-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,hipster-green+3))))
   `(org-formula ((t (:foreground ,hipster-yellow-2))))
   `(org-headline-done ((t (:foreground ,hipster-green+3))))
   `(org-hide ((t (:foreground ,hipster-bg-1))))
   `(org-level-1 ((t (:foreground ,hipster-orange))))
   `(org-level-2 ((t (:foreground ,hipster-green+4))))
   `(org-level-3 ((t (:foreground ,hipster-blue-1))))
   `(org-level-4 ((t (:foreground ,hipster-yellow-2))))
   `(org-level-5 ((t (:foreground ,hipster-cyan))))
   `(org-level-6 ((t (:foreground ,hipster-green+2))))
   `(org-level-7 ((t (:foreground ,hipster-red-4))))
   `(org-level-8 ((t (:foreground ,hipster-blue-4))))
   `(org-link ((t (:foreground ,hipster-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,hipster-green+4))))
   `(org-scheduled-previously ((t (:foreground ,hipster-red))))
   `(org-scheduled-today ((t (:foreground ,hipster-blue+1))))
   `(org-sexp-date ((t (:foreground ,hipster-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,hipster-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,hipster-orange))))
   `(org-todo ((t (:background, hipster-red-1 :bold t :foreground ,hipster-bg :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,hipster-red :weight bold :underline nil))))
   `(org-column ((t (:background ,hipster-bg-1))))
   `(org-column-title ((t (:background ,hipster-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,hipster-fg :background ,hipster-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,hipster-bg :background ,hipster-red-1))))
   `(org-ellipsis ((t (:foreground ,hipster-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,hipster-cyan :underline t))))
;;;;; outline
   `(outline-1 ((t (:foreground ,hipster-orange))))
   `(outline-2 ((t (:foreground ,hipster-green+4))))
   `(outline-3 ((t (:foreground ,hipster-blue-1))))
   `(outline-4 ((t (:foreground ,hipster-yellow-2))))
   `(outline-5 ((t (:foreground ,hipster-cyan))))
   `(outline-6 ((t (:foreground ,hipster-green+2))))
   `(outline-7 ((t (:foreground ,hipster-red-4))))
   `(outline-8 ((t (:foreground ,hipster-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,hipster-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,hipster-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,hipster-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,hipster-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,hipster-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,hipster-fg :background ,hipster-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,hipster-bg :background ,hipster-orange))))
   `(proof-error-face ((t (:foreground ,hipster-fg :background ,hipster-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,hipster-bg :background ,hipster-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,hipster-bg :background ,hipster-orange))))
   `(proof-locked-face ((t (:background ,hipster-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,hipster-bg :background ,hipster-orange))))
   `(proof-queue-face ((t (:background ,hipster-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,hipster-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,hipster-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,hipster-bg))))
   `(proof-warning-face ((t (:foreground ,hipster-bg :background ,hipster-yellow-1))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,hipster-cyan))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,hipster-yellow))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,hipster-blue+1))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,hipster-red+1))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,hipster-green+1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,hipster-blue-1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,hipster-orange))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,hipster-magenta))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,hipster-yellow-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,hipster-green+2))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,hipster-blue+1))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,hipster-red-4))))
;;;;; rainbow-delimiters
   ;; `(rainbow-delimiters-depth-1-face ((t (:foreground ,hipster-fg))))
   ;; `(rainbow-delimiters-depth-2-face ((t (:foreground ,hipster-green+4))))
   ;; `(rainbow-delimiters-depth-3-face ((t (:foreground ,hipster-yellow-2))))
   ;; `(rainbow-delimiters-depth-4-face ((t (:foreground ,hipster-cyan))))
   ;; `(rainbow-delimiters-depth-5-face ((t (:foreground ,hipster-green+2))))
   ;; `(rainbow-delimiters-depth-6-face ((t (:foreground ,hipster-blue+1))))
   ;; `(rainbow-delimiters-depth-7-face ((t (:foreground ,hipster-yellow-1))))
   ;; `(rainbow-delimiters-depth-8-face ((t (:foreground ,hipster-green+1))))
   ;; `(rainbow-delimiters-depth-9-face ((t (:foreground ,hipster-blue-2))))
   ;; `(rainbow-delimiters-depth-10-face ((t (:foreground ,hipster-orange))))
   ;; `(rainbow-delimiters-depth-11-face ((t (:foreground ,hipster-green))))
   ;; `(rainbow-delimiters-depth-12-face ((t (:foreground ,hipster-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,hipster-blue))))
   `(rcirc-other-nick ((t (:foreground ,hipster-orange))))
   `(rcirc-bright-nick ((t (:foreground ,hipster-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,hipster-blue-2))))
   `(rcirc-server ((t (:foreground ,hipster-green))))
   `(rcirc-server-prefix ((t (:foreground ,hipster-green+1))))
   `(rcirc-timestamp ((t (:foreground ,hipster-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,hipster-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,hipster-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,hipster-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,hipster-green))))
   `(rpm-spec-doc-face ((t (:foreground ,hipster-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,hipster-red))))
   `(rpm-spec-macro-face ((t (:foreground ,hipster-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,hipster-red))))
   `(rpm-spec-package-face ((t (:foreground ,hipster-red))))
   `(rpm-spec-section-face ((t (:foreground ,hipster-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,hipster-blue))))
   `(rpm-spec-var-face ((t (:foreground ,hipster-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,hipster-orange))))
   `(rst-level-2-face ((t (:foreground ,hipster-green+1))))
   `(rst-level-3-face ((t (:foreground ,hipster-blue-1))))
   `(rst-level-4-face ((t (:foreground ,hipster-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,hipster-cyan))))
   `(rst-level-6-face ((t (:foreground ,hipster-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,hipster-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,hipster-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,hipster-red+1 :background ,hipster-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,hipster-bg+3 :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,hipster-red+1 :background ,hipster-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,hipster-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,hipster-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,hipster-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-red)))
      (t
       (:underline ,hipster-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-orange)))
      (t
       (:underline ,hipster-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-yellow)))
      (t
       (:underline ,hipster-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hipster-green)))
      (t
       (:underline ,hipster-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,hipster-green+2))))
   `(speedbar-directory-face ((t (:foreground ,hipster-cyan))))
   `(speedbar-file-face ((t (:foreground ,hipster-fg))))
   `(speedbar-highlight-face ((t (:foreground ,hipster-bg :background ,hipster-green+2))))
   `(speedbar-selected-face ((t (:foreground ,hipster-red))))
   `(speedbar-separator-face ((t (:foreground ,hipster-bg :background ,hipster-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,hipster-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,hipster-fg
                                    :background ,hipster-bg))))
   `(tabbar-selected ((t (:foreground ,hipster-fg
                                      :background ,hipster-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,hipster-fg
                                        :background ,hipster-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,hipster-bg
                                       :background ,hipster-bg-1))))
   `(term-color-red ((t (:foreground ,hipster-red-2
                                       :background ,hipster-red-4))))
   `(term-color-green ((t (:foreground ,hipster-green
                                       :background ,hipster-green+2))))
   `(term-color-yellow ((t (:foreground ,hipster-orange
                                       :background ,hipster-yellow))))
   `(term-color-blue ((t (:foreground ,hipster-blue-1
                                      :background ,hipster-blue-4))))
   `(term-color-magenta ((t (:foreground ,hipster-magenta
                                         :background ,hipster-red))))
   `(term-color-cyan ((t (:foreground ,hipster-cyan
                                       :background ,hipster-blue))))
   `(term-color-white ((t (:foreground ,hipster-fg
                                       :background ,hipster-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,hipster-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,hipster-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,hipster-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,hipster-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,hipster-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,hipster-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,hipster-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,hipster-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,hipster-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,hipster-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,hipster-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,hipster-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,hipster-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,hipster-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,hipster-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,hipster-bg+1 :foreground ,hipster-bg+1))))
   `(whitespace-hspace ((t (:background ,hipster-bg+1 :foreground ,hipster-bg+1))))
   `(whitespace-tab ((t (:background ,hipster-red-1))))
   `(whitespace-newline ((t (:foreground ,hipster-bg+1))))
   `(whitespace-trailing ((t (:background ,hipster-red))))
   `(whitespace-line ((t (:background ,hipster-bg :foreground ,hipster-magenta))))
   `(whitespace-space-before-tab ((t (:background ,hipster-orange :foreground ,hipster-orange))))
   `(whitespace-indentation ((t (:background ,hipster-yellow :foreground ,hipster-red))))
   `(whitespace-empty ((t (:background ,hipster-yellow))))
   `(whitespace-space-after-tab ((t (:background ,hipster-yellow :foreground ,hipster-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,hipster-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,hipster-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,hipster-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,hipster-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,hipster-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,hipster-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,hipster-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,hipster-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,hipster-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,hipster-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,hipster-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,hipster-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,hipster-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,hipster-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,hipster-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,hipster-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,hipster-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,hipster-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,hipster-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,hipster-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,hipster-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,hipster-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,hipster-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,hipster-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,hipster-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,hipster-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,hipster-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,hipster-bg-1 :foreground ,hipster-bg-1))))
   ))

;;; Theme Variables
(hipster-with-color-variables
  (custom-theme-set-variables
   'hipster
;;;;; ansi-color
   `(ansi-color-names-vector [,hipster-bg ,hipster-red ,hipster-green ,hipster-yellow
                                          ,hipster-blue ,hipster-magenta ,hipster-cyan ,hipster-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,hipster-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,hipster-red-1)
       ( 40. . ,hipster-red)
       ( 60. . ,hipster-orange)
       ( 80. . ,hipster-yellow-2)
       (100. . ,hipster-yellow-1)
       (120. . ,hipster-yellow)
       (140. . ,hipster-green-1)
       (160. . ,hipster-green)
       (180. . ,hipster-green+1)
       (200. . ,hipster-green+2)
       (220. . ,hipster-green+3)
       (240. . ,hipster-green+4)
       (260. . ,hipster-cyan)
       (280. . ,hipster-blue-2)
       (300. . ,hipster-blue-1)
       (320. . ,hipster-blue)
       (340. . ,hipster-blue+1)
       (360. . ,hipster-magenta)))
   `(vc-annotate-very-old-color ,hipster-magenta)
   `(vc-annotate-background ,hipster-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar hipster-add-font-lock-keywords nil
  "Whether to add font-lock keywords for hipster color names.
In buffers visiting library `hipster-theme.el' the hipster
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar hipster-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after hipster activate)
;;   "Maybe also add font-lock keywords for hipster colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or hipster-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "hipster-theme.el")))
;;     (unless hipster-colors-font-lock-keywords
;;       (setq hipster-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car hipster-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc hipster-colors-alist))))))
;;     (font-lock-add-keywords nil hipster-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after hipster activate)
;;   "Also remove font-lock keywords for hipster colors."
;;   (font-lock-remove-keywords nil hipster-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'hipster)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; hipster-theme.el ends here
