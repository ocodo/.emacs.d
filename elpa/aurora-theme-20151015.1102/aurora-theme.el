;;; aurora-theme.el --- A theme inspired by SublimeText's Material theme

;; Author: Luis Cairampoma <redshacker11@gmail.com>
;; URL: http://github.com/xzerocode/aurora-theme
;; Package-Version: 20151015.1102
;; Package-X-Original-Version: 20150818.1359
;; Version: 0.1

;;; Commentary:

;; This theme is inspired by the popular theme of Sublime Text, 'Material'.

;;; Credits:

;;  A Mattia Astorino, by the colors that made the end this theme.

;;; Code:

(deftheme aurora "Aurora theme")

;;; Color Palette
(defvar aurora-colors-alist
  '(("aurora-fg+1"     . "#FFFFFF")
    ("aurora-fg"       . "#CDD3D3")
    ("aurora-fg-1"     . "#000000")
    ("aurora-bg-2"     . "#FFFFFF")
    ("aurora-bg-1"     . "#2B3B40")
    ("aurora-bg-05"    . "#232A2F")
    ("aurora-bg"       . "#263238")
    ("aurora-bg+05"    . "#324148")
    ("aurora-bg+1"     . "#232A2F")
    ("aurora-bg+2"     . "#FFFFFF")
    ("aurora-bg+3"     . "#455a64")
    ("aurora-red"      . "#74CBC4")
    ("aurora-orange"   . "#C2E982")
    ("aurora-yellow"   . "#C792EA")
    ("aurora-yellow+1" . "#FFC400")
    ("aurora-green"    . "#546D7A")
    ("aurora-green+1"  . "#FF516D")
    ("aurora-green+2"  . "#9FC59F")
    ("aurora-green+3"  . "#859900")
    ("aurora-green+4"  . "#F77669")
    ("aurora-cyan"     . "#FF516D")
    ("aurora-blue+1"   . "#D9F5DD")
    ("aurora-blue"     . "#82B1FF")
    ("aurora-magenta"  . "#FFCB6B"))
  "List of aurora colors.
Each element has the form (NAME . HEX).
`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro aurora-with-color-variables (&rest body)
  "`let' bind all colors defined in `aurora-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   aurora-colors-alist))
     ,@body))

;;; Theme Faces
(aurora-with-color-variables
  (custom-theme-set-faces
   'aurora
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,aurora-magenta :underline t :weight bold))))
   `(link-visited ((t (:foreground ,aurora-yellow+1 :underline t :weight normal))))
   `(default ((t (:foreground ,aurora-fg :background ,aurora-bg))))
   `(cursor ((t (:foreground ,aurora-fg :background ,aurora-fg+1))))
   `(escape-glyph ((t (:foreground ,aurora-yellow :bold t)))) 
   `(fringe ((t (:foreground ,aurora-fg :background ,aurora-bg))))
   `(header-line ((t (:foreground ,aurora-magenta
                                  :background ,aurora-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,aurora-bg-05))))
   `(success ((t (:foreground ,aurora-green :weight bold))))
   `(warning ((t (:foreground ,aurora-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,aurora-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,aurora-green))))
   `(compilation-error-face ((t (:foreground ,aurora-red :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,aurora-fg))))
   `(compilation-info-face ((t (:foreground ,aurora-blue))))
   `(compilation-info ((t (:foreground ,aurora-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,aurora-green))))
   `(compilation-line-face ((t (:foreground ,aurora-yellow))))
   `(compilation-line-number ((t (:foreground ,aurora-yellow))))
   `(compilation-message-face ((t (:foreground ,aurora-blue))))
   `(compilation-warning-face ((t (:foreground ,aurora-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,aurora-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,aurora-cyan :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,aurora-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,aurora-fg))))
   `(grep-error-face ((t (:foreground ,aurora-cyan :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,aurora-blue))))
   `(grep-match-face ((t (:foreground ,aurora-orange :weight bold))))
   `(match ((t (:background ,aurora-bg-1 :foreground ,aurora-orange :weight bold))))

;;;;; isearch
   `(isearch ((t (:foreground ,aurora-yellow+1 :weight bold :background ,aurora-bg-05))))
   `(isearch-fail ((t (:foreground ,aurora-fg :background ,"#e53935"))))
   `(lazy-highlight ((t (:foreground ,aurora-yellow+1 :weight bold :background ,aurora-bg-05))))

   `(menu ((t (:foreground ,aurora-fg :background ,aurora-bg-1))))
   `(minibuffer-prompt ((t (:foreground ,aurora-red))))
   `(mode-line
     ((,class (:foreground ,"#FFFFFF"
                           :background ,"#324148"
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
  `(mode-line-buffer-id ((t (:foreground ,aurora-magenta :weight bold))))
  `(mode-line-inactive
     ((t (:foreground ,aurora-fg
                      :background ,"#516872"
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,aurora-bg+3))

             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,aurora-bg+3))))
   `(trailing-whitespace ((t (:background ,aurora-cyan))))
   `(vertical-border ((t (:foreground ,aurora-bg+3))))

;;;;; powerline buffer
   `(powerline-active1 ((t (:background ,"#516872" :inherit mode-line))))
   `(powerline-active2 ((t (:background ,"#324148" :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,"#324148" :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,"#324148" :inherit mode-line-inactive))))

;;;;; font lock212121
   `(font-lock-builtin-face ((t (:foreground ,aurora-red :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,aurora-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,aurora-green))))
   `(font-lock-constant-face ((t (:foreground ,aurora-green+4))))
   `(font-lock-doc-face ((t (:foreground ,aurora-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,aurora-cyan))))
   `(font-lock-doc-string-face ((t (:foreground , aurora-fg-1))))
   `(font-lock-keyword-face ((t (:foreground ,aurora-yellow))))
   `(font-lock-negation-char-face ((t (:foreground ,aurora-magenta :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,aurora-red))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,aurora-yellow+1 :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,aurora-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,aurora-orange))))
   `(font-lock-type-face ((t (:foreground ,aurora-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,aurora-red))))
   `(font-lock-warning-face ((t (:foreground ,aurora-yellow+1 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,aurora-fg))))
   `(newsticker-default-face ((t (:foreground ,aurora-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,aurora-green+3))))
   `(newsticker-extra-face ((t (:foreground ,aurora-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,aurora-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,aurora-green))))
   `(newsticker-new-item-face ((t (:foreground ,aurora-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,aurora-red))))
   `(newsticker-old-item-face ((t (:foreground ,aurora-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,aurora-fg))))
   `(newsticker-treeview-face ((t (:foreground ,aurora-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,aurora-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,aurora-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,aurora-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,aurora-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,aurora-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,aurora-bg-1 :foreground ,aurora-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,aurora-fg-1 :background ,aurora-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,aurora-green+2 :background ,aurora-bg :inverse-video nil))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,aurora-green+1))))
   `(android-mode-error-face ((t (:foreground ,aurora-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,aurora-fg))))
   `(android-mode-verbose-face ((t (:foreground ,aurora-green))))
   `(android-mode-warning-face ((t (:foreground ,aurora-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,aurora-cyan :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,aurora-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,aurora-yellow))))
   `(font-latex-italic-face ((t (:foreground ,aurora-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,aurora-orange))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,aurora-bg+3 :foreground ,aurora-bg-2))))
   `(ac-selection-face ((t (:background ,aurora-blue :foreground ,aurora-fg))))
   `(popup-tip-face ((t (:background ,aurora-yellow+1 :foreground ,aurora-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,aurora-blue))))
   `(popup-scroll-bar-background-face ((t (:background ,aurora-bg-1))))
   `(popup-isearch-match ((t (:background ,aurora-bg :foreground ,aurora-fg))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,aurora-yellow :background ,aurora-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,aurora-blue :background ,aurora-bg+1))))
   `(company-tooltip-selection ((t (:foreground ,aurora-fg :background ,aurora-bg-1))))
   `(company-template-field ((t (:foreground ,aurora-magenta :background ,aurora-bg-1))))
   `(company-tooltip-mouse ((t (:background ,aurora-bg-1))))
   `(company-tooltip-common ((t (:foreground ,aurora-yellow :background ,aurora-bg-1))))
   `(company-tooltip-common-selection ((t (:background ,aurora-bg-1))))
   `(company-scrollbar-fg ((t (:background ,aurora-green+1))))
   `(company-scrollbar-bg ((t (:background ,aurora-bg-1))))
   `(company-preview ((t (:background ,aurora-green+1))))
   `(company-preview-common ((t (:background ,aurora-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,aurora-yellow :foreground ,aurora-bg))))
   `(bm-fringe-face ((t (:background ,aurora-yellow :foreground ,aurora-bg))))
   `(bm-fringe-persistent-face ((t (:background ,aurora-green :foreground ,aurora-bg))))
   `(bm-persistent-face ((t (:background ,aurora-green :foreground ,aurora-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,aurora-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,aurora-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,aurora-green+1 :weight bold :underline t))))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,aurora-blue :foreground ,aurora-bg))))
   `(ctbl:face-continue-bar ((t (:background ,aurora-bg-05 :foreground ,aurora-bg))))
   `(ctbl:face-row-select ((t (:background ,aurora-cyan :foreground ,aurora-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,aurora-green+4 :background nil))
                 (t (:foreground ,aurora-green :background nil))))
   `(diff-changed ((t (:foreground ,aurora-yellow))))
   `(diff-removed ((,class (:foreground ,aurora-red :background nil))
                   (t (:foreground ,aurora-red :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,aurora-bg+2))
                  (t (:background ,aurora-fg :foreground ,aurora-bg))))
   `(diff-file-header
     ((,class (:background ,aurora-bg+2 :foreground ,aurora-fg :bold t))
      (t (:background ,aurora-fg :foreground ,aurora-bg :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,aurora-blue :background ,aurora-bg-05))))
   `(diff-hl-delete ((,class (:foreground ,aurora-red :background ,aurora-bg-05))))
   `(diff-hl-insert ((,class (:foreground ,aurora-green+1 :background ,aurora-bg-05))))
   `(diff-hl-unknown ((,class (:foreground ,aurora-yellow :background ,aurora-bg-05))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,aurora-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,aurora-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,aurora-orange))))
   `(diredp-date-time ((t (:foreground ,aurora-magenta))))
   `(diredp-deletion ((t (:foreground ,aurora-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,aurora-red))))
   `(diredp-dir-heading ((t (:foreground ,aurora-blue :background ,aurora-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,aurora-cyan))))
   `(diredp-exec-priv ((t (:foreground ,aurora-red))))
   `(diredp-executable-tag ((t (:foreground ,aurora-green+1))))
   `(diredp-file-name ((t (:foreground ,aurora-blue))))
   `(diredp-file-suffix ((t (:foreground ,aurora-green))))
   `(diredp-flag-mark ((t (:foreground ,aurora-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,aurora-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,aurora-red))))
   `(diredp-link-priv ((t (:foreground ,aurora-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,aurora-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,aurora-orange))))
   `(diredp-no-priv ((t (:foreground ,aurora-fg))))
   `(diredp-number ((t (:foreground ,aurora-green+1))))
   `(diredp-other-priv ((t (:foreground ,aurora-yellow))))
   `(diredp-rare-priv ((t (:foreground ,aurora-red))))
   `(diredp-read-priv ((t (:foreground ,aurora-green))))
   `(diredp-symlink ((t (:foreground ,aurora-yellow))))
   `(diredp-write-priv ((t (:foreground ,aurora-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,aurora-fg :background ,aurora-red))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,aurora-fg :background ,aurora-red))))
   `(ediff-current-diff-B ((t (:foreground ,aurora-fg :background ,aurora-green))))
   `(ediff-current-diff-C ((t (:foreground ,aurora-fg :background ,aurora-blue))))
   `(ediff-even-diff-A ((t (:background ,aurora-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,aurora-bg+1))))
   `(ediff-even-diff-B ((t (:background ,aurora-bg+1))))
   `(ediff-even-diff-C ((t (:background ,aurora-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,aurora-fg :background ,aurora-red :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,aurora-fg :background ,aurora-red weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,aurora-fg :background ,aurora-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,aurora-fg :background ,aurora-blue :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,aurora-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,aurora-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,aurora-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,aurora-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,aurora-fg))))
   `(egg-help-header-1 ((t (:foreground ,aurora-yellow))))
   `(egg-help-header-2 ((t (:foreground ,aurora-green+3))))
   `(egg-branch ((t (:foreground ,aurora-yellow))))
   `(egg-branch-mono ((t (:foreground ,aurora-yellow))))
   `(egg-term ((t (:foreground ,aurora-yellow))))
   `(egg-diff-add ((t (:foreground ,aurora-green+4))))
   `(egg-diff-del ((t (:foreground ,aurora-red))))
   `(egg-diff-file-header ((t (:foreground ,aurora-yellow+1))))
   `(egg-section-title ((t (:foreground ,aurora-yellow))))
   `(egg-stash-mono ((t (:foreground ,aurora-green+4))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,aurora-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,aurora-yellow+1
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,aurora-red :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,aurora-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,aurora-green+2 :background ,aurora-bg))))
   `(w3m-lnum-match ((t (:background ,aurora-bg-1
                                     :foreground ,aurora-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,aurora-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,aurora-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,aurora-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,aurora-yellow))))
   `(erc-keyword-face ((t (:foreground ,aurora-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,aurora-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,aurora-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,aurora-green))))
   `(erc-pal-face ((t (:foreground ,aurora-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,aurora-orange :background ,aurora-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,aurora-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,aurora-green+4 :background ,aurora-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,aurora-red :background ,aurora-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,aurora-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,aurora-red :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,aurora-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,aurora-red :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,aurora-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,aurora-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,aurora-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,aurora-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-red) :inherit unspecified))
      (t (:foreground ,aurora-red :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-yellow) :inherit unspecified))
      (t (:foreground ,aurora-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-cyan) :inherit unspecified))
      (t (:foreground ,aurora-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,aurora-red :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,aurora-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,aurora-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,aurora-red :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,aurora-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,aurora-green :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-orange) :inherit unspecified))
      (t (:foreground ,aurora-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-red) :inherit unspecified))
      (t (:foreground ,aurora-red :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,aurora-fg))))
   `(ack-file ((t (:foreground ,aurora-blue))))
   `(ack-line ((t (:foreground ,aurora-yellow))))
   `(ack-match ((t (:foreground ,aurora-orange :background ,aurora-bg-1 :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,aurora-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,aurora-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,aurora-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,aurora-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,aurora-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,aurora-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,aurora-magenta :weight bold))))
;;;;; git-rebase-mode
   `(git-rebase-hash ((t (:foreground, aurora-orange))))
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
   `(gnus-summary-cancelled ((t (:foreground ,aurora-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,aurora-blue))))
   `(gnus-summary-high-read ((t (:foreground ,aurora-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,aurora-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,aurora-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,aurora-blue))))
   `(gnus-summary-low-read ((t (:foreground ,aurora-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,aurora-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,aurora-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,aurora-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,aurora-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,aurora-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,aurora-fg))))
   `(gnus-summary-selected ((t (:foreground ,aurora-magenta :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,aurora-blue))))
   `(gnus-cite-10 ((t (:foreground ,aurora-yellow))))
   `(gnus-cite-11 ((t (:foreground ,aurora-yellow))))
   `(gnus-cite-2 ((t (:foreground ,aurora-blue))))
   `(gnus-cite-3 ((t (:foreground ,aurora-blue))))
   `(gnus-cite-4 ((t (:foreground ,aurora-green+2))))
   `(gnus-cite-5 ((t (:foreground ,aurora-green+1))))
   `(gnus-cite-6 ((t (:foreground ,aurora-green))))
   `(gnus-cite-7 ((t (:foreground ,aurora-red))))
   `(gnus-cite-8 ((t (:foreground ,aurora-red))))
   `(gnus-cite-9 ((t (:foreground ,aurora-red))))
   `(gnus-group-news-1-empty ((t (:foreground ,aurora-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,aurora-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,aurora-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,aurora-blue))))
   `(gnus-group-news-5-empty ((t (:foreground ,aurora-blue))))
   `(gnus-group-news-6-empty ((t (:foreground ,aurora-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,aurora-bg+2))))
   `(gnus-signature ((t (:foreground ,aurora-yellow))))
   `(gnus-x ((t (:background ,aurora-fg :foreground ,aurora-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,aurora-blue))))
   `(guide-key/key-face ((t (:foreground ,aurora-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,aurora-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,aurora-green
                      :background ,aurora-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,aurora-magenta
                      :background ,aurora-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,aurora-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,aurora-bg+1))))
   `(helm-visible-mark ((t (:foreground ,aurora-bg :background ,aurora-yellow+1))))
   `(helm-candidate-number ((t (:foreground ,aurora-green+4 :background ,aurora-bg-1))))
   `(helm-separator ((t (:foreground ,aurora-red :background ,aurora-bg))))
   `(helm-time-zone-current ((t (:foreground ,aurora-green+2 :background ,aurora-bg))))
   `(helm-time-zone-home ((t (:foreground ,aurora-red :background ,aurora-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,aurora-orange :background ,aurora-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,aurora-magenta :background ,aurora-bg))))
   `(helm-bookmark-info ((t (:foreground ,aurora-green+2 :background ,aurora-bg))))
   `(helm-bookmark-man ((t (:foreground ,aurora-yellow :background ,aurora-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,aurora-magenta :background ,aurora-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,aurora-red :background ,aurora-bg))))
   `(helm-buffer-process ((t (:foreground ,aurora-cyan :background ,aurora-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,aurora-fg :background ,aurora-bg))))
   `(helm-buffer-size ((t (:foreground ,aurora-fg-1 :background ,aurora-bg))))
   `(helm-ff-directory ((t (:foreground ,aurora-cyan :background ,aurora-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,aurora-fg :background ,aurora-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,aurora-green+2 :background ,aurora-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,aurora-red :background ,aurora-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,aurora-yellow :background ,aurora-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,aurora-bg :background ,aurora-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,aurora-cyan :background ,aurora-bg))))
   `(helm-grep-file ((t (:foreground ,aurora-fg :background ,aurora-bg))))
   `(helm-grep-finish ((t (:foreground ,aurora-green+2 :background ,aurora-bg))))
   `(helm-grep-lineno ((t (:foreground ,aurora-fg-1 :background ,aurora-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,aurora-red :background ,aurora-bg))))
   `(helm-moccur-buffer ((t (:foreground ,aurora-cyan :background ,aurora-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,aurora-fg-1 :background ,aurora-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,aurora-fg :background ,aurora-bg))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,aurora-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,aurora-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,aurora-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,aurora-cyan :weight bold))))
   `(ido-only-match ((t (:foreground ,aurora-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,aurora-magenta))))
   `(ido-indicator ((t (:foreground ,aurora-magenta :background ,aurora-red))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,aurora-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,aurora-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,aurora-blue))))
   `(jabber-roster-user-dnd ((t (:foreground ,aurora-red))))
   `(jabber-rare-time-face ((t (:foreground ,aurora-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,aurora-blue))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,aurora-red))))
   `(jabber-activity-face((t (:foreground ,aurora-red))))
   `(jabber-activity-personal-face ((t (:foreground ,aurora-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,aurora-orange))))
   `(js2-error ((t (:foreground ,aurora-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,aurora-green))))
   `(js2-jsdoc-type ((t (:foreground ,aurora-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,aurora-green+3))))
   `(js2-function-param ((t (:foreground, aurora-green+3))))
   `(js2-external-variable ((t (:foreground ,aurora-orange))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,aurora-red :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,aurora-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,aurora-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,aurora-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,aurora-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,aurora-blue))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,aurora-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,aurora-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,aurora-orange))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,aurora-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,aurora-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,aurora-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,aurora-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,aurora-red :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,aurora-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,aurora-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,aurora-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,aurora-green+2 :background ,aurora-bg))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,aurora-green+2 :background ,aurora-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,aurora-red :background ,aurora-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,aurora-blue+1 :background ,aurora-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,aurora-magenta :background ,aurora-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,aurora-yellow :background ,aurora-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-item-highlight ((t (:background ,aurora-bg+1))))
   `(magit-section-title ((t (:foreground ,aurora-yellow :weight bold))))
   `(magit-process-ok ((t (:foreground ,aurora-green :weight bold))))
   `(magit-process-ng ((t (:foreground ,aurora-red :weight bold))))
   `(magit-branch ((t (:foreground ,aurora-blue :weight bold))))
   `(magit-log-author ((t (:foreground ,aurora-orange))))
   `(magit-log-sha1 ((t (:foreground, aurora-orange))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,aurora-green+1))))
   `(message-header-other ((t (:foreground ,aurora-green))))
   `(message-header-to ((t (:foreground ,aurora-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,aurora-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,aurora-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,aurora-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,aurora-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,aurora-green))))
   `(message-mml ((t (:foreground ,aurora-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,aurora-orange))))
   `(mew-face-header-from ((t (:foreground ,aurora-yellow))))
   `(mew-face-header-date ((t (:foreground ,aurora-green))))
   `(mew-face-header-to ((t (:foreground ,aurora-red))))
   `(mew-face-header-key ((t (:foreground ,aurora-green))))
   `(mew-face-header-private ((t (:foreground ,aurora-green))))
   `(mew-face-header-important ((t (:foreground ,aurora-blue))))
   `(mew-face-header-marginal ((t (:foreground ,aurora-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,aurora-red))))
   `(mew-face-header-xmew ((t (:foreground ,aurora-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,aurora-red))))
   `(mew-face-body-url ((t (:foreground ,aurora-orange))))
   `(mew-face-body-comment ((t (:foreground ,aurora-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,aurora-green))))
   `(mew-face-body-cite2 ((t (:foreground ,aurora-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,aurora-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,aurora-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,aurora-red))))
   `(mew-face-mark-review ((t (:foreground ,aurora-blue))))
   `(mew-face-mark-escape ((t (:foreground ,aurora-green))))
   `(mew-face-mark-delete ((t (:foreground ,aurora-red))))
   `(mew-face-mark-unlink ((t (:foreground ,aurora-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,aurora-green))))
   `(mew-face-mark-unread ((t (:foreground ,aurora-red))))
   `(mew-face-eof-message ((t (:foreground ,aurora-green))))
   `(mew-face-eof-part ((t (:foreground ,aurora-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,aurora-cyan :background ,aurora-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,aurora-bg :background ,aurora-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,aurora-bg :background ,aurora-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,aurora-blue))))
   `(mingus-pausing-face ((t (:foreground ,aurora-magenta))))
   `(mingus-playing-face ((t (:foreground ,aurora-cyan))))
   `(mingus-playlist-face ((t (:foreground ,aurora-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,aurora-yellow))))
   `(mingus-stopped-face ((t (:foreground ,aurora-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,aurora-yellow))))
   `(nav-face-button-num ((t (:foreground ,aurora-cyan))))
   `(nav-face-dir ((t (:foreground ,aurora-green))))
   `(nav-face-hdir ((t (:foreground ,aurora-red))))
   `(nav-face-file ((t (:foreground ,aurora-fg))))
   `(nav-face-hfile ((t (:foreground ,aurora-red))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,aurora-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,aurora-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,aurora-blue  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,aurora-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,aurora-blue  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,aurora-green :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,aurora-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,aurora-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,aurora-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,aurora-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,aurora-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,aurora-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,aurora-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,aurora-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,aurora-fg :weight bold))))
   `(org-checkbox ((t (:background ,aurora-bg+05 :foreground ,aurora-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,aurora-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,aurora-red))))
   `(org-done ((t (:bold t :weight bold :foreground ,aurora-red))))
   `(org-formula ((t (:foreground ,aurora-yellow+1))))
   `(org-headline-done ((t (:foreground ,aurora-green+3))))
   `(org-hide ((t (:foreground ,aurora-bg-1))))
   `(org-level-1 ((t (:foreground ,aurora-orange))))
   `(org-level-2 ((t (:foreground ,aurora-green+4))))
   `(org-level-3 ((t (:foreground ,aurora-blue))))
   `(org-level-4 ((t (:foreground ,aurora-yellow+1))))
   `(org-level-5 ((t (:foreground ,aurora-cyan))))
   `(org-level-6 ((t (:foreground ,aurora-green+2))))
   `(org-level-7 ((t (:foreground ,aurora-red))))
   `(org-level-8 ((t (:foreground ,aurora-blue+1))))
   `(org-link ((t (:foreground ,aurora-yellow+1 :underline t))))
   `(org-scheduled ((t (:foreground ,aurora-green+4))))
   `(org-scheduled-previously ((t (:foreground ,aurora-red))))
   `(org-scheduled-today ((t (:foreground ,aurora-blue+1))))
   `(org-sexp-date ((t (:foreground ,aurora-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,aurora-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,aurora-orange))))
   `(org-todo ((t (:foreground ,aurora-cyan :weight bold)))) ;;ToDo
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,aurora-red :weight bold :underline nil))))
   `(org-column ((t (:background ,aurora-bg-1))))
   `(org-column-title ((t (:background ,aurora-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,aurora-fg :background ,aurora-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,aurora-bg :background ,aurora-red))))
   `(org-ellipsis ((t (:foreground ,aurora-yellow :underline t))))
   `(org-footnote ((t (:foreground ,aurora-cyan :underline t))))
;;;;; outline
   `(outline-1 ((t (:foreground ,aurora-orange))))
   `(outline-2 ((t (:foreground ,aurora-green+4))))
   `(outline-3 ((t (:foreground ,aurora-blue))))
   `(outline-4 ((t (:foreground ,aurora-yellow+1))))
   `(outline-5 ((t (:foreground ,aurora-cyan))))
   `(outline-6 ((t (:foreground ,aurora-green+2))))
   `(outline-7 ((t (:foreground ,aurora-red))))
   `(outline-8 ((t (:foreground ,aurora-blue))))
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
   `(persp-selected-face ((t (:foreground ,aurora-yellow+1 :inherit mode-line))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,aurora-fg :background ,aurora-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,aurora-bg :background ,aurora-orange))))
   `(proof-error-face ((t (:foreground ,aurora-fg :background ,aurora-red))))
   `(proof-highlight-dependency-face ((t (:foreground ,aurora-bg :background ,aurora-yellow))))
   `(proof-highlight-dependent-face ((t (:foreground ,aurora-bg :background ,aurora-orange))))
   `(proof-locked-face ((t (:background ,aurora-blue))))
   `(proof-mouse-highlight-face ((t (:foreground ,aurora-bg :background ,aurora-orange))))
   `(proof-queue-face ((t (:background ,aurora-red))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,aurora-red))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,aurora-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,aurora-bg))))
   `(proof-warning-face ((t (:foreground ,aurora-bg :background ,aurora-yellow))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,aurora-cyan))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,aurora-yellow))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,aurora-blue+1))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,aurora-red))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,aurora-green+1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,aurora-blue))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,aurora-orange))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,aurora-magenta))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,aurora-yellow+1))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,aurora-green+2))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,aurora-blue+1))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,aurora-red))))
;;;;; rainbow-delimiters
   ;; `(rainbow-delimiters-depth-1-face ((t (:foreground ,aurora-fg))))
   ;; `(rainbow-delimiters-depth-2-face ((t (:foreground ,aurora-green+4))))
   ;; `(rainbow-delimiters-depth-3-face ((t (:foreground ,aurora-yellow+1))))
   ;; `(rainbow-delimiters-depth-4-face ((t (:foreground ,aurora-cyan))))
   ;; `(rainbow-delimiters-depth-5-face ((t (:foreground ,aurora-green+2))))
   ;; `(rainbow-delimiters-depth-6-face ((t (:foreground ,aurora-blue+1))))
   ;; `(rainbow-delimiters-depth-7-face ((t (:foreground ,aurora-yellow))))
   ;; `(rainbow-delimiters-depth-8-face ((t (:foreground ,aurora-green+1))))
   ;; `(rainbow-delimiters-depth-9-face ((t (:foreground ,aurora-blue))))
   ;; `(rainbow-delimiters-depth-10-face ((t (:foreground ,aurora-orange))))
   ;; `(rainbow-delimiters-depth-11-face ((t (:foreground ,aurora-green))))
   ;; `(rainbow-delimiters-depth-12-face ((t (:foreground ,aurora-blue))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,aurora-blue))))
   `(rcirc-other-nick ((t (:foreground ,aurora-orange))))
   `(rcirc-bright-nick ((t (:foreground ,aurora-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,aurora-blue))))
   `(rcirc-server ((t (:foreground ,aurora-green))))
   `(rcirc-server-prefix ((t (:foreground ,aurora-green+1))))
   `(rcirc-timestamp ((t (:foreground ,aurora-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,aurora-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,aurora-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,aurora-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,aurora-green))))
   `(rpm-spec-doc-face ((t (:foreground ,aurora-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,aurora-red))))
   `(rpm-spec-macro-face ((t (:foreground ,aurora-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,aurora-red))))
   `(rpm-spec-package-face ((t (:foreground ,aurora-red))))
   `(rpm-spec-section-face ((t (:foreground ,aurora-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,aurora-blue))))
   `(rpm-spec-var-face ((t (:foreground ,aurora-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,aurora-orange))))
   `(rst-level-2-face ((t (:foreground ,aurora-green+1))))
   `(rst-level-3-face ((t (:foreground ,aurora-blue))))
   `(rst-level-4-face ((t (:foreground ,aurora-yellow+1))))
   `(rst-level-5-face ((t (:foreground ,aurora-cyan))))
   `(rst-level-6-face ((t (:foreground ,aurora-green))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,aurora-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,aurora-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,aurora-red :background ,aurora-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,aurora-bg+3 :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,aurora-red :background ,aurora-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,aurora-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,aurora-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,aurora-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-red)))
      (t
       (:underline ,aurora-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-orange)))
      (t
       (:underline ,aurora-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-yellow)))
      (t
       (:underline ,aurora-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,aurora-green)))
      (t
       (:underline ,aurora-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,aurora-green+2))))
   `(speedbar-directory-face ((t (:foreground ,aurora-cyan))))
   `(speedbar-file-face ((t (:foreground ,aurora-fg))))
   `(speedbar-highlight-face ((t (:foreground ,aurora-bg :background ,aurora-green+2))))
   `(speedbar-selected-face ((t (:foreground ,aurora-red))))
   `(speedbar-separator-face ((t (:foreground ,aurora-bg :background ,aurora-blue))))
   `(speedbar-tag-face ((t (:foreground ,aurora-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,aurora-fg
                                    :background ,aurora-bg))))
   `(tabbar-selected ((t (:foreground ,aurora-fg
                                      :background ,aurora-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,aurora-fg
                                        :background ,aurora-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,aurora-bg
                                       :background ,aurora-bg-1))))
   `(term-color-red ((t (:foreground ,aurora-red
                                       :background ,aurora-red))))
   `(term-color-green ((t (:foreground ,aurora-green
                                       :background ,aurora-green+2))))
   `(term-color-yellow ((t (:foreground ,aurora-orange
                                       :background ,aurora-yellow))))
   `(term-color-blue ((t (:foreground ,aurora-blue
                                      :background ,aurora-blue))))
   `(term-color-magenta ((t (:foreground ,aurora-magenta
                                         :background ,aurora-red))))
   `(term-color-cyan ((t (:foreground ,aurora-cyan
                                       :background ,aurora-blue))))
   `(term-color-white ((t (:foreground ,aurora-fg
                                       :background ,aurora-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,aurora-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,aurora-red :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,aurora-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,aurora-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,aurora-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,aurora-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,aurora-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,aurora-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,aurora-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,aurora-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,aurora-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,aurora-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,aurora-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,aurora-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,aurora-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,aurora-bg+1 :foreground ,aurora-bg+1))))
   `(whitespace-hspace ((t (:background ,aurora-bg+1 :foreground ,aurora-bg+1))))
   `(whitespace-tab ((t (:background ,aurora-red))))
   `(whitespace-newline ((t (:foreground ,aurora-bg+1))))
   `(whitespace-trailing ((t (:background ,aurora-red))))
   `(whitespace-line ((t (:background ,aurora-bg :foreground ,aurora-magenta))))
   `(whitespace-space-before-tab ((t (:background ,aurora-orange :foreground ,aurora-orange))))
   `(whitespace-indentation ((t (:background ,aurora-yellow :foreground ,aurora-red))))
   `(whitespace-empty ((t (:background ,aurora-yellow))))
   `(whitespace-space-after-tab ((t (:background ,aurora-yellow :foreground ,aurora-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,aurora-red))))
   `(wl-highlight-folder-many-face ((t (:foreground ,aurora-red))))
   `(wl-highlight-folder-path-face ((t (:foreground ,aurora-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,aurora-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,aurora-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,aurora-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,aurora-red))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,aurora-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,aurora-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,aurora-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,aurora-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,aurora-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,aurora-red))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,aurora-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,aurora-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,aurora-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,aurora-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,aurora-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,aurora-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,aurora-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,aurora-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,aurora-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,aurora-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,aurora-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,aurora-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,aurora-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,aurora-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,aurora-bg-1 :foreground ,aurora-bg-1))))
   ))

;;; Theme Variables
(aurora-with-color-variables
  (custom-theme-set-variables
   'aurora
;;;;; ansi-color
   `(ansi-color-names-vector [,aurora-bg ,aurora-red ,aurora-green ,aurora-yellow
                                          ,aurora-blue ,aurora-magenta ,aurora-cyan ,aurora-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,aurora-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,aurora-red)
       ( 40. . ,aurora-red)
       ( 60. . ,aurora-orange)
       ( 80. . ,aurora-yellow+1)
       (100. . ,aurora-yellow)
       (120. . ,aurora-yellow)
       (140. . ,aurora-green)
       (160. . ,aurora-green)
       (180. . ,aurora-green+1)
       (200. . ,aurora-green+2)
       (220. . ,aurora-green+3)
       (240. . ,aurora-green+4)
       (260. . ,aurora-cyan)
       (280. . ,aurora-blue)
       (300. . ,aurora-blue)
       (320. . ,aurora-blue)
       (340. . ,aurora-blue+1)
       (360. . ,aurora-magenta)))
   `(vc-annotate-very-old-color ,aurora-magenta)
   `(vc-annotate-background ,aurora-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar aurora-add-font-lock-keywords nil
  "Whether to add font-lock keywords for aurora color names.
In buffers visiting library `aurora-theme.el' the aurora
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar aurora-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after aurora activate)
;;   "Maybe also add font-lock keywords for aurora colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or aurora-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "aurora-theme.el")))
;;     (unless aurora-colors-font-lock-keywords
;;       (setq aurora-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car aurora-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc aurora-colors-alist))))))
;;     (font-lock-add-keywords nil aurora-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after aurora activate)
;;   "Also remove font-lock keywords for aurora colors."
;;   (font-lock-remove-keywords nil aurora-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'aurora)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; aurora-theme.el ends here
