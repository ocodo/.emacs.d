;;; darkburn-theme.el --- A not-so-low contrast color theme for Emacs.

;; Copyright 2014 Jonas Gorauskas

;; Author: Jonas Gorauskas <jgorauskas@gmail.com>
;; URL: http://github.com/gorauskas/darkburn-theme
;; Version: 20140510.1449
;; X-Original-Version: 0.2

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

;; A mod of the Zenburn theme for Emacs 24 by Bozhidar Batsov to make
;; backgrounds a little darker. The original can be found on github at:
;; http://github.com/bbatsov/zenburn-emacs

;;; Credits:

;; Bozhidar deserves all the credit! I only changed a few colors.

;;; Code:

(deftheme darkburn "The Darkburn color theme")

;;; Color Palette

(defvar darkburn-colors-alist
  '(("darkburn-fg+1"     . "#FFFFEF")
    ("darkburn-fg"       . "#DCDCCC")
    ("darkburn-fg-1"     . "#656555")
    ("darkburn-bg-2"     . "#000000")
    ("darkburn-bg-15"    . "#111111")       ;; DB
    ("darkburn-bg-1"     . "#2B2B2B")
    ("darkburn-bg-05"    . "#383838")
    ("darkburn-bg"       . "#3F3F3F")
    ("darkburn-bg+1"     . "#4F4F4F")
    ("darkburn-bg+2"     . "#5F5F5F")
    ("darkburn-bg+3"     . "#6F6F6F")
    ("darkburn-red+1"    . "#DCA3A3")
    ("darkburn-red"      . "#CC9393")
    ("darkburn-red-1"    . "#BC8383")
    ("darkburn-red-2"    . "#AC7373")
    ("darkburn-red-3"    . "#9C6363")
    ("darkburn-red-4"    . "#8C5353")
    ("darkburn-orange"   . "#DFAF8F")
    ("darkburn-yellow"   . "#F0DFAF")
    ("darkburn-yellow-1" . "#E0CF9F")
    ("darkburn-yellow-2" . "#D0BF8F")
    ("darkburn-green-2"  . "#1F3F1F")       ;; DB
    ("darkburn-green-1"  . "#5F7F5F")
    ("darkburn-green"    . "#7F9F7F")
    ("darkburn-green+1"  . "#8FB28F")
    ("darkburn-green+2"  . "#9FC59F")
    ("darkburn-green+3"  . "#AFD8AF")
    ("darkburn-green+4"  . "#BFEBBF")
    ("darkburn-cyan"     . "#93E0E3")
    ("darkburn-blue+1"   . "#94BFF3")
    ("darkburn-blue"     . "#8CD0D3")
    ("darkburn-blue-1"   . "#7CB8BB")
    ("darkburn-blue-2"   . "#6CA0A3")
    ("darkburn-blue-3"   . "#5C888B")
    ("darkburn-blue-4"   . "#4C7073")
    ("darkburn-blue-5"   . "#366060")
    ("darkburn-magenta"  . "#DC8CC3"))
  "List of Darkburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro darkburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `darkburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   darkburn-colors-alist))
     ,@body))

;;; Theme Faces
(darkburn-with-color-variables
  (custom-theme-set-faces
   'darkburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,darkburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,darkburn-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,darkburn-fg :background ,darkburn-bg-15))))              ;; DB
   `(cursor ((t (:foreground ,darkburn-fg :background ,darkburn-fg+1))))
   `(escape-glyph ((t (:foreground ,darkburn-yellow :bold t))))
   `(fringe ((t (:foreground ,darkburn-fg :background ,darkburn-bg+1))))
   `(header-line ((t (:foreground ,darkburn-yellow
                                  :background ,darkburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,darkburn-bg-05))))
   `(success ((t (:foreground ,darkburn-green :weight bold))))
   `(warning ((t (:foreground ,darkburn-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,darkburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,darkburn-green))))
   `(compilation-error-face ((t (:foreground ,darkburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,darkburn-fg))))
   `(compilation-info-face ((t (:foreground ,darkburn-blue))))
   `(compilation-info ((t (:foreground ,darkburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,darkburn-green))))
   `(compilation-line-face ((t (:foreground ,darkburn-yellow))))
   `(compilation-line-number ((t (:foreground ,darkburn-yellow))))
   `(compilation-message-face ((t (:foreground ,darkburn-blue))))
   `(compilation-warning-face ((t (:foreground ,darkburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,darkburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,darkburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,darkburn-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,darkburn-fg))))
   `(grep-error-face ((t (:foreground ,darkburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,darkburn-blue))))
   `(grep-match-face ((t (:foreground ,darkburn-orange :weight bold))))
   `(match ((t (:background ,darkburn-bg-1 :foreground ,darkburn-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,darkburn-yellow-2 :weight bold :background ,darkburn-bg+2))))
   `(isearch-fail ((t (:foreground ,darkburn-fg :background ,darkburn-red-4))))
   `(lazy-highlight ((t (:foreground ,darkburn-yellow-2 :weight bold :background ,darkburn-bg-05))))

   `(menu ((t (:foreground ,darkburn-fg :background ,darkburn-bg-15))))    ;; DB
   `(minibuffer-prompt ((t (:foreground ,darkburn-yellow))))
   `(mode-line
     ((,class (:foreground ,darkburn-green+1
                           :background ,darkburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,darkburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,darkburn-green-1
                      :background ,darkburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,darkburn-green-2))                           ;; DB
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,darkburn-bg+2))))
   `(trailing-whitespace ((t (:background ,darkburn-red))))
   `(vertical-border ((t (:foreground ,darkburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,darkburn-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,darkburn-green :italic t))))       ;; DB
   `(font-lock-comment-delimiter-face ((t (:foreground ,darkburn-green-1))))
   `(font-lock-constant-face ((t (:foreground ,darkburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,darkburn-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,darkburn-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,darkburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,darkburn-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,darkburn-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,darkburn-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,darkburn-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,darkburn-red))))
   `(font-lock-type-face ((t (:foreground ,darkburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,darkburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,darkburn-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,darkburn-fg))))
   `(newsticker-default-face ((t (:foreground ,darkburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,darkburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,darkburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,darkburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,darkburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,darkburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,darkburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,darkburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,darkburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,darkburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,darkburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,darkburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,darkburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,darkburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,darkburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,darkburn-bg-1 :foreground ,darkburn-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,darkburn-fg-1 :background ,darkburn-bg-15 :inverse-video nil))))      ;; DB
   `(ace-jump-face-foreground
     ((t (:foreground ,darkburn-green+2 :background ,darkburn-bg-15 :inverse-video nil))))   ;; DB
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,darkburn-cyan :weight bold))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,darkburn-fg))))
   `(ack-file ((t (:foreground ,darkburn-blue))))
   `(ack-line ((t (:foreground ,darkburn-yellow))))
   `(ack-match ((t (:foreground ,darkburn-orange :background ,darkburn-bg-1 :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,darkburn-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,darkburn-yellow))))
   `(font-latex-italic-face ((t (:foreground ,darkburn-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,darkburn-orange))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,darkburn-bg+3 :foreground ,darkburn-bg-2))))
   `(ac-selection-face ((t (:background ,darkburn-blue-4 :foreground ,darkburn-fg))))
   `(popup-tip-face ((t (:background ,darkburn-yellow-2 :foreground ,darkburn-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,darkburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,darkburn-bg-1))))
   `(popup-isearch-match ((t (:background ,darkburn-bg-15 :foreground ,darkburn-fg))))     ;; DB
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,darkburn-green+1))))
   `(android-mode-error-face ((t (:foreground ,darkburn-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,darkburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,darkburn-green))))
   `(android-mode-warning-face ((t (:foreground ,darkburn-yellow))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,darkburn-yellow :background ,darkburn-bg+1))))
   `(company-tooltip-selection ((t (:foreground ,darkburn-fg :background ,darkburn-bg-1))))
   `(company-tooltip-mouse ((t (:background ,darkburn-bg-1))))
   `(company-tooltip-common ((t (:foreground ,darkburn-yellow :background ,darkburn-bg-1))))
   `(company-tooltip-common-selection ((t (:background ,darkburn-bg-1))))
   `(company-scrollbar-fg ((t (:background ,darkburn-green+1))))
   `(company-scrollbar-bg ((t (:background ,darkburn-bg-1))))
   `(company-preview ((t (:background ,darkburn-green+1))))
   `(company-preview-common ((t (:background ,darkburn-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,darkburn-yellow-1 :foreground ,darkburn-bg))))
   `(bm-fringe-face ((t (:background ,darkburn-yellow-1 :foreground ,darkburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,darkburn-green-1 :foreground ,darkburn-bg))))
   `(bm-persistent-face ((t (:background ,darkburn-green-1 :foreground ,darkburn-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,darkburn-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,darkburn-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,darkburn-green+1 :weight bold :underline t))))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,darkburn-blue :foreground ,darkburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,darkburn-bg-05 :foreground ,darkburn-bg))))
   `(ctbl:face-row-select ((t (:background ,darkburn-cyan :foreground ,darkburn-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,darkburn-green+4 :background nil))
                 (t (:foreground ,darkburn-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,darkburn-yellow))))
   `(diff-removed ((,class (:foreground ,darkburn-red :background nil))
                   (t (:foreground ,darkburn-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,darkburn-bg+2))
                  (t (:background ,darkburn-fg :foreground ,darkburn-bg))))
   `(diff-file-header
     ((,class (:background ,darkburn-bg+2 :foreground ,darkburn-fg :bold t))
      (t (:background ,darkburn-fg :foreground ,darkburn-bg :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,darkburn-blue-2 :background ,darkburn-bg-05))))
   `(diff-hl-delete ((,class (:foreground ,darkburn-red+1 :background ,darkburn-bg-05))))
   `(diff-hl-insert ((,class (:foreground ,darkburn-green+1 :background ,darkburn-bg-05))))
   `(diff-hl-unknown ((,class (:foreground ,darkburn-yellow :background ,darkburn-bg-05))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,darkburn-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,darkburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,darkburn-orange))))
   `(diredp-date-time ((t (:foreground ,darkburn-magenta))))
   `(diredp-deletion ((t (:foreground ,darkburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,darkburn-red))))
   `(diredp-dir-heading ((t (:foreground ,darkburn-blue :background ,darkburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,darkburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,darkburn-red))))
   `(diredp-executable-tag ((t (:foreground ,darkburn-green+1))))
   `(diredp-file-name ((t (:foreground ,darkburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,darkburn-green))))
   `(diredp-flag-mark ((t (:foreground ,darkburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,darkburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,darkburn-red))))
   `(diredp-link-priv ((t (:foreground ,darkburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,darkburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,darkburn-orange))))
   `(diredp-no-priv ((t (:foreground ,darkburn-fg))))
   `(diredp-number ((t (:foreground ,darkburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,darkburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,darkburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,darkburn-green-1))))
   `(diredp-symlink ((t (:foreground ,darkburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,darkburn-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,darkburn-fg :background ,darkburn-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,darkburn-fg :background ,darkburn-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,darkburn-fg :background ,darkburn-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,darkburn-fg :background ,darkburn-blue-5))))
   `(ediff-even-diff-A ((t (:background ,darkburn-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,darkburn-bg+1))))
   `(ediff-even-diff-B ((t (:background ,darkburn-bg+1))))
   `(ediff-even-diff-C ((t (:background ,darkburn-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,darkburn-fg :background ,darkburn-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,darkburn-fg :background ,darkburn-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,darkburn-fg :background ,darkburn-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,darkburn-fg :background ,darkburn-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,darkburn-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,darkburn-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,darkburn-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,darkburn-bg+2))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,darkburn-green+4 :background ,darkburn-bg-15))))       ;; DB
   `(ert-test-result-unexpected ((t (:foreground ,darkburn-red :background ,darkburn-bg-15))))         ;; DB
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,darkburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,darkburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,darkburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,darkburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,darkburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,darkburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,darkburn-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,darkburn-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-red-1) :inherit unspecified))
      (t (:foreground ,darkburn-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-yellow) :inherit unspecified))
      (t (:foreground ,darkburn-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-cyan) :inherit unspecified))
      (t (:foreground ,darkburn-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,darkburn-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,darkburn-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,darkburn-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,darkburn-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,darkburn-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,darkburn-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-orange) :inherit unspecified))
      (t (:foreground ,darkburn-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-red) :inherit unspecified))
      (t (:foreground ,darkburn-red-1 :weight bold :underline t))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,darkburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,darkburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,darkburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,darkburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,darkburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,darkburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,darkburn-green))))
   `(erc-pal-face ((t (:foreground ,darkburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,darkburn-orange :background ,darkburn-bg-15 :weight bold))))  ;; DB
   `(erc-timestamp-face ((t (:foreground ,darkburn-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,darkburn-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,darkburn-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,darkburn-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,darkburn-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,darkburn-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,darkburn-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,darkburn-magenta :weight bold))))
;;;;; git-rebase-mode
   `(git-rebase-hash ((t (:foreground, darkburn-orange))))
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
   `(gnus-summary-cancelled ((t (:foreground ,darkburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,darkburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,darkburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,darkburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,darkburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,darkburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,darkburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,darkburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,darkburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,darkburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,darkburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,darkburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,darkburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,darkburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,darkburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,darkburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,darkburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,darkburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,darkburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,darkburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,darkburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,darkburn-green))))
   `(gnus-cite-7 ((t (:foreground ,darkburn-red))))
   `(gnus-cite-8 ((t (:foreground ,darkburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,darkburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,darkburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,darkburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,darkburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,darkburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,darkburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,darkburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,darkburn-bg+2))))
   `(gnus-signature ((t (:foreground ,darkburn-yellow))))
   `(gnus-x ((t (:background ,darkburn-fg :foreground ,darkburn-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,darkburn-blue))))
   `(guide-key/key-face ((t (:foreground ,darkburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,darkburn-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,darkburn-green
                      :background ,darkburn-bg-15           ;; DB
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,darkburn-yellow
                      :background ,darkburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,darkburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,darkburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,darkburn-bg :background ,darkburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,darkburn-green+4 :background ,darkburn-bg-1))))
   `(helm-separator ((t (:foreground ,darkburn-red :background ,darkburn-bg-15))))                    ;; DB
   `(helm-time-zone-current ((t (:foreground ,darkburn-green+2 :background ,darkburn-bg-15))))        ;; DB
   `(helm-time-zone-home ((t (:foreground ,darkburn-red :background ,darkburn-bg-15))))               ;; DB
   `(helm-bookmark-addressbook ((t (:foreground ,darkburn-orange :background ,darkburn-bg-15))))      ;; DB
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,darkburn-magenta :background ,darkburn-bg-15))))            ;; DB
   `(helm-bookmark-info ((t (:foreground ,darkburn-green+2 :background ,darkburn-bg-15))))            ;; DB
   `(helm-bookmark-man ((t (:foreground ,darkburn-yellow :background ,darkburn-bg-15))))              ;; DB
   `(helm-bookmark-w3m ((t (:foreground ,darkburn-magenta :background ,darkburn-bg-15))))             ;; DB
   `(helm-buffer-not-saved ((t (:foreground ,darkburn-red :background ,darkburn-bg-15))))             ;; DB
   `(helm-buffer-process ((t (:foreground ,darkburn-cyan :background ,darkburn-bg-15))))              ;; DB
   `(helm-buffer-saved-out ((t (:foreground ,darkburn-fg :background ,darkburn-bg-15))))              ;; DB
   `(helm-buffer-size ((t (:foreground ,darkburn-fg-1 :background ,darkburn-bg-15))))                 ;; DB
   `(helm-ff-directory ((t (:foreground ,darkburn-cyan :background ,darkburn-bg-15 :weight bold))))   ;; DB
   `(helm-ff-file ((t (:foreground ,darkburn-fg :background ,darkburn-bg-15 :weight normal))))        ;; DB
   `(helm-ff-executable ((t (:foreground ,darkburn-green+2 :background ,darkburn-bg-15 :weight normal)))) ;; DB
   `(helm-ff-invalid-symlink ((t (:foreground ,darkburn-red :background ,darkburn-bg-15 :weight bold))))  ;; DB
   `(helm-ff-symlink ((t (:foreground ,darkburn-yellow :background ,darkburn-bg-15 :weight bold))))       ;; DB
   `(helm-ff-prefix ((t (:foreground ,darkburn-bg :background ,darkburn-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,darkburn-cyan :background ,darkburn-bg-15))))               ;; DB
   `(helm-grep-file ((t (:foreground ,darkburn-fg :background ,darkburn-bg-15))))                     ;; DB
   `(helm-grep-finish ((t (:foreground ,darkburn-green+2 :background ,darkburn-bg-15))))              ;; DB
   `(helm-grep-lineno ((t (:foreground ,darkburn-fg-1 :background ,darkburn-bg-15))))                 ;; DB
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,darkburn-red :background ,darkburn-bg-15))))                 ;; DB
   `(helm-moccur-buffer ((t (:foreground ,darkburn-cyan :background ,darkburn-bg-15))))               ;; DB
   `(helm-mu-contacts-address-face ((t (:foreground ,darkburn-fg-1 :background ,darkburn-bg-15))))    ;; DB
   `(helm-mu-contacts-name-face ((t (:foreground ,darkburn-fg :background ,darkburn-bg-15))))         ;; DB
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,darkburn-bg-1))                                              ;; DB
                   (t :weight bold)))
   `(hl-line ((,class (:background ,darkburn-bg-1)) ; old emacsen                                     ;; DB
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,darkburn-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,darkburn-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,darkburn-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,darkburn-yellow))))
   `(ido-indicator ((t (:foreground ,darkburn-yellow :background ,darkburn-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,darkburn-bg+2 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,darkburn-orange))))
   `(js2-error ((t (:foreground ,darkburn-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,darkburn-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,darkburn-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,darkburn-green+3))))
   `(js2-function-param ((t (:foreground, darkburn-green+3))))
   `(js2-external-variable ((t (:foreground ,darkburn-orange))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,darkburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,darkburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,darkburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,darkburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,darkburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,darkburn-red+1))))
   `(jabber-activity-face((t (:foreground ,darkburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,darkburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,darkburn-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,darkburn-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,darkburn-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,darkburn-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,darkburn-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,darkburn-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,darkburn-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,darkburn-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,darkburn-orange))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,darkburn-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,darkburn-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,darkburn-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,darkburn-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,darkburn-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,darkburn-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,darkburn-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,darkburn-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,darkburn-green+2 :background ,darkburn-bg-15))))          ;; DB
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,darkburn-green+2 :background ,darkburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,darkburn-red+1 :background ,darkburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,darkburn-blue+1 :background ,darkburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,darkburn-magenta :background ,darkburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,darkburn-yellow :background ,darkburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-item-highlight ((t (:background ,darkburn-bg+1))))
   `(magit-section-title ((t (:foreground ,darkburn-yellow :weight bold))))
   `(magit-process-ok ((t (:foreground ,darkburn-green :weight bold))))
   `(magit-process-ng ((t (:foreground ,darkburn-red :weight bold))))
   `(magit-branch ((t (:foreground ,darkburn-blue :weight bold))))
   `(magit-log-author ((t (:foreground, darkburn-orange))))
   `(magit-log-sha1 ((t (:foreground, darkburn-orange))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,darkburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,darkburn-yellow))))
   `(egg-help-header-2 ((t (:foreground ,darkburn-green+3))))
   `(egg-branch ((t (:foreground ,darkburn-yellow))))
   `(egg-branch-mono ((t (:foreground ,darkburn-yellow))))
   `(egg-term ((t (:foreground ,darkburn-yellow))))
   `(egg-diff-add ((t (:foreground ,darkburn-green+4))))
   `(egg-diff-del ((t (:foreground ,darkburn-red+1))))
   `(egg-diff-file-header ((t (:foreground ,darkburn-yellow-2))))
   `(egg-section-title ((t (:foreground ,darkburn-yellow))))
   `(egg-stash-mono ((t (:foreground ,darkburn-green+4))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,darkburn-green+1))))
   `(message-header-other ((t (:foreground ,darkburn-green))))
   `(message-header-to ((t (:foreground ,darkburn-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,darkburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,darkburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,darkburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,darkburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,darkburn-green))))
   `(message-mml ((t (:foreground ,darkburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,darkburn-orange))))
   `(mew-face-header-from ((t (:foreground ,darkburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,darkburn-green))))
   `(mew-face-header-to ((t (:foreground ,darkburn-red))))
   `(mew-face-header-key ((t (:foreground ,darkburn-green))))
   `(mew-face-header-private ((t (:foreground ,darkburn-green))))
   `(mew-face-header-important ((t (:foreground ,darkburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,darkburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,darkburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,darkburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,darkburn-red))))
   `(mew-face-body-url ((t (:foreground ,darkburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,darkburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,darkburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,darkburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,darkburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,darkburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,darkburn-red))))
   `(mew-face-mark-review ((t (:foreground ,darkburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,darkburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,darkburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,darkburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,darkburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,darkburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,darkburn-green))))
   `(mew-face-eof-part ((t (:foreground ,darkburn-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,darkburn-cyan :background ,darkburn-bg-15 :weight bold))))      ;; DB
   `(paren-face-mismatch ((t (:foreground ,darkburn-bg :background ,darkburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,darkburn-bg :background ,darkburn-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,darkburn-blue))))
   `(mingus-pausing-face ((t (:foreground ,darkburn-magenta))))
   `(mingus-playing-face ((t (:foreground ,darkburn-cyan))))
   `(mingus-playlist-face ((t (:foreground ,darkburn-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,darkburn-yellow))))
   `(mingus-stopped-face ((t (:foreground ,darkburn-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,darkburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,darkburn-cyan))))
   `(nav-face-dir ((t (:foreground ,darkburn-green))))
   `(nav-face-hdir ((t (:foreground ,darkburn-red))))
   `(nav-face-file ((t (:foreground ,darkburn-fg))))
   `(nav-face-hfile ((t (:foreground ,darkburn-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,darkburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,darkburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,darkburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,darkburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,darkburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,darkburn-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,darkburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,darkburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,darkburn-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,darkburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,darkburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,darkburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,darkburn-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,darkburn-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,darkburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,darkburn-bg+2 :foreground ,darkburn-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,darkburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,darkburn-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,darkburn-green+3))))
   `(org-formula ((t (:foreground ,darkburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,darkburn-green+3))))
   `(org-hide ((t (:foreground ,darkburn-bg-1))))
   `(org-level-1 ((t (:foreground ,darkburn-orange))))
   `(org-level-2 ((t (:foreground ,darkburn-green+4))))
   `(org-level-3 ((t (:foreground ,darkburn-blue-1))))
   `(org-level-4 ((t (:foreground ,darkburn-yellow-2))))
   `(org-level-5 ((t (:foreground ,darkburn-cyan))))
   `(org-level-6 ((t (:foreground ,darkburn-green+2))))
   `(org-level-7 ((t (:foreground ,darkburn-red-4))))
   `(org-level-8 ((t (:foreground ,darkburn-blue-4))))
   `(org-link ((t (:foreground ,darkburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,darkburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,darkburn-red))))
   `(org-scheduled-today ((t (:foreground ,darkburn-blue+1))))
   `(org-sexp-date ((t (:foreground ,darkburn-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,darkburn-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,darkburn-orange))))
   `(org-todo ((t (:bold t :foreground ,darkburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,darkburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,darkburn-bg-1))))
   `(org-column-title ((t (:background ,darkburn-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,darkburn-fg :background ,darkburn-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,darkburn-bg :background ,darkburn-red-1))))
   `(org-ellipsis ((t (:foreground ,darkburn-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,darkburn-cyan :underline t))))
;;;;; outline
   `(outline-1 ((t (:foreground ,darkburn-orange))))
   `(outline-2 ((t (:foreground ,darkburn-green+4))))
   `(outline-3 ((t (:foreground ,darkburn-blue-1))))
   `(outline-4 ((t (:foreground ,darkburn-yellow-2))))
   `(outline-5 ((t (:foreground ,darkburn-cyan))))
   `(outline-6 ((t (:foreground ,darkburn-green+2))))
   `(outline-7 ((t (:foreground ,darkburn-red-4))))
   `(outline-8 ((t (:foreground ,darkburn-blue-4))))
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
   `(persp-selected-face ((t (:foreground ,darkburn-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,darkburn-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,darkburn-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,darkburn-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,darkburn-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,darkburn-fg :background ,darkburn-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,darkburn-bg :background ,darkburn-orange))))
   `(proof-error-face ((t (:foreground ,darkburn-fg :background ,darkburn-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,darkburn-bg :background ,darkburn-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,darkburn-bg :background ,darkburn-orange))))
   `(proof-locked-face ((t (:background ,darkburn-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,darkburn-bg :background ,darkburn-orange))))
   `(proof-queue-face ((t (:background ,darkburn-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,darkburn-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,darkburn-bg-15))))   ;; DB
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,darkburn-bg-15))))     ;; DB
   `(proof-warning-face ((t (:foreground ,darkburn-bg :background ,darkburn-yellow-1))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,darkburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,darkburn-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,darkburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,darkburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,darkburn-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,darkburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,darkburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,darkburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,darkburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,darkburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,darkburn-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,darkburn-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,darkburn-blue))))
   `(rcirc-other-nick ((t (:foreground ,darkburn-orange))))
   `(rcirc-bright-nick ((t (:foreground ,darkburn-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,darkburn-blue-2))))
   `(rcirc-server ((t (:foreground ,darkburn-green))))
   `(rcirc-server-prefix ((t (:foreground ,darkburn-green+1))))
   `(rcirc-timestamp ((t (:foreground ,darkburn-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,darkburn-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,darkburn-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,darkburn-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,darkburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,darkburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,darkburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,darkburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,darkburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,darkburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,darkburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,darkburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,darkburn-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,darkburn-orange))))
   `(rst-level-2-face ((t (:foreground ,darkburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,darkburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,darkburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,darkburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,darkburn-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,darkburn-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,darkburn-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,darkburn-red+1 :background ,darkburn-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,darkburn-bg+3 :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,darkburn-red+1 :background ,darkburn-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,darkburn-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,darkburn-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,darkburn-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-red)))
      (t
       (:underline ,darkburn-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-orange)))
      (t
       (:underline ,darkburn-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-yellow)))
      (t
       (:underline ,darkburn-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,darkburn-green)))
      (t
       (:underline ,darkburn-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,darkburn-green+2))))
   `(speedbar-directory-face ((t (:foreground ,darkburn-cyan))))
   `(speedbar-file-face ((t (:foreground ,darkburn-fg))))
   `(speedbar-highlight-face ((t (:foreground ,darkburn-bg :background ,darkburn-green+2))))
   `(speedbar-selected-face ((t (:foreground ,darkburn-red))))
   `(speedbar-separator-face ((t (:foreground ,darkburn-bg :background ,darkburn-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,darkburn-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,darkburn-fg
                                    :background ,darkburn-bg-15))))     ;; DB
   `(tabbar-selected ((t (:foreground ,darkburn-fg
                                      :background ,darkburn-bg-15       ;; DB
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,darkburn-fg
                                        :background ,darkburn-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,darkburn-bg
                                       :background ,darkburn-bg-1))))
   `(term-color-red ((t (:foreground ,darkburn-red-2
                                       :background ,darkburn-red-4))))
   `(term-color-green ((t (:foreground ,darkburn-green
                                       :background ,darkburn-green+2))))
   `(term-color-yellow ((t (:foreground ,darkburn-orange
                                       :background ,darkburn-yellow))))
   `(term-color-blue ((t (:foreground ,darkburn-blue-1
                                      :background ,darkburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,darkburn-magenta
                                         :background ,darkburn-red))))
   `(term-color-cyan ((t (:foreground ,darkburn-cyan
                                       :background ,darkburn-blue))))
   `(term-color-white ((t (:foreground ,darkburn-fg
                                       :background ,darkburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,darkburn-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,darkburn-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,darkburn-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,darkburn-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,darkburn-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,darkburn-bg-05))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,darkburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,darkburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,darkburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,darkburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,darkburn-green+2 :background ,darkburn-bg-15))))   ;; DB
   `(w3m-lnum-match ((t (:background ,darkburn-bg-1
                                     :foreground ,darkburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,darkburn-yellow))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,darkburn-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,darkburn-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,darkburn-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,darkburn-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,darkburn-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,darkburn-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,darkburn-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,darkburn-bg-15))))         ;; DB
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,darkburn-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,darkburn-bg+1 :foreground ,darkburn-bg+1))))
   `(whitespace-hspace ((t (:background ,darkburn-bg+1 :foreground ,darkburn-bg+1))))
   `(whitespace-tab ((t (:background ,darkburn-red-1))))
   `(whitespace-newline ((t (:foreground ,darkburn-bg+1))))
   `(whitespace-trailing ((t (:background ,darkburn-red))))
   `(whitespace-line ((t (:background ,darkburn-bg-15 :foreground ,darkburn-magenta))))              ;; DB
   `(whitespace-space-before-tab ((t (:background ,darkburn-orange :foreground ,darkburn-orange))))
   `(whitespace-indentation ((t (:background ,darkburn-yellow :foreground ,darkburn-red))))
   `(whitespace-empty ((t (:background ,darkburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,darkburn-yellow :foreground ,darkburn-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,darkburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,darkburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,darkburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,darkburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,darkburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,darkburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,darkburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,darkburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,darkburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,darkburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,darkburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,darkburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,darkburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,darkburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,darkburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,darkburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,darkburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,darkburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,darkburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,darkburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,darkburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,darkburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,darkburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,darkburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,darkburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,darkburn-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,darkburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,darkburn-bg-1 :foreground ,darkburn-bg-1))))
   ))

;;; Theme Variables
(darkburn-with-color-variables
  (custom-theme-set-variables
   'darkburn
;;;;; ansi-color
   `(ansi-color-names-vector [,darkburn-bg ,darkburn-red ,darkburn-green ,darkburn-yellow
                                          ,darkburn-blue ,darkburn-magenta ,darkburn-cyan ,darkburn-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,darkburn-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,darkburn-red-1)
       ( 40. . ,darkburn-red)
       ( 60. . ,darkburn-orange)
       ( 80. . ,darkburn-yellow-2)
       (100. . ,darkburn-yellow-1)
       (120. . ,darkburn-yellow)
       (140. . ,darkburn-green-1)
       (160. . ,darkburn-green)
       (180. . ,darkburn-green+1)
       (200. . ,darkburn-green+2)
       (220. . ,darkburn-green+3)
       (240. . ,darkburn-green+4)
       (260. . ,darkburn-cyan)
       (280. . ,darkburn-blue-2)
       (300. . ,darkburn-blue-1)
       (320. . ,darkburn-blue)
       (340. . ,darkburn-blue+1)
       (360. . ,darkburn-magenta)))
   `(vc-annotate-very-old-color ,darkburn-magenta)
   `(vc-annotate-background ,darkburn-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar darkburn-add-font-lock-keywords nil
  "Whether to add font-lock keywords for darkburn color names.
In buffers visiting library `darkburn-theme.el' the darkburn
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar darkburn-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after darkburn activate)
;;   "Maybe also add font-lock keywords for darkburn colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or darkburn-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "darkburn-theme.el")))
;;     (unless darkburn-colors-font-lock-keywords
;;       (setq darkburn-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car darkburn-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc darkburn-colors-alist))))))
;;     (font-lock-add-keywords nil darkburn-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after darkburn activate)
;;   "Also remove font-lock keywords for darkburn colors."
;;   (font-lock-remove-keywords nil darkburn-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'darkburn)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; darkburn-theme.el ends here
