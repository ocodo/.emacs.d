;;; hc-zenburn-theme.el --- An higher contrast version of the Zenburn theme.

;; Copyright (C)2014 Nantas Nardelli

;; Author: Nantas Nardelli <nantas.nardelli@gmail.com>
;; URL: https:github.com/edran/hc-zenburn-emacs
;; Package-Version: 20150928.933
;; Version 2.1

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

;; An higher contrast version of the Zenburn theme

;;; Credits:

;; Bozhidar Batsov <bozhidar@batsov.com> created the Zenburn theme
;; for emacs, which was a port of the vim theme made by Jani Nurminen.
;; His repository can be found at: https://github.com/bbatsov/zenburn-emacs


;;; Code:

(deftheme hc-zenburn "An higher constrast Zenburn color theme")

;;; Color Palette

(defvar hc-zenburn-colors-alist
  '(("hc-zenburn-fg+1"     . "#FFFFEF")
    ("hc-zenburn-fg"       . "#DCDCCC")
    ("hc-zenburn-fg-1"     . "#70705E")
    ("hc-zenburn-bg-2"     . "#000000")
    ("hc-zenburn-bg-1"     . "#202020")
    ("hc-zenburn-bg-05"    . "#2D2D2D")
    ("hc-zenburn-bg"       . "#313131")
    ("hc-zenburn-bg+05"    . "#383838")
    ("hc-zenburn-bg+1"     . "#3E3E3E")
    ("hc-zenburn-bg+2"     . "#4E4E4E")
    ("hc-zenburn-bg+3"     . "#5E5E5E")
    ("hc-zenburn-red+1"    . "#E9B0B0")
    ("hc-zenburn-red"      . "#D9A0A0")
    ("hc-zenburn-red-1"    . "#C99090")
    ("hc-zenburn-red-2"    . "#B98080")
    ("hc-zenburn-red-3"    . "#A97070")
    ("hc-zenburn-red-4"    . "#996060")
    ("hc-zenburn-orange"   . "#ECBC9C")
    ("hc-zenburn-yellow"   . "#FDECBC")
    ("hc-zenburn-yellow-1" . "#EDDCAC")
    ("hc-zenburn-yellow-2" . "#DDCC9C")
    ("hc-zenburn-green-1"  . "#6C8C6C")
    ("hc-zenburn-green"    . "#8CAC8C")
    ("hc-zenburn-green+1"  . "#9CBF9C")
    ("hc-zenburn-green+2"  . "#ACD2AC")
    ("hc-zenburn-green+3"  . "#BCE5BC")
    ("hc-zenburn-green+4"  . "#CCF8CC")
    ("hc-zenburn-cyan"     . "#A0EDF0")
    ("hc-zenburn-blue+1"   . "#9CC7FB")
    ("hc-zenburn-blue"     . "#99DDE0")
    ("hc-zenburn-blue-1"   . "#89C5C8")
    ("hc-zenburn-blue-2"   . "#79ADB0")
    ("hc-zenburn-blue-3"   . "#699598")
    ("hc-zenburn-blue-4"   . "#597D80")
    ("hc-zenburn-blue-5"   . "#436D6D")
    ("hc-zenburn-magenta"  . "#E090C7"))
  "List of Hc-Zenburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro hc-zenburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `hc-zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   hc-zenburn-colors-alist))
     ,@body))

;;; Theme Faces
(hc-zenburn-with-color-variables
  (custom-theme-set-faces
   'hc-zenburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(default ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg))))
   `(link ((t (:foreground ,hc-zenburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,hc-zenburn-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg))))
   `(cursor ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-fg+1))))
   `(escape-glyph ((t (:foreground ,hc-zenburn-yellow :bold t))))
   `(fringe ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg+1))))
   `(header-line ((t (:foreground ,hc-zenburn-yellow
                                  :background ,hc-zenburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,hc-zenburn-bg-05))))
   `(success ((t (:foreground ,hc-zenburn-green :weight bold))))
   `(warning ((t (:foreground ,hc-zenburn-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,hc-zenburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,hc-zenburn-green))))
   `(compilation-error-face ((t (:foreground ,hc-zenburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,hc-zenburn-fg))))
   `(compilation-info-face ((t (:foreground ,hc-zenburn-blue))))
   `(compilation-info ((t (:foreground ,hc-zenburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,hc-zenburn-green))))
   `(compilation-line-face ((t (:foreground ,hc-zenburn-yellow))))
   `(compilation-line-number ((t (:foreground ,hc-zenburn-yellow))))
   `(compilation-message-face ((t (:foreground ,hc-zenburn-blue))))
   `(compilation-warning-face ((t (:foreground ,hc-zenburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,hc-zenburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,hc-zenburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,hc-zenburn-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,hc-zenburn-fg))))
   `(grep-error-face ((t (:foreground ,hc-zenburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,hc-zenburn-blue))))
   `(grep-match-face ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(match ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-orange :weight bold))))
;;;;; neotree
   `(neo-file-link-face ((t (:background ,hc-zenburn-bg :foreground ,hc-zenburn-fg))))
   `(neo-root-dir-face ((t (:foreground ,hc-zenburn-blue :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,hc-zenburn-green+1))))
;;;;; isearch
   `(isearch ((t (:foreground ,hc-zenburn-yellow-2 :weight bold :background ,hc-zenburn-bg+2))))
   `(isearch-fail ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-red-4))))
   `(lazy-highlight ((t (:foreground ,hc-zenburn-yellow-2 :weight bold :background ,hc-zenburn-bg-05))))

   `(menu ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,hc-zenburn-yellow))))
   `(mode-line
     ((,class (:foreground ,hc-zenburn-green+1
                           :background ,hc-zenburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,hc-zenburn-green-1
                      :background ,hc-zenburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,hc-zenburn-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,hc-zenburn-bg+2))))
   `(trailing-whitespace ((t (:background ,hc-zenburn-red))))
   `(vertical-border ((t (:foreground ,hc-zenburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,hc-zenburn-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,hc-zenburn-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,hc-zenburn-green-1))))
   `(font-lock-constant-face ((t (:foreground ,hc-zenburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,hc-zenburn-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,hc-zenburn-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,hc-zenburn-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,hc-zenburn-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,hc-zenburn-red))))
   `(font-lock-type-face ((t (:foreground ,hc-zenburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,hc-zenburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,hc-zenburn-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-default-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,hc-zenburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,hc-zenburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,hc-zenburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,hc-zenburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,hc-zenburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,hc-zenburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,hc-zenburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,hc-zenburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,hc-zenburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,hc-zenburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,hc-zenburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,hc-zenburn-fg-1 :background ,hc-zenburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg :inverse-video nil))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,hc-zenburn-green+1))))
   `(android-mode-error-face ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,hc-zenburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,hc-zenburn-green))))
   `(android-mode-warning-face ((t (:foreground ,hc-zenburn-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,hc-zenburn-cyan :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,hc-zenburn-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,hc-zenburn-yellow))))
   `(font-latex-italic-face ((t (:foreground ,hc-zenburn-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,hc-zenburn-orange))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,hc-zenburn-bg+3 :foreground ,hc-zenburn-bg-2))))
   `(ac-selection-face ((t (:background ,hc-zenburn-blue-4 :foreground ,hc-zenburn-fg))))
   `(popup-tip-face ((t (:background ,hc-zenburn-yellow-2 :foreground ,hc-zenburn-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,hc-zenburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,hc-zenburn-bg-1))))
   `(popup-isearch-match ((t (:background ,hc-zenburn-bg :foreground ,hc-zenburn-fg))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,hc-zenburn-orange :background ,hc-zenburn-bg+1))))
   `(company-tooltip-selection ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg-1))))
   `(company-tooltip-mouse ((t (:background ,hc-zenburn-bg-1))))
   `(company-tooltip-common ((t (:foreground ,hc-zenburn-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,hc-zenburn-green+2))))
   `(company-scrollbar-fg ((t (:background ,hc-zenburn-bg-1))))
   `(company-scrollbar-bg ((t (:background ,hc-zenburn-bg+2))))
   `(company-preview ((t (:background ,hc-zenburn-green+2))))
   `(company-preview-common ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,hc-zenburn-yellow-1 :foreground ,hc-zenburn-bg))))
   `(bm-fringe-face ((t (:background ,hc-zenburn-yellow-1 :foreground ,hc-zenburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,hc-zenburn-green-1 :foreground ,hc-zenburn-bg))))
   `(bm-persistent-face ((t (:background ,hc-zenburn-green-1 :foreground ,hc-zenburn-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,hc-zenburn-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,hc-zenburn-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,hc-zenburn-green+1 :weight bold :underline t))))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,hc-zenburn-blue :foreground ,hc-zenburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,hc-zenburn-bg-05 :foreground ,hc-zenburn-bg))))
   `(ctbl:face-row-select ((t (:background ,hc-zenburn-cyan :foreground ,hc-zenburn-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,hc-zenburn-green+4 :background nil))
                 (t (:foreground ,hc-zenburn-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,hc-zenburn-yellow))))
   `(diff-removed ((,class (:foreground ,hc-zenburn-red :background nil))
                   (t (:foreground ,hc-zenburn-red-3 :background nil))))
   `(diff-refine-added ((t (:inherit diff-added :weight bold))))
   `(diff-refine-change ((t (:inherit diff-changed :weight bold))))
   `(diff-refine-removed ((t (:inherit diff-removed :weight bold))))
   `(diff-header ((,class (:background ,hc-zenburn-bg+2))
                  (t (:background ,hc-zenburn-fg :foreground ,hc-zenburn-bg))))
   `(diff-file-header
     ((,class (:background ,hc-zenburn-bg+2 :foreground ,hc-zenburn-fg :bold t))
      (t (:background ,hc-zenburn-fg :foreground ,hc-zenburn-bg :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,hc-zenburn-blue-2 :background ,hc-zenburn-bg-05))))
   `(diff-hl-delete ((,class (:foreground ,hc-zenburn-red+1 :background ,hc-zenburn-bg-05))))
   `(diff-hl-insert ((,class (:foreground ,hc-zenburn-green+1 :background ,hc-zenburn-bg-05))))
   `(diff-hl-unknown ((,class (:foreground ,hc-zenburn-yellow :background ,hc-zenburn-bg-05))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,hc-zenburn-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,hc-zenburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,hc-zenburn-orange))))
   `(diredp-date-time ((t (:foreground ,hc-zenburn-magenta))))
   `(diredp-deletion ((t (:foreground ,hc-zenburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,hc-zenburn-red))))
   `(diredp-dir-heading ((t (:foreground ,hc-zenburn-blue :background ,hc-zenburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,hc-zenburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,hc-zenburn-red))))
   `(diredp-executable-tag ((t (:foreground ,hc-zenburn-green+1))))
   `(diredp-file-name ((t (:foreground ,hc-zenburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,hc-zenburn-green))))
   `(diredp-flag-mark ((t (:foreground ,hc-zenburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,hc-zenburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,hc-zenburn-red))))
   `(diredp-link-priv ((t (:foreground ,hc-zenburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,hc-zenburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,hc-zenburn-orange))))
   `(diredp-no-priv ((t (:foreground ,hc-zenburn-fg))))
   `(diredp-number ((t (:foreground ,hc-zenburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,hc-zenburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,hc-zenburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,hc-zenburn-green-1))))
   `(diredp-symlink ((t (:foreground ,hc-zenburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,hc-zenburn-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-blue-5))))
   `(ediff-even-diff-A ((t (:background ,hc-zenburn-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,hc-zenburn-bg+1))))
   `(ediff-even-diff-B ((t (:background ,hc-zenburn-bg+1))))
   `(ediff-even-diff-C ((t (:background ,hc-zenburn-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,hc-zenburn-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,hc-zenburn-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,hc-zenburn-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,hc-zenburn-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,hc-zenburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,hc-zenburn-yellow))))
   `(egg-help-header-2 ((t (:foreground ,hc-zenburn-green+3))))
   `(egg-branch ((t (:foreground ,hc-zenburn-yellow))))
   `(egg-branch-mono ((t (:foreground ,hc-zenburn-yellow))))
   `(egg-term ((t (:foreground ,hc-zenburn-yellow))))
   `(egg-diff-add ((t (:foreground ,hc-zenburn-green+4))))
   `(egg-diff-del ((t (:foreground ,hc-zenburn-red+1))))
   `(egg-diff-file-header ((t (:foreground ,hc-zenburn-yellow-2))))
   `(egg-section-title ((t (:foreground ,hc-zenburn-yellow))))
   `(egg-stash-mono ((t (:foreground ,hc-zenburn-green+4))))
;;;;; elfeed
   `(elfeed-search-date-face ((t (:foreground ,hc-zenburn-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,hc-zenburn-green))))
   `(elfeed-search-feed-face ((t (:foreground ,hc-zenburn-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,hc-zenburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,hc-zenburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,hc-zenburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,hc-zenburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg))))
   `(w3m-lnum-match ((t (:background ,hc-zenburn-bg-1
                                     :foreground ,hc-zenburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,hc-zenburn-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,hc-zenburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,hc-zenburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,hc-zenburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,hc-zenburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,hc-zenburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,hc-zenburn-green))))
   `(erc-pal-face ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,hc-zenburn-orange :background ,hc-zenburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,hc-zenburn-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,hc-zenburn-green+4 :background ,hc-zenburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,hc-zenburn-red :background ,hc-zenburn-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,hc-zenburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,hc-zenburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,hc-zenburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,hc-zenburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,hc-zenburn-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,hc-zenburn-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-red-1) :inherit unspecified))
      (t (:foreground ,hc-zenburn-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-yellow) :inherit unspecified))
      (t (:foreground ,hc-zenburn-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-cyan) :inherit unspecified))
      (t (:foreground ,hc-zenburn-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,hc-zenburn-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,hc-zenburn-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hc-zenburn-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hc-zenburn-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,hc-zenburn-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-orange) :inherit unspecified))
      (t (:foreground ,hc-zenburn-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-red) :inherit unspecified))
      (t (:foreground ,hc-zenburn-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,hc-zenburn-fg))))
   `(ack-file ((t (:foreground ,hc-zenburn-blue))))
   `(ack-line ((t (:foreground ,hc-zenburn-yellow))))
   `(ack-match ((t (:foreground ,hc-zenburn-orange :background ,hc-zenburn-bg-1 :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,hc-zenburn-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,hc-zenburn-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,hc-zenburn-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,hc-zenburn-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,hc-zenburn-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,hc-zenburn-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,hc-zenburn-magenta :weight bold))))
;;;;; git-rebase-mode
   `(git-rebase-hash ((t (:foreground, hc-zenburn-orange))))
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
   `(gnus-summary-cancelled ((t (:foreground ,hc-zenburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,hc-zenburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,hc-zenburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,hc-zenburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,hc-zenburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,hc-zenburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,hc-zenburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,hc-zenburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,hc-zenburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,hc-zenburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,hc-zenburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,hc-zenburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,hc-zenburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,hc-zenburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,hc-zenburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,hc-zenburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,hc-zenburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,hc-zenburn-green))))
   `(gnus-cite-7 ((t (:foreground ,hc-zenburn-red))))
   `(gnus-cite-8 ((t (:foreground ,hc-zenburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,hc-zenburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,hc-zenburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,hc-zenburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,hc-zenburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,hc-zenburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,hc-zenburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,hc-zenburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,hc-zenburn-bg+2))))
   `(gnus-signature ((t (:foreground ,hc-zenburn-yellow))))
   `(gnus-x ((t (:background ,hc-zenburn-fg :foreground ,hc-zenburn-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,hc-zenburn-blue))))
   `(guide-key/key-face ((t (:foreground ,hc-zenburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,hc-zenburn-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,hc-zenburn-green
                      :background ,hc-zenburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,hc-zenburn-yellow
                      :background ,hc-zenburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,hc-zenburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,hc-zenburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,hc-zenburn-green+4 :background ,hc-zenburn-bg-1))))
   `(helm-separator ((t (:foreground ,hc-zenburn-red :background ,hc-zenburn-bg))))
   `(helm-time-zone-current ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg))))
   `(helm-time-zone-home ((t (:foreground ,hc-zenburn-red :background ,hc-zenburn-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,hc-zenburn-orange :background ,hc-zenburn-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,hc-zenburn-magenta :background ,hc-zenburn-bg))))
   `(helm-bookmark-info ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg))))
   `(helm-bookmark-man ((t (:foreground ,hc-zenburn-yellow :background ,hc-zenburn-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,hc-zenburn-magenta :background ,hc-zenburn-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,hc-zenburn-red :background ,hc-zenburn-bg))))
   `(helm-buffer-process ((t (:foreground ,hc-zenburn-cyan :background ,hc-zenburn-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg))))
   `(helm-buffer-size ((t (:foreground ,hc-zenburn-fg-1 :background ,hc-zenburn-bg))))
   `(helm-ff-directory ((t (:foreground ,hc-zenburn-cyan :background ,hc-zenburn-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,hc-zenburn-red :background ,hc-zenburn-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,hc-zenburn-yellow :background ,hc-zenburn-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,hc-zenburn-cyan :background ,hc-zenburn-bg))))
   `(helm-grep-file ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg))))
   `(helm-grep-finish ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg))))
   `(helm-grep-lineno ((t (:foreground ,hc-zenburn-fg-1 :background ,hc-zenburn-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,hc-zenburn-red :background ,hc-zenburn-bg))))
   `(helm-moccur-buffer ((t (:foreground ,hc-zenburn-cyan :background ,hc-zenburn-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,hc-zenburn-fg-1 :background ,hc-zenburn-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,hc-zenburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,hc-zenburn-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,hc-zenburn-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,hc-zenburn-yellow))))
   `(ido-indicator ((t (:foreground ,hc-zenburn-yellow :background ,hc-zenburn-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,hc-zenburn-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,hc-zenburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,hc-zenburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,hc-zenburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,hc-zenburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,hc-zenburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,hc-zenburn-red+1))))
   `(jabber-activity-face((t (:foreground ,hc-zenburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,hc-zenburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,hc-zenburn-orange))))
   `(js2-error ((t (:foreground ,hc-zenburn-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,hc-zenburn-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,hc-zenburn-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,hc-zenburn-green+3))))
   `(js2-function-param ((t (:foreground, hc-zenburn-green+3))))
   `(js2-external-variable ((t (:foreground ,hc-zenburn-orange))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,hc-zenburn-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,hc-zenburn-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,hc-zenburn-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,hc-zenburn-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,hc-zenburn-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,hc-zenburn-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,hc-zenburn-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,hc-zenburn-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,hc-zenburn-orange))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,hc-zenburn-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,hc-zenburn-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,hc-zenburn-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,hc-zenburn-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,hc-zenburn-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,hc-zenburn-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,hc-zenburn-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,hc-zenburn-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg))))
;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,hc-zenburn-blue-1))))
   `(lui-hilight-face ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,hc-zenburn-green+2 :background ,hc-zenburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,hc-zenburn-red+1 :background ,hc-zenburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,hc-zenburn-blue+1 :background ,hc-zenburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,hc-zenburn-magenta :background ,hc-zenburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,hc-zenburn-yellow :background ,hc-zenburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,hc-zenburn-bg+05))))
   `(magit-section-heading             ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,hc-zenburn-bg+05  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,hc-zenburn-bg+05
                                            :foreground ,hc-zenburn-orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,hc-zenburn-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,hc-zenburn-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,hc-zenburn-bg+2
                                            :foreground ,hc-zenburn-orange))))
   `(magit-diff-lines-heading          ((t (:background ,hc-zenburn-orange
                                            :foreground ,hc-zenburn-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,hc-zenburn-bg+05
                                            :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,hc-zenburn-green+4))))
   `(magit-diffstat-removed ((t (:foreground ,hc-zenburn-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,hc-zenburn-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,hc-zenburn-green-1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,hc-zenburn-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,hc-zenburn-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,hc-zenburn-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,hc-zenburn-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,hc-zenburn-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,hc-zenburn-orange))))
   `(magit-log-date      ((t (:foreground ,hc-zenburn-fg-1))))
   `(magit-log-graph     ((t (:foreground ,hc-zenburn-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,hc-zenburn-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,hc-zenburn-green))))
   `(magit-sequence-part ((t (:foreground ,hc-zenburn-yellow))))
   `(magit-sequence-head ((t (:foreground ,hc-zenburn-blue))))
   `(magit-sequence-drop ((t (:foreground ,hc-zenburn-red))))
   `(magit-sequence-done ((t (:foreground ,hc-zenburn-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,hc-zenburn-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,hc-zenburn-green))))
   `(magit-bisect-skip ((t (:foreground ,hc-zenburn-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,hc-zenburn-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-blue-2))))
   `(magit-blame-hash    ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-blue-2))))
   `(magit-blame-name    ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-orange))))
   `(magit-blame-date    ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-orange))))
   `(magit-blame-summary ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,hc-zenburn-bg+3))))
   `(magit-hash           ((t (:foreground ,hc-zenburn-bg+3))))
   `(magit-tag            ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,hc-zenburn-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,hc-zenburn-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,hc-zenburn-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,hc-zenburn-blue   :weight bold))))
   `(magit-refname        ((t (:background ,hc-zenburn-bg+2 :foreground ,hc-zenburn-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,hc-zenburn-bg+2 :foreground ,hc-zenburn-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,hc-zenburn-bg+2 :foreground ,hc-zenburn-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,hc-zenburn-green))))
   `(magit-signature-bad       ((t (:foreground ,hc-zenburn-red))))
   `(magit-signature-untrusted ((t (:foreground ,hc-zenburn-yellow))))
   `(magit-cherry-unmatched    ((t (:foreground ,hc-zenburn-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,hc-zenburn-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,hc-zenburn-green))))
   `(magit-reflog-amend        ((t (:foreground ,hc-zenburn-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,hc-zenburn-green))))
   `(magit-reflog-checkout     ((t (:foreground ,hc-zenburn-blue))))
   `(magit-reflog-reset        ((t (:foreground ,hc-zenburn-red))))
   `(magit-reflog-rebase       ((t (:foreground ,hc-zenburn-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,hc-zenburn-green))))
   `(magit-reflog-remote       ((t (:foreground ,hc-zenburn-cyan))))
   `(magit-reflog-other        ((t (:foreground ,hc-zenburn-cyan))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,hc-zenburn-green+1))))
   `(message-header-other ((t (:foreground ,hc-zenburn-green))))
   `(message-header-to ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,hc-zenburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,hc-zenburn-green))))
   `(message-mml ((t (:foreground ,hc-zenburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,hc-zenburn-orange))))
   `(mew-face-header-from ((t (:foreground ,hc-zenburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,hc-zenburn-green))))
   `(mew-face-header-to ((t (:foreground ,hc-zenburn-red))))
   `(mew-face-header-key ((t (:foreground ,hc-zenburn-green))))
   `(mew-face-header-private ((t (:foreground ,hc-zenburn-green))))
   `(mew-face-header-important ((t (:foreground ,hc-zenburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,hc-zenburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,hc-zenburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,hc-zenburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,hc-zenburn-red))))
   `(mew-face-body-url ((t (:foreground ,hc-zenburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,hc-zenburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,hc-zenburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,hc-zenburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,hc-zenburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,hc-zenburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,hc-zenburn-red))))
   `(mew-face-mark-review ((t (:foreground ,hc-zenburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,hc-zenburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,hc-zenburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,hc-zenburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,hc-zenburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,hc-zenburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,hc-zenburn-green))))
   `(mew-face-eof-part ((t (:foreground ,hc-zenburn-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,hc-zenburn-cyan :background ,hc-zenburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,hc-zenburn-blue))))
   `(mingus-pausing-face ((t (:foreground ,hc-zenburn-magenta))))
   `(mingus-playing-face ((t (:foreground ,hc-zenburn-cyan))))
   `(mingus-playlist-face ((t (:foreground ,hc-zenburn-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,hc-zenburn-yellow))))
   `(mingus-stopped-face ((t (:foreground ,hc-zenburn-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,hc-zenburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,hc-zenburn-cyan))))
   `(nav-face-dir ((t (:foreground ,hc-zenburn-green))))
   `(nav-face-hdir ((t (:foreground ,hc-zenburn-red))))
   `(nav-face-file ((t (:foreground ,hc-zenburn-fg))))
   `(nav-face-hfile ((t (:foreground ,hc-zenburn-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,hc-zenburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,hc-zenburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,hc-zenburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,hc-zenburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,hc-zenburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,hc-zenburn-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,hc-zenburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,hc-zenburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,hc-zenburn-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,hc-zenburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,hc-zenburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,hc-zenburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,hc-zenburn-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,hc-zenburn-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,hc-zenburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,hc-zenburn-bg+2 :foreground ,hc-zenburn-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,hc-zenburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,hc-zenburn-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,hc-zenburn-green+3))))
   `(org-formula ((t (:foreground ,hc-zenburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,hc-zenburn-green+3))))
   `(org-hide ((t (:foreground ,hc-zenburn-bg-1))))
   `(org-level-1 ((t (:foreground ,hc-zenburn-orange))))
   `(org-level-2 ((t (:foreground ,hc-zenburn-green+4))))
   `(org-level-3 ((t (:foreground ,hc-zenburn-blue-1))))
   `(org-level-4 ((t (:foreground ,hc-zenburn-yellow-2))))
   `(org-level-5 ((t (:foreground ,hc-zenburn-cyan))))
   `(org-level-6 ((t (:foreground ,hc-zenburn-green+2))))
   `(org-level-7 ((t (:foreground ,hc-zenburn-red-4))))
   `(org-level-8 ((t (:foreground ,hc-zenburn-blue-4))))
   `(org-link ((t (:foreground ,hc-zenburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,hc-zenburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,hc-zenburn-red))))
   `(org-scheduled-today ((t (:foreground ,hc-zenburn-blue+1))))
   `(org-sexp-date ((t (:foreground ,hc-zenburn-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,hc-zenburn-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,hc-zenburn-orange))))
   `(org-todo ((t (:bold t :foreground ,hc-zenburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,hc-zenburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,hc-zenburn-bg-1))))
   `(org-column-title ((t (:background ,hc-zenburn-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-red-1))))
   `(org-ellipsis ((t (:foreground ,hc-zenburn-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,hc-zenburn-cyan :underline t))))
;;;;; outline
   `(outline-1 ((t (:foreground ,hc-zenburn-orange))))
   `(outline-2 ((t (:foreground ,hc-zenburn-green+4))))
   `(outline-3 ((t (:foreground ,hc-zenburn-blue-1))))
   `(outline-4 ((t (:foreground ,hc-zenburn-yellow-2))))
   `(outline-5 ((t (:foreground ,hc-zenburn-cyan))))
   `(outline-6 ((t (:foreground ,hc-zenburn-green+2))))
   `(outline-7 ((t (:foreground ,hc-zenburn-red-4))))
   `(outline-8 ((t (:foreground ,hc-zenburn-blue-4))))
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
   `(persp-selected-face ((t (:foreground ,hc-zenburn-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,hc-zenburn-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,hc-zenburn-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,hc-zenburn-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,hc-zenburn-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-orange))))
   `(proof-error-face ((t (:foreground ,hc-zenburn-fg :background ,hc-zenburn-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-orange))))
   `(proof-locked-face ((t (:background ,hc-zenburn-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-orange))))
   `(proof-queue-face ((t (:background ,hc-zenburn-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,hc-zenburn-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,hc-zenburn-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,hc-zenburn-bg))))
   `(proof-warning-face ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-yellow-1))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,hc-zenburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,hc-zenburn-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,hc-zenburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,hc-zenburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,hc-zenburn-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,hc-zenburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,hc-zenburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,hc-zenburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,hc-zenburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,hc-zenburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,hc-zenburn-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,hc-zenburn-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,hc-zenburn-blue))))
   `(rcirc-other-nick ((t (:foreground ,hc-zenburn-orange))))
   `(rcirc-bright-nick ((t (:foreground ,hc-zenburn-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,hc-zenburn-blue-2))))
   `(rcirc-server ((t (:foreground ,hc-zenburn-green))))
   `(rcirc-server-prefix ((t (:foreground ,hc-zenburn-green+1))))
   `(rcirc-timestamp ((t (:foreground ,hc-zenburn-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,hc-zenburn-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,hc-zenburn-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,hc-zenburn-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,hc-zenburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,hc-zenburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,hc-zenburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,hc-zenburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,hc-zenburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,hc-zenburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,hc-zenburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,hc-zenburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,hc-zenburn-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,hc-zenburn-orange))))
   `(rst-level-2-face ((t (:foreground ,hc-zenburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,hc-zenburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,hc-zenburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,hc-zenburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,hc-zenburn-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,hc-zenburn-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,hc-zenburn-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,hc-zenburn-red+1 :background ,hc-zenburn-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,hc-zenburn-bg+3 :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,hc-zenburn-red+1 :background ,hc-zenburn-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,hc-zenburn-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,hc-zenburn-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,hc-zenburn-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-red)))
      (t
       (:underline ,hc-zenburn-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-orange)))
      (t
       (:underline ,hc-zenburn-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-yellow)))
      (t
       (:underline ,hc-zenburn-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,hc-zenburn-green)))
      (t
       (:underline ,hc-zenburn-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,hc-zenburn-green+2))))
   `(speedbar-directory-face ((t (:foreground ,hc-zenburn-cyan))))
   `(speedbar-file-face ((t (:foreground ,hc-zenburn-fg))))
   `(speedbar-highlight-face ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-green+2))))
   `(speedbar-selected-face ((t (:foreground ,hc-zenburn-red))))
   `(speedbar-separator-face ((t (:foreground ,hc-zenburn-bg :background ,hc-zenburn-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,hc-zenburn-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,hc-zenburn-fg
                                    :background ,hc-zenburn-bg))))
   `(tabbar-selected ((t (:foreground ,hc-zenburn-fg
                                      :background ,hc-zenburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,hc-zenburn-fg
                                        :background ,hc-zenburn-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,hc-zenburn-bg
                                       :background ,hc-zenburn-bg-1))))
   `(term-color-red ((t (:foreground ,hc-zenburn-red-2
                                       :background ,hc-zenburn-red-4))))
   `(term-color-green ((t (:foreground ,hc-zenburn-green
                                       :background ,hc-zenburn-green+2))))
   `(term-color-yellow ((t (:foreground ,hc-zenburn-orange
                                       :background ,hc-zenburn-yellow))))
   `(term-color-blue ((t (:foreground ,hc-zenburn-blue-1
                                      :background ,hc-zenburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,hc-zenburn-magenta
                                         :background ,hc-zenburn-red))))
   `(term-color-cyan ((t (:foreground ,hc-zenburn-cyan
                                       :background ,hc-zenburn-blue))))
   `(term-color-white ((t (:foreground ,hc-zenburn-fg
                                       :background ,hc-zenburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,hc-zenburn-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,hc-zenburn-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,hc-zenburn-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,hc-zenburn-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,hc-zenburn-cyan))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,hc-zenburn-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,hc-zenburn-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,hc-zenburn-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,hc-zenburn-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,hc-zenburn-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,hc-zenburn-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,hc-zenburn-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,hc-zenburn-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,hc-zenburn-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,hc-zenburn-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,hc-zenburn-bg+1 :foreground ,hc-zenburn-bg+1))))
   `(whitespace-hspace ((t (:background ,hc-zenburn-bg+1 :foreground ,hc-zenburn-bg+1))))
   `(whitespace-tab ((t (:background ,hc-zenburn-red-1))))
   `(whitespace-newline ((t (:foreground ,hc-zenburn-bg+1))))
   `(whitespace-trailing ((t (:background ,hc-zenburn-red))))
   `(whitespace-line ((t (:background ,hc-zenburn-bg :foreground ,hc-zenburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,hc-zenburn-orange :foreground ,hc-zenburn-orange))))
   `(whitespace-indentation ((t (:background ,hc-zenburn-yellow :foreground ,hc-zenburn-red))))
   `(whitespace-empty ((t (:background ,hc-zenburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,hc-zenburn-yellow :foreground ,hc-zenburn-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,hc-zenburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,hc-zenburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,hc-zenburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,hc-zenburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,hc-zenburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,hc-zenburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,hc-zenburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,hc-zenburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,hc-zenburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,hc-zenburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,hc-zenburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,hc-zenburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,hc-zenburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,hc-zenburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,hc-zenburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,hc-zenburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,hc-zenburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,hc-zenburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,hc-zenburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,hc-zenburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,hc-zenburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,hc-zenburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,hc-zenburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,hc-zenburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,hc-zenburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,hc-zenburn-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,hc-zenburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,hc-zenburn-bg-1 :foreground ,hc-zenburn-bg-1))))
   ))

;;; Theme Variables
(hc-zenburn-with-color-variables
  (custom-theme-set-variables
   'hc-zenburn
;;;;; ansi-color
   `(ansi-color-names-vector [,hc-zenburn-bg ,hc-zenburn-red ,hc-zenburn-green ,hc-zenburn-yellow
                                          ,hc-zenburn-blue ,hc-zenburn-magenta ,hc-zenburn-cyan ,hc-zenburn-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,hc-zenburn-bg+3)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,hc-zenburn-red-1)
       ( 40. . ,hc-zenburn-red)
       ( 60. . ,hc-zenburn-orange)
       ( 80. . ,hc-zenburn-yellow-2)
       (100. . ,hc-zenburn-yellow-1)
       (120. . ,hc-zenburn-yellow)
       (140. . ,hc-zenburn-green-1)
       (160. . ,hc-zenburn-green)
       (180. . ,hc-zenburn-green+1)
       (200. . ,hc-zenburn-green+2)
       (220. . ,hc-zenburn-green+3)
       (240. . ,hc-zenburn-green+4)
       (260. . ,hc-zenburn-cyan)
       (280. . ,hc-zenburn-blue-2)
       (300. . ,hc-zenburn-blue-1)
       (320. . ,hc-zenburn-blue)
       (340. . ,hc-zenburn-blue+1)
       (360. . ,hc-zenburn-magenta)))
   `(vc-annotate-very-old-color ,hc-zenburn-magenta)
   `(vc-annotate-background ,hc-zenburn-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar hc-zenburn-add-font-lock-keywords nil
  "Whether to add font-lock keywords for hc-zenburn color names.
In buffers visiting library `hc-zenburn-theme.el' the hc-zenburn
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar hc-zenburn-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after hc-zenburn activate)
;;   "Maybe also add font-lock keywords for hc-zenburn colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or hc-zenburn-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "hc-zenburn-theme.el")))
;;     (unless hc-zenburn-colors-font-lock-keywords
;;       (setq hc-zenburn-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car hc-zenburn-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc hc-zenburn-colors-alist))))))
;;     (font-lock-add-keywords nil hc-zenburn-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after hc-zenburn activate)
;;   "Also remove font-lock keywords for hc-zenburn colors."
;;   (font-lock-remove-keywords nil hc-zenburn-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'hc-zenburn)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; hc-zenburn-theme.el ends here
