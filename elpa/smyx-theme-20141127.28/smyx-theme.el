;;; smyx-theme.el --- smyx Color Theme

;; Copyright 2012-2014, Uriel G Maldonado

;; Author: Uriel G Maldonado <uriel781@gmail.com>
;; Keywords: color theme smyx
;; Package-Version: 20141127.28
;; Version: 0.10

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
;;

;;; Commentary:
;;; This theme is based on smyck created by hukl.
;;; (https://github.com/hukl/Smyck-Color-Scheme)
;;; The cyberpunk theme was used as a starting point for smyx which was
;;; created by Nicholas M. Van Horn.

;; This theme is a port of the overtone/emacs-live theme of the same name
;; (https://github.com/overtone/emacs-live). The original theme was
;; designed for use with the color-theme package. This theme adopts the
;; new built-in theme support deftheme. Additionally, this
;; theme strives to offer as many mode-specific customizations as
;; possible, with further tweaks that suit my fancy.

;;; Code:

(deftheme smyx "The smyx color theme")

(let ((class '((class color) (min-colors 89)))
      ;; smyx palette
      (smyx-fg "#F7F7F7")

      (smyx-bg-05 "#151515")
      (smyx-bg-1 "#202020")
      (smyx-bg "#282828")
      (smyx-bg+1 "#383838")
      (smyx-bg+2 "#484848")
      (smyx-bg+3 "#585858")

      (smyx-red+1 "#992222")
     (smyx-red "#FAB1AB")
      (smyx-red-1 "#FAB1AB")
      (smyx-red-2 "#FAB1AB")
      (smyx-red-3 "#FAB1AB")
      (smyx-red-4 "#FAB1AB")
      (smyx-red-5 "#FAB1AB")

      (smyx-pink  "#EBA9F7")
      (smyx-pink-1 "#DB98E7")
      (smyx-pink-2 "#CB88D7")

      (smyx-orange-2 "#FFBB00")
      (smyx-orange-1 "#F0AA00") ;; DarkOrange
      (smyx-orange "#FFA600")

      (smyx-yellow "#F6DC69")
      (smyx-yellow-1 "#FBDE2D")
      (smyx-yellow-2 "#d0bf8f")
      (smyx-yellow-3 "#D8FA3C")
      (smyx-yellow-4 "#E9C062")
      (smyx-yellow-5 "#ffd700")
      (smyx-yellow-org-todo "#DDDD00")

      (smyx-green-2 "#617a11")
      (smyx-green-1 "#91Ba31")
      (smyx-green "#D1FA71")
      (smyx-green+1 "#E1Fa81")
      (smyx-green+2 "#9fc59f")
      (smyx-green+3 "#afd8af")
      (smyx-green+4 "#bfebbf")
      (smyx-green-blue-org "#BBFFD1")   


      (smyx-cyan "#96D9F1")
      (smyx-blue+1 "#94bff3")
      (smyx-blue "#96D9F1")   
      (smyx-blue-1 "#7b68ee")
      (smyx-blue-2 "#6a5acd") 
      (smyx-blue-3 "#add8e6") 
      (smyx-blue-4 "#b2dfee") 
      (smyx-blue-5 "#96D9F1")
      (smyx-blue-6 "#96D9F1")
      (smyx-blue-7 "#00ffff")
      (smyx-blue-8 "#4F94CD")

      (smyx-magenta "#dc8cc3")

      (smyx-black "#282828")
      (smyx-black-2 "#353535")
      (smyx-black-3 "#414141")
      (smyx-black-3 "#515151")

      (smyx-gray "#d3d3d3")
      (smyx-gray-2 "#8B8989")
      (smyx-gray-3 "#919191")
      (smyx-gray-4 "#999999")
      (smyx-gray-5 "#333333")
      (smyx-gray-6 "#1A1A1A")
      (smyx-gray-7 "#4D4D4D")
      (smyx-gray-8 "#262626")
      (smyx-gray-9 "#8F8F8F")
      (smyx-white "#ffffff")
      (smyx-white-2 "#F8F8F8")
      (smyx-white-3 "#fffafa"))

 (custom-theme-set-faces
   'smyx
   '(button ((t (:underline t))))
   `(link ((,class (:foreground ,smyx-yellow :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,smyx-yellow-2 :underline t :weight normal))))
   `(blue ((,class (:foreground ,smyx-blue))))
   `(bold ((,class (:bold t))))
   `(bold-italic ((,class (:bold t))))
   `(border-glyph ((,class (nil))))
   `(buffers-tab ((,class (:background ,smyx-black-2 :foreground ,smyx-white-2))))

   ;;; basic coloring
   `(default ((,class (:foreground ,smyx-fg :background ,smyx-black))))
   `(cursor ((,class (:background ,smyx-fg))))
   `(escape-glyph-face ((,class (:foreground ,smyx-red))))
   ;; `(fringe ((,class (:foreground ,smyx-fg :background ,smyx-bg+1))))
   `(header-line ((,class (:foreground ,smyx-yellow
                                       :background ,smyx-bg-1
                                       :box (:line-width -1 :style released-button)))))
   `(highlight ((,class (:background ,smyx-gray-8))))

   ;;; highlight current line
   `(hl-line ((,class (:background ,smyx-bg+2))))
   
   ;;; compilation
   `(compilation-column-face ((,class (:foreground ,smyx-blue))))
   `(compilation-enter-directory-face ((,class (:foreground ,smyx-blue))))
   `(compilation-error-face ((,class (:foreground ,smyx-red ,:weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,smyx-fg))))
   `(compilation-info-face ((,class (:foreground ,smyx-blue))))
   `(compilation-info ((,class (:foreground ,smyx-blue :underline nil))))
   `(compilation-leave-directory-face ((,class (:foreground ,smyx-blue))))
   `(compilation-line-face ((,class (:foreground ,smyx-green))))
   `(compilation-line-number ((,class (:foreground ,smyx-yellow))))
   `(compilation-message-face ((,class (:foreground ,smyx-blue))))
   `(compilation-warning-face ((,class (:foreground ,smyx-yellow :weight bold :underline t))))

   ;;; grep
   `(grep-context-face ((,class (:foreground ,smyx-black :background ,smyx-pink-1))))
   `(grep-error-face ((,class (:foreground ,smyx-red :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,smyx-black :background ,smyx-red))))
   `(grep-match-face ((,class (:foreground ,smyx-black :background ,smyx-pink-1))))
   `(match ((,class (:background ,smyx-black :foreground ,smyx-pink-1))))


   ;;; multiple-cursors
   `(mc/cursor-face ((,class (:inverse-video nil, :background ,smyx-pink :foreground ,smyx-black))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,smyx-black :background ,smyx-green))))
   `(isearch-fail ((,class (:foreground ,smyx-black :background ,smyx-pink))))

   `(lazy-highlight ((,class (:foreground ,smyx-black :background ,smyx-yellow))))
   `(query-replace ((,class (:background ,smyx-gray-5))))
   `(Highline-face ((,class (:background ,smyx-green))))
   `(italic ((,class (nil))))
   `(left-margin ((,class (nil))))
   `(toolbar ((,class (nil))))
   `(underline ((,class (:underline nil))))
   `(text-cursor ((,class (:background ,smyx-yellow :foreground ,smyx-black))))
   `(menu ((,class (:foreground ,smyx-fg :background ,smyx-bg))))
   `(minibuffer-prompt ((,class (:foreground ,smyx-green+1 :background ,smyx-black))))
   `(mode-line
     ((,class (:foreground ,smyx-blue ;;; dominant
                           :background ,smyx-black-2))))
   ;; `(mode-line-buffer-id ((,class (:foreground ,smyx-yellow :weight bold))))

   `(mode-line-inactive
     ((,class (:foreground ,smyx-gray-7
                           :background ,smyx-gray-6))))
   
   `(region ((,class (:background ,smyx-black-3))))
   `(secondary-selection ((,class (:background ,smyx-bg+2))))
   `(trailing-whitespace ((,class (:background ,smyx-red))))
   `(vertical-border ((,class (:foreground ,smyx-gray-5 :background ,smyx-black))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,smyx-orange))))
   `(font-lock-comment-face ((,class (:foreground ,smyx-gray-9 :italic t))))
   ;; `(font-lock-comment-delimiter-face ((,class (:foreground ,smyx-green)))) 
   `(font-lock-constant-face ((,class (:foreground ,smyx-red))))
   ;; `(font-lock-doc-face ((,class (:foreground ,smyx-green+0))))
   `(font-lock-doc-string-face ((,class (:foreground ,smyx-yellow))))
   `(font-lock-function-name-face ((,class (:foreground ,smyx-blue))))
   `(font-lock-keyword-face ((,class (:foreground ,smyx-green))))
   `(font-lock-negation-char-face ((,class (:foreground ,smyx-red))))
   `(font-lock-preprocessor-face ((,class (:foreground ,smyx-gray-3))))
   `(font-lock-string-face ((,class (:foreground ,smyx-yellow))))
   `(font-lock-type-face ((,class (:foreground ,smyx-blue))))

   `(font-lock-variable-name-face ((,class (:foreground ,smyx-blue))))
   `(font-lock-warning-face ((,class (:foreground ,smyx-pink))))
   `(font-lock-reference-face ((,class (:foreground ,smyx-gray))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,smyx-yellow))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,smyx-red))))


   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   `(gui-element ((,class (:background ,smyx-gray-5 :foreground ,smyx-blue-6))))
   

   ;;; newsticker
   ;; These are currently placeholders that probably look terrible.
   ;; Someone who uses newsticker is welcome to change these
   `(newsticker-date-face ((,class (:foreground ,smyx-fg))))
   `(newsticker-default-face ((,class (:foreground ,smyx-fg))))
   `(newsticker-enclosure-face ((,class (:foreground ,smyx-green+3))))
   `(newsticker-extra-face ((,class (:foreground ,smyx-bg+2 :height 0.8))))
   `(newsticker-feed-face ((,class (:foreground ,smyx-fg))))
   `(newsticker-immortal-item-face ((,class (:foreground ,smyx-green))))
   `(newsticker-new-item-face ((,class (:foreground ,smyx-blue))))
   `(newsticker-obsolete-item-face ((,class (:foreground ,smyx-red))))
   `(newsticker-old-item-face ((,class (:foreground ,smyx-bg+3))))
   `(newsticker-statistics-face ((,class (:foreground ,smyx-fg))))
   `(newsticker-treeview-face ((,class (:foreground ,smyx-fg))))
   `(newsticker-treeview-immortal-face ((,class (:foreground ,smyx-green))))
   `(newsticker-treeview-listwindow-face ((,class (:foreground ,smyx-fg))))
   `(newsticker-treeview-new-face ((,class (:foreground ,smyx-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((,class (:foreground ,smyx-red))))
   `(newsticker-treeview-old-face ((,class (:foreground ,smyx-bg+3))))
   `(newsticker-treeview-selection-face ((,class (:foreground ,smyx-yellow))))

   ;;; external

   ;; full-ack
   `(ack-separator ((,class (:foreground ,smyx-fg))))
   `(ack-file ((,class (:foreground ,smyx-blue))))
   `(ack-line ((,class (:foreground ,smyx-yellow))))
   `(ack-match ((,class (:foreground ,smyx-orange :background ,smyx-bg-1 :weigth bold))))

   ;; auctex
   `(font-latex-bold ((,class (:inherit bold))))
   `(font-latex-warning ((,class (:inherit font-lock-warning))))
   `(font-latex-sedate ((,class (:foreground ,smyx-yellow :weight bold))))
   `(font-latex-string ((,class (:foreground ,smyx-green))))
   `(font-latex-title-4 ((,class (:inherit variable-pitch :weight bold))))
   `(font-latex-sectioning-0 ((,class (:foreground ,smyx-blue :background ,smyx-black :scale 1.5))))
   `(font-latex-sectioning-1 ((,class (:foreground ,smyx-blue :background ,smyx-black :scale 1.5))))

   ;; auto-complete
   `(ac-completion-face ((,class (:background ,smyx-gray-4 :underline t))))
   `(ac-candidate-face ((,class (:background ,smyx-gray-5 :foreground ,smyx-white))))
   `(ac-selection-face ((,class (:background ,smyx-green :foreground ,smyx-black))))
   `(popup-tip-face ((,class (:background ,smyx-gray-5 :foreground ,smyx-white))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,smyx-black-3))))
   `(popup-scroll-bar-background-face ((,class (:background ,smyx-gray-5))))
   `(popup-isearch-match ((,class (:background ,smyx-black :foreground ,smyx-pink))))
   `(window-number-face ((,class (:background ,smyx-gray-6 :foreground ,smyx-green))))

   ;; company-mode
   ;; company--
   `(company-tooltip ((,class (:background ,smyx-gray-5 :foreground ,smyx-white))))
   `(company-tooltip-common ((,class (:inherit company-tooltip :foreground ,smyx-green))))
   `(company-tooltip-common-selection ((,class (:inherit company-tooltip-selection :foreground ,smyx-black-3))))
   `(company-tooltip-selection ((,class (:foreground ,smyx-black :background ,smyx-green))))
   `(company-tooltip-annotation ((,class (:foreground , smyx-black-3 :background ,smyx-gray-5))))

   `(company-scrollbar-fg ((,class (:background ,smyx-black-3))))
   `(company-scrollbar-bg ((,class (:background ,smyx-gray-3))))
   `(company-preview ((,class (:foreground ,smyx-gray-2 :background ,smyx-pink))))

   ;; diff
   `(diff-added ((,class (:foreground ,smyx-green))))
   `(diff-changed ((,class (:foreground ,smyx-yellow))))
   `(diff-removed ((,class (:foreground ,smyx-red))))
   `(diff-header ((,class (:background ,smyx-bg+2))))
   `(diff-file-header ((,class (:background ,smyx-bg+2 :foreground ,smyx-fg :bold t))))

   ;; ediff
   `(ediff-current-diff-Ancestor ((,class (:foreground ,smyx-fg :background ,smyx-pink))))
   `(ediff-current-diff-A ((,class (:foreground ,smyx-fg :background ,smyx-bg-05))))
   `(ediff-current-diff-B ((,class (:foreground ,smyx-fg :background ,smyx-bg+1))))
   `(ediff-current-diff-C ((,class (:foreground ,smyx-fg :background ,smyx-bg+2))))
   `(ediff-even-diff-Ancestor ((,class (:foreground ,smyx-white :background ,smyx-bg-05))))
   `(ediff-even-diff-A ((,class (:foreground ,smyx-white :background ,smyx-bg+1))))
   `(ediff-even-diff-B ((,class (:foreground ,smyx-white :background ,smyx-bg+2))))
   `(ediff-even-diff-C ((,class (:foreground ,smyx-white :background ,smyx-bg+3))))
   `(ediff-fine-diff-Ancestor ((,class (:foreground ,smyx-black :background ,smyx-pink))))
   `(ediff-fine-diff-A ((,class (:foreground ,smyx-black :background ,smyx-blue-5))))
   `(ediff-fine-diff-B ((,class (:foreground ,smyx-black :background ,smyx-blue-5))))
   `(ediff-fine-diff-C ((,class (:foreground ,smyx-black :background ,smyx-blue-5))))
   `(ediff-odd-diff-Ancestor ((,class (:foreground ,smyx-black :background ,smyx-gray-2))))
   `(ediff-odd-diff-A ((,class (:foreground ,smyx-black :background ,smyx-gray-3))))
   `(ediff-odd-diff-B ((,class (:foreground ,smyx-black :background ,smyx-gray-4))))
   `(ediff-odd-diff-C ((,class (:foreground ,smyx-black :background ,smyx-gray))))

   ;; ert
   `(ert-test-result-expected ((,class (:foreground ,smyx-green+4 :background ,smyx-bg))))
   `(ert-test-result-unexpected ((,class (:foreground ,smyx-red :background ,smyx-bg))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,smyx-yellow :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,smyx-red-1 :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,smyx-blue+1 :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,smyx-red+1 :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,smyx-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,smyx-yellow :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,smyx-cyan :weight bold))))

   ;; flymake
   `(flymake-errline ((,class (:foreground ,smyx-red :weight bold :underline t))))
   `(flymake-warnline ((,class (:foreground ,smyx-orange :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,smyx-yellow :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,smyx-orange :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,smyx-blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,smyx-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,smyx-yellow))))
   `(erc-keyword-face ((,class (:foreground ,smyx-blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,smyx-yellow :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,smyx-red :weigth bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,smyx-green))))
   `(erc-pal-face ((,class (:foreground ,smyx-orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,smyx-orange :background ,smyx-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,smyx-green+1))))
   `(erc-underline-face ((t (:underline t))))

   ;; gnus
   `(gnus-group-mail-1 ((,class (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((,class (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((,class (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((,class (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((,class (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((,class (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((,class (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((,class (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((,class (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((,class (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((,class (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((,class (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((,class (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((,class (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((,class (:inherit message-header-other))))
   `(gnus-header-from ((,class (:inherit message-header-from))))
   `(gnus-header-name ((,class (:inherit message-header-name))))
   `(gnus-header-newsgroups ((,class (:inherit message-header-other))))
   `(gnus-header-subject ((,class (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((,class (:foreground ,smyx-orange))))
   `(gnus-summary-high-ancient ((,class (:foreground ,smyx-blue))))
   `(gnus-summary-high-read ((,class (:foreground ,smyx-green :weight bold))))
   `(gnus-summary-high-ticked ((,class (:foreground ,smyx-orange :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,smyx-fg :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,smyx-blue))))
   `(gnus-summary-low-read ((t (:foreground ,smyx-green))))
   `(gnus-summary-low-ticked ((,class (:foreground ,smyx-orange :weight bold))))
   `(gnus-summary-low-unread ((,class (:foreground ,smyx-fg))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,smyx-blue+1))))
   `(gnus-summary-normal-read ((,class (:foreground ,smyx-green))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,smyx-orange :weight bold))))
   `(gnus-summary-normal-unread ((,class (:foreground ,smyx-fg))))
   `(gnus-summary-selected ((,class (:foreground ,smyx-yellow :weight bold))))
   `(gnus-cite-1 ((,class (:foreground ,smyx-yellow-2))))
   `(gnus-cite-10 ((,class (:foreground ,smyx-yellow-1))))
   `(gnus-cite-11 ((,class (:foreground ,smyx-yellow))))
   `(gnus-cite-2 ((,class (:foreground ,smyx-blue-1))))
   `(gnus-cite-3 ((,class (:foreground ,smyx-blue-2))))
   `(gnus-cite-4 ((,class (:foreground ,smyx-green+2))))
   `(gnus-cite-5 ((,class (:foreground ,smyx-green+1))))
   `(gnus-cite-6 ((,class (:foreground ,smyx-green))))
   `(gnus-cite-7 ((,class (:foreground ,smyx-red))))
   `(gnus-cite-8 ((,class (:foreground ,smyx-red-1))))
   `(gnus-cite-9 ((,class (:foreground ,smyx-red-2))))
   `(gnus-group-news-1-empty ((,class (:foreground ,smyx-yellow))))
   `(gnus-group-news-2-empty ((,class (:foreground ,smyx-green+3))))
   `(gnus-group-news-3-empty ((,class (:foreground ,smyx-green+1))))
   `(gnus-group-news-4-empty ((,class (:foreground ,smyx-blue-2))))
   `(gnus-group-news-5-empty ((,class (:foreground ,smyx-blue-3))))
   `(gnus-group-news-6-empty ((,class (:foreground ,smyx-bg+2))))
   `(gnus-group-news-low-empty ((,class (:foreground ,smyx-bg+2))))
   `(gnus-signature ((,class (:foreground ,smyx-yellow))))
   `(gnus-x ((,class (:background ,smyx-fg :foreground ,smyx-bg))))

   ;; helm
   `(helm-header
     ((,class (:foreground ,smyx-green
                           :background ,smyx-bg
                           :underline nil
                           :box nil))))
   `(helm-source-header
     ((,class (:foreground ,smyx-yellow
                           :background ,smyx-bg-1
                           :underline nil
                           :weight bold
                           :box (:line-width -1 :style released-button)))))
   `(helm-selection ((,class (:background ,smyx-bg+1 :underline nil))))
   `(helm-selection-line ((,class (:background ,smyx-bg+1))))
   `(helm-visible-mark ((,class (:foreground ,smyx-bg :background ,smyx-yellow-2))))
   `(helm-candidate-number ((,class (:foreground ,smyx-green+4 :background ,smyx-bg-1))))

   ;; hl-line-mode
   `(hl-sexp-face ((,class (:background ,smyx-gray-5))))
   `(hl-line-face ((,class (:background ,smyx-gray-5))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,smyx-blue :background ,smyx-black))))
   `(ido-only-match ((,class (:foreground ,smyx-blue :background ,smyx-black))))
   `(ido-subdir ((,class (:foreground ,smyx-gray-4 :background ,smyx-black))))
   `(ido-indicator ((,class (:foreground ,smyx-bg :background ,smyx-pink))))

   ;; js2-mode
   `(js2-warning-face ((,class (:underline ,smyx-blue))))
   `(js2-error-face ((,class (:foreground ,smyx-red :weight bold))))
   `(js2-jsdoc-tag-face ((,class (:foreground ,smyx-green))))
   `(js2-jsdoc-type-face ((,class (:foreground ,smyx-green))))
   `(js2-jsdoc-value-face ((,class (:foreground ,smyx-green))))

   `(js2-function-param-face ((,class (:foreground, smyx-blue))))
   `(js2-external-variable ((,class (:foreground , smyx-blue))))
   `(js2-function-param ((,class (:foreground, smyx-yellow))))

   ;; jabber-mode
   `(jabber-roster-user-away ((,class (:foreground ,smyx-green+2))))
   `(jabber-roster-user-online ((,class (:foreground ,smyx-blue-1))))
   `(jabber-roster-user-dnd ((,class (:foreground ,smyx-red+1))))
   `(jabber-rare-time-face ((,class (:foreground ,smyx-green+1))))
   `(jabber-chat-prompt-local ((,class (:foreground ,smyx-blue-1))))
   `(jabber-chat-prompt-foreign ((,class (:foreground ,smyx-red+1))))
   `(jabber-activity-face((,class (:foreground ,smyx-red+1))))
   `(jabber-activity-personal-face ((,class (:foreground ,smyx-blue+1))))
   `(jabber-title-small ((,class (:height 1.1 :weight bold))))
   `(jabber-title-medium ((,class (:height 1.2 :weight bold))))
   `(jabber-title-large ((,class (:height 1.3 :weight bold))))

   ;; linum-mode
   `(linum ((,class (:foreground ,smyx-gray-9 :background ,smyx-bg))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,smyx-green))))
   `(magit-branch ((,class (:foreground ,smyx-blue))))
   `(magit-item-highlight ((,class (:background ,smyx-black-3))))
   `(magit-log-sha1 ((,class (:foreground ,smyx-blue))))
   `(magit-log-author ((,class (:foreground ,smyx-green))))
   `(magit-log-head-label-default ((,class (:background ,smyx-bg :foreground ,smyx-green))))
   `(magit-log-head-label-head ((,class (:background ,smyx-bg :foreground ,smyx-green))))
   `(magit-log-head-label-remote  ((,class (:background ,smyx-bg :foreground ,smyx-yellow))))
   `(magit-log-head-label-tags    ((,class (:background ,smyx-bg :foreground ,smyx-yellow))))
   `(magit-log-head-label-local    ((,class (:background ,smyx-bg :foreground ,smyx-blue))))
 

   `(magit-diff-add ((,class (:foreground ,smyx-green))))
   `(magit-diff-del ((,class (:foreground ,smyx-red))))
   `(magit-diff-hunk-header ((,class (:foreground ,smyx-yellow))))

   `(eval-sexp-fu-flash ((,class (:background ,smyx-gray-8 :foreground ,smyx-pink-2))))

   ;; message-mode
   `(message-cited-text ((,class (:inherit font-lock-comment))))
   `(message-header-name ((,class (:foreground ,smyx-blue-5))))
   `(message-header-other ((,class (:foreground ,smyx-green))))
   `(message-header-to ((,class (:foreground ,smyx-pink-1 :weight bold))))
   `(message-header-from ((,class (:foreground ,smyx-yellow :weight bold))))
   `(message-header-cc ((,class (:foreground ,smyx-yellow :weight bold))))
   `(message-header-newsgroups ((,class (:foreground ,smyx-yellow :weight bold))))
   `(message-header-subject ((,class (:foreground ,smyx-orange :weight bold))))
   `(message-header-xheader ((,class (:foreground ,smyx-green))))
   `(message-mml ((,class (:foreground ,smyx-yellow :weight bold))))
   `(message-separator ((,class (:inherit font-lock-comment))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,smyx-orange))))
   `(mew-face-header-from ((,class (:foreground ,smyx-yellow))))
   `(mew-face-header-date ((,class (:foreground ,smyx-green))))
   `(mew-face-header-to ((,class (:foreground ,smyx-red))))
   `(mew-face-header-key ((,class (:foreground ,smyx-green))))
   `(mew-face-header-private ((,class (:foreground ,smyx-green))))
   `(mew-face-header-important ((,class (:foreground ,smyx-blue))))
   `(mew-face-header-marginal ((,class (:foreground ,smyx-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,smyx-red))))
   `(mew-face-header-xmew ((,class (:foreground ,smyx-green))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,smyx-red))))
   `(mew-face-body-url ((,class (:foreground ,smyx-orange))))
   `(mew-face-body-comment ((,class (:foreground ,smyx-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,smyx-green))))
   `(mew-face-body-cite2 ((,class (:foreground ,smyx-blue))))
   `(mew-face-body-cite3 ((,class (:foreground ,smyx-orange))))
   `(mew-face-body-cite4 ((,class (:foreground ,smyx-yellow))))
   `(mew-face-body-cite5 ((,class (:foreground ,smyx-red))))
   `(mew-face-mark-review ((,class (:foreground ,smyx-blue))))
   `(mew-face-mark-escape ((,class (:foreground ,smyx-green))))
   `(mew-face-mark-delete ((,class (:foreground ,smyx-red))))
   `(mew-face-mark-unlink ((,class (:foreground ,smyx-yellow))))
   `(mew-face-mark-refile ((,class (:foreground ,smyx-green))))
   `(mew-face-mark-unread ((,class (:foreground ,smyx-red-2))))
   `(mew-face-eof-message ((,class (:foreground ,smyx-green))))
   `(mew-face-eof-part ((,class (:foreground ,smyx-yellow))))

   ;; mic-paren
   `(paren-face-match ((,class (:foreground ,smyx-cyan :background ,smyx-bg :weight bold))))
   `(paren-face-mismatch ((,class (:foreground ,smyx-bg :background ,smyx-magenta :weight bold))))
   `(paren-face-no-match ((,class (:foreground ,smyx-bg :background ,smyx-red :weight bold))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,smyx-yellow))))
   `(nav-face-button-num ((,class (:foreground ,smyx-cyan))))
   `(nav-face-dir ((,class (:foreground ,smyx-green))))
   `(nav-face-hdir ((,class (:foreground ,smyx-red))))
   `(nav-face-file ((,class (:foreground ,smyx-fg))))
   `(nav-face-hfile ((,class (:foreground ,smyx-red-4))))

   ;; mumamo
   `(mumamo-background-chunk-major ((,class (:background ,smyx-black))))
   `(mumamo-background-chunk-submode1 ((,class (:background ,smyx-black))))
   `(mumamo-background-chunk-submode2 ((,class (:background ,smyx-bg+2))))
   `(mumamo-background-chunk-submode3 ((,class (:background ,smyx-bg+3))))
   `(mumamo-background-chunk-submode4 ((,class (:background ,smyx-bg+1))))

   ;; org-mode
   `(org-document-title ((,class (:foreground ,smyx-blue :background ,smyx-black :weight bold ))))
   `(org-document-info ((,class (:foreground ,smyx-green :background ,smyx-black :weight bold))))
   `(org-document-info-keyword ((,class (:foreground ,smyx-gray-2 :background ,smyx-black))))
   `(org-agenda-date-today
     ((,class (:foreground ,smyx-yellow :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face))))
   `(org-archived ((,class (:slant italic))))
   `(org-checkbox ((,class (:background ,smyx-gray-2 :foreground ,smyx-white
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((,class (:foreground ,smyx-green))))
   `(org-done ((,class (:bold t :weight bold :foreground ,smyx-green-1
                              :box (:line-width 1 :style none)))))
   `(org-todo ((,class (:bold t :foreground ,smyx-yellow-org-todo :weight bold
                              :box (:line-width 1 :style none)))))

   `(org-level-1 ((,class (:foreground ,smyx-blue ))))
   `(org-level-2 ((,class (:foreground ,smyx-green ))))
   `(org-level-3 ((,class (:foreground ,smyx-yellow ))))
   `(org-level-4 ((,class (:foreground ,smyx-orange))))
   `(org-level-5 ((,class (:foreground ,smyx-blue))))
   `(org-level-6 ((,class (:foreground ,smyx-green))))
   `(org-level-7 ((,class (:foreground ,smyx-yellow))))
   `(org-level-8 ((,class (:foreground ,smyx-orange))))

   `(org-link ((,class (:foreground ,smyx-blue+1 :underline t))))
   `(org-tag ((,class (:bold t :weight bold))))
   `(org-column ((,class (:background ,smyx-yellow :foreground ,smyx-black))))
   `(org-column-title ((,class (:background ,smyx-bg-1 :weight bold))))
   `(org-block ((,class (:foreground ,smyx-fg :background ,smyx-bg-05))))
   `(org-block-begin-line 
     ((,class (:foreground "#008ED1" :background ,smyx-bg-1))))
   `(org-block-background ((,class (:background ,smyx-bg-05))))
   `(org-block-end-line 
     ((,class (:foreground "#008ED1" :background ,smyx-bg-1))))
   `(org-hide ((,class (:foreground ,smyx-bg)))) ;; hide leading stars
   `(org-headline-done ((,class (:foreground ,smyx-green-1))))

   ;; `(org-deadline-announce ((,class (:foreground ,smyx-red-1))))
   ;; `(org-scheduled ((,class (:foreground ,smyx-green+4))))
   ;; `(org-scheduled-previously ((,class (:foreground ,smyx-red-4))))
   ;; `(org-scheduled-today ((,class (:foreground ,smyx-blue+1))))
   ;; `(org-special-keyword ((,class (:foreground ,smyx-yellow-1))))
   ;; `(org-table ((,class (:foreground ,smyx-green+2))))
   ;; `(org-time-grid ((,class (:foreground ,smyx-orange))))
   ;; `(org-upcoming-deadline ((,class (:inherit font-lock-keyword-face))))
   ;; `(org-warning ((,class (:bold t :foreground ,smyx-red :weight bold :underline nil))))
   ;; `(org-formula ((,class (:foreground ,smyx-yellow-2))))

   ;; outline
   `(outline-8 ((,class (:inherit default))))
   `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
   `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
   `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
   `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
   `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
   `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
   `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,smyx-red-1))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,smyx-green-2))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,smyx-pink-1))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,smyx-yellow))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,smyx-green))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,smyx-blue-3))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,smyx-orange))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,smyx-blue-2))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,smyx-gray))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,smyx-white))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,smyx-blue+1))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,smyx-red-4))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,smyx-green))))
   `(rpm-spec-doc-face ((,class (:foreground ,smyx-green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,smyx-red))))
   `(rpm-spec-macro-face ((,class (:foreground ,smyx-yellow))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,smyx-red))))
   `(rpm-spec-package-face ((,class (:foreground ,smyx-red))))
   `(rpm-spec-section-face ((,class (:foreground ,smyx-yellow))))
   `(rpm-spec-tag-face ((,class (:foreground ,smyx-blue))))
   `(rpm-spec-var-face ((,class (:foreground ,smyx-red))))

   ;; rst-mode
   `(rst-level-1-face ((,class (:foreground ,smyx-orange))))
   `(rst-level-2-face ((,class (:foreground ,smyx-green+1))))
   `(rst-level-3-face ((,class (:foreground ,smyx-blue-1))))
   `(rst-level-4-face ((,class (:foreground ,smyx-yellow-2))))
   `(rst-level-5-face ((,class (:foreground ,smyx-cyan))))
   `(rst-level-6-face ((,class (:foreground ,smyx-green-1))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,smyx-red-3 :background ,smyx-black))))
   `(show-paren-match ((,class (:foreground ,smyx-black :background ,smyx-pink-1))))

   `(naeu-green-face ((,class (:foreground ,smyx-green :background ,smyx-black))))
   `(naeu-pink-face ((,class (:foreground ,smyx-pink-1 :background ,smyx-black))))
   `(naeu-blue-face ((,class (:foreground ,smyx-blue-1 :background ,smyx-black))))
   `(naeu-orange-face ((,class (:foreground ,smyx-yellow-1 :background ,smyx-black))))
   `(naeu-red-face ((,class (:foreground ,smyx-orange :background ,smyx-black))))
   `(naeu-grey-face ((,class (:foreground ,smyx-gray-7 :background ,smyx-black))))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,smyx-red))))

  ;; elscreen
     `(elscreen-tab-other-screen-face (
        (,class (:foreground ,smyx-gray :background ,smyx-black))))

     `(elscreen-tab-current-screen-face (
        (,class (:foreground ,smyx-gray-5 :background ,smyx-gray))))
    

  ;;; ansi-term
   `(term-color-black ((,class (:foreground ,smyx-bg
                                            :background ,smyx-bg-1))))
   `(term-color-red ((,class (:foreground ,smyx-red-2
                                          :background ,smyx-red-4))))
   `(term-color-green ((,class (:foreground ,smyx-green
                                            :background ,smyx-green+2))))
   `(term-color-yellow ((,class (:foreground ,smyx-orange
                                             :background ,smyx-yellow))))
   `(term-color-blue ((,class (:foreground ,smyx-blue-1
                                           :background ,smyx-blue-4))))
   `(term-color-magenta ((,class (:foreground ,smyx-magenta
                                              :background ,smyx-red))))
   `(term-color-cyan ((,class (:foreground ,smyx-cyan
                                           :background ,smyx-blue))))
   `(term-color-white ((,class (:foreground ,smyx-fg
                                            :background ,smyx-bg-1))))
   `(term-default-fg-color ((,class (:inherit term-color-white))))
   `(term-default-bg-color ((,class (:inherit term-color-black))))

   ;; volatile-highlights
   `(vhl/default-face ((,class (:background ,smyx-gray-5))))

   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,smyx-pink-1 :background ,smyx-black))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,smyx-bg :foreground ,smyx-bg+1))))
   `(whitespace-hspace ((,class (:background ,smyx-bg :foreground ,smyx-bg+1))))
   `(whitespace-tab ((,class (:background ,smyx-bg :foreground ,smyx-red))))
   `(whitespace-newline ((,class (:foreground ,smyx-bg+1))))
   `(whitespace-trailing ((,class (:foreground ,smyx-red :background ,smyx-bg))))
   `(whitespace-line ((,class (:background ,smyx-bg-05 :foreground ,smyx-magenta))))
   `(whitespace-space-before-tab ((,class (:background ,smyx-orange :foreground ,smyx-orange))))
   `(whitespace-indentation ((,class (:background ,smyx-yellow :foreground ,smyx-red))))
   `(whitespace-empty ((,class (:background ,smyx-yellow :foreground ,smyx-red))))
   `(whitespace-space-after-tab ((,class (:background ,smyx-yellow :foreground ,smyx-red))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,smyx-red-2))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,smyx-red-1))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,smyx-orange))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,smyx-blue))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,smyx-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,smyx-blue))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,smyx-red-1))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,smyx-red))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,smyx-green+2))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,smyx-blue))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,smyx-blue+1))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,smyx-green))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,smyx-red+1))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,smyx-green+2))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,smyx-green+1))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,smyx-green+2))))
   `(wl-highlight-message-signature ((,class (:foreground ,smyx-green))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,smyx-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,smyx-blue))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,smyx-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,smyx-blue))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,smyx-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,smyx-yellow))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,smyx-magenta))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,smyx-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((,class (:foreground ,smyx-green+4))))

   ;; yasnippet
   `(yas/field-highlight-face ((,class (:background ,smyx-pink-1 :foreground ,smyx-black))))

   ;; enh-ruby-mode
   `(enh-ruby-op-face ((,class (:foreground ,smyx-orange)))) ; || and +
   `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,smyx-blue))))
   `(enh-ruby-string-delimiter-face ((,class (:foreground ,smyx-green))))
   `(enh-ruby-regexp-delimiter-face ((,class (:color ,smyx-red))))
   `(erm-syn-errline ((,class (:color ,smyx-red))))
   `(erm-syn-warnline ((,class (:color ,smyx-red))))

   ;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,smyx-green))))
   `(guide-key/key-face ((t (:foreground ,smyx-white))))
   `(guide-key/prefix-command-face ((t (:foreground ,smyx-yellow))))

   ;; yascroll
   `(yascroll:thumb-text-area ((,class (:background ,smyx-bg-1))))
   `(yascroll:thumb-fringe ((,class (:background ,smyx-bg-1 :foreground ,smyx-bg-1))))
   )

  ;;; custom theme variables
  (custom-theme-set-variables
   'smyx
   `(ansi-color-names-vector [,smyx-bg ,smyx-red-2 ,smyx-green ,smyx-orange
                                          ,smyx-blue-1 ,smyx-magenta ,smyx-cyan ,smyx-fg])
   ;; fill-column-indicator
   `(fci-rule-color ,smyx-bg-05)))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smyx)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; smyx-theme.el ends here.
