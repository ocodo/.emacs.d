;;; qsimpleq-theme.el --- Based on solarized color theme for Emacs.

;; Copyright (C) 2011,2012 Bozhidar Batsov, Kirill Babikhin

;; Author: Kirill Babikhin <mrakobes86@gmail.com>
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; Author: Thomas Frössman <thomasf@jossystem.se>
;; URL: https://bitbucket.org/qsimpleq/qsimpleq-theme
;; Version: 0.1.3

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
;;
;; Based on solarized color theme for Emacs.
;;
;;; Installation:
;;
;;   Drop the theme in a folder that is on `custom-theme-load-path'
;; and enjoy!
;;
;; Don't forget that the theme requires Emacs 24.
;;
;;; Bugs
;;
;; None that I'm aware of.
;;
;;; Credits
;;
;; Ethan Schoonover created the original theme for vim on such this port
;; is based.
;;
;;; Code

(deftheme qsimpleq "The light variant of the Qsimpleq color theme")

(let* ((class '((class color) (min-colors 89)))
       (base03    "#002b36")
       (base02    "#073642")
       ;; emphasized content
       (base01    "#586e75")
       ;; primary content
       (base00    "#000000")
       (base0     "#839496")
       ;; comments
       (base1     "#93a1a1")
       ;; background highlight light
       (base2     "#F2F2F2")
       ;; background light
       (base3     "#FAF9F8")
       (base4     "#D1D1D1")

       ;; Qsimpleq accented colors
       (yellow    "#b58900")
       (orange    "#cb4b16")
       (red       "#dc322f")
       (magenta   "#d33682")
       (violet    "#6c71c4")
       (blue      "#268BD2")
       (cyan      "#00877C")
       (green     "#859900")

       ;; Darker and lighter accented colors
       ;; Only use these in exceptional circumstances!
       (yellow-d  "#7B6000")
       (yellow-l  "#DEB542")
       (orange-d  "#8B2C02")
       (orange-l  "#F2804F")
       (red-d     "#990A1B")
       (red-l     "#FF6E64")
       (magenta-d "#93115C")
       (magenta-l "#F771AC")
       (violet-d  "#3F4D91")
       (violet-l  "#9EA0E5")
       (blue-d    "#00629D")
       (blue-l    "#6DA8D2")
       (cyan-d    "#00736F")
       (cyan-l    "#69CABF")
       (green-d   "#546E00")
       (green-l   "#B4C342")

       ;; Light/Dark adaptive qsimpleq colors
       (qsimpleq-fg base00)
       (qsimpleq-bg base3)
       (qsimpleq-hl base2)
       (qsimpleq-emph base01)
       (qsimpleq-comments base1)

       ;; Light/Dark adaptive higher/lower contrast accented colors
       ;; Only use these in exceptional cirmumstances!
       (yellow-hc yellow-d)
       (yellow-lc yellow-l)
       (orange-hc orange-d)
       (orange-lc orange-l)
       (red-hc red-d)
       (red-lc red-l)
       (magenta-hc magenta-d)
       (magenta-lc magenta-l)
       (violet-hc violet-d)
       (violet-lc violet-l)
       (blue-hc blue-d)
       (blue-lc blue-l)
       (cyan-hc cyan-d)
       (cyan-lc cyan-l)
       (green-hc green-d)
       (green-lc green-l))
  (custom-theme-set-faces
   'qsimpleq
   '(button ((t (:underline t))))

   ;; basic coloring
   `(default ((,class (:foreground ,qsimpleq-fg :background ,qsimpleq-bg))))
   `(shadow ((,class (:foreground ,qsimpleq-comments))))
   `(match ((,class (:background ,qsimpleq-hl :foreground ,qsimpleq-emph :weight bold))))
   `(cursor ((,class (:foreground ,qsimpleq-bg :background ,qsimpleq-fg :inverse-video t))))
   `(escape-glyph-face ((,class (:foreground ,red))))
   `(fringe ((,class (:foreground ,qsimpleq-fg :background ,qsimpleq-hl))))
   `(header-line ((,class (:foreground ,yellow
                                       :background ,qsimpleq-hl
                                       :box (:line-width -1 :style released-button)))))
   `(highlight ((,class (:background ,qsimpleq-hl))))
   `(link ((,class (:foreground ,yellow :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,yellow :underline t :weight normal))))
   `(success ((,class (:foreground ,green ))))
   `(warning ((,class (:foreground ,yellow ))))
   `(error ((,class (:foreground ,orange  :weight bold :underline t))))
   `(lazy-highlight ((,class (:foreground ,qsimpleq-emph :background ,qsimpleq-hl :bold t))))
   `(escape-glyph ((,class (:foreground ,violet))))

   ;; compilation
   `(compilation-column-face ((,class (:foreground ,yellow))))
   `(compilation-enter-directory-face ((,class (:foreground ,green))))
   `(compilation-error-face ((,class (:foreground ,red :weight bold :underline t))))
   `(compilation-face ((,class (:foreground ,qsimpleq-fg))))
   `(compilation-info-face ((,class (:foreground ,blue))))
   `(compilation-info ((,class (:foreground ,green :underline t))))
   `(compilation-leave-directory-face ((,class (:foreground ,green))))
   `(compilation-line-face ((,class (:foreground ,yellow))))
   `(compilation-line-number ((,class (:foreground ,yellow))))
   `(compilation-message-face ((,class (:foreground ,blue))))
   `(compilation-warning-face ((,class (:foreground ,yellow :weight bold :underline t))))

   ;; cperl
   `(cperl-array-face ((,class (:background nil :foreground nil :inherit font-lock-variable-name-face :weight bold))))
   `(cperl-hash-face  ((,class (:background nil :foreground nil :inherit font-lock-warning-face :slant italic  :weight bold))))
   `(cperl-nonoverridable-face ((,class (:foreground nil :inherit font-lock-function-name-face))))

   ;; cua
   `(cua-global-mark ((,class (:background ,yellow :foreground ,qsimpleq-bg))))
   `(cua-rectangle ((,class (:inherit region :background ,magenta :foreground ,qsimpleq-bg))))
   `(cua-rectangle-noselect ((,class (:inherit region :background ,qsimpleq-hl
                                               :foreground ,qsimpleq-comments))))

   ;; diary
   `(diary ((,class (:foreground ,yellow))))

   ;; dired
   `(dired-directory ((,class (:foreground ,blue :weight bold))))
   `(dired-flagged ((,class (:foreground ,red))))
   `(dired-header ((,class (:foreground ,qsimpleq-bg :background ,blue))))
   `(dired-ignored ((,class (:inherit shadow))))
   `(dired-mark ((,class (:foreground ,yellow :weight bold))))
   `(dired-marked ((,class (:foreground ,magenta :weight bold))))
   `(dired-perm-write ((,class (:foreground ,qsimpleq-fg :underline t))))
   `(dired-symlink ((,class (:foreground ,cyan :weight normal :slant italic))))
   `(dired-warning ((,class (:foreground ,orange :underline t))))

   ;; grep
   `(grep-context-face ((,class (:foreground ,qsimpleq-fg))))
   `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
   `(grep-hit-face ((,class (:foreground ,blue))))
   `(grep-match-face ((,class (:foreground ,orange :weight bold))))

   ;; faces used by isearch
   `(isearch ((,class (:foreground ,yellow :background ,qsimpleq-hl :bold t))))
   `(isearch-fail ((,class (:foreground ,red :background ,qsimpleq-bg :bold t))))

   ;; misc faces
   `(menu ((,class (:foreground ,qsimpleq-fg :background ,qsimpleq-bg))))
   `(minibuffer-prompt ((,class (:foreground ,qsimpleq-emph))))
   `(mode-line
     ((,class (:foreground ,qsimpleq-fg
                           :background ,qsimpleq-hl
                           :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:foreground ,qsimpleq-emph :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,qsimpleq-fg
                           :background ,qsimpleq-bg
                           :box (:line-width -1 :style released-button)))))
   `(region ((,class (:foreground ,qsimpleq-bg :background ,qsimpleq-emph))))
   `(secondary-selection ((,class (:background ,qsimpleq-bg))))
   `(trailing-whitespace ((,class (:background ,red))))
   `(vertical-border ((,class (:foreground ,qsimpleq-fg))))

   ;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,violet :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,qsimpleq-comments))))
   `(font-lock-comment-face ((,class (:foreground ,qsimpleq-comments))))
   `(font-lock-constant-face ((,class (:foreground ,magenta :weight bold))))
   `(font-lock-doc-face ((,class (:foreground ,cyan :slant italic))))
   `(font-lock-doc-string-face ((,class (:foreground ,blue))))
   `(font-lock-function-name-face ((,class (:foreground ,blue :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,green :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,qsimpleq-fg))))
   `(font-lock-preprocessor-face ((,class (:foreground ,blue))))
   `(font-lock-string-face ((,class (:foreground ,cyan))))
   `(font-lock-type-face ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,violet))))
   `(font-lock-warning-face ((,class (:foreground ,orange :weight bold :underline t))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

     ;;; external

   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,class (:foreground ,qsimpleq-comments :background ,qsimpleq-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((,class (:foreground ,red :background ,qsimpleq-bg :inverse-video nil))))

   ;; auto highlight symbol
   `(ahs-definition-face ((,class (:foreground ,qsimpleq-bg :background ,blue :underline t))))
   `(ahs-edit-mode-face ((,class (:foreground ,qsimpleq-bg :background ,yellow))))
   `(ahs-face ((,class (:foreground ,qsimpleq-bg :background ,blue))))
   `(ahs-plugin-bod-face ((,class (:foreground ,qsimpleq-bg :background ,blue))))
   `(ahs-plugin-defalt-face ((,class (:foreground ,qsimpleq-bg :background ,cyan))))
   `(ahs-plugin-whole-buffer-face ((,class (:foreground ,qsimpleq-bg :background ,green))))
   `(ahs-warning-face ((,class (:foreground ,red :weight bold))))

   ;; auto-complete
   `(ac-candidate-face ((,class (:background ,base4 :foreground ,qsimpleq-fg ))))
   `(ac-candidate-mouse-face ((,class (:background ,blue :foreground ,qsimpleq-fg ))))
   `(ac-nrepl-candidate-face ((,class (:background nil :foreground nil :inherit ac-candidate-face))))
   `(ac-slime-menu-face ((,class (:background nil :foreground nil :inherit ac-candidate-face))))

   ;; bm
   `(bm-face ((,class (:background ,yellow-lc :foreground ,qsimpleq-bg))))
   `(bm-fringe-face ((,class (:background ,yellow-lc :foreground ,qsimpleq-bg))))
   `(bm-fringe-persistent-face ((,class (:background ,green-lc :foreground ,qsimpleq-bg))))
   `(bm-persistent-face ((,class (:background ,green-lc :foreground ,qsimpleq-bg))))

   ;; custom
   `(custom-variable-tag ((,class (:foreground ,cyan))))
   `(custom-comment-tag ((,class (:foreground ,qsimpleq-comments))))
   `(custom-group-tag ((,class (:foreground ,blue))))
   `(custom-state ((,class (:foreground ,green))))

   ;; diff
   `(diff-added ((,class (:foreground ,green))))
   `(diff-changed ((,class (:foreground ,yellow))))
   `(diff-removed ((,class (:foreground ,red))))
   `(diff-header ((,class (:background ,qsimpleq-bg))))
   `(diff-file-header
     ((,class (:background ,qsimpleq-bg :foreground ,qsimpleq-fg :weight bold))))

   ;; eshell
   `(eshell-prompt ((,class (:foreground ,yellow :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,red :weight bold))))
   `(eshell-ls-backup ((,class (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((,class (:inherit font-lock-comment))))
   `(eshell-ls-directory ((,class (:foreground ,blue :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,red :weight bold))))
   `(eshell-ls-unreadable ((,class (:foreground ,qsimpleq-fg))))
   `(eshell-ls-missing ((,class (:inherit font-lock-warning))))
   `(eshell-ls-product ((,class (:inherit font-lock-doc))))
   `(eshell-ls-special ((,class (:foreground ,yellow :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,cyan :weight bold))))

   ;; flymake
   `(flymake-errline
     ((,class (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
   `(flymake-infoline ((,class (:foreground ,green-hc :background ,green-lc))))
   `(flymake-warnline
     ((,class (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))

   ;; flyspell
   `(flyspell-duplicate ((,class (:foreground ,yellow :weight bold :underline t))))
   `(flyspell-incorrect ((,class (:foreground ,red :weight bold :underline t))))

   ;; erc
   `(erc-action-face ((,class (:inherit erc-default-face))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,blue :weight bold))))
   `(erc-dangerous-host-face ((,class (:inherit font-lock-warning))))
   `(erc-default-face ((,class (:foreground ,qsimpleq-fg))))
   `(erc-direct-msg-face ((,class (:inherit erc-default))))
   `(erc-error-face ((,class (:inherit font-lock-warning :weight bold :underline t))))
   `(erc-fool-face ((,class (:inherit erc-default))))
   `(erc-highlight-face ((,class (:inherit hover-highlight))))
   `(erc-input-face ((,class (:foreground ,yellow))))
   `(erc-keyword-face ((,class (:foreground ,blue :weight bold))))
   `(erc-nick-default-face ((,class (:foreground ,yellow :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,red :weight bold))))
   `(erc-nick-msg-face ((,class (:inherit erc-default))))
   `(erc-notice-face ((,class (:foreground ,green))))
   `(erc-pal-face ((,class (:foreground ,orange :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,orange :background ,qsimpleq-bg :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,green))))
   `(erc-underline-face ((t (:underline t))))

   ;; gnus
   `(gnus-group-mail-1-face ((,class (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty-face ((,class (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2-face ((,class (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty-face ((,class (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3-face ((,class (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty-face ((,class (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4-face ((,class (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty-face ((,class (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5-face ((,class (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty-face ((,class (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6-face ((,class (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty-face ((,class (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low-face ((,class (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty-face ((,class (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1-face ((,class (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2-face ((,class (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3-face ((,class (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4-face ((,class (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5-face ((,class (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6-face ((,class (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low-face ((,class (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content-face ((,class (:inherit message-header-other))))
   `(gnus-header-from-face ((,class (:inherit message-header-from))))
   `(gnus-header-name-face ((,class (:inherit message-header-name))))
   `(gnus-header-newsgroups-face ((,class (:inherit message-header-other))))
   `(gnus-header-subject-face ((,class (:inherit message-header-subject))))
   `(gnus-summary-cancelled-face ((,class (:foreground ,orange))))
   `(gnus-summary-high-ancient-face ((,class (:foreground ,blue))))
   `(gnus-summary-high-read-face ((,class (:foreground ,green :weight bold))))
   `(gnus-summary-high-ticked-face ((,class (:foreground ,orange :weight bold))))
   `(gnus-summary-high-unread-face ((,class (:foreground ,qsimpleq-fg :weight bold))))
   `(gnus-summary-low-ancient-face ((,class (:foreground ,blue))))
   `(gnus-summary-low-read-face ((t (:foreground ,green))))
   `(gnus-summary-low-ticked-face ((,class (:foreground ,orange :weight bold))))
   `(gnus-summary-low-unread-face ((,class (:foreground ,qsimpleq-fg))))
   `(gnus-summary-normal-ancient-face ((,class (:foreground ,blue))))
   `(gnus-summary-normal-read-face ((,class (:foreground ,green))))
   `(gnus-summary-normal-ticked-face ((,class (:foreground ,orange :weight bold))))
   `(gnus-summary-normal-unread-face ((,class (:foreground ,qsimpleq-fg))))
   `(gnus-summary-selected-face ((,class (:foreground ,yellow :weight bold))))
   `(gnus-cite-1-face ((,class (:foreground ,blue))))
   `(gnus-cite-10-face ((,class (:foreground ,yellow))))
   `(gnus-cite-11-face ((,class (:foreground ,yellow))))
   `(gnus-cite-2-face ((,class (:foreground ,blue))))
   `(gnus-cite-3-face ((,class (:foreground ,blue))))
   `(gnus-cite-4-face ((,class (:foreground ,green))))
   `(gnus-cite-5-face ((,class (:foreground ,green))))
   `(gnus-cite-6-face ((,class (:foreground ,green))))
   `(gnus-cite-7-face ((,class (:foreground ,red))))
   `(gnus-cite-8-face ((,class (:foreground ,red))))
   `(gnus-cite-9-face ((,class (:foreground ,red))))
   `(gnus-group-news-1-empty-face ((,class (:foreground ,yellow))))
   `(gnus-group-news-2-empty-face ((,class (:foreground ,green))))
   `(gnus-group-news-3-empty-face ((,class (:foreground ,green))))
   `(gnus-group-news-4-empty-face ((,class (:foreground ,blue))))
   `(gnus-group-news-5-empty-face ((,class (:foreground ,blue))))
   `(gnus-group-news-6-empty-face ((,class (:foreground ,qsimpleq-bg))))
   `(gnus-group-news-low-empty-face ((,class (:foreground ,qsimpleq-bg))))
   `(gnus-signature-face ((,class (:foreground ,yellow))))
   `(gnus-x-face ((,class (:background ,qsimpleq-fg :foreground ,qsimpleq-bg))))

   ;; helm (these probably needs tweaking)
   `(helm-apt-deinstalled ((,class (:foreground ,qsimpleq-comments))))
   `(helm-apt-installed ((,class (:foreground ,green))))
   `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
   `(helm-bookmark-file ((,class (:foreground ,qsimpleq-fg))))
   `(helm-bookmark-gnus ((,class (:foreground ,cyan))))
   `(helm-bookmark-info ((,class (:foreground ,green))))
   `(helm-bookmark-man ((,class (:foreground ,violet))))
   `(helm-bookmark-w3m ((,class (:foreground ,yellow))))
   `(helm-bookmarks-su ((,class (:foreground ,orange))))
   `(helm-buffer-not-saved ((,class (:foreground ,orange))))
   `(helm-buffer-saved-out ((,class (:foreground ,red :background ,qsimpleq-bg
                                                 :inverse-video t))))
   `(helm-buffer-size ((,class (:foreground ,qsimpleq-comments))))
   `(helm-candidate-number ((,class (:background ,qsimpleq-hl :foreground ,qsimpleq-emph
                                                 :bold t))))
   `(helm-ff-directory ((,class (:background ,qsimpleq-bg  :foreground ,blue))))
   `(helm-ff-executable ((,class (:foreground ,green))))
   `(helm-ff-file ((,class (:background ,qsimpleq-bg :foreground ,qsimpleq-fg))))
   `(helm-ff-invalid-symlink ((,class (:background ,qsimpleq-bg :foreground ,orange
                                                   :slant italic))))
   `(helm-ff-prefix ((,class (:background ,yellow :foreground ,qsimpleq-bg))))
   `(helm-ff-symlink ((,class (:foreground ,cyan))))
   `(helm-grep-file ((,class (:foreground ,cyan :underline t))))
   `(helm-grep-finish ((,class (:foreground ,green))))
   `(helm-grep-lineno ((,class (:foreground ,orange))))
   `(helm-grep-match ((,class (:inherit match))))
   `(helm-grep-running ((,class (:foreground ,red))))
   `(helm-header ((,class (:inherit header-line))))
   `(helm-lisp-completion-info ((,class (:foreground ,qsimpleq-fg))))
   `(helm-lisp-show-completion ((,class (:foreground ,yellow  :background ,qsimpleq-hl
                                                     :bold t))))
   `(helm-M-x-key ((,class (:foreground ,orange :underline t))))
   `(helm-match ((,class (:inherit match))))
   `(helm-selection ((,class (:background ,qsimpleq-hl :underline t))))
   `(helm-selection-line ((,class (:background ,qsimpleq-hl :foreground ,qsimpleq-emph
                                               :underline nil))))
   `(helm-separator ((,class (:foreground ,red))))
   `(helm-source-header ((,class (:background ,blue-lc :foreground ,qsimpleq-bg
                                              :underline nil))))
   `(helm-time-zone-current ((,class (:foreground ,green))))
   `(helm-time-zone-home ((,class (:foreground ,red))))
   `(helm-visible-mark ((,class (:background ,qsimpleq-bg :foreground ,magenta :bold t))))

   ;; hi-lock-mode
   `(hi-yellow ((,class (:foreground ,yellow-lc :background ,yellow-hc))))
   `(hi-pink ((,class (:foreground ,magenta-lc :background ,magenta-hc))))
   `(hi-green ((,class (:foreground ,green-lc :background ,green-hc))))
   `(hi-blue ((,class (:foreground ,blue-lc :background ,blue-hc))))
   `(hi-black-b ((,class (:foreground ,qsimpleq-emph :background ,qsimpleq-bg :weight bold))))
   `(hi-blue-b ((,class (:foreground ,blue-lc :weight bold))))
   `(hi-green-b ((,class (:foreground ,green-lc :weight bold))))
   `(hi-red-b ((,class (:foreground ,red :weight bold))))
   `(hi-black-hb ((,class (:foreground ,qsimpleq-emph :background ,qsimpleq-bg :weight bold))))

   ;; highlight-changes
   `(highlight-changes ((,class (:foreground ,orange))))
   `(highlight-changes-delete ((,class (:foreground ,red :underline t))))

   ;; hl-line-mode
   `(hl-line ((,class (:background ,qsimpleq-hl))))
   `(hl-line-face ((,class (:background ,qsimpleq-hl))))

   ;; ido-mode
   `(ido-first-match ((,class (:foreground ,green :weight bold))))
   `(ido-only-match ((,class (:foreground ,qsimpleq-bg :background ,green :weight bold))))
   `(ido-subdir ((,class (:foreground ,blue :weight bold))))
   `(ido-incomplete-regexp ((,class (:foreground ,red :weight bold ))))
   `(ido-indicator ((,class (:background ,red :foreground ,qsimpleq-bg :width condensed))))
   `(ido-virtual ((,class (:foreground ,cyan))))

   ;; linum-mode
   `(linum ((,class (:foreground ,qsimpleq-fg :background ,qsimpleq-bg))))

   ;; magit
   `(magit-section-title ((,class (:foreground ,yellow :weight bold))))
   `(magit-branch ((,class (:foreground ,orange :weight bold))))
   `(magit-item-highlight ((,class (:background ,qsimpleq-hl))))
   `(magit-log-graph ((,class (:foreground ,qsimpleq-comments))))
   `(magit-log-head-label-bisect-bad ((,class (:background ,red-hc :foreground ,red-lc :box 1))))
   `(magit-log-head-label-bisect-good ((,class (:background ,green-hc :foreground ,green-lc
                                                            :box 1))))
   `(magit-log-head-label-default ((,class (:background ,qsimpleq-hl :box 1))))
   `(magit-log-head-label-local ((,class (:background ,blue-lc :foreground ,blue-hc :box 1))))
   `(magit-log-head-label-patches ((,class (:background ,red-lc :foreground ,red-hc :box 1))))
   `(magit-log-head-label-remote ((,class (:background ,green-lc :foreground ,green-hc :box 1))))
   `(magit-log-head-label-tags ((,class (:background ,yellow-lc :foreground ,yellow-hc :box 1))))
   `(magit-log-sha1 ((,class (:foreground ,yellow))))

   ;; message-mode
   `(message-cited-text ((,class (:foreground ,qsimpleq-comments))))
   `(message-header-name ((,class (:foreground ,green))))
   `(message-header-other ((,class (:foreground ,green))))
   `(message-header-to ((,class (:foreground ,yellow :weight bold))))
   `(message-header-cc ((,class (:foreground ,orange :weight bold))))
   `(message-header-newsgroups ((,class (:foreground ,yellow :weight bold))))
   `(message-header-subject ((,class (:foreground ,orange))))
   `(message-header-xheader ((,class (:foreground ,cyan))))
   `(message-mml ((,class (:foreground ,yellow :weight bold))))
   `(message-separator ((,class (:foreground ,qsimpleq-comments :slant italic))))

   ;; mew
   `(mew-face-header-subject ((,class (:foreground ,orange))))
   `(mew-face-header-from ((,class (:foreground ,yellow))))
   `(mew-face-header-date ((,class (:foreground ,green))))
   `(mew-face-header-to ((,class (:foreground ,red))))
   `(mew-face-header-key ((,class (:foreground ,green))))
   `(mew-face-header-private ((,class (:foreground ,green))))
   `(mew-face-header-important ((,class (:foreground ,blue))))
   `(mew-face-header-marginal ((,class (:foreground ,qsimpleq-fg :weight bold))))
   `(mew-face-header-warning ((,class (:foreground ,red))))
   `(mew-face-header-xmew ((,class (:foreground ,green))))
   `(mew-face-header-xmew-bad ((,class (:foreground ,red))))
   `(mew-face-body-url ((,class (:foreground ,orange))))
   `(mew-face-body-comment ((,class (:foreground ,qsimpleq-fg :slant italic))))
   `(mew-face-body-cite1 ((,class (:foreground ,green))))
   `(mew-face-body-cite2 ((,class (:foreground ,blue))))
   `(mew-face-body-cite3 ((,class (:foreground ,orange))))
   `(mew-face-body-cite4 ((,class (:foreground ,yellow))))
   `(mew-face-body-cite5 ((,class (:foreground ,red))))
   `(mew-face-mark-review ((,class (:foreground ,blue))))
   `(mew-face-mark-escape ((,class (:foreground ,green))))
   `(mew-face-mark-delete ((,class (:foreground ,red))))
   `(mew-face-mark-unlink ((,class (:foreground ,yellow))))
   `(mew-face-mark-refile ((,class (:foreground ,green))))
   `(mew-face-mark-unread ((,class (:foreground ,red))))
   `(mew-face-eof-message ((,class (:foreground ,green))))
   `(mew-face-eof-part ((,class (:foreground ,yellow))))

   ;; mingus
   `(mingus-directory-face ((,class (:foreground ,blue))))
   `(mingus-pausing-face ((,class (:foreground ,magenta))))
   `(mingus-playing-face ((,class (:foreground ,cyan))))
   `(mingus-playlist-face ((,class (:foreground ,cyan ))))
   `(mingus-song-file-face ((,class (:foreground ,yellow))))
   `(mingus-stopped-face ((,class (:foreground ,red))))

   ;; moccur
   `(moccur-current-line-face ((,class (:underline t))))
   `(moccur-edit-done-face ((,class
                             (:foreground ,qsimpleq-comments
                                          :background ,qsimpleq-bg
                                          :slant italic))))
   `(moccur-edit-face
     ((,class (:background ,yellow :foreground ,qsimpleq-bg))))
   `(moccur-edit-file-face ((,class (:background ,qsimpleq-hl))))
   `(moccur-edit-reject-face ((,class (:foreground ,red))))
   `(moccur-face ((,class (:background ,qsimpleq-hl :foreground ,qsimpleq-emph
                                       :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face ((,class (:foreground ,green :slant italic :weight normal))))
   `(mu4e-cited-2-face ((,class (:foreground ,blue :slant italic :weight normal))))
   `(mu4e-cited-3-face ((,class (:foreground ,orange :slant italic :weight normal))))
   `(mu4e-cited-4-face ((,class (:foreground ,yellow :slant italic :weight normal))))
   `(mu4e-cited-5-face ((,class (:foreground ,cyan :slant italic :weight normal))))
   `(mu4e-cited-6-face ((,class (:foreground ,green :slant italic :weight normal))))
   `(mu4e-cited-7-face ((,class (:foreground ,blue :slant italic :weight normal))))
   `(mu4e-flagged-face ((,class (:foreground ,magenta :weight bold))))
   `(mu4e-view-url-number-face ((,class (:foreground ,orange :weight bold))))
   `(mu4e-warning-face ((,class (:foreground ,red :slant normal :weight bold))))

   ;; mumamo
   `(mumamo-background-chunk-submode1 ((,class (:background ,qsimpleq-hl))))

   ;; nav
   `(nav-face-heading ((,class (:foreground ,yellow))))
   `(nav-face-button-num ((,class (:foreground ,cyan))))
   `(nav-face-dir ((,class (:foreground ,green))))
   `(nav-face-hdir ((,class (:foreground ,red))))
   `(nav-face-file ((,class (:foreground ,qsimpleq-fg))))
   `(nav-face-hfile ((,class (:foreground ,red))))

   ;; nav-flash
   `(nav-flash-face ((,class (:foreground ,orange :background ,qsimpleq-hl))))

   ;; org-mode
   `(org-agenda-structure
     ((,class (:inherit font-lock-comment-face :foreground ,magenta :inverse-video t))))
   `(org-agenda-date
     ((,class (:foreground ,qsimpleq-fg :background ,qsimpleq-hl :weight bold
                           :box (:line-width 4 :color ,qsimpleq-hl) ))) t)
   `(org-agenda-date-weekend ((,class (:inherit org-agenda-date :slant italic))) t)
   `(org-agenda-date-today
     ((,class (:inherit org-agenda-date :slant italic underline: t))) t)
   `(org-agenda-done ((,class (:foreground ,green))) t)
   `(org-archived ((,class (:foreground ,qsimpleq-comments :weight normal))))
   `(org-block ((,class (:foreground ,qsimpleq-comments))))
   `(org-block-begin-line ((,class (:foreground ,qsimpleq-comments :slant italic))))
   `(org-checkbox ((,class (:background ,qsimpleq-bg :foreground ,qsimpleq-fg
                                        :box (:line-width 1 :style released-button)))))
   `(org-code ((,class (:foreground ,qsimpleq-comments))))
   `(org-date ((,class (:foreground ,blue :underline t))))
   `(org-done ((,class (:weight bold :foreground ,green))))
   `(org-ellipsis ((,class (:foreground ,qsimpleq-comments))))
   `(org-formula ((,class (:foreground ,yellow))))
   `(org-headline-done ((,class (:foreground ,green))))
   `(org-hide ((,class (:foreground ,qsimpleq-bg))))
   `(org-level-1 ((,class (:foreground ,orange))))
   `(org-level-2 ((,class (:foreground ,green))))
   `(org-level-3 ((,class (:foreground ,blue))))
   `(org-level-4 ((,class (:foreground ,yellow))))
   `(org-level-5 ((,class (:foreground ,cyan))))
   `(org-level-6 ((,class (:foreground ,green))))
   `(org-level-7 ((,class (:foreground ,red))))
   `(org-level-8 ((,class (:foreground ,blue))))
   `(org-link ((,class (:foreground ,yellow :underline t))))
   `(org-sexp-date ((,class (:foreground ,violet))))
   `(org-scheduled ((,class (:foreground ,green))))
   `(org-scheduled-previously ((,class (:foreground ,yellow))))
   `(org-scheduled-today ((,class (:foreground ,blue :weight bold))))
   `(org-special-keyword ((,class (:foreground ,qsimpleq-comments :weight bold))))
   `(org-table ((,class (:foreground ,green))))
   `(org-tag ((,class (:weight bold))))
   `(org-time-grid ((,class (:foreground ,cyan))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-upcoming-deadline ((,class (:foreground ,yellow ))))
   `(org-warning ((,class (:foreground ,orange :weight bold :underline t))))
   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face ((,class (:background ,blue-lc :foreground ,blue-hc))))
   `(org-habit-clear-future-face ((,class (:background ,blue-lc))))
   `(org-habit-ready-face ((,class (:background ,green-lc :foreground ,green))))
   `(org-habit-ready-future-face ((,class (:background ,green-lc))))
   `(org-habit-alert-face ((,class (:background ,yellow :foreground ,yellow-lc))))
   `(org-habit-alert-future-face ((,class (:background ,yellow-lc))))
   `(org-habit-overdue-face ((,class (:background ,red :foreground ,red-lc))))
   `(org-habit-overdue-future-face ((,class (:background ,red-lc))))
   ;; latest additions
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,qsimpleq-comments))))
   `(org-agenda-restriction-lock ((,class (:background ,yellow))))
   `(org-clock-overlay ((,class (:background ,yellow))))
   `(org-column ((,class (:background ,qsimpleq-hl :strike-through nil
                                      :underline nil :slant normal :weight normal))))
   `(org-column-title ((,class (:background ,qsimpleq-hl :underline t :weight bold))))
   `(org-date-selected ((,class (:foreground ,red :inverse-video t))))
   `(org-document-info ((,class (:foreground ,qsimpleq-fg))))
   `(org-document-title ((,class (:foreground ,qsimpleq-emph  :weight bold :height 1.44))))
   `(org-drawer ((,class (:foreground ,cyan))))
   `(org-footnote ((,class (:foreground ,magenta :underline t))))
   `(org-latex-and-export-specials ((,class (:foreground ,orange))))
   `(org-mode-line-clock-overrun ((,class (:inherit modeline :background ,red))))

   ;; outline
   `(outline-8 ((,class (:inherit default))))
   `(outline-7 ((,class (:inherit outline-8 :height 1.0))))
   `(outline-6 ((,class (:inherit outline-7 :height 1.0))))
   `(outline-5 ((,class (:inherit outline-6 :height 1.0))))
   `(outline-4 ((,class (:inherit outline-5 :height 1.0))))
   `(outline-3 ((,class (:inherit outline-4 :height 1.0))))
   `(outline-2 ((,class (:inherit outline-3 :height 1.0))))
   `(outline-1 ((,class (:inherit outline-2 :height 1.0))))

   ;; pretty-mode
   `(pretty-mode-symbol-face  ((,class (:foreground ,green))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,cyan :weight normal))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,yellow :weight normal))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,violet :weight normal))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,orange :weight normal))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,green :weight normal))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,yellow :weight normal))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,violet :weight normal))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,orange :weight normal))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,green :weight normal))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,yellow :weight normal))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,violet :weight normal))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,orange :weight normal))))
   `(rainbow-delimiters-unmatched-face
     ((,class (:foreground ,qsimpleq-fg :background ,qsimpleq-bg :inverse-video t))))

   ;; rst-mode
   `(rst-level-1-face ((,class (:background ,yellow   :foreground ,qsimpleq-bg))))
   `(rst-level-2-face ((,class (:background ,cyan    :foreground ,qsimpleq-bg))))
   `(rst-level-3-face ((,class (:background ,blue    :foreground ,qsimpleq-bg))))
   `(rst-level-4-face ((,class (:background ,violet  :foreground ,qsimpleq-bg))))
   `(rst-level-5-face ((,class (:background ,magenta :foreground ,qsimpleq-bg))))
   `(rst-level-6-face ((,class (:background ,red     :foreground ,qsimpleq-bg))))

   ;; rpm-mode
   `(rpm-spec-dir-face ((,class (:foreground ,green))))
   `(rpm-spec-doc-face ((,class (:foreground ,green))))
   `(rpm-spec-ghost-face ((,class (:foreground ,red))))
   `(rpm-spec-macro-face ((,class (:foreground ,yellow))))
   `(rpm-spec-obsolete-tag-face ((,class (:foreground ,red))))
   `(rpm-spec-package-face ((,class (:foreground ,red))))
   `(rpm-spec-section-face ((,class (:foreground ,yellow))))
   `(rpm-spec-tag-face ((,class (:foreground ,blue))))
   `(rpm-spec-var-face ((,class (:foreground ,red))))

   ;; sh-mode
   `(sh-quoted-exec ((,class (:foreground ,violet :weight bold))))
   `(sh-escaped-newline ((,class (:foreground ,yellow :weight bold))))
   `(sh-heredoc ((,class (:foreground ,yellow :weight bold))))

   ;; show-paren
   `(show-paren-match
     ((,class (:foreground ,cyan :background ,qsimpleq-bg :weight normal :inverse-video t))))
   `(show-paren-mismatch
     ((,class (:foreground ,red :background ,qsimpleq-bg :weight normal :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,class (:foreground ,cyan :background ,qsimpleq-bg :weight normal :inverse-video t))))
   `(paren-face-mismatch
     ((,class (:foreground ,red :background ,qsimpleq-bg :weight normal :inverse-video t))))
   `(paren-face-no-match
     ((,class (:foreground ,red :background ,qsimpleq-bg :weight normal :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face ((,class (:foreground ,red))))

   ;; speedbar
   `(speedbar-button-face ((,class (:inherit variable-pitch :foreground ,qsimpleq-comments))))
   `(speedbar-directory-face ((,class (:inherit variable-pitch :foreground ,blue))))
   `(speedbar-file-face ((,class (:inherit variable-pitch :foreground ,qsimpleq-fg))))
   `(speedbar-highlight-face ((,class (:inherit variable-pitch :background ,qsimpleq-hl))))
   `(speedbar-selected-face ((,class (:inherit variable-pitch :foreground ,yellow :underline t))))
   `(speedbar-separator-face ((,class (:inherit variable-pitch
                                                :background ,blue :foreground ,qsimpleq-bg
                                                :overline ,cyan-lc))))
   `(speedbar-tag-face ((,class (:inherit variable-pitch :foreground ,green))))

   ;; sunrise commander headings
   `(sr-active-path-face ((,class (:background ,blue :foreground ,qsimpleq-bg
                                               :height 100  :weight bold))))
   `(sr-editing-path-face ((,class (:background ,yellow :foreground ,qsimpleq-bg
                                                :weight bold :height 100))))
   `(sr-highlight-path-face ((,class (:background ,green :foreground ,qsimpleq-bg
                                                  :weight bold :height 100))))
   `(sr-passive-path-face ((,class (:background ,qsimpleq-comments :foreground ,qsimpleq-bg
                                                :weight bold :height 100))))
   ;; sunrise commander marked
   `(sr-marked-dir-face ((,class (:inherit dired-marked))))
   `(sr-marked-file-face ((,class (:inherit dired-marked))))
   `(sr-alt-marked-dir-face ((,class (:background ,magenta :foreground ,qsimpleq-bg
                                                  :weight bold))))
   `(sr-alt-marked-file-face ((,class (:background ,magenta :foreground ,qsimpleq-bg
                                                   :weight bold))))
   ;; sunrise commander fstat
   `(sr-directory-face ((,class (:inherit dired-directory :weight normal))))
   `(sr-symlink-directory-face ((,class (:inherit dired-directory :slant italic :weight normal))))
   `(sr-symlink-face ((,class (:inherit dired-symlink :slant italic :weight normal))))
   `(sr-broken-link-face ((,class (:inherit dired-warning :slant italic :weight normal))))
   ;; sunrise commander file types
   `(sr-compressed-face ((,class (:foreground ,qsimpleq-fg))))
   `(sr-encrypted-face ((,class (:foreground ,qsimpleq-fg))))
   `(sr-log-face ((,class (:foreground ,qsimpleq-fg))))
   `(sr-packaged-face ((,class (:foreground ,qsimpleq-fg))))
   `(sr-html-face ((,class (:foreground ,qsimpleq-fg))))
   `(sr-xml-face ((,class (:foreground ,qsimpleq-fg))))
   ;; sunrise commander misc
   `(sr-clex-hotchar-face ((,class (:background ,red  :foreground ,qsimpleq-bg :weight bold))))

   ;; table
   `(table-cell ((,class (:foreground ,qsimpleq-fg :background ,qsimpleq-hl))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip ((,class (:background ,yellow-lc :foreground ,yellow-hc
                                   :inherit variable-pitch))))

   ;; tuareg
   `(tuareg-font-lock-governing-face ((,class (:foreground ,magenta :weight bold))))
   `(tuareg-font-lock-multistage-face ((,class (:foreground ,blue :background ,qsimpleq-hl :weight bold))))
   `(tuareg-font-lock-operator-face ((,class (:foreground ,qsimpleq-emph))))
   `(tuareg-font-lock-error-face ((,class (:foreground ,yellow :background ,red :weight bold :underline t))))
   `(tuareg-font-lock-interactive-output-face ((,class (:foreground ,cyan))))
   `(tuareg-font-lock-interactive-error-face ((,class (:foreground ,red :weight bold :underline t))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,class (:foreground ,qsimpleq-comments :background ,qsimpleq-bg))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,cyan :inverse-video t))))
   `(undo-tree-visualizer-active-branch-face
     ((,class (:foreground ,qsimpleq-emph :background ,qsimpleq-bg :weight bold))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

   ;; volatile highlights
   `(vhl/default-face ((,class (:background ,green-lc :foreground ,green-hc))))

   ;; w3m
   `(w3m-anchor ((,class (:inherit link))))
   `(w3m-arrived-anchor ((,class (:inherit link-visited))))
   `(w3m-form ((,class (:background ,base03 :foreground ,qsimpleq-fg))))
   `(w3m-header-line-location-title ((,class (:background ,base02 :foreground ,yellow))))
   `(w3m-header-line-location-content ((,class (:background ,base02 :foreground ,qsimpleq-fg))))
   `(w3m-bold ((,class (:foreground ,qsimpleq-emph :weight bold))))
   `(w3m-image-anchor ((,class (:background ,qsimpleq-bg :foreground ,cyan :inherit link))))
   `(w3m-image ((,class (:background ,qsimpleq-bg :foreground ,cyan))))
   `(w3m-lnum-minibuffer-prompt ((,class (:foreground ,qsimpleq-emph))))
   `(w3m-lnum-match ((,class (:background ,qsimpleq-hl))))
   `(w3m-lnum ((,class (:underline nil :bold nil :foreground ,red))))

   ;; whitespace-mode
   `(whitespace-space ((,class (:background ,qsimpleq-bg :foreground ,yellow-lc
                                            :inverse-video t))))
   `(whitespace-hspace ((,class (:background ,qsimpleq-bg :foreground ,red-lc
                                             :inverse-video t))))
   `(whitespace-tab ((,class (:background ,qsimpleq-bg :foreground ,orange-lc
                                          :inverse-video t))))
   `(whitespace-newline ((,class (:foreground ,qsimpleq-comments))))
   `(whitespace-trailing ((,class (:foreground ,blue-lc :background ,qsimpleq-bg
                                               :inverse-video t))))
                                        ; removing inverse video on this
   `(whitespace-line ((,class (:background ,qsimpleq-bg :foreground ,magenta
                                           :inverse-video nil))))
   `(whitespace-space-before-tab ((,class (:background ,qsimpleq-bg :foreground ,green-lc
                                                       :inverse-video t))))
   `(whitespace-indentation ((,class (:background ,qsimpleq-bg :foreground ,magenta-lc
                                                  :inverse-video t))))
   `(whitespace-empty ((,class (:background ,qsimpleq-fg :foreground ,red-lc :inverse-video t))))
   `(whitespace-space-after-tab ((,class (:background ,qsimpleq-bg  :foreground ,violet-lc
                                                      :inverse-video t))))

   ;; wanderlust
   `(wl-highlight-folder-few-face ((,class (:foreground ,red))))
   `(wl-highlight-folder-many-face ((,class (:foreground ,red))))
   `(wl-highlight-folder-path-face ((,class (:foreground ,orange))))
   `(wl-highlight-folder-unread-face ((,class (:foreground ,blue))))
   `(wl-highlight-folder-zero-face ((,class (:foreground ,qsimpleq-fg))))
   `(wl-highlight-folder-unknown-face ((,class (:foreground ,blue))))
   `(wl-highlight-message-citation-header ((,class (:foreground ,red))))
   `(wl-highlight-message-cited-text-1 ((,class (:foreground ,red))))
   `(wl-highlight-message-cited-text-2 ((,class (:foreground ,green))))
   `(wl-highlight-message-cited-text-3 ((,class (:foreground ,blue))))
   `(wl-highlight-message-cited-text-4 ((,class (:foreground ,blue))))
   `(wl-highlight-message-header-contents-face ((,class (:foreground ,green))))
   `(wl-highlight-message-headers-face ((,class (:foreground ,red))))
   `(wl-highlight-message-important-header-contents ((,class (:foreground ,green))))
   `(wl-highlight-message-header-contents ((,class (:foreground ,green))))
   `(wl-highlight-message-important-header-contents2 ((,class (:foreground ,green))))
   `(wl-highlight-message-signature ((,class (:foreground ,green))))
   `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,qsimpleq-fg))))
   `(wl-highlight-summary-answered-face ((,class (:foreground ,blue))))
   `(wl-highlight-summary-disposed-face ((,class (:foreground ,qsimpleq-fg
                                                              :slant italic))))
   `(wl-highlight-summary-new-face ((,class (:foreground ,blue))))
   `(wl-highlight-summary-normal-face ((,class (:foreground ,qsimpleq-fg))))
   `(wl-highlight-summary-thread-top-face ((,class (:foreground ,yellow))))
   `(wl-highlight-thread-indent-face ((,class (:foreground ,magenta))))
   `(wl-highlight-summary-refiled-face ((,class (:foreground ,qsimpleq-fg))))
   `(wl-highlight-summary-displaying-face ((,class (:underline t :weight bold))))

   ;; which-func-mode
   `(which-func ((,class (:foreground ,green))))

   ;; window-number-mode
   `(window-number-face ((,class (:foreground ,green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,class (:foreground ,qsimpleq-comments :background ,qsimpleq-comments))))
   `(yascroll:thumb-fringe
     ((,class (:foreground ,qsimpleq-comments :background ,qsimpleq-comments))))

   ;; zencoding
   `(zencoding-preview-input ((,class (:background ,qsimpleq-hl :box ,qsimpleq-emph))))

   ;; zjl-hl
   `(zjl-hl-elisp-function-call-face ((,class (:foreground nil :inherit font-lock-function-name-face :weight normal))))
   `(zjl-hl-elisp-number-face ((,class (:foreground nil :inherit font-lock-constant-face :weight normal))))
   `(zjl-hl-elisp-setq-face ((,class (:foreground nil :inherit font-lock-function-name-face :weight normal))))
   `(zjl-hl-function-call-face ((,class (:foreground nil :inherit zjl-hl-elisp-function-call-face :weight normal))))
   `(zjl-hl-font-lock-bracket-face ((,class (:foreground nil :inherit rainbow-delimiters-depth-1-face))))
   `(zjl-hl-number-face ((,class (:foreground nil :inherit zjl-hl-elisp-number-face)))))

  (custom-theme-set-variables
   'qsimpleq
   `(ansi-color-names-vector [,qsimpleq-bg ,red ,green ,yellow
                                           ,blue ,magenta ,cyan ,qsimpleq-fg])
   `(ansi-term-color-vector [unspecific ,base01 ,red ,green ,yellow ,blue ,magenta ,cyan ,base03])
   ;; fill-column-indicator
   `(fci-rule-color ,qsimpleq-hl)

   ;; highlight-changes
   `(highlight-changes-colors '(,magenta ,violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,qsimpleq-hl . 0)(,green-lc . 20)(,cyan-lc . 30)(,blue-lc . 50)
       (,yellow-lc . 60)(,orange-lc . 70)(,magenta-lc . 85)(,qsimpleq-hl . 100)))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'qsimpleq)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; qsimpleq-theme.el ends here.
