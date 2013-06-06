;;; soothe-theme.el --- a dark colorful theme for Emacs24.
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;; Url: https://github.com/jasonm23/emacs-soothe-theme
;; Version: 20130410.1004
;;; X-Original-Version: 0.3.17
;;; Package-Requires: ((emacs "24.1"))
;;;
;;; Change Log:
;;; 0.3.17 : Added support for which-func face (milkypostman)
;;; 0.3.16 : Fixed flymake colors; also added Package Requres Emacs 24 by Steve Purcell
;;; 0.3.15 : update by Sabof; Magit diff settings migrated over to diff
;;;        : (magit will inherit from diff)
;;; 0.3.14 : Changed main-line colors and style, added powerline colors
;;; 0.3.13 : Different colors for hl-line and region
;;; 0.3.12 : Fixed issue #4 - region bg contrast too low.
;;; 0.3.11 : Added rainbow-mode switch for editing
;;; 0.3.10 : Themed Mode line emphasis
;;; 0.3.9  : Slightly improved dired support
;;; 0.3.8  : Added support for isearch
;;;        : Additional support for mode-line
;;;        : modified main-line colors
;;;        : modified link / link-visited
;;;        : added tooltip face
;;; 0.3.7  : extended magit support
;;;        : added iedit support
;;;        : added CUA support
;;;        : added AutoComplete support
;;;        : changed rainbow delimiters for better visibility
;;; 0.3.6  : beginning work on additional mode support
;;;        : added git-gutter
;;;        : added dropdown-list
;;;        : added stubs/extention lists for gnus, rcirc, message
;;;        :       ido + magit (more),
;;;        :       ac, compilation,
;;;        :       dired, diff, iedit,
;;;        :       cua, change, Man, woman,
;;;        :       commint, info, custom, popup
;;;        :       widget, whitespace, w3m, more...
;;; 0.3.5  : bugfix
;;; 0.3.4  : added support for main-line and flymake
;;; 0.3.0  : cleaned up for elpa
;;; 0.1.0  : initial version
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, version 3 of the License.
;;;
;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs.
;;;
;;; This file is not a part of Emacs
;;;
;;; Commentary: An amalgam of muted color tones and highlighted
;;;   backgrounds.  Builtin support for rainbow-delimiters, org-mode,
;;;   whitespace-mode, ECB, flyspell, ido, linum, highlight
;;;   indentation, show-paren-mode, further mode support to come.

(unless (>= 24 emacs-major-version)
  (error "soothe-theme requires Emacs 24 or later."))

(deftheme soothe
  "soothe-theme An amalgam of muted color tones and highlighted
   backgrounds.  Has builtin support for rainbow-delimiters,
   org-mode, whitespace-mode, ECB, flyspell, ido, linum,
   highlight indentation, show-paren-mode, further mode support
   to come.")

(let  (
       ;; Palette
       (foam             "#E0E4CC")
       (snow-code        "#ECE5CE")
       (crem             "#F4EAD5")
       (dirty-crem       "#DBD2BF")
       (dirty-crem-bg    "#2B2A26")
       (gray-1           "#aaaaaa")
       (gray-2           "#828282")
       (gray-3           "#333333")
       (gray-4           "#2a2a2a")
       (gray-5           "#252525")
       (gray-6           "#202020")
       (gray-1bg         "#0a0a0a")
       (gray-2bg         "#111111")
       (gray-3bg         "#141414")
       (gray-4bg         "#171717")
       (gray-5bg         "#1a1a1a")
       (gray-6bg         "#1e1e1e")
       (red-1            "#b13120")
       (red-2            "#a23f1e")
       (red-3            "#AA1100")
       (red-4            "#660000")
       (red-1bg          "#1D1515")
       (red-2bg          "#251c1e")
       (brown-1          "#8f621d")
       (brown-1bg        "#2a1f1f")
       (orange-1         "#d94a05")
       (orange-2         "#FF5211")
       (orange-1bg       "#1F1710")
       (yellow-1         "#ceae3e")
       (yellow-1bg       "#18140C")
       (green-1          "#719f34")
       (green-2          "#3e8f75")
       (green-3          "#839F5E")
       (green-1bg        "#1a2321")
       (green-2bg        "#1a2321")
       (turquoise-1      "#01535F")
       (turquoise-2      "#073E46")
       (turquoise-1bg    "#04181C")
       (turquoise-2bg    "#031316")
       (blue-1           "#7c9fc9")
       (blue-2           "#317598")
       (blue-3           "#009090")
       (blue-4           "#364e7a")
       (blue-1bg         "#1e252f")
       (blue-2bg         "#1b333e")
       (blue-3bg         "#132228")
       (blue-4bg         "#172028")
       (purple-1         "#7868b5")
       (purple-2         "#8A7FB5")
       (purple-3         "#483E6C")
       (purple-4         "#342B58")
       (purple-1bg       "#1D1B25")
       (purple-2bg       "#302948")
       (purple-3bg       "#241F36")
       (foreground       "#F4EAD5")
       (hl-line          "#11252a")
       (selection        "#11253a")
       (background       "#110F13")
       (background-dark  "#0F0D11")
       (alt-background   "#111013"))

  ;; Terminal colors - set background to black.
  ;; TODO: Use defface method (class color) (min-colors ...)

  (unless (window-system)
    (setq background      "#000000")
    (setq background-dark "#000000")
    (setq alt-background  "#000000")
    (setq dirty-crem-bg   "#000000")
    (setq gray-1bg        "#000000")
    (setq gray-2bg        "#000000")
    (setq gray-3bg        "#000000")
    (setq gray-4bg        "#000000")
    (setq gray-5bg        "#000000")
    (setq gray-6bg        "#000000")
    (setq red-1bg         "#000000")
    (setq red-2bg         "#000000")
    (setq brown-1bg       "#000000")
    (setq orange-1bg      "#000000")
    (setq yellow-1bg      "#000000")
    (setq green-1bg       "#000000")
    (setq green-2bg       "#000000")
    (setq turquoise-1bg   "#000000")
    (setq turquoise-2bg   "#000000")
    (setq blue-1bg        "#000000")
    (setq blue-2bg        "#000000")
    (setq blue-3bg        "#000000")
    (setq blue-4bg        "#000000")
    (setq purple-1bg      "#000000")
    (setq purple-2bg      "#000000")
    (setq purple-3bg      "#000000")
    )

  (custom-theme-set-faces
   'soothe

   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Basics
   `(default                                   ((t (:foreground ,foreground  :background ,background                    ))))
   `(cursor                                    ((t (                         :background ,orange-1                      ))))
   `(region                                    ((t (:foreground nil          :background ,selection                     ))))
   `(highlight                                 ((t (:foreground ,blue-3      :background ,blue-3bg                      ))))
   `(hl-line                                   ((t (                         :background ,hl-line                       ))))
   `(minibuffer-prompt                         ((t (:foreground ,orange-1    :background ,orange-1bg                    ))))
   `(escape-glyph                              ((t (:foreground ,red-1       :background ,purple-1bg                    ))))
   `(error                                     ((t (:foreground ,red-1       :background ,red-1bg                       ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Font-lock stuff
   `(font-lock-builtin-face                    ((t (:foreground ,red-2       :background ,red-1bg                       ))))
   `(font-lock-constant-face                   ((t (:foreground ,purple-1    :background ,purple-1bg                    ))))
   `(font-lock-comment-face                    ((t (:foreground ,turquoise-2 :background ,alt-background :italic t      ))))
   `(font-lock-comment-delimiter-face          ((t (:foreground ,turquoise-1 :background ,alt-background :italic t      ))))
   `(font-lock-doc-face                        ((t (:foreground ,blue-3      :background ,gray-1bg                      ))))
   `(font-lock-doc-string-face                 ((t (:foreground ,blue-3      :background ,gray-1bg                      ))))
   `(font-lock-function-name-face              ((t (:foreground ,red-1       :background ,red-1bg                       ))))
   `(font-lock-keyword-face                    ((t (:foreground ,orange-1    :background ,orange-1bg                    ))))
   `(font-lock-negation-char-face              ((t (:foreground ,yellow-1    :background ,yellow-1bg                    ))))
   `(font-lock-preprocessor-face               ((t (:foreground ,orange-1    :background ,orange-1bg                    ))))
   `(font-lock-string-face                     ((t (:foreground ,blue-3      :background ,turquoise-2bg                 ))))
   `(font-lock-type-face                       ((t (:foreground ,red-2       :background ,red-1bg        :bold nil      ))))
   `(font-lock-variable-name-face              ((t (:foreground ,blue-1      :background ,blue-1bg                      ))))
   `(font-lock-warning-face                    ((t (:foreground ,red-2       :background ,red-2bg                       ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; UI related
   `(link                                      ((t (:foreground ,blue-1      :background ,blue-1bg                      ))))
   `(link-visited                              ((t (:foreground ,blue-3      :background ,blue-4bg                      ))))
   `(fringe                                    ((t (                         :background ,gray-3bg                      ))))
   `(vertical-border                           ((t (:foreground ,gray-4      :background ,background                    ))))
   `(mode-line                                 ((t (:foreground ,gray-2      :background ,gray-3bg  :box nil :height 85 ))))
   `(mode-line-inactive                        ((t (:foreground ,gray-5      :background ,gray-2bg  :inherit mode-line  ))))
   `(mode-line-highlight                       ((t (:foreground ,red-1))))
   `(mode-line-buffer-id                       ((t (:foreground ,orange-1))))
   `(mode-line-emphasis                        ((t (:bold))))
   `(which-func                                ((t (:foreground ,blue-1))))

   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; isearch
   `(isearch                                   ((t (:foreground ,foam        :background ,purple-3                      ))))
   `(isearch-fail                              ((t (:foreground ,foam        :background ,red-4                         ))))
   `(lazy-highlight                            ((t (:foreground ,purple-1    :background ,green-2bg                     ))))

   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Compilation mode
   ;; `(compilation-column-number              ((t (:foreground ,foam        :background ,background                    ))))
   ;; `(compilation-error                      ((t (:foreground ,foam        :background ,background                    ))))
   ;; `(compilation-info                       ((t (:foreground ,foam        :background ,background                    ))))
   ;; `(compilation-line-number                ((t (:foreground ,foam        :background ,background                    ))))
   ;; `(compilation-mode-line-exit             ((t (:foreground ,foam        :background ,background                    ))))
   ;; `(compilation-mode-line-fail             ((t (:foreground ,foam        :background ,background                    ))))
   ;; `(compilation-mode-line-run              ((t (:foreground ,foam        :background ,background                    ))))
   ;; `(compilation-warning                    ((t (:foreground ,foam        :background ,background                    ))))
   ;; `(completions-annotations                ((t (:foreground ,foam        :background ,background                    ))))
   ;; `(completions-common-part                ((t (:foreground ,foam        :background ,background                    ))))
   ;; `(completions-first-difference           ((t (:foreground ,foam        :background ,background                    ))))

   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Auto Complete
   ;;
   `(ac-selection-face                         ((t (:foreground ,dirty-crem  :background ,dirty-crem-bg                 ))))
   `(ac-candidate-face                         ((t (:foreground ,background  :background ,dirty-crem                    ))))
   `(ac-yasnippet-candidate-face               ((t (:foreground ,background  :background ,green-2                       ))))
   `(ac-yasnippet-selection-face               ((t (:foreground ,foam        :background ,dirty-crem-bg                 ))))
   `(ac-gtags-candidate-face                   ((t (:foreground ,background  :background ,purple-3                      ))))
   `(ac-gtags-selection-face                   ((t (:foreground ,dirty-crem  :background ,dirty-crem-bg                 ))))
   `(ac-candidate-mouse-face                   ((t (:foreground ,foam        :background ,turquoise-1                   ))))
   `(ac-completion-face                        ((t (:foreground ,snow-code   :background ,purple-3bg :underline t       ))))
   ;; used by AC
   `(popup-tip-face                            ((t (:foreground ,dirty-crem  :background ,dirty-crem-bg                 ))))
   `(tooltip                                   ((t (:foreground ,dirty-crem-bg  :background ,dirty-crem :height 110 ))))

   ;;; popup-face
   ;;; popup-isearch-match
   ;;; popup-menu-face
   ;;; popup-menu-mouse-face
   ;;; popup-menu-selection-face
   ;;; popup-menu-summary-face
   ;;; popup-scroll-bar-background-face
   ;;; popup-scroll-bar-foreground-face
   ;;; popup-summary-face
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Dired
   `(dired-directory                           ((t (:foreground ,purple-1    :background ,purple-1bg                    ))))
   `(dired-flagged                             ((t (:foreground ,red-1       :background ,orange-1bg                    ))))
   `(dired-header                              ((t (:foreground ,orange-1    :background ,background                    ))))
   `(dired-ignored                             ((t (:foreground ,turquoise-1 :background ,background                    ))))
   `(dired-mark                                ((t (:foreground ,orange-2    :background ,background                    ))))
   `(dired-marked                              ((t (:foreground ,green-3     :background ,orange-1bg                    ))))
   `(dired-perm-write                          ((t (:foreground ,foam        :background ,background                    ))))
   `(dired-symlink                             ((t (:foreground ,blue-1      :background ,blue-4bg                      ))))
   `(dired-warning                             ((t (:foreground ,red-1       :background ,red-2bg                       ))))

   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Gnus

   ;; gnus-group-mail-1
   ;; gnus-group-mail-1-empty
   ;; gnus-group-mail-2
   ;; gnus-group-mail-2-empty
   ;; gnus-group-mail-3
   ;; gnus-group-mail-3-empty
   ;; gnus-group-mail-low
   ;; gnus-group-mail-low-empty
   ;; gnus-group-news-1
   ;; gnus-group-news-1-empty
   ;; gnus-group-news-2
   ;; gnus-group-news-2-empty
   ;; gnus-group-news-3
   ;; gnus-group-news-3-empty
   ;; gnus-group-news-4
   ;; gnus-group-news-4-empty
   ;; gnus-group-news-5
   ;; gnus-group-news-5-empty
   ;; gnus-group-news-6
   ;; gnus-group-news-6-empty
   ;; gnus-group-news-low
   ;; gnus-group-news-low-empty
   ;; gnus-splash
   ;; gnus-summary-cancelled
   ;; gnus-summary-high-ancient
   ;; gnus-summary-high-read
   ;; gnus-summary-high-ticked
   ;; gnus-summary-high-undownloaded
   ;; gnus-summary-high-unread
   ;; gnus-summary-low-ancient
   ;; gnus-summary-low-read
   ;; gnus-summary-low-ticked
   ;; gnus-summary-low-undownloaded
   ;; gnus-summary-low-unread
   ;; gnus-summary-normal-ancient
   ;; gnus-summary-normal-read
   ;; gnus-summary-normal-ticked
   ;; gnus-summary-normal-undownloaded
   ;; gnus-summary-normal-unread
   ;; gnus-summary-selected

   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Diff
   `(diff-added                                ((t (:foreground ,green-3     :background ,green-2bg                     ))))
   `(diff-removed                              ((t (:foreground ,red-1       :background ,red-1bg                       ))))
   `(diff-file-header                          ((t (:foreground ,orange-1    :background ,orange-1bg                    ))))
   `(diff-context                              ((t (:foreground ,foam                                                   ))))
   `(diff-hunk-header                          ((t (:foreground ,purple-1    :background ,purple-1bg                    ))))
   ;; diff-changed
   ;; diff-function
   ;; diff-header
   ;; diff-index
   ;; diff-indicator-added
   ;; diff-indicator-changed
   ;; diff-indicator-removed
   ;; diff-nonexistent
   ;; diff-refine-added
   ;; diff-refine-change
   ;; diff-refine-removed

   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Linum
   `(linum                                     ((t (:foreground ,gray-6 :background ,alt-background :height 90   ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; show-paren-mode
   `(show-paren-match                          ((t (:foreground ,foam        :background ,orange-1                      ))))
   `(show-paren-mismatch                       ((t (:foreground ,orange-1    :background ,red-2bg                       ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; ido
   `(ido-only-match                            ((t (:foreground ,green-1     :background ,green-1bg                     ))))
   `(ido-subdir                                ((t (:foreground ,purple-1    :background ,purple-1bg                    ))))
   `(ido-first-match                           ((t (:foreground ,orange-1    :background ,orange-1bg                    ))))
   `(ido-incomplete-regexp                     ((t (:foreground ,red-1       :background ,orange-1bg                    ))))
   `(ido-indicator                             ((t (:foreground ,turquoise-1 :background ,turquoise-1bg                 ))))
   `(ido-virtual                               ((t (:foreground ,green-3     :background ,turquoise-1bg                 ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; whitespace-mode
   `(whitespace-empty                          ((t (:foreground ,yellow-1    :background ,turquoise-2bg                 ))))
   `(whitespace-hspace                         ((t (:foreground ,turquoise-2 :background ,turquoise-2bg                 ))))
   `(whitespace-indentation                    ((t (:foreground ,turquoise-2 :background ,turquoise-2bg                 ))))
   `(whitespace-line                           ((t (:foreground ,orange-1    :background ,turquoise-2bg                 ))))
   `(whitespace-newline                        ((t (:foreground ,turquoise-2 :background ,turquoise-2bg                 ))))
   `(whitespace-space                          ((t (:foreground ,turquoise-2 :background ,turquoise-2bg                 ))))
   `(whitespace-space-after-tab                ((t (:foreground ,turquoise-2 :background ,turquoise-2bg                 ))))
   `(whitespace-tab                            ((t (:foreground ,turquoise-2 :background ,turquoise-2bg                 ))))
   `(whitespace-trailing                       ((t (:foreground ,red-1       :background ,turquoise-2bg                 ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; flyspell-mode
   `(flyspell-incorrect                        ((t (:underline ,red-2                                                   ))))
   `(flyspell-duplicate                        ((t (:underline ,green-2                                                 ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; flymake-mode
   `(flymake-errline                           ((t (:underline ,red-2        :background nil :inherit nil               ))))
   `(flymake-warnline                          ((t (:underline ,green-2      :background nil :inherit nil               ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; dropdown-list
   `(dropdown-list-selection-face              ((t (:foreground ,foam        :background ,purple-1bg                    ))))
   `(dropdown-list-face                        ((t (:foreground ,background  :background ,foam                          ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; git gutter mode
   `(git-gutter:added                          ((t (:foreground ,green-1     :background ,green-2bg                     ))))
   `(git-gutter:deleted                        ((t (:foreground ,red-1       :background ,red-1bg                       ))))
   `(git-gutter:modified                       ((t (:foreground ,purple-1    :background ,purple-1bg                    ))))
   `(git-gutter:unchanged                      ((t (                         :background ,yellow-1bg                    ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; magit
   `(magit-item-highlight                      ((t (                         :background ,purple-3bg                    ))))
   `(magit-branch                              ((t (:foreground ,green-2     :background ,green-2bg                     ))))
   `(magit-whitespace-warning-face             ((t (:foreground ,red-3       :background ,red-1bg                       ))))
   `(magit-section-title                       ((t (:foreground ,purple-1    :background ,purple-1bg                    ))))
   `(magit-header                              ((t (:foreground ,orange-1    :background ,orange-1bg                    ))))
   `(magit-item-mark                           ((t (:foreground ,green-1                                                ))))
   `(magit-diff-merge-proposed                 ((t (:foreground ,foam                                                   ))))
   `(magit-diff-merge-current                  ((t (:foreground ,blue-1                                                 ))))
   `(magit-diff-merge-separator                ((t (:foreground ,blue-2                                                 ))))
   `(magit-log-author                          ((t (:foreground ,purple-1    :background ,purple-1bg                    ))))
   `(magit-log-graph                           ((t (:foreground ,blue-2      :background ,blue-2bg                      ))))
   `(magit-log-head-label-bisect-good          ((t (:foreground ,turquoise-1 :background ,turquoise-1bg                 ))))
   `(magit-log-head-label-local                ((t (:foreground ,foam        :background ,turquoise-1bg                 ))))
   `(magit-log-head-label-remote               ((t (:foreground ,foam        :background ,purple-2bg                    ))))
   `(magit-log-message                         ((t (:foreground ,dirty-crem  :background ,background                    ))))
   `(magit-log-date                            ((t (:foreground ,blue-4      :background ,background                    ))))
   `(magit-log-head-label-bisect-bad           ((t (:foreground ,red-1       :background ,red-1bg                       ))))
   `(magit-log-head-label-default              ((t (:foreground ,foam        :background ,turquoise-1bg                 ))))
   `(magit-log-head-label-patches              ((t (:foreground ,blue-2      :background ,blue-1bg                      ))))
   `(magit-log-head-label-tags                 ((t (:foreground ,orange-1    :background ,orange-1bg                    ))))
   `(magit-log-sha1                            ((t (:foreground ,turquoise-1 :background ,turquoise-1bg                 ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; IEdit
   `(iedit-occurrence                          ((t (:foreground ,green-3   :background ,orange-1bg                     ))))
   `(iedit-read-only-occurrence                ((t (:foreground ,red-1     :background ,orange-1bg                     ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; highlight-indentation-mode
   `(highlight-indentation-face                ((t (                         :background ,background-dark               ))))
   `(highlight-indentation-current-column-face ((t (                         :background ,gray-5                        ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; ECB
   `(ecb-default-general-face                  ((t (:foreground ,gray-1      :background ,gray-1bg                      ))))
   `(ecb-default-highlight-face                ((t (:foreground ,red-1       :background ,red-1bg                       ))))
   `(ecb-method-face                           ((t (:foreground ,red-1       :background ,red-1bg                       ))))
   `(ecb-tag-header-face                       ((t (                         :background ,blue-2bg                      ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; org-mode
   `(org-date                                  ((t (:foreground ,purple-1    :background ,purple-1bg                    ))))
   `(org-done                                  ((t (:foreground ,green-1     :background ,green-1bg                     ))))
   `(org-hide                                  ((t (:foreground ,gray-3      :background ,gray-1bg                      ))))
   `(org-link                                  ((t (:foreground ,blue-1      :background ,blue-1bg                      ))))
   `(org-todo                                  ((t (:foreground ,red-1       :background ,red-1bg                       ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; cua-mode
   `(cua-global-mark        ((t(:foreground ,foam :background ,turquoise-1                                              ))))
   `(cua-rectangle          ((t(:foreground ,foam :background ,purple-4                                                 ))))
   `(cua-rectangle-noselect ((t(:foreground ,foam :background ,orange-1                                                 ))))
   )

  (custom-theme-set-variables
   'soothe

   `(main-line-color1 ,gray-6bg)
   `(main-line-color2 ,gray-2bg)

   `(main-line-separator-style
     'chamfer
     )

   `(powerline-color1 ,gray-6bg)
   `(powerline-color2 ,gray-2bg)

   `(Linum-format "%7i ")
   `(fringe-mode 4)

   ;; Fill Column Indicator mode
   `(fci-rule-color ,gray-6)
   `(fci-rule-character-color ,gray-6)

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red-1 ,green-1 ,yellow-1 ,blue-1 ,purple-1 ,blue-3 ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red-1 ,green-1 ,yellow-1 ,blue-1 ,purple-1 ,blue-3 ,foreground])
   )
  )

;; Rainbow delimiters
(defun soothe-rainbow-delim-set-face ()
  (set-face-attribute 'rainbow-delimiters-depth-1-face   nil :foreground "#2B583D")
  (set-face-attribute 'rainbow-delimiters-depth-2-face   nil :foreground "#2B5858")
  (set-face-attribute 'rainbow-delimiters-depth-3-face   nil :foreground "#2B3C58")
  (set-face-attribute 'rainbow-delimiters-depth-4-face   nil :foreground "#342B58")
  (set-face-attribute 'rainbow-delimiters-depth-5-face   nil :foreground "#4F2B58")
  (set-face-attribute 'rainbow-delimiters-depth-6-face   nil :foreground "#582B45")
  (set-face-attribute 'rainbow-delimiters-depth-7-face   nil :foreground "#58462B")
  (set-face-attribute 'rainbow-delimiters-depth-8-face   nil :foreground "#4E582B")
  (set-face-attribute 'rainbow-delimiters-depth-9-face   nil :foreground "#33582B")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#660000"))


(eval-after-load "rainbow-delimiters" '(soothe-rainbow-delim-set-face))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'soothe)

;;; List of themes to include...

;;; Man-overstrike
;;; Man-reverse
;;; Man-underline

;;; bold
;;; bold-italic
;;; border
;;; buffer-menu-buffer
;;; button

;;; c-annotation-face

;;; change-log-acknowledgment
;;; change-log-conditionals
;;; change-log-date
;;; change-log-email
;;; change-log-file
;;; change-log-function
;;; change-log-list
;;; change-log-name

;;; comint-highlight-input
;;; comint-highlight-prompt

;;; completions-annotations
;;; completions-common-part
;;; completions-first-difference

;;; custom-button
;;; custom-button-mouse
;;; custom-button-pressed
;;; custom-button-pressed-unraised
;;; custom-button-unraised
;;; custom-changed
;;; custom-comment
;;; custom-comment-tag
;;; custom-documentation
;;; custom-face-tag
;;; custom-group-subtitle
;;; custom-group-tag
;;; custom-group-tag-1
;;; custom-invalid
;;; custom-link
;;; custom-modified
;;; custom-rogue
;;; custom-saved
;;; custom-set
;;; custom-state
;;; custom-themed
;;; custom-variable-button
;;; custom-variable-tag
;;; custom-visibility

;;; ecb-default-general-face
;;; ecb-default-highlight-face
;;; ecb-method-face
;;; ecb-tag-header-face

;;; error
;;; escape-glyph
;;; file-name-shadow
;;; fixed-pitch

;;; glyphless-char

;;; header-line
;;; help-argument-name

;;; hi-black-b
;;; hi-black-hb
;;; hi-blue
;;; hi-blue-b
;;; hi-green
;;; hi-green-b
;;; hi-pink
;;; hi-red-b
;;; hi-yellow

;;; hideshowvis-hidable-face

;;; highlight
;;; highlight-changes
;;; highlight-changes-delete
;;; highlight-indentation-current-column-face
;;; highlight-indentation-face

;;; hl-line

;;; hs-face
;;; hs-fringe-face

;;; info-header-node
;;; info-header-xref
;;; info-menu-header
;;; info-menu-star
;;; info-node
;;; info-title-1
;;; info-title-2
;;; info-title-3
;;; info-title-4
;;; info-xref
;;; info-xref-visited

;;; italic

;;; lazy-highlight

;;; link
;;; link-visited

;;; log-edit-header
;;; log-edit-summary
;;; log-edit-unknown-header

;;; mac-ts-caret-position
;;; mac-ts-converted-text
;;; mac-ts-raw-text
;;; mac-ts-selected-converted-text

;;; match
;;; menu

;;; message-cited-text
;;; message-header-cc
;;; message-header-name
;;; message-header-newsgroups
;;; message-header-other
;;; message-header-subject
;;; message-header-to
;;; message-header-xheader
;;; message-mml
;;; message-separator

;;; minibuffer-prompt


;;; mouse

;;; next-error

;;; nobreak-space

;;; org-date
;;; org-done
;;; org-hide
;;; org-link
;;; org-todo

;;; powerline-active1
;;; powerline-active2
;;; powerline-inactive1
;;; powerline-inactive2

;;; proced-mark
;;; proced-marked
;;; proced-sort-header

;;; query-replace

;;; rcirc-bright-nick
;;; rcirc-dim-nick
;;; rcirc-keyword
;;; rcirc-my-nick
;;; rcirc-nick-in-message
;;; rcirc-nick-in-message-full-line
;;; rcirc-other-nick
;;; rcirc-prompt
;;; rcirc-server
;;; rcirc-server-prefix
;;; rcirc-timestamp
;;; rcirc-track-keyword
;;; rcirc-track-nick
;;; rcirc-url

;;; rst-adornment
;;; rst-block
;;; rst-comment
;;; rst-definition
;;; rst-directive
;;; rst-emphasis1
;;; rst-emphasis2
;;; rst-external
;;; rst-level-1
;;; rst-level-2
;;; rst-level-3
;;; rst-level-4
;;; rst-level-5
;;; rst-level-6
;;; rst-literal
;;; rst-reference
;;; rst-transition

;;; scroll-bar

;;; secondary-selection
;;; shadow

;;; show-paren-match
;;; show-paren-mismatch

;;; speedbar-button-face
;;; speedbar-directory-face
;;; speedbar-file-face
;;; speedbar-highlight-face
;;; speedbar-selected-face
;;; speedbar-separator-face
;;; speedbar-tag-face

;;; success

;;; tool-bar


;;; trailing-whitespace

;;; underline

;;; variable-pitch

;;; vertical-border

;;; w3m-anchor
;;; w3m-arrived-anchor
;;; w3m-bold
;;; w3m-current-anchor
;;; w3m-form-button
;;; w3m-form-button-mouse
;;; w3m-form-button-pressed
;;; w3m-header-line-location-content
;;; w3m-header-line-location-title
;;; w3m-history-current-url
;;; w3m-image
;;; w3m-image-anchor
;;; w3m-insert
;;; w3m-italic
;;; w3m-strike-through
;;; w3m-tab-background
;;; w3m-tab-mouse
;;; w3m-tab-selected
;;; w3m-tab-selected-background
;;; w3m-tab-selected-retrieving
;;; w3m-tab-unselected
;;; w3m-tab-unselected-retrieving
;;; w3m-tab-unselected-unseen
;;; w3m-underline

;;; warning

;;; whitespace-empty
;;; whitespace-hspace
;;; whitespace-indentation
;;; whitespace-line
;;; whitespace-newline
;;; whitespace-space
;;; whitespace-space-after-tab
;;; whitespace-tab
;;; whitespace-trailing

;;; widget-button
;;; widget-button-pressed
;;; widget-documentation
;;; widget-field
;;; widget-inactive
;;; widget-single-line-field

;;; woman-addition
;;; woman-bold
;;; woman-italic
;;; woman-unknown

;;; yas--field-debug-face
;;; yas-field-highlight-face

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; soothe-theme.el ends here
