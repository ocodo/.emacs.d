;;; soothe-theme.el --- a dark colorful theme for Emacs24.
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;; Url: https://github.com/jasonm23/emacs-soothe-theme
;; Package-Version: 20141027.1441
;;; Version: 20141027.2233
;;; Package-Requires: ((emacs "24.1"))
;;;
;;; Change Log:
;;; 20141027.2233 : Fix rainbow delimiters
;;; 20141027.0939 : Fix comment color
;;; 20141023.2139 : Fixed rainbow delimiters for better legibility, @osener
;;;                 Added new faces @osener
;;;                 Moved rainbow delimiters to custom faces
;;; 20130805.0658 : Deferred ansi-term issue - will revist.
;;; 20130805.0148 : Fix ansi-term issue
;;; 0.3.19        : Styles for js3 added
;;; 0.3.18        : Added stubs for helm and js3
;;; 0.3.17        : Added support for which-func face (milkypostman)
;;; 0.3.16        : Fixed flymake colors; also added Package Requres Emacs 24 by Steve Purcell
;;; 0.3.15        : update by Sabof; Magit diff settings migrated over to diff
;;;               : (magit will inherit from diff)
;;; 0.3.14        : Changed main-line colors and style, added powerline colors
;;; 0.3.13        : Different colors for hl-line and region
;;; 0.3.12        : Fixed issue #4 - region bg contrast too low.
;;; 0.3.11        : Added rainbow-mode switch for editing
;;; 0.3.10        : Themed Mode line emphasis
;;; 0.3.9         : Slightly improved dired support
;;; 0.3.8         : Added support for isearch
;;;               : Additional support for mode-line
;;;               : modified main-line colors
;;;               : modified link / link-visited
;;;               : added tooltip face
;;; 0.3.7         : extended magit support
;;;               : added iedit support
;;;               : added CUA support
;;;               : added AutoComplete support
;;;               : changed rainbow delimiters for better visibility
;;; 0.3.6         : beginning work on additional mode support
;;;               : added git-gutter
;;;               : added dropdown-list
;;;               : added stubs/extention lists for gnus, rcirc, message
;;;               :       ido + magit (more),
;;;               :       ac, compilation,
;;;               :       dired, diff, iedit,
;;;               :       cua, change, Man, woman,
;;;               :       commint, info, custom, popup
;;;               :       widget, whitespace, w3m, more...
;;; 0.3.5         : bugfix
;;; 0.3.4         : added support for main-line and flymake
;;; 0.3.0         : cleaned up for elpa
;;; 0.1.0         : initial version
;;;
;;;
;;; Commentary:
;;;   An amalgam of muted color tones and highlighted
;;;   backgrounds.  Builtin support for rainbow-delimiters, org-mode,
;;;   whitespace-mode, ECB, flyspell, ido, linum, highlight
;;;   indentation, show-paren-mode, further mode support to come.
;;;
;;; License:
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
;;; Code:

(unless (>= emacs-major-version 24)
  (error "Soothe-theme requires Emacs 24 or later"))

(deftheme soothe
  "soothe-theme An amalgam of muted color tones and highlighted
   backgrounds.  Has builtin support for rainbow-delimiters,
   org-mode, whitespace-mode, ECB, flyspell, ido, linum,
   highlight indentation, show-paren-mode, further mode support
   to come.")

(let*  (
       ;; Palette
       (foam             "#E0E4CC")
       (snow-code        "#ECE5CE")
       (crem             "#F4EAD5")
       (dirty-crem       "#DBD2BF")
       (dirty-crem-bg    "#2B2A26")
       (gray-1           "#AAAAAA")
       (gray-2           "#828282")
       (gray-3           "#333333")
       (gray-4           "#2A2A2A")
       (gray-5           "#252525")
       (gray-6           "#202020")
       (gray-1bg         "#0A0A0A")
       (gray-2bg         "#111111")
       (gray-3bg         "#141414")
       (gray-4bg         "#171717")
       (gray-5bg         "#1A1A1A")
       (gray-6bg         "#1E1E1E")
       (red-1            "#B13120")
       (red-2            "#A23F1E")
       (red-3            "#AA1100")
       (red-4            "#660000")
       (red-1bg          "#1D1515")
       (red-2bg          "#251C1E")
       (brown-1          "#8F621D")
       (brown-1bg        "#2A1F1F")
       (orange-1         "#D94A05")
       (orange-2         "#FF5211")
       (orange-1bg       "#1F1710")
       (yellow-1         "#CEAE3E")
       (yellow-1bg       "#18140C")
       (green-1          "#719F34")
       (green-2          "#3E8F75")
       (green-3          "#839F5E")
       (green-1bg        "#1A2321")
       (green-2bg        "#1A2321")
       (turquoise-1      "#01535F")
       (turquoise-2      "#073E46")
       (turquoise-1bg    "#04181C")
       (turquoise-2bg    "#031316")
       (blue-1           "#7C9FC9")
       (blue-2           "#317598")
       (blue-3           "#009090")
       (blue-4           "#364E7A")
       (blue-1bg         "#1E252F")
       (blue-2bg         "#1B333E")
       (blue-3bg         "#132228")
       (blue-4bg         "#172028")
       (purple-1         "#7868B5")
       (purple-2         "#8A7FB5")
       (purple-3         "#483E6C")
       (purple-4         "#342B58")
       (purple-1bg       "#1D1B25")
       (purple-2bg       "#302948")
       (purple-3bg       "#241F36")
       (foreground       "#F4EAD5")
       (hl-line          "#11252A")
       (selection        "#11253A")
       (background       "#110F13")
       (background-dark  "#0F0D11")
       (alt-background   "#111013"))

  ;; Terminal colors - set background to black.
  ;; TODO: Use defface method (class color) (min-colors ...)

  (unless (window-system)
    (message "In a Terminal Install xterm-frobs.el (via MELPA) and use xterm-256color for the greater good")
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
   `(font-lock-comment-face                    ((t (:foreground ,purple-1    :background ,alt-background :italic t      ))))
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
   `(mode-line                                 ((t (:foreground ,gray-2      :background ,gray-3bg  :box nil            ))))
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
   `(popup-tip-face                            ((t (:foreground ,dirty-crem     :background ,dirty-crem-bg              ))))
   `(tooltip                                   ((t (:foreground ,dirty-crem-bg  :background ,dirty-crem                 ))))

   ;; `(popup-face                       ((t (:foreground nil :background nil ))))
   ;; `(popup-isearch-match              ((t (:foreground nil :background nil ))))
   ;; `(popup-menu-face                  ((t (:foreground nil :background nil ))))
   ;; `(popup-menu-mouse-face            ((t (:foreground nil :background nil ))))
   ;; `(popup-menu-selection-face        ((t (:foreground nil :background nil ))))
   ;; `(popup-menu-summary-face          ((t (:foreground nil :background nil ))))
   ;; `(popup-scroll-bar-background-face ((t (:foreground nil :background nil ))))
   ;; `(popup-scroll-bar-foreground-face ((t (:foreground nil :background nil ))))
   ;; `(popup-summary-face               ((t (:foreground nil :background nil ))))

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

   ;; `(gnus-group-mail-1                ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-mail-1-empty          ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-mail-2                ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-mail-2-empty          ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-mail-3                ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-mail-3-empty          ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-mail-low              ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-mail-low-empty        ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-1                ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-1-empty          ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-2                ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-2-empty          ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-3                ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-3-empty          ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-4                ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-4-empty          ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-5                ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-5-empty          ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-6                ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-6-empty          ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-low              ((t (:foreground nil :background nil ))))
   ;; `(gnus-group-news-low-empty        ((t (:foreground nil :background nil ))))
   ;; `(gnus-splash                      ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-cancelled           ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-high-ancient        ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-high-read           ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-high-ticked         ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-high-undownloaded   ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-high-unread         ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-low-ancient         ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-low-read            ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-low-ticked          ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-low-undownloaded    ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-low-unread          ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-normal-ancient      ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-normal-read         ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-normal-ticked       ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-normal-undownloaded ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-normal-unread       ((t (:foreground nil :background nil ))))
   ;; `(gnus-summary-selected            ((t (:foreground nil :background nil ))))

   ;; ------------------------------------------------------------------------------------------
   ;; helm

   ;; `(helm-M-x-key              ((t (:foreground nil :background nil ))))
   ;; `(helm-action               ((t (:foreground nil :background nil ))))
   ;; `(helm-bookmark-addressbook ((t (:foreground nil :background nil ))))
   ;; `(helm-bookmark-directory   ((t (:foreground nil :background nil ))))
   ;; `(helm-bookmark-file        ((t (:foreground nil :background nil ))))
   ;; `(helm-bookmark-gnus        ((t (:foreground nil :background nil ))))
   ;; `(helm-bookmark-info        ((t (:foreground nil :background nil ))))
   ;; `(helm-bookmark-man         ((t (:foreground nil :background nil ))))
   ;; `(helm-bookmark-w3m         ((t (:foreground nil :background nil ))))
   ;; `(helm-buffer-not-saved     ((t (:foreground nil :background nil ))))
   ;; `(helm-buffer-process       ((t (:foreground nil :background nil ))))
   ;; `(helm-buffer-saved-out     ((t (:foreground nil :background nil ))))
   ;; `(helm-buffer-size          ((t (:foreground nil :background nil ))))
   ;; `(helm-candidate-number     ((t (:foreground nil :background nil ))))
   ;; `(helm-ff-directory         ((t (:foreground nil :background nil ))))
   ;; `(helm-ff-executable        ((t (:foreground nil :background nil ))))
   ;; `(helm-ff-file              ((t (:foreground nil :background nil ))))
   ;; `(helm-ff-invalid-symlink   ((t (:foreground nil :background nil ))))
   ;; `(helm-ff-prefix            ((t (:foreground nil :background nil ))))
   ;; `(helm-ff-symlink           ((t (:foreground nil :background nil ))))
   ;; `(helm-grep-cmd-line        ((t (:foreground nil :background nil ))))
   ;; `(helm-grep-file            ((t (:foreground nil :background nil ))))
   ;; `(helm-grep-finish          ((t (:foreground nil :background nil ))))
   ;; `(helm-grep-lineno          ((t (:foreground nil :background nil ))))
   ;; `(helm-grep-match           ((t (:foreground nil :background nil ))))
   ;; `(helm-grep-running         ((t (:foreground nil :background nil ))))
   ;; `(helm-header               ((t (:foreground nil :background nil ))))
   ;; `(helm-helper               ((t (:foreground nil :background nil ))))
   ;; `(helm-history-deleted      ((t (:foreground nil :background nil ))))
   ;; `(helm-history-remote       ((t (:foreground nil :background nil ))))
   ;; `(helm-match                ((t (:foreground nil :background nil ))))
   ;; `(helm-moccur-buffer        ((t (:foreground nil :background nil ))))
   ;; `(helm-selection            ((t (:foreground nil :background nil ))))
   ;; `(helm-selection-line       ((t (:foreground nil :background nil ))))
   ;; `(helm-separator            ((t (:foreground nil :background nil ))))
   ;; `(helm-source-header        ((t (:foreground nil :background nil ))))
   ;; `(helm-visible-mark         ((t (:foreground nil :background nil ))))

   ;; ------------------------------------------------------------------------------------------
   ;; JS3 mode

   `(js3-error-face                    ((t (:underline ,red-1        :background ,red-1bg ))))
   `(js3-warning-face                  ((t (:underline ,yellow-1     :background ,yellow-1bg ))))
   `(js3-external-variable-face        ((t (:foreground ,purple-1    :background ,purple-1bg ))))
   `(js3-function-param-face           ((t (:foreground ,blue-3      :background ,blue-3bg ))))
   `(js3-instance-member-face          ((t (:foreground ,dirty-crem  :background ,purple-1bg ))))
   `(js3-magic-paren-face              ((t (:foreground ,snow-code   :background ,purple-1bg ))))
   `(js3-private-function-call-face    ((t (:foreground ,orange-1    :background ,orange-1bg ))))
   `(js3-private-member-face           ((t (:foreground ,orange-2    :background ,orange-1bg ))))
   `(js3-jsdoc-html-tag-delimiter-face ((t (:foreground ,blue-4      :background ,blue-2bg ))))
   `(js3-jsdoc-html-tag-name-face      ((t (:foreground ,foam        :background ,blue-3bg ))))
   `(js3-jsdoc-tag-face                ((t (:foreground ,green-3     :background ,green-2bg ))))
   `(js3-jsdoc-type-face               ((t (:foreground ,green-2     :background ,green-2bg ))))
   `(js3-jsdoc-value-face              ((t (:foreground ,green-1     :background ,green-1bg ))))


   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Diff
   `(diff-added                                ((t (:foreground ,green-3     :background ,green-2bg                     ))))
   `(diff-removed                              ((t (:foreground ,red-1       :background ,red-1bg                       ))))
   `(diff-file-header                          ((t (:foreground ,orange-1    :background ,orange-1bg                    ))))
   `(diff-context                              ((t (:foreground ,foam                                                   ))))
   `(diff-hunk-header                          ((t (:foreground ,purple-1    :background ,purple-1bg                    ))))
   ;; `(diff-changed           ((t (:foreground nil :background nil ))))
   ;; `(diff-function          ((t (:foreground nil :background nil ))))
   ;; `(diff-header            ((t (:foreground nil :background nil ))))
   ;; `(diff-index             ((t (:foreground nil :background nil ))))
   ;; `(diff-indicator-added   ((t (:foreground nil :background nil ))))
   ;; `(diff-indicator-changed ((t (:foreground nil :background nil ))))
   ;; `(diff-indicator-removed ((t (:foreground nil :background nil ))))
   ;; `(diff-nonexistent       ((t (:foreground nil :background nil ))))
   ;; `(diff-refine-added      ((t (:foreground nil :background nil ))))
   ;; `(diff-refine-change     ((t (:foreground nil :background nil ))))
   ;; `(diff-refine-removed    ((t (:foreground nil :background nil ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Linum
   `(linum                                     ((t (:foreground ,gray-6 :background ,alt-background ))))
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
   ;; diff-hl
   `(diff-hl-insert                            ((t (:foreground ,green-1     :background ,green-2bg                     ))))
   `(diff-hl-delete                            ((t (:foreground ,red-1       :background ,red-1bg                       ))))
   `(diff-hl-change                            ((t (:foreground ,purple-1    :background ,purple-1bg                    ))))
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
   `(cua-global-mark                           ((t(:foreground ,foam :background ,turquoise-1                           ))))
   `(cua-rectangle                             ((t(:foreground ,foam :background ,purple-4                              ))))
   `(cua-rectangle-noselect                    ((t(:foreground ,foam :background ,orange-1                              ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; hl-sexp
   `(hl-sexp-face                              ((t (                         :background ,turquoise-2bg                 ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face           ((t (:foreground "#D65921"                                               ))))
   `(rainbow-delimiters-depth-2-face           ((t (:foreground "#2B88A8"                                               ))))
   `(rainbow-delimiters-depth-3-face           ((t (:foreground "#FFA07E"                                               ))))
   `(rainbow-delimiters-depth-4-face           ((t (:foreground "#30D039"                                               ))))
   `(rainbow-delimiters-depth-5-face           ((t (:foreground "#58A0A2"                                               ))))
   `(rainbow-delimiters-depth-6-face           ((t (:foreground "#6070DF"                                               ))))
   `(rainbow-delimiters-depth-7-face           ((t (:foreground "#D78060"                                               ))))
   `(rainbow-delimiters-depth-8-face           ((t (:foreground "#FFDD77"                                               ))))
   `(rainbow-delimiters-depth-9-face           ((t (:foreground "#44FF88"                                               ))))
   `(rainbow-delimiters-unmatched-face         ((t (:foreground "#F92672"                                               ))))
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
   )
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'soothe)

;;; List of themes to include...

;; `(Man-overstrike                            ((t (:foreground nil :background nil ))))
;; `(Man-reverse                               ((t (:foreground nil :background nil ))))
;; `(Man-underline                             ((t (:foreground nil :background nil ))))

;; `(bold                                      ((t (:foreground nil :background nil ))))
;; `(bold-italic                               ((t (:foreground nil :background nil ))))
;; `(border                                    ((t (:foreground nil :background nil ))))
;; `(buffer-menu-buffer                        ((t (:foreground nil :background nil ))))
;; `(button                                    ((t (:foreground nil :background nil ))))

;; `(c-annotation-face                         ((t (:foreground nil :background nil ))))

;; `(change-log-acknowledgment                 ((t (:foreground nil :background nil ))))
;; `(change-log-conditionals                   ((t (:foreground nil :background nil ))))
;; `(change-log-date                           ((t (:foreground nil :background nil ))))
;; `(change-log-email                          ((t (:foreground nil :background nil ))))
;; `(change-log-file                           ((t (:foreground nil :background nil ))))
;; `(change-log-function                       ((t (:foreground nil :background nil ))))
;; `(change-log-list                           ((t (:foreground nil :background nil ))))
;; `(change-log-name                           ((t (:foreground nil :background nil ))))

;; `(comint-highlight-input                    ((t (:foreground nil :background nil ))))
;; `(comint-highlight-prompt                   ((t (:foreground nil :background nil ))))

;; `(completions-annotations                   ((t (:foreground nil :background nil ))))
;; `(completions-common-part                   ((t (:foreground nil :background nil ))))
;; `(completions-first-difference              ((t (:foreground nil :background nil ))))

;; `(custom-button                             ((t (:foreground nil :background nil ))))
;; `(custom-button-mouse                       ((t (:foreground nil :background nil ))))
;; `(custom-button-pressed                     ((t (:foreground nil :background nil ))))
;; `(custom-button-pressed-unraised            ((t (:foreground nil :background nil ))))
;; `(custom-button-unraised                    ((t (:foreground nil :background nil ))))
;; `(custom-changed                            ((t (:foreground nil :background nil ))))
;; `(custom-comment                            ((t (:foreground nil :background nil ))))
;; `(custom-comment-tag                        ((t (:foreground nil :background nil ))))
;; `(custom-documentation                      ((t (:foreground nil :background nil ))))
;; `(custom-face-tag                           ((t (:foreground nil :background nil ))))
;; `(custom-group-subtitle                     ((t (:foreground nil :background nil ))))
;; `(custom-group-tag                          ((t (:foreground nil :background nil ))))
;; `(custom-group-tag-1                        ((t (:foreground nil :background nil ))))
;; `(custom-invalid                            ((t (:foreground nil :background nil ))))
;; `(custom-link                               ((t (:foreground nil :background nil ))))
;; `(custom-modified                           ((t (:foreground nil :background nil ))))
;; `(custom-rogue                              ((t (:foreground nil :background nil ))))
;; `(custom-saved                              ((t (:foreground nil :background nil ))))
;; `(custom-set                                ((t (:foreground nil :background nil ))))
;; `(custom-state                              ((t (:foreground nil :background nil ))))
;; `(custom-themed                             ((t (:foreground nil :background nil ))))
;; `(custom-variable-button                    ((t (:foreground nil :background nil ))))
;; `(custom-variable-tag                       ((t (:foreground nil :background nil ))))
;; `(custom-visibility                         ((t (:foreground nil :background nil ))))

;; `(ecb-default-general-face                  ((t (:foreground nil :background nil ))))
;; `(ecb-default-highlight-face                ((t (:foreground nil :background nil ))))
;; `(ecb-method-face                           ((t (:foreground nil :background nil ))))
;; `(ecb-tag-header-face                       ((t (:foreground nil :background nil ))))

;; `(error                                     ((t (:foreground nil :background nil ))))
;; `(escape-glyph                              ((t (:foreground nil :background nil ))))
;; `(file-name-shadow                          ((t (:foreground nil :background nil ))))
;; `(fixed-pitch                               ((t (:foreground nil :background nil ))))

;; `(glyphless-char                            ((t (:foreground nil :background nil ))))

;; `(header-line                               ((t (:foreground nil :background nil ))))
;; `(help-argument-name                        ((t (:foreground nil :background nil ))))

;; `(hi-black-b                                ((t (:foreground nil :background nil ))))
;; `(hi-black-hb                               ((t (:foreground nil :background nil ))))
;; `(hi-blue                                   ((t (:foreground nil :background nil ))))
;; `(hi-blue-b                                 ((t (:foreground nil :background nil ))))
;; `(hi-green                                  ((t (:foreground nil :background nil ))))
;; `(hi-green-b                                ((t (:foreground nil :background nil ))))
;; `(hi-pink                                   ((t (:foreground nil :background nil ))))
;; `(hi-red-b                                  ((t (:foreground nil :background nil ))))
;; `(hi-yellow                                 ((t (:foreground nil :background nil ))))

;; `(hideshowvis-hidable-face                  ((t (:foreground nil :background nil ))))

;; `(highlight                                 ((t (:foreground nil :background nil ))))
;; `(highlight-changes                         ((t (:foreground nil :background nil ))))
;; `(highlight-changes-delete                  ((t (:foreground nil :background nil ))))
;; `(highlight-indentation-current-column-face ((t (:foreground nil :background nil ))))
;; `(highlight-indentation-face                ((t (:foreground nil :background nil ))))

;; `(hl-line                                   ((t (:foreground nil :background nil ))))

;; `(hs-face                                   ((t (:foreground nil :background nil ))))
;; `(hs-fringe-face                            ((t (:foreground nil :background nil ))))

;; `(info-header-node                          ((t (:foreground nil :background nil ))))
;; `(info-header-xref                          ((t (:foreground nil :background nil ))))
;; `(info-menu-header                          ((t (:foreground nil :background nil ))))
;; `(info-menu-star                            ((t (:foreground nil :background nil ))))
;; `(info-node                                 ((t (:foreground nil :background nil ))))
;; `(info-title-1                              ((t (:foreground nil :background nil ))))
;; `(info-title-2                              ((t (:foreground nil :background nil ))))
;; `(info-title-3                              ((t (:foreground nil :background nil ))))
;; `(info-title-4                              ((t (:foreground nil :background nil ))))
;; `(info-xref                                 ((t (:foreground nil :background nil ))))
;; `(info-xref-visited                         ((t (:foreground nil :background nil ))))

;; `(italic                                    ((t (:foreground nil :background nil ))))

;; `(lazy-highlight                            ((t (:foreground nil :background nil ))))

;; `(link                                      ((t (:foreground nil :background nil ))))
;; `(link-visited                              ((t (:foreground nil :background nil ))))

;; `(log-edit-header                           ((t (:foreground nil :background nil ))))
;; `(log-edit-summary                          ((t (:foreground nil :background nil ))))
;; `(log-edit-unknown-header                   ((t (:foreground nil :background nil ))))

;; `(mac-ts-caret-position                     ((t (:foreground nil :background nil ))))
;; `(mac-ts-converted-text                     ((t (:foreground nil :background nil ))))
;; `(mac-ts-raw-text                           ((t (:foreground nil :background nil ))))
;; `(mac-ts-selected-converted-text            ((t (:foreground nil :background nil ))))

;; `(match                                     ((t (:foreground nil :background nil ))))
;; `(menu                                      ((t (:foreground nil :background nil ))))

;; `(message-cited-text                        ((t (:foreground nil :background nil ))))
;; `(message-header-cc                         ((t (:foreground nil :background nil ))))
;; `(message-header-name                       ((t (:foreground nil :background nil ))))
;; `(message-header-newsgroups                 ((t (:foreground nil :background nil ))))
;; `(message-header-other                      ((t (:foreground nil :background nil ))))
;; `(message-header-subject                    ((t (:foreground nil :background nil ))))
;; `(message-header-to                         ((t (:foreground nil :background nil ))))
;; `(message-header-xheader                    ((t (:foreground nil :background nil ))))
;; `(message-mml                               ((t (:foreground nil :background nil ))))
;; `(message-separator                         ((t (:foreground nil :background nil ))))

;; `(minibuffer-prompt                         ((t (:foreground nil :background nil ))))


;; `(mouse                                     ((t (:foreground nil :background nil ))))

;; `(next-error                                ((t (:foreground nil :background nil ))))

;; `(nobreak-space                             ((t (:foreground nil :background nil ))))

;; `(org-date                                  ((t (:foreground nil :background nil ))))
;; `(org-done                                  ((t (:foreground nil :background nil ))))
;; `(org-hide                                  ((t (:foreground nil :background nil ))))
;; `(org-link                                  ((t (:foreground nil :background nil ))))
;; `(org-todo                                  ((t (:foreground nil :background nil ))))

;; `(powerline-active1                         ((t (:foreground nil :background nil ))))
;; `(powerline-active2                         ((t (:foreground nil :background nil ))))
;; `(powerline-inactive1                       ((t (:foreground nil :background nil ))))
;; `(powerline-inactive2                       ((t (:foreground nil :background nil ))))

;; `(proced-mark                               ((t (:foreground nil :background nil ))))
;; `(proced-marked                             ((t (:foreground nil :background nil ))))
;; `(proced-sort-header                        ((t (:foreground nil :background nil ))))

;; `(query-replace                             ((t (:foreground nil :background nil ))))

;; `(rcirc-bright-nick                         ((t (:foreground nil :background nil ))))
;; `(rcirc-dim-nick                            ((t (:foreground nil :background nil ))))
;; `(rcirc-keyword                             ((t (:foreground nil :background nil ))))
;; `(rcirc-my-nick                             ((t (:foreground nil :background nil ))))
;; `(rcirc-nick-in-message                     ((t (:foreground nil :background nil ))))
;; `(rcirc-nick-in-message-full-line           ((t (:foreground nil :background nil ))))
;; `(rcirc-other-nick                          ((t (:foreground nil :background nil ))))
;; `(rcirc-prompt                              ((t (:foreground nil :background nil ))))
;; `(rcirc-server                              ((t (:foreground nil :background nil ))))
;; `(rcirc-server-prefix                       ((t (:foreground nil :background nil ))))
;; `(rcirc-timestamp                           ((t (:foreground nil :background nil ))))
;; `(rcirc-track-keyword                       ((t (:foreground nil :background nil ))))
;; `(rcirc-track-nick                          ((t (:foreground nil :background nil ))))
;; `(rcirc-url                                 ((t (:foreground nil :background nil ))))

;; `(rst-adornment                             ((t (:foreground nil :background nil ))))
;; `(rst-block                                 ((t (:foreground nil :background nil ))))
;; `(rst-comment                               ((t (:foreground nil :background nil ))))
;; `(rst-definition                            ((t (:foreground nil :background nil ))))
;; `(rst-directive                             ((t (:foreground nil :background nil ))))
;; `(rst-emphasis1                             ((t (:foreground nil :background nil ))))
;; `(rst-emphasis2                             ((t (:foreground nil :background nil ))))
;; `(rst-external                              ((t (:foreground nil :background nil ))))
;; `(rst-level-1                               ((t (:foreground nil :background nil ))))
;; `(rst-level-2                               ((t (:foreground nil :background nil ))))
;; `(rst-level-3                               ((t (:foreground nil :background nil ))))
;; `(rst-level-4                               ((t (:foreground nil :background nil ))))
;; `(rst-level-5                               ((t (:foreground nil :background nil ))))
;; `(rst-level-6                               ((t (:foreground nil :background nil ))))
;; `(rst-literal                               ((t (:foreground nil :background nil ))))
;; `(rst-reference                             ((t (:foreground nil :background nil ))))
;; `(rst-transition                            ((t (:foreground nil :background nil ))))

;; `(scroll-bar                                ((t (:foreground nil :background nil ))))

;; `(secondary-selection                       ((t (:foreground nil :background nil ))))
;; `(shadow                                    ((t (:foreground nil :background nil ))))

;; `(show-paren-match                          ((t (:foreground nil :background nil ))))
;; `(show-paren-mismatch                       ((t (:foreground nil :background nil ))))

;; `(speedbar-button-face                      ((t (:foreground nil :background nil ))))
;; `(speedbar-directory-face                   ((t (:foreground nil :background nil ))))
;; `(speedbar-file-face                        ((t (:foreground nil :background nil ))))
;; `(speedbar-highlight-face                   ((t (:foreground nil :background nil ))))
;; `(speedbar-selected-face                    ((t (:foreground nil :background nil ))))
;; `(speedbar-separator-face                   ((t (:foreground nil :background nil ))))
;; `(speedbar-tag-face                         ((t (:foreground nil :background nil ))))

;; `(success                                   ((t (:foreground nil :background nil ))))

;; `(tool-bar                                  ((t (:foreground nil :background nil ))))


;; `(trailing-whitespace                       ((t (:foreground nil :background nil ))))

;; `(underline                                 ((t (:foreground nil :background nil ))))

;; `(variable-pitch                            ((t (:foreground nil :background nil ))))

;; `(vertical-border                           ((t (:foreground nil :background nil ))))

;; `(w3m-anchor                                ((t (:foreground nil :background nil ))))
;; `(w3m-arrived-anchor                        ((t (:foreground nil :background nil ))))
;; `(w3m-bold                                  ((t (:foreground nil :background nil ))))
;; `(w3m-current-anchor                        ((t (:foreground nil :background nil ))))
;; `(w3m-form-button                           ((t (:foreground nil :background nil ))))
;; `(w3m-form-button-mouse                     ((t (:foreground nil :background nil ))))
;; `(w3m-form-button-pressed                   ((t (:foreground nil :background nil ))))
;; `(w3m-header-line-location-content          ((t (:foreground nil :background nil ))))
;; `(w3m-header-line-location-title            ((t (:foreground nil :background nil ))))
;; `(w3m-history-current-url                   ((t (:foreground nil :background nil ))))
;; `(w3m-image                                 ((t (:foreground nil :background nil ))))
;; `(w3m-image-anchor                          ((t (:foreground nil :background nil ))))
;; `(w3m-insert                                ((t (:foreground nil :background nil ))))
;; `(w3m-italic                                ((t (:foreground nil :background nil ))))
;; `(w3m-strike-through                        ((t (:foreground nil :background nil ))))
;; `(w3m-tab-background                        ((t (:foreground nil :background nil ))))
;; `(w3m-tab-mouse                             ((t (:foreground nil :background nil ))))
;; `(w3m-tab-selected                          ((t (:foreground nil :background nil ))))
;; `(w3m-tab-selected-background               ((t (:foreground nil :background nil ))))
;; `(w3m-tab-selected-retrieving               ((t (:foreground nil :background nil ))))
;; `(w3m-tab-unselected                        ((t (:foreground nil :background nil ))))
;; `(w3m-tab-unselected-retrieving             ((t (:foreground nil :background nil ))))
;; `(w3m-tab-unselected-unseen                 ((t (:foreground nil :background nil ))))
;; `(w3m-underline                             ((t (:foreground nil :background nil ))))

;; `(warning                                   ((t (:foreground nil :background nil ))))

;; `(whitespace-empty                          ((t (:foreground nil :background nil ))))
;; `(whitespace-hspace                         ((t (:foreground nil :background nil ))))
;; `(whitespace-indentation                    ((t (:foreground nil :background nil ))))
;; `(whitespace-line                           ((t (:foreground nil :background nil ))))
;; `(whitespace-newline                        ((t (:foreground nil :background nil ))))
;; `(whitespace-space                          ((t (:foreground nil :background nil ))))
;; `(whitespace-space-after-tab                ((t (:foreground nil :background nil ))))
;; `(whitespace-tab                            ((t (:foreground nil :background nil ))))
;; `(whitespace-trailing                       ((t (:foreground nil :background nil ))))

;; `(widget-button                             ((t (:foreground nil :background nil ))))
;; `(widget-button-pressed                     ((t (:foreground nil :background nil ))))
;; `(widget-documentation                      ((t (:foreground nil :background nil ))))
;; `(widget-field                              ((t (:foreground nil :background nil ))))
;; `(widget-inactive                           ((t (:foreground nil :background nil ))))
;; `(widget-single-line-field                  ((t (:foreground nil :background nil ))))

;; `(woman-addition                            ((t (:foreground nil :background nil ))))
;; `(woman-bold                                ((t (:foreground nil :background nil ))))
;; `(woman-italic                              ((t (:foreground nil :background nil ))))
;; `(woman-unknown                             ((t (:foreground nil :background nil ))))

;; `(yas--field-debug-face                     ((t (:foreground nil :background nil ))))
;; `(yas-field-highlight-face                  ((t (:foreground nil :background nil ))))

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; soothe-theme.el ends here
