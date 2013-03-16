;;; soothe-theme.el --- a dark colorful theme for Emacs24.
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;; Url: https://github.com/jasonm23/emacs-soothe-theme
;;; Version: 0.3.5
;;;
;;; Changelog:
;;; 0.3.5 : bugfix
;;; 0.3.4 : added support for main-line and flymake
;;; 0.3.0 : cleaned up for elpa
;;; 0.1.0 : initial version
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
       (purple-1bg       "#1D1B25")
       (foreground       "#F4EAD5")
       (hl-line          "#11152a")
       (selection        "#11152a")
       (background       "#110F13")
       (background-dark  "#0F0D11")
       (alt-background   "#111013"))

  ;; Terminal colors - set background to black.
  ;; FIXME: Use defface method (class color) (min-colors ...)
 
  (unless (window-system)
    (setq background      "#000000")
    (setq background-dark "#000000")
    (setq alt-background  "#000000")
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
    (setq purple-1bg      "#000000"))

  (custom-theme-set-faces
   'soothe

   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Basics
   `(default                                   ((t (:foreground ,foreground  :background ,background                    ))))
   `(cursor                                    ((t (                         :background ,red-2                         ))))
   `(region                                    ((t (:foreground nil          :background ,selection                     ))))
   `(highlight                                 ((t (:foreground ,blue-3      :background ,blue-3bg                      ))))
   `(hl-line                                   ((t (                         :background ,hl-line                       ))))
   `(minibuffer-prompt                         ((t (:foreground ,orange-1    :background ,orange-1bg                    ))))
   `(escape-glyph                              ((t (:foreground ,red-1       :background ,purple-1bg                    ))))
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
   `(fringe                                    ((t (                         :background ,gray-3bg                      ))))
   `(mode-line                                 ((t (:foreground ,gray-2      :background ,gray-3bg  :box nil :height 85 ))))
   `(mode-line-inactive                        ((t (:foreground ,gray-6      :background ,gray-3bg  :box nil :height 85 ))))
   `(vertical-border                           ((t (:foreground ,gray-4      :background ,background                    ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; Linum
   `(linum                                     ((t (:foreground ,gray-5bg    :background ,alt-background :height 90     ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; show-paren-mode
   `(show-paren-match                          ((t (:foreground ,foam        :background ,red-1bg                       ))))
   `(show-paren-mismatch                       ((t (:foreground ,orange-1    :background ,red-2bg                       ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; ido
   `(ido-only-match                            ((t (:foreground ,green-1     :background ,green-1bg                     ))))
   `(ido-subdir                                ((t (:foreground ,purple-1    :background ,purple-1bg                    ))))
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
   `(flymake-errline                           ((t (:underline ,red-2                                                   ))))
   `(flymake-warnline                          ((t (:underline ,green-2                                                 ))))
   ;;-----------------------------------------------------------------------------------------------------------------------
   ;; magit
   `(magit-diff-add                            ((t (:foreground ,green-1                                                ))))
   `(magit-diff-del                            ((t (:foreground ,red-2                                                  ))))
   `(magit-item-highlight                      ((t (                         :background ,gray-1bg                      ))))
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
   )

  (custom-theme-set-variables
   'soothe

   `(powerline-color1 ,gray-6)
   `(powerline-color2 ,gray-3bg)
   `(main-line-color1 ,gray-6)
   `(main-line-color2 ,gray-3bg)
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
  (set-face-attribute 'rainbow-delimiters-depth-1-face   nil :foreground "#6ef" )
  (set-face-attribute 'rainbow-delimiters-depth-2-face   nil :foreground "#1ad" )
  (set-face-attribute 'rainbow-delimiters-depth-3-face   nil :foreground "#168" )
  (set-face-attribute 'rainbow-delimiters-depth-4-face   nil :foreground "#146" )
  (set-face-attribute 'rainbow-delimiters-depth-5-face   nil :foreground "#036" )
  (set-face-attribute 'rainbow-delimiters-depth-6-face   nil :foreground "#033" )
  (set-face-attribute 'rainbow-delimiters-depth-7-face   nil :foreground "#134" )
  (set-face-attribute 'rainbow-delimiters-depth-8-face   nil :foreground "#156" )
  (set-face-attribute 'rainbow-delimiters-depth-9-face   nil :foreground "#189" )
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#600" ))


(eval-after-load "rainbow-delimiters" '(soothe-rainbow-delim-set-face))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'soothe)

;;; soothe-theme.el ends here

