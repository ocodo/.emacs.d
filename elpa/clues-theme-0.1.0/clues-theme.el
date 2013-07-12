;;; clues-theme.el --- Emacs 24 theme which is ok I suppose
;;;
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;; Url: https://github.com/jasonm23/emacs-clues-theme 
;;; Version: 0.1.0
;;;
;;; Changelog :
;;;     0.1.0 : Basic start, the theme is really a note to myself -> Make theme editor
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
;;; http://emacsfodder.github.com
;;;

(deftheme clues
  "clues theme - Emacs 24 theme which is ok I suppose")

(custom-theme-set-variables
 'clues
 '(fringe-mode 10
               nil (fringe) )
 '(linum-format     " %6d "   )
 '(main-line-color1 "#222232"  )
 '(main-line-color2 "#333343"  )
 '(powerline-color1 "#222232"  )
 '(powerline-color2 "#333343"  )
 
 )

(custom-theme-set-faces
 'clues

 '(cursor                              ((t (                           :background "orange"                                                                 ))))
 '(default                             ((t (:foreground "#C0E9F6"      :background "#110B14"          :inherit (fixed-pitch)                                ))))
 '(linum                               ((t (:foreground "#434844"      :background "#110B14"          :height 70                                            ))))
 '(minibuffer-prompt                   ((t (:foreground "#1278A8"      :background "#000000"          :weight bold                                          ))))
 '(escape-glyph                        ((t (:foreground "orange"       :background "#000000"                                                                ))))
 '(highlight                           ((t (:foreground "#004A4F"      :background "DarkOrange"                                                             ))))
 '(region                              ((t (                           :background "#131D26"                                                                ))))
 '(shadow                              ((t (:foreground "#777777"      :background "#000000"                                                                                           ))))
 '(secondary-selection                 ((t (                           :background "#132125"                                                                ))))
 '(trailing-whitespace                 ((t (                           :background "#C74000"                                                                ))))
 '(font-lock-builtin-face              ((t (:foreground "#55B1E2"      :background "#000000"                                                                ))))
 '(font-lock-comment-face              ((t (:foreground "#90A0A0"      :background "#000000" :slant italic                                                  ))))
 '(font-lock-constant-face             ((t (:foreground "#CFC5A9"      :background "#000000"                                                                ))))
 `(font-lock-doc-string-face           ((t (:foreground "#DFD3E9"      :background "#000000"                                                                ))))
 '(font-lock-function-name-face        ((t (:foreground "#DFD3A9"      :background "#000000"                                                                ))))
 '(font-lock-keyword-face              ((t (:foreground "#55B1E2"      :background "#000000"                                                                ))))
 '(font-lock-negation-char-face        ((t (:foreground "#C75311"      :background "#000000"                                                                ))))
 '(font-lock-string-face               ((t (:foreground "#94D4D9"      :background "#000000"                                                                ))))
 '(font-lock-variable-name-face        ((t (:foreground "#BDBA9F"      :background "#000000"                                                                ))))
 '(font-lock-comment-delimiter-face    ((t (:foreground "#3499aa"      :background "#000000"                        :inherit (font-lock-comment-face)       ))))
 '(font-lock-preprocessor-face         ((t (:foreground "#A16C26"      :background "#000000"                        :inherit (font-lock-builtin-face)       ))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#f66500"      :background "#000000"                        :inherit (bold)                         ))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red"          :background "#000000"                        :inherit (bold)                         ))))
 '(font-lock-doc-face                  ((t (:foreground "#90A0A0"      :background "#000000"                        :inherit (font-lock-string-face)        ))))
 '(font-lock-warning-face              ((t (:foreground "#008000"      :background "#000000"                        :inherit (error)                        ))))
 '(font-lock-type-face                 ((t (:foreground "#3388dd"      :background "#000000"                        :inherit (default)                      ))))
 '(link                                ((t (:foreground "#00b7f0"      :background "#000000" :underline t                                                   ))))
 '(link-visited                        ((t (:foreground "magenta4"                           :underline t           :inherit (link)                         ))))
 '(button                              ((t (                           :background "#aaa"    :underline t           :inherit (link)                         ))))
 '(fringe                              ((t (                           :background "#000000"                                                                ))))
 '(next-error                          ((t (                                                                        :inherit (region)                       ))))
 '(query-replace                       ((t (                                                                        :inherit (isearch)                      ))))
 '(header-line                         ((t (:foreground "#222222"      :background "#bbbbbb"  :box nil              :inherit (mode-line)                    ))))
 '(mode-line-highlight                 ((t (                                                  :box nil                                                      ))))
 '(mode-line-emphasis                  ((t (                                                           :weight bold                                         ))))
 '(mode-line-buffer-id                 ((t (                                                  :box nil :weight bold                                         ))))
 '(mode-line-inactive                  ((t (:foreground "#555555"      :background "#111111"  :box nil :weight light :inherit (mode-line)                   ))))
 '(mode-line                           ((t (:foreground "#777777"      :background "#111111"  :box nil :height 85    :inherit (variable-pitch)              ))))
 '(isearch                             ((t (:foreground "#99ccee"      :background "#444444"                                                                ))))
 '(isearch-fail                        ((t (                           :background "#ffaaaa"                                                                ))))
 '(lazy-highlight                      ((t (                           :background "#77bbdd"                                                                ))))
 '(match                               ((t (                           :background "#3388cc"                                                                ))))
 '(tooltip                             ((t (:foreground "black"        :background "LightYellow"                     :inherit (variable-pitch)              ))))
 '(js3-function-param-face             ((t (:foreground "#DFD3A9" ))))
 
 '(js3-external-variable-face          ((t (:foreground "#A0B090" ))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'clues)

;;; clues-theme.el ends here
