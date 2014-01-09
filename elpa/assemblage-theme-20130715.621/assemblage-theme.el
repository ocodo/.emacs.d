;;; assemblage-theme.el --- a dark theme for Emacs 24
;;;
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;; Url: https://github.com/jasonm23/emacs-assemblage-theme 
;;; Version: 20130715.621
;;;
;;; Changelog :
;;;
;;; 20130715.621: Inital version
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

(deftheme assemblage
  "assemblage theme - a dark theme for Emacs 24")

(custom-theme-set-variables
 'assemblage
 '(fringe-mode 10 nil (fringe))
 '(linum-format     " %6d "  )
 '(main-line-color1 "#222912")
 '(main-line-color2 "#09150F")
 '(powerline-color1 "#222912")
 '(powerline-color2 "#09150F"))

(custom-theme-set-faces
 'assemblage

 '(cursor                              ((t (                      :background "orange"                                                           ))))
 '(default                             ((t (:foreground "#F0F0E0" :background "#090F10"                        :inherit (fixed-pitch)            ))))
 '(linum                               ((t (:foreground "#434844" :background "#09150F"    :height 70                                            ))))
 '(minibuffer-prompt                   ((t (:foreground "#1278A8" :background nil          :weight bold                                          ))))
 '(escape-glyph                        ((t (:foreground "orange"  :background nil                                                                ))))
 '(highlight                           ((t (:foreground "orange"  :background nil                                                                ))))
 '(region                              ((t (                      :background "#131D26"                                                          ))))
 '(shadow                              ((t (:foreground "#777777" :background nil                                                                ))))
 '(secondary-selection                 ((t (                      :background "#132125"                                                          ))))
 '(trailing-whitespace                 ((t (                      :background "#C74000"                                                          ))))

 `(font-lock-comment-face              ((t (:foreground "#008DCA" :background nil :slant italic                                                  ))))
 `(font-lock-constant-face             ((t (:foreground "#00E1DF" :background nil                                                                ))))
 `(font-lock-builtin-face              ((t (:foreground "#4FDaDF" :background nil                                                                ))))
 `(font-lock-function-name-face        ((t (:foreground "#2EFEDC" :background nil                                                                ))))
 `(font-lock-variable-name-face        ((t (:foreground "#22FFD3" :background nil                                                                ))))
 `(font-lock-keyword-face              ((t (:foreground "#41BC93" :background nil                                                                ))))
 `(font-lock-string-face               ((t (:foreground "#49a4C1" :background nil                                                                ))))
 `(font-lock-doc-string-face           ((t (:foreground "#1994C1" :background nil                                                                ))))
 `(font-lock-type-face                 ((t (:foreground "#81a6B4" :background nil                                                                ))))
 
 '(font-lock-negation-char-face        ((t (:foreground "#C75311" :background nil                                                                ))))
 '(font-lock-comment-delimiter-face    ((t (:foreground "#3499aa" :background nil                              :inherit (font-lock-comment-face) ))))
 '(font-lock-preprocessor-face         ((t (:foreground "#A16C26" :background nil                              :inherit (font-lock-builtin-face) ))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#f66500" :background nil                              :inherit (bold)                   ))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red"     :background nil                              :inherit (bold)                   ))))
 '(font-lock-doc-face                  ((t (:foreground "#90A0A0" :background nil                              :inherit (font-lock-string-face)  ))))
 '(font-lock-warning-face              ((t (:foreground "#008000" :background nil                              :inherit (error)                  ))))

 '(link                                ((t (:foreground "#00b7f0" :background nil       :underline t                                             ))))
 '(link-visited                        ((t (:foreground "magenta4"                      :underline t           :inherit (link)                   ))))
 '(button                              ((t (:foreground "#FFFFFF" :background "#333333" :underline t           :inherit (link)                   ))))
 '(fringe                              ((t (                      :background "#09150F" nil                                                      ))))
 '(next-error                          ((t (                                                                   :inherit (region)                 ))))
 '(query-replace                       ((t (                                                                   :inherit (isearch)                ))))
 '(header-line                         ((t (:foreground "#222222" :background "#bbbbbb" :box nil               :inherit (mode-line)              ))))
 '(mode-line-highlight                 ((t (                                            :box nil                                                 ))))
 '(mode-line-emphasis                  ((t (                                                     :weight bold                                    ))))
 '(mode-line-buffer-id                 ((t (                                            :box nil :weight bold                                    ))))
 '(mode-line-inactive                  ((t (:foreground "#555555" :background "#111111" :box nil :weight light :inherit (mode-line)              ))))
 '(mode-line                           ((t (:foreground "#777777" :background "#111111" :box nil :height 85    :inherit (variable-pitch)         ))))
 '(isearch                             ((t (:foreground "#99ccee" :background "#444444"                                                          ))))
 '(isearch-fail                        ((t (                      :background "#ffaaaa"                                                          ))))
 '(lazy-highlight                      ((t (                      :background "#77bbdd"                                                          ))))
 '(match                               ((t (                      :background "#3388cc"                                                          ))))
 '(tooltip                             ((t (:foreground "black"   :background "LightYellow"                    :inherit (variable-pitch)         ))))
 '(js3-function-param-face             ((t (:foreground "#AFD3C9"                                                                                ))))
 '(js3-external-variable-face          ((t (:foreground "#A0B0B0"                                                                                ))))
 '(cua-rectangle                       ((t (:foreground "white"   :background "#DD6600"                                                          ))))
 )

;; Rainbow delimiters
(defun assemblage-rainbow-delim-set-face ()
  (set-face-attribute 'rainbow-delimiters-depth-1-face   nil :foreground "#446622")
  (set-face-attribute 'rainbow-delimiters-depth-2-face   nil :foreground "#668844")
  (set-face-attribute 'rainbow-delimiters-depth-3-face   nil :foreground "#88aa66")
  (set-face-attribute 'rainbow-delimiters-depth-4-face   nil :foreground "#AACC88")
  (set-face-attribute 'rainbow-delimiters-depth-5-face   nil :foreground "#CCDDAA")
  (set-face-attribute 'rainbow-delimiters-depth-6-face   nil :foreground "#DEEEAA")
  (set-face-attribute 'rainbow-delimiters-depth-7-face   nil :foreground "#EFFFBB")
  (set-face-attribute 'rainbow-delimiters-depth-8-face   nil :foreground "#EFFFCC")
  (set-face-attribute 'rainbow-delimiters-depth-9-face   nil :foreground "#EFFFEE")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#AA0000"))

(eval-after-load "rainbow-delimiters" '(assemblage-rainbow-delim-set-face))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'assemblage)

;;; assemblage-theme.el ends here
