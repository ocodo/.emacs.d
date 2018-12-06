;;; bubbleberry-theme.el --- A theme based on LightTable for Emacs24
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;          Gaurav Giri github.com/grvgr
;;; Url: https://github.com/jasonm23/emacs-bubbleberry-theme
;; Package-Version: 20141017.944
;;; Version: 0.1.2
;;; Package-Requires: ((emacs "24.1"))
;;;
;;; Change Log:
;;  
;;  0.1.2         : Fixes from Steve Purcell
;;  0.1.1         : Bugfix
;;  0.1.0         : initial version
;;
;;; Licence:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.
;;
;; This file is not a part of Emacs
;;
;;; Commentary:
;;
;;  A theme inspired by the default LightTable theme, by Chris Granger

(unless (>= emacs-major-version 24)
  (error "bubbleberry-theme requires Emacs 24 or later."))

(deftheme bubbleberry "Light Table like theme")

(custom-theme-set-variables
 'bubbleberry
 '(linum-format "%3i")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 )

(custom-theme-set-faces
 'bubbleberry

 ;; basic theming.
 '(default                          ((t (:foreground "#CCCCCC" :background "#202020" ))))
 '(region                           ((t (:background "#151515" ))))
 '(cursor                           ((t (:background "#EEEEEE" ))))
 '(fringe                           ((t (:background "#2F2F2F" :foreground "#FFFFFF" ))))
 '(linum                            ((t (:background "#202020" :foreground "#777777" :box nil :height 100 ))))
 '(minibuffer-prompt                ((t (:foreground "#9489C4" :weight bold ))))
 '(minibuffer-message               ((t (:foreground "#FFFFFF" ))))
 '(mode-line                        ((t (:foreground "#BBBBBB" :background "#191919" ))))
 '(mode-line-inactive               ((t (:foreground "#777777" :background "#303030" :weight light :box nil :inherit (mode-line )))))

 '(font-lock-keyword-face           ((t (:foreground "#B8E9CB"))))
 '(font-lock-type-face              ((t (:foreground "#C5B3FD"))))
 '(font-lock-constant-face          ((t (:foreground "#C5B3FD"))))
 '(font-lock-variable-name-face     ((t (:foreground "#ADCEFD"))))
 '(font-lock-builtin-face           ((t (:foreground "#6767AE"))))
 '(font-lock-string-face            ((t (:foreground "#B3DADC"))))
 '(font-lock-comment-face           ((t (:foreground "#9AACCB"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#9AACCB"))))
 '(font-lock-warning-face           ((t (:background "#9AACCB"))))
 '(font-lock-preprocessor-face      ((t (:foreground "#B8E9CB"))))
 '(font-lock-reference-face         ((t (:foreground "#000000"))))
 '(font-lock-negation-char-face     ((t (:foreground "#000000"))))

 '(font-lock-function-name-face     ((t (:foreground "#ADCEFD"))))
 '(font-lock-doc-face               ((t (:foreground "#AAAAAA"))))
 '(font-lock-doc-string-face        ((t (:foreground "#496B83"))))

 '(flymake-errline               ((t (:foreground "#EE8888" :background nil :underline nil))))
 '(flymake-warnline              ((t (:foreground "#8888FF" :background "#303030" :underline nil))))

  ;; easy defaults...
 '(tooltip ((default nil) (nil nil)))
 '(next-error ((t          (:inherit (region)))))
 '(query-replace ((t       (:inherit (isearch)))))
 '(button ((t              (:inherit (link)))))
 '(fixed-pitch ((t         (:family "Monospace"))))
 '(variable-pitch ((t      (:family "Sans Serif"))))
 '(escape-glyph ((t        (:foreground "#FF6600"))))
 '(mode-line-emphasis ((t  (:weight bold))))
 '(mode-line-highlight ((t (:box nil (t (:inherit (highlight)))))))

 '(highlight
   ((((class color) (min-colors 88) (background light)) (:background "#003453"))
    (((class color) (min-colors 88) (background dark))  (:background "#003450"))
    (((class color) (min-colors 16) (background light)) (:background "#003450"))
    (((class color) (min-colors 16) (background dark))  (:background "#004560"))
    (((class color) (min-colors 8))                     (:foreground "#000000" :background "#00FF00")) (t (:inverse-video t))))

 '(shadow
   ((((class color grayscale) (min-colors 88) (background light)) (:foreground "#999999"))
    (((class color grayscale) (min-colors 88) (background dark))  (:foreground "#999999"))
    (((class color) (min-colors 8) (background light))            (:foreground "#00ff00"))
    (((class color) (min-colors 8) (background dark))             (:foreground "#ffff00"))))

 '(trailing-whitespace
   ((((class color) (background light)) (:background "#ff0000"))
    (((class color) (background dark))  (:background "#ff0000")) (t (:inverse-video t)))))

'(link
  ((((class color) (min-colors 88) (background light)) (:underline t :foreground "#00b7f0"))
   (((class color) (background light))                 (:underline t :foreground "#0044FF"))
   (((class color) (min-colors 88) (background dark))  (:underline t :foreground "#0099aa"))
   (((class color) (background dark))                  (:underline t :foreground "#0099aa")) (t (:inherit (underline)))))

'(link-visited
  ((default                            (:inherit (link)))
   (((class color) (background light)) (:inherit (link)))
   (((class color) (background dark))  (:inherit (link)))))

'(header-line
  ((default                                      (:inherit (mode-line))) (((type tty)) (:underline t :inverse-video nil))
   (((class color grayscale) (background light)) (:box nil :foreground "#222222" :background "#bbbbbb"))
   (((class color grayscale) (background dark))  (:box nil :foreground "#bbbbbb" :background "#222222"))
   (((class mono) (background light))            (:underline t :box nil :inverse-video nil :foreground "#000000" :background "#ffffff"))
   (((class mono) (background dark))             (:underline t :box nil :inverse-video nil :foreground "#ffffff" :background "#000000"))))

'(isearch
  ((((class color) (min-colors 88) (background light)) (:foreground "#99ccee" :background "#444444"))
   (((class color) (min-colors 88) (background dark))  (:foreground "#bb3311" :background "#444444"))
   (((class color) (min-colors 16))                    (:foreground "#0088cc" :background "#444444"))
   (((class color) (min-colors 8))                     (:foreground "#0088cc" :background "#444444")) (t (:inverse-video t))))

'(isearch-fail
  ((((class color) (min-colors 88) (background light)) (:background "#ffaaaa"))
   (((class color) (min-colors 88) (background dark))  (:background "#880000"))
   (((class color) (min-colors 16))                    (:background "#FF0000"))
   (((class color) (min-colors 8))                     (:background "#FF0000"))
   (((class color grayscale))                          (:foreground "#888888")) (t (:inverse-video t))))

'(lazy-highlight
  ((((class color) (min-colors 88)  (background light)) (:background "#77bbdd"))
   (((class color) (min-colors 88)  (background dark)) (:background "#77bbdd"))
   (((class color) (min-colors 16)) (:background "#4499ee"))
   (((class color) (min-colors 8))  (:background "#4499ee")) (t (:underline t))))

'(match
  ((((class color) (min-colors 88) (background light)) (:background "#3388cc"))
   (((class color) (min-colors 88) (background dark))  (:background "#3388cc"))
   (((class color) (min-colors 8)  (background light)) (:foreground "#000000" :background "#FFFF00"))
   (((class color) (min-colors 8)  (background dark))  (:foreground "#ffffff" :background "#0000FF"))
   (((type tty) (class mono))  (:inverse-video t))  (t (:background "#888888"))))

;; TODO - rainbow-delimiters support
;; TODO - additional mode support
;; TODO - review and expand terminal support

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bubbleberry)

;;; bubbleberry-theme.el ends here
