;;; darkmine-theme.el --- Yet another emacs dark color theme.

;; Copyright (C) 2014 Pierre Lecocq
;; Author: Pierre Lecocq <pierre.lecocq@gmail.com>
;; URL: https://github.com/pierre-lecocq/darkmine-theme
;; Package-Version: 20160406.624
;; Version: 0.5

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; Yet another emacs dark color theme
;;

;;; Code:

(deftheme darkmine "Yet another emacs dark theme")

(let ((grey-1       "#181818")
      (grey-2       "#1F1F1F")
      (grey-3       "#282828")
      (grey-4       "#383838")
      (grey-5       "#585858")
      (grey-6       "#939393")
      (grey-7       "#989898")
      (grey-8       "#a5a5a5")
      (grey-9       "#b8b8b8")
      (grey-10      "#E3E3E3")
      (blue-1       "#1D68C4")
      (blue-2       "#5FB7CC")
      (blue-3       "#8dd7e9")
      (green-1      "#7FFFD4")
      (yellow-1     "#ffffaa")
      (red-1        "#aa3939")
      (red-2        "#d46a6a")
      (white        "#FFFFFF"))

  (custom-theme-set-faces
   'darkmine

   ;; internals
   `(default ((t (:foreground ,grey-10 :background ,grey-1))))
   `(vertical-border ((t (:foreground ,grey-3))))
   `(show-paren-match-face ((t (:background ,grey-3))))
   `(region ((t (:foreground ,white :background ,blue-1))))
   `(which-func ((t (:foreground ,blue-1))))

   ;; fonts lock faces
   `(font-lock-comment-face ((t (:foreground ,grey-8))))
   `(font-lock-constant-face ((t (:foreground ,blue-3))))
   `(font-lock-builtin-face ((t (:foreground ,green-1))))
   `(font-lock-function-name-face ((t (:foreground ,blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,yellow-1))))
   `(font-lock-keyword-face ((t (:foreground ,blue-2))))
   `(font-lock-string-face ((t (:foreground ,red-1))))
   `(font-lock-doc-string-face ((t (:foreground ,red-2))))
   `(font-lock-type-face ((t (:foreground ,green-1))))

   ;; hl-line
   `(hl-line-face ((t (:background ,grey-3 :weight bold))))
   `(hl-line ((t (:background ,grey-3 :weight bold))))

   ;; mode-line
   `(mode-line ((t (:foreground ,white :background ,grey-3))))
   `(mode-line-inactive ((t (:foreground ,grey-6 :background ,grey-2))))

   ;; company
   `(company-tooltip ((t (:inherit default :background ,grey-4))))
   `(company-tooltip-selection ((t (:inherit default :background ,grey-5))))
   `(company-scrollbar-bg ((t (:inherit default :background ,grey-7))))
   `(company-scrollbar-fg ((t (:inherit default :background ,grey-9))))

   ;; rainbow delimiters - from material
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,"#e91e63"))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,"#1565C0"))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,"#EF6C00"))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,"#B388FF"))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,"#76ff03"))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,"#26A69A"))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,"#B71C1C"))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,"#795548"))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,"#827717"))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,grey-10 :background ,"#EF6C00"))))

   ;; helm
   `(helm-ff-file ((t (:foreground ,white))))
   `(helm-ff-directory ((t (:foreground ,blue-1))))
   `(helm-ff-symlink ((t (:foreground ,green-1))))
   `(helm-candidate-number ((t (:foreground ,grey-10 :background ,blue-1))))
   `(helm-selection ((t (:background ,grey-4 :bold t))))
   `(helm-selection-line ((t (:background ,grey-4 :bold t))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,green-1 :bold t))))

   ;; org-mode
   `(org-block-background ((t (:background ,grey-3))))

   ;; gnus
   `(gnus-group-mail-3 ((t (:foreground ,red-2))))
   `(gnus-group-mail-3-empty ((t (:foreground ,grey-9))))

   ;; ido
   `(ido-subdir ((t (:foreground ,blue-1))))
   `(ido-only-match ((t (:foreground ,white))))
  ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'darkmine)

;;; darkmine-theme.el ends here
