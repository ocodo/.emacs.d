;;; darkmine-theme.el --- Yet another emacs dark color theme.

;; Copyright (C) 2014 Pierre Lecocq
;; Author: Pierre Lecocq <pierre.lecocq@gmail.com>
;; URL: https://github.com/pierre-lecocq/darkmine-theme
;; Version: 20140829.714
;; X-Original-Version: 0.2

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
;; Yet another emacs dark color theme

;;; Code:

(deftheme darkmine "Yet another emacs dark theme")

(let ((darkmine-background "#181818")
      (darkmine-foreground "#E3E3E3")
      ;; specials
      (darkmine-border "#282828")
      (darkmine-cursor "#08CA5F")
      (darkmine-mouse "#181818")
      (darkmine-background-modeline "#282828")
      (darkmine-foreground-modeline "#FFFFFF")
      (darkmine-background-modeline-inactive "#1F1F1F")
      (darkmine-foreground-modeline-inactive "#939393")
      (darkmine-background-region "#1D68C4")
      (darkmine-foreground-region "#FFFFFF")
      (darkmine-foreground-which-func "#1D68C4")
      (darkmine-background-company "#383838")
      (darkmine-background-company-selection "#585858")
      (darkmine-background-company-scrollbar "#989898")
      (darkmine-foreground-company-scrollbar "#b8b8b8")
      ;; fonts
      (darkmine-font-comment "#a5a5a5")
      (darkmine-font-constant "#8dd7e9")
      (darkmine-font-builtin "#7FFFD4")
      (darkmine-font-function-name "#1D68C4")
      (darkmine-font-variable-name "#ffffaa")
      (darkmine-font-keyword "#5FB7CC")
      (darkmine-font-string "#aa3939")
      (darkmine-font-doc-string "#d46a6a")
      (darkmine-font-type "#7FFFD4")
      )

  (custom-theme-set-faces
   'darkmine
   `(default ((t (:foreground ,darkmine-foreground :background ,darkmine-background))))
   ;; specials
   `(vertical-border ((t (:foreground ,darkmine-border))))
   `(mode-line ((t (:foreground ,darkmine-foreground-modeline :background ,darkmine-background-modeline))))
   `(mode-line-inactive ((t (:foreground ,darkmine-foreground-modeline-inactive :background ,darkmine-background-modeline-inactive))))
   `(region ((t (:foreground ,darkmine-foreground-region :background ,darkmine-background-region))))
   `(which-func ((t (:foreground ,darkmine-foreground-which-func))))
   `(company-tooltip ((t (:inherit default :background ,darkmine-background-company))))
   `(company-tooltip-selection ((t (:inherit default :background ,darkmine-background-company-selection))))
   `(company-scrollbar-bg ((t (:inherit default :background ,darkmine-background-company-scrollbar))))
   `(company-scrollbar-fg ((t (:inherit default :background ,darkmine-foreground-company-scrollbar))))
   ;; fonts
   `(font-lock-comment-face ((t (:foreground ,darkmine-font-comment))))
   `(font-lock-constant-face ((t (:foreground ,darkmine-font-constant))))
   `(font-lock-builtin-face ((t (:foreground ,darkmine-font-builtin))))
   `(font-lock-function-name-face ((t (:foreground ,darkmine-font-function-name))))
   `(font-lock-variable-name-face ((t (:foreground ,darkmine-font-variable-name))))
   `(font-lock-keyword-face ((t (:foreground ,darkmine-font-keyword))))
   `(font-lock-string-face ((t (:foreground ,darkmine-font-string))))
   `(font-lock-doc-string-face ((t (:foreground ,darkmine-font-doc-string))))
   `(font-lock-type-face ((t (:foreground ,darkmine-font-type))))
  )
)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'darkmine)
;;; darkmine-theme.el ends here
