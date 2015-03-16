(deftheme coolingtower "coolingtower")

(custom-theme-set-variables 'coolingtower)

(custom-theme-set-faces 'coolingtower
                        '(default                      ((t (:foreground "#FFFFFF" :background "#030300" ))))
                        '(cursor                       ((t (                      :background "#FFFFFF" ))))
                        '(mode-line                    ((t (:foreground "#FFFFFF" :background "#323232" ))))
                        '(region                       ((t (                      :background "#323232" ))))
                        '(font-lock-comment-face       ((t (:foreground "#B4E99F"                       ))))
                        '(font-lock-constant-face      ((t (:foreground "#02C064"                       ))))
                        '(font-lock-builtin-face       ((t (:foreground "#5ACDB5"                       ))))
                        '(font-lock-function-name-face ((t (:foreground "#FFFF9B"                       ))))
                        '(font-lock-variable-name-face ((t (:foreground "#A3BC54"                       ))))
                        '(font-lock-keyword-face       ((t (:foreground "#BFFDFE"                       ))))
                        '(font-lock-string-face        ((t (:foreground "#97F9F1"                       ))))
                        '(font-lock-doc-string-face    ((t (:foreground "#97F9F1"                       ))))
                        '(font-lock-type-face          ((t (:foreground "#B4FBFF"                       ))))
                        )

(provide-theme 'coolingtower)

