(deftheme cactus "cactus")

(custom-theme-set-variables 'cactus)

(custom-theme-set-faces 'cactus
                        '(default                      ((t (:foreground "#ffffff" :background "#000018" ))))
                        '(cursor                       ((t (:foreground "#ffffff"                       ))))
                        '(fringe                       ((t (                      :background "#323232" ))))
                        '(mode-line                    ((t (:foreground "#ffffff" :background "#323232" ))))
                        '(region                       ((t (                      :background "#323232" ))))
                        '(font-lock-builtin-face       ((t (:foreground "#46b76d"                       ))))
                        '(font-lock-comment-face       ((t (:foreground "#36c36c"                       ))))
                        '(font-lock-function-name-face ((t (:foreground "#ff792d"                       ))))
                        '(font-lock-keyword-face       ((t (:foreground "#b59973"                       ))))
                        '(font-lock-string-face        ((t (:foreground "#b59973"                       ))))
                        '(font-lock-type-face          ((t (:foreground"#e25734"                        ))))
                        '(font-lock-constant-face      ((t (:foreground "#eeeeec"                       ))))
                        '(font-lock-variable-name-face ((t (:foreground "#eeeeec"                       ))))
                        '(minibuffer-prompt            ((t (:foreground "#729fcf" :bold t               ))))
                        '(font-lock-warning-face       ((t (:foreground "red" :bold t                   ))))
                        )

(provide-theme 'cactus)

