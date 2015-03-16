(deftheme ice-cream-sundae "ice-cream-sundae")

(custom-theme-set-variables 'ice-cream-sundae) 

(custom-theme-set-faces 'ice-cream-sundae
                        '(default ((t (:foreground "#ddd7c0" :background "#010513"))))
                        '(cursor ((t (:background "#ffffff"))))
                        '(fringe ((t (:background "#2a1414"))))
                        '(mode-line ((t (:foreground "#ffffff" :background "#370b0b"))))
                        '(region ((t (:background "#323232"))))
                        '(font-lock-builtin-face ((t (:foreground "#bedfda"))))
                        '(font-lock-comment-face ((t (:foreground "#7099cf"))))
                        '(font-lock-function-name-face ((t (:foreground "#d3ffec"))))
                        '(font-lock-keyword-face ((t (:foreground "#c96e00"))))
                        '(font-lock-string-face ((t (:foreground "#a296d0"))))
                        '(font-lock-type-face ((t (:foreground"#34c6e2"))))
                        '(font-lock-constant-face ((t (:foreground "#eeeeec"))))
                        '(font-lock-variable-name-face ((t (:foreground "#eeeeec"))))
                        '(minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
                        '(font-lock-warning-face ((t (:foreground "red" :bold t))))
                        )


(provide-theme 'ice-cream-sundae)

