(deftheme sevilleretro "sevilleretro")

(custom-theme-set-variables 'sevilleretro)

(custom-theme-set-faces 'sevilleretro
                        '(cursor ((t (:background "#a35000"))))
                        '(default ((t (:foreground "#ffe8b8" :background "#160903"))))
                        '(fringe ((t (:background "#472400"))))
                        '(mode-line ((t (:foreground "#ffb347" :background "#402008"))))
                        '(region ((t (:background "#482b0f"))))
                        '(font-lock-builtin-face ((t (:foreground "#d56b44"))))
                        '(font-lock-comment-face ((t (:foreground "#888a85"))))
                        '(font-lock-function-name-face ((t (:foreground "#f9971a"))))
                        '(font-lock-keyword-face ((t (:foreground "#ffb633"))))
                        '(font-lock-string-face ((t (:foreground "#b28153"))))
                        '(font-lock-type-face ((t (:foreground"#ff6e1a"))))
                        '(font-lock-constant-face ((t (:foreground "#e9771a"))))
                        '(font-lock-variable-name-face ((t (:foreground "#e05542"))))
                        '(minibuffer-prompt ((t (:foreground "#fa7d00" :bold t))))
                        '(font-lock-warning-face ((t (:foreground "red" :bold t))))
                        )

(provide-theme 'sevilleretro)

