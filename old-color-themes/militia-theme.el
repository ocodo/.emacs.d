(deftheme militia "militia")

(custom-theme-set-variables 'militia)

(custom-theme-set-faces 'militia
                        '(default                      ((t (:foreground "#FFFFFF" :background "#000E03"))))
                        '(cursor                       ((t (:background "#FFFFFF"))))
                        '(mode-line                    ((t (:foreground "#FFFFFF" :background "#323232"))))
                        '(region                       ((t (:background "#323232"))))
                        '(font-lock-comment-face       ((t (:foreground "#57FF8D"))))
                        '(font-lock-constant-face      ((t (:foreground "#C9AD65"))))
                        '(font-lock-builtin-face       ((t (:foreground "#519924"))))
                        '(font-lock-function-name-face ((t (:foreground "#BDF29A"))))
                        '(font-lock-variable-name-face ((t (:foreground "#19FF00"))))
                        '(font-lock-keyword-face       ((t (:foreground "#9E965A"))))
                        '(font-lock-string-face        ((t (:foreground "#E87300"))))
                        '(font-lock-doc-string-face    ((t (:foreground "#E87300"))))
                        '(font-lock-type-face          ((t (:foreground "#55CE0C"))))
                        )

(provide-theme 'militia)

