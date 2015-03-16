(deftheme grassyknoll "grassyknoll")

(custom-theme-set-variables 'grassyknoll)

(custom-theme-set-faces 'grassyknoll
                        '(default                      ((t (:foreground "#FFFFFF" :background "#000000"))))
                        '(cursor                       ((t (:background "#FFFFFF"))))
                        '(mode-line                    ((t (:foreground "#FFFFFF" :background "#323232"))))
                        '(region                       ((t (:background "#323232"))))
                        '(font-lock-comment-face       ((t (:foreground "#CCA063"))))
                        '(font-lock-constant-face      ((t (:foreground "#CCC158"))))
                        '(font-lock-builtin-face       ((t (:foreground "#ACFB2C"))))
                        '(font-lock-function-name-face ((t (:foreground "#D23F1A"))))
                        '(font-lock-variable-name-face ((t (:foreground "#599B00"))))
                        '(font-lock-keyword-face       ((t (:foreground "#696676"))))
                        '(font-lock-string-face        ((t (:foreground "#00FF00"))))
                        '(font-lock-doc-string-face    ((t (:foreground "#00FF00"))))
                        '(font-lock-type-face          ((t (:foreground "#86F410"))))
                        )

(provide-theme 'grassyknoll)

