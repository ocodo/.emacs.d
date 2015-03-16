(deftheme marshland "marshland")

(custom-theme-set-variables 'marshland)

(custom-theme-set-faces 'marshland
                        '(default                      ((t (:foreground "#FFFFFF" :background "#000000"))))
                        '(cursor                       ((t (:background "#FFFFFF"))))
                        '(mode-line                    ((t (:foreground "#FFFFFF" :background "#323232"))))
                        '(region                       ((t (:background "#323232"))))
                        '(font-lock-comment-face       ((t (:foreground "#80B5A2"))))
                        '(font-lock-constant-face      ((t (:foreground "#ADB934"))))
                        '(font-lock-builtin-face       ((t (:foreground "#FFB793"))))
                        '(font-lock-function-name-face ((t (:foreground "#FF8F00"))))
                        '(font-lock-variable-name-face ((t (:foreground "#97C62A"))))
                        '(font-lock-keyword-face       ((t (:foreground "#F8AC81"))))
                        '(font-lock-string-face        ((t (:foreground "#52E591"))))
                        '(font-lock-doc-string-face    ((t (:foreground "#52E591"))))
                        '(font-lock-type-face          ((t (:foreground "#FFB63C"))))
                        )

(provide-theme 'marshland)

