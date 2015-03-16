(deftheme verdantparty "verdantparty")

(custom-theme-set-variables 'verdantparty)

(custom-theme-set-faces 'verdantparty
                        '(default ((t (:foreground "#FFFFFF" :background "#0A0701"))))
                        '(cursor ((t (:background "#FFFFFF"))))
                        '(mode-line ((t (:foreground "#FFFFFF" :background "#323232"))))
                        '(region ((t (:background "#323232"))))
                        '(font-lock-comment-face ((t (:foreground "#71FFD3"))))
                        '(font-lock-constant-face ((t (:foreground "#A9FA90"))))
                        '(font-lock-builtin-face ((t (:foreground "#A0FF75"))))
                        '(font-lock-function-name-face ((t (:foreground "#1AD01A"))))
                        '(font-lock-variable-name-face ((t (:foreground "#37FFA2"))))
                        '(font-lock-keyword-face ((t (:foreground "#52FFBF"))))
                        '(font-lock-string-face ((t (:foreground "#88FF65"))))
                        '(font-lock-doc-string-face ((t (:foreground "#88FF65"))))
                        '(font-lock-type-face ((t (:foreground "#91FFEB"))))
                        )

(provide-theme 'verdantparty)
