(deftheme smoothy "smoothy")

(custom-theme-set-variables 'smoothy)


(custom-theme-set-faces 'smoothy
                        '(default ((t (:foreground "#FFFFFF" :background "#000000"))))
                        '(cursor ((t (:background "#FFFFFF"))))
                        '(mode-line ((t (:foreground "#FFFFFF" :background "#323232"))))
                        '(region ((t (:background "#323232"))))
                        '(font-lock-comment-face ((t (:foreground "#208598"))))
                        '(font-lock-constant-face ((t (:foreground "#A9430C"))))
                        '(font-lock-builtin-face ((t (:foreground "#7F669E"))))
                        '(font-lock-function-name-face ((t (:foreground "#C8FFBE"))))
                        '(font-lock-variable-name-face ((t (:foreground "#4DB021"))))
                        '(font-lock-keyword-face ((t (:foreground "#BFA155"))))
                        '(font-lock-string-face ((t (:foreground "#5BC500"))))
                        '(font-lock-doc-string-face ((t (:foreground "#5BC500"))))
                        '(font-lock-type-face ((t (:foreground "#DE5E12"))))
                        )

(provide-theme 'smoothy)

