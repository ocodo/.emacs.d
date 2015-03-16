(deftheme mossysparks "mossysparks")

(custom-theme-set-variables 'mossysparks)

(custom-theme-set-faces 'mossysparks
                        '(default ((t (:foreground "#FFFFFF" :background "#021100"))))
                        '(cursor ((t (:background "#FFFFFF"))))
                        '(mode-line ((t (:foreground "#FFFFFF" :background "#323232"))))
                        '(region ((t (:background "#323232"))))
                        '(font-lock-comment-face ((t (:foreground "#5BEDA7"))))
                        '(font-lock-constant-face ((t (:foreground "#60EAB9"))))
                        '(font-lock-builtin-face ((t (:foreground "#21FF66"))))
                        '(font-lock-function-name-face ((t (:foreground "#54FFF0"))))
                        '(font-lock-variable-name-face ((t (:foreground "#CCC904"))))
                        '(font-lock-keyword-face ((t (:foreground "#8BFF97"))))
                        '(font-lock-string-face ((t (:foreground "#72B084"))))
                        '(font-lock-doc-string-face ((t (:foreground "#72B084"))))
                        '(font-lock-type-face ((t (:foreground "#3FFF46"))))
                        )

(provide-theme 'mossysparks)

