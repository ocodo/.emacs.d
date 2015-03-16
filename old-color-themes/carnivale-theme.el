(deftheme carnivale
  "carnivale")

(custom-theme-set-variables 'carnivale)

(custom-theme-set-faces 'carnivale
                        '(default                      ((t (:foreground "#FFFFFF" :background "#000500" ))))
                        '(cursor                       ((t (                      :background "#FFFFFF" ))))
                        '(mode-line                    ((t (:foreground "#FFFFFF" :background "#323232" ))))
                        '(region                       ((t (                      :background "#323232" ))))
                        '(font-lock-comment-face       ((t (:foreground "#7EFFC6"                       ))))
                        '(font-lock-constant-face      ((t (:foreground "#E255FF"                       ))))
                        '(font-lock-builtin-face       ((t (:foreground "#FF8876"                       ))))
                        '(font-lock-function-name-face ((t (:foreground "#2F8F78"                       ))))
                        '(font-lock-variable-name-face ((t (:foreground "#FF8DA6"                       ))))
                        '(font-lock-keyword-face       ((t (:foreground "#FFF03E"                       ))))
                        '(font-lock-string-face        ((t (:foreground "#FFDA58"                       ))))
                        '(font-lock-doc-string-face    ((t (:foreground "#FFDA58"                       ))))
                        '(font-lock-type-face          ((t (:foreground "#FFC9C6"                       ))))
                        )

(provide-theme 'carnivale)

