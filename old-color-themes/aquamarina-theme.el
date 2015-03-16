(deftheme aquamarina "aquamarina")

(custom-theme-set-variables 'aquamarina )

(custom-theme-set-faces 'aquamarina
                        '(default                      ((t (:foreground "#FFFFFF" :background "#07050D" ))))
                        '(cursor                       ((t (                      :background "#FFFFFF" ))))
                        '(mode-line                    ((t (:foreground "#FFFFFF" :background "#323232" ))))
                        '(region                       ((t (                      :background "#323232" ))))
                        '(font-lock-comment-face       ((t (:foreground "#00A3FF"                       ))))
                        '(font-lock-constant-face      ((t (:foreground "#2978FF"                       ))))
                        '(font-lock-builtin-face       ((t (:foreground "#02F8FF"                       ))))
                        '(font-lock-function-name-face ((t (:foreground "#60F5FF"                       ))))
                        '(font-lock-variable-name-face ((t (:foreground "#00B7C2"                       ))))
                        '(font-lock-keyword-face       ((t (:foreground "#55BDB5"                       ))))
                        '(font-lock-string-face        ((t (:foreground "#FFFF7C"                       ))))
                        '(font-lock-doc-string-face    ((t (:foreground "#FFFF7C"                       ))))
                        '(font-lock-type-face          ((t (:foreground "#D5E8B1"                       ))))
                        )

(provide-theme 'aquamarina)

