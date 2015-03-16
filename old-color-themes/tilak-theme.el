(deftheme tilak "tilak")

(custom-theme-set-variables 'tilak)

(custom-theme-set-faces 'tilak
                        '(default ((t (:foreground "#ffffff" :background "#000d00"))))
                        '(cursor ((t (:background "#ffffff"))))
                        '(fringe ((t (:background "#0b1f23"))))
                        '(mode-line ((t (:foreground "#ffffff" :background "#093643"))))
                        '(region ((t (:background "#323232"))))
                        '(font-lock-builtin-face ((t (:foreground "#9cd2ff"))))
                        '(font-lock-comment-face ((t (:foreground "#acfff9"))))
                        '(font-lock-function-name-face ((t (:foreground "#ffaf6c"))))
                        '(font-lock-keyword-face ((t (:foreground "#78f6e3"))))
                        '(font-lock-string-face ((t (:foreground "#e0e8cc"))))
                        '(font-lock-type-face ((t (:foreground"#ffffff"))))
                        '(font-lock-constant-face ((t (:foreground "#a1ffff"))))
                        '(font-lock-variable-name-face ((t (:foreground "#bfa986"))))
                        '(minibuffer-prompt ((t (:foreground "#27ecdc" :bold t))))
                        '(font-lock-warning-face ((t (:foreground "red" :bold t))))
                        )

(provide-theme 'tilak)

