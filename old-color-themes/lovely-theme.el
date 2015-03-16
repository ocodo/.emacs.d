(deftheme lovely "lovely")

(custom-theme-set-variables 'lovely)

(custom-theme-set-faces `lovely
                        '(default                      ((t (:foreground "#FFFFFF" :background "#100000"))))
                        '(cursor                       ((t (:background "#FFFFFF"))))
                        '(mode-line                    ((t (:foreground "#FFFFFF" :background "#323232"))))
                        '(region                       ((t (:background "#323232"))))
                        '(font-lock-comment-face       ((t (:foreground "#008DCA"))))
                        '(font-lock-constant-face      ((t (:foreground "#00E19F"))))
                        '(font-lock-builtin-face       ((t (:foreground "#00BFFF"))))
                        '(font-lock-function-name-face ((t (:foreground "#2EFE3C"))))
                        '(font-lock-variable-name-face ((t (:foreground "#62FFA3"))))
                        '(font-lock-keyword-face       ((t (:foreground "#51FF93"))))
                        '(font-lock-string-face        ((t (:foreground "#1994C1"))))
                        '(font-lock-doc-string-face    ((t (:foreground "#1994C1"))))
                        '(font-lock-type-face          ((t (:foreground "#25DB23"))))
                        )

(provide-theme 'lovely)

