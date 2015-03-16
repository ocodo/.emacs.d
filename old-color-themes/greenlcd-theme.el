(deftheme greenlcd "greenlcd")

(custom-theme-set-variables 'greenlcd)

(custom-theme-set-faces 'greenlcd
                        '(cursor                       ((t (:background "#0d6d21"))))
                        '(default                      ((t (:foreground "#ddffdd" :background "#000700"))))
                        '(fringe                       ((t (:background "#002200"))))
                        '(mode-line                    ((t (:foreground "#47ff4e" :background "#0b2d12"))))
                        '(region                       ((t (:background "#002700"))))
                        '(font-lock-builtin-face       ((t (:foreground "#0b8e2f"))))
                        '(font-lock-comment-face       ((t (:foreground "#265f33"))))
                        '(font-lock-function-name-face ((t (:foreground "#60c376"))))
                        '(font-lock-keyword-face       ((t (:foreground "#0abd34"))))
                        '(font-lock-string-face        ((t (:foreground "#aea"))))
                        '(font-lock-type-face          ((t (:foreground"#1f8e3a"))))
                        '(font-lock-constant-face      ((t (:foreground "#1ae94c"))))
                        '(font-lock-variable-name-face ((t (:foreground "#0ebe46"))))
                        '(minibuffer-prompt            ((t (:foreground "#00fa7f" :bold t))))
                        '(font-lock-warning-face       ((t (:foreground "Red" :bold t))))
                        )

(provide-theme 'greenlcd)

