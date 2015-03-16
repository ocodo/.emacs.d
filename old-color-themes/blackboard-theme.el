(deftheme blackboard "blackboard")

(custom-theme-set-variables 'blackboard )

(custom-theme-set-faces 'blackboard
                        '(default                      ((t ( :foreground "#f8f8f8" :background "#0c1021" ))))
                        '(cursor                       ((t (                       :background "#a7a7a7" ))))
                        '(fringe                       ((t (                       :background "#021b1d" ))))
                        '(mode-line                    ((t ( :foreground "#ffffff" :background "#143743" ))))
                        '(region                       ((t (                       :background "#253b76" ))))
                        '(font-lock-builtin-face       ((t ( :foreground "#f8f8f8"                       ))))
                        '(font-lock-comment-face       ((t ( :foreground "#547a7d"                       ))))
                        '(font-lock-function-name-face ((t ( :foreground "#ff6400"                       ))))
                        '(font-lock-keyword-face       ((t ( :foreground "#fbde2d"                       ))))
                        '(font-lock-string-face        ((t ( :foreground "#61ce3c"                       ))))
                        '(font-lock-type-face          ((t ( :foreground"#ffffff"                        ))))
                        '(font-lock-constant-face      ((t ( :foreground "#43addf"                       ))))
                        '(font-lock-variable-name-face ((t ( :foreground "#ff6400"                       ))))
                        '(minibuffer-prompt            ((t ( :foreground "#3ab7e9" :bold t               ))))
                        '(font-lock-warning-face       ((t ( :foreground "red" :bold t                   ))))
                        )

(provide-theme 'blackboard)

