(deftheme wholegrain "wholegrain")

(custom-theme-set-variables
 'wholegrain

 '(powerline-color1 "#333333")
 '(powerline-color2 "#232323")
 '(linum-format " %7i ")
 '(fringe-mode 6 nil (fringe))
 )

(custom-theme-set-faces
 'wholegrain

 '(default 
    ((t 
      (:foreground 
       "#ffffff" 
       :background
       "#000000"))))
 
 '(fixed-pitch 
   ((t (:family "Monospace"))))
 
 '(variable-pitch 
   ((t (:family "Sans Serif"))))
 
 '(escape-glyph
   ((t (:foreground "#FF6600" :background "#011d2c"))))
 
 '(mode-line ((t (:foreground "#FFFFFF" :background "#323232"))))
 '(mode-line-inactive
   ((t (:weight light :box nil :background "#232323" :foreground "#555555" :inherit (mode-line)))))
 '(mode-line-emphasis 
   ((t (:weight bold))))

 '(mode-line-highlight 
   ((t (:box nil (t (:inherit (highlight)))))))

 '(region ((t (:background "#202020"))))
 '(cursor ((t (:background "#232323"))))

 '(minibuffer-prompt 
   ((t (:weight bold :foreground "#9489C4"))))

 '(minibuffer-message 
   ((t (:foreground "#ffffff"))))

 '(fringe 
   ((t ( :background "#1f1f1f" :Foreground "#ffffff" ))))

 '(linum
   ((t (:background "#101010" :foreground "#323232" :box nil :height 100))))

 '(font-lock-type-face          ((t (:foreground "#3AA2C4"))))
 '(font-lock-keyword-face       ((t (:foreground "#9CBAC4"))))
 '(font-lock-constant-face      ((t (:foreground "#89A0C4"))))
 '(font-lock-variable-name-face ((t (:foreground "#9489C4"))))
 '(font-lock-builtin-face       ((t (:foreground "#6975B0"))))
 '(font-lock-string-face        ((t (:foreground "#5F7E89"))))
 '(font-lock-comment-face       ((t (:foreground "#445A62"))))
 '(font-lock-function-name-face ((t (:foreground "#75C494"))))
 '(font-lock-doc-string-face    ((t (:foreground "#85C475"))))
)

(provide-theme 'wholegrain)
