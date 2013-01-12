(deftheme deep-blue-day
  "deep-blue-day By: Jasonm23 / Created 2012-06-26.")

  (custom-theme-set-variables
   'deep-blue-day
   '(fringe-mode 6 nil (fringe))
   '(linum-format " %7d ")
   '(powerline-color1 "#00468a")
   '(powerline-color2 "#00172a")
   )

  (custom-theme-set-faces
   'deep-blue-day

   '(default 
      ((t (:foreground "#ffffff" :background "#00060A" ))))

   '(fixed-pitch 
     ((t (:family "Monospace"))))
   
   '(variable-pitch 
     ((t (:family "Sans Serif"))))

   '(escape-glyph ;; Things like  and other control chars.
     ((t (:foreground "#FF6600" :background "#011d3c"))))

   ;; Line Numbers (linum-mode)
   '(linum
     ((t (:background "#001526" :foreground "#003047" :box nil :height 100))))

   ;; Margin Fringes
   '(fringe
     ((t ( :background "#001526" :Foreground "#006080" ))))

   ;; Mode-line / status line
   '(mode-line 
     ((t (:background "#0b283d" :box nil :foreground "#0c86e4" :height 85))))

   '(mode-line-inactive
     ((t (:weight light :box nil :background "#002339" :foreground "#000000" :inherit (mode-line)))))
   '(mode-line-emphasis 
     ((t (:weight bold))))

   '(mode-line-highlight 
     ((t (:box nil (t (:inherit (highlight)))))))

   '(mode-line-buffer-id
     ((t (:weight bold :box nil))))

   ;; Cursor
   '(cursor
     ((t (:foreground "#ffffff" :background "orange"))))

   ;; Minibuffer
   '(minibuffer-prompt 
     ((t (:weight bold :foreground "#006a92"))))

   '(minibuffer-message 
     ((t (:foreground "#ffffff"))))

   ;; Region
   '(region 
     ((t (:background "#00002f"))))

   ;; Secondary region 
   '(secondary-selection
     ((((class color) (min-colors 88) (background dark)) (:background "#144083"))))

   ;; font-lock - syntax 
   '(font-lock-builtin-face              ((t (:foreground "#206590"))))
   '(font-lock-comment-face              ((t (:foreground "#205f89"))))
   '(font-lock-comment-delimiter-face    ((t (:foreground "#2078a2" ))))
   '(font-lock-doc-face                  ((t (:inherit (font-lock-string-face)))))
   '(font-lock-function-name-face        ((t (:foreground "#3083be"))))
   '(font-lock-keyword-face              ((t (:foreground "#0a8da7"))))
   '(font-lock-negation-char-face        ((t nil)))
   '(font-lock-preprocessor-face         ((t (:inherit (font-lock-builtin-face)))))
   '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
   '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
   '(font-lock-string-face               ((t (:foreground "#00adee"))))
   '(font-lock-constant-face             ((t (:foreground "#0a99e7"))))
   '(font-lock-type-face                 ((t (:foreground "#1f5e8a"))))
   '(font-lock-variable-name-face        ((t (:foreground "#0e8eb8"))))
   '(font-lock-warning-face              ((t (:weight bold :foreground "#FF0000"))))

   ;; Hightlight
   '(highlight 
     ((((class color) (min-colors 88) (background light))
       (:background "#003453")) 
      (((class color) 
        (min-colors 88) 
        (background dark)) 
       (:background "#003450")) 
      (((class color) (min-colors 16) 
        (background light)) 
       (:background "#003450")) 
      (((class color) 
        (min-colors 16) 
        (background dark)) 
       (:background "#004560")) 
      (((class color) 
        (min-colors 8)) 
       (:foreground "#000000" :background "#00FF00")) 
      (t (:inverse-video t))))

   '(shadow 
     ((((class color grayscale) 
        (min-colors 88) 
        (background light)) 
       (:foreground "#999999")) 
      (((class color grayscale)
        (min-colors 88)
        (background dark)) 
       (:foreground "#999999"))
      (((class color) 
        (min-colors 8) 
        (background light)) 
       (:foreground "#00ff00"))
      (((class color)
        (min-colors 8)
        (background dark)) 
       (:foreground "#ffff00"))))

   '(trailing-whitespace
     ((((class color) 
        (background light))
       (:background "#ff0000"))
      (((class color)
        (background dark))
       (:background "#ff0000")) (t (:inverse-video t))))


   '(link ((((class color) (min-colors 88) 
       (background light)) (:underline t :foreground "#00b7f0")) (((class color)                                                                         
       (background light)) (:underline t :foreground "#0044FF")) (((class color) (min-colors 88) 
       (background dark))  (:underline t :foreground "#0099aa")) (((class color) 
       (background dark))  (:underline t :foreground "#0099aa")) (t (:inherit (underline)))))

   '(link-visited ((default (:inherit (link))) (((class color) 
       (background light)) (:inherit (link))) (((class color) 
       (background dark)) (:inherit (link)))))

   '(button ((t (:inherit (link)))))

   '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline t :inverse-video nil)) 
       (((class color grayscale) 
         (background light)) (:box nil :foreground "#222222" :background "#bbbbbb")) 
       (((class color grayscale) 
         (background dark)) (:box nil :foreground "#bbbbbb" :background "#222222")) 
       (((class mono) 
         (background light)) (:underline t :box nil :inverse-video nil :foreground "#000000" :background "#ffffff")) 
       (((class mono) 
         (background dark)) (:underline t :box nil :inverse-video nil :foreground "#ffffff" :background "#000000"))))

   '(tooltip ((default nil) (nil nil)))

   '(isearch
     ((((class color) (min-colors 88) 
        (background light)) (:foreground "#99ccee" :background "#444444")) 
      (((class color) (min-colors 88) 
        (background dark)) (:foreground "#bb3311" :background "##444444")) 
      (((class color) (min-colors 16)) (:foreground "#0088cc" :background "#444444"))
      (((class color) (min-colors 8)) (:foreground "#0088cc" :background "#444444")) (t (:inverse-video t))))

   '(isearch-fail
     ((((class color)
        (min-colors 88)
        (background light))
       (:background "#ffaaaa"))
      (((class color)
        (min-colors 88)
        (background dark))
       (:background "#880000"))
      (((class color)
        (min-colors 16))
       (:background "#FF0000"))
      (((class color)
        (min-colors 8))
       (:background "#FF0000"))
      (((class color grayscale))
       (:foreground "#888888")) (t (:inverse-video t))))

   '(lazy-highlight
     ((((class color)
        (min-colors 88)
        (background light))
       (:background "#77bbdd"))
      (((class color)
        (min-colors 88)
        (background dark))
       (:background "#77bbdd"))
      (((class color)
        (min-colors 16))
       (:background "#4499ee"))
      (((class color)
        (min-colors 8))
       (:background "#4499ee")) (t (:underline t))))

   '(match
     ((((class color)
        (min-colors 88)
        (background light))
       (:background "#3388cc"))
      (((class color)
        (min-colors 88)
        (background dark))
       (:background "#3388cc"))
      (((class color)
        (min-colors 8)
        (background light))
       (:foreground "#000000" :background "#FFFF00"))
      (((class color)
        (min-colors 8)
        (background dark))
       (:foreground "#ffffff" :background "#0000FF")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "#888888"))))

   '(next-error
     ((t (:inherit (region)))))

   '(query-replace
     ((t (:inherit (isearch)))))

   )

(provide-theme 'deep-blue-day)


