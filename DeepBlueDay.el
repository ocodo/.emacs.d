(deftheme MechanicalTurq
  "MechanicalTurq By: Jasonm23 / Created 2012-06-26.")

(custom-theme-set-faces
 'MechanicalTurq
 '(default 
    ((t 
      (
       :family "Monaco" 
       :foundry "apple" 
       :width normal 
       :height 110 
       :weight normal 
       :slant normal 
       :underline nil 
       :overline nil 
       :strike-through nil 
       :box nil 
       :inverse-video nil 
       :foreground "#fff" 
       :background "#00080a" 
       :stipple nil 
       :inherit nil))))

 '(fixed-pitch 
   ((t (:family "Monospace"))))
 
 '(variable-pitch 
   ((t (:family "Sans Serif"))))

 '(escape-glyph ;; Things like  and other control chars.
   ((((background dark)) 
     (:foreground "#FF7000" :background "#011d2c"))))

 ;; Line Numbers (linum-mode)

 '(linum
   ((t (:background "#001014" :foreground "#000" :box nil :height 85))))

 ;;

 '(fringe ((t (:background "#052e2d"))))

 ;; Cursor

 '(cursor
   ((t (:foreground "#ffffff" :background "#011d2c"))))

 ;; Minibuffer

 '(minibuffer-prompt 
   ((t (:weight bold :foreground "#00aab2"))))

 ;; Region

 '(region 
   ((t (:background "#001015"))))

 ;; Secondary region 

 '(secondary-selection
   ((((class color) (min-colors 88) (background dark)) (:background "#144053"))))

 ;; font-lock - syntax highlighting

 '(font-lock-builtin-face              ((t (:foreground "#508590"))))
 '(font-lock-comment-face              ((t (:foreground "#265f59"))))
 '(font-lock-comment-delimiter-face    ((t (:foreground "#467882" ))))
 '(font-lock-doc-face                  ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face        ((t (:foreground "#60c3be"))))
 '(font-lock-keyword-face              ((t (:foreground "#0abda7"))))
 '(font-lock-negation-char-face        ((t nil)))
 '(font-lock-preprocessor-face         ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face               ((t (:foreground "#aaedee"))))
 '(font-lock-constant-face             ((t (:foreground "#1ae9d7"))))
 '(font-lock-type-face                 ((t (:foreground "#1f8e8a"))))
 '(font-lock-variable-name-face        ((t (:foreground "#0ebeb8"))))
 '(font-lock-warning-face              ((t (:weight bold :foreground "#FF0000"))))

 ;; Hightlight

 '(highlight 
   ((((class color) (min-colors 88) (background light))
     (:background "darkseagreen2")) 
    (((class color) 
      (min-colors 88) 
      (background dark)) 
     (:background "darkolivegreen")) 
    (((class color) (min-colors 16) 
      (background light)) 
     (:background "darkseagreen2")) 
    (((class color) 
      (min-colors 16) 
      (background dark)) 
     (:background "darkolivegreen")) 
    (((class color) 
      (min-colors 8)) 
     (:foreground "#000000" :background "green")) 
    (t (:inverse-video t))))

 '(shadow 
   ((((class color grayscale) 
      (min-colors 88) 
      (background light)) 
     (:foreground "grey50")) 
    (((class color grayscale)
      (min-colors 88)
      (background dark)) 
     (:foreground "grey70"))
    (((class color) 
      (min-colors 8) 
      (background light)) 
     (:foreground "green"))
    (((class color)
      (min-colors 8)
      (background dark)) 
     (:foreground "#FFFF00"))))

 '(trailing-whitespace
   ((((class color) 
      (background light))
     (:background "red1"))
    (((class color)
      (background dark))
     (:background "red1")) (t (:inverse-video t))))


 '(link ((((class color) (min-colors 88) 
           (background light)) (:underline t :foreground "#00b7f0")) (((class color)
           (background light)) (:underline t :foreground "#0044FF")) (((class color) (min-colors 88)
           (background dark))  (:underline t :foreground "#0099aa")) (((class color) 
           (background dark))  (:underline t :foreground "#0099aa")) (t (:inherit (underline)))))

 '(link-visited ((default (:inherit (link))) (((class color) 
           (background light)) (:foreground "#")) (((class color) 
           (background dark)) (:foreground "violet"))))

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

 '(mode-line ((t (:background "#0b2c2d" :foreground "#9cf6f4" :height 85))))

 '(mode-line-buffer-id ((t (:weight bold))))

 '(mode-line-emphasis ((t (:weight bold))))

 '(mode-line-highlight 
   ((((class color) 
      (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))

 '(mode-line-inactive
   ((t (:weight light :box nil :foreground "grey80" :background "grey30" :inherit (mode-line)))))

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
     (:background "#FFFF00"))
    (((class color)
      (min-colors 88)
      (background dark))
     (:background "RoyalBlue3"))
    (((class color)
      (min-colors 8)
      (background light))
     (:foreground "#000000" :background "#FFFF00"))
    (((class color)
      (min-colors 8)
      (background dark))
     (:foreground "#ffffff" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))

 '(next-error
   ((t (:inherit (region)))))

 '(query-replace
   ((t (:inherit (isearch)))))


)

(provide-theme 'MechanicalTurq)


