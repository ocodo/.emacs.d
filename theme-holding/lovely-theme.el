(deftheme lovely
    "lovely")

(custom-theme-set-variables
  'lovely
     '(powerline-color1 "#00779a")
     '(powerline-color2 "#00475a")
     '(linum-format " %7i ")
     '(fringe-mode 6 nil (fringe))
     )

(custom-theme-set-faces
  `lovely
  `(font-lock-comment-face       ((t (:foreground "#008DCA"))))
  `(font-lock-constant-face      ((t (:foreground "#00E19F"))))
  `(font-lock-builtin-face       ((t (:foreground "#00BFFF"))))
  `(font-lock-function-name-face ((t (:foreground "#2EFE3C"))))
  `(font-lock-variable-name-face ((t (:foreground "#62FFA3"))))
  `(font-lock-keyword-face       ((t (:foreground "#51FF93"))))
  `(font-lock-string-face        ((t (:foreground "#1994C1"))))
  `(font-lock-doc-string-face    ((t (:foreground "#1994C1"))))
  `(font-lock-type-face          ((t (:foreground "#25DB23"))))
)
;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name                                                                                                             ))))

(provide-theme 'lovely)
