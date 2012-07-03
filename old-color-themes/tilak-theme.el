;;(defun color-theme-tilak ()
;;  (interactive)
;;  (color-theme-install
;;   '(color-theme-tilak
;;      ((background-color . "#000d00")
;;      (background-mode . light)
;;      (border-color . "#0b1f23")
;;      (cursor-color . "#ffffff")
;;      (foreground-color . "#ffffff")
;;      (mouse-color . "black"))
;;     (fringe ((t (:background "#0b1f23"))))
;;     (mode-line ((t (:foreground "#ffffff" :background "#093643"))))
;;     (region ((t (:background "#323232"))))
;;     (font-lock-builtin-face ((t (:foreground "#9cd2ff"))))
;;     (font-lock-comment-face ((t (:foreground "#acfff9"))))
;;     (font-lock-function-name-face ((t (:foreground "#ffaf6c"))))
;;     (font-lock-keyword-face ((t (:foreground "#78f6e3"))))
;;     (font-lock-string-face ((t (:foreground "#e0e8cc"))))
;;     (font-lock-type-face ((t (:foreground"#ffffff"))))
;;     (font-lock-constant-face ((t (:foreground "#a1ffff"))))
;;     (font-lock-variable-name-face ((t (:foreground "#bfa986"))))
;;     (minibuffer-prompt ((t (:foreground "#27ecdc" :bold t))))
;;     (font-lock-warning-face ((t (:foreground "red" :bold t))))
;;     )))
;;(provide 'color-theme-tilak)

(deftheme tilak
    "tilak")

(custom-theme-set-variables
  'tilak
     '(powerline-color1 "#00779a")
     '(powerline-color2 "#00475a")
     '(linum-format " %7i ")
     '(fringe-mode 6 nil (fringe))
     )


(custom-theme-set-faces
  'tilak

)


(provide-theme 'tilak)

