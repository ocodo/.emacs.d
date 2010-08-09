(defun color-theme-almost-monokai ()
  (interactive)
  (color-theme-install
   '(color-theme-almost-monokai
      ((background-color . "#1f1f1f")
      (background-mode . dark)
      (border-color . "#072231")
      (cursor-color . "#dad085")
      (foreground-color . "#f8f8f2")
      (mouse-color . "black"))
     (fringe ((t (:background "#072231"))))
     (mode-line ((t (:foreground "#ffffff" :background "#20425a"))))
     (region ((t (:background "#154966"))))
     (font-lock-builtin-face ((t (:foreground "#a6e22a"))))
     (font-lock-comment-face ((t (:foreground "#ffffff"))))
     (font-lock-function-name-face ((t (:foreground "#f1266f"))))
     (font-lock-keyword-face ((t (:foreground "#66d9ef"))))
     (font-lock-string-face ((t (:foreground "#dfd874"))))
     (font-lock-type-face ((t (:foreground"#ffffff"))))
     (font-lock-constant-face ((t (:foreground "#a6e22a"))))
     (font-lock-variable-name-face ((t (:foreground "#a6e22a"))))
     (minibuffer-prompt ((t (:foreground "#ffffff" :bold t))))
     (font-lock-warning-face ((t (:foreground "red" :bold t))))
     )))
(provide 'color-theme-almost-monokai)