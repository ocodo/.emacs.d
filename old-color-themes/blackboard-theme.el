;;(defun color-theme-blackboard ()
;;  (interactive)
;;  (color-theme-install
;;   '(color-theme-blackboard
;;      ((background-color . "#0c1021")
;;      (background-mode . light)
;;      (border-color . "#021b1d")
;;      (cursor-color . "#a7a7a7")
;;      (foreground-color . "#f8f8f8")
;;      (mouse-color . "black"))
;;     (fringe ((t (:background "#021b1d"))))
;;     (mode-line ((t (:foreground "#ffffff" :background "#143743"))))
;;     (region ((t (:background "#253b76"))))
;;     (font-lock-builtin-face ((t (:foreground "#f8f8f8"))))
;;     (font-lock-comment-face ((t (:foreground "#547a7d"))))
;;     (font-lock-function-name-face ((t (:foreground "#ff6400"))))
;;     (font-lock-keyword-face ((t (:foreground "#fbde2d"))))
;;     (font-lock-string-face ((t (:foreground "#61ce3c"))))
;;     (font-lock-type-face ((t (:foreground"#ffffff"))))
;;     (font-lock-constant-face ((t (:foreground "#43addf"))))
;;     (font-lock-variable-name-face ((t (:foreground "#ff6400"))))
;;     (minibuffer-prompt ((t (:foreground "#3ab7e9" :bold t))))
;;     (font-lock-warning-face ((t (:foreground "red" :bold t))))
;;     )))
;;(provide 'color-theme-blackboard)

(deftheme blackboard
    "blackboard")

(custom-theme-set-variables
  'blackboard
     '(powerline-color1 "#00779a")
     '(powerline-color2 "#00475a")
     '(linum-format " %7i ")
     '(fringe-mode 6 nil (fringe))
     )


(custom-theme-set-faces
  'blackboard

)


(provide-theme 'blackboard)

