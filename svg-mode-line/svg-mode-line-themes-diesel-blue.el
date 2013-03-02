
(defun smt/diesel-blue-title-style (widget)
  (smt/combine-styles
   (smt/t-style (smt/t-prototype widget))
   (list :fill (if (smt/window-active-p)
                   "#21D0EE"
                   "#4C5055")
         :font-weight "bold")))

(smt/deftheme diesel-blue
  :prototype 'diesel
  :local-widgets
  (lambda (theme)
    (let (( parent-local-widgets
            (smt/t-local-widgets
             (smt/t-prototype theme))))
      (append (list (cons 'buffer-name
                          (smt/make-widget
                           :prototype 'buffer-name
                           :style 'smt/diesel-blue-title-style))
                    (cons 'minor-modes
                          (smt/make-widget
                           :prototype 'minor-modes
                           :style 'smt/diesel-blue-title-style)))
              parent-local-widgets))))

(provide 'svg-mode-line-themes-diesel-blue)
