(require 'svg-mode-line-themes-core)

(defun smt/nasa-title-style (widget)
  (list :font-weight "bold"
        :filter nil
        :fill (if (smt/window-active-p)
                  "#2B25E6"
                  "#404347")))

(defun smt/nasa-background (theme)
  (let (( width (smt/window-pixel-width))
        ( height (smt/t-pixel-height theme)))
    `((\defs
       (filter
        :id "blur"
        (feGaussianBlur
         :stdDeviation "5")
        (feComposite)))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "#888")
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "#eee" :filter "url(#blur)")
      (rect :width "100%" :height 1 :x 0 :y 0 :fill "black" :fill-opacity 0.3)
      (rect :width "100%" :height 1 :x 0 :y ,(1- height) :fill "black" :fill-opacity 0.3)
      )))

(defun smt/nasa-overlay (theme)
  (let (( width (smt/window-pixel-width))
        ( height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "grad2" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset "0%" :style "stop-color:#fff;stop-opacity:0.6")
        (stop :offset "66%" :style "stop-color:#888;stop-opacity:0.6")
        (stop :offset "100%" :style "stop-color:#222;stop-opacity:0.5")
        ))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad2)" )
      )))

(defun smt/nasa-major-mode-style (widget)
  (smt/combine-styles (smt/diesel-major-mode-style nil)
         (list :fill "#DC1A0C")))

(smt/deftheme nasa
  :defs (smt/filter-inset 0 1)
  :background 'smt/nasa-background
  :style
  (lambda (theme)
    (smt/combine-styles
     (smt/t-style (smt/t-prototype theme))
     `(:filter
       "url(#inset)"
       :fill "#404448")))
  :local-widgets
  (list (cons 'major-mode
              (smt/make-widget
               :prototype 'major-mode
               :style 'smt/nasa-major-mode-style))
        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'smt/nasa-title-style))
        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'smt/nasa-title-style)))
  :rows (list 'default-left 'default-position 'default-right)
  :overlay 'smt/nasa-overlay)

(provide 'svg-mode-line-themes-nasa)
;;; svg-mode-line-themes-nasa.el ends here
