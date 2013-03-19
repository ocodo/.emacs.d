(require 'svg-mode-line-themes-core)

(defun smt/diesel-title-style (widget)
  `(:fill ,(if (smt/window-active-p)
               "#D4A535"
               "#4C5055")
          :font-weight "bold"))

(defun smt/diesel-bg (theme)
  (let (( width (smt/window-pixel-width))
        ( height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "grad1" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset "0%" :style "stop-color:rgb(255,255,255);stop-opacity:0.1")
        (stop :offset "100%" :style "stop-color:rgb(0,0,0);stop-opacity:0.1"))
       (linearGradient
        :id "grad2" :x1 "0%" :y1 "0%" :x2 "100%" :y2 "0%"
        (stop :offset "0%" :style "stop-color:rgb(255,255,255);stop-opacity:0.0")
        (stop :offset "50%" :style "stop-color:rgb(255,255,255);stop-opacity:0.2")
        (stop :offset "100%" :style "stop-color:rgb(255,255,255);stop-opacity:0.0")))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "#666")
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad1)")
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#grad2)")
      )))

(defun smt/diesel-overlay (theme)
  (let (( width (smt/window-pixel-width))
        ( height (smt/t-pixel-height theme)))
    `((rect :width "100%" :height 1 :x 0 :y 0 :fill "white" :fill-opacity 0.3)
      (rect :width "100%" :height 1 :x 0 :y ,(- height 2) :fill "black" :fill-opacity 0.2)
      (rect :width "100%" :height 1 :x 0 :y ,(1- height) :fill "black" :fill-opacity 0.6)
      )))

(defun smt/diesel-major-mode-style (widget)
  `(:fill
    "#ccc"
    :font-family "Georgia, Serif"
    :font-style "italic"
    :filter nil
    :font-weight "bold"
    ))

(smt/deftheme diesel
  :defs (smt/filter-inset 0.5 0.3)
  :background 'smt/diesel-bg
  :style
  (lambda (theme)
    (smt/combine-styles
     (smt/t-style (smt/t-prototype theme))
     `(:filter
       "url(#inset)"
       :fill "#b7c3cd")))
  :local-widgets
  (list (cons 'major-mode
              (smt/make-widget
               :prototype 'major-mode
               :style 'smt/diesel-major-mode-style))
        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'smt/diesel-title-style))
        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'smt/diesel-title-style)))
  :rows (list 'default-left 'default-position 'default-right)
  :overlay 'smt/diesel-overlay)

(provide 'svg-mode-line-themes-diesel)
;; svg-mode-line-themes-diesel.el ends here
