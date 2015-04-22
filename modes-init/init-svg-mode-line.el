
;;; Commentary:

;; Yet another one of my half assed attempts at a super cool modeline for Emacs, this one using svg-mode-line-themes.

;;; Code:

(smt/enable)

(defun smt/buffer-indicators-text (widget)
  (let ((indicators
         (concat
          (if buffer-read-only "R/O" "R/W")
          (if (buffer-modified-p) "*" "")
          " ")
         ))

    (if (< 1 (length indicators))
        indicators
        "")))

(defun smt/minor-mode-indicator-text (widget)
  (let (( text
          (concat
           (when defining-kbd-macro "M")
           (when (bound-and-true-p aai-mode) "I")
           (when (or (bound-and-true-p evil-local-mode)
                     (bound-and-true-p evil-mode)) "E")
           (when truncate-lines "T")
           (when (bound-and-true-p dired-omit-mode) "O")
           (when (bound-and-true-p save-auto-hook) "A")
           (when (/= (- (point-max) (point-min)) (buffer-size)) "N")
           (when (bound-and-true-p wmi) "M")
           ;; Small letters are used for temporary modes
           (when (bound-and-true-p multiple-cursors-mode) "m")
           (when (bound-and-true-p iedit-mode) "i")
           )))
    (if (plusp (length text))
        (concat " " text)
        "")))


(defun smt/ocodo-background (theme)
  (let ((width (smt/window-pixel-width))
        (height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "twisted" :x1 "0%" :y1 "0%" :x2 "100%" :y2 "25%"
        (stop :offset "0%"  :style "stop-color:#000000;stop-opacity:0.3")
        (stop :offset "25%"  :style "stop-color:#484848;stop-opacity:0.3")
        (stop :offset "75%"  :style "stop-color:#484848;stop-opacity:0.3")
        (stop :offset "100%"  :style "stop-color:#000000;stop-opacity:0.3"))
       (filter
        :id "blur"
        (feGaussianBlur
         :stdDeviation "5")
        (feComposite)))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#twisted)" :fill-opacity 1)
      (rect :width "100%" :height 1 :x 0 :y height :fill "#383838" :fill-opacity 1))))

(defun smt/ocodo-overlay (theme)
  (let ((width (smt/window-pixel-width))
        (height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "over-gradient" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset "0%" :style "stop-color:#000000;stop-opacity:0.0")
        (stop :offset "90%" :style "stop-color:#000000;stop-opacity:0.3")
        (stop :offset "100%" :style "stop-color:#000000;stop-opacity:0.6")))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#over-gradient)")
      )))

(defun smt/ocodo-title-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :font-family "Menlo"
        :filter nil
        :fill (if (smt/window-active-p)
                  "#FFFFFF"
                  "#666666")))

(defun smt/ocodo-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :filter nil
        :font-family "Helvetica"
        :fill (if (smt/window-active-p)
                  "#FFFFFF"
                  "#666666")))

(defun smt/ocodo-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :filter nil
        :font-family "Helvetica"
        :fill (if (smt/window-active-p)
                  "#FFFFFF"
                  "#666666")))

(smt/deftheme ocodo
  :defs (smt/filter-inset 0 1)
  :background 'smt/ocodo-background
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
               :style 'smt/ocodo-major-mode-style))

        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'smt/ocodo-minor-mode-style))

        (cons 'version-control
              (smt/make-widget
               :prototype 'version-control
               :style 'smt/ocodo-minor-mode-style))

        (cons 'position-info
              (smt/make-widget
               :prototype 'position-info
               :style 'smt/ocodo-minor-mode-style))

        (cons 'buffer-info
              (smt/make-widget
               :prototype 'buffer-info
               :style 'smt/ocodo-title-style))

        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'smt/ocodo-title-style)))
  :rows (list 'default-left 'default-position 'default-right)
  :overlay 'smt/ocodo-overlay)

(let (( theme-archetype (cdr (assoc 'archetype smt/themes)))
      ( row-archetype (cdr (assoc 'archetype smt/rows))))
  (setf (getf theme-archetype :style)
        (list :font-family "Menlo"
              :font-size "10pt"))
  (setf (getf row-archetype :baseline) 14))

(smt/set-theme 'ocodo)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(provide 'init-svg-mode-line)
