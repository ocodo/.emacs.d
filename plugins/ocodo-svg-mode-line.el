;;; ocodo-svg-mode-line --- Yet another attempt at a super cool modeline for Emacs

;;; Commentary:

;; Made with the svg-mode-line-themes toolkit from Sabof.
;; https://github.com/sabof/svg-mode-line-themes

;; Hopefully a skillful Emacs user will see the correlation of xmlgen
;; and svg below (esp. ocodo:smt/background) and from there, with some
;; careful visual work, can pull any old thing from Inkscape and make
;; a nice adornment to their modeline.

;; Obviously this could proliferate to nyan-cats, hello kitty and
;; other pop culture iconography all over people's modelines... (no
;; comment on that, but I expect it will be fun for a moment.)

;; Please this is provided as an example / inspiration, there won't be
;; support etc. (not for now at least)

;; To install, put this file in your emacs load-path and do (require
;; 'ocodo-svg-mode-line) in your .emacs

;; I hope you enjoy it.

;; Please Note: I am using Menlo as my default named font, other
;; styles use the generic "sans-serif" font pointer.

;;; Code:

(require 'svg-mode-line-themes)

(smt/enable)

(defun smt/buffer-indicators-text (widget)
  (if buffer-read-only " RO " " RW "))

(defun smt/buffer-name-text (widget)
  (format-mode-line "%b"))

(defun smt/minor-mode-indicator-text (widget)
  (concat
     (when defining-kbd-macro                             " REC ")
     (when (bound-and-true-p projectile-mode)             " Prj ")
     (when (bound-and-true-p projectile-rails-mode)       " Rails ")
     (when (bound-and-true-p smartparens-mode)            " [S] ")
     (when (or (bound-and-true-p evil-local-mode)
               (bound-and-true-p evil-mode))              " Evil ")
     (when (bound-and-true-p dired-omit-mode)             " O ")
     (when (bound-and-true-p rainbow-mode)                " Rbow ")
     (when (bound-and-true-p global-auto-revert-mode)     " ARv ")
     (when (bound-and-true-p visual-line-mode)            " Vl ")
     (when (bound-and-true-p multiple-cursors-mode)       " Mc ")
     (when (bound-and-true-p iedit-mode)                  " iE ")))

(smt/defwidget buffer-dirty
  :text (lambda (widget)
          (if (and
               (buffer-modified-p)
               (or buffer-file-name buffer-offer-save))
              " ❉ " " ✓ ")))

(smt/defwidget position-info
  :text (lambda (widget)
          (format-mode-line "%l:%c [%p] %I"))
  :on-click (lambda (widget event)
              (what-cursor-position t)))

(smt/defwidget major-mode
  :text (lambda (widget)
          (format-mode-line mode-name))
  :on-click (lambda (widget event)
              (message "%s" (format-mode-line mode-line-modes))))

(smt/defrow default-position
  :widgets '(position-info)
  :align "right"
  :margin 5)

(smt/defrow default-left
  :widgets '(buffer-info buffer-name buffer-dirty which-function)
  :margin 10)

(smt/defrow default-right
  :widgets '(major-mode version-control minor-modes)
  :align "right"
  :margin 30)

(defun ocodo:smt/background (theme)
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
      (rect :width "100%" :height 1 :x 0 :y height :fill "#383838" :fill-opacity 1)

      (g :transform ,(format "translate(%i,0)" (- width 80))
         (g :transform "matrix(1.25,0,0,-1.25,0,32)"
            (g :transform "matrix(0.14833729,0,0,0.14833729,31.537529,25.978459)"    (path :fill "#6cb681" :fill-opacity 0.6 :d "M 0,0 39.208,-61.232 78.417,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,43.16965,25.978459)"     (path :fill "#62b286" :fill-opacity 0.6 :d "m 0,0 -39.208,-61.232 83.937,0 L 5.521,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,43.988605,25.978459)"    (path :fill "#aabd86" :fill-opacity 0.6 :d "M 0,0 39.208,-61.232 78.418,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,55.620993,25.978459)"    (path :fill "#bfb980" :fill-opacity 0.6 :d "m 0,0 -39.21,-61.232 83.938,0 L 5.521,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,56.439934,25.978459)"    (path :fill "#e2ae6e" :fill-opacity 0.6 :d "M 0,0 39.208,-61.232 68.317,-15.771 68.317,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,62.255883,16.89541)"     (path :fill "#e69c5e" :fill-opacity 0.6 :d "m 0,0 29.11,0 0,45.461 L 0,0 z"))

            (g :transform "matrix(0.14833729,0,0,0.14833729,31.128089,7.1725684)"    (path :fill "#028f85" :fill-opacity 0.6 :d "M 0,0 41.969,65.545 83.938,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,49.804673,16.895395)"    (path :fill "#3f9f88" :fill-opacity 0.6 :d "M 0,0 -41.969,-65.545 -83.938,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,43.57915,7.1725684)"     (path :fill "#72aa88" :fill-opacity 0.6 :d "M 0,0 41.969,65.545 83.939,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,62.255883,16.895395)"    (path :fill "#a5b785" :fill-opacity 0.6 :d "M 0,0 -41.968,-65.545 -83.938,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,56.030478,7.1726129)"    (path :fill "#c9cb87" :fill-opacity 0.6 :d "m 0,0 71.078,0 0,20.083 L 41.968,65.545 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,62.255883,16.89541)"     (path :fill "#e7bd73" :fill-opacity 0.6 :d "M 0,0 29.11,-45.462 29.11,0 0,0 z"))

            (g :transform "matrix(0.14833729,0,0,0.14833729,31.128059,7.1726129)"    (path :fill "#007d7e" :fill-opacity 0.6 :d "m 0,0 30.961,-48.353 22.016,0 L 83.938,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,38.986494,5.9236201e-7)" (path :fill "#69a489" :fill-opacity 0.6 :d "M 0,0 61.922,0 30.961,48.353 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,43.579135,7.1726129)"    (path :fill "#8bb285" :fill-opacity 0.6 :d "m 0,0 30.961,-48.353 22.016,0 L 83.939,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,51.437629,5.9236201e-7)" (path :fill "#bec680" :fill-opacity 0.6 :d "M 0,0 61.922,0 30.962,48.353 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,56.030478,7.1726129)"    (path :fill "#d7b866" :fill-opacity 0.6 :d "m 0,0 30.96,-48.353 22.016,0 18.102,28.27 L 71.078,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,63.88878,5.9236201e-7)"  (path :fill "#eeb767" :fill-opacity 0.6 :d "m 0,0 18.102,0 0,28.27 L 0,0 z"))))

      (g :transform "matrix(1.25,0,0,-1.25,0,32)"
         (g :transform "matrix(-0.14833729,0,0,0.14833729,105.25468,-47.37267)"
            (g :transform "translate(299.9707,494.4888)" (path :fill "#6cb681" :fill-opacity 0.6  :d "m 0,0 -39.209,-61.232 83.939,0 L 5.521,0 0,0 z" ))
            (g :transform "translate(305.4917,494.4888)" (path :fill "#62b286" :fill-opacity 0.6  :d "M 0,0 39.209,-61.232 78.418,0 0,0 z" ))
            (g :transform "translate(383.9101,494.4888)" (path :fill "#aabd86" :fill-opacity 0.6  :d "m 0,0 -39.209,-61.232 83.938,0 L 5.521,0 0,0 z" ))
            (g :transform "translate(389.431,494.4888)"  (path :fill "#bfb980" :fill-opacity 0.6  :d "M 0,0 39.208,-61.232 78.417,0 0,0 z" ))
            (g :transform "translate(467.8478,494.4888)" (path :fill "#e2ae6e" :fill-opacity 0.6  :d "m 0,0 -39.208,-61.232 83.937,0 L 5.521,0 0,0 z" ))
            (g :transform "translate(473.3686,494.4888)" (path :fill "#e69c5e" :fill-opacity 0.6  :d "M 0,0 39.208,-61.232 78.417,0 0,0 z" ))
            (g :transform "translate(551.7853,494.4888)" (path :fill "#e08951" :fill-opacity 0.6  :d "m 0,0 -39.208,-61.232 83.937,0 L 5.521,0 0,0 z" ))
            (g :transform "translate(557.3062,494.4888)" (path :fill "#f2a047" :fill-opacity 0.6  :d "M 0,0 39.208,-61.232 78.418,0 0,0 z" ))
            (g :transform "translate(635.7247,494.4888)" (path :fill "#e57d50" :fill-opacity 0.6  :d "m 0,0 -39.21,-61.232 83.938,0 L 5.521,0 0,0 z" ))
            (g :transform "translate(641.2455,494.4888)" (path :fill "#c14b47" :fill-opacity 0.6  :d "M 0,0 39.208,-61.232 68.317,-15.771 68.317,0 0,0 z" ))
            (g :transform "translate(680.4531,433.2564)" (path :fill "#b63840" :fill-opacity 0.6  :d "m 0,0 29.11,0 0,45.461 L 0,0 z" ))

            (g :transform "translate(344.7012,433.2563)" (path :fill "#028f85" :fill-opacity 0.6  :d "M 0,0 -41.97,-65.545 -83.939,0 0,0 z" ))
            (g :transform "translate(302.731,367.7109)"  (path :fill "#3f9f88" :fill-opacity 0.6  :d "M 0,0 41.97,65.545 83.94,0 0,0 z" ))
            (g :transform "translate(428.6396,433.2563)" (path :fill "#72aa88" :fill-opacity 0.6  :d "M 0,0 -41.969,-65.545 -83.938,0 0,0 z" ))
            (g :transform "translate(386.6709,367.7109)" (path :fill "#a5b785" :fill-opacity 0.6  :d "M 0,0 41.969,65.545 83.938,0 0,0 z" ))
            (g :transform "translate(512.5771,433.2563)" (path :fill "#c9cb87" :fill-opacity 0.6  :d "M 0,0 -41.969,-65.545 -83.938,0 0,0 z" ))
            (g :transform "translate(470.6084,367.7109)" (path :fill "#e7bd73" :fill-opacity 0.6  :d "M 0,0 41.969,65.545 83.938,0 0,0 z" ))
            (g :transform "translate(596.5146,433.2563)" (path :fill "#f6c576" :fill-opacity 0.6  :d "M 0,0 -41.969,-65.545 -83.938,0 0,0 z" ))
            (g :transform "translate(554.5459,367.7109)" (path :fill "#eea258" :fill-opacity 0.6  :d "M 0,0 41.969,65.545 83.939,0 0,0 z" ))
            (g :transform "translate(680.4531,433.2563)" (path :fill "#c76b4d" :fill-opacity 0.6  :d "M 0,0 -41.968,-65.545 -83.938,0 0,0 z" ))
            (g :transform "translate(638.4852,367.7112)" (path :fill "#b14f43" :fill-opacity 0.6  :d "m 0,0 71.078,0 0,20.083 L 41.968,65.545 0,0 z" ))
            (g :transform "translate(680.4531,433.2564)" (path :fill "#963336" :fill-opacity 0.6  :d "M 0,0 29.11,-45.462 29.11,0 0,0 z" ))

            (g :transform "translate(271.7697,319.3578)" (path :fill "#00707a" :fill-opacity 0.6  :d "M 0,0 61.923,0 30.962,48.353 0,0 z" ))
            (g :transform "translate(302.7312,367.7112)" (path :fill "#007d7e" :fill-opacity 0.6  :d "m 0,0 30.961,-48.353 22.017,0 L 83.939,0 0,0 z" ))
            (g :transform "translate(355.7091,319.3578)" (path :fill "#69a489" :fill-opacity 0.6  :d "M 0,0 61.922,0 30.961,48.353 0,0 z" ))
            (g :transform "translate(386.6706,367.7112)" (path :fill "#8bb285" :fill-opacity 0.6  :d "m 0,0 30.961,-48.353 22.016,0 L 83.938,0 0,0 z" ))
            (g :transform "translate(439.6474,319.3578)" (path :fill "#bec680" :fill-opacity 0.6  :d "M 0,0 61.922,0 30.961,48.353 0,0 z" ))
            (g :transform "translate(470.6082,367.7112)" (path :fill "#d7b866" :fill-opacity 0.6  :d "m 0,0 30.961,-48.353 22.016,0 L 83.938,0 0,0 z" ))
            (g :transform "translate(523.585,319.3578)"  (path :fill "#eeb767" :fill-opacity 0.6  :d "M 0,0 61.922,0 30.961,48.353 0,0 z" ))
            (g :transform "translate(554.5458,367.7112)" (path :fill "#d68651" :fill-opacity 0.6  :d "m 0,0 30.961,-48.353 22.016,0 L 83.939,0 0,0 z" ))
            (g :transform "translate(607.523,319.3578)"  (path :fill "#c16448" :fill-opacity 0.6  :d "M 0,0 61.922,0 30.962,48.353 0,0 z" ))
            (g :transform "translate(638.4852,367.7112)" (path :fill "#9b4641" :fill-opacity 0.6  :d "m 0,0 30.96,-48.353 22.016,0 18.102,28.27 L 71.078,0 0,0 z" ))
            (g :transform "translate(691.4611,319.3578)" (path :fill "#83373c" :fill-opacity 0.6  :d "m 0,0 18.102,0 0,28.27 L 0,0 z" )))))))


(defun ocodo:smt/overlay (theme)
  (let ((width (smt/window-pixel-width))
        (height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "over-gradient" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset "0%" :style "stop-color:#FFFFFF;stop-opacity:0.1")
        (stop :offset "20%" :style "stop-color:#000000;stop-opacity:0.0")
        (stop :offset "90%" :style "stop-color:#000000;stop-opacity:0.5")
        (stop :offset "100%" :style "stop-color:#000000;stop-opacity:0.8")))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#over-gradient)"))))

(defun smt/ocodo-title-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :filter nil
        :fill (if (smt/window-active-p)
                  "#FFFFFF"
                "#666666")))

(defun smt/ocodo-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p)
                  "#AAAAAA"
                "#666666")))

(defun smt/ocodo-info-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p)
                  "#999999"
                "#555555")))

(defun smt/ocodo-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :filter nil
        :fill (if (smt/window-active-p)
                  "#DDDDDD"
                "#999999")))

(defun smt/ocodo-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p)
                      "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p)
                    "#1F4F25" "#143519"))))

(defun smt/ocodo-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :filter nil
        :fill (if (smt/window-active-p)
                  "#FFFFFF"
                "#666666")))

(defun smt/ocodo-version-control-style (widget)
  (list :font-weight "bold"
        :font-size "8pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p)
                  "#60B18C"
                "#666666")))

(smt/deftheme ocodo:smt
  :pixel-height 32
  :background 'ocodo:smt/background
  :overlay    'ocodo:smt/overlay
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
               :style 'smt/ocodo-version-control-style))

        (cons 'position-info
              (smt/make-widget
               :prototype 'position-info
               :style 'smt/ocodo-position-info-style))

        (cons 'buffer-info
              (smt/make-widget
               :prototype 'buffer-info
               :style 'smt/ocodo-info-style))

        (cons 'buffer-dirty
              (smt/make-widget
               :prototype 'buffer-dirty
               :style 'smt/ocodo-dirty-style))

        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'smt/ocodo-title-style)))

  :rows (list
         'default-left
         'default-position
         'default-right))


(let ((theme (cdr (assoc 'archetype smt/themes)))
      (row (cdr (assoc 'archetype smt/rows))))

  ;; Customise to use your desired default font
  (setf (getf theme :style) (list :font-size "10pt" :font-family "Menlo"))

  (setf (getf row :baseline) 19))

(smt/set-theme 'ocodo:smt)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(provide 'ocodo-svg-mode-line)
;;; ocodo-svg-mode-line.el ends here
