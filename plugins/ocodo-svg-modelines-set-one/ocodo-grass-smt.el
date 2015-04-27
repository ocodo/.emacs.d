;;; ocodo-grass-smt --- Yet another attempt at a super cool modeline for Emacs

;;; Commentary:

;; Work in progress, check commit history for variations.

;; Made with the svg-mode-line-themes toolkit from Sabof.
;; https://github.com/sabof/svg-mode-line-themes

;; Hopefully a skillful Emacs user will see the correlation of xmlgen
;; and svg below (esp. ocodo-grass:smt/background) and from there, with some
;; careful visual work, can pull any old thing from Inkscape and make
;; a nice adornment to their modeline.

;; Note you will be able to include external svg/png (and other
;; format) images using the (image ) svg tag.

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
(require 'ocodo-smt-overrides)

;;;###autoload
(defun ocodo-grass-svg-modeline ()
  (interactive)
  (smt/enable)
  (let ((theme (cdr (assoc 'archetype smt/themes)))
        (row (cdr (assoc 'archetype smt/rows))))
    ;; *******************************************************
    ;;  Customise to use your desired default monospaced font
    ;; *******************************************************
    (setf (getf theme :style) (list :font-size "10pt" :font-family "Menlo"))

    (setf (getf row :baseline) 17))
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (smt/set-theme 'ocodo-grass:smt))


(defun ocodo-grass:smt/background (theme)
  (let ((width (smt/window-pixel-width))
        (height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "twisted" :x1 "0%" :y1 "0%" :x2 "100%" :y2 "25%"
        (stop :offset "0%"  :style "stop-color:40
;stop-opacity:0.3")
        (stop :offset "25%"  :style "stop-color:#484848;stop-opacity:0.3")
        (stop :offset "75%"  :style "stop-color:#484848;stop-opacity:0.3")
        (stop :offset "100%"  :style "stop-color:#000000;stop-opacity:0.3"))
       (filter
        :id "blur"
        (feGaussianBlur
         :stdDeviation "5")
        (feComposite)))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#twisted)" :fill-opacity 1)
      (rect :width "100%" :height 2 :x 0 :y 0 :fill "#383838" :fill-opacity 0.2)

      (g :transform "translate(-38,0)"
         (g :transform "matrix(1.25,0,0,-1.25,0,32)"
            (g :transform "matrix(-0.14833729,0,0,0.14833729,105.25468,-47.37267)"
               (g :transform "translate(299.9707,494.4888)" (path :fill "#9DB66C" :fill-opacity 0.7 :d "m 0,0 -39.209,-61.232 83.939,0 L 5.521,0 0,0 z" ))
               (g :transform "translate(305.4917,494.4888)" (path :fill "#97B261" :fill-opacity 0.7 :d "M 0,0 39.209,-61.232 78.418,0 0,0 z" ))
               (g :transform "translate(383.9101,494.4888)" (path :fill "#AABD86" :fill-opacity 0.7 :d "m 0,0 -39.209,-61.232 83.938,0 L 5.521,0 0,0 z" ))
               (g :transform "translate(389.431,494.4888)"  (path :fill "#AABF80" :fill-opacity 0.7 :d "M 0,0 39.208,-61.232 78.417,0 0,0 z" ))
               (g :transform "translate(467.8478,494.4888)" (path :fill "#BBE26E" :fill-opacity 0.7 :d "m 0,0 -39.208,-61.232 83.937,0 L 5.521,0 0,0 z" ))
               (g :transform "translate(473.3686,494.4888)" (path :fill "#B7E65A" :fill-opacity 0.7 :d "M 0,0 39.208,-61.232 78.417,0 0,0 z" ))
               (g :transform "translate(551.7853,494.4888)" (path :fill "#AFE04D" :fill-opacity 0.7 :d "m 0,0 -39.208,-61.232 83.937,0 L 5.521,0 0,0 z" ))
               (g :transform "translate(557.3062,494.4888)" (path :fill "#B9F247" :fill-opacity 0.7 :d "M 0,0 39.208,-61.232 78.418,0 0,0 z" ))
               (g :transform "translate(635.7247,494.4888)" (path :fill "#B3E550" :fill-opacity 0.7 :d "m 0,0 -39.21,-61.232 83.938,0 L 5.521,0 0,0 z" ))
               (g :transform "translate(641.2455,494.4888)" (path :fill "#98C146" :fill-opacity 0.7 :d "M 0,0 39.208,-61.232 68.317,-15.771 68.317,0 0,0 z" ))
               (g :transform "translate(680.4531,433.2564)" (path :fill "#8CB638" :fill-opacity 0.7 :d "m 0,0 29.11,0 0,45.461 L 0,0 z" ))

               (g :transform "translate(344.7012,433.2563)" (path :fill "#5F8F00" :fill-opacity 0.7 :d "M 0,0 -41.97,-65.545 -83.939,0 0,0 z" ))
               (g :transform "translate(302.731,367.7109)"  (path :fill "#7F9F3F" :fill-opacity 0.7 :d "M 0,0 41.97,65.545 83.94,0 0,0 z" ))
               (g :transform "translate(428.6396,433.2563)" (path :fill "#97AA72" :fill-opacity 0.7 :d "M 0,0 -41.969,-65.545 -83.938,0 0,0 z" ))
               (g :transform "translate(386.6709,367.7109)" (path :fill "#A6B785" :fill-opacity 0.7 :d "M 0,0 41.969,65.545 83.938,0 0,0 z" ))
               (g :transform "translate(512.5771,433.2563)" (path :fill "#B4CB87" :fill-opacity 0.7 :d "M 0,0 -41.969,-65.545 -83.938,0 0,0 z" ))
               (g :transform "translate(470.6084,367.7109)" (path :fill "#C0E773" :fill-opacity 0.7 :d "M 0,0 41.969,65.545 83.938,0 0,0 z" ))
               (g :transform "translate(596.5146,433.2563)" (path :fill "#CBF676" :fill-opacity 0.7 :d "M 0,0 -41.969,-65.545 -83.938,0 0,0 z" ))
               (g :transform "translate(554.5459,367.7109)" (path :fill "#BCEE58" :fill-opacity 0.7 :d "M 0,0 41.969,65.545 83.939,0 0,0 z" ))
               (g :transform "translate(680.4531,433.2563)" (path :fill "#9EC74D" :fill-opacity 0.7 :d "M 0,0 -41.968,-65.545 -83.938,0 0,0 z" ))
               (g :transform "translate(638.4852,367.7112)" (path :fill "#8CB143" :fill-opacity 0.7 :d "m 0,0 71.078,0 0,20.083 L 41.968,65.545 0,0 z" ))
               (g :transform "translate(680.4531,433.2564)" (path :fill "#749631" :fill-opacity 0.7 :d "M 0,0 29.11,-45.462 29.11,0 0,0 z" ))

               (g :transform "translate(271.7697,319.3578)" (path :fill "#517A00" :fill-opacity 0.7 :d "M 0,0 61.923,0 30.962,48.353 0,0 z" ))
               (g :transform "translate(302.7312,367.7112)" (path :fill "#547E00" :fill-opacity 0.7 :d "m 0,0 30.961,-48.353 22.017,0 L 83.939,0 0,0 z" ))
               (g :transform "translate(355.7091,319.3578)" (path :fill "#90A469" :fill-opacity 0.7 :d "M 0,0 61.922,0 30.961,48.353 0,0 z" ))
               (g :transform "translate(386.6706,367.7112)" (path :fill "#A3B285" :fill-opacity 0.7 :d "m 0,0 30.961,-48.353 22.016,0 L 83.938,0 0,0 z" ))
               (g :transform "translate(439.6474,319.3578)" (path :fill "#AEC680" :fill-opacity 0.7 :d "M 0,0 61.922,0 30.961,48.353 0,0 z" ))
               (g :transform "translate(470.6082,367.7112)" (path :fill "#B1D766" :fill-opacity 0.7 :d "m 0,0 30.961,-48.353 22.016,0 L 83.938,0 0,0 z" ))
               (g :transform "translate(523.585,319.3578)"  (path :fill "#C1EE67" :fill-opacity 0.7 :d "M 0,0 61.922,0 30.961,48.353 0,0 z" ))
               (g :transform "translate(554.5458,367.7112)" (path :fill "#A9D651" :fill-opacity 0.7 :d "m 0,0 30.961,-48.353 22.016,0 L 83.939,0 0,0 z" ))
               (g :transform "translate(607.523,319.3578)"  (path :fill "#98C148" :fill-opacity 0.7 :d "M 0,0 61.922,0 30.962,48.353 0,0 z" ))
               (g :transform "translate(638.4852,367.7112)" (path :fill "#7D9B41" :fill-opacity 0.7 :d "m 0,0 30.96,-48.353 22.016,0 18.102,28.27 L 71.078,0 0,0 z" ))
               (g :transform "translate(691.4611,319.3578)" (path :fill "#698336" :fill-opacity 0.7 :d "m 0,0 18.102,0 0,28.27 L 0,0 z" )))))

      (g :transform ,(format "translate(%i,0)" (- width 80))
         (g :transform "matrix(1.25,0,0,-1.25,0,32)"
            (g :transform "matrix(0.14833729,0,0,0.14833729,31.537529,25.978459)"    (path :fill "#9DB66C" :fill-opacity 0.7 :d "M 0,0 39.208,-61.232 78.417,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,43.16965,25.978459)"     (path :fill "#97B261" :fill-opacity 0.7 :d "m 0,0 -39.208,-61.232 83.937,0 L 5.521,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,43.988605,25.978459)"    (path :fill "#AABD86" :fill-opacity 0.7 :d "M 0,0 39.208,-61.232 78.418,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,55.620993,25.978459)"    (path :fill "#AABF80" :fill-opacity 0.7 :d "m 0,0 -39.21,-61.232 83.938,0 L 5.521,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,56.439934,25.978459)"    (path :fill "#BBE26E" :fill-opacity 0.7 :d "M 0,0 39.208,-61.232 68.317,-15.771 68.317,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,62.255883,16.89541)"     (path :fill "#B7E65A" :fill-opacity 0.7 :d "m 0,0 29.11,0 0,45.461 L 0,0 z"))

            (g :transform "matrix(0.14833729,0,0,0.14833729,31.128089,7.1725684)"    (path :fill "#5F8F00" :fill-opacity 0.7 :d "M 0,0 41.969,65.545 83.938,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,49.804673,16.895395)"    (path :fill "#7F9F3F" :fill-opacity 0.7 :d "M 0,0 -41.969,-65.545 -83.938,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,43.57915,7.1725684)"     (path :fill "#97AA72" :fill-opacity 0.7 :d "M 0,0 41.969,65.545 83.939,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,62.255883,16.895395)"    (path :fill "#A6B785" :fill-opacity 0.7 :d "M 0,0 -41.968,-65.545 -83.938,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,56.030478,7.1726129)"    (path :fill "#B4CB87" :fill-opacity 0.7 :d "m 0,0 71.078,0 0,20.083 L 41.968,65.545 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,62.255883,16.89541)"     (path :fill "#C0E773" :fill-opacity 0.7 :d "M 0,0 29.11,-45.462 29.11,0 0,0 z"))

            (g :transform "matrix(0.14833729,0,0,0.14833729,31.128059,7.1726129)"    (path :fill "#547E00" :fill-opacity 0.7 :d "m 0,0 30.961,-48.353 22.016,0 L 83.938,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,38.986494,5.9236201e-7)" (path :fill "#90A469" :fill-opacity 0.7 :d "M 0,0 61.922,0 30.961,48.353 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,43.579135,7.1726129)"    (path :fill "#A3B285" :fill-opacity 0.7 :d "m 0,0 30.961,-48.353 22.016,0 L 83.939,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,51.437629,5.9236201e-7)" (path :fill "#AEC680" :fill-opacity 0.7 :d "M 0,0 61.922,0 30.962,48.353 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,56.030478,7.1726129)"    (path :fill "#B1D766" :fill-opacity 0.7 :d "m 0,0 30.96,-48.353 22.016,0 18.102,28.27 L 71.078,0 0,0 z"))
            (g :transform "matrix(0.14833729,0,0,0.14833729,63.88878,5.9236201e-7)"  (path :fill "#C1EE67" :fill-opacity 0.7 :d "m 0,0 18.102,0 0,28.27 L 0,0 z"))))
      )))

(defun ocodo-grass:smt/overlay (theme)
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

(defun smt/ocodo-grass-buffer-name-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :filter nil
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun smt/ocodo-grass-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#AAAAAA" "#666666")))

(defun smt/ocodo-grass-info-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#999999" "#555555")))

(defun smt/ocodo-grass-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :filter nil
        :fill (if (smt/window-active-p) "#DDDDDD" "#999999")))

(defun smt/ocodo-grass-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p) "#1F4F25" "#143519"))))

(defun smt/ocodo-grass-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :filter nil
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun smt/ocodo-grass-version-control-style (widget)
  (list :font-weight "bold"
        :font-size "8pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#60B18C" "#666666")))

(smt/defrow default-left
  :widgets '(buffer-info buffer-name buffer-dirty)
  :margin 6)

(smt/defrow default-position
  :widgets '(position-info)
  :align "right"
  :margin 6)

(smt/defrow default-right
  :widgets '(major-mode version-control minor-modes)
  :align "right"
  :margin 25)

(smt/deftheme ocodo-grass:smt
  :pixel-height 26
  :background 'ocodo-grass:smt/background
  :overlay    'ocodo-grass:smt/overlay
  :local-widgets
  ;;; Note order of widgets are determined by smt/defrows above.
  (list (cons 'major-mode
              (smt/make-widget
               :prototype 'major-mode
               :style 'smt/ocodo-grass-major-mode-style))

        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'smt/ocodo-grass-minor-mode-style))

        (cons 'version-control
              (smt/make-widget
               :prototype 'version-control
               :style 'smt/ocodo-grass-version-control-style))

        (cons 'position-info
              (smt/make-widget
               :prototype 'position-info
               :style 'smt/ocodo-grass-position-info-style))

        (cons 'buffer-info
              (smt/make-widget
               :prototype 'buffer-info
               :style 'smt/ocodo-grass-info-style))

        (cons 'buffer-dirty
              (smt/make-widget
               :prototype 'buffer-dirty
               :style 'smt/ocodo-grass-dirty-style))

        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'smt/ocodo-grass-buffer-name-style)))

  :rows (list
         'default-left
         'default-position
         'default-right))


(provide 'ocodo-grass-smt)

;; Hi-lock: (("(\\(smt/def[^ ]*\\)" (1 'font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (hi-lock-mode)
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; ocodo-grass-smt.el ends here
