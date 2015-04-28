;;; ocodo-kawaii-light-smt --- Yet another attempt at a super cool modeline for Emacs
;;
;;; Commentary:
;;
;; Made with the svg-mode-line-themes toolkit from Sabof.
;; https://github.com/sabof/svg-mode-line-themes
;;
;;; Code:

(require 'svg-mode-line-themes)
(require 'ocodo-smt-overrides)

(smt/enable)

(defun ocodo-kawaii-light:smt/background (theme)
  (let ((bg-gradient-dark "#000000")
        (bg-gradient-main "#484848")
        (width (smt/window-pixel-width))
        (height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "twisted" :x1 "0%" :y1 "0%" :x2 "100%" :y2 "15%"
        (stop :offset "0%"  :stop-color ,bg-gradient-main :stop-opacity 0.2)
        (stop :offset "25%" :stop-color ,bg-gradient-main :stop-opacity 0.2)
        (stop :offset "75%" :stop-color ,bg-gradient-main :stop-opacity 0.2)
        (stop :offset "100%" :stop-color ,bg-gradient-dark :stop-opacity 0.2))
       (filter
        :id "blur"
        (feGaussianBlur
         :stdDeviation "5")
        (feComposite)))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "#FFFFFF")
      ;; Moustache Banana... inject your own cuteness/darkness/blanditude here
      (image :x -18 :y -12 :width 75 :height 75
             :xlink:href "http://i.imgur.com/Sv9cC3J.png"))))

(defun ocodo-kawaii-light:smt/overlay (theme)
  (let ((overlay-dark "#000000")
        (overlay-light "#FFFFFF")
        (width (smt/window-pixel-width))
        (height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "over-gradient" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset 0 :stop-color ,overlay-light :stop-opacity 0.2)
        (stop :offset 1 :stop-color ,overlay-dark :stop-opacity 0.5)))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#over-gradient)"))))

(defun smt/ocodo-kawaii-light-buffer-name-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :font-family "sans-serif"
        :filter nil
        :fill (if (smt/window-active-p) "#000000" "#333333")))

(defun smt/ocodo-kawaii-light-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#000000" "#333333")))

(defun smt/ocodo-kawaii-light-info-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#000000" "#333333")))

(defun smt/ocodo-kawaii-light-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#000000" "#333333")))

(defun smt/ocodo-kawaii-light-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p) "#3d7058" "#143519"))))

(defun smt/ocodo-kawaii-light-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :filter nil
        :fill (if (smt/window-active-p) "#000000" "#333333")))

(defun smt/ocodo-kawaii-light-version-control-style (widget)
  (list :font-weight "bold"
        :font-size "8pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#5D3D70" "#333333")))

(smt/defrow default-left
  :widgets '(buffer-info buffer-name buffer-dirty)
  :margin 8)

(smt/defrow default-right
  :widgets '(major-mode version-control minor-modes)
  :align "right"
  :margin 25)

(smt/defrow default-position
  :widgets '(position-info)
  :align "right"
  :margin 1)

(smt/deftheme ocodo-kawaii-light:smt
  :pixel-height 24
  :background 'ocodo-kawaii-light:smt/background
  :overlay    'ocodo-kawaii-light:smt/overlay
  :local-widgets
  ;;; Note order of widgets are determined by smt/defrows above, not here.
  (list (cons 'major-mode
              (smt/make-widget
               :prototype 'major-mode
               :style 'smt/ocodo-kawaii-light-major-mode-style))

        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'smt/ocodo-kawaii-light-minor-mode-style))

        (cons 'version-control
              (smt/make-widget
               :prototype 'version-control
               :style 'smt/ocodo-kawaii-light-version-control-style))

        (cons 'position-info
              (smt/make-widget
               :prototype 'position-info
               :style 'smt/ocodo-kawaii-light-position-info-style))

        (cons 'buffer-info
              (smt/make-widget
               :prototype 'buffer-info
               :style 'smt/ocodo-kawaii-light-info-style))

        (cons 'buffer-dirty
              (smt/make-widget
               :prototype 'buffer-dirty
               :style 'smt/ocodo-kawaii-light-dirty-style))

        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'smt/ocodo-kawaii-light-buffer-name-style)))

  :rows (list 'default-left 'default-right 'default-position))

(ocodo:smt/setup 17 "Menlo")
(provide 'ocodo-kawaii-light-smt)

;; Hi-lock: (("(\\(smt/[^ ]*\\)" (1 ' font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (hi-lock-mode)
;; End:

;;; ocodo-kawaii-light-smt.el ends here
