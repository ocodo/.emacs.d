;;; ocodo-mesh-aqua-smt --- Yet another attempt at a super cool modeline for Emacs
;;
;;; Commentary:
;;
;; Made with the svg-mode-line-themes toolkit from Sabof.
;; https://github.com/sabof/svg-mode-line-themes
;;
;;; Code:

(require 'ocodo-smt-overrides)

(setq ocodo-mesh-aqua:fileurl-prefix (concat "file://" (file-name-directory (or load-file-name buffer-file-name))))

(defun ocodo-mesh-aqua:smt/background (theme)
  (let* ((mesh-aqua (concat ocodo-mesh-aqua:fileurl-prefix "mesh-aqua.svg"))
         (width (smt/window-pixel-width))
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
      (image :x -50 :y 0px
             :width 98.625961 :height 26.125
             :xlink:href ,mesh-aqua)
      (image :x ,(- width 35) :y 0
             :width 98.625961 :height 26.125
             :xlink:href ,mesh-aqua))))

(defun ocodo-mesh-aqua:smt/overlay (theme)
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

(defun smt/ocodo-mesh-aqua-buffer-name-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :filter nil
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun smt/ocodo-mesh-aqua-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#AAAAAA" "#666666")))

(defun smt/ocodo-mesh-aqua-info-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#999999" "#555555")))

(defun smt/ocodo-mesh-aqua-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :filter nil
        :fill (if (smt/window-active-p) "#DDDDDD" "#999999")))

(defun smt/ocodo-mesh-aqua-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p) "#1F4F25" "#143519"))))

(defun smt/ocodo-mesh-aqua-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :filter nil
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun smt/ocodo-mesh-aqua-version-control-style (widget)
  (list :font-weight "bold"
        :font-size "8pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#60B18C" "#666666")))

(smt/defrow default-left
  :widgets '(buffer-info buffer-name buffer-dirty)
  :margin 8)

(smt/defrow default-position
  :widgets '(position-info)
  :align "right"
  :margin 6)

(smt/defrow default-right
  :widgets '(major-mode version-control minor-modes)
  :align "right"
  :margin 25)

(smt/deftheme ocodo-mesh-aqua:smt
  :pixel-height 26
  :background 'ocodo-mesh-aqua:smt/background
  :overlay    'ocodo-mesh-aqua:smt/overlay
  :local-widgets
  ;;; Note order of widgets are determined by smt/defrows above.
  (list (cons 'major-mode
              (smt/make-widget
               :prototype 'major-mode
               :style 'smt/ocodo-mesh-aqua-major-mode-style))

        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'smt/ocodo-mesh-aqua-minor-mode-style))

        (cons 'version-control
              (smt/make-widget
               :prototype 'version-control
               :style 'smt/ocodo-mesh-aqua-version-control-style))

        (cons 'position-info
              (smt/make-widget
               :prototype 'position-info
               :style 'smt/ocodo-mesh-aqua-position-info-style))

        (cons 'buffer-info
              (smt/make-widget
               :prototype 'buffer-info
               :style 'smt/ocodo-mesh-aqua-info-style))

        (cons 'buffer-dirty
              (smt/make-widget
               :prototype 'buffer-dirty
               :style 'smt/ocodo-mesh-aqua-dirty-style))

        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'smt/ocodo-mesh-aqua-buffer-name-style)))

  :rows (list 'default-left 'default-position 'default-right))

(ocodo:smt/setup 17 "Menlo")
(provide 'ocodo-mesh-aqua-smt)

;; Hi-lock: (("(\\(smt/[^ ]*\\)" (1 ' font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (hi-lock-mode)
;; End:

;;; ocodo-mesh-aqua-smt.el ends here
