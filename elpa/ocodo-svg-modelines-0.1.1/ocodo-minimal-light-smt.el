;;; ocodo-minimal-light-smt --- Yet another attempt at a super cool modeline for Emacs
;;
;; Author: ocodo <what.is.ocodo@gmail.com>
;; Package-Requires: ((svg-mode-line-themes))
;; Version: 0.1.1
;; URL: https://github.com/ocodo/ocodo-svg-modelines
;;
;;; Commentary:
;;
;; Made with the svg-mode-line-themes toolkit from Sabof.
;; https://github.com/sabof/svg-mode-line-themes
;;
;;; Code:

(require 'ocodo-smt-overrides)

(defun ocodo-minimal-light:smt/background (theme)
  (let ((width (smt/window-pixel-width))
        (height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "twisted" :x1 "0%" :y1 "0%" :x2 "100%" :y2 "15%"
        (stop :offset "0%"  :stop-color ,"#484848" :stop-opacity 0.2)
        (stop :offset "25%" :stop-color ,"#484848" :stop-opacity 0.2)
        (stop :offset "75%" :stop-color ,"#484848" :stop-opacity 0.2)
        (stop :offset "100%" :stop-color ,"#000000" :stop-opacity 0.2)))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "#FFFFFF")
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#twisted)"))))

(defun ocodo-minimal-light:smt/overlay (theme)
  (ocodo:smt/overlay theme))

(defun smt/ocodo-buffer-name-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :font-family "sans-serif"
        :filter nil
        :fill (if (smt/window-active-p) "#000000" "#777777")))

(defun smt/ocodo-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#000000" "#777777")))

(defun smt/ocodo-info-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#000000" "#777777")))

(defun smt/ocodo-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#000000" "#777777")))

(defun smt/ocodo-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#E5B7B7")
                ;; Untouched
                (if (smt/window-active-p) "#3d7058" "#A3CCA9"))))

(defun smt/ocodo-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :filter nil
        :fill (if (smt/window-active-p) "#000000" "#777777")))

(defun smt/ocodo-version-control-style (widget)
  (list :font-weight "bold"
        :font-size "8pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#5D3D70" "#777777")))

(smt/defrow ocodo-minimal-light:smt-left
  :widgets '(buffer-info buffer-name buffer-dirty)
  :always-visible t
  :margin 1)

(smt/defrow ocodo-minimal-light:smt-mid
  :widgets '(major-mode version-control minor-modes)
  :always-visible t
  :align "right"
  :margin 25)

(smt/defrow ocodo-minimal-light:smt-right
  :widgets '(position-info)
  :always-visible t
  :align "right"
  :margin 1)

(smt/deftheme ocodo-minimal-light:smt
  :pixel-height 24
  :background 'ocodo-minimal-light:smt/background
  :overlay    'ocodo-minimal-light:smt/overlay
  :local-widgets
  ;;; Note order of widgets are determined by smt/defrows above.
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
               :style 'smt/ocodo-buffer-name-style)))

  :rows (list 'ocodo-minimal-light:smt-left
              'ocodo-minimal-light:smt-mid
              'ocodo-minimal-light:smt-right))

(provide 'ocodo-minimal-light-smt)

;; Hi-lock: (("(\\(smt/[^ ]*\\)" (1 ' font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (hi-lock-mode)
;; End:

;;; ocodo-minimal-light-smt.el ends here
