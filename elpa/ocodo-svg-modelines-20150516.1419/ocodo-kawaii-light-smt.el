;;; ocodo-kawaii-light-smt --- Yet another attempt at a super cool modeline for Emacs
;;
;; Author: ocodo <what.is.ocodo@gmail.com>
;; Package-Requires: ((svg-mode-line-themes "0"))
;; Version: 0.1.4
;; URL: https://github.com/ocodo/ocodo-svg-modelines
;;
;;; Commentary:
;;
;; Made with the svg-mode-line-themes toolkit from Sabof.
;; https://github.com/sabof/svg-mode-line-themes
;;
;;; Code:

(require 'ocodo-smt-overrides)

(defvar ocodo-kawaii-light-folder
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar ocodo-kawaii-light-images
  (concat ocodo-kawaii-light-folder "images/"))

(defvar ocodo-kawaii-light-fileurl-prefix
  (concat "file://" ocodo-kawaii-light-folder))

(defvar ocodo-kawaii-light-graphic
  (concat "data:image/png;base64,"
          (ocodo-smt-string-from-file
           (concat ocodo-kawaii-light-images
                   "rainbow-stache-banana.png.base64"))))

(smt/defrow ocodo-kawaii-light-row-left
  :margin 7
  :always-visible t
  :widgets '(buffer-info buffer-name buffer-dirty)
  :align "left")

(smt/defrow ocodo-kawaii-light-row-right
  :margin 1
  :always-visible t
  :widgets '(position-info)
  :align "right")

(smt/defrow ocodo-kawaii-light-row-mid
  :margin 25
  :always-visible t
  :widgets '(major-mode version-control minor-modes)
  :align "right")

(defun ocodo-kawaii-light-smt-background (theme)
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
        (stop :offset "100%" :stop-color ,bg-gradient-dark :stop-opacity 0.2)))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "#FFFFFF")
      ;; Moustache Banana... inject your own cuteness/darkness/blanditude here
      (image :x -18 :y -12 :width 75 :height 75
             :xlink:href ,ocodo-kawaii-light-graphic))))

(defun ocodo-kawaii-light-smt-overlay (theme)
  (ocodo-smt-overlay theme))

(defun ocodo-kawaii-light-buffer-name-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#000000" "#777777")))

(defun ocodo-kawaii-light-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#000000" "#777777")))

(defun ocodo-kawaii-light-info-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#000000" "#777777")))

(defun ocodo-kawaii-light-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#000000" "#777777")))

(defun ocodo-kawaii-light-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#E5B7B7")
                ;; Untouched
                (if (smt/window-active-p) "#3d7058" "#A3CCA9"))))

(defun ocodo-kawaii-light-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :fill (if (smt/window-active-p) "#000000" "#777777")))

(defun ocodo-kawaii-light-version-control-style (widget)
  (list :font-weight "bold"
        :font-size "8pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#5D3D70" "#777777")))

(smt/deftheme ocodo-kawaii-light-smt
  :pixel-height 24
  :background 'ocodo-kawaii-light-smt-background
  :overlay    'ocodo-kawaii-light-smt-overlay
  :local-widgets
  ;;; Note order of widgets are determined by smt/defrows above, not here.
  (list (cons 'major-mode
              (smt/make-widget
               :prototype 'major-mode
               :style 'ocodo-kawaii-light-major-mode-style))

        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'ocodo-kawaii-light-minor-mode-style))

        (cons 'version-control
              (smt/make-widget
               :prototype 'version-control
               :style 'ocodo-kawaii-light-version-control-style))

        (cons 'position-info
              (smt/make-widget
               :prototype 'position-info
               :style 'ocodo-kawaii-light-position-info-style))

        (cons 'buffer-info
              (smt/make-widget
               :prototype 'buffer-info
               :style 'ocodo-kawaii-light-info-style))

        (cons 'buffer-dirty
              (smt/make-widget
               :prototype 'buffer-dirty
               :style 'ocodo-kawaii-light-dirty-style))

        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'ocodo-kawaii-light-buffer-name-style)))

  :rows (list 'ocodo-kawaii-light-row-left
              'ocodo-kawaii-light-row-mid
              'ocodo-kawaii-light-row-right))

(ocodo-smt-setup 17 "Menlo")
(provide 'ocodo-kawaii-light-smt)

;; Hi-lock: (("(\\(smt/[^ ]*\\)" (1 ' font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (hi-lock-mode)
;; End:

;;; ocodo-kawaii-light-smt.el ends here
