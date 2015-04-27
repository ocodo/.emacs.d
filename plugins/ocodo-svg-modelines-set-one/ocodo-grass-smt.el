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

(require 'ocodo-smt-overrides)

(when load-file-name (setq ocodo-grass-smt-fileurl (concat "file://" (file-name-directory load-file-name))))

(defun ocodo-grass:smt/background (theme)
  (let* ((mesh-left (concat ocodo-grass-smt-fileurl "mesh-grass-left.svg"))
         (mesh-right (concat ocodo-grass-smt-fileurl "mesh-grass-right.svg"))
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
      (image :xlink:href ,mesh-left)
      (image :x ,(- width 79)
             :xlink:href ,mesh-right)
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
  :margin 7)

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

  :rows (list 'default-left 'default-position 'default-right))

(smt/enable)

(ocodo:smt/setup 17 "Menlo")

(smt/set-theme 'ocodo-grass:smt)

(provide 'ocodo-grass-smt)

;; Hi-lock: (("(\\(smt/[^ ]*\\)" (1 ' font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (hi-lock-mode)
;; End:

;;; ocodo-grass-smt.el ends here
