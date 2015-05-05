;;; ocodo-mesh-grass-smt --- Yet another attempt at a super cool modeline for Emacs
;;
;;; Commentary:
;;
;; Made with the svg-mode-line-themes toolkit from Sabof.
;; https://github.com/sabof/svg-mode-line-themes
;;
;;; Code:

(require 'ocodo-smt-overrides)

(setq ocodo-mesh-grass:fileurl-prefix
      (concat "file://" (file-name-directory (or load-file-name buffer-file-name))))

(smt/defrow default-left
  :margin 0.5
  :padding-left (lambda (row) (frame-char-width))
  :padding-right (lambda (row) (frame-char-width))
  :always-visible t
  :background (lambda (row)
                `(rect :y 2
                       :width ,(* (frame-char-width) (smt/r-width row))
                       :height 19
                       :rx 7
                       :fill "#000000"
                       :fill-opacity 0.7))
  :overlay (lambda (row)
             `(rect :y 3
                    :width ,(* (frame-char-width) (smt/r-width row))
                    :height 19
                    :rx 7
                    :fill "#FFFFFF"
                    :fill-opacity 0.1))
  :widgets '(buffer-info buffer-name buffer-dirty)
  :align "left")

(smt/defrow default-position
  :margin 6
  :padding-left (lambda (row) (frame-char-width))
  :padding-right (lambda (row) (frame-char-width))
  :always-visible t
  :background (lambda (row)
                `(g :fill "#5f8a4c"
                  (rect :x 0 :y 3
                         :width ,(* (smt/r-width row) (frame-char-size))
                         :height 1
                         :fill-opacity 1)
                  (rect :x 0 :y 3
                         :width ,(* (smt/r-width row) (frame-char-size))
                         :height 19
                         :fill-opacity 0.3)))
  :widgets '(position-info)
  :align "right")

(smt/defrow default-right
  :margin 25
  :padding-left (lambda (row) (frame-char-width))
  :padding-right (lambda (row) (frame-char-width))
  :always-visible t
  :background (lambda (row)
                `(g :fill "#5F8A4C"
                  (rect :x 0 :y 3
                         :width ,(* (smt/r-width row) (frame-char-size))
                         :height 1
                         :fill-opacity 1)
                  (rect :x 0 :y 3
                         :width ,(* (smt/r-width row) (frame-char-size))
                         :height 19
                         :fill-opacity 0.3)))

  :widgets '(major-mode version-control minor-modes)
  :align "right")

(defun ocodo-mesh-grass:smt/background (theme)
  (ocodo:smt/edge-image theme
                        (concat
                         ocodo-mesh-aqua:fileurl-prefix
                         "images/mesh-grass.svg")))

(defun ocodo-mesh-grass:smt/overlay (theme)
  (ocodo:smt/overlay theme))

(defun smt/ocodo-mesh-grass-buffer-name-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun smt/ocodo-mesh-grass-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#AAAAAA" "#666666")))

(defun smt/ocodo-mesh-grass-info-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#999999" "#555555")))

(defun smt/ocodo-mesh-grass-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :fill (if (smt/window-active-p) "#DDDDDD" "#999999")))

(defun smt/ocodo-mesh-grass-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "9pt"
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p) "#1F4F25" "#143519"))))

(defun smt/ocodo-mesh-grass-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun smt/ocodo-mesh-grass-version-control-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#60B18C" "#3D7058")))

(smt/ocodo-mesh-grass-major-mode-style 'major-mode)
(smt/ocodo-mesh-grass-major-mode-style 'minor-modes)
(smt/ocodo-mesh-grass-major-mode-style 'version-control)
(smt/ocodo-mesh-grass-major-mode-style 'position-info)
(smt/ocodo-mesh-grass-major-mode-style 'buffer-name)
(smt/ocodo-mesh-grass-major-mode-style 'buffer-info)
(smt/ocodo-mesh-grass-major-mode-style 'buffer-dirty)

(smt/deftheme ocodo-mesh-grass:smt
  :pixel-height 26
  :background 'ocodo-mesh-grass:smt/background
  :overlay    'ocodo-mesh-grass:smt/overlay
  :local-widgets
  ;;; Note order of widgets are determined by smt/defrows above.
  (list (cons 'major-mode
              (smt/make-widget
               :prototype 'major-mode
               :style 'smt/ocodo-mesh-grass-major-mode-style))

        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'smt/ocodo-mesh-grass-minor-mode-style))

        (cons 'version-control
              (smt/make-widget
               :prototype 'version-control
               :style 'smt/ocodo-mesh-grass-version-control-style))

        (cons 'position-info
              (smt/make-widget
               :prototype 'position-info
               :style 'smt/ocodo-mesh-grass-position-info-style))

        (cons 'buffer-info
              (smt/make-widget
               :prototype 'buffer-info
               :style 'smt/ocodo-mesh-grass-info-style))

        (cons 'buffer-dirty
              (smt/make-widget
               :prototype 'buffer-dirty
               :style 'smt/ocodo-mesh-grass-dirty-style))

        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'smt/ocodo-mesh-grass-buffer-name-style)))

  :rows (list 'default-left 'default-position 'default-right))

(ocodo:smt/setup 15 "Menlo")

(provide 'ocodo-mesh-grass-smt)

;; Hi-lock: (("(\\(smt/[^ ]*\\)" (1 ' font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (hi-lock-mode)
;; End:

;;; ocodo-mesh-grass-smt.el ends here
