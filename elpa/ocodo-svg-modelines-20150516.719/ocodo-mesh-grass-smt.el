;;; ocodo-mesh-grass-smt --- Yet another attempt at a super cool modeline for Emacs
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

(defvar ocodo-mesh-grass-folder
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar ocodo-mesh-grass-images
  (concat ocodo-mesh-grass-folder "images/"))

(defvar ocodo-mesh-grass-fileurl-prefix
  (concat "file://" ocodo-mesh-grass-folder))

(defvar ocodo-mesh-grass-graphic
  (concat "data:image/svg+xml;base64,"
          (ocodo-smt-string-from-file
           (concat ocodo-mesh-grass-images "mesh-grass.svg.base64"))))

(smt/defrow ocodo-mesh-grass-row-left
  :margin 5
  :always-visible t
  :widgets '(buffer-info buffer-name buffer-dirty)
  :align "left")

(smt/defrow ocodo-mesh-grass-row-right
  :margin 5
  :always-visible t
  :widgets '(position-info)
  :align "right")

(smt/defrow ocodo-mesh-grass-row-mid
  :margin 25
  :always-visible t
  :widgets '(major-mode version-control minor-modes)
  :align "right")

(defun ocodo-mesh-grass-smt-background (theme)
  (ocodo-smt-edge-image theme ocodo-mesh-grass-graphic))

(defun ocodo-mesh-grass-smt-overlay (theme)
  (ocodo-smt-overlay theme))

(defun ocodo-mesh-grass-buffer-name-style (widget)
  (list :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun ocodo-mesh-grass-major-mode-style (widget)
  (list :font-size "10pt"
        :fill (if (smt/window-active-p) "#AAAAAA" "#666666")))

(defun ocodo-mesh-grass-info-style (widget)
  (list :font-size "6pt"
        :fill (if (smt/window-active-p) "#999999" "#555555")))

(defun ocodo-mesh-grass-position-info-style (widget)
  (list :fill (if (smt/window-active-p) "#DDDDDD" "#999999")))

(defun ocodo-mesh-grass-dirty-style (widget)
  (list :font-size "9pt"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p) "#1F4F25" "#143519"))))

(defun ocodo-mesh-grass-minor-mode-style (widget)
  (list :font-size "6pt"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun ocodo-mesh-grass-version-control-style (widget)
  (list :fill (if (smt/window-active-p) "#60B18C" "#3D7058")))

(smt/deftheme ocodo-mesh-grass-smt
  :pixel-height 26
  :background 'ocodo-mesh-grass-smt-background
  :overlay    'ocodo-mesh-grass-smt-overlay
  :style (lambda (theme)
           (smt/combine-styles
            (smt/t-style (smt/t-prototype theme))
            (list :font-family "sans-serif"
                  :font-weight "normal"
                  :font-size "8pt")))
  ;;; Note order of widgets are determined by smt/defrows above.
  :local-widgets
  (list (cons 'major-mode
              (smt/make-widget
               :prototype 'major-mode
               :style 'ocodo-mesh-grass-major-mode-style))

        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'ocodo-mesh-grass-minor-mode-style))

        (cons 'version-control
              (smt/make-widget
               :prototype 'version-control
               :style 'ocodo-mesh-grass-version-control-style))

        (cons 'position-info
              (smt/make-widget
               :prototype 'position-info
               :style 'ocodo-mesh-grass-position-info-style))

        (cons 'buffer-info
              (smt/make-widget
               :prototype 'buffer-info
               :style 'ocodo-mesh-grass-info-style))

        (cons 'buffer-dirty
              (smt/make-widget
               :prototype 'buffer-dirty
               :style 'ocodo-mesh-grass-dirty-style))

        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'ocodo-mesh-grass-buffer-name-style)))

  :rows (list 'ocodo-mesh-grass-row-left 'ocodo-mesh-grass-row-right 'ocodo-mesh-grass-row-mid))

(ocodo-smt-setup 15 "Menlo")

(provide 'ocodo-mesh-grass-smt)

;; Hi-lock: (("(\\(smt/[^ ]*\\)" (1 ' font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (hi-lock-mode)
;; End:

;;; ocodo-mesh-grass-smt.el ends here
