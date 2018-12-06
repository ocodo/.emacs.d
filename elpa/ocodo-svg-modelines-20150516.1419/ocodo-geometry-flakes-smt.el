;;; ocodo-geometry-flakes-smt --- Yet another attempt at a super cool modeline for Emacs
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

(defvar ocodo-geometry-flakes-folder
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar ocodo-geometry-flakes-images
  (concat ocodo-geometry-flakes-folder "images/"))

(defvar ocodo-geometry-flakes-fileurl-prefix
  (concat "file://" ocodo-geometry-flakes-folder))

(defvar ocodo-geometry-flakes-graphic
  (concat "data:image/png;base64,"
          (ocodo-smt-string-from-file
           (concat ocodo-geometry-flakes-images "geometry2.png.base64"))))

(smt/defrow ocodo-geometry-flakes-row-left
  :margin 5
  :always-visible t
  :widgets '(buffer-info buffer-name buffer-dirty)
  :align "left")

(smt/defrow ocodo-geometry-flakes-row-right
  :margin 6
  :always-visible t
  :widgets '(position-info)
  :align "right")

(smt/defrow ocodo-geometry-flakes-row-mid
  :margin 25
  :always-visible t
  :widgets '(major-mode version-control minor-modes)
  :align "right")

(defun ocodo-geometry-flakes-smt-background (theme)
  (ocodo-smt-edge-image theme ocodo-geometry-flakes-graphic))

(defun ocodo-geometry-flakes-smt-overlay (theme)
  (ocodo-smt-overlay theme))

(defun ocodo-geometry-flakes-buffer-name-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun ocodo-geometry-flakes-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#AAAAAA" "#666666")))

(defun ocodo-geometry-flakes-info-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#999999" "#555555")))

(defun ocodo-geometry-flakes-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :fill (if (smt/window-active-p) "#DDDDDD" "#999999")))

(defun ocodo-geometry-flakes-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p) "#1F4F25" "#143519"))))

(defun ocodo-geometry-flakes-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun ocodo-geometry-flakes-version-control-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#60ACB1" "#365E63")))

(smt/deftheme ocodo-geometry-flakes-smt
  :pixel-height 26
  :background 'ocodo-geometry-flakes-smt-background
  :overlay    'ocodo-geometry-flakes-smt-overlay
  :local-widgets
  ;;; Note order of widgets are determined by smt/defrows above.
  (list (cons 'major-mode
              (smt/make-widget
               :prototype 'major-mode
               :style 'ocodo-geometry-flakes-major-mode-style))

        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'ocodo-geometry-flakes-minor-mode-style))

        (cons 'version-control
              (smt/make-widget
               :prototype 'version-control
               :style 'ocodo-geometry-flakes-version-control-style))

        (cons 'position-info
              (smt/make-widget
               :prototype 'position-info
               :style 'ocodo-geometry-flakes-position-info-style))

        (cons 'buffer-info
              (smt/make-widget
               :prototype 'buffer-info
               :style 'ocodo-geometry-flakes-info-style))

        (cons 'buffer-dirty
              (smt/make-widget
               :prototype 'buffer-dirty
               :style 'ocodo-geometry-flakes-dirty-style))

        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'ocodo-geometry-flakes-buffer-name-style)))

  :rows (list 'ocodo-geometry-flakes-row-left 'ocodo-geometry-flakes-row-right 'ocodo-geometry-flakes-row-mid))

(ocodo-smt-setup 17 "Menlo")

(provide 'ocodo-geometry-flakes-smt)

;; Hi-lock: (("(\\(smt/[^ ]*\\)" (1 ' font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (hi-lock-mode)
;; End:

;;; ocodo-geometry-flakes-smt.el ends here
