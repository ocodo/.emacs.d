;;; ocodo-steps-grass-smt --- Yet another attempt at a super cool modeline for Emacs
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

(defvar ocodo-steps-grass-folder
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar ocodo-steps-grass-images
  (concat ocodo-steps-grass-folder "images/"))

(defvar ocodo-steps-grass-fileurl-prefix
  (concat "file://" ocodo-steps-grass-folder))

(defvar ocodo-steps-grass-graphic
  (concat "data:image/svg+xml;base64,"
          (ocodo-smt-string-from-file
           (concat ocodo-steps-grass-images "steps-grass.svg.base64"))))

(smt/defrow ocodo-steps-grass-row-left
  :margin 5
  :always-visible t
  :widgets '(buffer-info buffer-name buffer-dirty)
  :align "left")

(smt/defrow ocodo-steps-grass-row-right
  :margin 5
  :always-visible t
  :widgets '(position-info)
  :align "right")

(smt/defrow ocodo-steps-grass-row-mid
  :margin 25
  :always-visible t
  :widgets '(major-mode version-control minor-modes)
  :align "right")

(defun ocodo-steps-grass-smt-background (theme)
  (ocodo-smt-edge-image theme ocodo-steps-grass-graphic))

(defun ocodo-steps-grass-smt-overlay (theme)
  (ocodo-smt-overlay theme))

(defun ocodo-steps-grass-buffer-name-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun ocodo-steps-grass-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#AAAAAA" "#666666")))

(defun ocodo-steps-grass-info-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#999999" "#555555")))

(defun ocodo-steps-grass-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :fill (if (smt/window-active-p) "#DDDDDD" "#999999")))

(defun ocodo-steps-grass-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p) "#1F4F25" "#143519"))))

(defun ocodo-steps-grass-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun ocodo-steps-grass-version-control-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#60ACB1" "#365E63")))

(smt/deftheme ocodo-steps-grass-smt
  :pixel-height 26
  :background 'ocodo-steps-grass-smt-background
  :overlay    'ocodo-steps-grass-smt-overlay
  :local-widgets
  ;;; Note order of widgets are determined by smt/defrows above.
  (list (cons 'major-mode
              (smt/make-widget
               :prototype 'major-mode
               :style 'ocodo-steps-grass-major-mode-style))

        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'ocodo-steps-grass-minor-mode-style))

        (cons 'version-control
              (smt/make-widget
               :prototype 'version-control
               :style 'ocodo-steps-grass-version-control-style))

        (cons 'position-info
              (smt/make-widget
               :prototype 'position-info
               :style 'ocodo-steps-grass-position-info-style))

        (cons 'buffer-info
              (smt/make-widget
               :prototype 'buffer-info
               :style 'ocodo-steps-grass-info-style))

        (cons 'buffer-dirty
              (smt/make-widget
               :prototype 'buffer-dirty
               :style 'ocodo-steps-grass-dirty-style))

        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'ocodo-steps-grass-buffer-name-style)))

  :rows (list 'ocodo-steps-grass-row-left 'ocodo-steps-grass-row-right 'ocodo-steps-grass-row-mid))

(ocodo-smt-setup 17 "Menlo")

(provide 'ocodo-steps-grass-smt)

;; Hi-lock: (("(\\(smt/[^ ]*\\)" (1 ' font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (hi-lock-mode)
;; End:

;;; ocodo-steps-grass-smt.el ends here
