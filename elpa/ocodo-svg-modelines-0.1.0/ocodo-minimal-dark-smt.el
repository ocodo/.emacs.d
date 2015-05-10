;;; ocodo-minimal-dark-smt --- Yet another attempt at a super cool modeline for Emacs
;;
;;; Commentary:
;;
;; Made with the svg-mode-line-themes toolkit from Sabof.
;; https://github.com/sabof/svg-mode-line-themes
;;
;;; Code:

(require 'ocodo-smt-overrides)

(setq ocodo-minimal-dark:fileurl-prefix
      (concat "file://" (file-name-directory (or load-file-name buffer-file-name))))

(smt/defrow ocodo-minimal-dark:smt-left
  :margin 1
  :always-visible t
  :widgets '(buffer-info buffer-name buffer-dirty)
  :align "left")

(smt/defrow ocodo-minimal-dark:smt-right
  :margin 1
  :always-visible t
  :widgets '(position-info)
  :align "right")

(smt/defrow ocodo-minimal-dark:smt-mid
  :margin 25
  :always-visible t
  :widgets '(major-mode version-control minor-modes)
  :align "right")

(defun ocodo-minimal-dark:smt/background (theme)
  (ocodo:smt/edge-image theme nil))

(defun ocodo-minimal-dark:smt/overlay (theme)
  (ocodo:smt/overlay theme))

(defun smt/ocodo-minimal-dark-buffer-name-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun smt/ocodo-minimal-dark-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "10pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#AAAAAA" "#666666")))

(defun smt/ocodo-minimal-dark-info-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#999999" "#555555")))

(defun smt/ocodo-minimal-dark-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :fill (if (smt/window-active-p) "#DDDDDD" "#999999")))

(defun smt/ocodo-minimal-dark-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p) "#1F4F25" "#143519"))))

(defun smt/ocodo-minimal-dark-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :fill (if (smt/window-active-p) "#FFFFFF" "#666666")))

(defun smt/ocodo-minimal-dark-version-control-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#60ACB1" "#365E63")))

(smt/deftheme ocodo-minimal-dark:smt
  :pixel-height 26
  :background 'ocodo-minimal-dark:smt/background
  :overlay    'ocodo-minimal-dark:smt/overlay
  :local-widgets
  ;;; Note order of widgets are determined by smt/defrows above.
  (list (cons 'major-mode
              (smt/make-widget
               :prototype 'major-mode
               :style 'smt/ocodo-minimal-dark-major-mode-style))

        (cons 'minor-modes
              (smt/make-widget
               :prototype 'minor-modes
               :style 'smt/ocodo-minimal-dark-minor-mode-style))

        (cons 'version-control
              (smt/make-widget
               :prototype 'version-control
               :style 'smt/ocodo-minimal-dark-version-control-style))

        (cons 'position-info
              (smt/make-widget
               :prototype 'position-info
               :style 'smt/ocodo-minimal-dark-position-info-style))

        (cons 'buffer-info
              (smt/make-widget
               :prototype 'buffer-info
               :style 'smt/ocodo-minimal-dark-info-style))

        (cons 'buffer-dirty
              (smt/make-widget
               :prototype 'buffer-dirty
               :style 'smt/ocodo-minimal-dark-dirty-style))

        (cons 'buffer-name
              (smt/make-widget
               :prototype 'buffer-name
               :style 'smt/ocodo-minimal-dark-buffer-name-style)))

  :rows (list 'ocodo-minimal-dark:smt-left 'ocodo-minimal-dark:smt-right 'ocodo-minimal-dark:smt-mid))

(ocodo:smt/setup 17 "Menlo")

(provide 'ocodo-minimal-dark-smt)

;; Hi-lock: (("(\\(smt/[^ ]*\\)" (1 ' font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; eval: (hi-lock-mode)
;; End:

;;; ocodo-minimal-dark-smt.el ends here
