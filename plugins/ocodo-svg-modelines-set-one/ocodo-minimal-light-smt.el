;;; ocodo-minimal-light-smt --- Yet another attempt at a super cool modeline for Emacs

;;; Commentary:

;; Work in progress, check commit history for variations.

;; Made with the svg-mode-line-themes toolkit from Sabof.
;; https://github.com/sabof/svg-mode-line-themes

;; Hopefully a skillful Emacs user will see the correlation of xmlgen
;; and svg below (esp. ocodo-minimal-light:smt/background) and from there, with some
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

(require 'svg-mode-line-themes)

(smt/enable)

(let ((theme (cdr (assoc 'archetype smt/themes)))
      (row (cdr (assoc 'archetype smt/rows))))
  ;; *******************************************************
  ;;  Customise to use your desired default monospaced font
  ;; *******************************************************
  (setf (getf theme :style) (list :font-size "10pt" :font-family "Menlo"))
  (setf (getf row :baseline) 17))

(setq default-active "#000000")
(setq default-inactive "#333333")
(setq bg-gradient-main "#484848")
(setq bg-gradient-dark "#000000")
(setq bg-gradient-hi "#FFFFFF")
(setq overlay-dark "#000000")
(setq overlay-light "#FFFFFF")

(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(defun smt/buffer-indicators-text (widget)
  "Provide buffer state indicators.
WIDGET is a required param.
Overrides smt core."
  (if buffer-read-only " RO " " RW "))

(defun smt/buffer-name-text (widget)
  "Show the current buffer name.
WIDGET is a required param.
Overrides smt core."
  (format-mode-line "%b"))

(defun smt/minor-mode-indicator-text (widget)
  "Minor mode indication.
WIDGET is a required param.
Overrides smt core."
  (concat
   (when defining-kbd-macro                             " REC ")
   (when (bound-and-true-p projectile-mode)             " Prj ")
   (when (bound-and-true-p projectile-rails-mode)       " Rails ")
   (when (bound-and-true-p smartparens-mode)            " [S] ")
   (when (or (bound-and-true-p evil-local-mode)
             (bound-and-true-p evil-mode))              " Evil ")
   (when (bound-and-true-p dired-omit-mode)             " O ")
   (when (bound-and-true-p rainbow-mode)                " Rbow ")
   (when (bound-and-true-p global-auto-revert-mode)     " ARv ")
   (when (bound-and-true-p visual-line-mode)            " Vl ")
   (when (bound-and-true-p multiple-cursors-mode)       " Mc ")
   (when (bound-and-true-p iedit-mode)                  " iE ")))

(smt/defwidget buffer-dirty
  :text (lambda (widget)
          (if (and (buffer-modified-p)
                   (or buffer-file-name buffer-offer-save))
              " ❉ " " ✓ ")))

(smt/defwidget position-info
  :text (lambda (widget)
          (format-mode-line "%l:%c [%p] %I"))
  :on-click (lambda (widget event)
              (what-cursor-position t)))

(smt/defwidget major-mode
  :text (lambda (widget)
          (format-mode-line mode-name))
  :on-click (lambda (widget event)
              (message " %s " (format-mode-line mode-line-modes))))

(defun ocodo-minimal-light:smt/background (theme)
  (let ((width (smt/window-pixel-width))
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
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#twisted)")
      (path :fill "#EBEAEA" :d "M0 0 L230 0 L239 12 L230 26 L0 26 Z")
      (path :fill "#F6F1F1" :d ,(format
                                 "M%i 0 L%i 0 L%i 12 L%i 26 L%i 26 Z"
                                 width (- width 110) (- width 119) (- width 110) width))
      )))


(defun ocodo-minimal-light:smt/overlay (theme)
  (let ((width (smt/window-pixel-width))
        (height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "over-gradient" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset 0 :stop-color ,overlay-light :stop-opacity 0.1 )
        (stop :offset 0.5 :stop-color ,overlay-light :stop-opacity 0.0 )
        (stop :offset 0.5 :stop-color ,overlay-dark :stop-opacity 0.0 )
        (stop :offset 1 :stop-color ,overlay-dark :stop-opacity 0.1)))

      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#over-gradient)")
      )))

(defun smt/ocodo-buffer-name-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :font-family "sans-serif"
        :filter nil
        :fill (if (smt/window-active-p) default-active default-inactive)))

(defun smt/ocodo-major-mode-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) default-active default-inactive)))

(defun smt/ocodo-info-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) default-active default-inactive)))

(defun smt/ocodo-position-info-style (widget)
  (list :font-weight "normal"
        :font-size "8pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) default-active default-inactive)))

(defun smt/ocodo-dirty-style (widget)
  (list :font-weight "normal"
        :font-size "11pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (and (or buffer-file-name buffer-offer-save) (buffer-modified-p))
                  ;; Dirty
                  (if (smt/window-active-p) "#FF6060" "#763030")
                ;; Untouched
                (if (smt/window-active-p) "#3d7058" "#143519"))))

(defun smt/ocodo-minor-mode-style (widget)
  (list :font-weight "normal"
        :font-size "6pt"
        :filter nil
        :fill (if (smt/window-active-p) default-active default-inactive)))

(defun smt/ocodo-version-control-style (widget)
  (list :font-weight "bold"
        :font-size "8pt"
        :filter nil
        :font-family "sans-serif"
        :fill (if (smt/window-active-p) "#5D3D70" "#333333")))

(smt/defrow default-left
  ;; rw/ro filename saved?
  :widgets '(buffer-info buffer-name buffer-dirty)
  :margin 1)

(smt/defrow default-right
  :widgets '(major-mode version-control minor-modes)
  :align "right"
  :margin 25)

(smt/defrow default-position
  :widgets '(position-info)
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

  :rows (list 'default-left 'default-right 'default-position))

(smt/set-theme 'ocodo-minimal-light:smt)

(provide 'ocodo-minimal-light-smt)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; ocodo-minimal-light-smt.el ends here
