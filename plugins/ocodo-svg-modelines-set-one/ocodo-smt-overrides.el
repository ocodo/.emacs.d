;;; ocodo-smt-overrides --- Override some core parts of svg-mode-line-themes

;;; Code:
(require 'svg-mode-line-themes)

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

(defun ocodo:smt/setup (baseline &optional monofont)
  (let ((theme (cdr (assoc 'archetype smt/themes)))
        (row (cdr (assoc 'archetype smt/rows))))
    (setf (getf theme :style) (list :font-size "10pt" :font-family monofont))
    (setf (getf row :baseline) baseline))

  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil))

;; TODO: Extend to do optional positioning etc.
(defun ocodo:smt/edge-image (theme url)
  (let* ((width (smt/window-pixel-width))
         (height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "twisted" :x1 "0%" :y1 "0%" :x2 "100%" :y2 "25%"
        (stop :offset "0%"   :stop-color "#484848" :stop-opacity 0.3)
        (stop :offset "25%"  :stop-color "#484848" :stop-opacity 0.3)
        (stop :offset "75%"  :stop-color "#484848" :stop-opacity 0.3)
        (stop :offset "100%" :stop-color "#000000" :stop-opacity 0.3)))
      (rect  :width "100%"  :height "100%"  :x 0  :y 0  :fill "url(#twisted)"  :fill-opacity 1)
      (image :x -50           :y 0 :height 26 :width 100 :xlink:href ,url)
      (image :x ,(- width 50) :y 0 :height 26 :width 100 :xlink:href ,url))))

(defun ocodo:smt/overlay (theme)
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

(provide 'ocodo-smt-overrides)
