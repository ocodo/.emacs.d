;;; ocodo-smt-overrides --- Override some core parts of svg-mode-line-themes
;;
;; Author: ocodo <what.is.ocodo@gmail.com>
;; Package-Requires: ((svg-mode-line-themes "0"))
;; Version: 0.1.4
;; URL: https://github.com/ocodo/ocodo-svg-modelines
;;
;;; Code:
(require 'svg-mode-line-themes)

;;;###autoload
(defun ocodo-smt-string-from-file (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

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
          (when (buffer-file-name)
            (if (and (buffer-modified-p) (or buffer-file-name buffer-offer-save))
                " ❉ " " ✓ "))))

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

(defun ocodo-smt-setup (baseline &optional monofont)
  (let ((theme (cdr (assoc 'archetype smt/themes)))
        (row (cdr (assoc 'archetype smt/rows))))
    (setf (getf theme :style) (list :font-size "10pt" :font-family monofont))
    (setf (getf row :baseline) baseline))

  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil))

(setq ocodo-twisted-stops '(("0%"   "#484848" 0.3)
                            ("25%"  "#484848" 0.3)
                            ("75%"  "#484848" 0.3)
                            ("100%" "#000000" 0.3)))

;; TODO: Extend to do optional positioning etc.
(defun ocodo-smt-edge-image (theme url &optional extended)
  (let* ((stops ocodo-twisted-stops)
         (width (smt/window-pixel-width))
         (height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "twisted" :x1 "0%" :y1 "0%" :x2 "100%" :y2 "25%"
        (stop :offset ,(first (nth 1 stops)) :stop-color ,(second (nth 1 stops)) :stop-opacity ,(third (nth 1 stops)))
        (stop :offset ,(first (nth 2 stops)) :stop-color ,(second (nth 2 stops)) :stop-opacity ,(third (nth 2 stops)))
        (stop :offset ,(first (nth 3 stops)) :stop-color ,(second (nth 3 stops)) :stop-opacity ,(third (nth 3 stops)))
        (stop :offset ,(first (nth 4 stops)) :stop-color ,(second (nth 4 stops)) :stop-opacity ,(third (nth 4 stops)))))
      (rect  :width "100%"  :height "100%"  :x 0  :y 0  :fill "url(#twisted)"  :fill-opacity 1)
      (image :x -50           :y 0 :height 26 :width 100 :xlink:href ,url)
      (image :x ,(- width 50) :y 0 :height 26 :width 100 :xlink:href ,url)
      ,extended)))

(setq ocodo-overlay-stops '(("0%"   "#FFFFFF" 0.1)
                            ("20%"  "#000000" 0.0)
                            ("90%"  "#000000" 0.5)
                            ("100%" "#000000" 0.8)))

(defun ocodo-smt-overlay (theme &optional extended)
  (let ((stops  ocodo-overlay-stops)
        (width (smt/window-pixel-width))
        (height (smt/t-pixel-height theme)))
    `((\defs
       (linearGradient
        :id "over-gradient" :x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"
        (stop :offset ,(first (nth 1 stops)) :stop-color ,(second (nth 1 stops)) :stop-opacity ,(third (nth 1 stops)))
        (stop :offset ,(first (nth 2 stops)) :stop-color ,(second (nth 2 stops)) :stop-opacity ,(third (nth 2 stops)))
        (stop :offset ,(first (nth 3 stops)) :stop-color ,(second (nth 3 stops)) :stop-opacity ,(third (nth 3 stops)))
        (stop :offset ,(first (nth 4 stops)) :stop-color ,(second (nth 4 stops)) :stop-opacity ,(third (nth 4 stops)))))
      (rect :width "100%" :height "100%" :x 0 :y 0 :fill "url(#over-gradient)")
      ,extended)))

(provide 'ocodo-smt-overrides)
