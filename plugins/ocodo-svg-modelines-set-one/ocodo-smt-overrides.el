;;; ocodo-smt-overrides --- Override some core parts of svg-mode-line-themes
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

(provide 'ocodo-smt-overrides)
