(require 'svg-mode-line-themes-core)

(defun smt/buffer-name-text (widget)
  (concat
   (format-mode-line "%b")
   (if (and (or (buffer-file-name)
                buffer-offer-save)
            (buffer-modified-p))
       "*")))

(defun smt/buffer-indicators-text (widget)
  (let ((indicators
         (concat
          (unless (or (eq system-type 'windows-nt) (daemonp))
            "S")
          (when (window-dedicated-p) "D")
          (when buffer-read-only "R")
          " ")))
    (if (< 1 (length indicators))
        indicators
        "")))

(defun smt/minor-mode-indicator-text (widget)
  (let (( text
          (concat
           (when (bound-and-true-p es-aai-mode) "I")
           (when (or (bound-and-true-p evil-local-mode)
                     (bound-and-true-p evil-mode)) "E")
           (when truncate-lines "T")
           (when (bound-and-true-p dired-omit-mode) "O")
           (when (bound-and-true-p save-auto-hook) "A")
           (when (/= (- (point-max) (point-min)) (buffer-size)) "N")
           (when (bound-and-true-p wmi) "M")
           ;; Small letters are used for temporary modes
           (when (bound-and-true-p multiple-cursors-mode) "m")
           (when (bound-and-true-p iedit-mode) "i")
           )))
    (if (plusp (length text))
        (concat " " text)
        "")))

;;; Widgets

(smt/defwidget minor-modes
  :text 'smt/minor-mode-indicator-text)

(smt/defwidget major-mode
  :text (lambda (widget)
          (format-mode-line mode-name))
  :on-click (lambda (widget event)
              (message "%s" (format-mode-line
                             mode-line-modes))))

(smt/defwidget version-control
  :text (lambda (widget)
          (format-mode-line 'vc-mode))
  :on-click (lambda (widget event)
              (popup-menu vc-menu-map)))

(smt/defwidget buffer-name
  :on-click (lambda (widget event)
              (message (or (buffer-file-name)
                           (ignore-errors
                             (dired-current-directory)))))
  :text 'smt/buffer-name-text)

(smt/defwidget which-function
  :text (lambda (widget)
          (if (bound-and-true-p which-func-mode)
              (let ((text (format-mode-line which-func-current)))
                (when (and (> (length text) 0)
                           (not (equal text "???")))
                  (concat " :: " text)))
              "")))

(smt/defwidget position-info
  :text (lambda (widget)
          (format-mode-line "%l:%p"))
  :on-click (lambda (widget event)
              (what-cursor-position)))

(smt/defwidget buffer-info
  :text 'smt/buffer-indicators-text)

;;; Rows

(smt/defrow default-left
  :widgets '(buffer-info buffer-name which-function)
  :margin 2)

(smt/defrow default-right
  :widgets '(major-mode version-control minor-modes)
  :align "right"
  :margin 14)

(smt/defrow default-position
  :widgets '(position-info)
  :align "right"
  :margin 2)

(provide 'svg-mode-line-themes-widgets)
;; svg-mode-line-themes-widgets.el ends here
