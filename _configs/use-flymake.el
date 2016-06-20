(require 'flymake-checkers)
;; -------------------------------------------------------------------------------------------------
;; Flymake settings
(defun flymake-settings ()
  "Settings for `flymake'."
  ;; Flymake - stop those !@$!*$ modal dialogs
  (setq flymake-gui-warnings-enabled nil)

  (defun flymake-display-current-warning/error ()
    "Display warning/error under cursor."
    (interactive)
    (let ((ovs (overlays-in (point) (1+ (point)))))
      (dolist (ov ovs)
        (catch 'found
          (when (flymake-overlay-p ov)
            (message (overlay-get ov 'help-echo))
            (throw 'found t))))))

  (defun flymake-goto-next-error-disp ()
    "Go to next error in err ring, and then display warning/error."
    (interactive)
    (flymake-goto-next-error)
    (flymake-display-current-warning/error))

  (defun flymake-goto-prev-error-disp ()
    "Go to previous error in err ring, and then display warning/error."
    (interactive)
    (flymake-goto-prev-error)
    (flymake-display-current-warning/error)))

(eval-after-load "flymake"
  `(flymake-settings))

(provide 'use-flymake)
