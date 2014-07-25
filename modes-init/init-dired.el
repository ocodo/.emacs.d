;; Dired mode init
(require 'dired-details+)
(require 'dirtree)

(setq dired-details-initially-hide t)

(add-hook 'dired-mode-hook
    '(lambda ()
         (visual-line-mode 0) ;; unwrap lines.
         (linum-mode 0)       ;; turn off line numbers.
         (auto-revert-mode)   ;; auto-refresh dired
         )
      )

(add-hook 'dired-after-readin-hook
          #'(lambda () (dired-details-activate)))

(provide 'init-dired)
