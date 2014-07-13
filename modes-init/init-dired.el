;; Dired mode init
(require 'dired-details+)

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

(autoload 'dirtree "dirtree" "Add directory to tree view" t)

(provide 'init-dired)
