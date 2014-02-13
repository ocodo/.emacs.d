;; Dired mode init
(require 'dired-details+)

(defun dired-narrow-window ()
  "make the current dired mode window 30 chars wide"
  (interactive)
  (adjust-window-trailing-edge (selected-window) (- 30 (window-width)) t))

(add-hook 'dired-mode-hook
    '(lambda()
         (visual-line-mode 0) ;; unwrap lines.
         (linum-mode 0) ;; turn off line numbers.
         (auto-revert-mode) ;; auto-refresh dired
         (define-key dired-mode-map [mouse-3] 'dired-maybe-insert-subdir)
       (define-key dired-mode-map (kbd "C-{") 'dired-narrow-window)
         )
      )

(autoload 'dirtree "dirtree" "Add directory to tree view" t)

(provide 'init-dired)
