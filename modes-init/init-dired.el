;; Dired mode init
(require 'dired)
(require 'dired-details+)
(require 'dirtree)

(setq dired-details-initially-hide t)

(add-hook 'dired-mode-hook
    '(lambda ()
         (visual-line-mode 0) ;; unwrap lines.
         (linum-mode 0)       ;; turn off line numbers.
         (auto-revert-mode)   ;; auto-refresh dired
         ))

(defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "W" 'wdired-change-to-wdired-mode)
     (define-key dired-mode-map "F" 'my-dired-find-file)))

(add-hook 'dired-after-readin-hook
          #'(lambda () (dired-details-activate)))

(provide 'init-dired)
