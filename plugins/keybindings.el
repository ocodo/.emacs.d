;; C-n find-file
(global-set-key [(control n)] 'find-file)

;; Move between buffers with M-[ and M-]

(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

;; completion at point
(global-set-key [(control tab)] 'completion-at-point)

;; Darkroom additional bindings
(global-set-key '[M-S-return] 'darkroom-mode)