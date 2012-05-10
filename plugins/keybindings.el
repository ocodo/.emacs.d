;; C-n find-file
(global-set-key [(control n)] 'find-file)

;; Move between buffers with M-[ and M-]

(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-[") 'previous-buffer)

(global-set-key (kbd "<s-right>") 'next-buffer)
(global-set-key (kbd "<s-left>") 'previous-buffer)

(global-set-key (kbd "s-/" ) 'dabbrev-expand)
(global-set-key (kbd "<s-return>" ) 'dabbrev-expand)

(global-set-key (kbd "s-b") 'switch-to-buffer)

(global-set-key (kbd "s--") 'text-scale-decrease) 
(global-set-key (kbd "s-=") 'text-scale-increase) 

(global-set-key (kbd "s-n") 'find-file)

(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-horizontally)
(global-set-key (kbd "s-3") 'split-window-vertically)

(global-set-key (kbd "s-`") 'other-window)
(global-set-key (kbd "s-~") 'other-frame)

;; completion at point
(global-set-key [(control tab)] 'completion-at-point)

;; Darkroom additional bindings
(global-set-key '[M-S-return] 'darkroom-mode)

(global-set-key '[M-s-return] 'darkroom-mode)

;; Linum mode toggle

(global-set-key (kbd "s-0") 'linum-mode)

;; Dired mode additions

(eval-after-load 'dired
  '(define-key dired-mode-map [mouse-3] 'dired-maybe-insert-subdir))


;; More ...?
