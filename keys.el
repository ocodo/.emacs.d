;; Keys.
(progn 
  (global-set-key (kbd "s-]") 'next-buffer)
  (global-set-key (kbd "s-[") 'previous-buffer)
  (global-set-key (kbd "<s-right>") 'next-buffer)
  (global-set-key (kbd "<s-left>") 'previous-buffer)

  (global-set-key (kbd "s-/" ) 'hippie-expand)

  (global-set-key (kbd "s-b") 'switch-to-buffer)

  (global-set-key (kbd "s--") 'text-scale-decrease) 
  (global-set-key (kbd "s-=") 'text-scale-increase) 

  (global-set-key (kbd "s-o") 'find-file)

  (global-set-key (kbd "s-1") 'delete-other-windows)
  (global-set-key (kbd "s-2") 'split-window-horizontally)
  (global-set-key (kbd "s-3") 'split-window-vertically)
  (global-set-key (kbd "s-4") 'delete-other-windows-vertically)
  (global-set-key (kbd "s-5") 'delete-window)

  (global-set-key (kbd "C-M-,") 'shrink-window-horizontally)
  (global-set-key (kbd "C-M-.") 'enlarge-window-horizontally)

  (global-set-key (kbd "s-`") 'other-window)
  (global-set-key (kbd "s-~") 'other-frame)
  (global-set-key [(control tab)] 'completion-at-point)
  (global-set-key (kbd "<s-return>" ) 'completion-at-point)
  (global-set-key (kbd "s-0") 'linum-mode)
  )

;; Dired mode specific
(eval-after-load 'dired
  '(define-key dired-mode-map [mouse-3] 'dired-maybe-insert-subdir)
  )
