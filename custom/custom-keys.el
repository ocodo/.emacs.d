;; Keys.
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;;(global-set-key (kbd "s-]") 'next-buffer)
;;(global-set-key (kbd "s-[") 'previous-buffer)

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

(global-set-key (kbd "s-`") 'switch-window)
(global-set-key (kbd "s-~") 'other-frame)
(global-set-key [(control tab)] 'completion-at-point)
(global-set-key (kbd "<s-return>" ) 'completion-at-point)
(global-set-key (kbd "s-0") 'linum-mode)

;; bind Alt-Cmd-Return to fullscreen toggle on os x in
;; window mode, if ns-toggle-fullscreen is available.
(when 
    (and 
     (fboundp 'ns-toggle-fullscreen)
     (window-system)
     )
  (global-set-key (kbd "<M-s-return>") 
                  'ns-toggle-fullscreen)
  )

(provide 'custom-keys)

;; Mode specific keys live in mode init .el files in ~/.emacs.d/modes-init/ 
