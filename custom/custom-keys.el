;; Note: this is all very Mac specific, where super ie. s- maps to the Cmd/Knot/Apple key.

;; Keys.

;; Navigating around frames, windows, buffers in a OS X contemporary way.

(global-set-key (kbd "s-`") 'switch-window) 
(global-set-key (kbd "s-~") 'other-frame)

(global-set-key (kbd "<s-right>") 'next-buffer)
(global-set-key (kbd "<s-left>") 'previous-buffer)

(global-set-key (kbd "s-b") 'switch-to-buffer)

;; Shrink, Enlarge, Split and Delete "windows" (buffer windows, no OS Windows.)

(global-set-key (kbd "C-M-,") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-.") 'enlarge-window-horizontally)

(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-horizontally)
(global-set-key (kbd "s-3") 'split-window-vertically)
(global-set-key (kbd "s-4") 'delete-other-windows-vertically)
(global-set-key (kbd "s-5") 'delete-window)

;; Text "zoom"

(global-set-key (kbd "s--") 'text-scale-decrease) 
(global-set-key (kbd "s-=") 'text-scale-increase) 

;; Cmd-o to open a file...

(global-set-key (kbd "s-o") 'find-file)

;; Completion / Abbreviation

(global-set-key (kbd "s-/" ) 'hippie-expand)
(global-set-key [(control tab)] 'completion-at-point)
(global-set-key (kbd "<s-return>" ) 'completion-at-point)

;; line duplicate up / down
(global-set-key [M-s-down] "\C-a\C- \C-n\C-a\C-b\M-w\C-j\C-y")
(global-set-key [M-s-up] "\C-a\C- \C-n\C-a\C-b\M-w\C-p\C-j\C-a\C-y\C-a")

;; Auto fill mode (tidy up text line length automatically.)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Turn on off line numbers

(global-set-key (kbd "s-0") 'linum-mode)

;; bind Alt-Cmd-Return to fullscreen toggle on os x in
;; window mode, if ns-toggle-fullscreen is available.
(when 
    (and 
     (window-system) ;; and 
     (fboundp 'ns-toggle-fullscreen) 
     ) 
  (global-set-key (kbd "<M-s-return>") 
                  'ns-toggle-fullscreen)
  )


;; Mode specific keys live in mode init .el files in ~/.emacs.d/modes-init/ 

(provide 'custom-keys)
