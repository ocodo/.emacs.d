;; C-z undo 
(global-set-key [(control z)] 'undo)
;; C-n find-file
(global-set-key [(control n)] 'find-file)

;;
;; Rect-mark key bindings.
;;
(global-set-key [(S-down-mouse-1)] 'rm-set-mark)
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)

(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)

(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)

(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)

(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)

;; Darkroom additional bindings
(global-set-key (quote [M-S-return]) 'darkroom-mode)