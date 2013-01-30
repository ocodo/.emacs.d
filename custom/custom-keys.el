;; Note: this is all very Mac specific, where super ie. s- maps to the Cmd/Knot/Apple key.

;; Keys.

;; append region to file 
(global-set-key (kbd "C-x C-a") 'append-to-file)

;; Resize window horizontally
(global-set-key (kbd "C-M-,") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-.") 'enlarge-window-horizontally)

;; Completion / Abbreviation

(global-set-key [(control tab)] 'completion-at-point)

;; Mpve line / region
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

;; line duplicate up / down
(global-set-key [M-s-down] "\C-a\C- \C-n\C-a\C-b\M-w\C-j\C-y\C-a")
(global-set-key [M-s-up] "\C-a\C- \C-n\C-a\C-b\M-w\C-p\C-j\C-a\C-y\C-a")

;; Auto fill mode (tidy up text line length automatically.)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; bind Alt-Cmd-Return to fullscreen toggle on os x in
;; window mode, if ns-toggle-fullscreen is available.
;; otherwise just max size the frame.
(if (and (window-system) (fboundp 'ns-toggle-fullscreen)) 
    (global-set-key (kbd "<M-s-return>") 'ns-toggle-fullscreen)
  (global-set-key (kbd "<M-s-return>") 'set-frame-maximize))


;; unset some annoying things in the OS X build, no dialogs, also add
;; a few osx centric bindings.

(when (and (window-system) (eq system-type 'darwin)) 

  ;; Cmd-o to find-file...
  (global-set-key (kbd "s-o") 'find-file)

  ;; Cmd-Shift-s : write-file (no longer save-as)
  (global-set-key [8388691] 'write-file)

  ;; Cmd-Shift-r : write-region (new: save selection )
  (global-set-key [8388690] 'write-region)

  ;; Cmd-p : unbound from print dialog, focus stealing annoyance.
  ;; Instead bound to the useful find-file-at-point
  (global-set-key [8388720] 'find-file-at-point)

  ;; Cmd-m : unbound - I don't need minimize bound
  (global-unset-key [8388717])

  ;; Cmd-h : unbound - I don't need hide bound
  (global-unset-key [8388712])

  ;; Cmd-alt-l : Load library
  (global-set-key [142606508] 'load-library)

  ;; Cmd-t : hardwired for many people from browsers, TextMate, etc. as
  ;; open a new tab (ie. buffer) - so we bind it here as: open a file.
  ;; By default it's bound in OSX Emacs as open the font dialog. No, really.
  (global-set-key [8388724] 'find-file)

  ;; Cmd-u : is default wired to revert buffer, I quite like that, but
  ;; this seems like a good place to tag that info.

  ;; Navigating around frames, windows & buffers
  (global-set-key (kbd "s-`") 'switch-window) 

  (global-set-key (kbd "s-~") 'other-frame)

  (global-set-key (kbd "<s-right>") 'next-buffer)
  (global-set-key (kbd "<s-left>") 'previous-buffer)

  (global-set-key (kbd "s-b") 'switch-to-buffer)

  ;; window-splitting
  (global-set-key (kbd "s-1") 'delete-other-windows)
  (global-set-key (kbd "s-2") 'split-window-horizontally)
  (global-set-key (kbd "s-3") 'split-window-vertically)
  (global-set-key (kbd "s-4") 'delete-other-windows-vertically)

  (global-set-key (kbd "s-5") 'delete-window)
  ;; same command on Cmd-C-w 
  (global-set-key [C-s-w] 'delete-window)

  ;; Text "zoom"
  (global-set-key (kbd "s--") 'text-scale-decrease) 
  (global-set-key (kbd "s-=") 'text-scale-increase) 

  ;; line numbers off on
  (global-set-key (kbd "s-0") 'linum-mode)

  ;; highly inclusive expand
  (global-set-key (kbd "s-/" ) 'hippie-expand)

  ;; "contextual" completion 
  (global-set-key (kbd "<s-return>" ) 'completion-at-point)

  ;; narrow / widen region
  (global-set-key (kbd "s-§") 'narrow-to-region)
  (global-set-key (kbd "C-§") 'widen)
    
  )

;; Please Note: there are still bindings littered about, I will clean this up! (one day!)

(provide 'custom-keys)
