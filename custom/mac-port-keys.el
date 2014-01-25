;; Specific to the Mac Port by Yamamoto Mitsuharu aka. Railwaycat
;; (not to be confused with the Emacs installed by MacPorts)

;; Emacs Mac has a different set of defaults to Emacs Cocoa/NS
;; I've used NS/Cocoa long enough to consider it the right way,
;; and many of my key bindings extend from that.

;; You may also like the Emacs cocoa keys, and want to use them with
;; Emacs mac, so this is for you.

;; Check for the existence of a function with mac- prefix
(when (symbolp 'mac-control-modifier)
  ;; Re-map modifiers
  (setq mac-control-modifier 'control
        mac-option-modifier 'meta
        mac-command-modifier 'super)
  )

;; Fullscreen mode toggle in emacs mac. This invokes the Kiosk mode
;; fullscreen, ie. only one display is hijacked. not the obnoxious
;; Lion Fullscreen, which kills off all your other screens. Fine if
;; you're just on a lappy.  Not nice when you have several screens.
;; Very poor user interface design if you ask me.
;;
;; Emacs mac also has the lion style available for mouse operation,
;; with the Lion top right window button.
;;
;; So you get the best of both worlds.
;;
;; Side-note: Emacs mac port has pixel scrolling, and I'm a sucker for
;; that.

(defvar mac-fullscreen-on  nil
  "keep a track of mac-mouse-turn-o(n|ff)-fullscreen, assumes fullscreen is not on")
(defun mac-toggle-fullscreen ()
  "toggle fullscreen mode in Emacs mac (by Yamamoto Mitsuharu)"
  (interactive)
  ;; check we are in the emacs mac build
  (when (functionp 'mac-process-hi-command)
  (if (eq mac-fullscreen-on t)
      (progn
        (mac-mouse-turn-off-fullscreen t)
        (setq mac-fullscreen-on nil)
      )
      (progn
        (mac-mouse-turn-on-fullscreen t)
        (setq mac-fullscreen-on t)
      )
    )))

(when (and (window-system) (fboundp 'mac-mouse-turn-on-fullscreen))
  (global-set-key (kbd "<M-s-return>") 'mac-toggle-fullscreen)
)

;; Bind "Emacs Mac port" keys the same as Emacs NS/Cocoa
(when (symbolp 'mac-control-modifier)

  (global-set-key (kbd "s-s")    'save-buffer)
  (global-set-key (kbd "s-z")    'undo)
  (global-set-key (kbd "s-x")    'cua-cut-region)
  (global-set-key (kbd "s-c")    'cua-copy-region)
  (global-set-key (kbd "s-v")    'cua-paste)
  (global-set-key (kbd "s-w")    'delete-frame)
  (global-set-key (kbd "s-q")    'save-buffers-kill-emacs)
  (global-set-key (kbd "s-k")    'kill-this-buffer)
  (global-set-key (kbd "s-u")    'revert-buffer)
  (global-set-key (kbd "s-a")    'mark-whole-buffer)
  (global-set-key (kbd "s-l")    'goto-line)
  (global-set-key (kbd "s-i")    'ispell-complete-word)
  (global-set-key (kbd "M-s-i")  'ispell-word)
  (global-set-key (kbd "s-'")    'switch-window)

;;
;;  (global-set-key (kbd "<home>") 'beginning-of-buffer)
;;  (global-set-key (kbd "<end>") 'end-of-buffer)

  ;; Navigating around frames, windows & buffers
  (global-set-key (kbd "C-`") 'switch-window)
  (global-set-key (kbd "C-~") 'other-frame)

)

(provide 'mac-port-keys)
