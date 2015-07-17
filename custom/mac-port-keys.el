;;; mac-port-keys --- keys Specific to the Mac Port patches to Emacs by Yamamoto Mitsuharu. (at Chiba U, Japan)

;;; Commentary:
;; Specific to the Mac Port by Yamamoto Mitsuharu.
;; (not to be confused with the Emacs installed by MacPorts)

;; Emacs Mac has a different set of defaults to Emacs Cocoa/NS
;; I've used NS/Cocoa long enough to consider it the right way,
;; and many of my key bindings extend from that.

;; You may also like the Emacs cocoa keys, and want to use them with
;; Emacs mac, so this is for you.

;;; Code:

;; Check for the existence of a function with mac- prefix
(when (symbolp 'mac-control-modifier)
  ;; Re-map modifiers - Give us Hyper instead of the Fn key.
  (setq
   mac-control-modifier  'control
   mac-option-modifier   'meta
   mac-command-modifier  'super))

(when (symbolp 'mac-function-modifier)
  ;; PC Keyboard with Menu key mapped as Fn
  ;; via Seil/Karabiner
  ;;
  ;; Here -> Fn mapped as hyper
  (setq mac-function-modifier 'hyper)

  ;; If using mac-function-modifier as hyper, you must also rebind Home,
  ;; End, Pgup, Pgdn (others?) as they become Hyper-this/that.

  ;; So here we fix keys broken by function-modifier -> hyper
  (global-set-key (kbd "H-<left>")        'smart-beginning-of-line)
  (global-set-key (kbd "H-<right>")       'end-of-line)
  (global-set-key (kbd "H-<up>")          'scroll-down-command)
  (global-set-key (kbd "H-<down>")        'scroll-up-command)
  (global-set-key (kbd "H-<backspace>")   'delete-char)
  (global-set-key (kbd "M-H-<backspace>") 'kill-word)

  (if (symbolp 'kill-whole-word)
      ;; Map Mac Clear key to kill-whole-word - if available
      (global-set-key (kbd "H-6")             'kill-whole-word)

    ;; otherwise Map Mac Clear key to kill-whole-ine
    (global-set-key (kbd "H-6")             'kill-whole-line)))

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

;; Bind "Emacs Mac port" keys the same as Emacs NS/Cocoa
(when (symbolp 'mac-super-modifier)
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

  ;; Swipe left and right for buffer navigation tends to irritate me,
  ;; and I never use it purposely (C-x left, C-x right or super-left
  ;; and super-right are better anyway)
  (global-unset-key [swipe-left])
  (global-unset-key [swipe-right]))

(provide 'mac-port-keys)

;;; mac-port-keys.el ends here
