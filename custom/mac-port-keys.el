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
  (setq mac-control-modifier  'control
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
  (bind-keys
   ("H-<left>"         . smart-beginning-of-line)
   ("H-<right>"        . end-of-line)
   ("H-<up>"           . scroll-down-command)
   ("H-<down>"         . scroll-up-command)
   ("H-<backspace>"    . delete-char)
   ("M-H-<backspace>"  . kill-word)))

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
(when
    (or (symbolp 'mac-super-modifier)
        ;; Allow a linux system with GUI set to super to use these
        ;; bindings too:
        (or (eq x-super-keysym  nil)
            (eq x-super-keysym  'super)))

  (bind-keys
   ("s-s"    . save-buffer)
   ("s-z"    . undo)
   ("s-x"    . cua-cut-region)
   ("s-c"    . cua-copy-region)
   ("s-v"    . cua-paste)
   ("s-w"    . delete-frame)
   ("s-q"    . save-buffers-kill-emacs)
   ("s-k"    . kill-this-buffer)
   ("s-u"    . revert-buffer)
   ("s-a"    . mark-whole-buffer)
   ("s-l"    . goto-line)
   ("s-i"    . ispell-complete-word)
   ("M-s-i"  . ispell-word)
   ("s-'"    . switch-window)
   ("C-`"    . switch-window)
   ("C-~"    . other-frame))

  ;; Swipe left and right for buffer navigation tends to irritate me,
  ;; and I never use it purposely (C-x left, C-x right or super-left
  ;; and super-right are better anyway)
  (global-unset-key [swipe-left])
  (global-unset-key [swipe-right]))

(provide 'mac-port-keys)

;;; mac-port-keys.el ends here
