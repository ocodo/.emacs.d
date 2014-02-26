;; Note: this is all very Emacs Cocoa specific, ie. super "s-" maps to the Cmd/Apple key.

;; for Emacs Mac, (not Emacs Cocoa) -
;; (takes care of conditional loading itself.)
;; Brings the Emacs mac key bindings (closer) into line with Emacs Cocoa.
(require 'mac-port-keys)
(require 'custom-mac-fn-keys)

;; Keys - universal

;; Newest additions at the top...

(global-set-key (kbd "M-i") 'evil-mode)

(global-set-key (kbd "C-c C-w") 'rotate-window)
(global-set-key (kbd "C-c C-l") 'rotate-layout)
(global-set-key (kbd "C-x r e") 'rgrep)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c ;")   'iedit-mode)

(global-set-key (kbd "C-c C-1") 'ruby-toggle-block)
(global-set-key (kbd "C-c C-2") 'ruby-toggle-hash-syntax)

(global-set-key (kbd "M-s M-s") 'helm-git-grep)

(global-set-key (kbd "M-P")     'fiplr-find-file)
(global-set-key (kbd "s-|")     'shell-command-on-region-replace)

;; Ret and indent binding
(global-set-key (kbd "RET")     'newline-and-indent)

;; turn off M-` menu shortcut, and use it for getting magit-status instead
(global-set-key (kbd "M-`")     'magit-status)

;; I want C-z to undo, but I keep C-x,C-c,C-v as default. If CUA mode
;; is set full, they clobber too many nice Emacs features.  Instead,
;; since I'm mac centric I bind Cmd-x,c,v to do cut/copy/paste.
(global-set-key (kbd "C-z")     'undo)

;; append region to file
(global-set-key (kbd "C-x C-a") 'append-to-file)

;; Resize window horizontally
(global-set-key (kbd "C-M-,")   'shrink-window-horizontally) ;; Ctrl-Alt-<
(global-set-key (kbd "C-M-.")   'enlarge-window-horizontally) ;; Ctrl-Alt->

;; Completion / Abbreviation

(global-set-key [(control tab)] 'completion-at-point)

;; Move line / region
(global-set-key [M-up]               'move-text-up)
(global-set-key [M-down]             'move-text-down)
(global-set-key (kbd "<ESC> <up>")   'move-text-up)
(global-set-key (kbd "<ESC> <down>") 'move-text-down)

;; duplicate region/line
(global-set-key [s-down]        'duplicate-current-line-or-region)

;; Auto fill mode toggle (tidy up text line length automatically.)
(global-set-key (kbd "C-c q")   'auto-fill-mode)

;; bind Alt-Cmd-Return to fullscreen toggle on os x in
;; window mode, if ns-toggle-fullscreen is available.
;; otherwise just max size the frame.
(when (and (window-system) (fboundp 'ns-toggle-fullscreen))
    (global-set-key (kbd "<M-s-return>") 'ns-toggle-fullscreen))

;; unset some annoying things in the OS X build, no dialogs, also add
;; a few osx centric bindings.  If you like dialogs, you probably
;; don't want my bindings, if you're over that, and just need text,
;; and no GUI dialog modality stealing focus, you're in the right
;; place.  Of course, you WILL be better off building your own emacs
;; config from scratch, borrow ideas from here or enywhere
;; else. That's the true Emacs way, (if there is one.)

(when (and (window-system) (eq system-type 'darwin))

  (message "binding osx specific shortcuts")
  ;; Cmd-o to find-file...
  (global-set-key (kbd "s-o")         'find-file)

  ;; Cmd-Shift-s : write-file (not use save-as dialog)
  (global-set-key (kbd "s-S")         'write-file)

  ;; Cmd-Shift-r : write-region ( save selection )
  (global-set-key (kbd "s-R")         'write-region)

  ;; Cmd-p : unbound from print dialog, focus stealing annoyance.
  ;; Instead bound to the useful find-file-at-point
  (global-set-key (kbd "s-p")         'find-file-at-point)

  ;; Cmd-m : unbound - I don't need minimize bound
  ;; unbind : Ctrl-z too (minimize.)
  (global-unset-key (kbd "s-m"))

  ;; Cmd-h : unbound - I don't need/want hide bound
  (global-unset-key (kbd "s-h"))
  ;; Note: Emacs Mac is unaffected, as it leaves Cmd-h to the OS, and
  ;; it'll hide anyway.

  ;; Cmd-alt-l : Load library
  (global-set-key (kbd "M-s-l")       'load-library)

  ;; cmd-t : hardwired for many people from browsers, TextMate,
  ;; etc. as open a new tab (ie. buffer) - so we bind it here as: open
  ;; a file.  By default it's bound in OSX Emacs as open the font
  ;; dialog. Really? but no thanks.
  (global-set-key (kbd "s-t")         'fiplr-find-file)

  ;; Cmd-u : is default wired to revert buffer, I quite like that, but
  ;; this seems like a good place to tag that info. (ns cocoa emacs)

  ;; Navigating around frames, windows & buffers
  (global-set-key (kbd "s-`")         'switch-window)

  (global-set-key (kbd "s-~")         'other-frame)

  (global-set-key (kbd "<s-right>")   'next-buffer)
  (global-set-key (kbd "<s-left>")    'previous-buffer)

  (global-set-key (kbd "s-b")         'switch-to-buffer)

  ;; window-splitting
  (global-set-key (kbd "s-1")         'delete-other-windows)
  (global-set-key (kbd "s-2")         'split-window-horizontally)
  (global-set-key (kbd "s-3")         'split-window-vertically)
  (global-set-key (kbd "s-4")         'delete-other-windows-vertically)

  (global-set-key (kbd "s-5")         'delete-window)
  ;; same command on Cmd-C-w
  (global-set-key [C-s-w]             'delete-window)

  ;; Text "zoom"
  (global-set-key (kbd "s--")         'text-scale-decrease)
  (global-set-key (kbd "s-=")         'text-scale-increase)

  ;; line numbers off on
  (global-set-key (kbd "s-0")         'linum-mode)

  ;; highly inclusive expand
  (global-set-key (kbd "s-/" )        'hippie-expand)

  ;; "contextual" completion
  (global-set-key (kbd "<s-return>" ) 'completion-at-point)

  ;; narrow / widen region
  (global-set-key (kbd "s-§")         'narrow-to-region)
  (global-set-key (kbd "C-§")         'widen)

  )

;; Please Note: there are still bindings littered about, I will clean this up! (one day!)

(provide 'custom-keys)
