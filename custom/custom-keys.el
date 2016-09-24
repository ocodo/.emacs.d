;; These binding are for both Emacs Mac and Emacs Cocoa (25.0~) (it
;; should take care of conditional binding itself.)  Brings the Emacs
;; mac key bindings closer into line with Emacs Cocoa. Also adds a lot
;; of Mac specific bindings to Super (ie. ⌘) and elsewhere

;; Many universal / terminal

;; Keys - universal

;; Newest additions at the top...

;; PLEASE NOTE: to be more modular, these bindings will be moved out
;; to their respective modes init (see ../modes-init/init-*.el)

;;; Code:

(require 'mac-port-keys)
(require 'custom-mac-fn-keys)
(require 'misc)

(global-set-key (kbd "M-o w")        'other-window) ;; use as Esc, o, w (go Other Window)

(global-set-key (kbd "s-g")          'minibuffer-keyboard-quit)

(global-set-key (kbd "<home>")       'beginning-of-line)
(global-set-key (kbd "<end>")        'end-of-line)

(global-set-key (kbd "C-x /")        'align-regexp)

(global-set-key (kbd "C-^")          'join-line-from-below)

(global-set-key (kbd "C-x r u")      'cua-set-rectangle-mark)
(global-set-key (kbd "M-Z")          'zap-to-char)
(global-set-key (kbd "M-z")          'zap-up-to-char)

(global-set-key (kbd "M-i")          'evil-mode) ;; Toggle

(global-set-key (kbd "M-;")          'comment-dwim-2)

;; Migrate to init-rotate
(global-set-key (kbd "C-c C-w")      'rotate-window)
(global-set-key (kbd "C-c C-l")      'rotate-layout)

(global-set-key (kbd "C-c SPC")      'ace-jump-mode)
(global-set-key (kbd "C-c ;")        'iedit-mode)

;; Return and indent binding
(global-set-key (kbd "RET")          'newline-and-indent)

;; turn off M-` menu shortcut, and use it for getting magit-status instead
(global-set-key (kbd "M-`")          'magit-status)

;; append region to file
(global-set-key (kbd "C-x C-a")      'append-to-file)

;; Resize window horizontally
(global-set-key (kbd "C-M-,")        'shrink-window-horizontally) ;; Ctrl-Alt-<
(global-set-key (kbd "C-M-.")        'enlarge-window-horizontally) ;; Ctrl-Alt->

;; Completion / Abbreviation
(global-set-key [(control tab)]      'completion-at-point)

;; Eval
(global-set-key (kbd "C-x <ESC> e")  'eval-buffer)
(global-set-key (kbd "M-<ESC> e")    'eval-buffer)

;; Auto fill mode toggle (tidy up text line length automatically.)
(global-set-key (kbd "C-c q")        'auto-fill-mode)

;; unset some annoying things in the OS X build, no dialogs, also add
;; a few osx centric bindings.  If you like dialogs, you probably
;; don't want my bindings, if you're over that, and just need text,
;; and no GUI dialog modality stealing focus, you're in the right
;; place.  Of course, you WILL be better off building your own emacs
;; config from scratch, borrow ideas from here or enywhere
;; else. That's the true Emacs way, (if there is one.)

(when (and (window-system) (or (eq system-type  'darwin) (eq system-type 'gnu/linux)))

  (message "binding Super Key shortcuts - slightly osx specific, work with GNU/Linux too")

  ;; Toggle fullscreen (>= emacs-version 24.4)
  (when (>= (string-to-number (format "%i.%i"  emacs-major-version emacs-minor-version)) 24.4)
    (global-set-key (kbd "<S-s-return>")    'toggle-fullscreen))

  ;; Cmd-o to find-file...
  (global-set-key (kbd "s-o")               'find-file)

  ;; rotate layout/window
  (global-set-key (kbd "s-8")               'rotate-window)
  (global-set-key (kbd "s-7")               'rotate-layout)
  ;; Cmd-Shift-s : write-file (not use save-as dialog)
  (global-set-key (kbd "s-S")               'write-file)

  ;; Cmd-Shift-r : write-region ( save selection )
  (global-set-key (kbd "s-R")               'write-region)

  ;; Cmd-t : fuzzy open file in project
  (global-set-key (kbd "s-t")               'projectile-find-file)

  ;; Cmd-p : unbound from print dialog, focus stealing annoyance.
  ;; Instead bound to the useful find-file-at-point
  (global-set-key (kbd "s-p")               'find-file-at-point)

  ;; Cmd-m : unbound - I don't need minimize bound
  ;; unbind : Ctrl-z too (minimize.)
  (global-unset-key (kbd "s-m"))

  ;; Cmd-h : unbound - I don't need/want hide bound
  (global-unset-key (kbd "s-h"))
  ;; Note: Emacs Mac is unaffected, as it leaves Cmd-h to the OS, and
  ;; it'll hide anyway.

  ;; Cmd-alt-l : Load library
  (global-set-key (kbd "M-s-l")             'load-library)

  ;; Navigating around frames, windows & buffers
  (global-set-key (kbd "s-`")               'switch-window)

  (global-set-key (kbd "s-~")               'other-frame)

  (global-set-key (kbd "<s-right>")         'next-buffer)
  (global-set-key (kbd "<s-left>")          'previous-buffer)

  (global-set-key (kbd "s-b")               'switch-to-buffer)

  (global-set-key (kbd "s-|")               'shell-command-on-region-replace)

  ;; window-splitting
  (global-set-key (kbd "s-1")               'delete-other-windows)
  (global-set-key (kbd "s-2")               'split-window-vertically)
  (global-set-key (kbd "s-3")               'split-window-horizontally)
  (global-set-key (kbd "s-4")               'delete-other-windows-vertically)

  (global-set-key (kbd "s-0")               'delete-window)
  ;; same command on Cmd-C-w
  (global-set-key [C-s-w]                   'delete-window)

  ;; Text "zoom"
  (global-set-key (kbd "s--")               'text-scale-decrease)
  (global-set-key (kbd "s-=")               'text-scale-increase)

  ;; line numbers off on
  (global-set-key (kbd "s-\\")              'linum-mode)

  ;; highly inclusive expand
  (global-set-key (kbd "s-/" )              'hippie-expand)

  ;; "contextual" completion
  (global-set-key (kbd "<s-return>" )       'completion-at-point)

  )

(provide 'custom-keys)
