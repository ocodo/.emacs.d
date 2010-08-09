(progn (cd "~/.emacs.d") (normal-top-level-add-subdirs-to-load-path))
;; Load extensions here...

(load "cygwinsupport") ;; Minor hacks for using the cygwin shell's instead of cmd.exe.
;; There is a better library than this, upgrade when time permits.

;; Libs.
(require 'color-theme)
(require 'dropins)
(require 'autopair) 
(autopair-global-mode)

(require 'darkroom-mode)

(require 'delsel) 
(delete-selection-mode t)

(require 'gist)
(require 'ido)
(require 'movetext)
(require 'markdown-mode)
(require 'rainbow-mode)
(require 'sr-speedbar)
(require 'tabbar-extension)
(require 'kill-buffer-without-confirm)

(require 'hideshow)
(require 'hideshowvis)

(server-start nil)

(load "keybindings")
