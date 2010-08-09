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
(require 'scrollbellfix)
(require 'sr-speedbar)
(require 'tabbar-extension)
(require 'kill-buffer-without-confirm)

(require 'hideshow)
(require 'hideshowvis)

(add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))

(dolist (hook (list 'emacs-lisp-mode-hook
		    'lisp-mode-hook
		    'ruby-mode-hook
		    'c-mode-hook
		    'java-mode-hook
		    'js-mode-hook
		    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))

(server-start nil)

(load "keybindings")
