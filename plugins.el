(progn (cd "~/.emacs.d") (normal-top-level-add-subdirs-to-load-path))
;; Load extensions here...

(load "cygwinsupport") ;; Minor hacks for using the cygwin shell's instead of cmd.exe.
;; There is a better library than this, upgrade when time permits.

;; Libs.
(require 'color-theme)
(require 'dropins)
(require 'autopair)
(autopair-global-mode)
(require 'cua-base)
(cua-mode 1)

;; CUA shift + click select region
 (define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
 (define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
 (put 'mouse-set-point 'CUA 'move)

(require 'darkroom-mode)
(require 'gist)
(require 'ido)
(require 'movetext)
(require 'markdown-mode)
(require 'rainbow-mode)
(require 'scrollbellfix)
(require 'sr-speedbar)
(require 'tabbar-extension)
(require 'kill-buffer-without-confirm)

;; Hideshow settings (code folding)
(require 'hideshow)

;; we use a version of hideshowvis which has
;; arrow bitmaps in the fringe, and switches
;; on the hidden number of lines view.
;; http://gist.github.com/514946
;; (require 'hideshowvis)

;; Perhaps CEDET / Semantic does this better?

(add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))

(add-to-list 'hs-special-modes-alist
	     '(css-mode "{" "}" "/[*/]" nil nil))

(dolist (hook (list 'emacs-lisp-mode-hook
		    'lisp-mode-hook
		    'ruby-mode-hook
		    'perl-mode-hook
		    'php-mode-hook
		    'python-mode-hook
		    'lua-mode-hook
		    'c-mode-hook
		    'java-mode-hook
		    'js-mode-hook
		    'css-mode-hook
		    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))

;; Start the emacs server.
(server-start nil)

;; Setup global key bindings.
(load "keybindings")
