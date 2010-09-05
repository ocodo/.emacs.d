(progn (cd "~/.emacs.d") (normal-top-level-add-subdirs-to-load-path))
;; Load extensions here...

(load "cygwin-support") ;; Minor hacks for using the cygwin shell's instead of cmd.exe.
;; There is a better library than this, upgrade when time permits.

;; Libs.
(require 'color-theme)
(require 'dropins)
(require 'autopair)
(autopair-global-mode)
(require 'cua-base)
(cua-mode 1)
(setq cua-auto-tabify-rectangles nil)

;; CUA shift + click select region
 (define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
 (define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
 (put 'mouse-set-point 'CUA 'move)

(require 'ido)
(require 'move-text)
(require 'markdown-mode)
(require 'textile-mode)
(require 'rainbow-mode)
(require 'scroll-bell-fix)
(require 'squeeze-view)
(require 'sr-speedbar)
(require 'sunrise-commander)
(require 'kill-buffer-without-confirm)
(require 'xfrp_find_replace_pairs)

(require 'package)

;; (require 'csharp-mode)

;; (setq auto-mode-alist
;;       (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
;; (defun my-csharp-mode-fn ()
;;   "function that runs when csharp-mode is initialized for a buffer."
;;   ...insert your code here...
;;   ...most commonly, your custom key bindings ...
;;   )
;; (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C# code completion (requires CEDET semantic)

;; (setq load-path
;;       (append '("~/users/dinoch/elisp/cedet/semantic"
;; 		"~/users/dinoch/elisp/cedet/semantic/bovine"
;; 		"~/users/dinoch/elisp/cedet/common"
;; 		"~/users/dinoch/elisp/cedet/eieio"
;; 		"~/users/dinoch/elisp/cedet/contrib"
;; 		)  load-path ))

;; (load "semantic")
;; (load "semantic-load")
;; (load "wisent-csharp")

;; (require 'csharp-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All specific non terminal mode stuff.
(when (window-system)
  (require 'darkroom-mode)
  (require 'tabbar-extension)
;; Hideshow settings (code folding)
;; (require 'hideshow)

;; we use a version of hideshowvis which has
;; arrow bitmaps in the fringe, and switches
;; on the hidden number of lines view.
;; http://gist.github.com/514946
  (require 'hideshowvis)
  (add-to-list 'hs-special-modes-alist
	       '(ruby-mode
		 "\\(def\\|do\\|{\\)"
		 "\\(end\\|\\)}"
		 "#"
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
    (add-hook hook 'hideshowvis-enable)))

;; Things to run specifically when in a terminal
(when (not (window-system))
  (menu-bar-mode -1))

;; Start the emacs server.
(server-start nil)

;; Setup global key bindings.
(load "keybindings")
