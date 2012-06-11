;;                     _                                             _       _   
;;  ___   ___ ___   __| | ___     ___ _ __ ___   __ _  ___ ___    __| | ___ | |_ 
;; / _ \ / __/ _ \ / _` |/ _ \   / _ \ '_ ` _ \ / _` |/ __/ __|  / _` |/ _ \| __|
;;| (_) | (_| (_) | (_| | (_) | |  __/ | | | | | (_| | (__\__ \ | (_| | (_) | |_ 
;; \___/ \___\___/ \__,_|\___/   \___|_| |_| |_|\__,_|\___|___/  \__,_|\___/ \__|
;;                                                                               
;; init.el 
(progn (cd "~/.emacs.d") (normal-top-level-add-subdirs-to-load-path))

;; Note, I don't use auto-load very often, which can lead to Emacs taking a couple of seconds to be ready, 
;; however, I usually leave it running for days on end and OS X opens another Frame (window) 
;; when I "open with..." Emacs. 

;; with Emacs 24.1rc common-lisp should be required explicitly as soon as possible, some .el's forget to require it.
;; previous versions seem to be more lax about it.
(require 'cl) 

;; Dropins is a quick way to load a bunch of simple .el packages
;; .el's placed in the ./dropins folder will be loaded automatically.
(require 'dropins)
(load-dropins)

;; el-get installer
; (add-to-list 'load-path "~/.emacs.d/el-get/el-get") (unless (require 'el-get nil t) (with-current-buffer (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el") (goto-char (point-max)) (eval-print-last-sexp))) (el-get 'sync)
;; Too unstable (same goes for package.el) for my liking, but feel free to uncomment to use el-get (apt-get for emacs).

(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; My personally installed packages (they live in ./plugins/)
(progn 
  (require 'resize-window)
  (require 'dired-details+)
  (require 'ls-lisp)
  (require 'frame-play)
  (require 'ido)
  (require 'gist)
  (require 'move-text)
  (require 'markdown-mode)
  (require 'textile-mode)
  (require 'rainbow-mode)
  (require 'scroll-bell-fix)
  (require 'squeeze-view)
  (require 'kill-buffer-without-confirm)
  (require 'xfrp_find_replace_pairs)
  (require 'haml-mode)
  (require 'scss-mode)
  (require 'highlight-indentation)
  (require 'lorem-ipsum)
  (require 'org-ghi)
)


;; turn off menubar - uncomment
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; turn off toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(require 'yasnippet-bundle)
(yas/load-directory "~/.emacs.d/snippets" )

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(when (window-system)
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

;; Customize stuff goes in custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load-file "~/.emacs.d/custom.el")

;; Key bindings live in keys.el
(load-file "~/.emacs.d/keys.el")

;; One of my custom themes, blue shades on a very dark blue.
(color-theme-deepblueday)

;; 3/4 width to the left of the display.
(big-left-3)

;; Start the emacs server.
(server-start nil)
