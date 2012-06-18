;;                     _                                             _       _   
;;  ___   ___ ___   __| | ___     ___ _ __ ___   __ _  ___ ___    __| | ___ | |_ 
;; / _ \ / __/ _ \ / _` |/ _ \   / _ \ '_ ` _ \ / _` |/ __/ __|  / _` |/ _ \| __|
;;| (_) | (_| (_) | (_| | (_) | |  __/ | | | | | (_| | (__\__ \ | (_| | (_) | |_ 
;; \___/ \___\___/ \__,_|\___/   \___|_| |_| |_|\__,_|\___|___/  \__,_|\___/ \__|
;;                                                                               
;; init.el 
;; Go dark immediately when on x, os x etc.
(when (window-system)
  (set-face-foreground 'default "#777777")
  (set-face-background 'default "#000000")
  ;; This will be reset by color theme
  )

;; turn off that horrible toolbar & menu
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

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
(add-to-list 'load-path "~/.emacs.d/el-get/el-get") (unless (require 'el-get nil t) (with-current-buffer (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el") (goto-char (point-max)) (eval-print-last-sexp))) (el-get 'sync)
;; Too unstable (same goes for package.el) for my liking, but feel free to uncomment to use el-get (apt-get for emacs).

(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; My personally installed packages (they live in ./plugins/)
(require 'resize-window               ) ;; interactively size window
(require 'dired-details+              ) ;; a better (less verbose) directory viewing mode. (requires dired-details itself.)
(require 'ls-lisp                     ) ;; ls replacement in emacs-lisp
(require 'frame-play                  ) ;; a few pre-sets for sizing and moving frames (your OS calls them windows)
(require 'ido                         ) ;; Interactively DO things...
(require 'gist                        ) ;; Github Gist mode
(require 'move-text                   ) ;; Move the current line or region up / donw. (I've bound this to M-up M-down)
(require 'markdown-mode               ) ;; Markdown text mode
(require 'textile-mode                ) ;; Textile text mode
(require 'rainbow-mode                ) ;; Colours hex rgb and other color modes (X11 color names etc.)
(require 'scroll-bell-fix             ) ;; a small hack to turn off the buffer scroll past top/end bell.
(require 'squeeze-view                ) ;; try squeeze-view-narrow or squeeze-view-wide (unsqueeze-view to turn off)
(require 'kill-buffer-without-confirm ) ;; yes, I really meant to close it.
(require 'xfrp_find_replace_pairs     ) ;; it's all in the name...
(require 'haml-mode                   ) ;; I love Haml. html made beautiful
(require 'scss-mode                   ) ;; LESS but more.
(require 'highlight-indentation       ) ;; visual guides for indentation
(require 'lorem-ipsum                 ) ;; Throw some lipsum text in there.
(require 'org-ghi                     ) ;; Github Issues in Org-Mode.
(require 'yaml-mode                   ) ;; yaml editing mode

(put 'erase-buffer     'disabled nil )
(put 'downcase-region  'disabled nil )
(put 'upcase-region    'disabled nil )
(put 'narrow-to-region 'disabled nil )

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

(setq custom-file "~/.emacs.d/custom.el") ;; Customize stuff goes in custom.el
(load-file        "~/.emacs.d/custom.el") ;; <- load customizations...
(load-file        "~/.emacs.d/keys.el"  ) ;; Key bindings live in keys.el
(server-start nil)                        ;; Start the emacs server.
(color-theme-deepblueday)                 ;; DeepBlue shades...

;;; modern/fancy modeline modification
(require 'powerline)
;;; reload after theme changes


