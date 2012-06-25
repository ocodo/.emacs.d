;;                       _                                             _       _ 
;;    ___   ___ ___   __| | ___     ___ _ __ ___   __ _  ___ ___    __| | ___ | |_ 
;;   / _ \ / __/ _ \ / _` |/ _ \   / _ \ '_ ` _ \ / _` |/ __/ __|  / _` |/ _ \| __|
;;  | (_) | (_| (_) | (_| | (_) | |  __/ | | | | | (_| | (__\__ \ | (_| | (_) | |_ 
;;   \___/ \___\___/ \__,_|\___/   \___|_| |_| |_|\__,_|\___|___/  \__,_|\___/ \__|
;;                                                                                 

;; init.el 
(require 'cl)

;; dirty, but cheap way to get .emacs.d subfolders into the load path.
(progn (cd "~/.emacs.d/") (normal-top-level-add-subdirs-to-load-path) (cd "~"))

;; Go dark immediately when on x, os x etc.
(when (window-system)
  (set-face-foreground 'default "#777777")
  (set-face-background 'default "#000000")
  ;; This will be reset by color theme
  )

;; turn off toolbar 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(& uncomment this to turn off menu)
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(put 'erase-buffer     'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; .el's placed in the ./dropins folder will be loaded automatically.
(require 'dropins)
(load-dropins)

;; default color theme.
(color-theme-deepblueday)

;; Modes init (things that need more than just a require.) 
(require 'init-el-get)
(require 'init-elpa)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-hideshowvis)   
 
;; manually installed packages (find them in ./plugins/) - these could
;; probably all become auto-loaded. (next time)

;;;# specialized syntax editing
(require 'haml-mode)                   ;; http://haml.info 
(require 'scss-mode)                   ;; http://sass-lang.com 
(require 'yaml-mode)

;;;# docs and blogs
(require 'markdown-mode)               ;; Markdown text mode
(require 'textile-mode)                ;; Textile text mode

;;;# New Edit methods 
(require 'move-text)                   ;; Move the current line or region up / done.
(require 'xfrp_find_replace_pairs) 

;;;# Convenience and completion
(require 'ido)                         ;; Interactively DO things...
(require 'lorem-ipsum)                 ;; Throw some Lorem ipsum filler text in.
(require 'switch-window)               ;; Select windows by number.

;;;# asthetic convenience
(require 'rainbow-mode)                ;; Colours hex rgb and other color modes (X11 color names etc.)
(require 'resize-window)               ;; interactively size window
(require 'highlight-indentation)       ;; visual guides for indentation
(require 'squeeze-view)                ;; try squeeze-view-narrow or squeeze-view-wide (unsqueeze-view to turn off)

;;;# annoyance reduction.
(require 'frame-play)                  ;; a few presets for sizing and moving frames (your OS calls them windows)
(require 'kill-buffer-without-confirm) ;; yes, I really meant to close it.
(require 'scroll-bell-fix)             ;; a small hack to turn off the buffer scroll past top/end bell.

;;;# .. github convenience
(require 'org-ghi)                     ;; Github Issues in Org-Mode.
(require 'gist)                        ;; Github Gist mode

;;; modern/fancy modeline modification, seems to need a reload after
;;; the theme changes - it's a bit experimental / unstable.
(require 'powerline)
;; Notes on Powerline:
;; rounded radius / chamfer size
;; 
;; This is as good a place as any to make a note about the powerline
;; bitmaps I've added, as of this moment I've placed a few fixed size
;; bitmap images into the powerline.el however, I think it would be a
;; lot nicer to be able to specify the rounded corner or chamfer
;; radius.
;;
;; But I'll need a couple of geometric functions and I'm short on time
;; to do this now, so this is a note to bug me in the future.
;; 

(setq custom-file "~/.emacs.d/custom/custom.el") ;; Customize stuff goes in custom.el
(load-file        "~/.emacs.d/custom/custom.el") ;; <- load customizations...

(require 'custom-keys)                           ;; Key bindings live in keys.el

(server-start nil) ;; Start the emacs server.

;; Anything that needs to run after custom.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Quick access to default font face for fickle f-users, like me.
(set-face-font 'default "Monaco")

;;(set-face-font 'default "Inconsolata")
;;(set-face-font 'default "Menlo")
;;(set-face-font 'default "Bitstream Vera Sans Mono")
;;(set-face-font 'default "Consolas")
;;(set-face-font 'default "Droid Sans Mono")

;; default color theme
(color-theme-deepblueday)                 ;; DeepBlue shades...
