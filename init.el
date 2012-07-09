;;                       _                                             _       _ 
;;    ___   ___ ___   __| | ___     ___ _ __ ___   __ _  ___ ___    __| | ___ | |_ 
;;   / _ \ / __/ _ \ / _` |/ _ \   / _ \ '_ ` _ \ / _` |/ __/ __|  / _` |/ _ \| __|
;;  | (_) | (_| (_) | (_| | (_) | |  __/ | | | | | (_| | (__\__ \ | (_| | (_) | |_ 
;;   \___/ \___\___/ \__,_|\___/   \___|_| |_| |_|\__,_|\___|___/  \__,_|\___/ \__|
;;                                          

;; A little hack to find Git on systems that only have XCode installed
;; versions of Git. (git is now required for many Emacs things now.)
(when (eq system-type 'darwin)
  (when (file-exists-p "/Developer/usr/bin")
    (setq exec-path (append exec-path '("/Developer/usr/bin"))))
  (when (file-exists-p "/Applications/Xcode.app/Contents/Developer/usr/bin")
    (setq exec-path (append exec-path '("/Applications/Xcode.app/Contents/Developer/usr/bin"))))
  )

(require 'cl)

;; dirty, but cheap way to get .emacs.d subfolders into the load path,
;; and then return us to the user home directory, for find-file etc.
(progn (cd "~/.emacs.d/") (normal-top-level-add-subdirs-to-load-path) (cd "~"))

;; turn off toolbar 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(& uncomment this to turn off menu)
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; assume you'd prefer y / n to typing yes / no
;; with power comes responsibility. 
;; Comment out for standard yes/no behaviour.
(fset 'yes-or-no-p 'y-or-n-p)

(put 'erase-buffer     'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Modes init (things that need more than just a require.) 
(when (string-match "Emacs 24" (version))
  (message "Running Emacs 24")
  ;; Emacs 24 Specific stuff...
  (require 'init-elpa)
)

(when (string-match "Emacs 23" (version))
 (message "Running Emacs 23")
 ;; Emacs 23 Specific stuff...
)

(require 'init-el-get)
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
(require 'textmate)                    ;; Textmate emulation : Tim Vishners version.
(require 'move-text)                   ;; Move the current line or region up / done.
(require 'xfrp_find_replace_pairs)     ;;

;;;# Convenience and completion
(require 'ido)                         ;; Interactively DO things...
(require 'lorem-ipsum)                 ;; Throw some Lorem ipsum filler text in.
(require 'switch-window)               ;; Select windows by number.

;; Blogging with Jekyll and Hyde
(require 'hyde)

;;;# asthetic convenience
(require 'rainbow-mode)                ;; Colours hex rgb and other color modes (X11 color names etc.)
(require 'resize-window)               ;; interactively size window
(require 'highlight-indentation)       ;; visual guides for indentation
(require 'squeeze-view)                ;; squeeze view, use in conjuction with fullscreen mode

;;;# annoyance reduction.
(require 'frame-play)                  ;; a few presets for sizing and moving frames (your OS calls them windows)
(require 'kill-buffer-without-confirm) ;; yes, I really meant to close it.
(require 'scroll-bell-fix)             ;; a small hack to turn off the buffer scroll past top/end bell.

;;;# .. github convenience
(require 'org-ghi)                     ;; Github Issues in Org-Mode.
(require 'gist)                        ;; Github Gist mode
(require 'hexrgb)                      ;; hexrgb functions

;; Magit
(require 'magit)

(when (window-system)

;; window system stuff (only tested on Emacs.app Mac OS X.)

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
)

(setq custom-file "~/.emacs.d/custom/custom.el") ;; Customize stuff goes in custom.el
(load custom-file)

(require 'custom-keys)                           ;; Key bindings live in keys.el

(server-start nil) ;; Start the emacs server.

;; Anything that needs to run after custom.
(add-hook 'text-mode-hook 'turn-on-auto-fill)


;; Default Font for different window systems
(when (window-system)

  ;; Mac OS X
  (when (eq system-type 'darwin)
    (set-face-font 'default "Monaco")
    )

  ;; Windows whatever...
  (when (eq system-type 'windows-nt)
    (set-face-font 'default "Consolas")
    )

  ;; GNU Linux (Droid or Vera)
  (when (eq system-type 'gnu/linux)
    ;; (set-face-font 'default "Droid Sans Mono") ;; for quick swapping.
    (set-face-font 'default "Bitstream Vera Sans Mono")
    )

  ;; Quick access for some alternatives..
  ;;(set-face-font 'default "Inconsolata")
  ;;(set-face-font 'default "Monaco")
  ;;(set-face-font 'default "Menlo")
  ;;(set-face-font 'default "Bitstream Vera Sans Mono")
  ;;(set-face-font 'default "Consolas")
  ;;(set-face-font 'default "Droid Sans Mono")
)

(require 'handy-functions) ;; my lab area for little defuns

(unless (window-system)
  (message "running on the terminal")
  ;; Do your terminal specific stuff.
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

  (load-theme 'Jadedragon)
  ;; Works with Emacs 23 & 24, earlier versions... sorry.
  ;; 256 color xterm compatible
)
