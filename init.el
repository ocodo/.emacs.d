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
    (setq exec-path (append '("/Developer/usr/bin") exec-path)))
  (when (file-exists-p "/Applications/Xcode.app/Contents/Developer/usr/bin")
    (setq exec-path (append '("/Applications/Xcode.app/Contents/Developer/usr/bin") exec-path)))
  (when (file-exists-p "~/.rvm/")
    (setq exec-path (append '("~/.rvm/bin") exec-path)))
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

;; 
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
(require 'coffee-mode)                 ;; http://coffeescript.org
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(require 'mustache-mode)

;;;# docs and blogs
(require 'markdown-mode)               ;; Markdown text mode
(require 'textile-mode)                ;; Textile text mode
;; AsciiDoc modw
(autoload 'asciidoc-mode "asciidoc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . asciidoc-mode))

;;;# New Edit methods 
(require 'textmate)                    ;; Textmate emulation : Tim Vishners version.
(require 'xfrp_find_replace_pairs)     ;;

(require 'frame-adjust)                  ;; a few presets for sizing and moving frames (aka OS Windows)
;; This deserves a little note here's the frame functions
;; set-frame-position-right-hand-side 
;; set-frame-position-left-hand-side 
;; set-frame-height-to-display-height
;; set-frame-height-to-85-percent-display-height 
;; set-frame-width-to-two-thirds-display-width 
;; set-frame-width-to-three-quarters-display-width 
;; set-frame-width-to-half-display-width 
;; set-frame-width-to-display-width 
;; set-frame-big-left-1 - flush to top left, two thirds wide, 100% high
;; set-frame-big-left-2 - flush to top left, three quarters wide, 100% high
;; set-frame-big-left-3 - flush to top left, three quarters wide, 85% high (I like a little room sometimes)
;; set-frame-big-right-1 - as above but flush top right.
;; set-frame-big-right-2 - ...
;; set-frame-big-right-3 - ...

;;;# Convenience and completion
(require 'move-text)                   ;; move line/region up down. (bound to meta-arrow-up/down)
(require 'auto-complete-config)        ;; Very nice autocomplete.
(ac-config-default)

;; (require 'ido)                         ;; Interactively DO things... 
(require 'lorem-ipsum)                 ;; Throw some Lorem ipsum filler text in.
(require 'switch-window)               ;; Select windows by number.

;; Blogging with Jekyll and Hyde
(require 'hyde) ;; this is going soon, I'm switching to octopress

;;;# asthetic convenience
(require 'rainbow-mode)                ;; Colours hex rgb and other color modes (X11 color names etc.)
(require 'rainbow-delimiters)          ;; Delimiter coloring
(require 'resize-window)               ;; interactively size window
(require 'highlight-indentation)       ;; visual guides for indentation
(require 'squeeze-view)                ;; squeeze view, use in conjuction with fullscreen mode

;; Stupid
(require 'nyan-mode)                   ;; The good kind of stupid

;;;# annoyance reduction.
(require 'kill-buffer-without-confirm) ;; yes, I really meant to close it.
(require 'scroll-bell-fix)             ;; a small hack to turn off the buffer scroll past top/end bell.

;;;# .. github convenience
(require 'org-ghi)                     ;; Github Issues in Org-Mode.
(require 'gist)                        ;; Github Gist mode
(require 'hexrgb)                      ;; hexrgb functions

;; Magit
(require 'magit)

(setq custom-file "~/.emacs.d/custom/custom.el") ;; Customize stuff goes in custom.el
(load custom-file)

(require 'custom-keys)                           ;; Key bindings live in keys.el

(server-start nil) ;; Start the emacs server.

;; Anything that needs to run after custom.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Default Font for different window systems
(when (window-system)

  ;; powerline
  ;; (require 'powerline) ;; off for the moment.

  ;; Mac OS X
  (when (eq system-type 'darwin)
    ;;(set-face-font 'default "Monaco")
    ;;(set-face-font 'default "Source Code Pro")
    (set-face-font 'default "Menlo")
    )
  ;; Sample Text for font viewing 
  '("ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "1234567890!@#$%^&*()-=_+[]\{}|;':<>?,./")
  
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
  ;;(set-face-font 'default "Source Code Pro")
)

(require 'handy-functions) ;; my lab area for little defuns

(unless (window-system)
  (menu-bar-mode -1)
  (load-theme 'mesa)
)

;; Trying a modeline hack - has a cleaner design than powerline, and doesn't break in HTML mode (the main dealbreaker.)
;; For powerline go back to c1b702e6c2abd2dea2306f3ea2655bac00705e86 
(load-file "~/.emacs.d/custom/mode-line-hack.el")

;; Trying out Nyan-mode for a laugh... 
(nyan-mode t) ;; on
(setq nyan-wavy-trail nil) ;; no wavy tail, I like things sensible.
(nyan-start-animation)
