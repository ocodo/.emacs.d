;;                       _                                             _       _
;;    ___   ___ ___   __| | ___     ___ _ __ ___   __ _  ___ ___    __| | ___ | |_
;;   / _ \ / __/ _ \ / _` |/ _ \   / _ \ '_ ` _ \ / _` |/ __/ __|  / _` |/ _ \| __|
;;  | (_) | (_| (_) | (_| | (_) | |  __/ | | | | | (_| | (__\__ \ | (_| | (_) | |_
;;   \___/ \___\___/ \__,_|\___/   \___|_| |_| |_|\__,_|\___|___/  \__,_|\___/ \__|
;;
;; --

;; First a dirty, but cheap way to get .emacs.d subfolders into the load path,
;; and then return us to the user home directory, for find-file etc.
(progn (cd "~/.emacs.d/") (normal-top-level-add-subdirs-to-load-path) (cd "~"))

;; -- Path -----------------------------------------------------------------------------------------------
;; find XCode and RVM command line tools on OSX (cover the legacy and current XCode directory structures.)
(when (eq system-type 'darwin)
  (when (file-exists-p "/Developer/usr/bin")
    (setq exec-path (append '("/Developer/usr/bin") exec-path)))
  (when (file-exists-p "/Applications/Xcode.app/Contents/Developer/usr/bin")
    (setq exec-path (append '("/Applications/Xcode.app/Contents/Developer/usr/bin") exec-path)))
  (when (file-exists-p "~/.rvm/bin")
    (rvm-use-default)
    (setq exec-path (append '("~/.rvm/bin") exec-path)))
  (when (file-exists-p "/usr/local/bin/")
    (setq exec-path (append '("/usr/local/bin") exec-path)))
  (when (file-exists-p "/usr/local/share/npm/bin")
    (setq exec-path (append '("/usr/local/share/npm/bin") exec-path))))

(add-to-list 'exec-path "~/bin")

;; turn off toolbar.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; menu bar mode only on OS X, just because it's pretty much out of
;; the way, as opposed to sitting right there in the frame.
(if  (and (window-system) (eq system-type 'darwin))
    (menu-bar-mode 1)
  (menu-bar-mode -1)
)

;; Modes init (things that need more than just a require.)
(when (string-match "Emacs 24" (version))
  (message "Running Emacs 24")
  ;; Only run elpa on E24
  (require 'init-elpa)
)

;; Explicitly require libs that autoload borks
;; Include common lisp
(require 'cl) ;; one day can remove this...
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)
(require 'iedit)
(require 'ag)
(require 'multiple-cursors)
(require 'js2-refactor)

;; Custom themes from elpa/melpa/marmalade added to load-path
(-each
 (-map
  (lambda (item)
    (format "~/.emacs.d/elpa/%s" item))
  (-filter
   (lambda (item) (s-contains? "theme" item))
   (directory-files "~/.emacs.d/elpa")))
 (lambda (item)
   (add-to-list 'custom-theme-load-path item)))

(setq custom-file "~/.emacs.d/custom/custom.el") ;; Customize stuff goes in custom.el
(load custom-file)
(require 'custom-keys)

(setq frame-title-format '("%b %I %+%@%t%Z %m %n %e"))

(load-theme 'clues)
;; -------------------------------------------------------------------------------------------------
;; Additional requires
;; -------------------------------------------------------------------------------------------------
;; Emacs Mac port specific frame adjust
(require 'mac-frame-adjust)            ;; a few presets for sizing and moving frames (aka OS Windows)

;;; Convenience and completion
(require 'auto-complete-config)        ;; Very nice autocomplete.
(ac-config-default)

(require 'dropdown-list)               ;; dropdown list for use with yasnippet
(require 'switch-window)               ;; Select windows by number.
(require 'resize-window)               ;; interactively size window
(require 'highlight-indentation)       ;; visual guides for indentation
(require 'squeeze-view)                ;; squeeze view, give yourself a write-room/typewriter like writing page
(require 'kill-buffer-without-confirm) ;; yes, I really meant to close it.
(require 'scroll-bell-fix)             ;; a small hack to turn off the buffer scroll past top/end bell.

;; auto-load hyde mode for Jekyll
(require 'hyde-autoloads) ;; ./vendor/hyde

;; Safe cleanup, probably a package?
(defun safe-buffer-cleanup ()
  "Clean whitespace, kill tabs, set to UTF8"
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(add-hook 'before-save-hook 'safe-buffer-cleanup)

;; Check for w3m
(if
    (s-contains? "w3m not found" (shell-command-to-string "which w3m"))
     (message "w3m not available")
  (require 'w3m))

;; --- Main-line only on window systems ----- (a fork of Powerline
(when (window-system)
  (load-library "main-line")
  (setq main-line-separator-style 'wave))

;; -------------------------------------------------------------------------------------------------
;; Explicit mode inits
(require 'init-dired)
(require 'init-hideshowvis)
(require 'init-multi-web-mode)
(require 'init-nxml)
(require 'init-flymake)
(require 'init-markdown)
(require 'init-ruby)

;; conditional - add your own init-marmalade or just login manually
(load-library "marmalade")
;; modes-init/init-marmalade.el is in .gitignore
(when (file-readable-p "modes-init/init-marmalade.el")
  (load-file "modes-init/init-marmalade.el"))

(require 'handy-functions) ;; my lab area for little defuns...

;; Turn on things that auto-load isn't doing for us...
(yas-global-mode t)

;; Autopair alternative
(flex-autopair-mode t)

;; probably all become auto-loaded. (next time)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; AsciiDoc mode
(autoload 'asciidoc-mode "asciidoc-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.asciidoc$" . asciidoc-mode))

;; Auto add executable permissions to scripts when saved
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Rainbow mode for css automatically
(add-hook 'css-mode-hook 'rainbow-mode)

;; Rainbow delimiters for all prog modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Git gutter global mode
(add-hook 'prog-mode-hook 'git-gutter-mode)

;; Smoother scrolling (no multiline jumps.)
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)

;; show paren mode
(show-paren-mode 1)
(setq show-paren-delay 0)

;; use y / n instead of yes / no
(fset 'yes-or-no-p 'y-or-n-p)

;; allow "restricted" features
(put 'set-goal-column           'disabled nil)
(put 'erase-buffer              'disabled nil)
(put 'downcase-region           'disabled nil)
(put 'upcase-region             'disabled nil)
(put 'narrow-to-region          'disabled nil)
(put 'narrow-to-page            'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; -------------------------------------------------------------------------------------------------
;; Highlight TODO/FIXME/BUG/HACK/REFACTOR & THE HORROR in code - I'm hoping the last one will catch on.
(add-hook 'prog-mode-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\<\\(NOTE\\|FIXME\\|TODO\\|BUG\\|HACK\\|REFACTOR\\|THE HORROR\\)" 1 font-lock-warning-face t)))))

;; -------------------------------------------------------------------------------------------------
;; use aspell for ispell
(when (file-exists-p "/usr/local/bin/aspell")
  (set-variable 'ispell-program-name "/usr/local/bin/aspell"))

;; -------------------------------------------------------------------------------------------------
;; JavaScript/JSON special files
(dolist (pattern '("\\.jshintrc$" "\\.jslint$"))
  (add-to-list 'auto-mode-alist (cons pattern 'json-mode)))

;; Start Emacs Server
(server-start)

;; Default Font for different window systems
(when (window-system)
 (global-linum-mode 1)
  ;; Mac OS X
  (when (eq system-type 'darwin)
    ;;(set-face-font 'default "Monaco")
    ;;(set-face-font 'default "Source Code Pro")
    (set-face-font 'default "Menlo"))
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
    (set-face-font 'default "Droid Sans Mono") ;; for quick swapping.
    ;; (set-face-font 'default "Bitstream Vera Sans Mono")
    )
)


(set-face-attribute 'default nil :height 140)
(put 'scroll-left 'disabled nil)
