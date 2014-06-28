;;; init --- ocodo's emacs config
;;; Commentary:
;;                       _                                             _       _
;;    ___   ___ ___   __| | ___     ___ _ __ ___   __ _  ___ ___    __| | ___ | |_
;;   / _ \ / __/ _ \ / _` |/ _ \   / _ \ '_ ` _ \ / _` |/ __/ __|  / _` |/ _ \| __|
;;  | (_) | (_| (_) | (_| | (_) | |  __/ | | | | | (_| | (__\__ \ | (_| | (_) | |_
;;   \___/ \___\___/ \__,_|\___/   \___|_| |_| |_|\__,_|\___|___/  \__,_|\___/ \__|
;;
;;; Code:

(setq custom-file (concat user-emacs-directory "custom/custom.el"))
(load custom-file)

;; turn off toolbar.
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; menu bar mode only on OS X, just because it's pretty much out of
;; the way, as opposed to sitting right there in the frame.
(if  (and (window-system) (eq system-type 'darwin))
    (menu-bar-mode 1)
  (menu-bar-mode -1))
(setq frame-title-format '("%b %I %+%@%t%Z %m %n %e"))

(progn
  (cd user-emacs-directory)
  (normal-top-level-add-subdirs-to-load-path)
  (cd "~/"))


;; Explicit Requires ...
(add-hook 'after-init-hook
          (lambda ())
          "require and init"
          (mapcar 'require
                  (list
                   'cl
                   'cl-lib
                   'init-elpa
                   's
                   'dash
                   'f
                   'handy-functions
                   'custom-keys
                   'ag
                   'armitp-mode-line
                   'auto-complete-config
                   'dropdown-list
                   'highlight-indentation
                   'hyde-autoloads
                   'iedit
                   'js2-refactor
                   'kill-buffer-without-confirm
                   'mac-frame-adjust
                   'multiple-cursors
                   'resize-window
                   'scroll-bell-fix
                   'squeeze-view
                   'switch-window)))

;; mode inits
(process-mode-inits
 '("asciidoc"
   "autocomplete"
   "buffer-clean"
   "codenotes"
   "coffee"
   "dired"
   "elpa-themes"
   "emacs-daemon"
   "exec-path"
   "flycheck"
   "guide"
   "git-gutter"
   "haml"
   "hideshowvis"
   "json-mode"
   "ido"
   "ispell"
   "markdown"
   "misc-settings"
   "multi-web-mode"
   "nxml"
   "projectile"
   "projectile-rails"
   "rainbow"
   "rainbow-delimiters"
   "ruby"
   "rvm"
   "remember-theme"
   "sh"
   "smartparens"
   "smooth-scroll"
   "winner"
   "yaml"
   "yasnippet"))

(load-library "marmalade")

;; Optional init modes (for example those which contain security
;; keys/tokens) - These files are added to .gitignore and only loaded
;; when present.
(optional-mode-inits
 '("marmalade"
   "pivotal"
   "paradox"))

;; Default Font for different window systems
(when (window-system)
  (global-linum-mode 1)
  ;; Mac OS X
  (when (eq system-type 'darwin)
    ;;(set-face-font 'default "Monaco")
    ;;(set-face-font 'default "Source Code Pro")
    ;;(set-face-font 'default "Source Code Pro Light")
    ;;(set-face-font 'default "Inconsolata")
    ;;(set-face-font 'default "Bitstream Vera Sans Mono")
    (set-face-font 'default "Menlo"))
  ;; Sample Text for font viewing
  '("ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "1234567890!@#$%^&*()-=_+[]\{}|;':<>?,./")

  ;; Windows whatever...
  (when (eq system-type 'windows-nt)
    (set-face-font 'default "Consolas"))

  ;; GNU Linux (Droid or Vera)
  (when (eq system-type 'gnu/linux)
    (set-face-font 'default "Droid Sans Mono") ;; for quick swapping.
    ;; (set-face-font 'default "Bitstream Vera Sans Mono")
    ))

;;; init.el ends here
