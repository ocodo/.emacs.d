;; init --- ocodo's emacs config
;;; Commentary:
;;                       _                                             _       _
;;    ___   ___ ___   __| | ___     ___ _ __ ___   __ _  ___ ___    __| | ___ | |_
;;   / _ \ / __/ _ \ / _` |/ _ \   / _ \ '_ ` _ \ / _` |/ __/ __|  / _` |/ _ \| __|
;;  | (_) | (_| (_) | (_| | (_) | |  __/ | | | | | (_| | (__\__ \ | (_| | (_) | |_
;;   \___/ \___\___/ \__,_|\___/   \___|_| |_| |_|\__,_|\___|___/  \__,_|\___/ \__|
;;
;;; Code:

;; Custom mac osx startup look
(when (and (window-system) (eq system-type 'darwin))
  (setq mode-line-format nil))

;; Custom loaded from local (non-git) or regular (git-shared)
(let (local-custom)
  (setq local-custom (concat user-emacs-directory "local/custom.el"))
  (if (file-readable-p local-custom)
      (setq custom-file local-custom)
    (setq custom-file (concat user-emacs-directory "custom/custom.el")))
  (load custom-file))

;; Optional modes-init handling
(defun load-optional-mode-init (name)
  "Check for existence of a mode init script NAME, and load if found."
  (let (file)
    (setq file (format "%smodes-init/init-%s.el" user-emacs-directory name))
    (when (file-readable-p file)
      (load-file file))))

(defun optional-mode-inits (names)
  "Processes a list of optional mode init NAMES.
The convention used, is to store an optional init file in
`.emacs.d/modes-init/' named as init-{name} (replacing {name}
with the name you wish to use, usually the name of the
mode/feature being initialized.)

For example, for paradox, a github token is required, which you
shouldn't keep in a public git repository with the rest of your
Emacs config.  So we'd add `modes-init/init-paradox.el' to
.gitignore.

To avoid issues when we want to load the init script, we use
load-optional-mode-init to check that the script exists, before
trying to run it."

  (mapcar 'load-optional-mode-init names))

(defun load-mode-init (name)
  "Load a mode-init file NAME expect an error if it doesn't map to an existing file."
  (let (file)
    (setq file (format "%smodes-init/init-%s.el" user-emacs-directory name))
    (if (file-exists-p file)
        (load-file file)
      (message "Warning: %s doesn't exist" file))))

(defun process-mode-inits (names)
  "Process a list of mandatory mode init NAMES, convention is as above."
  (mapcar 'load-mode-init names))

(defun set-window-system-font ()
  "Set the window system font."
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
      ;; for quick swapping.
      ;; (set-face-font 'default "Bitstream Vera Sans Mono")
      ;; (set-face-font 'default "Droid Sans Mono")
      (set-face-font 'default "DejaVu Sans Mono"))))

(defun load-local-init ()
  "Load local init if found."
  (let ((local-init (concat user-emacs-directory "local/init.el")))
    (when (file-readable-p local-init)
      (load-file local-init))))

;; Manage history
(require 'savehist)
(eval-after-load "savehist-mode"
  (progn
    (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
    (setq savehist-file "~/.emacs.d/tmp/savehist")))

(savehist-mode 1)

;; turn off toolbar.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; menu bar mode only on OS X, just because it's pretty much out of
;; the way, as opposed to sitting right there in the frame.
(unless  (and (window-system) (eq system-type 'darwin))
  (menu-bar-mode -1))

(setq frame-title-format '("%b %I %+%@%t%Z %m %n %e"))

(cd user-emacs-directory)
(normal-top-level-add-subdirs-to-load-path)
(cd "~/")

;; Explicit Requires ...
(dolist (i (list
           'init-elpa
           'cl
           'cl-lib
           's
           'f
           'dash
           )
         "Mandatory Requires done.")
  (require i))

;; Requires loaded after init
(add-hook 'after-init-hook
          (lambda ()
            "Load all libraries in this list"
            (dolist (i '(
                         "handy-functions"
                         "custom-keys"
                         "ag"
                         "armitp-mode-line"
                         "diff-region"
                         "highlight-indentation"
                         "hyde-autoloads"
                         "kurecolor"
                         "iedit"
                         "js2-refactor"
                         "kill-buffer-without-confirm"
                         "mac-frame-adjust"
                         "multiple-cursors"
                         "resize-window"
                         "scroll-bell-fix"
                         "squeeze-view"
                         "switch-window"
                         "xterm-256-to-hex"
                         )) (load-library i)
                            (message "Loaded library: %s" i))
            (setq debug-on-error nil)
            (set-window-system-font)
            (load-local-init)
            ))

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
   "eww"
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
   "moonscript"
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

;;; init.el ends here
