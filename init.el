;; init --- ocodo's emacs config
;;; Commentary:
;;                       _                                             _       _
;;    ___   ___ ___   __| | ___     ___ _ __ ___   __ _  ___ ___    __| | ___ | |_
;;   / _ \ / __/ _ \ / _` |/ _ \   / _ \ '_ ` _ \ / _` |/ __/ __|  / _` |/ _ \| __|
;;  | (_) | (_| (_) | (_| | (_) | |  __/ | | | | | (_| | (__\__ \ | (_| | (_) | |_
;;   \___/ \___\___/ \__,_|\___/   \___|_| |_| |_|\__,_|\___|___/  \__,_|\___/ \__|
;;
;;; Code:
(require 'cl)
(let ((default-directory user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "init-helpers")))

(require 'init-helpers)
(init-set-custom)
(manage-toolbar-and-menubar)
(manage-history)
(set-window-system-font)
(setq debug-on-error nil)
(setq frame-title-format '("%b %I %+%@%t%Z %m %n %e"))

(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; Explicit Requires ...
(dolist (i '("init-elpa"
             "cl"
             "cl-lib"
             "s"
             "f"
             "dash"
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
             "super-num-zero-map"))
  (load-library i))

;; mode inits
(dolist (i '("asciidoc"
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
  (load-library (format "init-%s" i)))

(load-local-init)

;; Optional init modes (for example those which contain security
;; keys/tokens) - These files are added to .gitignore and only loaded
;; when present.
(mapcar 'load-optional-mode-init '("pivotal"))

;;; init.el ends here
