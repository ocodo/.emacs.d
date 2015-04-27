;; init --- ocodo's emacs config
;;; Commentary:
;;                       _                                             _       _
;;    ___   ___ ___   __| | ___     ___ _ __ ___   __ _  ___ ___    __| | ___ | |_
;;   / _ \ / __/ _ \ / _` |/ _ \   / _ \ '_ ` _ \ / _` |/ __/ __|  / _` |/ _ \| __|
;;  | (_) | (_| (_) | (_| | (_) | |  __/ | | | | | (_| | (__\__ \ | (_| | (_) | |_
;;   \___/ \___\___/ \__,_|\___/   \___|_| |_| |_|\__,_|\___|___/  \__,_|\___/ \__|
;;
;;; Code:
(let ((default-directory user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "init-helpers")))

(load-library "init-helpers")
(init-set-custom)
(manage-toolbar-and-menubar)
(manage-history)
(set-window-system-font)
(setq debug-on-error nil)
(setq frame-title-format '("%b %I %+%@%t%Z %m %n %e"))

(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; Explicit Requires ...
(dolist (lib '(init-elpa
               handy-functions
               custom-keys
               ag
               armitp-mode-line
               diff-region
               highlight-indentation
               hyde-autoloads
               kurecolor
               iedit
               js2-refactor
               kill-buffer-without-confirm
               mac-frame-adjust
               multiple-cursors
               opl-coffee
               resize-window
               scroll-bell-fix
               squeeze-view
               switch-window
               xterm-256-to-hex
               super-num-zero-map))
  (load-library (symbol-name lib)))

;; mode inits
(dolist (init '(exec-path
                asciidoc
                autocomplete
                anzu
                buffer-clean
                codenotes
                coffee
                ;; company ;; Sorry, no thank you company mode, still not comparably useful vs AC mode.
                dired
                elpa-themes
                emacs-daemon
                eww
                flycheck
                guide
                git-gutter
                haml
                hideshowvis
                json-mode
                ido
                ispell
                markdown
                misc-settings
                moonscript
                multi-web-mode
                nxml
                pivotal
                projectile
                projectile-rails
                rainbow
                rainbow-delimiters
                ruby
                rvm
                remember-theme
                sh
                smartparens
                smooth-scroll
                swiper
                winner
                yaml
                yasnippet))
  (load-mode-init (symbol-name init)))

;; When GUI (hopefully svg is available!)
;; Load an SVG Modeline
(when (and (image-type-available-p 'svg) (window-system))
  (load-library "ocodo-grass-smt")
  (smt/enable)
  (smt/set-theme 'ocodo-grass:smt))

;; This is set by some packages erroneously. (e.g. AsciiDoc)
;; send fix patches to package authors who do this.
(setq debug-on-error nil)

(load-local-init)

;; Optional init modes (for example those which contain security
;; keys/tokens) - These files are added to .gitignore and only loaded
;; when present.
;; (mapcar 'load-optional-mode-init '( ... list of optionals ... ))

;;; init.el ends here
