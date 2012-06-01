;; init.el 
(progn (cd "~/.emacs.d") (normal-top-level-add-subdirs-to-load-path))

(require 'color-theme)
(require 'dropins)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get") (unless (require 'el-get nil t) (with-current-buffer (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el") (goto-char (point-max)) (eval-print-last-sexp))) (el-get 'sync)

(require 'resize-window)
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

;; turn off menubar - uncomment
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; turn off toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(require 'yasnippet-bundle)
(yas/load-directory "~/.emacs.d/snippets" )

;; We default, with customize, to MacBook settings 
  (set-face-attribute 'default nil
		      :family "Monaco"
		      :height 130)
;; ...and override here.

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Load a nice theme...
;;(defun color-theme-tq2323 () (interactive) (color-theme-install '(color-theme-tq2323 ((background-color . "#000407") (foreground-color . "#ddfffc") (background-mode . dark) (border-color . "#052e2d") (cursor-color . "#0d6d6c") (mouse-color . "#323232")) (fringe ((t (:background "#052e2d")))) (mode-line ((t (:foreground "#9cf6f4" :background "#0b2c2d")))) (region ((t (:background "#0c5f59")))) (font-lock-builtin-face ((t (:foreground "#0b8e87")))) (font-lock-comment-face ((t (:foreground "#265f59")))) (font-lock-function-name-face ((t (:foreground "#60c3be")))) (font-lock-keyword-face ((t (:foreground "#0abda7")))) (font-lock-string-face ((t (:foreground "#aaedee")))) (font-lock-type-face ((t (:foreground"#1f8e8a")))) (font-lock-constant-face ((t (:foreground "#1ae9d7")))) (font-lock-variable-name-face ((t (:foreground "#0ebeb8")))) (minibuffer-prompt ((t (:foreground "#00faf2" :bold t)))) (font-lock-warning-face ((t (:foreground "red" :bold t)))))))
;;(color-theme-tq2323)
;; this is now external... 

(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-verbosity nil)

(defadvice ls-lisp-format (around my-ls-lisp-format 
  (file-name file-attr file-size switches time-index now))
  "Advice definition which removes unnecessary information
during file listing in dired. For such purposes 
`ls-lisp-verbosity' customized variable can be used, but 
even if it is equal to nil dired will display file 
permissions field like \"drwxrwxrwx\".\. So here we just 
get full control to what dired shows and leave only those 
fields which we need."
  (progn
    ad-do-it
    (setq ad-return-value 
          (concat (substring ad-return-value 0 1)
                  " "
                  (substring ad-return-value 29 )))))
(ad-activate 'ls-lisp-format t)

(when (window-system)
  (require 'darkroom-mode)

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


;; Setup global key bindings.
;; C-n find-file
(global-set-key [(control n)] 'find-file)

;; Move between buffers with M-[ and M-]

(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-[") 'previous-buffer)

(global-set-key (kbd "<s-right>") 'next-buffer)
(global-set-key (kbd "<s-left>") 'previous-buffer)

(global-set-key (kbd "s-/" ) 'dabbrev-expand)

(global-set-key (kbd "s-b") 'switch-to-buffer)

(global-set-key (kbd "s--") 'text-scale-decrease) 
(global-set-key (kbd "s-=") 'text-scale-increase) 

(global-set-key (kbd "s-n") 'find-file)

(global-set-key (kbd "s-1") 'delete-other-windows)
(global-set-key (kbd "s-2") 'split-window-horizontally)
(global-set-key (kbd "s-3") 'split-window-vertically)
(global-set-key (kbd "s-4") 'delete-other-windows-vertically)
(global-set-key (kbd "s-5") 'delete-window)


(global-set-key (kbd "C-M-,") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-.") 'enlarge-window-horizontally)

(global-set-key (kbd "s-`") 'other-window)
(global-set-key (kbd "s-~") 'other-frame)

;; completion at point
(global-set-key [(control tab)] 'completion-at-point)
(global-set-key (kbd "<s-return>" ) 'completion-at-point)

;; Darkroom additional bindings
(global-set-key '[M-S-return] 'darkroom-mode)
(global-set-key '[M-s-return] 'darkroom-mode)

;; Linum mode toggle
(global-set-key (kbd "s-0") 'linum-mode)

;; Dired mode additions

(eval-after-load 'dired
  '(define-key dired-mode-map [mouse-3] 'dired-maybe-insert-subdir))

;; Start the emacs server.
(server-start nil)

;; Customize stuff... 
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-install-save-confirm nil)
 '(color-theme-illegal-faces "^w3-")
 '(color-theme-is-cumulative t)
 '(color-theme-mode-hook nil)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(dired-listing-switches "-alhF")
 '(ediff-custom-diff-program "diff")
 '(ediff-diff-program "diff")
 '(ediff-diff3-program "diff3")
 '(fringe-mode 8 nil (fringe))
 '(global-hl-line-mode nil)
 '(global-linum-mode t)
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote right))
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(initial-major-mode (quote markdown-mode))
 '(initial-scratch-message nil)
 '(keyboard-coding-system (quote utf-8-auto-unix))
 '(linum-delay nil)
 '(linum-eager t)
 '(linum-format "%7d")
 '(ls-lisp-emulation (quote MacOS))
 '(make-backup-files nil)
 '(markdown-css-path "/screen.css")
 '(mode-line-format (quote ("%e" #(" " 0 1 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")) mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification #("   " 0 3 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")) mode-line-position (vc-mode vc-mode) #("  " 0 2 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")) mode-line-modes (which-func-mode ("" which-func-format #("  " 0 2 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")))) (global-mode-string ("" global-mode-string #("  " 0 2 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")))) #("-%+" 0 1 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")))))
 '(mouse-wheel-scroll-amount (quote (1)))
 '(scroll-bar-mode nil)
 '(scss-compile-at-save nil)
 '(sql-mysql-program "/usr/local/mysql/bin/mysql")
 '(tab-width 4)
 '(tabbar-background-color "grey20")
 '(tabbar-separator (quote (0.2)))
 '(truncate-lines t)
 '(yas/global-mode t nil (yasnippet))
 '(yas/prompt-functions (quote (yas/dropdown-prompt yas/x-prompt yas/completing-prompt yas/ido-prompt yas/no-prompt)))
 '(yas/triggers-in-field t)
 '(yas/wrap-around-region t)
)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Monaco"))))
 '(cua-rectangle ((default (:inherit region :background "#330000")) (((class color)) (:background "maroon" :foreground "white"))))
 '(cursor ((t (:background "#5599bb" :foreground "white"))))
 '(custom-button ((((type x w32 ns) (class color)) (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button) :family "Trebuchet MS"))))
 '(custom-button-mouse ((((type x w32 ns) (class color)) (:background "grey90" :foreground "black" :box (:line-width 2 :style released-button) :family "Trebuchet MS"))))
 '(custom-button-pressed ((((type x w32 ns) (class color)) (:background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button) :family "Trebuchet MS"))))
 '(fringe ((((class color) (background dark)) (:background "#111111" :foreground "white"))))
 '(hl-line ((t (:inherit highlight :background "#191929"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background dark)) (:background "#002244"))))
 '(linum ((t (:inherit font-lock-constant-face :weight normal :height 0.8))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.1))))
 '(mode-line ((t (:background "grey30" :foreground "gray90" :weight light :height 0.8 :family "verdana"))))
 '(mode-line-buffer-id ((t (:inherit mode-line :weight bold))))
 '(mode-line-emphasis ((t (:inherit mode-line :weight bold))))
 '(mode-line-highlight ((t nil)))
 '(region ((t (:background "#993300"))))
 '(secondary-selection ((((class color) (min-colors 88) (background dark)) (:background "#2200BB"))))
 '(speedbar-button-face ((((class color) (background dark)) (:inherit variable-pitch :foreground "green3" :height 0.8))))
 '(speedbar-directory-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "light blue"))))
 '(speedbar-file-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "cyan"))))
 '(speedbar-highlight-face ((((class color) (background dark)) (:inherit speedbar-button-face :background "sea green"))))
 '(speedbar-selected-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "red" :underline t))))
 '(speedbar-separator-face ((((class color) (background dark)) (:inherit speedbar-button-face :background "blue" :foreground "white" :overline "gray"))))
 '(speedbar-tag-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "yellow"))))
 '(tabbar-default ((t (:height 0.9 :family "Arial"))))
 '(tabbar-highlight ((t (:background "white" :foreground "black" :box (:line-width 4 :color "white")))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "grey60" :foreground "black" :box (:line-width 4 :color "grey60")))))
 '(tabbar-unselected ((t (:inherit tabbar-default :background "grey20" :box (:line-width 4 :color "grey20")))))
 '(tooltip ((default nil) (nil nil)))
 '(vhdl-speedbar-architecture-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "LightSkyBlue"))))
 '(vhdl-speedbar-architecture-selected-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "LightSkyBlue" :underline t))))
 '(vhdl-speedbar-configuration-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "Salmon"))))
 '(vhdl-speedbar-configuration-selected-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "Salmon" :underline t))))
 '(vhdl-speedbar-entity-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "PaleGreen"))))
 '(vhdl-speedbar-entity-selected-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "PaleGreen" :underline t))))
 '(vhdl-speedbar-instantiation-face ((((min-colors 88) (class color) (background dark)) (:inherit speedbar-button-face :foreground "Yellow1"))))
 '(vhdl-speedbar-instantiation-selected-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "Yellow" :underline t))))
 '(vhdl-speedbar-library-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "Orchid1"))))
 '(vhdl-speedbar-package-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "Grey80"))))
 '(vhdl-speedbar-package-selected-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "Grey80" :underline t))))
 '(vhdl-speedbar-subprogram-face ((((class color) (background dark)) (:inherit speedbar-button-face :foreground "BurlyWood2"))))
)

(color-theme-tq2323)
