;; init.el 
(progn (cd "~/.emacs.d") (normal-top-level-add-subdirs-to-load-path))

(require 'cl)
(require 'dropins)
(load-dropins)
;; el-get installer

; (add-to-list 'load-path "~/.emacs.d/el-get/el-get") (unless (require 'el-get nil t) (with-current-buffer (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el") (goto-char (point-max)) (eval-print-last-sexp))) (el-get 'sync)

(progn 
  (require 'resize-window)
  (require 'dired-details+)
  (require 'ls-lisp)
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
  (require 'highlight-indentation)
  (require 'scss-mode))

(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; turn off menubar - uncomment
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; turn off toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(require 'yasnippet-bundle)
(yas/load-directory "~/.emacs.d/snippets" )

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Load a nice theme...
;;(defun color-theme-tq2323 () (interactive) (color-theme-install '(color-theme-tq2323 ((background-color . "#000407") (foreground-color . "#ddfffc") (background-mode . dark) (border-color . "#052e2d") (cursor-color . "#0d6d6c") (mouse-color . "#323232")) (fringe ((t (:background "#052e2d")))) (mode-line ((t (:foreground "#9cf6f4" :background "#0b2c2d")))) (region ((t (:background "#0c5f59")))) (font-lock-builtin-face ((t (:foreground "#0b8e87")))) (font-lock-comment-face ((t (:foreground "#265f59")))) (font-lock-function-name-face ((t (:foreground "#60c3be")))) (font-lock-keyword-face ((t (:foreground "#0abda7")))) (font-lock-string-face ((t (:foreground "#aaedee")))) (font-lock-type-face ((t (:foreground"#1f8e8a")))) (font-lock-constant-face ((t (:foreground "#1ae9d7")))) (font-lock-variable-name-face ((t (:foreground "#0ebeb8")))) (minibuffer-prompt ((t (:foreground "#00faf2" :bold t)))) (font-lock-warning-face ((t (:foreground "red" :bold t)))))))
;;(color-theme-tq2323)
;; this is now external... 

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

;; Keys.
(progn 
  (global-set-key [(control n)] 'find-file)

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
  (global-set-key [(control tab)] 'completion-at-point)
  (global-set-key (kbd "<s-return>" ) 'completion-at-point)
  (global-set-key '[M-S-return] 'darkroom-mode)
  (global-set-key '[M-s-return] 'darkroom-mode)
  (global-set-key (kbd "s-0") 'linum-mode)
  )

;; Dired mode specific
(eval-after-load 'dired
  '(define-key dired-mode-map [mouse-3] 'dired-maybe-insert-subdir)
  )

(setq custom-file "~/.emacs.d/custom.el")

;; (color-theme-tq2323)

(color-theme-deepblueday)

(big-left-3)

;; Start the emacs server.
(server-start nil)
