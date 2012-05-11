(progn (cd "~/.emacs.d") (normal-top-level-add-subdirs-to-load-path))
;; Load extensions here...

(load "cygwin-support") ;; Minor hacks for using the cygwin shell's instead of cmd.exe.
;; There is a better library than this, upgrade when time permits.

;; Libs.
(require 'color-theme)
(require 'dropins)

(require 'autopair)
(autopair-global-mode) ;; Todo: issues with scss mode... resolve.

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
(require 'package)
(require 'haml-mode)
(require 'scss-mode)

;; All specific non terminal mode stuff.
(when (window-system)
  (require 'darkroom-mode)

;; we use a version of hideshowvis which has
;; arrow bitmaps in the fringe, and switches
;; on the hidden number of lines view.
;; http://gist.github.com/514946
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

;; Things to run specifically when in a terminal
(when (not (window-system))
  (menu-bar-mode -1))

;; Start the emacs server.
(server-start nil)

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

;; More ...?
