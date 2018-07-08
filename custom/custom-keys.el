;;; custom-keys --- Custom keys for ocodo emacs

;;; Commentary:

;;; Custom keys

;;; Code:

(require 'bind-key)
(require 'mac-port-keys)
(require 'custom-mac-fn-keys)
(require 'misc)

(bind-keys
 ("M-o w"        . other-window) ;; use as Esc, o, w (go Other Window)
 ("s-g"          . minibuffer-keyboard-quit)
 ("<home>"       . beginning-of-line)
 ("<end>"        . end-of-line)
 ("C-x /"        . align-regexp)
 ("C-^"          . join-line-from-below)
 ("C-x r u"      . cua-set-rectangle-mark)
 ("M-Z"          . zap-to-char)
 ("M-z"          . zap-up-to-char)
 ("M-i"          . evil-mode) ;; Toggle
 ("M-;"          . comment-dwim-2)
 ("C-c C-w"      . rotate-window)
 ("C-c C-l"      . rotate-layout)
 ("C-c SPC"      . ace-jump-mode)
 ("C-c ;"        . iedit-mode)
 ("RET"          . newline-and-indent)
 ("M-`"          . magit-status)
 ("C-x C-a"      . append-to-file)
 ("C-M-,"        . shrink-window-horizontally) ;; Ctrl-Alt-<
 ("C-M-."        . enlarge-window-horizontally) ;; Ctrl-Alt->
 ("C-<tab>"      . completion-at-point)
 ("C-x <ESC> e"  . eval-buffer)
 ("M-<ESC> e"    . eval-buffer)
 ("C-c q"        . auto-fill-mode))

;; unset some annoying things in the OS X build, no dialogs, also add
;; a few osx centric bindings.  If you like dialogs, you probably
;; don't want my bindings, if you're over that, and just need text,
;; and no GUI dialog modality stealing focus, you're in the right
;; place.  Of course, you WILL be better off building your own emacs
;; config from scratch, borrow ideas from here or enywhere
;; else.
;;
;; That's the true Emacs way, it's programmable, so program it.

(defun text-scale-reset ()
  "Reset text-scale to 0."
  (interactive)
  (text-scale-set 0))

(when (and (window-system) (or (eq system-type  'darwin) (eq system-type 'gnu/linux)))
  (message "binding Super Key shortcuts - slightly osx specific, work with GNU/Linux too")
  ;; Toggle fullscreen (>= emacs-version 24.4)
  (when (>= (string-to-number (format "%i.%i"  emacs-major-version emacs-minor-version)) 24.4)
    (global-unset-key (kbd "s-m"))
    (global-unset-key (kbd "s-h"))

    (bind-keys
     ("<S-s-return>" . toggle-fullscreen)
     ("s-o"          . find-file)
     ("s-8"          . rotate-window)
     ("s-7"          . rotate-layout)
     ("s-S"          . write-file)
     ("s-R"          . write-region)
     ("s-t"          . projectile-find-file)
     ("s-p"          . find-file-at-point)
     ("M-s-l"        . load-library)
     ("s-`"          . switch-window)
     ("s-~"          . other-frame)
     ("<s-right>"    . next-buffer)
     ("<s-left>"     . previous-buffer)
     ("s-b"          . switch-to-buffer)
     ("s-|"          . shell-command-on-region-replace)
     ("s-1"          . delete-other-windows)
     ("s-2"          . split-window-vertically)
     ("s-3"          . split-window-horizontally)
     ("s-4"          . delete-other-windows-vertically)
     ("s-0"          . delete-window)
     ("s--"          . text-scale-decrease)
     ("s-="          . text-scale-increase)
     ("s-+"          . text-scale-reset)
     ("s-\\"         . linum-mode)
     ("s-/"          . hippie-expand)
     ("<s-return>"   . completion-at-point))))

(provide 'custom-keys)

;;; custom-keys.el ends here
