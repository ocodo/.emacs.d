;;; evil-tabs.el --- Integrating Vim-style tabs for Evil mode users.
;; Copyright 2013 Kris Jenkins
;;
;; Author: Kris Jenkins <krisajenkins@gmail.com>
;; Maintainer: Kris Jenkins <krisajenkins@gmail.com>
;; Keywords: evil tab tabs vim
;; Package-Version: 20150127.1346
;; URL: https://github.com/krisajenkins/evil-tabs
;; Created: 30th September 2013
;; Version: 0.1.0
;; Package-Requires: ((evil "0.0.0") (elscreen "0.0.0"))

;;; Commentary:
;;
;; Integrating Vim-style tabs for Evil mode users.

;;; Code:

(require 'evil)
(require 'elscreen)

(defvar evil-tabs-mode-map (make-sparse-keymap)
  "Evil-tabs-mode's keymap.")

(evil-define-command evil-tabs-tabedit (file)
  (interactive "<f>")
  (elscreen-create)
  (find-file file))

(evil-define-command evil-tab-sensitive-quit (&optional bang)
  :repeat nil
  (interactive "<!>")
  (if (> (length (elscreen-get-screen-list)) 1)
    (elscreen-kill)
    (evil-quit bang)))

(evil-define-command evil-tabs-current-buffer-to-tab ()
  (let ((nwindows (length (window-list)))
        (cb (current-buffer)))
    (when (> nwindows 1)
      (delete-window)
      (elscreen-create)
      (switch-to-buffer cb))))

(evil-define-motion evil-tabs-goto-tab (&optional count)
  (if count
     (elscreen-goto (- count 1))
     (elscreen-next)))

(evil-ex-define-cmd "tabe[dit]" 'evil-tabs-tabedit)
(evil-ex-define-cmd "tabclone" 'elscreen-clone)
(evil-ex-define-cmd "tabc[lose]" 'elscreen-kill)
(evil-ex-define-cmd "tabd[isplay]" 'elscreen-toggle-display-tab)
(evil-ex-define-cmd "tabg[oto]" 'elscreen-goto)
(evil-ex-define-cmd "tabo[nly]" 'elscreen-kill-others)
(evil-ex-define-cmd "tabnew" 'elscreen-create)
(evil-ex-define-cmd "tabn[ext]" 'elscreen-next)
(evil-ex-define-cmd "tabp[rev]" 'elscreen-previous)
(evil-ex-define-cmd "tabr[ename]" 'elscreen-screen-nickname)
(evil-ex-define-cmd "tabs[elect]" 'elscreen-select-and-goto)
(evil-ex-define-cmd "tabw[ith]" 'elscreen-find-and-goto-by-buffer)
(evil-ex-define-cmd "q[uit]" 'evil-tab-sensitive-quit)

(evil-define-key 'normal evil-tabs-mode-map
  "gt" 'elscreen-next
  "gT" 'elscreen-previous
  "gt" 'evil-tabs-goto-tab
  "T" 'evil-tabs-current-buffer-to-tab)

;;;###autoload
(define-minor-mode evil-tabs-mode
  "Integrating Vim-style tabs for Evil mode users."
  :global t
  :keymap evil-tabs-mode-map
  (let ((prev-state evil-state))
    (evil-normal-state)
    (evil-change-state prev-state)

    (elscreen-start)))

;;;###autoload
(defun turn-on-evil-tabs-mode ()
  "Enable `evil-tabs-mode' in the current buffer."
  (evil-tabs-mode 1))

;;;###autoload
(defun turn-off-evil-tabs-mode ()
  "Disable `evil-tabs-mode' in the current buffer."
  (evil-tabs-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-tabs-mode evil-tabs-mode turn-on-evil-tabs-mode)

(provide 'evil-tabs)

;;; evil-tabs.el ends here
