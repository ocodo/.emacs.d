;;; init-ido --- initialize ido mode.
;;; Commentary:
;;; Code:

(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(ido-mode 1)
(ido-vertical-mode 1)

(ido-ubiquitous-mode 1)
(setq gc-cons-threshold 30000000)
(flx-ido-mode 1)

(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))

(provide 'init-ido)

;;; init-ido.el ends here
