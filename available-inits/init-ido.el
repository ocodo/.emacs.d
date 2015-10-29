;;; init-ido --- initialize ido mode.
;;; Commentary:
;;; Code:
(require 'ido-grid-mode)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode 1)
(ido-grid-mode 1)
(setq ido-grid-mode-min-rows 10)
(setq ido-grid-mode-first-line ())
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
