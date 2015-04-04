(projectile-global-mode t)

(defun projectile-open-last-known-project ()
    "Open the root folder of the last known project."
    (interactive)
    (find-file (car (projectile-load-known-projects))))

;; Projectile find file - flx based
(global-set-key (kbd "M-P")                 'projectile-find-file)
(global-set-key (kbd "M-p")                 'projectile-find-file)
(global-set-key (kbd "C-x p")               'projectile-find-file)

(provide 'init-projectile)
