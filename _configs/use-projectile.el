(projectile-global-mode t)

(defun projectile-open-last-known-project ()
    "Open the root folder of the last known project."
    (interactive)
    (find-file (car (projectile-load-known-projects))))

;; Projectile find file - flx based
(bind-keys
 ("M-P" . projectile-find-file)
 ("M-p" . projectile-find-file)
 ("C-x p" . projectile-find-file))

(provide 'use-projectile)

;;; use-projectile.el ends here
