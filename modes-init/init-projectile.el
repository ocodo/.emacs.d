(projectile-global-mode t)

(defun projectile-open-last-known-project ()
    "Open the root folder of the last known project."
    (interactive)
    (find-file (car (projectile-load-known-projects))))

(provide 'init-projectile)
