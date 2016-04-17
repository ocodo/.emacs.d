;; Additional key bindings for projectile rails

(eval-after-load "projectile-rails"
  '(progn
     (define-key projectile-rails-mode-map (kbd "M-s-m") 'projectile-rails-find-model)
     (define-key projectile-rails-mode-map (kbd "M-s-c") 'projectile-rails-find-controller)
     (define-key projectile-rails-mode-map (kbd "M-s-v") 'projectile-rails-find-view)
     (define-key projectile-rails-mode-map (kbd "M-s-s") 'projectile-rails-find-spec)
     (define-key projectile-rails-mode-map (kbd "M-s-n") 'projectile-rails-find-migration)
     (define-key projectile-rails-mode-map (kbd "M-s-j") 'projectile-rails-find-javascript)
     (define-key projectile-rails-mode-map (kbd "M-s-o") 'projectile-rails-find-current-spec)
     (define-key projectile-rails-mode-map (kbd "M-s-p") 'projectile-rails-goto-file-at-point)))

(provide 'use-projectile-rails)
