;; init-git-gutter

(if (window-system)
    (progn (require 'git-gutter-fringe+))
  (progn (require 'git-gutter+)))

(add-hook 'prog-mode-hook 'git-gutter+-turn-on)
(add-hook 'css-mode-hook 'git-gutter+-turn-on)

(provide 'init-git-gutter)
