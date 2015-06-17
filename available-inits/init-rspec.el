;; Rspec init

(require 'rvm)
(require 'rspec-mode)

(rspec-install-snippets)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(setq rspec-use-rvm t)

(provide 'init-rspec)
