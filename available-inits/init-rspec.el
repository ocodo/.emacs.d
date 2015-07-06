;; init-rspec -- Initialize rspec
;;; Commentary:
;;  Initialize rspec mode
;;
;;; Code:

(require 'rvm)
(require 'rspec-mode)

(rspec-install-snippets)
(setq rspec-use-rvm t)

;; Use C-x C-q to switch to interactive mode, for example with pry/debugger
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(provide 'init-rspec)
;;; init-rspec.el ends here
