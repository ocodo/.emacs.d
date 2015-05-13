(when (file-exists-p "~/.rvm")
  (require 'rvm)
  (when (file-exists-p "~/.rvm")
    (rvm-use-default)))

(provide 'init-rvm)
