(when (file-exists-p "~/.rvm")
  (require 'rvm)
  (async-start
   (lambda ()
     (when (file-exists-p "~/.rvm")
       (rvm-use-default)))))

(provide 'init-rvm)
