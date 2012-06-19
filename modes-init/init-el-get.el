;; el-get installer
(add-to-list 'load-path "~/.emacs.d/el-get/el-get") 
(unless 
    (require 'el-get nil t) (with-current-buffer (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el") 
                              (goto-char (point-max)) 
                              (eval-print-last-sexp))) 
(el-get 'sync) ;; init el-get packages

(provide 'init-el-get)
