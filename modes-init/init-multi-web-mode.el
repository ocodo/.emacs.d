;; Configure multi-web-mode
(require 'multi-web-mode)

(setq mweb-default-major-mode 'nxml-mode)
(setq mweb-tags '((php-mode         "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode          "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (js-mode          "<script +\\(type=\"application/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (mustache-mode    "<script +\\(type=\"text/html\"\\)[^>]*>" "</script>")
                  (handlebars-mode  "<script +\\(type=\"text/text/x-handlebars-template\"\\)[^>]*>" "</script>")
                  (nxml-mode        "<script +\\(type=\"text/text/x-kendo-template\"\\)[^>]*>" "</script>")
                  (css-mode         "<style +type=\"text/css\"[^>]*>" "</style>")))

(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

(provide 'init-multi-web-mode)
