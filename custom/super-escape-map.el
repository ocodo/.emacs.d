(defvar super-escape-map (make-keymap)
  "Custom Keymap for user bindings prefixed by s-ESC.")
(fset 'super-escape-map super-escape-map)
(define-key global-map [s-escape] 'super-escape-map)

(define-key super-escape-map "o" 'open-opsmanager)
(define-key super-escape-map "v" 'jasmine-coffee/verify-suite)
(define-key super-escape-map "r" 'reload-current-chrome-tab-osx)

(provide 'super-escape-map)
