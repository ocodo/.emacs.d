;; TODO : Replace with a defhydra...?

(defvar super-num-zero-map (make-keymap)
  "Custom Keymap for user bindings prefixed by s-kp-0.")
(fset 'super-num-zero-map super-num-zero-map)
(define-key global-map [s-kp-0] 'super-num-zero-map)
(define-key super-num-zero-map "o" 'open-opsmanager)
(define-key super-num-zero-map "v" 'jasmine-coffee/verify-suite)
(define-key super-num-zero-map "r" 'reload-current-chrome-tab-osx)

(provide 'super-num-zero-map)
