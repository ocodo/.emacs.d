;; init-autocomplete
(require 'auto-complete)
(require 'auto-complete-config)
(require 'dropdown-list)

(ac-config-default)

(setf ac-delay 0.25)

(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-filename))

(provide 'init-autocomplete)
