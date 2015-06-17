;; init-autocomplete
(require 'auto-complete)
(require 'auto-complete-config)
(require 'dropdown-list)

(ac-config-default)

(defun ac-coffee-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))

(defun ac-ruby-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet ac-source-robe) ac-sources)))

(defun ac-common-setup ()
  (append ac-sources '(ac-source-filename ac-source-files-in-current-dir)))

(provide 'init-autocomplete)
