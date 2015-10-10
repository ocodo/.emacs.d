;; init-autocomplete
(require 'auto-complete)
(require 'auto-complete-config)
(require 'dropdown-list)
(require 'ac-dabbrev)

(global-set-key (kbd "M-/") 'ac-start)

(ac-config-default)

(add-to-list 'ac-sources 'ac-source-dabbrev)
(add-to-list 'ac-sources 'ac-source-filename)
(add-to-list 'ac-sources 'ac-source-files-in-current-dir)

(defun setup-ac-for-haml ()
  (require 'ac-haml)
  (require 'ac-html-default-data-provider)
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  (ac-haml-setup)
  (setq ac-sources
        (append
         '(ac-source-haml-tag
           ac-source-haml-attr
           ac-source-haml-attrv)
         ac-sources)))

(add-hook 'haml-mode-hook 'setup-ac-for-haml)

(defun ac-coffee-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))

(defun ac-ruby-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet ac-source-robe) ac-sources)))

(provide 'init-autocomplete)
