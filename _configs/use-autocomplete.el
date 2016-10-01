;;; use-autocomplete --- initialize auto complete mode
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package auto-complete-config)
(use-package ac-dabbrev)
(use-package dropdown-list)

(use-package auto-complete
  :init
  (progn
    (defun ac-auto-complete-mode-start ()
      "Start up ac-mode."
      (interactive)
      (auto-complete-mode 1)
      (ac-start))

    (bind-key "M-RET" 'ac-auto-complete-mode-start)

    (ac-config-default)

    (defun ac-setup-global-default-source ()
      "Setup default global AutoComplete sources"
      (dolist (source '(ac-source-dabbrev
                        ac-source-filename
                        ac-source-files-in-current-dir))
        (add-to-list 'ac-sources source)))

    (add-hook 'auto-complete-mode-hook 'ac-setup-global-default-source)

    (defun setup-ac-for-haml ()
      (use-package ac-haml)
      (use-package ac-html-default-data-provider)
      (ac-html-enable-data-provider 'ac-html-default-data-provider)
      (ac-haml-setup)
      (auto-complete-mode 1)
      (dolist (source '(ac-source-haml-tag
                        ac-source-haml-attr
                        ac-source-haml-attrv))
        (add-to-list 'ac-sources source)))

    (add-hook 'haml-mode-hook 'setup-ac-for-haml)

    (defun ac-coffee-mode-setup ()
      (dolist (source '(ac-source-yasnippet))
        (add-to-list 'ac-sources source)))

    (add-hook 'coffee-mode-hook 'ac-coffee-mode-setup)

    (defun ac-ruby-mode-setup ()
      (dolist (source '(ac-source-yasnippet ac-source-robe))
        (add-to-list 'ac-sources source)))))

(provide 'use-autocomplete)
;;; use-autocomplete ends here
