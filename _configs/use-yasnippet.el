;; use-yasnippet
(yas-global-mode t)

(setq yas-prompt-functions '(yas-ido-prompt
                             yas-dropdown-prompt
                             yas/x-prompt
                             yas/completing-prompt
                             yas/no-prompt))


(provide 'use-yasnippet)
