;; Markdown mode - TAB for <pre></pre> block
(add-hook 'markdown-mode-hook
          (lambda ()
            (define-key markdown-mode-map (kbd "<tab>") 'markdown-insert-pre)
            (define-key markdown-mode-map (kbd "C-<up>") nil)
            (define-key markdown-mode-map (kbd "C-<down>") nil)
            (define-key markdown-mode-map (kbd "M-<up>") nil)
            (define-key markdown-mode-map (kbd "M-<down>") nil)
            (define-key markdown-mode-map (kbd "M-<left>") nil)
            (define-key markdown-mode-map (kbd "M-<right>") nil)
            (define-key markdown-mode-map (kbd "M-S-<left>") nil)
            (define-key markdown-mode-map (kbd "M-S-<right>") nil)
            (define-key markdown-mode-map (kbd "C-c b") 'markdown-insert-bold)
            (define-key markdown-mode-map (kbd "C-c l") 'markdown-insert-link)
            (define-key markdown-mode-map (kbd "C-c c") 'markdown-insert-code)
            (sp-local-pair 'markdown-mode "<kbd>" "</kbd>" :wrap "C-c k")
            ))

;; Markdown file handling
(dolist (pattern '("\\.md$" "\\.markdown$"))
  (add-to-list 'auto-mode-alist (cons pattern 'markdown-mode)))

(provide 'init-markdown)
