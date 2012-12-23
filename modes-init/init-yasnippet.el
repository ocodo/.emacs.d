;; YASnippet
(require 'yasnippet)
;; Develop and keep personal snippets under ~/.emacs.d/mysnippets
(setq yas/root-directory '(
                           "~/.emacs.d/yasnippet/snippets/"
                           "~/.emacs.d/snippets/"
                           "~/Downloads/yas-snippets/"))
;; Load the snippets
(yas/reload-all)
(yas/global-mode 1)
;;
(provide 'init-yasnippet)
