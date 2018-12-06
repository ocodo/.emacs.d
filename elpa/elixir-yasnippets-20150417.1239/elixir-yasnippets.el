;;; elixir-yasnippets.el --- Yasnippets for Elixir

;; Copyright (C) 2013 Yinghai ZHAO

;; Author: Yinghai Zhao <zyinghai@gmail.com>
;; Keywords: snippets
;; Version: 0.0.1
;; Package-Requires: ((yasnippet "0.8.0"))

;;; Code:

(setq elixir-snippets-dir (file-name-directory (or (buffer-file-name)
                                                    load-file-name)))

;;;###autoload
(defun elixir-snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" elixir-snippets-dir)))
    (add-to-list 'yas-snippet-dirs snip-dir t)
    (yas/load-directory snip-dir)))

;;;###autoload
(eval-after-load 'yasnippet
   '(elixir-snippets-initialize))

(require 'yasnippet)

(provide 'elixir-yasnippets)

;;; elixir-yasnippets.el ends here
