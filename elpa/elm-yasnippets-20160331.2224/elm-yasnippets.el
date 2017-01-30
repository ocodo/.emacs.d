;;; elm-yasnippets.el --- Yasnippets for Elm

;; Copyright (C) 2016 Austin Bingham

;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Keywords: snippets
;; Version: 0.0.1
;; Package-Requires: ((yasnippet "0.8.0"))

;;; Code:

(setq elm-snippets-dir (file-name-directory load-file-name))

;;;###autoload
(defun elm-snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" elm-snippets-dir)))
    (add-to-list 'yas-snippet-dirs snip-dir t)
    (yas/load-directory snip-dir)))

;;;###autoload
(eval-after-load 'yasnippet
   '(elm-snippets-initialize))

(provide 'elm-yasnippets)

;;; elm-yasnippets.el ends here
