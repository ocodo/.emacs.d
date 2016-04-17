;;; use-text --- initialize text-mode
;;; Commentary:
;;;
;;; Note text-mode is a bit weird, in that you cannot require it (it's already loaded) so use-package isn't needed.
;;;
;;; Code:
(require 'bind-key)

(bind-key "C-c m" 'markdown-mode text-mode-map)

(provide 'use-text)
;;; use-text ends here
