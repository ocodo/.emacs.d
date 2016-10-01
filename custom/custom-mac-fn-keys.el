;;; custom-mac-fn-keys --- initialize custom-mac-fn-keys
;;; Commentary:
;;; Code:
(require 'bind-key)

(when (and (window-system) (eq system-type 'darwin))
  (bind-keys
   ("<f13>" . overwrite-mode)
   ("<f14>" . ibuffer)
   ("<f15>" . magit-status)))

(provide 'custom-mac-fn-keys)

;;; custom-mac-fn-keys ends here
