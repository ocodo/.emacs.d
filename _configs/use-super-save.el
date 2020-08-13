;;; use-super-save --- initialize super-save
;;; Commentary:
;;; Code:
(require 'use-package)

(defcustom super-save-triggers '(switch-to-buffer other-window)
  "A list of commands which would trigger `super-save-command'."
  :group 'super-save
  :type '(repeat string))

(use-package super-save
  :init
  (super-save-mode t))

(provide 'use-super-save)
;;; use-super-save ends here
