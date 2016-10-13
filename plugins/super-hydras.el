;;; super-hydras.el --- A set of hydras bound to super

;; Author: Jason Milkins <jasonm23@gmail.com>

;; URL: https://github.com/ocodo/.emacs.d/plugins/super-f-one.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Misc commands on Super-f (cmd-f / win-f)
;;
;;; Code:

(require 'handy-functions)

(bind-key "s-r" (defhydra region-commands (:color blue)
                  "Region commands"
                  ("SPC" er/expand-region "Expand region")
                  ("t" case-transform/body "Case transform")))

(bind-key "s-f" (defhydra avy-commands (:color blue)
                  "Avy commands"
                  ("j" avy-goto-line "Avy goto line")
                  ("c" avy-copy-line "Avy copy line")
                  ("m" avy-move-line "Avy move line")
                  ("r" avy-copy-region "Avy copy region")
                  ("k" avy-kill-region "Avy kill region")
                  ("z" avy-zap-up-to-char-dwim "Avy zap up to DWIM")
                  ("Z" avy-zap-to-char-dwim "Avy zap to DWIM")
                  ("f" avy-goto-char "Avy goto char")
                  ("F" avy-flycheck-goto-error "Avy goto error")))

(bind-key "s-F" (defhydra file-commands (:color blue)
                  "File commands"
                  ("r" rename-this-buffer-and-file "Rename this buffer & file")
                  ("d" delete-this-buffer-and-file "Delete this buffer & file")))

(provide 'super-hydras)

;;; super-hydras.el ends here
