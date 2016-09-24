;;; super-f-one.el --- A hydra bound to super-f1 which binds to misc useful things, add more commands as req'd

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
;;  Misc commands on a prominent GUI accessible key chord Super-f1 (cmd / win)
;;

;;; Code:

(require 'handy-functions)

(bind-key "s-<f1>" (defhydra super-f-one (:color blue)
                     "Misc commands"
                     ("s-<f1>" avy-goto-char "Avy goto char")
                     ("s-m" rename-this-buffer-and-file "Rename this buffer & file")
                     ("s-d" delete-this-buffer-and-file "Delete this buffer & file")
                     ("s-r" reload-current-chrome-tab-osx "Reload current chrome tab OS X (uses inline AppleScript)")
                     ("s-v" jasmine-coffee/verify-suite "Jasmine (Coffee) / Verify Suite")))

(provide 'super-f-one)

;;; super-f-one.el ends here
