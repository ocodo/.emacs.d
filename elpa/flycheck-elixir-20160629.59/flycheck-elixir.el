;;; flycheck-elixir.el --- Support Elixir in flycheck

;; Copyright (C) 2016 Lorenzo Bolla <lbolla@gmail.com>
;;
;; Author: Lorenzo Bolla <lbolla@gmail.com>
;; Created: 26 March 2016
;; Version: 1.0
;; Package-Version: 20160629.59
;; Package-Requires: ((flycheck "0.25"))

;;; Commentary:

;; This package adds support for elixir to flycheck.  It requires
;; elixir>=1.2.3.
;; Warning: running the checker will effectively execute the buffer,
;; therefore it may be unsafe to run.  See
;; https://github.com/flycheck/flycheck/issues/630

;; To use it, add to your init.el:

;; (require 'flycheck-elixir)
;; (add-hook 'elixir-mode-hook 'flycheck-mode)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'flycheck)


(flycheck-define-checker elixir
  "Elixir checker."
  :command ("elixirc"
            "--ignore-module-conflict"  ; Avoid module conflict warnings
            source-inplace)  ; Check as soon as possible, not just on file-save
  :error-patterns
  ((warning line-start
            (file-name)
            ":"
            line
            ": warning: "
            (message)
            line-end)
   (error line-start
          "** ("
          (one-or-more word)
          "Error) "
          (file-name)
          ":"
          line
          ": "
          (message)
          line-end))
  :modes elixir-mode)

(add-to-list 'flycheck-checkers 'elixir t)

(provide 'flycheck-elixir)
;;; flycheck-elixir.el ends here
