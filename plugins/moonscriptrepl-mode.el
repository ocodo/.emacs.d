;;; moonscriptrepl-mode.el --- a major-mode for editing Moonscript in a repl
;;
;;; Author: @GriffinSchneider, @k2052, @EmacsFodder
;;; Version: 20140803-0.1.0
;;; Commentary:
;;
;;  A basic major mode for MoonScript REPL
;;
;;; License: MIT Licence
;;
;;; Code:

(require 'moonscript-mode)

(define-derived-mode moonscriptrepl-mode comint-mode "moonscriptrepl"
  (setq font-lock-defaults '(moonscript-font-lock-defaults))

  (modify-syntax-entry ?\- ". 12b" moonscript-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" moonscript-mode-syntax-table)
  (modify-syntax-entry ?\_ "w" moonscript-mode-syntax-table))

(provide 'moonscriptrepl-mode)
;;; moonscriptrepl-mode.el ends here
