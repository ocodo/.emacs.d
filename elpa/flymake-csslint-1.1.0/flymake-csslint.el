;;; flymake-csslint.el --- making flymake work with CSSLint

;; Copyright (C) 2011, 2012 Arne Jørgensen <arne@arnested.dk>
;; Copyright (C) 2011 Wilfred Hughes <me@wilfred.me.uk>

;; Author: Arne Jørgensen <arne@arnested.dk>
;; URL: https://github.com/arnested/flymake-csslint
;; Created: 1 December 2011
;; Version: 1.1.0
;; Package-Requires: ((flymake "0.3"))
;; Keywords: flymake, csslint, css

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

;;; Commentary:

;; To use CSSLint with emacs, you will need CSSLint installed and available on
;; your path. You should be able to do

;; $ csslint

;; without problem. To do this, you can install node.js, npm and
;; csslint by doing the following:

;; $ apt-get install nodejs # or your distro / OS equivalent
;; $ curl http://npmjs.org/install.sh | sh
;; $ npm install -g csslint

;; flymake-csslint.el is very much based on flymake-jshint.el by
;; Wilfred Hughes <me@wilfred.me.uk>.

;;; Usage:

;; Add to your emacs config:

;; (require 'flymake-csslint)
;; (add-hook 'css-mode-hook 'flymake-mode)

;; making sure that flymake-csslint.el is on your load-path. If not,
;; also add to your config:

;; (add-to-list 'load-path "~/.emacs.d/path/to/flymake-csslint.el")

;;; Debugging:

;; If CSSLint isn't working for any reason, execute

;; M-x set-variable flymake-log-level <RET> 3

;; and you will see what is going wrong listed in the *Messages*
;; buffer.

;;; Code:

(require 'flymake)

(defgroup flymake-csslint nil
  "Flymake CCSlint configuration."
  :group 'flymake)

(defcustom flymake-csslint-program (executable-find "csslint")
  "Name of the CSSLint program."
  :type '(choice (file :tag "Location of csslint")
                 (const :tag "csslint is not installed"))
  :group 'flymake-csslint)

;;;###autoload
(defun flymake-csslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     (if (fboundp 'flymake-create-temp-copy)
                         'flymake-create-temp-copy
                       'flymake-create-temp-inplace)))
         (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list flymake-csslint-program (list "--format=compact" local-file))))

;;;###autoload
(eval-after-load 'flymake
  '(progn
     (add-to-list 'flymake-allowed-file-name-masks
                  '(".+\\.css$"
                    flymake-csslint-init
                    flymake-simple-cleanup
                    flymake-get-real-file-name))
     (add-to-list 'flymake-err-line-patterns
                  '("^\\(.*\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$"
                    1 2 3 4))
     ;; load flymake automatically in CSS mode
     (add-hook 'css-mode-hook (lambda() (flymake-mode 1)) t)))



(provide 'flymake-csslint)

;; Local Variables:
;; coding: utf-8
;; End:

;;; flymake-csslint.el ends here
