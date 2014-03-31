;;; flymake-jshint.el --- making flymake work with JSHint

;; Copyright (C) 2012 Wilfred Hughes <me@wilfred.me.uk>

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 23 June 2011
;; Version: 20140319.1500
;; X-Original-Version: 2.2
;; Keywords: flymake, jshint, javascript
;; Package-Requires: ((flymake-easy "0.8"))

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

;; Please see README.md (online at
;; https://github.com/Wilfred/flymake-jshint.el#flymake-jshint ) for
;; installation and usage instructions.

;;; Code:

(require 'flymake-easy)

(defconst flymake-jshint-err-line-patterns
  '(("^\\(.*\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$"
     1 2 3 4)))

(defcustom jshint-configuration-path nil
  "Path to a JSON configuration file for JSHint."
  :type 'file
  :group 'flymake-jshint)

(defun flymake-jshint-command (filename)
  "Construct a command that flymake can use to check javscript source for FILENAME."
  (if jshint-configuration-path
      (list "jshint" filename "--config" (expand-file-name jshint-configuration-path))
    (list "jshint" filename)))

;;;###autoload
(defun flymake-jshint-load ()
  "Configure flymake mode to check the current buffer's JavaScript syntax."
  (interactive)
  (flymake-easy-load
   'flymake-jshint-command
   flymake-jshint-err-line-patterns
   'tempdir
   "js"))

(provide 'flymake-jshint)
;;; flymake-jshint.el ends here
