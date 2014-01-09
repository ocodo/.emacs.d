;;; flymake-jshint.el --- making flymake work with JSHint

;; Copyright (C) 2012 Wilfred Hughes <me@wilfred.me.uk>

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Created: 23 June 2011
;; Version: 20130610.1057
;; X-Original-Version: 2.0
;; Keywords: flymake, jshint, javascript
;; Package-Requires: ((flymake-easy "0.1"))

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

;; To use JSHint with Emacs, you will need JSHint installed and available on
;; your path.  You should be able to do

;; $ jshint

;; without problem.  To do this, you can install node.js, npm and
;; jshint by doing the following:

;; $ apt-get install nodejs # or your distro / OS equivalent
;; $ sudo npm install -g jshint

;; You will probably want to configure the warnings that JSHint
;; produces.  The full list is at http://www.jshint.com/options/ but
;; for reference I use:

;; { "browser": true, //browser constants, such as alert
;;   "curly": true, // require {} on one-line if
;;   "undef": true, // non-globals must be declared before use
;;   "newcap": true, // constructors must start with capital letter
;;   "jquery": true, // jQuery constants
;;   "nomen": false, // permit leading/trailing underscores, these do actually mean private in jQuery plugins
;;   "nonew": true, // don't permit object creation for side-effects only
;;   "strict": true // require "use strict";
;; }

;; Save this in a file called whatever.json and then set
;; jshint-configuration-path to point to it.

;;; Usage

;; Add to your Emacs config:

;; (require 'flymake-jshint)
;; (add-hook 'js-mode-hook 'flymake-jshint-load)

;; making sure that flymake-jshint.el is on your load-path.  If not,
;; also add to your config:

;; (add-to-list 'load-path "~/.emacs.d/path/to/flymake-jshint.el")

;;; Debugging

;; If JSHint isn't working for any reason, execute

;; M-x set-variable flymake-log-level <RET> 3

;; and you will see what is going wrong listed in the *Messages*
;; buffer.

;;; Alternatives

;; * https://github.com/lunaryorn/flycheck supports JSHint
;; * https://github.com/illusori/emacs-flymake is a fork of flymake
;;   that also supports JSHint (but does not support JSHint
;;   configuration)
;; * https://github.com/purcell/flymake-jslint will probably also
;;   work with JSHint

;;; Changelog

;; v2.0 -- Updated usage instructions following the port to flymake-easy
;; v1.3 -- Refactored to use flymake-easy

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
