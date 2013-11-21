;;; jss.el --- An emacs interface to webkit and mozilla debuggers

;; Copyright (C) 2013 Edward Marco Baringer

;; Author: Marco Baringer <mb@bese.it>
;; Version: 0.7
;; Keywords: languages
;; Package-Requires: ((emacs "24.1") (websocket "0") (js2-mode "0"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; An emacs implementation of the client side protocol of webkit and
;; firefox's over-the-wire debugging protocols.

;; jsSlime (jss for short) is designed for emacs users who program web
;; based javascript applications. It connects directly to the browser
;; and uses the tools (debugger, inspector, source code browsing,
;; evaluator, etc.) as they are in the browser.

;; jss is, currently at least, focused on the features required by
;; javascript developers, tools for live editing (or even just
;; inspecting) the DOM and CSS are not currently available.

;; See README.org for more details.

;;; Code:

;; This is just the top file for jsSlime. It simply loads the various
;; .el files in the right order

(require 'jss-browser-api)
(require 'jss-script)
(require 'jss-remote-value)
(require 'jss-browser-webkit)
(require 'jss-browser-firefox)
(require 'jss-browser)
(require 'jss-prompt)
(require 'jss-console)
(require 'jss-io-pretty-printers)
(require 'jss-io)
(require 'jss-debugger)

(require 'jss-super-mode)
(require 'jss-http-repl)

(provide 'jss)

;;; jss.el ends here
