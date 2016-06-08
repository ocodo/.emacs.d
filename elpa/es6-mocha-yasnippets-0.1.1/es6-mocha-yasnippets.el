;;; es6-mocha-yasnippets.el --- Yasnippets for the Mocha JS Testing Framework

;; Copyright (C) 2016 Jason Milkins

;; Author: Jason Milkins <jasonm23@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((yasnippet "0.8.0"))
;; Maintainer: Jason Milkins <jasonm23@gmail.com>
;; Keywords: test javascript mocha chai BDD TDD

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides yas snippets for mocha testing in
;; js2-mode with es6 syntax
;;
;; SNIPPETS
;;
;; desc -> describe block
;; befr -> before/beforeEach block
;; aftr -> after/afterEach block
;; it -> it block
;; ex -> expect
;; m -> select a matcher/chain (from chai)
;;
;; test -> xUnit test
;; suite -> xUnit suite
;;
;; TODO: Sinon spy/stub snippets and more.
;;
;;; Code:

(setq es6-mocha-yasnippets-root
      (file-name-directory
       (or load-file-name
           (buffer-file-name))))

;;;###autoload
(defun es6-mocha-yasnippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" es6-mocha-yasnippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas-load-directory snip-dir)))

;;;###autoload
(eval-after-load "yasnippet"
  '(es6-mocha-yasnippets-initialize))

(provide 'es6-mocha-yasnippets)
;;; es6-mocha-yasnippets.el ends here
