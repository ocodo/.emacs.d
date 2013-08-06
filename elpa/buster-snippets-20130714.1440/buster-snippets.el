;;; buster-snippets.el --- Yasnippets for the Buster javascript testing framework

;; Copyright (C) 2011 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: snippets

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

;; Yasnippets for the Buster javascript testing framework
;;
;; Common snippets
;;
;;     tc => new testCase (one for node, browser and node+browser)
;;     tt => additional test
;;     cx => nested context
;;     su => setup method
;;     td => teardown method
;;
;; Assert and refute snippets follow a common pattern. They start with `as` or `re`
;; followed by a mnemonic shortcut. Most are simply the 'initials' of the method name, but
;; the best shortcuts are saved for the most common assertions.
;;
;;     ase - assert.equals
;;     asm - assert.match
;;     ass - assert.same
;;     asx - assert.exception
;;     asd - assert.defined
;;     ast - assert.threw
;;     asat - assert.alwaysThrew
;;     asin - assert.isNull
;;     asio - assert.isObject
;;     asto - assert.typeOf
;;     ascn - assert.className
;;     astn - assert.tagName
;;
;; Buster also includes Sinon and its assertions:
;;
;;     asc - assert.called
;;     asc1 - assert.calledOnce
;;     asc1w - assert.calledOnceWith
;;     asc2 - assert.calledTwice
;;     asc3 - assert.calledThrice
;;     ascw - assert.calledWith
;;     ascc - assert.callCount
;;     asco - assert.callOrder
;;     asco - assert.calledOn
;;     asaco - assert.alwaysCalledOn
;;     asacw - assert.alwaysCalledWith
;;     asacwe - assert.alwaysCalledWithExactly
;;     ascwe - assert.calledWithExactly
;;
;; Refutations mirrors assertions exactly, except that they start with `re` instead of
;; `as`. It is the beautiful symmetry of the buster assertions package.

;; Installation

;; If you haven't, install [yasnippet](http://capitaomorte.github.com/yasnippet/)
;; then install buster-snippets like so:
;;
;;     git submodule add https://github.com/magnars/buster-snippets.el.git site-lisp/buster-snippets
;;
;; Then require buster-snippets at some point after yasnippet.
;;
;;     (require 'buster-snippets)

;; Customization

;; Add `"use strict"`-declarations to the test cases:
;;
;;     (setq buster-use-strict t)
;;
;; Declare `assert` and `refute` if you've disabled additional globals:
;;
;;     (setq buster-exposed-asserts nil)
;;
;; Set the default global namespace-object on a per-project basis:
;;
;;     (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (when (string-match-p "projects/zombietdd" (buffer-file-name))
;;               (setq js2-additional-externs '("ZOMBIE"))
;;               (setq buster-default-global "ZOMBIE"))))
;;
;;     ;; example from one of my projects
;;
;; Add the default global to the IIFE (immediately invoked function expression)
;;
;;     (setq buster-add-default-global-to-iife t)
;;
;; The global will by default be shortened to a one-letter var, like this:
;;
;;     (function (Z) {
;;
;;        // use Z instead of ZOMBIE inside the namespace
;;
;;     }(ZOMBIE));

;;; Code:

(require 'yasnippet)

(defcustom buster-use-strict nil
  "On non-nil value, add strict-declarations to test cases.")

(defcustom buster-exposed-asserts t
  "On nil value, declare assert and refute in local scope.")

(defcustom buster-add-default-global-to-iife nil
  "On non-nil value, add the default global to the wrapping iife
   (immediately invoked function expression)")

(defcustom buster-test-prefix "should "
  "The default buster test prefix.")

(defcustom buster-default-global "GLOBAL"
  "The default suggested global namespace-object.")
(make-variable-buffer-local 'buster-default-global)

(defcustom buster-testcase-snippets-enabled t
  "A buffer local way of turning off the testCase snippets.
To let you define your own testCase snippets for other
frameworks while still using the buster-assertions package.")
(make-variable-buffer-local 'buster-testcase-snippets-enabled)

(require 'buster-snippet-helpers)

(setq buster-snippets-root (file-name-directory (or load-file-name
                                                     (buffer-file-name))))

;;;###autoload
(defun buster-snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" buster-snippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas/load-directory snip-dir)))

;;;###autoload
(eval-after-load "yasnippet"
  '(buster-snippets-initialize))

(provide 'buster-snippets)
