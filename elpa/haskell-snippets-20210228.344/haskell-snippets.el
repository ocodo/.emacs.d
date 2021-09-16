;;; haskell-snippets.el --- Yasnippets for Haskell

;; Copyright (C) 2013-2015 Luke Hoersten

;; Author: Luke Hoersten <luke@hoersten.org>
;; URL: https://github.com/haskell/haskell-snippets
;; Keywords: snippets, haskell
;; Version: 0.1.0
;; Package-Requires: ((cl-lib "0.5") (yasnippet "0.8.0"))

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;;   Haskell-Snippets is a collection of YASnippet Haskell snippets for Emacs.
;;
;;   Available Expansion Keys:
;;
;;       new  - newtype
;;       mod  - module [simple, exports]
;;       main - main module and function
;;       let  - let bindings
;;       lang - language extension pragmas
;;       opt  - GHC options pragmas
;;       \    - lambda function
;;       inst - instance declairation
;;       imp  - import modules [simple, qualified]
;;       if   - if conditional [inline, block]
;;       <-   - monadic get
;;       fn   - top level function [simple, guarded, clauses]
;;       data - data type definition [inline, record]
;;       =>   - type constraint
;;       {-   - block comment
;;       case - case statement
;;
;;   Design Ideals:
;;
;;       Keep snippet keys (the prefix used to auto-complete) to four
;;       characters or less while still being as easy to guess as
;;       possible.
;;
;;       Have as few keys as possible. The more keys there are to
;;       remember, the harder snippets are to use and learn.
;;
;;       Leverage ido-mode when reasonable. For instance, to keep the
;;       number of snippet keys to a minimum as well as auto complete
;;       things like Haskell Langauge Extension Pragmas. When multiple
;;       snippets share a key (ex: 'fn'), the ido-mode prompts are unique to
;;       one character (ex: 'guarded function' and 'simple function' are 'g' and
;;       's' respectively).

;;; Code:

(require 'cl-lib)
(require 'yasnippet)

(setq haskell-snippets-dir
      (file-name-directory load-file-name))

;;;###autoload
(defun haskell-snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" haskell-snippets-dir)))
    (add-to-list 'yas-snippet-dirs snip-dir t)
    (yas-load-directory snip-dir)))

;;;###autoload
(eval-after-load 'yasnippet
  '(haskell-snippets-initialize))

(provide 'haskell-snippets)

;;; haskell-snippets.el ends here
