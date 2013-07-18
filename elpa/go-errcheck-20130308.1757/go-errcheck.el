;;; go-errcheck.el --- errcheck integration for go-mode

;; Copyright (C) 2013 Dominik Honnef

;; Author: Dominik Honnef <dominikh@fork-bomb.org>
;; Version: 20130308.1757
;; X-Original-Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'compile)

(defgroup go-errcheck nil
  "errcheck integration for go-mode."
  :group 'go)

(defcustom go-errcheck-ignorepkg nil
  "List of package paths to ignore."
  :type '(repeat string)
  :group 'go-errcheck
  :safe 'listp)

(defcustom go-errcheck-ignore ""
  "Regular expression of function names to ignore.

Note that this uses RE2 regex syntax, not Emacs regex syntax."
  :type 'string
  :group 'go-errcheck
  :safe 'stringp)


(defun go-errcheck--compilation-hook (p)
  (set (make-local-variable 'compilation-error-regexp-alist) '(("^\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) .+$" 1 2 3 1 1))))

(defun go-errcheck--build-arguments (ignorepkg ignore)
  (list (unless (string= "" ignore)
          (concat "-ignore=\"" (shell-quote-argument ignore)  "\""))
        (if ignorepkg
            (concat "-ignorepkg=\"" (mapconcat 'identity ignorepkg ",") "\""))))

;;;###autoload
(defun go-errcheck (directory ignorepkg ignore)
  "Run errcheck on the current buffer's directory and display the
  output in a compilation buffer.

If ARG is non-nil, go-errcheck will query for the values of
IGNOREPKG and IGNORE which will override any defaults or file
local variables.

When called non-interactively, DIRECTORY, IGNOREPKG and IGNORE
can be specified as arguments."
  (interactive
   (list
    (if buffer-file-name
        (file-name-directory buffer-file-name)
      default-directory)
    (if current-prefix-arg
        (split-string
         (read-from-minibuffer "ignorepkg (Space-separated list of packages to ignore): ")
         " "))
    (if current-prefix-arg
        (read-from-minibuffer "ignore (RE2 regexp to ignore functions): "))))
  (add-hook 'compilation-start-hook 'go-errcheck--compilation-hook)
  (compile (concat
            "errcheck "
            (mapconcat 'identity (go-errcheck--build-arguments
                                  (or ignorepkg go-errcheck-ignorepkg)
                                  (or ignore go-errcheck-ignore))
                       " ")
            " "
            (shell-quote-argument (file-truename directory))))
  (remove-hook 'compilation-start-hook 'go-errcheck--compilation-hook))


(provide 'go-errcheck)

;;; go-errcheck.el ends here
