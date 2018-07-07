;;; counsel-codesearch.el --- Counsel interface for codesearch.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Austin Bingham

;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.0.1
;; Package-Version: 20180527.728
;; Keywords: tools
;; URL: https://github.com/abingham/emacs-counsel-codesearch
;; Package-Requires: ((codesearch "1") (counsel "0.10.0") (emacs "24") (ivy "0.10.0"))

;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; Description:
;;
;; Adds support for using codesearch through counsel (ivy).
;;
;; For more details, see the project page at
;; https://github.com/abingham/emacs-counsel-codesearch
;;
;; For more details on codesearch, see its project page at
;; https://github.com/google/codesearch
;;
;; Installation:
;;
;; The simple way is to use package.el:
;;
;;   M-x package-install counsel-codesearch
;;
;; Example config:
;;
;;   (require 'counsel-codesearch)
;;

;;; Code:

(require 'codesearch)
(require 'counsel)
(require 'ivy)

(defgroup counsel-codesearch nil
  "Variables related to counsel-codesearch"
  :prefix "counsel-codesearch-"
  :group 'tools)

(defcustom counsel-codesearch-mininum-input-length 1
  "The minimum number of input characters before running a search."
  :type 'integer)

(defconst counsel-codesearch--match-regex "^\\(.*\\):\\([0-9]+\\):"
  "The regular expression used to find matches in the codesearch output.")

(defun counsel-codesearch--function (str)
  "Execute codesearch to find match for STR."
  (if (< (length str) counsel-codesearch-mininum-input-length)
      (counsel-more-chars counsel-codesearch-mininum-input-length)
    (let ((index-file (codesearch--csearchindex default-directory))
          (process-environment (copy-alist process-environment)))
      (setenv "CSEARCHINDEX" (expand-file-name index-file))
      (counsel--async-command
       (format "%s -n %s"
               codesearch-csearch
               str)))
    '("" "working...")))

(defun counsel-codesearch--handle-selection (selection)
  "Jump to the file/line indicated by SELECTION."
  (with-ivy-window
    (save-match-data
      (when (and selection
                 (string-match
                  counsel-codesearch--match-regex
                  selection))
        (let ((filename (match-string 1 selection))
              (line-number (string-to-number (match-string 2 selection))))
          (find-file filename)
          (widen)
          (goto-char (point-min))
          (forward-line (- line-number 1)))))))

;;;###autoload
(defun counsel-codesearch (&optional initial-input)
  "Call the \"csearch\" shell command.

INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read "Locate: " #'counsel-codesearch--function
            :initial-input initial-input
            :dynamic-collection t
            :history #'counsel-locate-history
            :action #'counsel-codesearch--handle-selection
            :unwind #'counsel-delete-process
            :caller 'counsel-codesearch))

(provide 'counsel-codesearch)

;;; counsel-codesearch.el ends here
