;;; listing-codesearch.el --- Simple, list-based UI for codesearch
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 1
;; URL: https://github.com/abingham/emacs-codesearch
;; Keywords: tools, development, search
;; Package-Requires: ((dash "2.8.0))
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2016-2017 Austin Bingham
;;
;;; Commentary:
;;
;; Description:
;;
;; Provides a simple, list-oriented UI for results from codesearch.
;;
;; For more details, see the project page at
;; https://github.com/abingham/emacs-codesearch
;;
;; For more details on codesearch, see its project page at
;; https://github.com/google/codesearch
;;
;; Installation:
;;
;; The simple way is to use package.el:
;;
;;   M-x package-install listing-codesearch
;;
;; Or, copy codesearch.el to some location in your emacs load
;; path. Then add "(require 'listing-codesearch)" to your emacs initialization
;; (.emacs, init.el, or something).
;;
;; Example config:
;;
;;   (require 'listing-codesearch)
;;
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

;;; Code:

(require 'codesearch)
(require 'dash)

(defgroup listing-codesearch nil
  "Variables related to listing-codesearch."
  :prefix "listing-codesearch-"
  :group 'tools)

(defface listing-codesearch-filename
  '((t :inherit font-lock-constant-face))
  "Face used to highlight filenames in matches."
  :group 'listing-codesearch)

(defface listing-codesearch-line-number
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight line numbers in matches."
  :group 'listing-codesearch)

(defconst listing-codesearch--match-regex "^\\(.*\\):\\([0-9]+\\):"
  "The regular expression used to find matches in the codesearch output.")

(define-button-type 'listing-codesearch--filename-match-button
  'face 'listing-codesearch-filename
  'follow-link 't
  'button 't)

(define-button-type 'listing-codesearch--line-number-match-button
  'face 'listing-codesearch-line-number
  'follow-link 't
  'button 't)

(defun listing-codesearch--make-filenames-clickable (buff)
  "Finds all codesearch matches in `buff', turning them into
clickable buttons that link to the matched file/line-number.

`buff' is assumed to contain the output from running csearch.
"
  (with-current-buffer buff
    (beginning-of-buffer)
    (while (re-search-forward listing-codesearch--match-regex nil t)
      (lexical-let* ((filename (match-string 1))
                     (line-number (string-to-number (match-string 2)))
                     (visit-match (lambda (b)
                                    (find-file-other-window filename)
                                    (goto-line line-number))))
        (make-text-button
         (match-beginning 1)
         (match-end 1)
         'type 'listing-codesearch--filename-match-button
         'action visit-match)

        (make-text-button
         (match-beginning 2)
         (match-end 2)
         'type 'listing-codesearch--line-number-match-button
         'action visit-match)))))

(defvar listing-codesearch-pattern-history nil)

(defvar listing-codesearch-file-pattern-history nil)

;;;###autoload
(defun listing-codesearch-search (pattern file-pattern)
  "Search files matching `file-pattern'in the index for `pattern'."
  (interactive
   (list
    (read-string "Pattern: " (thing-at-point 'symbol)
                 'listing-codesearch-pattern-history (car listing-codesearch-pattern-history))
    (read-string "File pattern: " ".*"
                 'listing-codesearch-file-pattern-history (car listing-codesearch-file-pattern-history))))
  (lexical-let ((file-pattern (if (memq system-type '(windows-nt ms-dos))
                                  (replace-regexp-in-string "/" "\\\\\\\\" file-pattern)
                                file-pattern))
                (buff (get-buffer-create "*codesearch-results*"))
                (proc (codesearch-run-csearch
                       default-directory
                       (list "-f" file-pattern
                             "-n" pattern))))

    (with-current-buffer buff
      (read-only-mode 0)
      (erase-buffer))

    (set-process-sentinel
     proc
     #'(lambda (process event)
         (when (string-equal event "finished\n")
           (listing-codesearch--make-filenames-clickable buff)
           (pop-to-buffer buff)
           (read-only-mode 1))))

    (set-process-filter
     proc
     #'(lambda (process output)
         (let ((switch-to-visible-buffer t))
           (with-current-buffer buff
             (insert output)))))))

;;;###autoload
(defun listing-codesearch-list-directories ()
  "List the directories currently being indexed."
  (interactive)
  (lexical-let ((buff (get-buffer-create "*codesearch-directories*"))
                (proc (codesearch-run-cindex
                       nil nil
                       "-list")))
    (with-current-buffer buff
      (read-only-mode 0)
      (erase-buffer)
      (insert "[codesearch: currently indexed directories]\n\n")
      (pop-to-buffer buff))

    (set-process-filter
     proc
     #'(lambda (process output)
         (with-current-buffer buff
           (insert output))))))

(provide 'listing-codesearch)

;;; listing-codesearch.el ends here
