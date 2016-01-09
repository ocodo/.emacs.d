;;; flx-isearch.el --- Fuzzy incremental searching for emacs -*- lexical-binding: t -*-

;; Copyright (C) 2014, 2015, 2016 PythonNut

;; Author: PythonNut <pythonnut@pythonnut.com>
;; Keywords: convenience, search, flx
;; Package-Version: 20160105.1217
;; Version: 20141313
;; URL: https://github.com/pythonnut/flx-isearch
;; Package-Requires: ((emacs "24") (flx "20140821") (cl-lib "0.5"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Fuzzy matching is awesome, when done right.

;; This program lets you isearch for "fiis" and be taken to matches for `flx-isearch-initialize-state'.
;; Searches currently only apply to symbols. The input string is flex matched to all symbols in the buffer,
;; and all matching symbols are searched one by one.

;; For example, searching for `fii` in `flx-isearch.el' first takes you to
;; * all instances of `flx-isearch-index' one by one
;; * all instances of `flx-isearch-initialize-state' one by one
;; * all instances of `flx-isearch-lazy-index' one by one
;; * [...]
;; * all instances of `bounds-of-thing-at-point' one by one

;; The _hope_ is that `flx' will be smart enough to quickly take you to the symbol you're thinking of with minimal effort.

;; Usage:

;; By default, `flx-isearch` does not bind any keys. `package.el' will
;; automatically setup the appropriate autoloads, and you can then do this:

;;     (global-set-key (kbd "C-M-s") #'flx-isearch-forward)
;;     (global-set-key (kbd "C-M-r") #'flx-isearch-backward)

;;; Code:

(require 'flx)
(require 'cl-lib)

;; derived from flex-isearch.el
(defgroup flx-isearch nil
  "Flex matching in isearch with flx"
  :prefix "flx-isearch-"
  :group 'isearch
  :link '(url-link :tag "Development and bug reports"
                   "https://github.com/PythonNut/flx-isearch"))

;; derived from flex-isearch.el
(defcustom flx-isearch-message-prefix "[flx] "
  "Prepended to the isearch prompt when flx searching is activated."
  :type 'string
  :group 'flx-isearch)

;; flx-isearch has to store a lot of state
(defvar flx-isearch-index 0
  "The index of the current symbol being searched for")

(defvar flx-isearch-point 0
  "The (point) at which the search started")

(defvar flx-isearch-last-search ""
  "The previous search string")

(defvar flx-isearch-was-wrapped nil
  "Whether or not the last searched caused the current search to wrap")

(defvar flx-isearch-lazy-flag nil
  "Whether or not the current searches are of the lazy variety")
(defvar flx-isearch-last-lazy-flag nil
  "Whether or not the last search was of the lazy variety")

(defvar flx-isearch-lazy-index nil
  "Holds the last value of `flx-isearch-index' during lazy
highlighting")

(defvar flx-isearch-lazy-point nil
  "Holds the last value of `flx-isearch-point' during lazy
highlighting")

(defvar flx-isearch-original-search-fun nil
  "Stores the previous value of `isearch-search-fun-function'
during flx-isearch searches")

(defvar flx-isearch-activated nil
  "When nil, search is never flexible")

(defun flx-isearch-collect-symbols ()
  (interactive)
  (let ((coll nil))
    (save-excursion
      (goto-char (point-min))
      (while (forward-thing 'symbol)
        (push `(
                ,(substring-no-properties
                  (thing-at-point 'symbol))
                ,(car (bounds-of-thing-at-point 'symbol)))
              coll))
      coll)))

(defun flx-isearch-fuse-alist (pairs)
  "Turn an alist with duplicate keys into a hash table that maps
keys to lists of values. Ordering is preserved."
  (let ((mapping (make-hash-table
                  :test 'equal
                  :size 1000))
        (result))
    (dolist (elt pairs mapping)
      (puthash (car elt) (cons
                          (cadr elt)
                          (gethash (car elt) mapping nil))
               mapping))))

(defun flx-isearch-hash-table-to-alist (hash-table)
  "Convert a hash table to an alist. No ordering is guaranteed."
  (let ((result))
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    result))

(defun flx-isearch-sort (str symbols &optional cache)
  "Sort the strings in `symbols' according to their `flx-score'
with respect to `str'"
  (mapcar (lambda (item) (car item))
          (sort (cl-remove-if-not
                 #'cdr
                 (mapcar (lambda (item)
                           (cons item
                                 (car (flx-score (car item) str cache))))
                         symbols))
                (lambda (a b)
                  (> (cdr a) (cdr b))))))

;; and the cache stuff
(defvar flx-isearch-cache-level-1 nil
  "Used to store the list of symbols in the current buffer, so
the buffer is not scanned on every search")

(defvar flx-isearch-cache-level-2 nil
  "Used to store the flx cache which speeds up flx's own internals")

(defvar flx-isearch-cache-level-3 nil
  "Used to store the complete list of sorted symbols so they are not
recomputed on `isearch-repeat-forward' and `isearch-repeat-backward'")

(defun flx-isearch-heatmap (symbol-name)
  "The flx heatmap used to store the symbols in the current buffer"
  (flx-get-heatmap-str symbol-name))

(defun flx-isearch-make-cache ()
  "The flx cache used to store the symbols in the current buffer"
  (flx-make-string-cache #'flx-isearch-heatmap))

(defun flx-isearch-initialize-state ()
  "Reset all stateful variables to their default values
should be called before any search is started"
  (setq flx-isearch-cache-level-1 (flx-isearch-hash-table-to-alist
                                   (flx-isearch-fuse-alist
                                    (flx-isearch-collect-symbols)))
        flx-isearch-cache-level-2 (flx-isearch-make-cache)
        flx-isearch-cache-level-3 nil
        flx-isearch-lazy-flag nil
        flx-isearch-last-lazy-flag nil
        flx-isearch-was-wrapped nil
        flx-isearch-index 0
        flx-isearch-point (point)
        flx-isearch-last-search ""))

(defun flx-isearch-resolve-last-state ()
  "Resolves changes of isearch state including switches from
and to lazy highlighting and isearch wrapping"
  ;; when switching to lazy highlighting
  (when (and flx-isearch-lazy-flag
             (not flx-isearch-last-lazy-flag))
    ;; stash the state that will be clobbered
    (setq flx-isearch-lazy-index flx-isearch-index
          flx-isearch-lazy-point flx-isearch-point)
    ;; reset variables
    (setq flx-isearch-last-lazy-flag t
          flx-isearch-index 0
          flx-isearch-point (point)))

  ;; when switching back from lazy highlighting
  (when (and (not flx-isearch-lazy-flag)
             flx-isearch-last-lazy-flag)
    ;; restore the stashed state
    (setq flx-isearch-last-lazy-flag nil
          flx-isearch-index flx-isearch-lazy-index
          flx-isearch-point flx-isearch-lazy-point))

  ;; also reset when isearch wraps
  (when (and isearch-wrapped (not flx-isearch-was-wrapped))
    (setq
     flx-isearch-was-wrapped t
     flx-isearch-point (point-min)
     flx-isearch-index 0)))

(defun flx-isearch-compute-matches (string)
  "Returns the list of matching symbols, sorted by flx-score.
Takes advantage of all caches"
  (if (equal string flx-isearch-last-search)
      flx-isearch-cache-level-3
    (progn
      (goto-char flx-isearch-point)
      (setq flx-isearch-index 0
            flx-isearch-last-search string
            flx-isearch-cache-level-3
            (flx-isearch-sort string
                              flx-isearch-cache-level-1
                              flx-isearch-cache-level-2)))))

(defun flx-search-forward (string &optional bound noerror count)
  "Search forward just like `search-forward' but with flx matching.
Note that this means that newpoint > oldpoint is not always true."
  (interactive "M")
  (flx-isearch-resolve-last-state)
  (let* ((matches (flx-isearch-compute-matches string))
         (match (elt matches flx-isearch-index)))
    (if (re-search-forward
         (concat "\\_<" (car match) "\\_>")
         nil t count)
        (point)
      (progn
        (setq flx-isearch-index (1+ flx-isearch-index))
        (if (>= flx-isearch-index (length matches))
            (if noerror
                nil
              (error "flx forward search failed"))
          (progn
            (goto-char flx-isearch-point)
            (flx-search-forward string bound noerror count)))))))

(defun flx-search-backward (string &optional bound noerror count)
  "Search backward just like `search-backward' but with flx matching.
Note that this means that newpoint < oldpoint is not always true."
  (interactive "M")
  (flx-isearch-resolve-last-state)
  (let* ((matches (flx-isearch-compute-matches string))
         (match (elt matches flx-isearch-index)))
    (if (re-search-backward
         (concat "\\_<" (car match) "\\_>")
         nil t count)
        (point)
      (progn
        (setq flx-isearch-index (1+ flx-isearch-index))
        (if (>= flx-isearch-index (length matches))
            (if noerror
                nil
              (error "flx backward search failed"))
          (progn
            (goto-char flx-isearch-point)
            (flx-search-backward string bound noerror count)))))))

(defadvice isearch-lazy-highlight-search (around flx-isearch-set-lazy-flag)
  "Sets a flag so flx-isearch can determine if the current search is
of the lazy variety"
  (let ((flx-isearch-lazy-flag t))
    ad-do-it))

;; derived from flex-isearch.el
(defadvice isearch-message-prefix (after flx-isearch-message-prefix activate)
  (if flx-isearch-activated
      (setq ad-return-value (concat flx-isearch-message-prefix ad-return-value))
    ad-return-value))

;; derived from flex-isearch.el
(defun flx-isearch-search-fun ()
  "Set to `isearch-search-fun-function' when `flx-isearch-mode' is
enabled."
  (cond
   (isearch-word
    (if isearch-forward
        #'word-search-forward
      #'word-search-backward))
   (isearch-regexp
    (if isearch-forward
        #'re-search-forward
      #'re-search-backward))
   (flx-isearch-activated
    (if isearch-forward
        #'flx-search-forward
      #'flx-search-backward))
   (t
    (if isearch-forward
        #'search-forward
      #'search-backward))))

(defun flx-isearch-activate ()
  (interactive)
  (setq flx-isearch-activated t))

(defun flx-isearch-deactivate ()
  (interactive)
  (setq flx-isearch-activated nil))

;; derived from flex-isearch.el
;;;###autoload
(define-minor-mode flx-isearch-mode
  :init-value nil
  :group 'flx-isearch
  (if flx-isearch-mode
      (progn
        (setq flx-isearch-original-search-fun isearch-search-fun-function)
        (setq isearch-search-fun-function #'flx-isearch-search-fun)
        (add-hook 'isearch-mode-end-hook #'flx-isearch-deactivate)
        (add-hook 'isearch-mode-hook #'flx-isearch-initialize-state)
        (ad-enable-advice 'isearch-lazy-highlight-search
                          'around 'flx-isearch-set-lazy-flag)
        (ad-activate 'isearch-lazy-highlight-search))
    (progn
      (setq isearch-search-fun-function flx-isearch-original-search-fun)
      (remove-hook 'isearch-mode-end-hook #'flx-isearch-deactivate)
      (remove-hook 'isearch-mode-hook #'flx-isearch-initialize-state)
      (ad-disable-advice 'isearch-lazy-highlight-search
                         'around 'flx-isearch-set-lazy-flag)
      (ad-activate 'isearch-lazy-highlight-search))))

(defun flx-isearch-activate-maybe (regexp-p)
  (unless flx-isearch-mode
    (flx-isearch-mode +1))
  (when (null regexp-p)
    (flx-isearch-activate)))

;; derived from flex-isearch.el
;;;###autoload
(defun flx-isearch-forward (&optional regexp-p no-recursive-edit)
  "Start a fuzzy forward isearch"
  (interactive "P\np")
  (flx-isearch-activate-maybe regexp-p)
  (isearch-mode t (not (null regexp-p)) nil (not no-recursive-edit)))

;; derived from flex-isearch.el
;;;###autoload
(defun flx-isearch-backward (&optional regexp-p no-recursive-edit)
  "Start a fuzzy backward isearch"
  (interactive "P\np")
  (flx-isearch-activate-maybe regexp-p)
  (isearch-mode nil (not (null regexp-p)) nil (not no-recursive-edit)))

(defadvice isearch-forward
    (around flx-isearch activate preactivate compile)
  (when (and flx-isearch-mode
             (equal (ad-get-arg 0) '(16)))
    (flx-isearch-activate)
    (ad-set-arg 0 nil))
  ad-do-it)

(defadvice isearch-backward
    (around flx-isearch activate preactivate compile)
  (when (and flx-isearch-mode
             (equal (ad-get-arg 0) '(16)))
    (flx-isearch-activate)
    (ad-set-arg 0 nil))
  ad-do-it)

(provide 'flx-isearch)
;;; flx-isearch.el ends here
