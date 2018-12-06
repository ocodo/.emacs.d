;;; jasminejs-mode.el --- A minor mode for manipulating jasmine test files -*- lexical-binding: t -*-

;; Copyright (C) 2014 Eric Stolten
;; Filename: jasminejs-mode.el
;; Description: Work with jasminejs files
;; Author: Eric Stolten <stoltene2@gmail.com>
;; Created: 20 Nov 2014
;; Keywords: javascript jasmine
;; Homepage: https://github.com/stoltene2/jasminejs-mode
;; Version: 2.0

;; This file is not part of GNU Emacs.

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
;; The main purpose of this mode is to interact with jasmine testing
;; files for javascript.

;;; Code:

(defun jasminejs-toggle-focus-it ()
  "Toggle the `it` function to focus.
When you toggle a test it will toggle it between `it` and `fit`."
  (interactive)
  (jasminejs--toggle-previous-word "it" "f" "x"))

(defun jasminejs-toggle-focus-describe ()
  "Toggle the `describe` function to focus.
When you toggle a test it will toggle it between `describe` and `fdescribe`."
  (interactive)
  (jasminejs--toggle-previous-word "describe" "f" "x"))

(defun jasminejs-toggle-pending-it ()
  "Toggle the `it` function to pending.
When you toggle a test it will toggle it between `it` and `xit`."
  (interactive)
  (jasminejs--toggle-previous-word "it" "x" "f"))

(defun jasminejs-toggle-pending-describe ()
  "Toggle the `describe` function to pending.
When you toggle a test it will toggle it between `describe` and `xdescribe`."
  (interactive)
  (jasminejs--toggle-previous-word "describe" "x" "f"))

(defun jasminejs--toggle-previous-word (word toggle-char &optional remove-char)
  "Toggle WORD on or off by prefixing it with TOGGLE-CHAR.

If you pass the optional REMOVE-CHAR is passed we check to see if
REMOVE-CHAR precedes WORD.  If it does we remove it.

This is useful for toggling between an xdescribe and a ddescribe, for
example."
  (let* ((word-regex (concat  "\\<[" toggle-char remove-char "]?" word "\w*("))
         (toggle-word (concat toggle-char word)))
    (save-excursion
      (if (re-search-backward word-regex (point-min) 'no-error)
          (progn
            (beginning-of-line-text)
            (if remove-char
                (when (looking-at (concat remove-char word))
                  (delete-char (length remove-char))))
            (when (looking-at word)
              (insert toggle-char))
            (when (looking-at toggle-word)
              (delete-char (length toggle-char))))
        (message "I could not find '%s'" word)))))

(defgroup jasminejs-mode nil
  "jasminejs-mode customizations"
  :group 'development)

(defconst jasminejs-snippet-path
  (file-name-directory (if (bound-and-true-p load-file-name)
                           load-file-name
                         (buffer-file-name)))

  "This is the location of the bundled jasminejs snippets.")

(defcustom jasminejs-prefix-key (kbd "C-c C-j")
  "This is the standard key prefix key for leading into jasminejs shortcuts.

WARNING: Changing this prefix will not take effect
dynamically.  You will need to reload to take effect."
  :type 'key-sequence)

(defvar jasminejs-prefix-map
  (let ((jasminejs-prefix-map))
    (define-prefix-command 'jasminejs-prefix-map)
    (define-key jasminejs-prefix-map (kbd "it") 'jasminejs-toggle-focus-it)
    (define-key jasminejs-prefix-map (kbd "ip") 'jasminejs-toggle-pending-it)

    (define-key jasminejs-prefix-map (kbd "dt") 'jasminejs-toggle-focus-describe)
    (define-key jasminejs-prefix-map (kbd "dp") 'jasminejs-toggle-pending-describe)

;;    (define-key prefix-map jasminejs-prefix-key prefix-map)
    jasminejs-prefix-map)

  "Keymap for jasminejs mode.")

(defvar jasminejs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map jasminejs-prefix-key 'jasminejs-prefix-map)
    map))

(defun jasminejs-add-snippets-to-yas-snippet-dirs (&optional snippet-path)
  "This activates jasminejs-mode as an extra mode for yasnippet.
It also puts the snippet directory at the front of the
yas-snippet-dirs list.  If the &optional SNIPPET-PATH is not
passed, the fefault value of jasminejs-snippet-path is used."
  (if yas-snippet-dirs
      (let* ((snippet-dir (or snippet-path jasminejs-snippet-path)))
        (yas-activate-extra-mode 'jasminejs-mode)
        (add-to-list 'yas-snippet-dirs snippet-dir)
        (yas-load-directory snippet-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autoload the minor mode
;;;###autoload
(define-minor-mode jasminejs-mode
  "To better edit your files"
  nil " Jas" jasminejs-mode-map)

(provide 'jasminejs-mode)
;;; jasminejs-mode.el ends here
