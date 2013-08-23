;;; pcmpl-ack.el --- completion for ack    -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2013  Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Keywords: tools, processes, convenience
;; Created: 2012-09-26
;; URL: https://github.com/leoliu/ack-el

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

;; Provide pcompletion support for the cli tool `ack' which can be
;; downloaded from http://beyondgrep.com.
;;
;; Install:
;;   (autoload 'pcomplete/ack "pcmpl-ack")
;;
;; Usage:
;;   - To complete short options type '-' first
;;   - To complete long options type '--' first
;;   - Color name completion is supported following
;;       --color-filename=, --color-match= and --color-lineno=
;;   - Type completion is supported following --type=

;;; Code:

(require 'pcomplete)

(defcustom pcmpl-ack-program
  (file-name-nondirectory (or (executable-find "ack-grep")
                              (executable-find "ack")
                              "ack"))
  "Name of the ack program."
  :type 'file
  :group 'pcomplete)

(defvar pcmpl-ack-color-options
  '("clear"
    "reset"
    "dark"
    "bold"
    "underline"
    "underscore"
    "blink"
    "reverse"
    "concealed"
    "black"
    "red"
    "green"
    "yellow"
    "blue"
    "magenta"
    "on_black"
    "on_red"
    "on_green"
    "on_yellow"
    "on_blue"
    "on_magenta"
    "on_cyan"
    "on_white")
  "Color names for the `ack' command.")

(defun pcmpl-ack-run (buffer &rest args)
  "Run ack with ARGS and send the output to BUFFER."
  (condition-case nil
      (apply 'call-process (or pcmpl-ack-program "ack") nil buffer nil args)
    (file-error -1)))

(defun pcmpl-ack-short-options ()
  "Short options for the `ack' command."
  (with-temp-buffer
    (let (options)
      (when (zerop (pcmpl-ack-run t "--help"))
        (goto-char (point-min))
        (while (re-search-forward "^  -\\([^-]\\)" nil t)
          (push (match-string 1) options))
        (mapconcat 'identity (nreverse options) "")))))

(defun pcmpl-ack-long-options (&optional arg)
  "Long options for the `ack' command."
  (with-temp-buffer
    (let (options)
      (when (zerop (pcmpl-ack-run t (or arg "--help")))
        (goto-char (point-min))
        (while (re-search-forward
                "\\(?:   ?\\|, \\)\\(--\\(\\[no\\]\\)?\\([[:alnum:]-]+=?\\)\\)"
                nil t)
          (if (not (match-string 2))
              (push (match-string 1) options)
            (push (concat "--" (match-string 3)) options)
            (push (concat "--no" (match-string 3)) options)))
        (nreverse options)))))

(defun pcmpl-ack-type-options ()
  "A list of types for the `ack' command."
  (pcmpl-ack-long-options "--help-types"))

;;;###autoload
(defun pcomplete/ack ()
  "Completion for the `ack' command.
Start an argument with '-' to complete short options and '--' for
long options."
  ;; No space after =
  (while t
    (if (pcomplete-match "^-" 0)
        (cond
         ((pcomplete-match "^--color-\\w+=\\(\\S-*\\)" 0)
          (pcomplete-here* pcmpl-ack-color-options
                           (pcomplete-match-string 1 0) t))
         ((pcomplete-match "^--\\(?:no\\)?ignore-dir=\\(\\S-*\\)" 0)
          (pcomplete-here* (pcomplete-dirs)
                           (pcomplete-match-string 1 0) t))
         ((pcomplete-match "^--type=\\(\\S-*\\)" 0)
          (pcomplete-here* (mapcar (lambda (type-option)
                                     (substring type-option 2))
                                   (pcmpl-ack-type-options))
                           (pcomplete-match-string 1 0) t))
         ((pcomplete-match "^--" 0)
          (pcomplete-here* (append (pcmpl-ack-long-options)
                                   (pcmpl-ack-type-options))))
         (t (pcomplete-opt (pcmpl-ack-short-options))))
      (pcomplete-here* (pcomplete-dirs-or-entries)))))

;;;###autoload
(defalias 'pcomplete/ack-grep 'pcomplete/ack)

(provide 'pcmpl-ack)
;;; pcmpl-ack.el ends here
