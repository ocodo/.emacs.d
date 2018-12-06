;;; helm-smex.el --- Helm interface for smex -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Peter Vasil

;; Author: Peter Vasil <mail@petervasil.net>
;; Version: 0.3
;; Package-Version: 20171004.2008
;; Package-Requires: ((emacs "24") (smex "3.0") (helm "1.7.7"))
;; Keywords: convenience

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

;; This package provides the helm interface for smex.
;;
;; Example config:
;;
;;   (require 'helm-smex)
;;   (global-set-key [remap execute-extended-command] #'helm-smex)
;;   (global-set-key (kbd "M-X") #'helm-smex-major-mode-commands)

;;; Code:

(require 'smex)
(require 'helm)
(require 'helm-elisp)
(require 'helm-source)
(require 'helm-command)

(defgroup helm-smex nil
  "Helm interface for smex"
  :group 'helm)

(defcustom helm-smex-show-bindings nil
  "Show bindings next to the command if non-nil."
  :type 'boolean)

(defun helm-smex--init ()
  (unless smex-initialized-p
    (smex-initialize))
  (and smex-auto-update
       (smex-detect-new-commands)
       (smex-update)))

(defun helm-smex--execute-command (command)
  (unless (commandp command)
    (error "`%s' is not a valid command name" command))
  (setq this-command command)
  ;; Normally `real-this-command' should never be changed, but here we really
  ;; want to pretend that M-x <cmd> RET is nothing more than a "key binding" for
  ;; <cmd>.
  (setq real-this-command command)
  (let ((prefix-arg current-prefix-arg))
    (unwind-protect
        (command-execute command 'record)
      (smex-rank command))))

(defun helm-smex--persistent-action (candidate)
  (helm-elisp--persistent-help
   candidate 'helm-describe-function))

(defun helm-smex--transformer (candidates _source)
  "Transformer function for `helm-smex' CANDIDATES."
  (helm-M-x-transformer-1 candidates))

(defclass helm-smex-source (helm-source-sync)
  ((init :initform 'helm-smex--init)
   (candidates :initform 'smex-ido-cache)
   (match :initform 'helm-fuzzy-match)
   (action :initform 'helm-smex--execute-command)
   (coerce :initform 'intern)
   (persistent-action
    :initform helm-smex--persistent-action)
   (persistent-help :initform "Describe command")
   (filtered-candidate-transformer
    :initform (and helm-smex-show-bindings
                   'helm-smex--transformer))))

(defun helm-smex--major-mode-commands (mode map)
  (unless smex-initialized-p
    (smex-initialize))
  (let ((commands
         (delete-dups
          (append (smex-extract-commands-from-keymap map)
                  (smex-extract-commands-from-features mode)))))
    (mapcar #'symbol-name
            (smex-sort-according-to-cache commands))))

;;;###autoload
(defun helm-smex ()
  (interactive)
  (let ((helm--mode-line-display-prefarg t))
    (helm :buffer "*helm-smex*"
          :sources (helm-make-source "Smex" 'helm-smex-source))))

;;;###autoload
(defun helm-smex-major-mode-commands ()
  (interactive)
  (let ((helm--mode-line-display-prefarg t)
        (candidates-fn (apply #'helm-smex--major-mode-commands
                              (list major-mode (current-local-map)))))
    (helm :buffer "*helm-smex*"
          :sources (helm-make-source
                       "Smex-major-mode-commands" 'helm-smex-source
                     :init nil :candidates candidates-fn))))

(provide 'helm-smex)

;;; helm-smex.el ends here
