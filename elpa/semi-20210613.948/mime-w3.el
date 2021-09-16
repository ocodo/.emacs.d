;;; mime-w3.el --- mime-view content filter for text  -*- lexical-binding: t -*-

;; Copyright (C) 1994,95,96,97,98,99,2000 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: HTML, MIME, multimedia, mail, news

;; This file is part of SEMI (Suite of Emacs MIME Interfaces).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'w3 nil t)
(require 'mime)

(eval-and-compile
  (defmacro mime-put-keymap-region (start end keymap)
    `(put-text-property ,start ,end 'local-map ,keymap)))

(defmacro mime-save-background-color (&rest body)
  `(cons 'progn ,body))

(defvar mime-w3-message-structure nil)

(defun mime-preview-text/html (entity _situation)
  (setq mime-w3-message-structure (mime-find-root-entity entity))
  (goto-char (point-max))
  (let ((p (point)))
    (insert "\n")
    (goto-char p)
    (save-restriction
      (narrow-to-region p p)
      (mime-insert-text-content entity)
      (run-hooks 'mime-text-decode-hook)
      (condition-case err
	  (w3-region p (point-max))
	 (error (message "%s" err)))
      (mime-put-keymap-region p (point-max) w3-mode-map))))

(defun url-cid (url &optional _proxy-info)
  (let ((entity
	 (mime-find-entity-from-content-id (mime-uri-parse-cid url)
					   mime-w3-message-structure))
	buffer)
    (when entity
      (setq buffer (generate-new-buffer (format " *cid %s" url)))
      (save-excursion
	(set-buffer buffer)
	(mime-insert-entity-content entity)
	(if (boundp 'url-current-mime-type)
	    (setq url-current-mime-type (mime-entity-type/subtype entity)))))
    buffer))

(if (fboundp 'url-register-protocol)
    (url-register-protocol "cid"
			   'url-cid
			   'url-identity-expander)
  (provide 'url-cid))


;;; @ end
;;;

(provide 'mime-w3)

;;; mime-w3.el ends here
