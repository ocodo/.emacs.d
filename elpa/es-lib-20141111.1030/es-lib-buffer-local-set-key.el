;;; es-lib-buffer-local-set-key.el --- Define keys in one buffer, not shared by buffers of the same mode
;;; Version: 0.4
;;; Author: sabof
;;; URL: https://github.com/sabof/es-lib

;;; Commentary:

;; The project is hosted at https://github.com/sabof/es-lib
;; The latest version, and all the relevant information can be found there.

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cl-lib)
(require 'es-lib-core-macros)

(defvar es-buffer-local-mode nil)
(make-variable-buffer-local 'es-buffer-local-mode)

(cl-defun es-buffer-local-set-key (key action)
  (when es-buffer-local-mode
    (define-key (es-mode-keymap es-buffer-local-mode)
        key action)
    (cl-return-from es-buffer-local-set-key))
  (let* ((mode-name-loc (cl-gensym "-blm")))
    (eval `(define-minor-mode ,mode-name-loc nil nil nil (make-sparse-keymap)))
    (setq es-buffer-local-mode mode-name-loc)
    (funcall mode-name-loc 1)
    (define-key (es-mode-keymap mode-name-loc) key action)))

(defun es-buffer-local-set-keys (&rest bindings)
  (while bindings
    (es-buffer-local-set-key (pop bindings) (pop bindings))))
(put 'es-buffer-local-set-keys
     'common-lisp-indent-function
     '(&body))

(provide 'es-lib-buffer-local-set-key)
;; es-lib-buffer-local-set-key.el ends here
