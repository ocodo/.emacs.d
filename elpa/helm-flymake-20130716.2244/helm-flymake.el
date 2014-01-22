;;; helm-flymake.el --- helm interface for flymake

;; Copyright (C) 2012-2013 Akira Tamamori

;; Author: Akira Tamamori <tamamori5917@gmail.com>
;; URL: https://github.com/tam17aki
;; Version: 20130716.2244
;; X-Original-Version: 0.1.7
;; Package-Requires: ((helm "1.0"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `helm' interface for flymake.
;; When `flymake-mode' is t, M-x `helm-flymake' lists warning and error
;; messages in *helm flymake* buffer.
;; C-u M-x `helm-flymake' insert the line number of current cursor position
;; into minibuffer.
;;

;;; Installation:
;;
;; Add followings on your .emacs.
;;
;;   (require 'helm-config)
;;   (require 'helm-flymake)
;;

;;; History:
;;
;; Revision 0.1.7
;; * convert prefix of helm-c-* into helm-*.
;;
;; Revision 0.1.6
;; * fix typo.
;;
;; Revision 0.1.5
;; * remove global variable `helm-c-flymake-err-list'.
;;
;; Revision 0.1.4
;; * revise documents.
;;
;; Revision 0.1.3
;; * add default input, line number at current point when `helm-flymake'
;;   is invoked with prefix argument.
;;
;; Revision 0.1.2
;; * add magic comment `autoload' to `helm-flymake'.
;;
;; Revision 0.1.1
;; * add variable `helm-c-flymake-buffer'.
;;
;; Revision 0.1
;; * Initial revision

;;; Code:

(eval-when-compile (require 'cl))
(require 'flymake)
(require 'helm)

(defvar helm-flymake-buffer "*helm flymake*")

(defun helm-flymake-get-err-list ()
  (loop for err-info in flymake-err-info
        for err = (nth 1 err-info)
        append err))

(defun helm-flymake-get-candidate (err-type)
  (mapcar (lambda (err)
            (let* ((type (flymake-ler-type err))
                   (text (flymake-ler-text err))
                   (line (flymake-ler-line err)))
              (cond
               ((and (equal type err-type)
                     (equal err-type "w"))
                (format "%4s:%s" line text))
               ((and (equal type err-type)
                     (equal err-type "e"))
                (format "%4s:%s" line text)))))
          (helm-flymake-get-err-list)))

(defun helm-flymake-init (err-type)
  (helm-init-candidates-in-buffer
   'local (helm-flymake-get-candidate err-type)))

(defvar helm-source-flymake-warning
  '((name . "Flymake Warning")
    (init . (lambda () (helm-flymake-init "w")))
    (candidates-in-buffer)
    (candidate-transformer . (lambda (cands) (delete "" cands)))
    (type . line)
    (recenter)))

(defvar helm-source-flymake-error
  '((name . "Flymake Error")
    (init . (lambda () (helm-flymake-init "e")))
    (candidates-in-buffer)
    (candidate-transformer . (lambda (cands) (delete "" cands)))
    (type . line)
    (recenter)))

;;;###autoload
(defun helm-flymake (arg)
  "helm interface for flymake."
  (interactive "P")
  (let ((buf (get-buffer-create helm-flymake-buffer))
        (linum (format "%4d:" (line-number-at-pos (point)))))
    (helm :sources '(helm-source-flymake-warning
                     helm-source-flymake-error)
          :buffer buf
          :input (and arg linum))))

(provide 'helm-flymake)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-flymake.el ends here
