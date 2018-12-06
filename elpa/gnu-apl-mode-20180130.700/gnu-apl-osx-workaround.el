;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'gnu-apl-util)

(defun gnu-apl-update-fontset-character (spec)
  (dolist (s gnu-apl--symbols)
    (let ((char (aref (second s) 0)))
      (when (> char 255)
        (set-fontset-font t (cons char char) (font-spec :family spec)))))
  (set-fontset-font t '(#x2500 . #x2594) (font-spec :family spec)))

(provide 'gnu-apl-osx-workaround)
