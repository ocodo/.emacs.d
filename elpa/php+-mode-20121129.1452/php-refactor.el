;;; php-refactor.el --- Functions that deal with PHP refactoring

;; Version: 1.0
;; Created: 08-08-2011
;; Copyright © 2011 Michael Dwyer
;; Author(s): 
;; Michael Dwyer <mdwyer@ehtech.in>

;;; *****
;;; About
;;; *****

;; php-refactor.el is a part of the php+-mode suite and contains
;; convenience functions for renaming and moving PHP structures.

;; ********************************************************************

;; ************
;; REQUIREMENTS
;; ************
(require 'php-edit)
(require 'php-format)
(require 'php-parse)
(require 'string-utils)

(defvar php-refactor-last-buffer nil "Last buffer selected.")

(defun php-refactor-move-thing-to-buffer (buffer &optional type)
  "Move the current PHP structure to BUFFER and place it in its
proper place.  Optionally specify TYPE of structure."
  (interactive `(,(read-buffer "Buffer: " php-refactor-last-buffer)))
  (when (get-buffer buffer)
    (setq php-refactor-last-buffer buffer)
    (php-kill-current type)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (save-excursion
          (php-yank)
          (php-rearrange-current))))))

(defun php-refactor-move-all-things-in-class/interface-to-buffer (buffer)
  "Move all the structures in the current buffer to BUFFER using
`php-refactor-move-thing-to-buffer'.  Behavior when dealing with
anything but constants, properties and methods inside of a class
or interface is currently undefined."
  (interactive `(,(read-buffer "Buffer: " php-refactor-last-buffer)))
  (catch 'done
    (while t
      (re-search-forward non-ws-re nil t)
      (let ((thing (php-parse-current)))
        (if (and (php-parse-p thing)
                 (member (rest (assoc 'type thing)) 
                         '(constant property method)))
            (let ((begin (rest (assoc 'begin thing))))
              (goto-char begin)
              (php-refactor-move-thing-to-buffer buffer))
          (throw 'done t))))))

(provide 'php-refactor)
