;;; easy-kill-line-edge.el --- line-edge selectors for easy-kill.

;; Copyright (c) 2014-2015 Akinori MUSHA
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/easy-kill-extras.el
;; Created: 4 Mar 2015
;; Package-Requires: ((easy-kill "0.9.4"))
;; Keywords: killing, convenience

;;; Commentary:
;;
;; This tweak adds line-edge selectors to easy-kill.
;;
;; This library is part of the easy-kill-extras package and not meant
;; to be used standalone.

;;; Code:

(require 'easy-kill)

;;;###autoload
(defun forward-line-edge (arg)
  "Move between line edges.  ARG specifies which edge to move to.

If ARG is -2 or less, move to the BOL.

If ARG is -1, move to the first non-whitespace character after
the point on the line, or BOL if there is none.

If ARG is 0, stay.

If ARG is 1, move to the position right after the last
non-whitespace character after the point on the line, or EOL if
there is none.

If ARG is 2 or greater, move to the EOL."
  (interactive "p")
  (pcase arg
    (0)
    (1
     (if (looking-at "\\(.*[^[:space:]]\\)[[:space:]]+$")
         (goto-char (match-end 1))
       (end-of-line)))
    ((pred (<= 2))
     (end-of-line))
    (-1
     (if (looking-back "^[[:space:]]*")
         (beginning-of-line)
       (back-to-indentation)))
    (_
     (beginning-of-line))))

;;;###autoload
(defun backward-line-edge (arg)
  "Equivalent to `forward-line-edge' with a negative ARG."
  (interactive "p")
  (forward-line-edge (- arg)))

;;;###autoload
(defun easy-kill-on-forward-line-edge (n)
  "Provide an easy-kill target `forward-line-edge', which works like vi's `^'/`0' commands in the opposite direction."
  (easy-kill-adjust-candidate 'forward-line-edge
                              (point)
                              (save-excursion
                                (forward-line-edge
                                 (pcase n
                                   (`+ 2)
                                   (`- 1)
                                   (1 (if (eq (easy-kill-get thing) 'forward-line-edge) 2 1))
                                   (_ n)))
                                (point))))

;;;###autoload
(defun easy-kill-on-backward-line-edge (n)
  "Provide an easy-kill target `backward-line-edge', which works like vi's `^'/`0' commands."
  (easy-kill-adjust-candidate 'backward-line-edge
                              (point)
                              (save-excursion
                                (backward-line-edge
                                 (pcase n
                                   (`+ 2)
                                   (`- 1)
                                   (1 (if (eq (easy-kill-get thing) 'backward-line-edge) 2 1))
                                   (_ n)))
                                (point))))

(provide 'easy-kill-line-edge)
;;; easy-kill-line-edge.el ends here
