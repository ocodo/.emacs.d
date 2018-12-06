;;; easy-kill-buffer.el --- buffer selectors for easy-kill.

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
;; This tweak adds buffer selectors to easy-kill.
;;
;; This library is part of the easy-kill-extras package and not meant
;; to be used standalone.

;;; Code:

(require 'easy-kill)

;;;###autoload
(defun easy-kill-on-buffer (n)
  "Provide an easy-kill target `buffer' which selects the whole buffer."
  (easy-kill-adjust-candidate 'buffer (point-min) (point-max)))

;;;###autoload
(defun easy-kill-on-buffer-after-point (n)
  "Provide an easy-kill target `buffer-after-point', which works like vi's `G' command.
The +/- operation determines inclusion/exclusion of the current line."
  (easy-kill-adjust-candidate 'buffer-after-point
                              (pcase n
                                (`+
                                 (point-at-bol))
                                (`-
                                 (point-at-bol 2))
                                (_
                                 (point)))
                              (point-max)))

;;;###autoload
(defun easy-kill-on-buffer-before-point (n)
  "Provide an easy-kill target `buffer-before-point', which works like vi's `gg' command.
The +/- operation determines inclusion/exclusion of the current line."
       (easy-kill-adjust-candidate 'buffer-before-point
                                   (point-min)
                                   (pcase n
                                     (`+
                                      (point-at-bol 2))
                                     (`-
                                      (point-at-bol))
                                     (_
                                      (point)))))

(provide 'easy-kill-buffer)
;;; easy-kill-buffer.el ends here
