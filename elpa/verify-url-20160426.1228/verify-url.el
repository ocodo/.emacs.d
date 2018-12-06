;;; verify-url.el --- find out invalid urls in the buffer or region

;; Copyright (C) 2004-2015 DarkSun <lujun9972@gmail.com>.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2015-12-21
;; Version: 0.1
;; Package-Version: 20160426.1228
;; Keywords: convenience, usability, url
;; Package-Requires: ((cl-lib "0.5"))
;; URL: https://github.com/lujun9972/verify-url

;; This file is NOT part of GNU Emacs.

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

;;; Source code
;;
;; verify-url's code can be found here:
;;   http://github.com/lujun9972/verify-url

;;; Commentary:

;; verify-url is a little tool that used to find out invalid urls in the buffer or region

;; Quick start:

;; execute the following commands:
;; verify-url
;;

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'url-file)
(require 'url-http)
(require 'url-ftp)

(defgroup verify-url nil
  "verify url group"
  :prefix "verify-url"
  :group 'url)

(defcustom verify-url/regex
  "\\(file\\|ftp\\|http\\|https\\)://[^][:blank:]\r\n<>{}()*#$^['\\|]+"
  "regex that used to recognize urls")

(defcustom verify-url/time-out 10
  "expire time when connect to remote machine"
  :group 'verify-url)

(defcustom verify-url/auto-jump-to-first-invalid-url t
  "If non-nil, automatically jump to the first invalid url")

(defface verify-url/invalid-url-face '((t :underline t
                                          :inherit 'font-lock-warning-face))
         "Face for the invalid url."
         :group 'verify-url)

(defun verify-url--url-readable-p (url)
  (ignore-errors
    (save-match-data
      (with-timeout (verify-url/time-out nil)
        (let ((url-type (url-type (url-generic-parse-url url))))
          (cond ((equal url-type "ftp")
                 (url-ftp-file-readable-p url))
                ((equal url-type "file")
                 (url-file-file-readable-p url))
                ((equal url-type "http")
                 (url-http-file-readable-p url))
                ((equal url-type "https")
                 (url-https-file-readable-p url))
                (t
                 (file-readable-p url))))))))

(defun verify-url/modification-hook (overlay after-change-p beg end &optional length)
  "Remove the invalid-url-overlay when the url changed"
  (delete-overlay overlay))

(defun verify-url--make-invalid-url-overlay (start end)
  "make an invalid-url-overlay between START and END which face is `verify-url/invalid-url-face'"
  (let ((o (make-overlay start end)))
    (overlay-put o 'face 'verify-url/invalid-url-face)
    (overlay-put o 'verify-url/invalid-url-overlay t)
    (overlay-put o 'modification-hooks '(verify-url/modification-hook))
    ;; (overlay-put o 'help-echo "invalid-url")
    o))

(defun verify-url--invalid-url-overlay-p (overlay)
  (overlay-get overlay 'verify-url/invalid-url-overlay))

;;;###autoload
(defun verify-url (&optional start end)
  "find out invalid urls in buffer or region"
  (interactive)
  (if (use-region-p)
      (setq start (region-beginning)
            end (region-end))
    (setq start (point-min)
          end (point-max)))
  (let (invalid-urls)
    (with-silent-modifications
      (save-excursion
        (goto-char start)
        (while (re-search-forward verify-url/regex end t)
          (let* ((url (match-string 0 ))
                 (beg (match-beginning 0))
                 (end (match-end 0)))
            (unless (verify-url--url-readable-p url)
              (push url invalid-urls)
              (remove-overlays beg end)
              (verify-url--make-invalid-url-overlay beg end)))))
      (when invalid-urls
        (message "verify-url(s):\n%s" (mapconcat #'identity invalid-urls "\n")))
      (when (and verify-url/auto-jump-to-first-invalid-url
                 invalid-urls)
        (verify-url/next-invalid-url (point-min))))))

(defun verify-url--find-invalid-url-overlays (start end)
  "find out invalid-url-overlays between START and END"
  (let ((overlays (overlays-in start end)))
    (cl-remove-if-not (lambda (o)
                        (not (verify-url--invalid-url-overlay-p o)))
                      overlays)))

(defun verify-url--invalid-url-overlay-at (pos)
  "returns invalid-url-overlay that cover the character at position POS"
  (let ((overlays (overlays-at pos)))
    (cl-find-if #'verify-url--invalid-url-overlay-p overlays)))

(defun verify-url/next-invalid-url (&optional pos)
  "goto next invalid-url after POS which is default to (point). if none,returns (point-max)"
  (interactive)
  (let* ((pos (or pos (point)))
         (overlay (verify-url--invalid-url-overlay-at pos)))
    (when overlay
      (setq pos (overlay-end overlay)))
    (while (not (or (verify-url--invalid-url-overlay-at pos)
                    (equal pos (point-max))))
      (setq pos (next-overlay-change pos)))
    (goto-char pos)))

(defun verify-url/previous-invalid-url (&optional pos)
  "goto next invalid-url before POS which is default to (point). if none,returns (point-min)"
  (interactive)
  (let* ((pos (or pos (point)))
         (overlay (verify-url--invalid-url-overlay-at pos)))
    (when overlay
      (setq pos (overlay-start overlay))
      (unless (equal (point-min) pos)
        (setq pos (- pos 1))))
    (while (not (or (verify-url--invalid-url-overlay-at pos)
                    (equal pos (point-min))))
      (setq pos (previous-overlay-change pos)))
    (goto-char pos)))

(provide 'verify-url)

;;; verify-url.el ends here
