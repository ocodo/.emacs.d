;;; evil-search-highlight-persist.el --- Persistent highlights after search
;; Version: 20150107.4
;; Package-Version: 20150216.154
;; X-Original-Version: 20140918

;; Author: Juanjo Alvarez <juanjo@juanjoalvarez.net>
;; Created:  September 18, 2014
;; Package-Requires: ((highlight "0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139,
;; USA.

;;; Commentary:
;;
;; This extension will make isearch and evil-ex-search-incremental to
;; highlight the search term (taken as a regexp) in all the buffer and
;; persistently until you make another search or clear the highlights
;; with the evil-search-highlight-persist-remove-all command (default
;; binding to C-x SPC). This is how Vim search works by default when
;; you enable hlsearch.
;;
;; To enable:
;;
;; (require 'evil-search-highlight-persist)
;; (global-evil-search-highlight-persist t)


;;; Code:

;;; User Customizable Variables:
(require 'advice)
(require 'highlight)

(defgroup evil-search-highlight-persist nil
  "evil-search-highlight-persist -- Search Highlight Remain, Vim's style"
  :tag "SearchHighlightPersist"
  :group 'environment)


(defface evil-search-highlight-persist-highlight-face
  '((((class color))
     (:background "yellow1")))
  "Face for the highlighted text."
  :group 'evil-search-highlight-persist)


(defun evil-search-highlight-persist-remove-all ()
  (interactive)
  (hlt-unhighlight-region-in-buffers (list (current-buffer))))

(defun evil-search-highlight-persist-mark ()
  (let ((hlt-use-overlays-flag t)
        (hlt-last-face 'evil-search-highlight-persist-highlight-face))
    (hlt-highlight-regexp-region-in-buffers
     (car-safe (if isearch-regexp
                   regexp-search-ring
                 search-ring))
     (list (current-buffer)))))

(defadvice isearch-exit (after isearch--highlight-persist)
  (evil-search-highlight-persist-remove-all)
  (evil-search-highlight-persist-mark))

(defadvice evil-flash-search-pattern (after evil-flash-search--highlight-persist)
  (evil-search-highlight-persist-remove-all)
  (evil-search-highlight-persist-mark))

;;;###autoload
(define-minor-mode evil-search-highlight-persist
 "Keep the highlights persist after a search"
 :keymap (let ((map (make-sparse-keymap)))
           (define-key map (kbd "C-x SPC") 'evil-search-highlight-persist-remove-all)
            map)
 (if evil-search-highlight-persist
   (progn
      (ad-activate 'isearch-exit)
      (ad-activate 'evil-flash-search-pattern))
   (progn
      (evil-search-highlight-persist-remove-all)
      (ad-deactivate 'isearch-exit)
      (ad-deactivate 'evil-flash-search-pattern))))


;;;###autoload
(defun turn-on-search-highlight-persist ()
  "Enable search-highlight-persist in the current buffer."
  (evil-search-highlight-persist 1))

;;;###autoload
(defun turn-off-search-highlight-persist ()
  "Disable evil-search-highlight-persist in the current buffer."
  (evil-search-highlight-persist -1))


;;;###autoload
(define-globalized-minor-mode global-evil-search-highlight-persist
  evil-search-highlight-persist turn-on-search-highlight-persist)

;; * provide
(provide 'evil-search-highlight-persist)
;;; evil-search-highlight-persist.el ends here
