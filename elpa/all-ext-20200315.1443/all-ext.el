;;; all-ext.el --- M-x all with helm-swoop/anything/multiple-cursors/line-number

;; Copyright (C) 2013, 2016, rubikitch, all rights reserved.

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; Created: 2013-01-31 16:05:17
;; Keywords: matching all search replace anything helm helm-swoop occur
;; Package-Version: 20200315.1443
;; Package-Commit: c865c62506af2c9edc7705a7c24dc8b70d5d4de2
;; Version: 0.5
;; URL: https://github.com/rubikitch/all-ext
;; Package-Requires: ((emacs "24.4") (all "1.0"))

;; Features that might be required by this library:

;; `all', `anything', `helm', `helm-swoop', `multiple-cursors'


;;; This file is NOT part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Extend M-x all (older than M-x occur-edit-mode) to be replacement of it.
;;   - Show line number before line content (using overlay)
;;   - Can navigate with M-x next-error / M-x previous-error
;;   - Use C-x h in *All* to get all matched lines.

;; Call M-x all from anything/helm:
;;   1. Call anything/helm command showing lineno and content
;;      such as M-x anything-occur / anything-browse-code /
;;              helm-occur / helm-swoop(RECOMMENDED!) etc
;;   2. Press C-c C-a to show anything/helm contents into *All* buffer
;;   3. You can edit *All* buffer!

;; Multiple-cursors in *All*:
;;   - Press C-c C-m sets one cursor to all lines in *All* buffer.
;;   - If you use helm-swoop, set multiple cursors at the first match!
;;     - Otherwise, they are at the beginning of line.  (limitation)

;; *All* is undo-able!

;;; Installation:

;; Put all-ext.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:

;;   (add-to-list 'load-path (expand-file-name "~/elisp"))

;; And the following to your ~/.emacs startup file.

;;   (require 'all-ext)

;; No need more.

;;; Code:

(require 'all)
(require 'cl-lib)

(defgroup all nil
  "Listing and editing matching lines."
  :group 'matching)

(defcustom all-from-occur-select-window-flag t
  "Select *All* window from `helm-occur' or `anything-occur'."
  :type 'boolean
  :group 'all)


;;; anything/helm integration

;; Call `all' from anything/helm
(declare-function anything-run-after-quit "ext:anything")
(declare-function helm-run-after-exit "ext:helm")
(defvar anything-buffer)
(defvar anything-current-buffer)
(defvar helm-buffer)
(defvar helm-current-buffer)
(defvar anything-map)
(defvar helm-map)
(defvar helm-source-occur)

(with-eval-after-load "anything-config"
  (define-key anything-map (kbd "C-c C-a") 'all-from-anything-occur))

(defun all-from-anything-occur ()
  "Call `all' from `anything' content."
  (interactive)
  (anything-run-after-quit
   'all-from-anything-occur-internal "anything-occur"
   anything-buffer anything-current-buffer))

(with-eval-after-load "helm"
  (define-key helm-map (kbd "C-c C-a") 'all-from-helm-occur))

(with-eval-after-load "helm-regexp"
  (setq helm-source-occur
        (delete '(nomark) helm-source-occur)))

(defun all-from-helm-occur ()
  "Call `all' from `helm' content."
  (interactive)
  (helm-run-after-exit
   'all-from-anything-occur-internal "helm-occur"
   helm-buffer helm-current-buffer))

(defun all-from-anything-occur-internal (from anybuf srcbuf)
  (kill-All-buffer-maybe)
  (let ((all-initialization-p t)
        (buffer srcbuf)
        (marked-candidates)
        (tempbuf)
        (temp-buffer-show-function
         (and all-from-occur-select-window-flag
              ;; Use timer because `helm-swoop-line-overlay' remains!
              (lambda (b) (run-with-timer 0 nil 'pop-to-buffer b)))))
    (with-output-to-temp-buffer "*All*"
      (with-current-buffer standard-output
        (all-mode)
        (buffer-enable-undo)
        (setq all-buffer buffer)
        (insert "From " from "\n")
        (insert "--------\n"))
      (if (eq buffer standard-output)
          (goto-char (point-max)))
      (with-current-buffer anybuf
        (save-excursion
          (ignore-errors (delete-overlay helm-selection-overlay))
          (setq marked-candidates (mapcar (lambda (o) (overlay-get o 'string))
                                          (or (bound-and-true-p anything-visible-mark-overlays)
                                              (bound-and-true-p helm-visible-mark-overlays))))
          (goto-char (point-min))
          (forward-line 1)              ;ignore title line
          ;; Find next match, but give up if prev match was at end of buffer.
          (cl-loop with regexp = (format "^\\(%s:\\| *\\)\\([0-9]+\\)[ :]\\(.+\\)$"
                                         (buffer-name srcbuf))
                   while (re-search-forward regexp nil t)
                   for lineno = (string-to-number (match-string 2))
                   for content = (match-string 3)
                   ;; TODO make function
                   for match-beg = (save-excursion
                                     (goto-char (match-beginning 3))
                                     (cl-loop with pt
                                              while (and (setq pt (next-char-property-change (point)))
                                                         pt (< pt (point-at-eol)))
                                              do
                                              (goto-char pt)
                                              (when (eq (or (get-char-property (point) 'read-face-name)
                                                            (get-char-property (point) 'face))
                                                        'helm-swoop-target-word-face)
                                                (return (- (point) (match-beginning 3))))))
                   do
                   (when (or (null marked-candidates)
                             (member (buffer-substring (point-at-bol) (min (point-max) (1+ (point-at-eol)))) marked-candidates))
                     (with-current-buffer srcbuf
                       (save-excursion
                         (goto-char (point-min))
                         (goto-char (point-at-bol lineno))
                         (all-from-anything-occur-insert
                          (point) (progn (forward-line 1) (point)) lineno content match-beg)))))
          (when tempbuf (kill-buffer tempbuf)))))))

(defun all-from-anything-occur-insert (start end lineno content match-beg)
  (let ((marker (copy-marker start)))
    (with-current-buffer standard-output
      (let ((from (point)) to)
        (insert content "\n")
        (setq to (point))
        (goto-char from)
        (and match-beg
             (put-text-property (+ (point) match-beg) (+ (point) 1 match-beg)
                                'face 'match))
        (all-make-lineno-overlays-from-here to lineno)
        (goto-char to)
        (overlay-put (make-overlay from to) 'all-marker marker)))))

;;; `next-error' and `previous-error' from `all' (shoddy implementation)
(defun all-next-error (&optional argp reset)
  (let ((w (get-buffer-window "*All*")))
    (if (not w)
        (error "Cannot find *All* buffer window.")
      (with-selected-window w
        (when (= (point-at-bol) (point-min))
          (forward-line 1))
        (forward-line argp)
        (all-mode-goto)))))
(advice-add 'all-mode :after
            (lambda (&rest ignore) (setq next-error-function 'all-next-error)))


;;; multiple-cursors integration

(require 'multiple-cursors nil t)
(define-key all-mode-map (kbd "C-c C-m") 'mc/edit-lines-in-all)

(defun mc/edit-lines-in-all ()
  "Invoke `multiple-cursors' from *All*."
  (interactive)
  (goto-char (point-min))
  (search-forward "--------\n" nil t)   ;or (forward-line 2)
  (cl-loop with initpos
           while (not (eobp))
           for i from 0
           for match-beg = (text-property-any  (point)(point-max) 'face 'match)
           do
           (when match-beg
             (goto-char match-beg))
           (if (zerop i)
               (setq initpos (point))
             (mc/create-fake-cursor-at-point))
           (forward-line 1)
           finally (goto-char initpos))
  (multiple-cursors-mode))

(provide 'all-ext)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; all-ext.el ends here
