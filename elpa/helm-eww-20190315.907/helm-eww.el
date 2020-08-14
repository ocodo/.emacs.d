;;; helm-eww.el --- Helm UI wrapper for EWW. -*- lexical-binding: t -*-

;; Copyright (C) 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-eww
;; Package-Version: 20190315.907
;; Package-Commit: 76ba59fda8dd6f32a1bc7c6df0b43c6f76169911
;; Version: 1.2
;; Package-Requires: ((emacs "24.4") (helm "2.8.6") (seq "1.8"))
;; Keywords: helm, packages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Helm UI wrapper for EWW, the Emacs Web Wowser.

;;; Code:
(require 'helm)
(require 'helm-buffers)
(require 'helm-utils)
(require 'eww)
(require 'thingatpt)

(defvar helm-eww-buffer-max-length 51
  "Max length of EWW buffer names before truncating.
When disabled (nil) use the longest `buffer-name' length found.

See `helm-buffer-max-length`.  This variable's default is so that
the EWW title starts at the column of the open parenthesis in
`helm-buffers-list' detailed view.")

(defun helm-eww-buffer-length ()
  "Return the dynamic length of EWW buffer names.
It depends on the width of the current Helm window.
It won't shrink under `helm-eww-buffer-max-length'."
  (max helm-eww-buffer-max-length
       (/ (with-helm-window (window-body-width)) 2)))

(defun helm-eww-toggle-buffers-details ()
  (interactive)
  (with-helm-alive-p
    (let* ((buf (helm-get-selection))
           ;; `helm-buffer--get-preselection' uses `helm-buffer-max-length'.
           (helm-buffer-max-length (helm-eww-buffer-length))
           (preselect (and (bufferp buf) (helm-buffer--get-preselection buf))))
      (setq helm-buffer-details-flag (not helm-buffer-details-flag))
      ;; TODO: `helm-force-update' seems to be necessary to be necessary to
      ;; update the buffer live.  It is not the case for helm-buffers-list
      ;; though.  Why?
      (if (bufferp buf)
          (helm-force-update (lambda ()
                               (helm-awhile (re-search-forward preselect nil t)
                                 (helm-mark-current-line)
                                 (when (equal buf (helm-get-selection))
                                   (cl-return t)))))
        (helm-force-update)))))
(put 'helm-eww-toggle-buffers-details 'helm-only t)

(defun helm-eww-new-buffer (&optional url)
  "Fetch URL and render the page in a new buffer.
If the input doesn't look like an URL or a domain name, the
word(s) will be searched for via `eww-search-prefix'."
  (let ((b (generate-new-buffer "*eww*"))
        (url-at-point (thing-at-point-url-at-point)))
    (save-window-excursion
      (with-current-buffer b
        (eww-mode)
        (eww (or (and url (not (string= "" url)) url)
                 url-at-point
                 ""))))
    b))

(defun helm-eww-switch-buffers (_candidate)
  "Open marked URL(s) in EWW.
If more than one URL is marked, or with prefix argument, open in
new buffer."
  (interactive)
  (let ((c (helm-marked-candidates)))
    (if (and (= 1 (length c))
             (null helm-current-prefix-arg))
        (eww (car c))
      (helm-window-show-buffers (mapcar 'helm-eww-new-buffer c)))))
(put 'helm-eww-switch-buffers 'helm-only t)

(defun helm-eww-switch-other-window (_candidate)
  "Open marked URL(s) in other windows."
  (interactive)
  (helm-window-show-buffers (mapcar 'helm-eww-new-buffer (helm-marked-candidates)) t))
(put 'helm-eww-switch-other-window 'helm-only t)

(defun helm-eww-switch-other-frame (candidate)
  "Open URL of marked CANDIDATE in other frame."
  (interactive)
  (switch-to-buffer-other-frame (helm-eww-new-buffer candidate)))
(put 'helm-eww-switch-other-frame 'helm-only t)

(defun helm-eww-browser-with-external-browser (_candidates)
  "Like `eww-browse-with-external-browser' with marked URL(s)."
  (dolist (c (helm-marked-candidates))
    (eww-browse-with-external-browser
     (if (bufferp c)
         (with-current-buffer c
           (plist-get eww-data :url))
       c))))
(put 'helm-eww-browser-with-external-browser 'helm-only t)


(defvar helm-eww-buffers-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-buffer-switch-other-window)
    (define-key map (kbd "C-c C-o") 'helm-buffer-switch-other-frame)
    (define-key map (kbd "M-D") 'helm-buffer-run-kill-buffers)
    (define-key map (kbd "C-]") 'helm-eww-toggle-buffers-details)
    map)
  "Keymap for browser source in Helm.")

;; Inspired by `helm-highlight-buffers'.
(defun helm-eww-highlight-buffers (buffers)
  "Transformer function to highlight EWW BUFFERS."
  (cl-loop for i in buffers
           ;; Warning: URL can be missing.
           for (url title) = (with-current-buffer i (list (or (eww-current-url) "")
                                                          (plist-get eww-data :title)))
           for truncbuf = (if (> (string-width url) (helm-eww-buffer-length))
                              (helm-substring-by-width
                               url (helm-eww-buffer-length)
                               helm-buffers-end-truncated-string)
                            (concat url
                                    (make-string
                                     (- (+ (helm-eww-buffer-length)
                                           (length helm-buffers-end-truncated-string))
                                        (string-width url))
                                     ? )))
           collect (let ((helm-pattern (helm-buffers--pattern-sans-filters
                                        (and helm-buffers-fuzzy-matching ""))))
                     (cons (if helm-buffer-details-flag
                               (concat
                                (funcall helm-fuzzy-matching-highlight-fn truncbuf)
                                "  " (propertize title 'face 'helm-buffer-process))
                             (funcall helm-fuzzy-matching-highlight-fn url))
                           (get-buffer i)))))

(defun helm-eww-buffers-build-source ()
  "Build source for EWW buffers.
See `helm-eww' for more details."
  (helm-build-sync-source "EWW buffers"
    :candidates (seq-filter (lambda (b) (with-current-buffer b (derived-mode-p 'eww-mode))) (buffer-list))
    :candidate-transformer 'helm-eww-highlight-buffers
    :action `(("Switch to buffer(s)" . helm-buffer-switch-buffers)
              ("Open URL(s) in external browser" . helm-eww-browser-with-external-browser)
              (,(substitute-command-keys "Switch to buffer(s) in other window \\<helm-eww-buffers-map>`\\[helm-buffer-switch-other-window]'")
               . helm-buffer-switch-buffers-other-window)
              (,(substitute-command-keys "Switch to buffer in other frame \\<helm-eww-buffers-map>`\\[helm-buffer-switch-other-frame]'")
               . switch-to-buffer-other-frame)
              (,(substitute-command-keys "Kill buffer(s) \\<helm-eww-buffers-map>`\\[helm-buffer-run-kill-buffers]'")
               . helm-kill-marked-buffers))
    ;; When follow-mode is on, the persistent-action allows for multiple candidate selection.
    :persistent-action 'helm-buffers-list-persistent-action
    :keymap helm-eww-buffers-map))
(add-to-list 'helm-source-names-using-follow "EWW buffers")

;;;###autoload
(defun helm-eww-buffers ()
  "Preconfigured `helm' to list EWW buffers."
  (interactive)
  (helm :sources (helm-eww-buffers-build-source)
        :truncate-lines t
        :buffer "*helm-eww-buffers*"))


(defvar helm-eww-bookmarks-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-eww-switch-other-window)
    (define-key map (kbd "C-c C-o") 'helm-eww-switch-other-frame)
    (define-key map (kbd "M-D") 'helm-eww-bookmarks-delete)
    (define-key map (kbd "C-]") 'helm-eww-toggle-buffers-details)
    map)
  "Keymap for bookmarks source in Helm.")

(defun helm-eww-highlight-bookmarks (candidates)
  "Transformer function to highlight CANDIDATES list.
Each candidate is a list of (URL TITLE)."
  (cl-loop for (url title) in candidates
           for truncbuf = (if (> (string-width url) (helm-eww-buffer-length))
                              (helm-substring-by-width
                               url (helm-eww-buffer-length)
                               helm-buffers-end-truncated-string)
                            (concat url
                                    (make-string
                                     (- (+ (helm-eww-buffer-length)
                                           (length helm-buffers-end-truncated-string))
                                        (string-width url))
                                     ? )))
           collect (let ((helm-pattern (helm-buffers--pattern-sans-filters
                                        (and helm-buffers-fuzzy-matching ""))))
                     (cons (if helm-buffer-details-flag
                               (concat
                                (funcall helm-fuzzy-matching-highlight-fn truncbuf)
                                "  " (propertize title 'face 'helm-buffer-process))
                             (funcall helm-fuzzy-matching-highlight-fn url))
                           url))))

(defun helm-eww-bookmarks-delete (&optional _candidate)
  "Delete all bookmarks with the URLs of the marked candidates."
  (interactive)
  (dolist (c (helm-marked-candidates))
    (setq eww-bookmarks (seq-remove (lambda (b) (string= (plist-get b :url) c))
                                    eww-bookmarks)))
  (eww-write-bookmarks))
(put 'helm-eww-bookmarks-delete 'helm-only t)

(defun helm-eww-bookmarks-build-source ()
  "Build source for EWW bookmarks.
See `helm-eww-bookmarks' for more details."
  ;; Bookmarks are only first loaded when `eww-bookmark-prepare' is called.
  ;; This can be too late for us, so we do it here.
  (unless eww-bookmarks
    (eww-read-bookmarks))
  (helm-build-sync-source "EWW bookmarks"
    :candidates (mapcar (lambda (e) (list (plist-get e :url) (plist-get e :title))) eww-bookmarks)
    :candidate-transformer 'helm-eww-highlight-bookmarks
    :candidate-number-limit 1000
    :action `(("Open URL(s)" . helm-eww-switch-buffers)
              ("Open URL(s) in external browser" . helm-eww-browser-with-external-browser)
              (,(substitute-command-keys "Open URL(s) in other window \\<helm-eww-bookmarks-map>`\\[helm-eww-switch-other-window]'")
               . helm-eww-switch-other-window)
              (,(substitute-command-keys "Open URL in other frame \\<helm-eww-bookmarks-map>`\\[helm-eww-switch-other-frame]'")
               . helm-eww-switch-other-frame)
              (,(substitute-command-keys "Delete bookmark(s) \\<helm-eww-bookmarks-map>`\\[helm-eww-bookmarks-delete]'")
               . helm-eww-bookmarks-delete))
    :keymap helm-eww-bookmarks-map))

;;;###autoload
(defun helm-eww-bookmarks ()
  "Preconfigured `helm' to list EWW bookmarks."
  (interactive)
  (helm :sources (helm-eww-bookmarks-build-source)
        :truncate-lines t
        :buffer "*helm-eww-bookmarks*"))


(defvar helm-eww-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-]") 'helm-eww-toggle-buffers-details)
    map)
  "Keymap for EWW history source in Helm.")

(defun helm-eww-highlight-history (candidates)
  "Transformer function to highlight CANDIDATES list.
CANDIDATES are `eww-history' elements."
  (cl-loop for e in candidates
           for (url title) = (list (plist-get e :url) (plist-get e :title))
           for truncbuf = (if (> (string-width url) (helm-eww-buffer-length))
                              (helm-substring-by-width
                               url (helm-eww-buffer-length)
                               helm-buffers-end-truncated-string)
                            (concat url
                                    (make-string
                                     (- (+ (helm-eww-buffer-length)
                                           (length helm-buffers-end-truncated-string))
                                        (string-width url))
                                     ? )))
           collect (let ((helm-pattern (helm-buffers--pattern-sans-filters
                                        (and helm-buffers-fuzzy-matching ""))))
                     (cons (if helm-buffer-details-flag
                               (concat
                                (funcall helm-fuzzy-matching-highlight-fn truncbuf)
                                "  " (propertize title 'face 'helm-buffer-process))
                             (funcall helm-fuzzy-matching-highlight-fn url))
                           e))))

(defun helm-eww-history-build-source ()
  "Build source for EWW history.
See `helm-eww-bookmarks' for more details."
  (helm-build-sync-source "EWW history"
    :candidates eww-history
    :candidate-transformer 'helm-eww-highlight-history
    :candidate-number-limit 1000
    :nomark t      ; TODO: Enable marking and use current candidate only for `eww-restore-history'.
    :action '(("Go back to" . eww-restore-history)
              ;; ("Open URI (s) in other window `C-c o'" . helm-buffer-switch-buffers-other-window)
              ;; ("Switch to buffer in other frame `C-c C-o'" . switch-to-buffer-other-frame)
              ;; ("Kill buffer(s) `M-D`" . helm-kill-marked-buffers) ; TODO: Kill action?
              )
    ;; TODO: Persistent action that does not change history.  Need to fix `eww-forward-url' upstream first.
    :keymap helm-eww-history-map))

;;;###autoload
(defun helm-eww-history ()
  "Preconfigured `helm' to list EWW history."
  (interactive)
  (unless (derived-mode-p 'eww-mode)
    (user-error "Not a EWW buffer"))
  (helm :sources (list (helm-eww-history-build-source))
        :truncate-lines t
        :buffer "*helm-eww-history*"))


(defvar helm-eww-new
  (helm-build-dummy-source "Open new page (empty for URL at point)"
    :action (helm-make-actions
             "Open new page" 'helm-eww-new-buffer
             "Open new page externally" 'helm-eww-browser-with-external-browser
             "Open new page in new window" 'helm-eww-switch-other-window
             "Open new page in new frame" 'helm-eww-switch-other-frame)))

;; All in one.
;;;###autoload
(defun helm-eww ()
  "Preconfigured `helm' to list all EWW sources."
  (interactive)
  (helm :sources (list 'helm-eww-new
                       (helm-eww-buffers-build-source)
                       ;; TODO: Can we add history as well?  It would make sense if we only allow history entries in new buffers.
                       ;; (helm-eww-history-build-source)
                       (helm-eww-bookmarks-build-source))
        :truncate-lines t
        :buffer "*helm-eww*"))

(provide 'helm-eww)
;;; helm-eww.el ends here
