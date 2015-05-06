;;; buffer-combine.el --- Display parts of other buffers in one display buffer
;;
;; Copyright (C) 2012 Sheldon McGrandle
;;
;; Author: Sheldon McGrandle <developer@rednemesis.com>
;; Version: 0.1
;; Created: 12th April 2013
;; Keywords: utility
;; URL: https://github.com/smmcg/showcss-mode
;;
;; This file is not part of GNU Emacs.
;;
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; author of this program or to the Free Software Foundation, 675 Mass
;; Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; DESCRIPTION AND USAGE
;;
;; (let ((buf (generate-new-buffer "Some name for display")))
;;   (set-buffer buf)         ;create a new buffer and set focus to it
;;   (buffer-combine-mode)    ;turn on buffer combine mode
;;   (bc/start data :readonly nil :hidden nil))  ;send it the data
;;
;; Data structure bc/start requires:
;; '((filename-or-buffer (start end) (start end) ...)
;;   (another-filename-or-buffer (start end) ...)
;;   (...))
;;
;; INSTALLATION
;;
;; BUGS


;;; Code:

(require 'cl-lib)
(require 's)


(defvar bc/buffers-data nil
  "data format:
'((filename overlay1 overlay2 overlay3)
  (anotherfile overlay1 overlay2))")
;(make-local-variable bc/buffers-data)

(defvar bc/this-buffer nil)
;(make-local-variable bc/this-buffer)


(defun* bc/start (data parents &key (readonly nil) (hidden nil))
  "Recieve and parse the data
two optional flags readonly and hidden"
  (set-buffer (get-buffer-create "Show CSS"))
  (add-hook 'kill-buffer-hook 'bc/remove-source-overlays nil t)
  (setq bc/this-buffer (current-buffer))
  (eval (read (format "(%s)" showcss/display-buffer-mode)))
  (bc/remove-source-overlays)
  (let ((buffers-data '()))
    (dolist (filelist data)
      ;;for each file and its fragment positions:
      (let ((buffer (bc/load-file (car filelist) hidden)))

        ;;for each fragment position:
        (setq buffers-data
              (cons (bc/mark-fragments-in-source buffer (cdr filelist))
                    buffers-data))));)
    (setq bc/buffers-data buffers-data)
    (bc/build-display buffers-data parents))
  (display-buffer bc/this-buffer))


;; rewrite so this function assumes a buffer
;; only, instead of a buffer or file.
(defun bc/load-file (file hidden)
  "Load the files from disk and if hidden is t,
rename them with a space in front of the buffer title"
  (let ((return-buffer nil))
    ;; if file is actually a buffer, do nothing
    (if (bufferp file)
        ;; return: <file>
        (setq return-buffer file)
      ;; since file is a string, see if its
      ;; already loaded in a buffer
      (dolist (b (buffer-list))
        (if (string= (buffer-file-name b) (file-truename file))
            (setq return-buffer b))))
    ;; since file is not a buffer and its not
    ;; already loaded, load it now
    (if (not return-buffer)
        (setq return-buffer (find-file-noselect file)))

    (set-buffer return-buffer)
    (if hidden
        (rename-buffer (concat " " (s-trim-left (buffer-name))))
      ;; if file is *not* to be hidden but it is already,
      ;; unhide it by removing the space at the begining.
      (if (s-starts-with? " " (buffer-name))
          (rename-buffer (s-trim-left (buffer-name)))))

    ;;return: <buffer handle>
    return-buffer))


(defun bc/mark-fragments-in-source(buffer fragments)
  "Iterate over all the fragments in one buffer"
  (set-buffer buffer)
  ;;(remove-overlays)
  (add-hook 'first-change-hook 'bc/set-modification-flag nil t)
  (add-hook 'after-save-hook 'bc/unset-modification-flag nil t)

  (let ((buffer-overlay-list (cons buffer '())))
    (dolist (fragment fragments)
      (setq buffer-overlay-list
            (cons
             (bc/mark-fragment-in-source
              buffer
              (nth 0 fragment)   ;start
              (nth 1 fragment))  ;end
             buffer-overlay-list)))
    (reverse buffer-overlay-list)))


(defun bc/set-modification-flag(&optional beginning end length)
  (bc/change-modification-flag 'on))

(defun bc/unset-modification-flag()
  (bc/change-modification-flag 'off))

(defun bc/change-modification-flag(state &optional bname)
  "Set a modification flag if the source buffer has changed"
  ;(save-excursion
    (let ((source-buffer (if bname bname (buffer-name))))
      (set-buffer bc/this-buffer)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (if (string= (overlay-get ov 'filename) source-buffer)
          (progn
            (goto-char (overlay-start ov))
            (if (eq state 'on)
                (if (not (looking-at "*"))
                    (insert "*"))
              (if (looking-at "*")
                  (delete-char 1))))))));)


(defun bc/mark-fragment-in-source (buffer start end)
  "Mark a fragment in a buffer with an overlay"
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face 'showcss/source-region-face)
    (overlay-put ov 'before-string
                 "Changes made in the following region will be
|overwritten by edits made in the Show CSS buffer:\n")
    (overlay-put ov 'line-prefix "|")
    (overlay-put ov 'wrap-prefix "|")

    ;; return <ov>:
    ov))


(defun bc/build-display(buffers-data parents)
  "Build the display for each fragment"
  (set-buffer bc/this-buffer)
  (remove-overlays)
  (erase-buffer)

  ;; Insert breadcrumb
  (insert (showcss/build-breadcrumb parents))

  ;; Insert file names
  (let ((dupe-list nil))
    (dolist (file-and-overlays buffers-data)
      (let* ((buf (car file-and-overlays))
             (header-ov nil)
             (header-path-ov nil)
             (path (file-name-directory (buffer-file-name buf)))
             (save-point nil))

        (unless (memq (buffer-file-name buf) dupe-list)

          (insert (format "\n%s" (file-name-nondirectory
                                  (buffer-file-name buf))))
          (setq header-ov (make-overlay
                           (progn (move-beginning-of-line nil) (point))
                           (progn (move-end-of-line nil) (point)) nil nil nil))
          (overlay-put header-ov 'face 'showcss/header-face)
          ;; this is here so it can be easily found
          (overlay-put header-ov 'filename (file-name-nondirectory
                                            (buffer-file-name buf)))
          (insert "  ")
          ;; view button
          (insert-text-button
           "View"
           'action (lambda (button)
                     (switch-to-buffer
                      (button-get button 'source-buffer) nil t))
           'source-buffer buf
           'follow-link t
           'help-echo (format "Switch to %s"
                              (file-name-nondirectory (buffer-file-name buf))))
          (insert "  ")
          ;; save button
          (insert-text-button
           "Save"
           'action (lambda (button)
                     (set-buffer (button-get button 'source-buffer)) (save-buffer))
           'source-buffer buf
           'follow-link t
           'help-echo (format "Save %s"
                              (file-name-nondirectory (buffer-file-name buf))))
          (insert "\n")
          ;; insert file path
          (setq save-point (point))
          (insert path)
          (setq header-path-ov (make-overlay save-point (point)))
          (overlay-put header-path-ov 'face 'showcss/header-filepath-face)

                                        ;(insert "\n"))

          ;; add the file name to the duplicate list
          (setq dupe-list (cons (buffer-file-name buf) dupe-list))
          )

        (insert "\n")

        ;; clear duplicate list so it can be used with the fragments
        (setq dupe-list nil)

        ;; Insert css
        (dolist (source-ov (cdr file-and-overlays))
          (let* ((source-start (overlay-start source-ov))
                 (source-end (overlay-end source-ov))
                 (display-length (- source-end source-start)))
            ;; insert css fragments
            (insert-buffer-substring-no-properties
             buf
             (overlay-start source-ov)
             (overlay-end source-ov))
            (let ((display-ov (make-overlay
                               (point) (- (point) display-length) nil nil nil)))
              (overlay-put display-ov 'before-string "\n")
              (overlay-put display-ov 'modification-hooks
                           '(bc/send-back-to-source))
              (overlay-put display-ov 'source-overlay source-ov)
              (overlay-put display-ov 'face 'showcss/region-face)
              (overlay-put display-ov 'line-prefix " ")
              ;; if the fragment doesn't end in a newline, add one to the display
              (unless (string= (string (char-before (overlay-end display-ov))) "\n")
                (insert "\n")))
            )))))
        (goto-char (point-min)))


(defun bc/send-back-to-source(ov &optional flag &rest rv)
  "Any edits made in the display buffer are sent back to the
linked overlay in the source buffer"
  (if flag
      (progn
        (let ((here (point)))
          (bc/change-modification-flag
           'on
           (buffer-name (overlay-buffer (overlay-get ov 'source-overlay))))
          (goto-char here))
        (let*
            ((source-ov (overlay-get ov 'source-overlay))
             (source-start (overlay-start source-ov))
             (source-end (overlay-end source-ov))
             (display-start (overlay-start ov))
             (display-end (overlay-end ov))
                  (content (buffer-substring-no-properties
                            display-start display-end)))
          (set-buffer (overlay-buffer source-ov))
          (goto-char source-start)
          (insert content)
          (delete-region (point) (overlay-end source-ov))

         ;; set modification flag

         ))))


(defun bc/save-source-buffers()
  (interactive)
  "Save all the source buffers"
  (message "saving source buffers")
  (dolist (buffer-and-fragments bc/buffers-data)
    (let ((buffer (car buffer-and-fragments)))
      (set-buffer buffer)
      (save-buffer)
  )))


(defun bc/remove-source-overlays()
  "Remove all the overlays from the source buffers"
  (dolist (buffer-and-fragments bc/buffers-data)
    (let ((buffer (car buffer-and-fragments)))
      (if buffer
          (progn
            (set-buffer buffer)
            (remove-hook 'first-change-hook 'bc/set-modification-flag)
            (remove-hook 'after-save-hook 'bc/unset-modification-flag)
            (dolist (ov (cdr buffer-and-fragments))
              (delete-overlay ov))

            ;; temporary only.  This will delete ALL overlays
            ;; even if set from another mode.
            (dolist (ov (overlays-in (point-min) (point-max)))
              (delete-overlay ov))

            )))))


(defvar buffer-combine-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (read-kbd-macro "C-x C-s") 'bc/save-source-buffers)
    map)
  "some documentation")


;; derive from css-mode, sass-mode?
;;;;(define-derived-mode buffer-combine-mode css-mode "Combine"
;;;;  "Display fragments from other buffers
;;;;\\{buffer-combine-mode-map}"
;;;;
;;;;  (setq bc/this-buffer (current-buffer))
;;;;  (add-hook 'kill-buffer-hook 'bc/remove-source-overlays nil t))


;  (if buffer-combine-mode
;      (progn
;        (setq bc/this-buffer (current-buffer))
;        (add-hook 'kill-buffer-hook 'bc/remove-source-overlays nil t))
;    (bc/remove-source-overlays))
;)


(provide 'buffer-combine)

;;; buffer-combine.el ends here
