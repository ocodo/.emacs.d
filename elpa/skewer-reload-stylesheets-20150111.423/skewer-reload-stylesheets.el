;;; skewer-reload-stylesheets.el --- live-edit CSS stylesheets.

;; This is free and unencumbered software released into the public domain.

;;; Author: Nate Eagleson <nate@nateeag.com>
;;; Created: November 23, 2013
;;; Package-Requires: ((skewer-mode "1.5.3"))
;;; Version: 0.0.1

;;; Commentary:

;; This minor mode provides live-editing of CSS stylesheets via skewer.
;; skewer-css works for many cases, but if you're dealing with multiple
;; stylesheets and involved cascading (a.k.a. "legacy code"), it isn't so
;; useful. What you see while live-editing is not what you see when you
;; refresh.

;; Enter this minor mode.

;; Start skewer (see its docs for how) then skewer the browser window you want
;; to live-edit.

;; Next, open a CSS file used on the skewered page, and activate this mode.
;; Make some edits then press `C-x C-r`. The stylesheet will be saved, and the
;; browser will reload it from disk, by removing its link tag from the DOM then
;; re-inserting it.

;; and there you are - cross-browser live-editing for arbitrarily complex
;; stylesheets.

;; Key bindings:

;; * C-x C-r -- `skewer-reload-stylesheets-reload-buffer`

;;; Code:
(require 'skewer-mode)

(defvar skewer-reload-stylesheets-data-root (file-name-directory load-file-name)
  "Location of data files needed by skewer-reload-stylesheets-mode.")

(defun skewer-reload-stylesheets-reload-buffer ()
  "Reload the current buffer IF it is already included as a link tag."
  (interactive)
  (save-buffer)

  ;; TODO I tried to use skewer-apply, but it said skewer.reloadStylesheet was
  ;; not a valid function.
  (skewer-eval (concat "skewer.reloadStylesheet(\"" (buffer-file-name) "\");")))

(defun skewer-reload-stylesheets-skewer-js-hook ()
  "Skewer hook function to insert JS for reloading CSS files."
  (insert-file-contents
   (expand-file-name "skewer-reload-stylesheets.js" skewer-reload-stylesheets-data-root)))

(add-hook 'skewer-js-hook 'skewer-reload-stylesheets-skewer-js-hook)

;; Minor mode definition

(defvar skewer-reload-stylesheets-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-x C-r") 'skewer-reload-stylesheets-reload-buffer)))
  "Keymap for skewer-reload-stylesheets-mode.")

;;;###autoload
(define-minor-mode skewer-reload-stylesheets-mode
  "Minor mode for interactively reloading CSS stylesheets."
  :lighter " reload-ss"
  :keymap skewer-reload-stylesheets-mode-map
  :group 'skewer)

(provide 'skewer-reload-stylesheets)
;;; skewer-reload-stylesheets.el ends here
