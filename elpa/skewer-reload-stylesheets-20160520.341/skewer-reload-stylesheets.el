;;; skewer-reload-stylesheets.el --- live-edit CSS stylesheets.

;; This is free and unencumbered software released into the public domain.

;; Author: Nate Eagleson <nate@nateeag.com>
;; Created: November 23, 2013
;; Package-Requires: ((skewer-mode "1.5.3"))
;; Version: 0.1.0

;;; Commentary:

;; This minor mode provides live-editing of CSS stylesheets via skewer.

;; skewer-css works for many cases, but if you're dealing with multiple
;; stylesheets and involved cascading (a.k.a. "legacy code"), it isn't so
;; useful. What you see while live-editing is not what you see when you
;; refresh, since skewer-css puts the updated CSS in new style tags.

;; Enter this minor mode.

;; It refreshes stylesheets on save by adding (or updating) a query string to
;; the current buffer's link tag in the browser.

;; Thus, what you see on a fresh pageload is always exactly what you see while
;; live-editing.

;;; Setup

;; Put the following in your css-mode-hook:
;;
;;     (skewer-reload-stylesheets-mode)
;;     (skewer-reload-stylesheets-reload-on-save)

;;; Usage

;; Start skewer. Open browser windows for the URLs whose CSS you want to
;; live-edit and skewer those windows.

;; Open the stylesheet(s) you want to work in.

;; Make some changes in a stylesheet and save it. The updates will immediately
;; be reflected in the skewered windows.

;; and there you are - cross-browser live-editing for arbitrarily complex
;; stylesheets.

;; Note that browser plugins like
;; [Custom Javascript for Websites](https://chrome.google.com/webstore/detail/custom-javascript-for-web/poakhlngfciodnhlhhgnaaelnpjljija?hl=en)
;; make it easy to auto-skewer URLs on pageload, so you don't have to re-skewer
;; after every refresh.

;; Key bindings:

;; * C-x C-r -- `skewer-reload-stylesheets-reload-buffer`
;; Note that this keybinding is deprecated, as current usage reloads
;; stylesheets with an after-save-hook, so there is no need for a custom
;; keybinding.

;;; Code:
(require 'skewer-mode)

(defvar skewer-reload-stylesheets-data-root (file-name-directory load-file-name)
  "Location of data files needed by skewer-reload-stylesheets-mode.")

(defun skewer-reload-stylesheets-reload-buffer ()
  "Save current buffer and ask skewer to reload it."

  (declare (obsolete skewer-reload-stylesheets-reload-on-save "0.1.0"))

  (interactive)
  (save-buffer)

  (skewer-reload-stylesheets-reload))

(defun skewer-reload-stylesheets-reload ()
  "Ask browser to reload the stylesheet for the current buffer."

  ;; TODO I tried to use skewer-apply, but it said skewer.reloadStylesheet was
  ;; not a valid function.
  (skewer-eval (concat "skewer.reloadStylesheet(\"" (buffer-file-name) "\");")))

(defun skewer-reload-stylesheets-reload-on-save ()
  "Ask skewer to reload stylesheets immediately after save.

Call this in your css-mode-hook to automatically reload stylesheets on save."

  (add-hook 'after-save-hook
            'skewer-reload-stylesheets-reload
            nil
            t))

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
