;;; skewer-reload-stylesheets.el --- live-edit CSS, SCSS, Less, and friends.

;; This is free and unencumbered software released into the public domain.

;; Author: Nate Eagleson <nate@nateeag.com>
;; Created: November 23, 2013
;; Package-Requires: ((skewer-mode "1.5.3"))
;; Version: 0.3.0

;;; Commentary:

;; This minor mode provides live-editing of CSS and transpile-to-CSS languages
;; via skewer.

;; skewer-css works for many cases, but if you're dealing with multiple
;; stylesheets and involved cascading (a.k.a. "legacy code"), it isn't so
;; useful. What you see while live-editing is not what you see when you
;; refresh, since skewer-css puts the updated CSS in new style tags, thus
;; changing their specificity.

;; skewer-css also doesn't work with Less, SCSS, or any of the lesser-known
;; compile-to-CSS languages - just vanilla CSS (though there is the skewer-less
;; package, if you run Less's in-browser JS version for development).

;; Enter this minor mode.

;; It refreshes stylesheets after saves by adding or updating a query string to
;; the relevant link tags in the browser.

;; Thus, what you see on a fresh pageload is always exactly what you see while
;; live-editing.

;;; Setup

;; Install from MELPA, then put the following somewhere in your init file:

;; (add-hook 'css-mode-hook 'skewer-reload-stylesheets-start-editing)

;; If you're live-editing Less, SCSS, or similar, add
;; `skewer-reload-stylesheets-start-editing' to the appropriate hook variable,
;; then set `skewer-reload-css-compile-command' to your transpile command:
;;
;;     (setq skewer-reload-stylesheets-compile-command "gulp css")
;;
;; This variable is best set in .dir-locals.el, so it can be set correctly
;; per-project.

;;; Usage

;; Open a browser window for the URL whose stylesheets you want to live-edit.
;; Skewer that window.

;; In emacs, open the stylesheet(s) you want to live-edit.

;; Make some changes in the stylesheet and save it. The updates will immediately
;; be reflected in the skewered windows.

;; and there you are - cross-browser live-editing for arbitrarily complex
;; stylesheets.

;; Note that browser plugins like
;; [Custom Javascript for Websites](https://chrome.google.com/webstore/detail/custom-javascript-for-web/poakhlngfciodnhlhhgnaaelnpjljija?hl=en)
;; make it easy to auto-skewer URLs on pageload, so you don't have to manually
;; re-skewer after every refresh.

;; Key bindings:

;; * C-x C-r -- `skewer-reload-stylesheets-reload-buffer`
;; Note that this keybinding is deprecated, as current usage reloads
;; stylesheets with an after-save-hook, so there is no need for a custom
;; keybinding.

;;; Code:
(require 'skewer-mode)

(defvar skewer-reload-stylesheets-data-root (file-name-directory load-file-name)
  "Location of data files needed by skewer-reload-stylesheets-mode.")

(defvar skewer-reload-stylesheets-compile-command nil
  "Command to compile current stylesheet into a CSS file.

If using SCSS, Less, or similar, set this to your compile command.

The command will be run asynchronously after a save, and all stylesheets in the
document will be reloaded once the build is complete.

By reloading all stylesheets, we work around the need to figure out which
stylesheets actually need to be reloaded, which could be involved for a complex
set of stylesheets.

The best place to define this is .dir-locals.el for a given project.

TODO Automatically pick up compile command from scss-mode config?
There may be a lot of smart defaulting we could do for the various CSS
variant modes.

TODO See if there's a need to reload only changed stylesheets. If a project had
a lot of stylesheets, reloading them all could be pretty slow, but the
compile-to-CSS projects I've seen tend to have just one main one along with a
few vendor files that will usually be 304s.")

(defun skewer-reload-stylesheets-reload-buffer ()
  "Save current buffer and ask skewer to reload it."

  (declare (obsolete skewer-reload-stylesheets-reload-on-save "0.1.0"))

  (interactive)
  (save-buffer)

  (skewer-reload-stylesheets-reload))

(defun skewer-reload-stylesheets-compile-sentinel (process event)
  "Sentinel to handle status changes in CSS compilation jobs.

TODO It would be nice to handle failed compiles more cleanly.
Right now it just throws an error, but things like syntax errors are
expected outcomes while programming."

  (cond ((equal event "finished\n")
         (skewer-eval "skewer.reloadStylesheets.reloadAll();"))

        ;; TODO Just use 't' as my second cond?
        ;; Is any event other than successful exit a failure?
        ((string-prefix-p "exited abnormally" event)
         (error "CSS compilation failure: %s" event))))

(defun skewer-reload-stylesheets-reload ()
  "Ask browser to reload the stylesheet for the current buffer."

  (if skewer-reload-stylesheets-compile-command
      ;; Compile CSS, and reload all stylesheets when finished.
      (let ((compile-css (start-process-shell-command
                          "skewer-make-css"
                          "*skewer-reload-stylesheets-compile-log*"
                          skewer-reload-stylesheets-compile-command)))

        (set-process-sentinel compile-css
                              'skewer-reload-stylesheets-compile-sentinel))

    ;; No compile command, so we must be in a CSS file. Just reload the
    ;; corresponding stylesheets.
    ;; TODO I tried to use skewer-apply, but it said skewer.reloadStylesheet was
    ;; not a valid function.
    (skewer-eval (concat "skewer.reloadStylesheets.reload(\"" (buffer-file-name) "\");"))))

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

;;;###autoload
(defun skewer-reload-stylesheets-start-editing ()
  "Configure current buffer for live-editing.

Add this to your stylesheet editing mode hook to make every
stylesheet live-editable by default."

  ;; Check for "is simple-httpd running?" snagged from simple-httpd.el's
  ;; stop-httpd function.
  ;; TODO Submit a PR to simple-httpd for checking if the server is running.
  (unless (process-status "httpd")
    (run-skewer))

  (skewer-reload-stylesheets-mode)
  (skewer-reload-stylesheets-reload-on-save))

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
