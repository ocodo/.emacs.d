;;; ac-emmet-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ac-emmet" "ac-emmet.el" (0 0 0 0))
;;; Generated autoloads from ac-emmet.el

(defface ac-emmet-candidate-face '((t (:inherit ac-candidate-face))) "\
Face for emmet candidates." :group (quote auto-complete))

(defface ac-emmet-selection-face '((t (:inherit ac-selection-face))) "\
Face for the emmet selected candidate." :group (quote auto-complete))

(defconst ac-emmet-source-defaults '((candidate-face . ac-emmet-candidate-face) (selection-face . ac-emmet-selection-face) (symbol . "a") (requires . 1) (action lambda nil (call-interactively 'emmet-expand-line))) "\
Defaults common to the various completion sources.")

(defvar ac-source-emmet-html-snippets (append '((candidates . ac-emmet-html-snippets-keys) (document lambda (s) (ac-emmet-document s ac-emmet-html-snippets-hash))) ac-emmet-source-defaults) "\
Auto-complete source for emmet-mode's html snippet completion.")

(defvar ac-source-emmet-html-aliases (append '((candidates . ac-emmet-html-aliases-keys) (document lambda (s) (ac-emmet-document s ac-emmet-html-aliases-hash))) ac-emmet-source-defaults) "\
Auto-complete source for emmet-mode's html alias completion.")

(defvar ac-source-emmet-css-snippets (append '((candidates . ac-emmet-css-snippets-keys) (document lambda (s) (ac-emmet-document s ac-emmet-css-snippets-hash))) ac-emmet-source-defaults) "\
Auto-complete source for emmet-mode's css snippet completion.")

(autoload 'ac-emmet-html-setup "ac-emmet" "\
Add the emmet-mode's html completion source to the front of `ac-sources'.
This affects only the current buffer.

\(fn)" t nil)

(autoload 'ac-emmet-css-setup "ac-emmet" "\
Add the emmet-mode's css completion source to the front of `ac-sources'.
This affects only the current buffer.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ac-emmet" '("ac-emmet-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ac-emmet-autoloads.el ends here
