;;; evil-visual-mark-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-visual-mark-mode" "evil-visual-mark-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-visual-mark-mode.el

(defvar evil-visual-mark-mode nil "\
Non-nil if Evil-Visual-Mark mode is enabled.
See the `evil-visual-mark-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-visual-mark-mode'.")

(custom-autoload 'evil-visual-mark-mode "evil-visual-mark-mode" nil)

(autoload 'evil-visual-mark-mode "evil-visual-mark-mode" "\
Makes evil marks visible and easy to remember.

If called interactively, enable Evil-Visual-Mark mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-visual-mark-mode" '("evil-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-visual-mark-mode-autoloads.el ends here
