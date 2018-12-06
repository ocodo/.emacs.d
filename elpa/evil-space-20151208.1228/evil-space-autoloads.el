;;; evil-space-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-space" "evil-space.el" (0 0 0 0))
;;; Generated autoloads from evil-space.el

(autoload 'evil-space-setup "evil-space" "\
Makes KEY repeatable with `evil-space-next-key' and `evil-space-prev-key'.

NEXT and PREV represent the key bindings that repeat KEY forward and backwards,
respectively.

KEY, NEXT and PREV can be a key, function symbol, or forms that evaluate to a
function.

KEYMAP, if non-nil, specifies where to lookup KEY, NEXT and PREV. If nil, it
defaults to `evil-motion-state-map'.

Examples:
    (evil-space-setup \"f\" \";\" \",\")

    ;; Probably not a great idea.
    (evil-space-setup \"s-/\" \"s-/\" \"s-/\" evil-commentary-mode-map)

    ;; Map * in evil-visualstar-mode-map, in visual state
    (evil-space-setup \"*\" \"n\" \"N\" (evil-get-auxiliary-keymap evil-visualstar-mode-map 'visual))

    ;; Map functions directly, rather than keys
    (evil-space-setup evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)

\(fn KEY NEXT PREV &optional KEYMAP)" nil t)

(defvar evil-space-mode nil "\
Non-nil if Evil-Space mode is enabled.
See the `evil-space-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-space-mode'.")

(custom-autoload 'evil-space-mode "evil-space" nil)

(autoload 'evil-space-mode "evil-space" "\
Evil space mode.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'evil-space-default-setup 'evil-space-mode "0.0.4")

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-space-autoloads.el ends here
