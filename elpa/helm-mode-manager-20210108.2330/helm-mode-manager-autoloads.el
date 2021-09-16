;;; helm-mode-manager-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-mode-manager" "helm-mode-manager.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from helm-mode-manager.el

(autoload 'helm-enable-minor-mode "helm-mode-manager" "\
Return a `helm' selection of all available minor modes.
Selecting a target will activate the minor mode. The persistent
action is to show help about the selected minor mode." t nil)

(autoload 'helm-disable-minor-mode "helm-mode-manager" "\
Return a `helm' selection of active minor modes. Selecting a
target will deactivate the minor mode. The persistent action is
to show help about the selected minor mode." t nil)

(autoload 'helm-switch-major-mode "helm-mode-manager" "\
Return a `helm' selection of all available major modes.
Selecting a target will activate the major mode. The persistent
action is to show help about the selected major mode." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-mode-manager" '("helm-mode-manager-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-mode-manager-autoloads.el ends here
