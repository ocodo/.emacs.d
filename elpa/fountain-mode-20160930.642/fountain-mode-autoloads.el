;;; fountain-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "fountain-mode" "fountain-mode.el" (22519 51781
;;;;;;  156431 77000))
;;; Generated autoloads from fountain-mode.el

(add-to-list 'auto-mode-alist '("\\.fountain\\'" . fountain-mode))

(with-eval-after-load 'autoinsert (define-auto-insert '(fountain-mode . "Fountain metadata skeleton") fountain-metadata-skeleton))

(autoload 'fountain-mode "fountain-mode" "\
Major mode for screenwriting in Fountain markup.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; fountain-mode-autoloads.el ends here
