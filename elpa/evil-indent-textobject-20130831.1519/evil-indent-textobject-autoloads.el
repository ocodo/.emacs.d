;;; evil-indent-textobject-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "evil-indent-textobject" "evil-indent-textobject.el"
;;;;;;  (21439 25206 210176 999000))
;;; Generated autoloads from evil-indent-textobject.el

(eval-after-load 'evil '(progn (autoload 'evil-indent-i-indent "evil-indent-textobject" nil t) (autoload 'evil-indent-a-indent "evil-indent-textobject" nil t) (autoload 'evil-indent-a-indent-lines "evil-indent-textobject" nil t) (define-key evil-inner-text-objects-map "i" 'evil-indent-i-indent) (define-key evil-outer-text-objects-map "i" 'evil-indent-a-indent) (define-key evil-outer-text-objects-map "I" 'evil-indent-a-indent-lines)))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-indent-textobject-autoloads.el ends here
