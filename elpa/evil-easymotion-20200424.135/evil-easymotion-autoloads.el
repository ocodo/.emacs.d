;;; evil-easymotion-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-easymotion" "evil-easymotion.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from evil-easymotion.el

(autoload 'evilem--collect "evil-easymotion" "\
Repeatedly execute func, and collect the cursor positions into a list

\(fn FUNC &optional SCOPE ALL-WINDOWS INITIAL-POINT SORT-KEY COLLECT-POSTPROCESS INCLUDE-INVISIBLE)" nil nil)
 (autoload 'evilem-motion-forward-word-begin "evil-easymotion" nil t)
 (autoload 'evilem-motion-forward-WORD-begin "evil-easymotion" nil t)
 (autoload 'evilem-motion-forward-word-end "evil-easymotion" nil t)
 (autoload 'evilem-motion-forward-WORD-end "evil-easymotion" nil t)
 (autoload 'evilem-motion-backward-word-begin "evil-easymotion" nil t)
 (autoload 'evilem-motion-backward-WORD-begin "evil-easymotion" nil t)
 (autoload 'evilem-motion-backward-word-end "evil-easymotion" nil t)
 (autoload 'evilem-motion-backward-WORD-end "evil-easymotion" nil t)
 (autoload 'evilem-motion-next-line "evil-easymotion" nil t)
 (autoload 'evilem-motion-previous-line "evil-easymotion" nil t)
 (autoload 'evilem-motion-next-visual-line "evil-easymotion" nil t)
 (autoload 'evilem-motion-previous-visual-line "evil-easymotion" nil t)
 (autoload 'evilem-motion-find-char-to "evil-easymotion" nil t)
 (autoload 'evilem-motion-find-char-to-backward "evil-easymotion" nil t)
 (autoload 'evilem-motion-find-char "evil-easymotion" nil t)
 (autoload 'evilem-motion-find-char-backward "evil-easymotion" nil t)
 (autoload 'evilem-motion-backward-section-begin "evil-easymotion" nil t)
 (autoload 'evilem-motion-backward-section-end "evil-easymotion" nil t)
 (autoload 'evilem-motion-forward-section-begin "evil-easymotion" nil t)
 (autoload 'evilem-motion-forward-section-end "evil-easymotion" nil t)
 (autoload 'evilem-motion-backward-sentence-begin "evil-easymotion" nil t)
 (autoload 'evilem-motion-forward-sentence-begin "evil-easymotion" nil t)
 (autoload 'evilem-motion-search-next "evil-easymotion" nil t)
 (autoload 'evilem-motion-search-previous "evil-easymotion" nil t)
 (autoload 'evilem-motion-search-word-forward "evil-easymotion" nil t)
 (autoload 'evilem-motion-search-word-backward "evil-easymotion" nil t)
 (autoload 'evilem-motion-previous-line-first-non-blank "evil-easymotion" nil t)
 (autoload 'evilem-motion-next-line-first-non-blank "evil-easymotion" nil t)

(autoload 'evilem-default-keybindings "evil-easymotion" "\
Define easymotions for all motions evil defines by default

\(fn PREFIX)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-easymotion" '("evilem-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-easymotion-autoloads.el ends here
