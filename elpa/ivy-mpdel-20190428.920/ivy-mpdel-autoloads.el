;;; ivy-mpdel-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-mpdel" "ivy-mpdel.el" (0 0 0 0))
;;; Generated autoloads from ivy-mpdel.el

(autoload 'ivy-mpdel-list "ivy-mpdel" "\
Select a child of ENTITY.
If ENTITY is nil, select from all artists.

\(fn &optional ENTITY)" t nil)

(autoload 'ivy-mpdel-artists "ivy-mpdel" "\
Select music from a list of artists." t nil)

(autoload 'ivy-mpdel-stored-playlists "ivy-mpdel" "\
Select music from a stored playlist or edit one." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-mpdel" '("ivy-mpdel--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-mpdel-autoloads.el ends here
