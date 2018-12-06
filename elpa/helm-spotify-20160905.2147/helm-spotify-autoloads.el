;;; helm-spotify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-spotify" "helm-spotify.el" (0 0 0 0))
;;; Generated autoloads from helm-spotify.el

(defvar helm-source-spotify-track-search '((name . "Spotify") (volatile) (delayed) (multiline) (requires-pattern . 2) (candidates-process . helm-spotify-search) (action-transformer . helm-spotify-actions-for-track)))

(autoload 'helm-spotify "helm-spotify" "\
Bring up a Spotify search interface in helm.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "helm-spotify" '("helm-spotify-" "spotify-" "alist-get")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-spotify-autoloads.el ends here
