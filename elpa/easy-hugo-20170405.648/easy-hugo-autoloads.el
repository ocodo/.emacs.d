;;; easy-hugo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "easy-hugo" "easy-hugo.el" (0 0 0 0))
;;; Generated autoloads from easy-hugo.el

(autoload 'easy-hugo-article "easy-hugo" "\
Open a list of articles written in hugo.

\(fn)" t nil)

(autoload 'easy-hugo-publish "easy-hugo" "\
Adapt local change to the server with hugo.

\(fn)" t nil)

(autoload 'easy-hugo-newpost "easy-hugo" "\
Create a new post with hugo.
POST-FILE needs to have and extension '.md' or '.org'.

\(fn POST-FILE)" t nil)

(autoload 'easy-hugo-preview "easy-hugo" "\
Preview hugo at localhost.

\(fn)" t nil)

(autoload 'easy-hugo-deploy "easy-hugo" "\
Execute deploy.sh script locate at 'easy-hugo-basedir'.

\(fn)" t nil)

(autoload 'easy-hugo "easy-hugo" "\
Easy hugo.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "easy-hugo" '("easy-hugo-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; easy-hugo-autoloads.el ends here
