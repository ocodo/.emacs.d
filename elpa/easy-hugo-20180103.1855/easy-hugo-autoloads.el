;;; easy-hugo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "easy-hugo" "easy-hugo.el" (23138 48782 270438
;;;;;;  699000))
;;; Generated autoloads from easy-hugo.el

(autoload 'easy-hugo-article "easy-hugo" "\
Open a list of articles written in hugo with dired.

\(fn)" t nil)

(autoload 'easy-hugo-image "easy-hugo" "\
Generate image link.

\(fn)" t nil)

(autoload 'easy-hugo-put-image "easy-hugo" "\
Move image to image directory and generate image link.

\(fn)" t nil)

(autoload 'easy-hugo-pull-image "easy-hugo" "\
Pull image from internet to image directory and generate image link.

\(fn)" t nil)

(autoload 'easy-hugo-publish "easy-hugo" "\
Adapt local change to the server with hugo.

\(fn)" t nil)

(autoload 'easy-hugo-publish-timer "easy-hugo" "\
A timer that publish after the N number of minutes has elapsed.

\(fn N)" t nil)

(autoload 'easy-hugo-cancel-publish-timer "easy-hugo" "\
Cancel timer that publish after the specified number of minutes has elapsed.

\(fn)" t nil)

(autoload 'easy-hugo-newpost "easy-hugo" "\
Create a new post with hugo.
POST-FILE needs to have and extension '.md' or '.org' or '.ad' or '.rst' or '.mmark' or '.html'.

\(fn POST-FILE)" t nil)

(autoload 'easy-hugo-preview "easy-hugo" "\
Preview hugo at localhost.

\(fn)" t nil)

(autoload 'easy-hugo-github-deploy "easy-hugo" "\
Execute `easy-hugo-github-deploy-script' script locate at `easy-hugo-basedir'.

\(fn)" t nil)

(autoload 'easy-hugo-github-deploy-timer "easy-hugo" "\
A timer that github-deploy after the N number of minutes has elapsed.

\(fn N)" t nil)

(autoload 'easy-hugo-cancel-github-deploy-timer "easy-hugo" "\
Cancel timer that github-deploy after the specified number of minutes has elapsed.

\(fn)" t nil)

(autoload 'easy-hugo-amazon-s3-deploy "easy-hugo" "\
Deploy hugo source at Amazon S3.

\(fn)" t nil)

(autoload 'easy-hugo-amazon-s3-deploy-timer "easy-hugo" "\
A timer that amazon-s3-deploy after the N number of minutes has elapsed.

\(fn N)" t nil)

(autoload 'easy-hugo-cancel-amazon-s3-deploy-timer "easy-hugo" "\
Cancel timer that amazon-s3-deploy after the specified number of minutes has elapsed.

\(fn)" t nil)

(autoload 'easy-hugo-google-cloud-storage-deploy "easy-hugo" "\
Deploy hugo source at Google Cloud Storage.

\(fn)" t nil)

(autoload 'easy-hugo-google-cloud-storage-deploy-timer "easy-hugo" "\
A timer that google-cloud-storage-deploy after the N number of minutes has elapsed.

\(fn N)" t nil)

(autoload 'easy-hugo-cancel-google-cloud-storage-deploy-timer "easy-hugo" "\
Cancel timer that google-cloud-storage-deploy after the specified number of minutes has elapsed.

\(fn)" t nil)

(autoload 'easy-hugo-ag "easy-hugo" "\
Search for blog article with counsel-ag or helm-ag.

\(fn)" t nil)

(autoload 'easy-hugo-open-config "easy-hugo" "\
Open Hugo's config file.

\(fn)" t nil)

(autoload 'easy-hugo "easy-hugo" "\
Easy hugo mode.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; easy-hugo-autoloads.el ends here
