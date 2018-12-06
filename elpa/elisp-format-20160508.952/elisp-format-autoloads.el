;;; elisp-format-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elisp-format" "elisp-format.el" (0 0 0 0))
;;; Generated autoloads from elisp-format.el

(autoload 'elisp-format-region "elisp-format" "\
Format current region or buffer.
This function will format region from START to END.
Or try to format `defun' around point.

\(fn &optional START END)" t nil)

(autoload 'elisp-format-buffer "elisp-format" "\
Format current buffer.

\(fn)" t nil)

(autoload 'elisp-format-file "elisp-format" "\
Format file with FILENAME.

\(fn FILENAME)" t nil)

(autoload 'elisp-format-file-batch "elisp-format" "\
Format elisp FILENAME.
But instead in `batch-mode'.
If SURPRESS-POPUP-WINDOW is non-nil, don't show output window.

\(fn FILENAME &optional SURPRESS-POPUP-WINDOW)" t nil)

(autoload 'elisp-format-directory "elisp-format" "\
Format recursive elisp files under DIR.

\(fn DIR)" t nil)

(autoload 'elisp-format-directory-batch "elisp-format" "\
Format recursive elisp files under DIR.
But instead in `batch-mode'.
If SURPRESS-POPUP-WINDOW is non-nil, don't show output window.

\(fn DIR &optional SURPRESS-POPUP-WINDOW)" t nil)

(autoload 'elisp-format-dired-mark-files "elisp-format" "\
Format dired mark files.

\(fn)" t nil)

(autoload 'elisp-format-library "elisp-format" "\
Format LIBRARY.

\(fn LIBRARY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "elisp-format" '("elisp-format-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elisp-format-autoloads.el ends here
