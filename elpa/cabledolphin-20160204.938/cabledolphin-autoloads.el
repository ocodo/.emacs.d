;;; cabledolphin-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cabledolphin" "cabledolphin.el" (0 0 0 0))
;;; Generated autoloads from cabledolphin.el

(autoload 'cabledolphin-set-pcap-file "cabledolphin" "\
Set the file where captured network data is written to.

If the file doesn't exist, or is empty, a PCAP file header will
be written to it.  Otherwise, any new data will be appended to
the file.

\(fn FILE)" t nil)

(autoload 'cabledolphin-trace-existing-connection "cabledolphin" "\
Start capturing network data for an existing connection.

\(fn PROCESS)" t nil)

(autoload 'cabledolphin-trace-new-connections "cabledolphin" "\
Capture data for any new connections matching REGEXP.
Matching is done against the process name.

\(fn REGEXP)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cabledolphin" '("cabledolphin-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cabledolphin-autoloads.el ends here
