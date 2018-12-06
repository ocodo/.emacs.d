;;; apt-sources-list-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "apt-sources-list" "apt-sources-list.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from apt-sources-list.el

(autoload 'apt-sources-list-mode "apt-sources-list" "\
Major mode for editing APT’s “.list” files.

The “/etc/apt/sources.list” file and other files in
“/etc/apt/sources.list.d” tell APT, found on Debian-based systems
and others, where to find packages for installation.

This format specifies a package source with a single line, e.g.:

    deb http://deb.debian.org/debian stable main contrib

For more information about the format you can read the manual
pages “apt(8)” and “sources.list(5)”, also on the web at URL
‘https://manpages.debian.org/stable/apt/sources.list.5.en.html’
and URL ‘https://manpages.debian.org/stable/apt/apt.8.en.html’.

\\{apt-sources-list-mode-map}

The above editing commands will raise errors if the current line
is not a correctly-formatted APT source.

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons (rx (or (and (any "./") "sources.list") (and "/sources.list.d/" (one-or-more anything) ".list")) string-end) #'apt-sources-list-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "apt-sources-list" '("apt-sources-list-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; apt-sources-list-autoloads.el ends here
