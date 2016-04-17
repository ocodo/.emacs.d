;;; textmate-to-yas-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "textmate-to-yas" "textmate-to-yas.el" (22291
;;;;;;  19058 692188 168000))
;;; Generated autoloads from textmate-to-yas.el

(autoload 'textmate-regexp-to-emacs-regexp "textmate-to-yas" "\
* Convert a textmate regular expression to an emacs regular expression (as much as possible)

\(fn REXP)" nil nil)

(autoload 'textmate-yas-submenu "textmate-to-yas" "\


\(fn LST SUBMENUS SPACE MODE-NAME)" nil nil)

(autoload 'textmate-import-drag-and-drop "textmate-to-yas" "\
* Drag and drop interface to import files.

\(fn URI &rest IGNORE)" nil nil)

(defadvice dnd-open-local-file (around textmate-import-drag-and-drop activate) "\
* Drag Textmate git-hub tar.gz files to import into Yasnippet." (unless (textmate-import-drag-and-drop (ad-get-arg 0)) ad-do-it))

(defadvice dnd-open-file (around textmate-import-drag-and-drop activate) "\
* Drag Textmate git-hub tar.gz files to import into Yasnippet." (unless (textmate-import-drag-and-drop (ad-get-arg 0)) ad-do-it))

(autoload 'textmate-import-git-tar\.gz "textmate-to-yas" "\
* Imports a TextMate git-hub bundle.

\(fn FILE PARENT-MODES)" t nil)

(autoload 'textmate-import-bundle "textmate-to-yas" "\
Imports textmate bundle to new-dir.  Mode may be a string or a function determining which mode to place files in...

\(fn DIR PARENT-MODES &optional ORIGINAL-AUTHOR YAS-DIR MODE TRANSFORM-FUNCTION)" t nil)

(autoload 'textmate-import-svn-from-url "textmate-to-yas" "\
* Imports a textmate bundle and extracts snippets from `textmate-import-svn-url'

\(fn)" t nil)

(autoload 'yas---t/ "textmate-to-yas" "\
* Textmate like mirror.  Uses textmate regular expression and textmate formatting.

\(fn TEXTMATE-REG TEXTMATE-REP &optional TEXTMATE-OPTION T-TEXT)" nil nil)

(autoload 'yas-text-on-moving-away "textmate-to-yas" "\
* Changes text when moving away AND original text has not changed

\(fn DEFAULT-TEXT)" nil nil)

;;;***

;;;### (autoloads nil nil ("textmate-to-yas-pkg.el") (22291 19058
;;;;;;  700997 371000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; textmate-to-yas-autoloads.el ends here
