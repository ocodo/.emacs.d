;;; auto-correct-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "auto-correct" "auto-correct.el" (0 0 0 0))
;;; Generated autoloads from auto-correct.el

(defvar auto-correct-mode nil "\
Non-nil if Auto-Correct mode is enabled.
See the `auto-correct-mode' command
for a description of this minor mode.")

(custom-autoload 'auto-correct-mode "auto-correct" nil)

(autoload 'auto-correct-mode "auto-correct" "\
Activate automatic corrections.

Auto correct expansions will only work when this mode is enabled,
but auto-correct can be trained with `auto-correct-fix-and-add'
even if this mode is disabled.

When this mode is enabled, corrections made with flyspell and
Ispell will be made automatically after fixing them once.

In order to add corrections to the auto-correct abbrev table in
flyspell (and thus have them corrected later), set
`flyspell-use-global-abbrev-table-p' to non-nil.

In order to set corrections as local using Ispell, use
the command `auto-correct-toggle-ispell-local'.

\\{auto-correct-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'auto-correct-fix-and-add "auto-correct" "\
Use `ispell-word' to fix a misspelled word at point.

Once the misspelled word is fixed, auto-correct will remember the
fix and auto-correct it from then on, so long as
`auto-correct-mode' is enabled.

With a non-nil argument LOCAL (interactively, the prefix argument),
create a fix for the typo that will be auto-corrected for buffers
using the current local mode.

This is pointless to use when `auto-correct-mode' is enabled;
instead, use `ispell-word' and `auto-correct-toggle-ispell-local'
to use the local abbrev table.

\(fn LOCAL)" t nil)

(autoload 'auto-correct-scan-buffer "auto-correct" "\
Scan current buffer for misspelled words.

When a misspelled word is found, offer to correct the misspelled
word and auto-correct the typo in the future.

When `auto-correct-mode' is enabled, use the `ispell' command
instead.

\(fn)" t nil)

(autoload 'auto-correct-scan-region "auto-correct" "\
Scan the region between START and END for misspelled words.

Interactively, START and END are the current region.

When a misspelled word is found, offer to correct the misspelled
word and auto-correct the typo in the future.

When `auto-correct-mode' is enabled, use the `ispell' command
instead.

\(fn START END)" t nil)

(autoload 'auto-correct-scan "auto-correct" "\
Scan the buffer or region for misspelled words.

When a misspelled word is found, offer to correct the misspelled
word and auto-correct the typo in the future.

When `auto-correct-mode' is enabled, use the `ispell' command
instead.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "auto-correct" '("auto-correct-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-correct-autoloads.el ends here
