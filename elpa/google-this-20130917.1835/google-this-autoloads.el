;;; google-this-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (google-this-mode google-forecast google-cpp-reference
;;;;;;  google-this-clean-error-string google-error google-this google-region
;;;;;;  google-symbol google-word google-line google-string google-lucky-search
;;;;;;  google-lucky-and-insert-url google-search) "google-this"
;;;;;;  "google-this.el" (21051 53349 0 0))
;;; Generated autoloads from google-this.el

(autoload 'google-search "google-this" "\
Write and do a google search.

\(fn PREFIX &optional SEARCH-URL)" t nil)

(autoload 'google-lucky-and-insert-url "google-this" "\
Fetch the url that would be visited by `google-lucky'.

If you just want to do an \"I'm feeling lucky search\", use
`google-lucky-search' instead.

Interactively:
* Insert the URL at point,
* Kill the searched term, removing it from the buffer (it is killed, not
  deleted, so it can be easily yanked back if desired).
* Search term defaults to region or line, and always queries for
  confirmation.

Non-Interactively:
* Runs synchronously,
* Only insert if INSERT is non-nil,
* Search term is an argument without confirmation.

\(fn TERM &optional INSERT)" t nil)

(autoload 'google-lucky-search "google-this" "\
Exactly like `google-search', but uses the \"I'm feeling lucky\" option.

\(fn PREFIX)" t nil)

(autoload 'google-string "google-this" "\
Google given TEXT, but ask the user first if NOCONFIRM is nil.

\(fn PREFIX &optional TEXT NOCONFIRM)" nil nil)

(autoload 'google-line "google-this" "\
Google the current line.

\(fn PREFIX)" t nil)

(autoload 'google-word "google-this" "\
Google the current word.

\(fn PREFIX)" t nil)

(autoload 'google-symbol "google-this" "\
Google the current symbol.

\(fn PREFIX)" t nil)

(autoload 'google-region "google-this" "\
Google the current region.

\(fn PREFIX)" t nil)

(autoload 'google-this "google-this" "\
Automatically decide what the user wants to google (always something under point).

Unlike `google-search' (which presents an empty prompt with
\"this\" as the default value), this function inserts the query
in the minibuffer to be edited.

\(fn PREFIX)" t nil)

(autoload 'google-error "google-this" "\
Google the current error in the compilation buffer.

\(fn PREFIX)" t nil)

(autoload 'google-this-clean-error-string "google-this" "\
Parse error strings and turn them into googleable strings.

Removes unhelpful details like file names and line numbers from
simple error strings (such as c-like erros).

Uses replacements in `google-error-regexp' and stops at the first match.

\(fn S)" t nil)

(autoload 'google-cpp-reference "google-this" "\
Visit the most probable cppreference.com page for this word.

\(fn)" t nil)

(autoload 'google-forecast "google-this" "\
Just searches google for \"weather\".

\(fn PREFIX)" t nil)

(defvar google-this-mode nil "\
Non-nil if Google-This mode is enabled.
See the command `google-this-mode' for a description of this minor mode.")

(custom-autoload 'google-this-mode "google-this" nil)

(autoload 'google-this-mode "google-this" "\
Toggle Google-This mode on or off.
With a prefix argument ARG, enable Google-This mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{google-this-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("google-this-pkg.el" "noflet.el") (21051
;;;;;;  53349 203923 0))

;;;***

(provide 'google-this-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; google-this-autoloads.el ends here
