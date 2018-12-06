;;; sx-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sx" "sx.el" (0 0 0 0))
;;; Generated autoloads from sx.el

(autoload 'sx-bug-report "sx" "\
File a bug report about the `sx' package.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx" '("sx-")))

;;;***

;;;### (autoloads nil "sx-auth" "sx-auth.el" (0 0 0 0))
;;; Generated autoloads from sx-auth.el

(autoload 'sx-authenticate "sx-auth" "\
Authenticate this application.
Authentication is required to read your personal data (such as
notifications) and to write with the API (asking and answering
questions).

When this function is called, `browse-url' is used to send the
user to an authorization page managed by StackExchange.  The
following privileges are requested:

* read_inbox
    use SX to manage and visit items in your inbox

* write_acesss
    write comments, ask questions, and post answers on your
    behalf

* no_expiry
    do not pester you to reauthorize again

* private_info
    TODO explanation.

After authorization with StackExchange, the user is then
redirected to a website managed by SX.  The access token required
to use authenticated methods is included in the hash (which is
parsed and displayed prominently on the page).

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-auth" '("sx-auth-")))

;;;***

;;;### (autoloads nil "sx-babel" "sx-babel.el" (0 0 0 0))
;;; Generated autoloads from sx-babel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-babel" '("sx-babel-")))

;;;***

;;;### (autoloads nil "sx-button" "sx-button.el" (0 0 0 0))
;;; Generated autoloads from sx-button.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-button" '("sx-")))

;;;***

;;;### (autoloads nil "sx-cache" "sx-cache.el" (0 0 0 0))
;;; Generated autoloads from sx-cache.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-cache" '("sx-cache-")))

;;;***

;;;### (autoloads nil "sx-compose" "sx-compose.el" (0 0 0 0))
;;; Generated autoloads from sx-compose.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-compose" '("sx-compose-")))

;;;***

;;;### (autoloads nil "sx-encoding" "sx-encoding.el" (0 0 0 0))
;;; Generated autoloads from sx-encoding.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-encoding" '("sx-encoding-")))

;;;***

;;;### (autoloads nil "sx-favorites" "sx-favorites.el" (0 0 0 0))
;;; Generated autoloads from sx-favorites.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-favorites" '("sx-favorite")))

;;;***

;;;### (autoloads nil "sx-filter" "sx-filter.el" (0 0 0 0))
;;; Generated autoloads from sx-filter.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-filter" '("sx-")))

;;;***

;;;### (autoloads nil "sx-inbox" "sx-inbox.el" (0 0 0 0))
;;; Generated autoloads from sx-inbox.el

(autoload 'sx-inbox "sx-inbox" "\
Display a buffer listing inbox items.
With prefix NOTIFICATIONS, list notifications instead of inbox.

\(fn &optional NOTIFICATIONS)" t nil)

(autoload 'sx-inbox-notifications "sx-inbox" "\
Display a buffer listing notification items.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-inbox" '("sx-inbox-")))

;;;***

;;;### (autoloads nil "sx-interaction" "sx-interaction.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from sx-interaction.el

(autoload 'sx-open-link "sx-interaction" "\
Visit element given by LINK inside Emacs.
Element can be a question, answer, or comment.

\(fn LINK &optional _)" t nil)

(autoload 'sx-org-get-link "sx-interaction" "\
Add a link to this post to Org's memory.

\(fn)" nil nil)

(autoload 'sx-ask "sx-interaction" "\
Start composing a question for SITE.
SITE is a string, indicating where the question will be posted.

\(fn SITE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-interaction" '("sx-")))

;;;***

;;;### (autoloads nil "sx-method" "sx-method.el" (0 0 0 0))
;;; Generated autoloads from sx-method.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-method" '("sx-method-post-from-data")))

;;;***

;;;### (autoloads nil "sx-networks" "sx-networks.el" (0 0 0 0))
;;; Generated autoloads from sx-networks.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-networks" '("sx-network-")))

;;;***

;;;### (autoloads nil "sx-notify" "sx-notify.el" (0 0 0 0))
;;; Generated autoloads from sx-notify.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-notify" '("sx-notify-")))

;;;***

;;;### (autoloads nil "sx-question" "sx-question.el" (0 0 0 0))
;;; Generated autoloads from sx-question.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-question" '("sx-")))

;;;***

;;;### (autoloads nil "sx-question-list" "sx-question-list.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from sx-question-list.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-question-list" '("sx-question-list-")))

;;;***

;;;### (autoloads nil "sx-question-mode" "sx-question-mode.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from sx-question-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-question-mode" '("sx-question-mode")))

;;;***

;;;### (autoloads nil "sx-question-print" "sx-question-print.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from sx-question-print.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-question-print" '("sx-question-mode-")))

;;;***

;;;### (autoloads nil "sx-request" "sx-request.el" (0 0 0 0))
;;; Generated autoloads from sx-request.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-request" '("sx-")))

;;;***

;;;### (autoloads nil "sx-search" "sx-search.el" (0 0 0 0))
;;; Generated autoloads from sx-search.el

(autoload 'sx-search "sx-search" "\
Display search on SITE for question titles containing QUERY.
When TAGS is given, it is a lists of tags, one of which must
match.  When EXCLUDED-TAGS is given, it is a list of tags, none
of which is allowed to match.

Interactively, the user is asked for SITE and QUERY.  With a
prefix argument, the user is asked for everything.

\(fn SITE QUERY &optional TAGS EXCLUDED-TAGS)" t nil)

(autoload 'sx-search-tag-at-point "sx-search" "\
Follow tag under position POS or point.

\(fn &optional POS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-search" '("sx-search-")))

;;;***

;;;### (autoloads nil "sx-site" "sx-site.el" (0 0 0 0))
;;; Generated autoloads from sx-site.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-site" '("sx-site-")))

;;;***

;;;### (autoloads nil "sx-switchto" "sx-switchto.el" (0 0 0 0))
;;; Generated autoloads from sx-switchto.el

(define-prefix-command 'sx-switchto-map)

;;;***

;;;### (autoloads nil "sx-tab" "sx-tab.el" (0 0 0 0))
;;; Generated autoloads from sx-tab.el

(autoload 'sx-tab-all-questions "sx-tab" nil t)

(autoload 'sx-tab-unanswered "sx-tab" nil t)

(autoload 'sx-tab-unanswered-my-tags "sx-tab" nil t)

(autoload 'sx-tab-featured "sx-tab" nil t)

(autoload 'sx-tab-starred "sx-tab" nil t)

(autoload 'sx-tab-frontpage "sx-tab" nil t)

(autoload 'sx-tab-newest "sx-tab" nil t)

(autoload 'sx-tab-topvoted "sx-tab" nil t)

(autoload 'sx-tab-hot "sx-tab" nil t)

(autoload 'sx-tab-week "sx-tab" nil t)

(autoload 'sx-tab-month "sx-tab" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-tab" '("sx-tab-")))

;;;***

;;;### (autoloads nil "sx-tag" "sx-tag.el" (0 0 0 0))
;;; Generated autoloads from sx-tag.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-tag" '("sx-tag-")))

;;;***

;;;### (autoloads nil "sx-time" "sx-time.el" (0 0 0 0))
;;; Generated autoloads from sx-time.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-time" '("sx-time-")))

;;;***

;;;### (autoloads nil "sx-user" "sx-user.el" (0 0 0 0))
;;; Generated autoloads from sx-user.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sx-user" '("sx-")))

;;;***

;;;### (autoloads nil nil ("sx-load.el" "sx-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sx-autoloads.el ends here
