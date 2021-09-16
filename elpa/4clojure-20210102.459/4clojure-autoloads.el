;;; 4clojure-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "4clojure" "4clojure.el" (0 0 0 0))
;;; Generated autoloads from 4clojure.el

(autoload '4clojure-open-question "4clojure" "\
Open problem PROBLEM-NUMBER in an aptly named buffer.

\(fn PROBLEM-NUMBER)" t nil)

(autoload '4clojure-login "4clojure" "\
Log in to the 4clojure website with the supplied USERNAME.
Prompts for a password.

\(fn USERNAME)" t nil)

(autoload '4clojure-next-question "4clojure" "\
Get the next question or 1st question based on the current buffer name." t nil)

(autoload '4clojure-previous-question "4clojure" "\
Open the previous question or 1st question based on the current buffer name." t nil)

(autoload '4clojure-check-answers "4clojure" "\
Send the first answer to 4clojure and check the result." t nil)

(defvar 4clojure-mode-map (let ((map (make-sparse-keymap))) (let ((prefix-map (make-sparse-keymap))) (define-key prefix-map (kbd "c") '4clojure-check-answers) (define-key prefix-map (kbd "n") '4clojure-next-question) (define-key map "C-c" prefix-map)) map) "\
Keymap for 4clojure mode.")

(autoload '4clojure-mode "4clojure" "\
4clojure Minor Mode.
  \\{4clojure-mode-map}

If called interactively, enable 4clojure mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "4clojure" '("4clojure-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; 4clojure-autoloads.el ends here
