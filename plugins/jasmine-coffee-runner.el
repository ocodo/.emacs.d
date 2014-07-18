;;; jasmine-coffee-runner --- Simple test launcher for Jasmine (in coffeescript).
;;; Author: Jason Milkins
;;; Version: 201407181200
;;; Commentary:
;;
;;  This file is not a part of Emacs
;;
;;  This launches a Jasmine coffeescript spec above the point.
;;
;;    M-x jasmine-coffee/verify-single
;;
;;  Example.
;;
;;  When our cursor is at the point noted below...
;;
;;  describe "base level", ->
;;
;;    describe "one level deep", ->
;;
;;      describe "we skip this", ->
;;
;;        # oh just ignore me...
;;        it "we skip this it definition", ->
;;          expect(nothing).toBeHappening()
;;
;;      it "is the one we #want...", ->
;;        expect(something).toBe somethingElse
;;        # My cursor is here...
;;        # note, we must replace # in the spec name.
;;
;;  expect:
;;    jasmine-coffee/base-url +
;;    "base%20level%20one%20level%20deep%20is%20the%20one%20we%20%23want..."
;;
;;; Licence: GPL3
;;
;;; Code:

(defvar jasmine-coffee/base-url
  "http://localhost:3000/jasmine?spec="
  "Base URL for our Jasmine spec runner.")

(defvar jasmine-coffee/it-regexp
  (rx
   "it"
   (any " " "(")
   (zero-or-more " ")
   (any "'" "\"")
   (group (zero-or-more not-newline))
   (any "'" "\"")
   (? ")")
   (zero-or-more " ")
   ","
   )
  "Regexp to find a jasmine coffee-mode `it'.")

(defvar jasmine-coffee/describe-regexp
  (rx
   "describe"
   (any " " "(")
   (zero-or-more " ")
   (any "'" "\"")
   (group (zero-or-more not-newline))
   (any "'" "\"")
   (? ")")
   (zero-or-more " ")
   ","
   )
  "Regexp to find a jasmine coffee-mode `describe'.")

(defun jasmine-coffee/verify-single ()
  "Compose the Spec URL launch a browser and run the spec at the cursor point."
  (interactive)
  (let* ((start-column 0) (spec-string ""))
    (save-excursion
      (re-search-backward jasmine-coffee/it-regexp)
      (setq start-column (current-column))
      (setq spec-string (match-string-no-properties 1))

      (while
          (re-search-backward jasmine-coffee/describe-regexp 0 t)
        (when (< (current-column) start-column)
          (setq start-column (current-column))
          (setq spec-string (format "%s %s" (match-string 1) spec-string)))))
    (setq spec-string (replace-regexp-in-string "#" "%23" spec-string))
    (browse-url (url-encode-url (concat jasmine-coffee/base-url spec-string)))))

(provide 'jasmine-coffee-runner)

;;; jasmine-coffee-runner.el ends here
