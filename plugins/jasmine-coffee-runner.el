;;; jasmine-coffee-runner --- Simple test launcher for Jasmine (in coffeescript).
;;; Commentary:
;;
;;  Launch a Jasmine coffeescript spec at the cursor position,
;;  effectively we just generate the required spec URL and browse
;;  it, unbound, just run:
;;
;;    M-x jasmine-coffee/verify-single
;;
;;  Example.
;;
;;  create/browse this url: localhost:3000/jasmine?spec=mockup%20is%20the%20one%20we%20want
;;  When our cursor is at the point noted below...
;;
;;  describe "mockup", ->
;;
;;    describe "we skip this", ->
;;
;;      describe "we also skip this", ->
;;
;;        # oh just ignore me...
;;        it "could be anything up here, we don't care", ->
;;          expect(nothing).toBeHappening()
;;
;;    it "is the one we want... ", ->
;;      expect(something).toBe somethingElse
;;      # My cursor is here...
;;
;;    it "is a red herring", ->
;;      expect(thing).toBe defined
;;
;;; Code:

(defvar jasmine-coffee/base-url
  "http://localhost:3000/jasmine?spec="
  "Base URL for our Jasmine spec runner.")

(defvar jasmine-coffee/it-regexp
  "it(? ?[\"']\\(.*\\)\\(?:[\"'],\\) ?)?"
  "Regexp to find a jasmine coffee-mode `it'.")

(defvar jasmine-coffee/describe-regexp
  "describe(? ?[\"']\\(.*\\)\\(?:[\"'],\\) ?)?"
  "Regexp to find a jasmine coffee-mode `describe'.")

(defun jasmine-coffee/get-previous-describe-marker-and-description (&optional all)
  "Find previous Jasmine `describe'.

Return start marker and description string.

If ALL is specified use collect all and return as a list."

  (save-excursion
    (with-demoted-errors
      (if all
          (progn (re-search-backward jasmine-coffee/describe-regexp 0 t -1))
          (progn
            (re-search-backward jasmine-coffee/describe-regexp 0 t )
            (destructuring-bind (start end description-start description-end) (match-data)
              (list start (buffer-substring-no-properties description-start description-end))))
          ))))

(defun jasmine-coffee/get-previous-it-marker-and-description ()
  "Find the nearest Jasmine `it' definition, return start marker and description string."
  (save-excursion
    (with-demoted-errors
    (re-search-backward jasmine-coffee/it-regexp)
    (destructuring-bind (start end description-start description-end) (match-data)
      (list start (buffer-substring-no-properties description-start description-end))))))

(defun jasmine-coffee/get-column-from-marker (marker)
  "Get the column number at the MARKER position."
  (save-excursion
    (goto-char (marker-position marker))
    (current-column)))

(defun jasmine-coffee/get-previous-describes-outdented-from-column (marker column)
  "Go to the MARKER and `re-search-backward' for all previous 'describe-regexp'.
Filter and keep only where column is less than COLUMN."
  (save-excursion
    (goto-char (marker-position marker))
    (destructuring-bind
        (new-marker string)
        (jasmine-coffee/get-previous-describe-marker-and-description )

      (when (< (jasmine-coffee/get-column-from-marker new-marker) column)
          (setq description string))
      (list new-marker description))
    ))

(defun jasmine-coffee/verify-single ()
  "Compose the Spec URL launch a browser and run the spec at the cursor point."
  (interactive)
  (save-excursion
    ;; Get the previous it
    (let* ((start-column 0) (spec-string ""))
      (destructuring-bind
        (marker string)
        (jasmine-coffee/get-previous-it-marker-and-description)
        (setq start-column (jasmine-coffee/get-column-from-marker marker))

        ;; TODO: Get all previous describes.

        )

    )))

(provide 'jasmine-coffee-runner)
;;; jasmine-coffee-runner.el ends here
