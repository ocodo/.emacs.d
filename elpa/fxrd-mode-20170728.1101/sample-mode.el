(require 'fxrd-mode)
(require 'fxrd-validators)

;;; Example FXRD file type
(defconst sample-spec
  ;; File Header Record
  `(("H" (
          (1 1 "Record Type (H)" ,(fxrd-alphanumeric-v :const "H"))
          (2 16 "Company Name" ,(fxrd-alphanumeric-v))
          (17 24 "Padding" ,(fxrd-alphanumeric-v))))
    ;; Data Record
    ("D" (
          (1 1 "Record Type (D)" ,(fxrd-alphanumeric-v :const "D"))
          (2 2 "Transaction Type" ,(fxrd-alphanumeric-v :enum '("D" "C" "R")))
          (3 8 "Account Number" ,(fxrd-numeric-v))
          (9 15 "Amount" ,(fxrd-decimal-v))
          (16 24 "Account name" ,(fxrd-alphanumeric-v))))
    ;; Trailer Record
    ("T" (
          (1 1 "Record Type (T)" ,(fxrd-alphanumeric-v :const "T"))
          (2 4 "Record count" ,(fxrd-numeric-v))
          (5 24 "Padding" ,(fxrd-alphanumeric-v))))))


;;;###autoload
(define-derived-mode sample-mode fxrd-mode "SAMPLE"
"Major mode for editing SAMPLE fixed field width files.

\\{fxrd-mode-map}"
(setq fxrd-current-spec sample-spec))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.fxrd-sample\\($\\|\\.\\)" . sample-mode))

(provide 'sample-mode)
