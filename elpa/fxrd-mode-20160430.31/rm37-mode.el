(require 'fxrd-mode)
(require 'fxrd-validators)

;;; Mastercard rebate transaction file

(defconst rm37-spec
  ;; File Header Record
  `(("H" (
          (1 1 "Record Type (H)" ,(fxrd-alphanumeric-v :const "H"))
          (2 9 "Record Date" ,(fxrd-numeric-v))
          (10 15 "Record Time" ,(fxrd-numeric-v))
          (16 26 "Member ICA" ,(fxrd-numeric-v))
          ;; Undocumented, but supported by Mastercard
          (27 86 "File Name" ,(fxrd-alphanumeric-v))
          (87 200 "Filler" ,(fxrd-alphanumeric-v))))
    ("D" (
          (1 1 "Record Type (D)" ,(fxrd-alphanumeric-v :const "D"))
          (2 14 "Transaction Sequence Number" ,(fxrd-numeric-v))
          (15 33 "Bank Account Number" ,(fxrd-numeric-v :pad " "))
          (34 46 "Transaction Amount" ,(fxrd-decimal-v :pad " "))
          (47 54 "Transaction Date" ,(fxrd-numeric-v))
          (55 67 "Rebate Amount" ,(fxrd-decimal-v :pad " "))
          (68 71 "Merchant Category Code" ,(fxrd-alphanumeric-v))
          (72 93 "Transaction Description" ,(fxrd-alphanumeric-v))
          (94 94 "Reversal Indicator" ,(fxrd-alphanumeric-v :enum '("Y" "N")))
          (95 116 "Merchant ID" ,(fxrd-alphanumeric-v))
          (117 122 "Issuer ICA Code" ,(fxrd-numeric-v))
          (123 124 "Program Code" ,(fxrd-alphanumeric-v))
          (125 144 "Bank Product Code" ,(fxrd-alphanumeric-v))
          (145 174 "Bank Customer Number" ,(fxrd-alphanumeric-v))
          (175 200 "Filler" ,(fxrd-alphanumeric-v))))
    ("T" (
          (1 1 "Record Type (T)" ,(fxrd-alphanumeric-v :const "T"))
          (2 13 "Record Count" ,(fxrd-numeric-v))
          (14 24 "Member ICA" ,(fxrd-numeric-v))
          (25 200 "Filler" ,(fxrd-alphanumeric-v))))))

;;;###autoload
(define-derived-mode rm37-mode fxrd-mode "RM37"
  "Major mode for editing RM37 fixed field width files.

\\{fxrd-mode-map}"
  (setq fxrd-current-spec rm37-spec))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.rm37\\($\\|\\.\\)" . rm37-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.rm39\\($\\|\\.\\)" . rm37-mode))

(provide 'rm37-mode)
