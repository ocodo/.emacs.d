(require 'fxrd-mode)
(require 'fxrd-validators)

;;; Nacha transaction file
(defconst nacha-spec
  ;; File Header Record
  `(("1" (
          (1 1 "Record Type 1 (File header)" ,(fxrd-numeric-v :const 1))
          (2 3 "Priority Code" ,(fxrd-numeric-v :const 1))
          (4 13 "Immediate Destination" ,(fxrd-numeric-v :pad " "))
          (14 23 "Immediate Origin" ,(fxrd-numeric-v))
          (24 29 "File Creation Date" ,(fxrd-numeric-v)) ;TODO: date type
          (30 33 "File Creation Time" ,(fxrd-numeric-v)) ;TODO: time type
          (34 34 "File ID Modifier" ,(fxrd-alphanumeric-v))
          (35 37 "Record Size" ,(fxrd-numeric-v :const 94))
          (38 39 "Blocking Factor" ,(fxrd-numeric-v :const 10))
          (40 40 "Format Code" ,(fxrd-numeric-v :const 1))
          (41 63 "Immediate Destination Name" ,(fxrd-alphanumeric-v))
          (64 86 "Immediate Origin Name" ,(fxrd-alphanumeric-v))
          (87 94 "Reference Code" ,(fxrd-alphanumeric-v))))
    ;; Batch Header Record
    ("5" (
          (1 1 "Record Type 5 (Company/batch header)" ,(fxrd-numeric-v :const 5))
          (2 4 "Service Class Code" ,(fxrd-numeric-v :enum '(200 220 225)))
          (5 20 "Company Name" ,(fxrd-alphanumeric-v))
          (21 40 "Company Discretionary Data" ,(fxrd-alphanumeric-v))
          (41 50 "Company Identification" ,(fxrd-alphanumeric-v))
          (51 53 "Standard Entry Class Code" ,(fxrd-alphanumeric-v
                                               :enum '("PPD" "CCD" "CTX")))
          (54 63 "Company Entry Description" ,(fxrd-alphanumeric-v))
          (64 69 "Company Descriptive Date" ,(fxrd-alphanumeric-v))
          (70 75 "Effective Entry Date" ,(fxrd-numeric-v))
          (76 78 "Settlement Date" ,(fxrd-alphanumeric-v))
          (79 79 "Originator Status Code" ,(fxrd-numeric-v :const 1))
          (80 87 "Originating DFI Identification" ,(fxrd-numeric-v))
          (88 94 "Batch Number" ,(fxrd-numeric-v))))
    ;; Entry Detail Record/Report
    ("6" (
          (1 1 "Record Type 6 (Entry detail)" ,(fxrd-numeric-v :const 6))
          ;; TODO: transaction code parsing/descriptions
          (2 3 "Transaction Code" ,(fxrd-numeric-v :enum '(22 23 27 28 32 33 37 38)))
          (4 11 "Receiving DFI Identification" ,(fxrd-numeric-v))
          (12 12 "Check Digit" ,(fxrd-numeric-v))
          (13 29 "DFI Account Number" ,(fxrd-alphanumeric-v))
          (30 39 "Amount" ,(fxrd-numeric-v :min 0))
          (40 54 "Individual Identification Number" ,(fxrd-alphanumeric-v))
          (55 76 "Individual Name" ,(fxrd-alphanumeric-v))
          (77 78 "Discretionary Data" ,(fxrd-alphanumeric-v))
          (79 79 "Addenda Record Indicator" ,(fxrd-numeric-v :enum '(0 1)))
          (80 94 "Trace Number" ,(fxrd-numeric-v))))
    ;; CCD Addenda Record
    ("7" (
          (1 1 "Record Type 7 (Addenda)" ,(fxrd-numeric-v :const 7))
          (2 3 "Addenda Type Code" ,(fxrd-numeric-v :const 5))
          (4 83 "Payment Related Information" ,(fxrd-alphanumeric-v))
          (84 87 "Addenda Sequence Number" ,(fxrd-numeric-v))
          (88 94 "Entry Detail Sequence Number" ,(fxrd-numeric-v))))
    ;; Batch Control Record
    ("8" (
          (1 1 "Record Type 8 (Company/batch control)" ,(fxrd-numeric-v :const 8))
          (2 4 "Service Class Code" ,(fxrd-numeric-v))
          (5 10 "Entry/Addenda Count" ,(fxrd-numeric-v))
          (11 20 "Entry Hash" ,(fxrd-numeric-v))
          (21 32 "Total Debit Entry Dollar Amount" ,(fxrd-numeric-v))
          (33 44 "Total Credit Entry Dollar Amount" ,(fxrd-numeric-v))
          (45 54 "Company Identification" ,(fxrd-alphanumeric-v))
          (55 73 "Message Authentication Code" ,(fxrd-alphanumeric-v)) ;TODO: reserved
          (74 79 "Reserved" ,(fxrd-alphanumeric-v))
          (80 87 "Originating DFI Identification" ,(fxrd-numeric-v))
          (88 94 "Batch Number" ,(fxrd-numeric-v))))
    ;; File Control Record
    ("9" (
          (1 1 "Record Type 9 (File control)" ,(fxrd-numeric-v :const 9))
          (2 7 "Batch Count" ,(fxrd-numeric-v))
          (8 13 "Block Count" ,(fxrd-numeric-v))
          (14 21 "Entry/Addenda Count" ,(fxrd-numeric-v))
          (22 31 "Entry Hash" ,(fxrd-numeric-v))
          (32 43 "Total Debit Entry Dollar Amount in File" ,(fxrd-numeric-v))
          (44 55 "Total Credit Entry Dollar Amount in File" ,(fxrd-numeric-v))
          (56 94 "Reserved" ,(fxrd-alphanumeric-v))))))


;;;###autoload
(define-derived-mode nacha-mode fxrd-mode "NACHA"
"Major mode for editing NACHA fixed field width files.

\\{fxrd-mode-map}"
(setq fxrd-current-spec nacha-spec))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.nacha\\($\\|\\.\\)" . nacha-mode))

(provide 'nacha-mode)
