(require 'fxrd-mode)
(require 'fxrd-validators)

;;; Mastercard rebate confirmation file
(defconst tso6-spec
  ;; Header Record
  `(("H" (
          (1 1 "Record Type (H)" ,(fxrd-alphanumeric-v :const "H"))
          (2 9 "Record Date" ,(fxrd-numeric-v))
          (10 15 "Record Time" ,(fxrd-numeric-v))
          (16 26 "Member ICA" ,(fxrd-numeric-v))
          ;; Undocumented, but supported by Mastercard
          (27 86 "File Name" ,(fxrd-alphanumeric-v))
          (87 201 "Filler" ,(fxrd-alphanumeric-v))))
    ;; Data Record
    ("D" (
          (1 1 "Record Type (D)" ,(fxrd-alphanumeric-v :const "D"))
          (2 31 "Bank Customer Number" ,(fxrd-alphanumeric-v))
          (32 50 "Bank Account Number" ,(fxrd-numeric-v :pad " "))
          (51 70 "Bank Product Code" ,(fxrd-alphanumeric-v))
          (71 92 "Transaction Description" ,(fxrd-alphanumeric-v))
          (93 105 "Rebate Amount" ,(fxrd-decimal-v :pad " "))
          (106 106 "Exception Reason Code" ,(fxrd-alphanumeric-v
                                             :enum '("A" "C" "I" "M" "O" "R")))
          ;; TODO: account for padding in enum
          (107 136 "Exception Reason Description" ,(fxrd-alphanumeric-v
                                                    :enum '("Account Not Found"
                                                            "Customer Not Found"
                                                            "Invalid Account"
                                                            "Multiple Accounts Found"
                                                            "Others"
                                                            "Invalid Account Country")))
          (137 144 "Rebate File Sent Date" ,(fxrd-numeric-v)) ;TODO: date
          (145 157 "Transaction Sequence Number" ,(fxrd-numeric-v :pad " "))
          (158 201 "Filler" ,(fxrd-alphanumeric-v))))
    ;; Trailer Record
    ("T" (
          (1 1 "Record Type (T)" ,(fxrd-alphanumeric-v :const "T"))
          (2 13 "Exception Record Count" ,(fxrd-numeric-v))
          (14 25 "Success Record Count" ,(fxrd-numeric-v))
          (26 37 "Total Processed Record Count" ,(fxrd-numeric-v))
          (38 48 "Member ICA" ,(fxrd-numeric-v))
          (49 201 "Filler" ,(fxrd-alphanumeric-v))))))

;;;###autoload
(define-derived-mode tso6-mode fxrd-mode "TSO6"
  "Major mode for editing TSO6 fixed field width files.

\\{fxrd-mode-map}"
  (setq fxrd-current-spec tso6-spec))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.tso6\\($\\|\\.\\)" . tso6-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.tso8\\($\\|\\.\\)" . tso6-mode))

(provide 'tso6-mode)
