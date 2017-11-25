(defun php+-mode-unittests ()
  (interactive)
  (ert "php\\+-test"))

(defalias 'php+-mode-check-yo-self 'php+-mode-unittests)

(provide 'php+-mode-unittest-setup)
