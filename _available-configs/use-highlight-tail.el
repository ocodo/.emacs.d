(require 'highlight-tail)

;; Rainbow trail
(defun highlight-tail-rainbow-trail ()
  "Rainbow trail for highlight tail mode"
  (interactive)
  (highlight-tail-mode 0)
  (setq  highlight-tail-colors '(("#AA1DD8" . 0)
                                 ("#1E2AEE" . 20)
                                 ("#0E6286" . 40)
                                 ("#14860E" . 60)
                                 ("#866B0E" . 80)
                                 ("#862F0E" . 99)))
  (highlight-tail-mode 1))

;; Blue flame
(defun highlight-tail-blue-flame ()
  "Blue flame for highlight tail mode"
  (interactive)
  (highlight-tail-mode 0)
  (setq  highlight-tail-colors '(("#000000" . 0)
                                 ("#101793" . 1)
                                 ("#1341AD" . 5)
                                 ("#0A5B6C" . 10)
                                 ("#08505F" . 15)
                                 ("#064552" . 20)
                                 ("#053A45" . 25)
                                 ("#000000" . 30)))
  (highlight-tail-mode 1))

;; Red flame
(defun highlight-tail-red-flame ()
  "Red flame for highlight tail mode"
  (interactive)
  (highlight-tail-mode 0)
  (setq  highlight-tail-colors '(("#000000" . 0)
                                 ("#932A10" . 1)
                                 ("#DD1111" . 5)
                                 ("#6C1D0A" . 10)
                                 ("#5F1908" . 15)
                                 ("#521505" . 20)
                                 ("#451104" . 25)
                                 ("#000000" . 30)))
  (highlight-tail-mode 1))

;; Orange flame
(defun highlight-tail-orange-flame ()
  "Orange flame for highlight tail mode"
  (interactive)
  (highlight-tail-mode 0)
  (setq  highlight-tail-colors '(("#000000" . 0)
                                 ("#972F00" . 1)
                                 ("#FF6600" . 5)
                                 ("#922F00" . 15)
                                 ("#5F2C00" . 20)
                                 ("#521500" . 25)
                                 ("#351F00" . 30)
                                 ("#000000" . 35)))
  (highlight-tail-mode 1))

;; Green flame
(defun highlight-tail-green-flame ()
  "Green flame for highlight tail mode"
  (interactive)

  (highlight-tail-mode 0)
  (setq  highlight-tail-colors '(("#000000" . 0)
                                 ("#00FF00" . 1)
                                 ("#006600" . 5)
                                 ("#002F00" . 15)
                                 ("#002C00" . 20)
                                 ("#001500" . 25)
                                 ("#001F00" . 30)
                                 ("#000000" . 35)))
  (highlight-tail-mode 1))

(defun highlight-tail-cyan-flame ()
  "Cyan flame for highlight tail mode"
  (interactive)
  (highlight-tail-mode 0)
  (setq  highlight-tail-colors '(("#000000" . 0)
                                 ("#00FFFF" . 1)
                                 ("#006666" . 5)
                                 ("#002F2F" . 15)
                                 ("#002C2C" . 20)
                                 ("#001515" . 25)
                                 ("#001F1F" . 30)
                                 ("#000000" . 35)))
  (highlight-tail-mode 1))

(provide 'use-highlight-tail)

;;; use-highlight-tail ends here
