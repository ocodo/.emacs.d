;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'quail)
(require 'gnu-apl-symbols)

(quail-define-package "APL-Z" "UTF-8" "⍞" t
                      "Input mode for APL"
                      '(("\t" . quail-completion))
                      t                 ; forget-last-selection
                      nil               ; deterministic
                      nil               ; kbd-translate
                      t                 ; show-layout
                      nil               ; create-decode-map
                      nil               ; maximum-shortest
                      nil               ; overlay-plist
                      nil               ; update-translation-function
                      nil               ; conversion-keys
                      t                 ; simple
                      )

(quail-select-package "APL-Z")

(macrolet ((make-quail-define-rules ()
             (let ((prefix "."))
               `(quail-define-rules
                 ,@(loop for command in gnu-apl--symbols
                         for key-command = (third command)
                         append (loop for s in (if (listp key-command) key-command (list key-command))
                                      collect (list (concat prefix s)
                                                    (second command))))
                 (,(concat prefix prefix) ,prefix)))))
  (make-quail-define-rules))

(provide 'gnu-apl-input)
