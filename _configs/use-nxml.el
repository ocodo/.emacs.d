;; nxml mode use
(add-hook 'nxml-mode-hook (lambda ()
                            (setq Lorem-ipsum-paragraph-separator "<br><br>\n"
                                  Lorem-ipsum-sentence-separator "&nbsp;&nbsp;"
                                  Lorem-ipsum-list-beginning "<ul>\n"
                                  Lorem-ipsum-list-bullet "<li>"
                                  Lorem-ipsum-list-item-end "</li>\n"
                                  Lorem-ipsum-list-end "</ul>\n")
                            (setq rng-schema-locating-files (list "~/.emacs.d/nxml-schemas/schemas.xml" "schemas.xml"))))

(add-to-list 'auto-mode-alist
             '("\\.\\(x[ms]l\\|rng\\)\\'" . nxml-mode))

(setq
 nxml-child-indent 2
 nxml-outline-child-indent 2
 nxml-slash-auto-complete-flag t)

(provide 'use-nxml)
