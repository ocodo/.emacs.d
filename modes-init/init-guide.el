;;; package --- init guide-key
;;; commentary:
;;; code:

(setq guide-key/guide-key-sequence
      '("C-x"
        "C-x ESC"
        "C-x RET"
        "C-x @"
        "C-x a"
        "C-x n"
        "C-x r"
        "C-x 8 1"
        "C-x 8 2"
        "C-x 8 /"
        "C-x 8 \""
        "C-x 8 '"
        "C-x 8 ^"
        "C-x 8 ~"
        "C-x 8 *"

        "C-c"
        "C-c @"
        "C-c !"
        "C-c &"
        "C-c p"
        "%"

        "s-<kp-0>"
        ))

(global-set-key (kbd "C-x n g") 'guide-key-mode)

(provide 'init-guide)
;;; init-guide.el ends here
