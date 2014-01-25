(when

    (and (window-system) (eq system-type 'darwin))

  ;;
  ;; extended f keys on mac keyboard
  ;;

  (global-set-key (kbd "<f13>") 'overwrite-mode )
  (global-set-key (kbd "<f14>") 'ibuffer )
  (global-set-key (kbd "<f15>") 'magit-status )
  (global-set-key (kbd "<f16>") 'load-theme )
  (global-set-key (kbd "<f17>") 'disable-theme )
  (global-set-key (kbd "<f18>") nil )
  (global-set-key (kbd "<f19>") 'package-list-packages )

  )

(provide 'custom-mac-fn-keys)
