(when

    (and (window-system) (eq system-type 'darwin))

  ;;
  ;; extended f keys on Apple keyboard (chicklet style, extended)
  ;;

  (global-set-key (kbd "<f13>") 'overwrite-mode )
  (global-set-key (kbd "<f14>") 'ibuffer )
  (global-set-key (kbd "<f15>") 'magit-status )
  (global-set-key (kbd "<f16>") 'select-themes )

  )

(provide 'custom-mac-fn-keys)
