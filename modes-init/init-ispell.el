;; init-ispell
(when (file-exists-p "/usr/local/bin/aspell")
  (set-variable 'ispell-program-name "/usr/local/bin/aspell"))
(provide 'init-ispell)