(define-package "impatient-mode" "20181002.1231" "Serve buffers live over HTTP"
  '((cl-lib "0.3")
    (simple-httpd "1.5.0")
    (htmlize "1.40"))
  :authors
  '(("Brian Taylor" . "el.wubo@gmail.com"))
  :maintainer
  '("Brian Taylor" . "el.wubo@gmail.com")
  :url "https://github.com/netguy204/imp.el")
;; Local Variables:
;; no-byte-compile: t
;; End:
