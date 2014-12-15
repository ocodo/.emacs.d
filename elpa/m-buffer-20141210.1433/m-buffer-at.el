;; Return information at location.
;; Stateless version of emacs code
(require 'm-buffer-macro)

(defun m-buffer-at-eolp (&rest location)
  "Return t if LOCATION is at the end of a line.
See also `eolp'."
  (m-buffer-with-current-location
      location
    (eolp)))

(defun m-buffer-at-bolp (&rest location)
  "Return t if LOCATION is at the begining of a line.
See also `bolp'"
  (m-buffer-with-current-location
      location
    (bolp)))

(defun m-buffer-at-line-beginning-position (&rest location)
  (m-buffer-with-current-location
      location
    (line-beginning-position)))

(defun m-buffer-at-line-end-position (&rest location)
  (m-buffer-with-current-location
      location
    (line-end-position)))

(provide 'm-buffer-at)
