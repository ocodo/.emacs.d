(defun re-backward-match-group (rexp &optional n)
  "Grab the previous matches of regexp and return the contents of
  the n match group (first group match if no n arg is specified)"
  (save-excursion
  (unless n
    (setq n 1))
  (when (numberp n)
    (re-search-backward-lax-whitespace rexp)
    (when  (= (+ 2 (* n 2)) (length (match-data)))
      (match-string-no-properties n)))))
