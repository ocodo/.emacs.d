;; Loop examples

(loop
 for n from 1 to 10
 do (print (format  "I did this %s times" n)))

(loop
 with c = 100
 for n from 1 to 10
 do (print (format "%s * %s = %s" c n (* c n))))

(loop
 for n from 1 to 10
 collect (* 100 n))
