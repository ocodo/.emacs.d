;;
;; Macro Support
;;
(defmacro m-buffer-with-markers (varlist &rest body)
  "Bind variables after VARLIST then eval BODY.
All variables should contain markers or collections of markers.
All markers are niled after BODY."
  ;; indent let part specially.
  (declare (indent 1)(debug let))
  ;; so, create a rtn var with make-symbol (for hygene)
  (let* ((rtn-var (make-symbol "rtn-var"))
         (marker-vars
          (mapcar 'car varlist))
         (full-varlist
          (append
           varlist
           `((,rtn-var
              (progn
                ,@body))))))
    `(let* ,full-varlist
       (m-buffer-nil-marker
        (list ,@marker-vars))
       ,rtn-var)))

(defmacro m-buffer-with-current-marker
    (marker &rest body)
  "At MARKER location run BODY."
  (declare (indent 1) (debug t))
  `(with-current-buffer
       (marker-buffer ,marker)
     (save-excursion
       (goto-char ,marker)
       ,@body)))

(defmacro m-buffer-with-current-position
    (buffer location &rest body)
  "In BUFFER at LOCATION, run BODY."
  (declare (indent 2)
           (debug t))
  `(with-current-buffer
       ,buffer
     (save-excursion
       (goto-char ,location)
      ,@body)))

(defmacro m-buffer-with-current-location
    (location &rest body)
  "At LOCATION, run BODY.
LOCATION should be a list. If a one element list, it is a marker.
If a two element, it is a buffer and position."
  (declare (indent 1) (debug t))
  ;; multiple eval of location!
  (let ((loc (make-symbol "loc")))
    `(let ((,loc ,location))
       (if (= 1 (length ,loc))
           (m-buffer-with-current-marker
               (nth 0 ,loc)
             ,@body)
         (if (= 2 (length ,loc))
             (m-buffer-with-current-position
                 (nth 0 ,loc)
                 (nth 1 ,loc)
               ,@body)
           (t
            (error "m-buffer-with-current-location requires a list of one or two elements")))))))


(provide 'm-buffer-macro)
