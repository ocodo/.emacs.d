;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Usage:
;;
;;   1. Start streaming daemon at command line.
;;
;;     $ emacs -Q --daemon=hoge -l "/path/to/simple-httpd.el" -l airplay-video-server.el
;;
;;   2. Start streaming from client emacs.
;;
;;     (server-eval-at "hoge" '(airplay/server:start "/path/to/movie.mp4"))
;;
;;   3. Enjoy!
;;
;;     (airplay/video:view "192.168.0.3:8080")
;;
(require 'simple-httpd)

(add-to-list 'httpd-status-codes '(206 . "Partial Content"))

(setq httpd-mime-types
      (append '(("ts" . "video/MP2T")
                ("mov" . "video/quicktime")
                ("m4v" . "video/mp4"))
              httpd-mime-types))

(defun airplay/server:--request-ranges (headers)
  (let ((range (cadr (assoc "Range" headers))))
    (when range
      (if (null (string-match "^bytes=\\([0-9]*\\)-\\([0-9]*\\)" range))
          (httpd-log '(warning (format "Invalid range header: %s" range)))
        (let ((beg (match-string 1 range))
              (end (match-string 2 range)))
          (cons (if (zerop (length beg)) nil (string-to-number beg))
                (if (zerop (length end)) nil (string-to-number end))))))))

(defun airplay/server:--accept-ranges (range file-size)
  (let ((beg (car range))
        (end (cdr range)))
    (when (null beg)
      (setq beg
            (if (null end) 0
              (setq beg (- file-size end))))
      (setq end (1- file-size)))
    (cons beg (or end (1- file-size)))))

(defun airplay/server:--response-video (proc path &optional req)
  "Response back to the client for the streaming of PATH.
Assumes \"Partial Responses\" and \"Range Requests\" (RFC 2616)."
  (let ((mtime (httpd-date-string (nth 4 (file-attributes path))))
        (file-size (nth 7 (file-attributes path)))
        (req-range (airplay/server:--request-ranges req))
        (status 200)
        headers acc-range range-beg range-end)
    (httpd-log `(file ,path))
    (when req-range
      (setq status 206)
      (setq acc-range (airplay/server:--accept-ranges req-range file-size))
      (setq range-beg (car acc-range))
      (setq range-end (1+ (cdr acc-range)))
      (setq headers `(:Last-Modified ,mtime
                      :Accept-Ranges "bytes"
                      :Content-Range ,(format "bytes %d-%d/%d"
                                              range-beg (1- range-end) file-size))))
    (setq headers (plist-put headers :Last-Modified mtime))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path nil range-beg range-end)
      (apply 'httpd-send-header proc
             (httpd-get-mime (file-name-extension path))
             status
             headers))))

(defun airplay/server:start (media)
  (lexical-let ((media media))
    (defun httpd/ (proc _no_ _use_ request)
      (airplay/server:--response-video proc media request)))
  (httpd-start))

