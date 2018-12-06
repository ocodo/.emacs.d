;;; peek-mode.el --- Serve buffers live over HTTP with elnode backend
;;; (i.e., impatient-mode ported to elnode)
 
;; LICENSE: GPL 2 or later
 
;; Author: Erik Iverson <erik@sigmafield.org>
;; Version: 0.1
;; URL: https://github.com/erikriverson/peek-mode
;; Package-Requires: ((elnode "0.9.8.1"))
 
;;; Commentary:
 
;; peek-mode is a minor mode that publishes live Emacs buffers through
;; an elnode server under
;; http://<server>:<port>/peek/<buffer>. Turning on peek-mode in a
;; buffer will publish it. To unpublish a buffer, toggle peek-mode
;; off.
 
;; peek-mode is (very very!) largely based on Brian Taylor's
;; <el.wubo@gmail.com> impatient-mode
;; <https://github.com/netguy204/imp.el> However, impatient-mode does
;; not use elnode, but rather a different emacs httpd backend
;; <https://github.com/skeeto/emacs-http-server>. I consider peek-mode
;; as "impatient-mode ported to elnode".
 
;; Start the elnode server (`elnode-start') and visit
;; http://<server>:<port>/peek/. There will be a listing of all the
;; buffers that currently have peek-mode enabled. You can evaluate the
;; line below to start the elnode server on localhost:8008 with the
;; proper dispatcher, assuming the code in this file is available by
;; having loaded it. 
;; (elnode-start 'peek-mode-dispatcher-handler :port 8008 :host "localhost")

;; Because html-mode buffers are sent raw, you can use peek-mode
;; see your edits to an HTML document live!
 
;; To receive updates the browser issues a long poll on the client
;; waiting for the buffer to change -- server push. The response
;; happens in an `after-change-functions' hook. Buffers that do not
;; run these hooks will not be displayed live to clients.
 
;;; Code:
 
(require 'cl)
(require 'url-util)
(require 'elnode)
 
(defgroup peek-mode nil
  "Serve buffers live over HTTP using Emacs elnode as a backend."
  :group 'comm)
 
(defvar peek-mode-map (make-sparse-keymap)
  "Keymap for peek-mode.")
 
(defvar peek-client-list ()
  "List of client processes watching the current buffer.")
 
(defvar peek-last-state 0
  "State sequence number.")
 
(defvar peek-related-files nil
  "Files that seem to be related to this buffer")
 
;;;###autoload
(define-minor-mode peek-mode
  "Serves the buffer live over HTTP."
  :group 'peek-mode
  :lighter " peek"
  :keymap peek-mode-map
  (make-local-variable 'peek-client-list)
  (make-local-variable 'peek-last-state)
  (make-local-variable 'peek-related-files)
  (if peek-mode
      (add-hook 'after-change-functions 'peek-on-change nil t)
    (remove-hook 'after-change-functions 'peek-on-change t)))
 
(defvar peek-shim-root (file-name-directory load-file-name)
  "Location of data files needed by peek-mode.")
 
(defun peek-buffer-enabled-p (buffer)
  "Return t if buffer has peek-mode enabled."
  (and buffer (with-current-buffer (get-buffer buffer) peek-mode)))
 
(defun peek-buffer-list ()
  "List of all buffers with peek-mode enabled"
  (remove-if-not 'peek-buffer-enabled-p (buffer-list)))
 

(defun peek-serve-buffer-list (httpcon)
  "Serve a list of peekable buffers."
  (elnode-send-html httpcon (concat "<html><head>\n
     <title>peek-mode buffer list</title>
     </head><body><h1>Peekable Buffers</h1>\n<hr/><ul>"
                    (mapconcat
                     (lambda(buffer)
                       (format "<li><a href=\"live/%s/\">%s</a></li>\n"
                                       (url-hexify-string (buffer-name buffer))
                                       (url-insert-entities-in-string (buffer-name buffer))))
                     (peek-buffer-list) "\n")
                    "</ul>\n<hr/>"
                    "Enable <code>peek-mode</code> in buffers to add them to this list."
                    "</body></html>")))
 

(defun peek-live-buffer (httpcon)          
  "Serve up the shim that lets us watch a buffer change"
  (let* ((path (elnode-http-pathinfo httpcon))
	 (index (expand-file-name "index.html" peek-shim-root))
         (parts (cdr (split-string path "/")))
         (buffer-name (nth 2 parts))
         (file (mapconcat 'identity (nthcdr 3 parts) "/"))
         (buffer (get-buffer buffer-name))
         (buffer-file (buffer-file-name buffer))
         (buffer-dir (and buffer-file (file-name-directory buffer-file))))
 
    (cond
     ((equal (file-name-directory path) "/peek/live/")
      (elnode-send-redirect httpcon (concat path "/")))
     ((not (peek-buffer-enabled-p buffer)) (peek-private httpcon buffer-name))
     ((and (not (string= file "")) buffer-dir)
      (let* ((full-file-name (expand-file-name file buffer-dir))
             (live-buffer (remove-if-not
                           (lambda (buf) (equal full-file-name (buffer-file-name buf)))
                           (peek-buffer-list))))
        (add-to-list 'peek-related-files full-file-name)
        (if (not live-buffer)
                    (elnode-send-file httpcon full-file-name))))
     (t (peek-buffer-enabled-p buffer)
                (elnode-send-file httpcon index)))))

 
(defconst peek-mode-urls
  '(("peek/$" . peek-buffer-list-handler)
    ("peek/buffer/.*$" . peek-long-poll-handler)
    ("peek/live/.*js$" . peek-live-js-handler)
    ("peek/live/.*$" . peek-live-buffer-handler)))

(defun peek-live-js-handler (httpcon)
  (let* ((path (elnode-http-pathinfo httpcon))
         (parts (cdr (split-string path "/")))
         (buffer-name (nth 2 parts))
         (file (mapconcat 'identity (nthcdr 3 parts) "/"))
	 (master-buffer (get-buffer buffer-name))
         (buffer (get-buffer file))
         (buffer-file (buffer-file-name buffer))
         (buffer-dir (and buffer-file (file-name-directory buffer-file)))
	 (full-file-name (expand-file-name file buffer-dir)))

    (with-current-buffer master-buffer
      (add-to-list 'peek-related-files full-file-name))
    
    (elnode-http-start httpcon 200 '("Cache-Control" . "no-cache")
		       '("Content-Type" . "application/javascript")
                       '("Connection" . "keep-alive" ))
    (elnode-http-return httpcon
			(with-current-buffer buffer 
			  (buffer-substring-no-properties 
			   (point-min) (point-max))))))

 
(defun peek-buffer-list-handler (httpcon)
   (peek-serve-buffer-list httpcon))
 
(defun peek-long-poll-handler (httpcon)
   (peek-long-poll-receive httpcon))
 
(defun peek-live-buffer-handler (httpcon)
   (peek-live-buffer httpcon))

(defun peek-mode-dispatcher-handler (httpcon)
  (elnode-dispatcher httpcon peek-mode-urls))
 
(defun peek-send-state (httpcon)
  (let ((id (number-to-string peek-last-state))
        (buffer (current-buffer)))
    (elnode-http-start httpcon 200 '("Cache-Control" . "no-cache")
		       '("Content-Type" . "text/plain")
                       '("Connection" . "keep-alive" ))
    (elnode-http-return httpcon
			(concat id " " (buffer-substring-no-properties
					(point-min) (point-max))))))
 

(defun peek-send-state-ignore-errors (httpcon)
  (condition-case error-case
      (peek-send-state httpcon)
    (error nil)))
 
(defun peek-notify-clients ()
  (while peek-client-list
    (peek-send-state-ignore-errors (pop peek-client-list))))
 
(defun peek-on-change (&rest args)
  "Hook for after-change-functions."
  (incf peek-last-state)
 
  ;; notify our clients
  (peek-notify-clients)
 
  ;; notify any clients that we're in the peek-related-files list for
  (let ((buffer-file (buffer-file-name (current-buffer))))
    (dolist (buffer (peek-buffer-list))
      (with-current-buffer buffer
        (when (member buffer-file peek-related-files)
          (peek-notify-clients))))))
 
(defun peek-private (httpcon buffer-name)
  (elnode-send-status httpcon 403
               (format "Buffer <code>%s</code> is <strong>not-peekable</strong>
                        or does not exist." buffer-name)))
 
(defun peek-long-poll-receive (httpcon)
  "Servlet that accepts long poll requests."
  (let* ((path (elnode-http-pathinfo httpcon))
	 (query (elnode--http-query-to-alist (elnode-http-query httpcon)))
	 (buffer-name (file-name-nondirectory path))
         (buffer (get-buffer buffer-name))
         (req-last-id (string-to-number
		       (or (cdr (assoc "id" query)) "0"))))
    (if (peek-buffer-enabled-p buffer)
        (with-current-buffer buffer
          (if (equal req-last-id peek-last-state)
	      (push httpcon peek-client-list)
            (peek-send-state-ignore-errors httpcon)))
      (peek-private httpcon buffer-name)))
  (elnode-defer-now 'peek-long-poll-receive))

(provide 'peek-mode)
;;; peek-mode.el ends here
