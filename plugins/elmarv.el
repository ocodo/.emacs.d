;;; elmarv -- Emacs lisp markdown viewer or elmarv

;; Author: Jason Milkins <jasonm23@gmail.com>

;;; Commentary:

;;  Emacs lisp markdown viewer or elmarv

;;; Code:

(require 'markdown-mode)
(require 'elnode)
(require 'websocket)

(defvar elmarv:jquery-cdn-url "http://oss.maxcdn.com/jquery/2.1.3/jquery.min.js")
(defvar elmarv:ws-conn)
(defvar elmarv:ws-server)
(defvar elmarv:ws-port 9898)
(defvar elmarv:elnode-port 9899)
(defvar elmarv:ws-server-closed nil)

;; Elnode server routing table
(defconst elmarv:routes
  `(("^/$" . elmarv:preview-handler)))

(defun elmarv:ws-start ()
  "Start elmarv websocket server and emacs connection."
  (setq elmarv:ws-server
        (websocket-server elmarv:ws-port
         :on-open
         (lambda (websocket) "Client connection opened!")

         :on-close
         (lambda (websocket)
           (setq elmarv:ws-server-closed t))))

  (setq elmarv:ws-conn
        (websocket-open
         (format "ws://localhost:%s" elmarv:ws-port))))

(defun elmarv:ws-stop ()
  (websocket-server-close elmarv:ws-server))

(defun elmarv:send-to-server ()
  (markdown)
  (websocket-send-text
   elmarv:ws-conn
   (with-current-buffer markdown-output-buffer-name (buffer-string))))

(defvar elmarv:html-head (format "<!DOCTYPE html>
<html>
  <head>
    <link href=\"%s\" media=\"all\" rel=\"stylesheet\" />
    <script type=\"application/javascript\" src=\"%s\" />
  </head>
  <body id=\"preview\" class=\"container\">" markdown-css-path elmarv:jquery-cdn-url))

(defvar elmarv:ws-js (format
                      "
    <script type='application/javascript'>
        $(function () {
            var ws = new WebSocket('ws://localhost:%s/');
            ws.onopen = function () {
                console.log('connected');
            };
            ws.onclose = function (ev) {
                console.log('closed');
            };
            ws.onmessage = function (ev) {
                console.log('received message')
                console.log(ev.data)
                $('#preview').html(ev.data);
            };
            ws.onerror = function (ev) {
                console.log('error');
                console.log(ev);
            };
        });
    </script>" elmarv:ws-port))

(defvar elmarv:html-foot (format "
    %s
  </body>
</html>" elmarv:ws-js))

(defun elmarv:preview-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
  (elnode-http-return
   httpcon
   (concat elmarv:html-head
           (with-current-buffer markdown-output-buffer-name (buffer-string))
           elmarv:html-foot)))

(defun elmarv:server (httpcon)
  (elnode-dispatcher httpcon elmarv:routes))

(defun elmarv:init ()
  (elmarv:ws-start)
  (elnode-start 'elmarv:server
                :port elmarv:elnode-port
                :host "localhost")
  (add-hook 'kill-emacs-hook 'elmarv:shutdown)
  (add-hook 'post-command-hook 'elmarv:send-to-server nil t))

(defun elmarv:shutdown ()
  (elmarv:ws-stop)
  (elnode-stop elmarv:elnode-port))

(define-minor-mode elmarv-mode
  "EmacsLisp markdown viewer."
  :group 'elmarv
  :init-value nil
  :global nil
  (if elmarv-mode
      (elmarv:init)
      (elmarv:shutdown)))

(provide 'elmarv)
;;; elmarv.el ends here
