(require 'elnode)
(require 'websocket)

(defvar elmd:ws-server)
(defvar elmd:ws-server-port 9898)
(defvar elmd:node-server-port 9899)
(defvar elmd:ws-server-closed nil)

(defconst elmd:routes
  `(("^/$"      . elmd:preview-handler)))

(setq elmd:ws-server
      (websocket-server elmd:ws-server-port

       :on-message
       (lambda (ws frame)
         (message "Server received text!")
         (websocket-send-text ws
                              (websocket-frame-payload frame)))

       :on-open
       (lambda (websocket) "Client connection opened!")

       :on-close
       (lambda (websocket)
         (setq elmd:ws-server-closed t))))


(defvar elmd:html-head (format "<!DOCTYPE html>
<html>
<head>
    <script type=\"application/javascript\" src=\"\" />
    <link href=\"%s\" media=\"all\" rel=\"stylesheet\" />
</head>
<body class=\"container\">" markdown-css-path))

(defvar elmd:html-foot (concat "</body></html>"))

(defun elmd:preview-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
  (elnode-http-return
   httpcon
   (concat elmd:html-head
           (with-current-buffer "*markdown-test*" (buffer-string))
           elmd:html-foot)))

(defun elmd:dispatcher (httpcon)
  (elnode-dispatcher httpcon elmd:routes))

(defvar elmd:ws-js (format
  "<script type='application/javascript'>
        $(function () {
            var ws = new WebSocket('ws://localhost:%s/markdown');
            ws.onopen = function () {
                console.log('connected');
            };
            ws.onclose = function (ev) {
                console.log('closed');
            };
            ws.onmessage = function (ev) {
                $('#preview').html(ev.data);
            };
            ws.onerror = function (ev) {
                console.log(ev);
            };
        });
   </script>" elmd:ws-server-port))

(elnode-start 'elmd:dispatcher :port 9899 :host "localhost")
