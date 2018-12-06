;;; redis-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "redis" "redis.el" (0 0 0 0))
;;; Generated autoloads from redis.el

(autoload 'redis-mode "redis" "\
Major mode for `redis' commands.

BEWARE: This is mode is for pseudo redis scripts which uses
\"#\" (number sign) as comment line. You can send the pseudo
scripts to redis removing the comments:

     $ grep -v '^#' myscript.redis | nc 127.0.0.1 6379

\\<redis-mode-map>

\(fn)" t nil)

(autoload 'redis-send-buffer-with-protocol "redis" "\
Send the buffer content to redis-cli executable with ARGS.

If PROTOCOL is not nil it sends to using redis protocol.  See:
http://redis.io/topics/protocol

\(fn &rest ARGS)" t nil)

(autoload 'redis-send-buffer-content "redis" "\
Send the contents of the current buffer to a redis PROCESS.

\(fn &optional PROCESS)" t nil)

(autoload 'redis-send-region-content "redis" "\
Send the contents from region BEGIN to END to a redis PROCESS.

\(fn BEGIN END &optional PROCESS)" t nil)

(autoload 'redis-send-current-line "redis" "\
Send current line to redis PROCESS.

\(fn &optional PROCESS)" t nil)

(autoload 'redis-cli-mode "redis" "\
Major mode for `redis-cli'.

\\<redis-cli-mode-map>

\(fn)" t nil)

(defalias 'run-redis #'redis-cli)

(autoload 'redis-cli "redis" "\
Run redis-cli process with ARGS.

\(fn &rest ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "redis" '("redis-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; redis-autoloads.el ends here
