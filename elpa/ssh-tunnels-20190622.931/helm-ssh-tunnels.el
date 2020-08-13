;;; helm-ssh-tunnels.el --- Helm interface for managing SSH tunnels

;; Author: death <github.com/death>
;; Version: 1.0
;; Package-Requires: ((cl-lib "0.5") (emacs "24") (helm "1.9.9"))
;; Keywords: tools, convenience, helm
;; URL: http://github.com/death/ssh-tunnels

;; This file is not part of GNU Emacs.

;; Copyright (c) 2018 death

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package lets you view, run, and kill SSH tunnels using Helm.
;; To use it, do the following:
;;
;; - Configure ssh-tunnels.
;;
;; - Type M-x helm-ssh-tunnels RET.
;;
;; - You should see the list of tunnels; running tunnels will have 'R'
;;   in their state column.
;;
;; - To run the tunnel at the current line, press F2.
;;
;; - To kill a running tunnel, press F3.
;;
;; - To toggle the tunnel's state (run it if it is inactive or kill it if it is
;;   running), use the default or persistent action.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'ssh-tunnels)

(defun helm-ssh-tunnels--format-tunnel (tunnel)
  (format "%s %-20s %-30s %-34s"
          (if (ssh-tunnels--check tunnel) "R" " ")
          (ssh-tunnels--pretty-name (ssh-tunnels--property tunnel :name))
          (ssh-tunnels--property tunnel :login)
          (format "%d:%s:%d"
                  (ssh-tunnels--property tunnel :local-port)
                  (ssh-tunnels--property tunnel :host)
                  (ssh-tunnels--property tunnel :remote-port))))

(defun helm-ssh-tunnels--get-candidates ()
  (cl-loop for tunnel in ssh-tunnels-configurations
           collect (cons (helm-ssh-tunnels--format-tunnel tunnel) tunnel)))

(defun helm-ssh-tunnels--persistent-action (candidate)
  (ssh-tunnels--toggle-state candidate)
  (helm-refresh))

(defun helm-ssh-tunnels ()
  "Show helm interface to ssh-tunnels."
  (interactive)
  (helm
   :buffer "*helm-ssh-tunnels*"
   :prompt "Tunnel: "
   :sources (helm-build-in-buffer-source "SSH tunnels"
              :candidates #'helm-ssh-tunnels--get-candidates
              :persistent-action #'helm-ssh-tunnels--persistent-action
              :action '(("Toggle state" . ssh-tunnels--toggle-state)
                        ("Run" . ssh-tunnels--run)
                        ("Kill" . ssh-tunnels--kill)))))

(provide 'helm-ssh-tunnels)

;;; helm-ssh-tunnels.el ends here
