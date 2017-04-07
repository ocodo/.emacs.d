;;; timonier-mode.el --- Mode for timonier

;; Copyright (C) 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;;; Code:

(require 'widget)
(require 'cl-lib)

(require 'all-the-icons)
(require 'f)
(require 'hydra)
(require 's)

(require 'timonier-custom)
(require 'timonier-io)
(require 'timonier-k8s)


;; ------------------
;; Customization
;; ------------------

(defgroup timonier-k8s-mode nil
  "Customization group for `timonier-k8s-mode'."
  :prefix "timonier-k8s-mode-"
  :tag "Timonier Kubernetes Mode"
  :group 'timonier)

(defcustom timonier-k8s-mode-buffer "*timonier-k8s*"
  "The Timonier kubernetes buffer name."
  :type 'string
  :group 'timonier-k8s-mode)

(defcustom timonier-k8s-description-mode-buffer "*timonier-k8s-desc*"
  "The Timonier kubernetes entity description buffer name."
  :type 'string
  :group 'timonier-k8s-mode)

(defcustom timonier-k8s-keymap-prefix "C-c C-k"
  "Prefix for `timonier-k8s-mode'."
  :group 'timonier-k8s-mode)

(defcustom timonier-k8s-mode-line
  " Timonier/Kubernetes "
  "Mode line lighter for Timonier."
  :group 'timonier-k8s-mode
  :type 'sexp
  :risky t)


;; ------------------
;; Faces
;; ------------------

(defgroup timonier-k8s-mode-faces '((timonier-k8s-mode custom-group))
  "Customization group for the faces of `timonier-k8s-mode'."
  :prefix "timonier-k8s-mode-"
  :tag "Timonier Kubernetes mode faces"
  :group 'timonier-k8s-mode)

(defface timonier-k8s-mode-title-face
  '((t :inherit font-lock-builtin-face))
  "Face used for buffer's title."
  :group 'timonier-k8s-mode)

(defface timonier-k8s-mode-key-face
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face used in the Timonier buffer."
  :group 'timonier-k8s-mode-faces)

(defface timonier-k8s-mode-pod-face
  '((t :weight bold :inherit font-lock-warning-name-face))
  "Face used in the Timonier buffer."
  :group 'timonier-k8s-mode-faces)

(defface timonier-k8s-mode-service-face
  '((t :weight bold :inherit font-lock-warning-name-face))
  "Face used in the Timonier buffer."
  :group 'timonier-k8s-mode-faces)

(defface timonier-k8s-mode-node-face
  '((t :weight bold :inherit font-lock-warning-name-face))
  "Face used in the Timonier buffer."
  :group 'timonier-k8s-mode-faces)

(defface timonier-k8s-mode-namespace-face
  '((t :inherit font-lock-doc-face))
  "Face used in the Timonier buffer"
  :group 'timonier-k8s-mode-faces)

(defface timonier-k8s-mode-pod-status-face
  '((t :inherit font-lock-string-face))
  "Face used in the Timonier buffer"
  :group 'timonier-k8s-mode-faces)

(defface timonier-k8s-mode-namespace-status-face
  '((t :inherit font-lock-string-face))
  "Face used in the Timonier buffer"
  :group 'timonier-k8s-mode-faces)

(defvar timonier-k8s-mode-padding 2
  "The number of columns used for padding on the left side of the buffer.")


;; ------------------
;; Modes
;; ------------------


(defmacro timonier-k8s-mode-with-widget (title &rest body)
  `(progn
     (set-buffer (get-buffer-create timonier-k8s-mode-buffer))
     (switch-to-buffer timonier-k8s-mode-buffer)
     (kill-all-local-variables)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (remove-overlays)
       (widget-insert (format "\n[%s]\n\n" ,title))
       ,@body)
     (use-local-map widget-keymap)
     (widget-setup)
     (timonier-k8s-mode)
     (widget-minor-mode)
     (goto-char 0)))


(defmacro timonier-k8s-mode-description-with-widget (title &rest body)
  `(progn
     (set-buffer (get-buffer-create timonier-k8s-description-mode-buffer))
     (switch-to-buffer timonier-k8s-description-mode-buffer)
     (kill-all-local-variables)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (remove-overlays)
       (widget-insert (format "\n[%s]\n\n" ,title))
       ,@body)
     (use-local-map widget-keymap)
     (widget-setup)
     (timonier-k8s-description-mode)
     (widget-minor-mode)
     (goto-char 0)))


(defvar timonier-k8s-mode-hook nil)


(defun timonier-k8s-mode-quit ()
  "Timonier exit."
  (interactive)
  (kill-buffer timonier-k8s-mode-buffer))


(defun timonier--k8s-mode-current-entity (type)
  "Return the current Kubernetes entities at point using `TYPE'."
  (get-text-property (point) type))


(defun timonier--k8s-mode-next-entity (type)
  "Move point to the next Kubernetes entity specified by `TYPE'."
  (interactive)
  (let ((pos (next-single-property-change (point) type)))
    (if pos
        (progn
          (goto-char pos)
          (unless (timonier--k8s-mode-current-entity type)
            (let ((pos (next-single-property-change pos type)))
              (when pos
                (goto-char pos)))))
      (message "No current position: %s" type))))


(defun timonier--k8s-mode-prev-entity (type)
  "Move point to the previous Kubernetes entity specified by `TYPE'."
  (interactive)
  (let ((pos (previous-single-property-change (point) type)))
    (if pos
        (progn
          (goto-char pos)
          (unless (timonier--k8s-mode-current-entity type)
            (let ((pos (previous-single-property-change pos type)))
              (when pos
                (goto-char pos)))))
      (message "No current position: %s" type))))


(defun timonier-k8s-mode-next-pod ()
  "Move point to the next Kubernetes pod."
  (interactive)
  (timonier--k8s-mode-next-entity :k8s-pod))


(defun timonier-k8s-mode-prev-pod ()
  "Move point to the previous Kubernetes pod."
  (interactive)
  (timonier--k8s-mode-prev-entity :k8s-pod))


(defun timonier-k8s-mode-next-service ()
  "Move point to the next Kubernetes service."
  (interactive)
  (timonier--k8s-mode-next-entity :k8s-service))


(defun timonier-k8s-mode-prev-service ()
  "Move point to the previous Kubernetes service."
  (interactive)
  (timonier--k8s-mode-prev-entity :k8s-service))

(defun timonier-k8s-mode-next-node ()
  "Move point to the next Kubernetes node."
  (interactive)
  (timonier--k8s-mode-next-entity :k8s-node))


(defun timonier-k8s-mode-prev-node ()
  "Move point to the previous Kubernetes node."
  (interactive)
  (timonier--k8s-mode-prev-entity :k8s-node))


(defun timonier-k8s-mode-describe-node ()
  (interactive)
  (let ((node (timonier--k8s-mode-current-entity :k8s-node)))
    (if node
        (progn
          (let ((node-data (timonier--k8s-extract-node-description node)))
            (timonier-k8s-mode-description-with-widget
             (propertize "Kubernetes / Nodes"
                         'face 'timonier-k8s-mode-title-face)
             (widget-insert
              (format "\n%s:"
                      (propertize "Details"
                                  'face 'timonier-k8s-mode-title-face))
              (format "\n%s: %s"
                      (propertize "Name"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize (plist-get node-data 'name)
                                  'face 'timonier-k8s-mode-pod-face))
              (format "\n%s: %s"
                      (propertize "Labels"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize
                       (s-join " " (plist-get node-data 'labels))))
              (format "\n%s: %s"
                      (propertize "External ID"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize (plist-get node-data 'external-id)
                                  'face 'timonier-k8s-mode-pod-face))
              (format "\n\n%s:"
                      (propertize "System info"
                                  'face 'timonier-k8s-mode-title-face))
              (format "\n%s: %s"
                      (propertize "System UUID"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize (plist-get node-data 'system-uuid)
                                  'face 'timonier-k8s-mode-pod-face))
              (format "\n%s: %s"
                      (propertize "Boot ID"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize (plist-get node-data 'boot-id)
                                  'face 'timonier-k8s-mode-pod-face))
              (format "\n%s: %s"
                      (propertize "Kernel Version"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize (plist-get node-data 'kernel-version)
                                  'face 'timonier-k8s-mode-pod-face))
              (format "\n%s: %s"
                      (propertize "OS Image"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize (plist-get node-data 'os-image)
                                  'face 'timonier-k8s-mode-pod-face))
              (format "\n%s: %s"
                      (propertize "Container Runtime Version"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize (plist-get node-data 'container-runtime)
                                  'face 'timonier-k8s-mode-pod-face))
              (format "\n%s: %s"
                      (propertize "Kubelet Version"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize (plist-get node-data 'kubelet-version)
                                  'face 'timonier-k8s-mode-pod-face))
              (format "\n%s: %s"
                      (propertize "Kube-Proxy Version"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize (plist-get node-data 'kubeproxy-version)
                                  'face 'timonier-k8s-mode-pod-face))
              (format "\n%s: %s"
                      (propertize "Operation system"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize (plist-get node-data 'os)
                                  'face 'timonier-k8s-mode-pod-face))
              (format "\n%s: %s"
                      (propertize "Architecture"
                                  'face 'timonier-k8s-mode-key-face)
                      (propertize (plist-get node-data 'architecture)
                                  'face 'timonier-k8s-mode-pod-face))
              ))))
      (message "No node available."))))


(defun timonier-k8s-mode-describe-pod ()
  (interactive)
  (let ((pod (timonier--k8s-mode-current-entity :k8s-pod)))
    (if pod
        (message "Pod %s" pod)
      (message "No pod available."))))



(defun timonier-k8s-mode-describe-service ()
  (interactive)
  (let ((service (timonier--k8s-mode-current-entity :k8s-service)))
    (if service
        (message "%s" service)
      (message "No service available."))))



(defhydra timonier-k8s-hydra (:color blue :hint none)
  "
             [ Timonier / Kubernetes ]

^Node^                      ^Service^                      ^Pod^
--------------------------------------------------------------------------------------
_i_: go to next node        _k_: go to next service        _g_: go to next pod
_j_: go to previous node    _l_: go to previous service    _h_: go to previous pod
_N_: describe current node  _S_: describe current service  _P_: describe current pod

_q_: quit
"
  ("g" timonier-k8s-mode-next-pod)
  ("h" timonier-k8s-mode-prev-pod)
  ("P" timonier-k8s-mode-describe-pod)

  ("k" timonier-k8s-mode-next-service)
  ("l" timonier-k8s-mode-prev-service)
  ("S" timonier-k8s-mode-describe-service)

  ("i" timonier-k8s-mode-next-node)
  ("j" timonier-k8s-mode-prev-node)
  ("N" timonier-k8s-mode-describe-node)

  ("q"  nil "quit" :hint t :color red))


(defvar timonier-k8s-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd timonier-k8s-keymap-prefix) 'timonier-k8s-hydra/body)
    map))


(define-derived-mode timonier-k8s-mode tabulated-list-mode
  "Timonier Kubernetes mode"
  "Major mode for Timonier."
  :group 'timonier
  )


(defun timonier-k8s-description-mode-quit ()
  "Quit the Kubernetes description mode."
  (interactive)
  (kill-buffer timonier-k8s-description-mode-buffer))


(defvar timonier-k8s-description-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "q") 'timonier-k8s-description-mode-quit)
    map))


(define-derived-mode timonier-k8s-description-mode tabulated-list-mode
  "Timonier Kubernetes mode"
  "Major mode for Timonier."
  :group 'timonier
  )


(defun timonier--k8s-mode-render-pod (pod)
  "Render a Kubernetes `POD' to the Timonier buffer."
  ;; (message "Pod: %s" pod)
  (let* ((pod-data (timonier--k8s-extract-pod-informations pod)))
    (insert (all-the-icons-octicon "package"))
    (let ((start (point)))
      (widget-insert
       (format " %s %s"
               (propertize (plist-get pod-data 'name)
                           'face 'timonier-k8s-mode-pod-face)
               (propertize (plist-get pod-data 'status)
                           'face 'timonier-k8s-mode-pod-status-face)))
      (put-text-property start (point) :k8s-pod pod)
      (widget-insert
       (format  "\n   %s: %s\n\n"
                (propertize "Namespace"
                            'face 'timonier-k8s-mode-key-face)
                (propertize (plist-get pod-data 'namespace)
                            'face 'timonier-k8s-mode-namespace-face))))))


(defun timonier--k8s-mode-render-pods (pods)
  "Render Kubernetes `PODS'."
  (widget-insert (format "\n== PODS ==\n\n"))
  (dotimes (i (length pods))
    (let ((pod (elt pods i)))
      (timonier--k8s-mode-render-pod pod)))
  (widget-insert "\n"))


(defun timonier--k8s-mode-render-service (service)
  "Render a Kubernetes `SERVICE' to the Timonier buffer."
  (let* ((service-data (timonier--k8s-extract-service-informations service)))
    (insert (all-the-icons-octicon "link-external"))
    (let ((start (point)))
      (widget-insert
       (format " %s"
               (propertize (plist-get service-data 'name)
                           'face 'timonier-k8s-mode-service-face)))
      (put-text-property start (point) :k8s-service service)
      (widget-insert
       (format "\n  %s: %s"
               (propertize "Namespace"
                           'face 'timonier-k8s-mode-key-face)
               (propertize (plist-get service-data 'namespace)
                           'face 'timonier-k8s-mode-namespace-face))
       (format "\n  %s: %s"
               (propertize "ClusterIP"
                           'face 'timonier-k8s-mode-key-face)
               (plist-get service-data 'cluster-ip))
       (format "\n  %s: %s"
               (propertize "Endpoints"
                           'face 'timonier-k8s-mode-key-face)
               (propertize
                (s-join ":" (mapcar (lambda (elt)
                                      (format "%s" elt))
                                    (plist-get service-data 'ports)))))
       (format "\n  %s: %s\n\n"
               (propertize "Labels"
                           'face 'timonier-k8s-mode-key-face)
               (propertize
                (s-join " " (plist-get service-data 'labels))))))))


(defun timonier--k8s-mode-render-services (services)
  "Render Kubernetes `SERVICES'."
  (widget-insert (format "\n== SERVICES ==\n\n"))
  (dotimes (i (length services))
    (let ((service (elt services i)))
      (timonier--k8s-mode-render-service service)))
  (widget-insert "\n"))


(defun timonier--k8s-mode-render-node (node)
  "Render a Kubernetes `NODE' to the Timonier buffer."
  (let* ((node-data (timonier--k8s-extract-node-informations node)))
    (insert (all-the-icons-octicon "server"))
    (let ((start (point)))
      (widget-insert
       (format " %s %s"
               (propertize (plist-get node-data 'name)
                           'face 'timonier-k8s-mode-service-face)
               (propertize (plist-get node-data 'creation)
                           'face 'timonier-k8s-mode-namespace-face)))
      (put-text-property start (point) :k8s-node node)
      (widget-insert
       (format "\n  %s: %s\n\n"
               (propertize "Labels"
                           'face 'timonier-k8s-mode-key-face)
               (propertize
                (s-join " " (plist-get node-data 'labels))))))))


(defun timonier--k8s-mode-render-nodes (nodes)
  "Render Kubernetes `NODES'."
  (widget-insert (format "\n== NODES ==\n\n"))
  (dotimes (i (length nodes))
    (let ((node (elt nodes i)))
      (timonier--k8s-mode-render-node node)))
  (widget-insert "\n"))



(defun timonier--k8s-mode-render-namespace (namespace)
  "Render a Kubernetes `NAMESPACE' to the Timonier buffer."
  (let* ((namespace-data (timonier--k8s-extract-namespace-informations namespace)))
    (insert (all-the-icons-octicon "organization"))
    (let ((start (point)))
      (widget-insert
       (format " %s %s\n"
               (propertize (plist-get namespace-data 'name)
                           'face 'timonier-k8s-mode-pod-face)
               (propertize (plist-get namespace-data 'status)
                           'face 'timonier-k8s-mode-namespace-status-face)))
      (put-text-property start (point) :k8s-namespace namespace))))


(defun timonier--k8s-mode-render-namespaces (namespaces)
  "Render Kubernetes `NAMESPACES'."
  (widget-insert (format "\n== NAMESPACES ==\n\n"))
  (dotimes (i (length namespaces))
    (let ((namespace (elt namespaces i)))
      (timonier--k8s-mode-render-namespace namespace)))
  (widget-insert "\n"))


;; ------------------
;; API
;; ------------------

(defvar timonier-k8s-mode-history nil)


;;;###autoload
(defun timonier-k8s ()
  "Display informations about the Kubernetes cluster."
  (interactive)
  (timonier--with-k8s
   (timonier-k8s-mode-with-widget
    (propertize "Kubernetes"
                'face 'timonier-k8s-mode-title-face)
    (let ((namespaces (timonier--assoc-cdr 'items (timonier--k8s-get-namespaces)))
          (nodes (timonier--assoc-cdr 'items (timonier--k8s-get-nodes)))
          (services (timonier--assoc-cdr 'items (timonier--k8s-get-services)))
          (pods (timonier--assoc-cdr 'items (timonier--k8s-get-pods))))
      (timonier--k8s-mode-render-namespaces namespaces)
      (timonier--k8s-mode-render-nodes nodes)
      (timonier--k8s-mode-render-services services)
      (timonier--k8s-mode-render-pods pods)
      ))))


(provide 'timonier-mode)
;;; timonier-mode.el ends here
