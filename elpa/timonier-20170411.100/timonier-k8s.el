;;; timonier-k8s.el --- Kubernetes API for Timonier

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

(require 'cl-lib)

(require 'timonier-custom)
(require 'timonier-utils)
(require 'timonier-io)


(defmacro timonier--with-k8s (&rest body)
  `(condition-case err
       ,@body
     (error (message "[Kubernetes] Error with API: %s" err))))


(defun timonier--k8s-get-uri (uri)
  "Retrieve the Kubernetes API complete url using the Kubernetes proxy.
`URI' is the api path."
  (if timonier-k8s-proxy
      (s-concat timonier-k8s-proxy "/" uri)
    (error (signal 'timonier-k8s-error '("Kubernetes proxy unknown.")))))


(defun timonier--k8s-get-api ()
  "Retrieve the Kubernetes API."
  (timonier--perform-http-request
   "GET" (timonier--k8s-get-uri "/api") '() 200))


(defmacro timonier--with-k8s-api-version (api-version &rest body)
  "Retrieve Kubernetes API and set into `API-VERSION' and execute the forms in `BODY'."
  (declare (indent 1) (debug t))
  `(let* ((response (timonier--k8s-get-api))
          (,api-version (elt (cdadr response) 0)))
     (message "[k8s] API version: %s" ,api-version)
     (if ,api-version
         ,@body
       (message "[Timonier] Can't retrieve Kubernetes version: %s" response))))


(defun timonier--k8s-get-namespaces ()
  "Retrieve namespaces from a Kubernetes cluster."
  (timonier--with-k8s-api-version api-version
    (timonier--perform-http-request
     "GET" (timonier--k8s-get-uri (s-concat "/api/" api-version "/namespaces")) '() 200)))

(defun timonier--k8s-extract-namespace-informations (namespace)
  "Extract commons informations from `NAMESPACE'.
Result is a property list.  Keys are : 'name and 'status."
  (let ((metadata (timonier--assoc-cdr 'metadata namespace))
        (status (timonier--assoc-cdr 'status namespace))
        properties)
    (setq properties
          (list 'name (timonier--assoc-cdr 'name metadata)
                'status (timonier--assoc-cdr 'phase status)))
    properties))


(defun timonier--k8s-get-pods ()
  "Retrieve pods from a Kubernetes cluster."
  (timonier--with-k8s-api-version api-version
    (timonier--perform-http-request
     "GET" (timonier--k8s-get-uri (s-concat "/api/" api-version "/pods")) '() 200)))


(defun timonier--k8s-extract-pod-informations (pod)
  "Extract commons informations from `POD'.
Result is a property list.  Keys are : 'name,  'namespace and 'status."
  (let ((metadata (timonier--assoc-cdr 'metadata pod))
        (status (timonier--assoc-cdr 'status pod))
        properties)
    (setq properties
          (list 'name (timonier--assoc-cdr 'name metadata)
                'namespace (timonier--assoc-cdr 'namespace metadata)
                'cluster-ip (timonier--assoc-cdr 'podIP status)
                'status (timonier--assoc-cdr 'phase status)))
    properties))


(defun timonier--k8s-get-nodes ()
  "Retrieve nodes from a Kubernetes cluster.."
  (timonier--with-k8s-api-version api-version
    (timonier--perform-http-request
     "GET" (timonier--k8s-get-uri (s-concat "/api/" api-version "/nodes")) '() 200)))


(defun timonier--k8s-extract-node-informations (node)
  "Extract commons informations from `NODE'.
Result is a property list.  Keys are : 'name,  'labels and 'creation."
  (let ((metadata (timonier--assoc-cdr 'metadata node))
        (infos (timonier--assoc-cdr 'nodeInfo node))
        properties)
    (setq properties
          (list 'name (timonier--assoc-cdr 'name metadata)
                'labels (mapcar 'cdr (timonier--assoc-cdr 'labels metadata))
                'creation (timonier--assoc-cdr 'creationTimestamp metadata)))
    properties))


(defun timonier--k8s-extract-node-description (node)
  "Extract complete description from `NODE'."
  (let* ((metadata  (timonier--assoc-cdr 'metadata node))
         (spec (timonier--assoc-cdr 'spec node))
         (status (timonier--assoc-cdr 'status node))
         (addresses (timonier--assoc-cdr 'addresses status))
         (node-info (timonier--assoc-cdr 'nodeInfo status))
         properties)
    (setq properties
          (list 'name (timonier--assoc-cdr 'name metadata)
                'labels (mapcar 'cdr (timonier--assoc-cdr 'labels metadata))
                'creation (timonier--assoc-cdr 'creationTimestamp metadata)
                'external-id (timonier--assoc-cdr 'externalID spec)
                'addresses (mapcar (lambda (adr)
                                     (mapcar 'cdr adr))
                                   addresses)
                'os-image (timonier--assoc-cdr 'osImage node-info)
                'system-uuid (timonier--assoc-cdr 'systemUUID node-info)
                'boot-id (timonier--assoc-cdr 'bootID node-info)
                'kernel-version (timonier--assoc-cdr 'kernelVersion node-info)
                'container-runtime (timonier--assoc-cdr 'containerRuntimeVersion node-info)
                'kubelet-version (timonier--assoc-cdr 'kubeletVersion node-info)
                'kubeproxy-version (timonier--assoc-cdr 'kubeProxyVersion node-info)
                'os (timonier--assoc-cdr 'operatingSystem node-info)
                'architecture (timonier--assoc-cdr 'architecture node-info)))
    properties))

(defun timonier--k8s-get-services ()
  "Retrieve services from a Kubernetes cluster."
  (timonier--with-k8s-api-version api-version
    (timonier--perform-http-request
     "GET" (timonier--k8s-get-uri (s-concat "/api/" api-version "/services")) '() 200)))


(defun timonier--k8s-extract-service-informations (service)
  "Extract commons informations from `SERVICE'.
Result is a property list.  Keys are : 'name,  'namespace."
  (let ((metadata (timonier--assoc-cdr 'metadata service))
        (spec (timonier--assoc-cdr 'spec service))
        properties)
    (setq properties
          (list 'name (timonier--assoc-cdr 'name metadata)
                'namespace (timonier--assoc-cdr 'namespace metadata)
                'labels (mapcar 'cdr (timonier--assoc-cdr 'labels metadata))
                'ports (mapcar (lambda (port)
                                 (mapcar 'cdr port))
                               (timonier--assoc-cdr 'ports spec))
                'cluster-ip (timonier--assoc-cdr 'clusterIP spec)))
    properties))



(provide 'timonier-k8s)
;;; timonier-k8s.el ends here
