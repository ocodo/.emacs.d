;;; timonier-utils.el --- Some tools for Timonier

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

(require 'json)
(require 'request)
(require 's)


(require 'timonier-version)
(require 'timonier-custom)


;; Errors

(define-error 'timonier-error "Timonier error")

(define-error 'timonier-k8s-error "Timonier Kubernetes Error" 'timonier-error)


;; HTTP tools


(defun timonier--get-headers ()
  "Generate HTTP headers for Travis API."
  `(("User-Agent" . ,(concat timonier--user-agent
                             "/"
                             (timonier--library-version)))))


(defun timonier--perform-http-request (method uri params status-code)
  "Do a HTTP METHOD request using URI and PARAMS.
If HTTP return code is STATUS-CODE, send the response content otherwise
raise an error."
  (let ((response (request uri
                           :type method
                           :headers (timonier--get-headers)
                           :sync t
                           :data params
                           :parser 'json-read)))
    (if (= status-code (request-response-status-code response))
        (request-response-data response)
      (error
       (signal 'travis-http-error
               (list (request-response-status-code response)
                     (request-response-data response)))))))






(provide 'timonier-utils)
;;; timonier-utils.el ends here
