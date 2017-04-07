;;; timonier.el --- Manage Kubernetes Applications

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/timonier
;; Version: 0.1.0
;; Keywords: kubernetes, docker

;; Package-Requires: ((emacs "24.4") (s "1.11.0") (f "0.19.0") (dash "2.12.0") (pkg-info "0.5.0") (hydra "0.13.6") (request "0.2.0") (all-the-icons "2.0.0"))

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

;;; Installation:

;; Available as a package in melpa

;; (add-to-list 'package-archives
;;              '("melpa" . "https://stable.melpa.org/packages/") t)
;;
;; M-x package-install timonier

;;; Usage:


;;; Code:


(require 'timonier-version)
(require 'timonier-custom)
(require 'timonier-io)
(require 'timonier-utils)
(require 'timonier-k8s)
(require 'timonier-mode)


(provide 'timonier)
;;; timonier.el ends here
