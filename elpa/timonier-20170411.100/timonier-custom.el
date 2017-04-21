;;; timonier-custom.el --- Customization for timonier

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


(require 'timonier-version)



(defgroup timonier nil
  "Timonier"
  :group 'applications
  :link '(url-link :tag "Socyl" "https://github.com/nlamirault/timonier")
  :link '(emacs-commentary-link :tag "Commentary" "timonier"))


(defcustom timonier-k8s-proxy "http://localhost:8001"
  "The Kubernetes proxy."
  :type 'string
  :group 'timonier)


(defconst timonier--user-agent "timonier"
  "The user agent for Kubernetes.")



(provide 'timonier-custom)
;;; timonier-custom.el ends here
