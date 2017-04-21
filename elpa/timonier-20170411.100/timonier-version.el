;;; timonier.el --- Version tools for Timonier

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


(require 'dash)
(require 'pkg-info)
(require 's)


(defun timonier--library-version ()
  "Get the version in the timonier library."
  (-when-let (version (pkg-info-library-version 'timonier))
    (pkg-info-format-version version)))


;;;###autoload
(defun timonier-version (&optional show-version)
  "Get the timonier version as string.
If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.
The returned string includes both, the version from package.el
and the library version, if both a present and different.
If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list (not (or executing-kbd-macro noninteractive))))
  (let* ((version (timonier--library-version)))
    (unless version
      (error "Could not find out timonier version"))
    (message "timonier %s" version)
    version))




(provide 'timonier-version)
;;; timonier-version.el ends here
