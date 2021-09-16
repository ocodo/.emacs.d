;;; jq-format.el --- Reformat JSON and JSONLines using jq  -*- lexical-binding: t; -*-

;; Copyright © 2019 wouter bolsterlee

;; Author: wouter bolsterlee <wouter@bolsterl.ee>
;; Keywords: languages
;; Package-Version: 20190428.1434
;; Package-Commit: 47e1c5adb89b37b4d53fe01302d8c675913c20e7
;; URL: https://github.com/wbolster/emacs-jq-format
;; Package-Requires: ((emacs "24") (reformatter "0.3"))
;; Version: 1.0.0

;; (this is the osi approved 3-clause "new bsd license".)

;; all rights reserved.

;; redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; * redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.

;; * redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.

;; * neither the name of the author nor the names of the contributors
;;   may be used to endorse or promote products derived from this
;;   software without specific prior written permission.

;; this software is provided by the copyright holders and contributors
;; "as is" and any express or implied warranties, including, but not
;; limited to, the implied warranties of merchantability and fitness
;; for a particular purpose are disclaimed. in no event shall the
;; copyright holder or contributors be liable for any direct,
;; indirect, incidental, special, exemplary, or consequential damages
;; (including, but not limited to, procurement of substitute goods or
;; services; loss of use, data, or profits; or business interruption)
;; however caused and on any theory of liability, whether in contract,
;; strict liability, or tort (including negligence or otherwise)
;; arising in any way out of the use of this software, even if advised
;; of the possibility of such damage.

;;; Commentary:

;; Commands for easy JSON (and jsonlines) reformatting using jq.

;;; Code:

(require 'reformatter)

(defgroup jq-format nil
  "JSON reformatting using jq."
  :group 'json)

(defcustom jq-format-command "jq"
  "Name of the jq executable."
  :group 'jq-format
  :type 'string)

(defcustom jq-format-sort-keys t
  "Whether to sort keys."
  :group 'jq-format
  :type 'boolean)

(defcustom jq-format-extra-args nil
  "Extra arguments to pass to jq."
  :group 'jq-format
  :type '(repeat string))

;;;###autoload (autoload 'jq-format-json-buffer "jq-format" nil t)
;;;###autoload (autoload 'jq-format-json-region "jq-format" nil t)
;;;###autoload (autoload 'jq-format-json-on-save-mode "jq-format" nil t)
(reformatter-define jq-format-json
  :program jq-format-command
  :args (jq-format--make-args)
  :lighter " JSONFmt"
  :group 'jq-format)

;;;###autoload (autoload 'jq-format-jsonlines-buffer "jq-format" nil t)
;;;###autoload (autoload 'jq-format-jsonlines-region "jq-format" nil t)
;;;###autoload (autoload 'jq-format-jsonlines-on-save-mode "jq-format" nil t)
(reformatter-define jq-format-jsonlines
  :program jq-format-command
  :args (append '("--compact-output") (jq-format--make-args))
  :lighter " JSONLFmt"
  :group 'jq-format)

(defun jq-format--make-args ()
  "Helper to build the argument list for jq."
  (append
   (when jq-format-sort-keys '("--sort-keys"))
   jq-format-extra-args
   '("." "-")))

(provide 'jq-format)
;;; jq-format.el ends here
