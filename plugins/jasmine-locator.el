;;; opl-jasmine-locator --- locate a jasmine spec in opsmanager from it's spec runner URL.
;;
;;; Author: Jason Milkins <jason@opsmanager.com>
;;
;;; PackageRequires: ((s . "0") (hydra . "0"))
;;
;;; Commentary:
;;  This package provides an interface to the jasmine_locator script in OpsManager
;;
;;  Test URLs:
;;
;;  http://localhost:3000/jasmine?spec=OPL.ViewModels.DailyDrillingReports.Personnel.IndexViewModel%20on%20load%20when%20there%20is%20at%20least%20one%20DDR%20in%20the%20location%20availableSubcontractorPositions%20is%20loaded%20from%20the%20DDR%5C%27s%20contract%5C%27s%20contract%20rates%5C%27%20names
;;  http://localhost:3000/jasmine?spec=OPL.ViewModels.DailyDrillingReports.Personnel.IndexViewModel
;;
;;; Code:

(require 's)
(require 'hydra)

(defvar opl-jasmine-locator-command "~/workspace/OpsManager/script/jasmine_locator")

;;;###autoload
(defun opl-jasmine-locator (url)
  "Jump to a jasmine spec from the given URL."
  (interactive (list (read-from-minibuffer "Jasmine url: ")))
  (unless (s-contains? "spec=" url)
    (error "Error %s is not a jasmine runner URL" url))
  (let* ((jasmine-location (shell-command-to-string (format "%s -eu %S" opl-jasmine-locator-command url)))
         (location-parts (s-split " " jasmine-location)))
    (unless (s-match "\+[0-9]+ " jasmine-location)
      (error "Error no match found for URL: %s" url))
    (find-file (s-chomp (second location-parts)))
    (goto-char 0)
    (forward-line (1- (string-to-number (first location-parts))))
    (recenter)))

;;;###autoload
(defun opl-jasmine-locator-pbpaste ()
  "Jump to a jasmine spec from the a URL held in the OSX pasteboard."
  (interactive)
  (let* ((url (shell-command-to-string "pbpaste")))
    (unless (s-contains? "spec=" url)
      (error "Error %s is not a jasmine runner URL" url))
    (let* ((jasmine-location (shell-command-to-string (format "%s -eu %S" opl-jasmine-locator-command url)))
           (location-parts (s-split " " jasmine-location)))
      (unless (s-match "\+[0-9]+ " jasmine-location)
        (error "Error no match found for URL: %s" url))
      (find-file (s-chomp (second location-parts)))
      (goto-char 0)
      (forward-line (1- (string-to-number (first location-parts))))
      (recenter))))

(defhydra jasmine-hydra (ctl-x-map "j" :color blue)
  "Jasmine helpers"
  ("l" opl-jasmine-locator-pbpaste "locate from url (in clipboard)")
  ("u" opl-jasmine-locator "locate from url")
  ("t" jasmine-coffee/toggle-code-and-spec "toggle code and spec")
  ("i" jasmine-coffee/verify-it "verify it")
  ("g" jasmine-coffee/verify-group "verify group"))

(provide 'jasmine-locator)

;; Hi-lock: (("\\(jasmine-coffee/[^ ]*\\)" (1 ' font-lock-keyword-face append)) ("\\(opl-[^ ]*\\)" (1 ' font-lock-keyword-face append)) ("(\\(defhydra\\)" (1 ' font-lock-keyword-face append)))
;; Hi-lock: end
;; Local Variables:
;; eval: (hi-lock-mode)
;; End:

;;; jasmine-locator.el ends here
