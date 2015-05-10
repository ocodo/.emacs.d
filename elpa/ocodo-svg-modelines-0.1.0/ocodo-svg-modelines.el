;;; ocodo-svg-modelines --- A collection of artisan SVG modelines
;;
;; Author: ocodo <what.is.ocodo@gmail.com>
;;
;; Package-Requires: ((svg-mode-line-themes))
;; Version: 0.1.0
;; URL: https://github.com/ocodo/ocodo-svg-modelines
;;
;;; Commentary:
;;
;;  # Ocodo SVG modelines
;;
;;  A completely superfluous, but otherwise most excellent collection
;;  of awesome modelines... if not now, then at some point in the
;;  future, you might want, need and maybe even love these.
;;
;;  <sub>Don't worry if this isn't true, it's just promotional bullshit.</sub>
;;
;;  Very much a work in progress, these modelines are relatively
;;  sparse, however, any info or widgets you'd like to have, please
;;  post an issue on the github repository
;;  https://github.com/ocodo/ocodo-svg-modelines/issues and I'll
;;  attempt to cater to you.

;;; Code:

(require 'svg-mode-line-themes)

;;;###autoload
(defun ocodo-smt-collection-one/init ()
  "Initialize Ocodo's SVG Modeline collection."
  (interactive)
  (when (image-type-available-p 'svg)
    (smt/enable)
    (require 'ocodo-kawaii-light-smt)
    (require 'ocodo-minimal-light-smt)
    (require 'ocodo-minimal-dark-smt)
    (require 'ocodo-steps-aqua-smt)
    (require 'ocodo-steps-grass-smt)
    (require 'ocodo-mesh-aqua-smt)
    (require 'ocodo-mesh-grass-smt)))

(provide 'ocodo-svg-modelines)
;;; ocodo-svg-modelines
