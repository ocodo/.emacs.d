;;; ocodo-svg-modelines --- A collection of artisan SVG modelines
;;
;;; Author: ocodo <what.is.ocodo@gmail.com>
;;          Jason Milkins <jasonm23@gmail.com>
;;
;;; Version: 0.1.0
;;
;;; Commentary:
;;  A completely superfluous, but otherwise most excellent and awesome
;;  modelines... if not now, then at some point in the future, you will
;;  both want, love and need these.
;;
;;  Very much a work in progress, these modelines are relatively
;;  sparse, however, any info or widgets you'd like to have, please
;;  post an issue on the github repository
;;  https://github.com/ocodo/ocodo-svg-modelines/issues and we'll
;;  attempt to cater to you.
;;
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
