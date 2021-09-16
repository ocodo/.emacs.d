;;; emms-volume-sndioctl.el --- a mode for changing volume using sndioctl -*- lexical-binding: t; -*-

;; Copyright (C) 2006, 2007, 2008, 2009, 2019 Free Software Foundation, Inc.

;; Authors: Omar Polo <op@omarpolo.com>

;; This file is part of EMMS.

;; EMMS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; EMMS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EMMS; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file defines a few simple functions to raise or lower the
;; volume using sndioctl. It can be used stand-alone, though it's
;; meant for usage with EMMS, particularly with emms-volume.el

;;; History:

;; Sep 09 2021: Based on emms-volume-mixerctl.el by Omar Polo

;;; Code:
(require 'subr-x)

(defcustom emms-volume-sndioctl-stream "output"
  "The stream to change the volume with.
Usually it's the global \"output\".  For a full list of available
controls, run `sndioctl' in a shell."
  :type '(choice (const :tag "output" "output")
                 (string :tag "Something else: "))
  :group 'emms-volume)

(defcustom emms-volume-sndioctl-device nil
  "The card number to change volume.
The card is identified by a number.  For a full list run `ls
/dev/mixer?*' in a shell."
  :type '(choice (const :tag "none" nil)
                 (string :tag "Device: "))
  :group 'emms-volume)

;;;###autoload
(defun emms-volume-sndioctl-change (amount)
  "Change sndioctl level by AMOUNT."
  (message "Playback channels: %s"
           (with-temp-buffer
             (when (zerop
                    (apply #'call-process
                           "sndioctl" nil (current-buffer) nil
                           `("-n"
                             ,@(when emms-volume-sndioctl-device
                                 `("-f" ,emms-volume-sndioctl-device))
                             ,(format "%s.level=%s%f"
                                      emms-volume-sndioctl-stream
                                      (if (> amount 0) "+" "")
                                      (/ (float amount) 100)))))
               (string-trim-right (buffer-string))))))

(provide 'emms-volume-sndioctl)

;;; emms-volume-sndioctl.el ends here
