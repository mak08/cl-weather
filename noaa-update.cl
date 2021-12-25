;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-12-25 19:20:45>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ================
;;; Periodic updates
;;; ================

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defvar *download-timer*)

(defun noaa-start-updates (&key (resolution '("1p00")) (max-offset 384))
  (download-latest-cycle :resolution resolution :max-offset max-offset)
  (when (cycle-updating-p)
    (download-cycle (current-cycle) :resolution resolution :max-offset max-offset :if-missing :wait))
  (setf *download-timer*
        (timers:add-timer (lambda ()
                            (download-cycle (current-cycle) :resolution resolution :max-offset max-offset :if-missing :wait))
                          :id (format nil "GFS-~a-UPDATE" resolution)
                          :hours '(3 9 15 21)
                          :minutes '(30)))
  (noaa-start-forecast-ht-cleanup))


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
