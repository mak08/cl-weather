;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-12-10 21:59:46>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ================
;;; Periodic updates
;;; ================

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defvar *update-timer*)
(defvar *forecast-cleanup-timer*)

(defun noaa-start-updates (&key (resolution "1p00") (max-offset 384))
  (download-latest-cycle :resolution resolution :max-offset max-offset)
  (when (cycle-updating-p)
    (download-cycle (current-cycle) :resolution resolution :max-offset max-offset :if-missing :wait))
  (setf *update-timer*
        (timers:add-timer (lambda ()
                            (download-cycle (current-cycle) :resolution resolution :max-offset max-offset :if-missing :wait))
                          :id (format nil "GFS-~a-UPDATE" resolution)
                          :hours '(3 9 15 21)
                          :minutes '(30))))

(defun noaa-start-cleanup ()
  (setf *forecast-cleanup-timer*
        (timers:add-timer #'noaa-forecast-ht-cleanup
                          :id "FORECAST-CLEANUP"
                          :hours '(5 11 17 23)
                          :minutes '(0))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
