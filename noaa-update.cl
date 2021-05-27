;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-05-28 00:01:24>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ================
;;; Periodic updates
;;; ================

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defvar *update-timer*)
(defvar *forecast-cleanup-timer*)

(defun noaa-start-updates (&key (max-offset 384))
  (download-latest-cycle :max-offset max-offset)
  (when (cycle-updating-p)
    (download-cycle (current-cycle) :max-offset max-offset :if-missing :wait))
  (setf *forecast-cleanup-timer*
        (timers:add-timer #'noaa-forecast-ht-cleanup
                          :id "FORECAST-CLEANUP"
                          :hours '(5 11 17 23)
                          :minutes '(0)))
  (setf *update-timer*
        (timers:add-timer (lambda ()
                            (download-cycle (current-cycle) :max-offset max-offset :if-missing :wait))
                          :id "WEATHER-UPDATER"
                          :hours '(3 9 15 21)
                          :minutes '(30))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
