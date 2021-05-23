;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-05-23 20:42:27>


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
    (multiple-value-bind (date cycle)
                                (current-cycle)
      (download-cycle date cycle :max-offset max-offset :if-missing :wait)))
  (setf *forecast-cleanup-timer*
        (timers:add-timer #'noaa-forecast-ht-cleanup
                          :id "FORECAST-CLEANUP"
                          :hours '(5 11 17 23)
                          :minutes '(0)))
  (setf *update-timer*
        (timers:add-timer (lambda ()
                            (multiple-value-bind (date cycle)
                                (current-cycle)
                              (download-cycle date cycle :max-offset max-offset :if-missing :wait)))
                          :id "WEATHER-UPDATER"
                          :hours '(3 9 15 21)
                          :minutes '(30))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
