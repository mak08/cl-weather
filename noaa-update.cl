;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2020-03-01 17:45:35>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ================
;;; Periodic updates
;;; ================

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defvar *update-timer*)

(defun noaa-start-updates (&key (max-offset 384))
  (download-latest-cycle :max-offset max-offset)
  (when (cycle-updating-p)
    (multiple-value-bind (date cycle)
                                (current-cycle)
      (download-cycle date cycle :max-offset max-offset :if-missing :wait)))
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
