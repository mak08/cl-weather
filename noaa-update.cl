;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2019-07-05 21:08:33>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ================
;;; Periodic updates
;;; ================

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defvar *update-timer*)

(defun noaa-start-updates ()
  (download-latest-cycle)
  (when (cycle-updating-p)
    (multiple-value-bind (date cycle)
                                (current-cycle)
      (download-cycle date cycle :if-missing :wait)))
  (setf *update-timer*
        (timers:add-timer (lambda ()
                            (multiple-value-bind (date cycle)
                                (current-cycle)
                              (download-cycle date cycle :if-missing :wait)))
                      :id "WEATHER-UPDATER"
                      :hours '(3 9 15 21)
                      :minutes '(30))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
