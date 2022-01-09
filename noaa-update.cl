;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2022-01-09 23:48:41>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ================
;;; Periodic updates
;;; ================

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defvar *download-timer*)
(defvar *cleanup-timer*)

(defun noaa-start-updates (&key (resolution '("1p00")) (max-offset 384))
  ;; Force download of previous cycle - even if the latest available cycle is complete,
  ;; previous cyclemay still be needed for interpolation.
  (download-cycle (previous-cycle (available-cycle (now)))
                  :resolution resolution)
  ;; Download the latest complete cycle
  (download-latest-cycle :resolution resolution :max-offset max-offset)
  ;; When a cycle is currently being output, start download immediately
  (when (cycle-updating-p)
    (download-cycle (current-cycle) :resolution resolution :max-offset max-offset :if-missing :wait))
  ;; NOW we can leave it to the 4/24 update. 
  (setf *download-timer*
        (timers:add-timer (lambda ()
                            (download-cycle (current-cycle) :resolution resolution :max-offset max-offset :if-missing :wait))
                          :id (format nil "GFS-~a-UPDATE" resolution)
                          :hours '(3 9 15 21)
                          :minutes '(30)))
  (setf *cleanup-timer*
        (timers:add-timer (lambda ()
                            (cleanup-cycles))
                          :id (format nil "CLEANUP-CYCLES" resolution)
                          :hours '(3 9 15 21)
                          :minutes '(10)))
  (noaa-start-forecast-ht-cleanup))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
