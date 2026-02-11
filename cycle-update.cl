;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2026-02-11 21:11:14>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Periodic updates

(defvar *cleanup-timer*)
(defvar *datasources* '(noaa-gfs-wind ecmwf-ifs-wind ecmwf-aifs-wind fw-current-agulhas))

(defun start-cycle-updates ()
  ;; Download the latest complete cycle
  (dolist (datasource *datasources*)
    (cl-weather::download-datasource
     (get-datasource datasource (latest-complete-cycle datasource))
     :async :datasource))
  ;; When a cycle is currently being output, start download immediately
  (dolist (datasource *datasources*)
    (when (cycle-updating-p datasource)
      (cl-weather::download-datasource
       (get-datasource datasource (current-cycle datasource))
       :async :datasource)))

  ;; Now we can leave it to the 4/24 update. 
  (dolist (datasource *datasources*)
    (cl-weather::schedule-download datasource))
  (setf *cleanup-timer*
        (timers:add-timer (lambda ()
                            (cleanup-cycles :dry-run nil))
                          :id (format nil "CLEANUP-CYCLES" resolution)
                          :hours '(3 9 15 21)
                          :minutes '(10)))
  (start-forecast-ht-cleanup))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
