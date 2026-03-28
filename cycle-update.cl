;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2026-03-19 22:05:26>

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

  (start-forecast-ht-cleanup))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
