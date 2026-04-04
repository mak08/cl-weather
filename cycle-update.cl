;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2026-04-04 14:53:58>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Periodic updates

(defvar *cleanup-timer*)
(defparameter *datasources* '(noaa-gfs-wind
                              gfswave-combined
                              fw-current-agulhas
                              fw-current-east-australia
                              fw-current-english-channel))

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
