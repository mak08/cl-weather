;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2026-04-04 14:53:58>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Periodic updates

(defvar *cleanup-timer*)
(defparameter *datasources* '(noaa-gfs-wind
                              ecmwf-ifs-wind
                              ecmwf-aifs-wind
                              ecmwf-ifs-current
                              gfswave-combined
                              arome-wind
                              fw-current-agulhas
                              fw-current-english-channel))

(defun startup-download-cycles (datasource time)
  "Plan completed and publishing runs at one timestamp, without duplicate downloads."
  (remove-duplicates
   (cons (latest-scheduled-cycle datasource time :availability :end)
         (publishing-cycles datasource time))
   :test (lambda (a b) (timestamp= (cycle-timestamp a) (cycle-timestamp b)))))

(defun start-cycle-updates (&optional (time (now)))
  "Recover complete and in-progress runs before enabling periodic downloads."
  (dolist (datasource *datasources*)
    (dolist (cycle (startup-download-cycles datasource time))
      (download-datasource (get-datasource datasource cycle) :async :datasource)))

  ;; Now we can leave it to the 4/24 update. 
  (dolist (datasource *datasources*)
    (cl-weather::schedule-download datasource))

  (start-forecast-ht-cleanup))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
