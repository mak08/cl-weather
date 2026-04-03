;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2026-04-03 16:03:16>

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))

(in-package "CL-WEATHER")

(defstruct params fc0 fc1 fraction)

(defun get-params (datasource timestamp
                   &key
                     (cycle (timestamp-cycle datasource timestamp)))
  (handler-case
      (get-params-for-cycle datasource timestamp :cycle cycle)
    (missing-forecast (e)
      (get-params-for-cycle datasource timestamp :cycle (previous-cycle datasource cycle)))))
      
(defun get-params-for-cycle (datasource-classname timestamp &key cycle)
  (let* ((datasource (get-datasource datasource-classname cycle))
         (step-0 (cycle-forecast datasource timestamp))
         (step-1 (next-forecast datasource step-0))
         (fc-0 (load-forecast datasource step-0))
         (fc-1 (load-forecast datasource step-1))
         (fraction (forecast-fraction fc-0 fc-1 timestamp)))
    (make-params :fc0 fc-0 :fc1 fc-1 :fraction fraction)))

(declaim (inline valid-position))
(defun valid-position (lat lon info)
  (let ((lat-start (gribinfo-lat-start info))
        (lat-end (gribinfo-lat-end info))
        (lon-start (gribinfo-lon-start info))
        (lon-end (gribinfo-lon-end info))
        (j-scan-pos-p (eql (gribinfo-j-scan-pos info) 1)))
    (and (if j-scan-pos-p
             (<= lat-start lat lat-end)
             (<= lat-end lat lat-start))
         (<= lon-start (mod lon 360) lon-end))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
