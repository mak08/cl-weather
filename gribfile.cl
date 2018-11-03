;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2018-08-05 23:11:19>

(in-package :cl-weather)

(defun get-values-from-index (index)
  (let* ((steps 
          (codes-index-get-long index "step"))
         (shortNames
          (codes-index-get-string index "shortName"))
         (forecasts
          (make-array (length steps)))
         (result (make-grib)))
    
    (log2:trace "Index steps (~a): ~a" (length steps) steps)
    (loop
       :for i ::from 0
       :for step :across steps
       :do (progn
             (codes-index-select-long index "step" step)
             (let ((u-array
                    (progn 
                      (codes-index-select-string index "shortName" "10u")
                      (with-handle-from-index (handle index)
                        (when (= i 0)
                          (get-grid-info result handle)
                          (log2:info "Processing ~a messages [~ax~a]"
                                     (length steps)
                                     (grib-lat-points result)
                                     (grib-lon-points result)))
                        (codes-get-double-array handle "values"))))
                   (v-array
                    (progn 
                      (codes-index-select-string index "shortName" "10v")
                      (with-handle-from-index (handle index)
                        (codes-get-double-array handle "values")))))
               (setf (aref forecasts i)
                     (make-grib-values :cycle (grib-cycle result)
                                       :offset (* step 60)
                                       :u-array u-array
                                       :v-array v-array)))))
    (codes-index-delete index)
    (setf (grib-data result) forecasts)
    (values
     result)))

(defun get-grid-info (grib message)
  (let* ((date (format nil "~a" (codes-get-long message "dataDate")))
         (time (codes-get-long message "dataTime"))
         (dummy (log2:info "Reading grib with date=~a time=~a" date time))
         (timestamp (parse-timestring
                     (format () "~a-~a-~aT~2,'0d:~2,'0d:00+00:00"
                             (subseq date 0 4)
                             (subseq date 4 6)
                             (subseq date 6 8)
                             (/ time 100)
                             (rem time 100))))
         (forecast-time (codes-get-long message "forecastTime")))
    (setf (grib-forecast-time grib) (adjust-timestamp timestamp (offset :hour forecast-time))
          (grib-cycle grib) (encode-cycle-timestamp date time)
          (grib-grid-size grib) (codes-get-size message "values")
          (grib-step-units grib) (codes-get-long message "stepUnits")
          (grib-lat-points grib) (codes-get-long message "numberOfPointsAlongAMeridian")
          (grib-lon-points grib) (codes-get-long message "numberOfPointsAlongAParallel")
          (grib-lat-start grib) (coerce (codes-get-long message "latitudeOfFirstGridPointInDegrees") 'double-float)
          (grib-lat-end grib) (coerce (codes-get-long message "latitudeOfLastGridPointInDegrees") 'double-float)
          (grib-lon-start grib) (coerce (codes-get-long message "longitudeOfFirstGridPointInDegrees") 'double-float)
          (grib-lon-end grib) (coerce (codes-get-long message "longitudeOfLastGridPointInDegrees") 'double-float)
          (grib-j-inc grib) (codes-get-double message "jDirectionIncrementInDegrees") ; "south to north"
          (grib-i-inc grib) (codes-get-double message "iDirectionIncrementInDegrees") ; "west to east"
          (grib-j-scan-pos grib) (codes-get-long message "jScansPositively")
          (grib-i-scan-neg  grib) (codes-get-long message "iScansNegatively"))))

(defun encode-cycle-timestamp (date time)
  (encode-timestamp 0 0 0
                    (/ time 100)
                    (parse-integer (subseq date 6 8))
                    (parse-integer (subseq date 4 6))
                    (parse-integer (subseq date 0 4))
                    :timezone +utc-zone+))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
