;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2019-01-02 23:40:47>

(in-package :cl-weather)

(defun get-values-from-index (index)
  (let* ((steps 
          (codes-index-get-long index "step"))
         (shortNames
          (codes-index-get-string index "shortName"))
         (forecasts
          (make-array (length steps)))
         (grib-info nil)
         (result
          (make-dataset)))
    
    (log2:trace "Index steps (~a): ~a" (length steps) steps)
    (loop
       :for i ::from 0
       :for step :across steps
       :do (progn
             (codes-index-select-long index "step" step)
             (multiple-value-bind (u-data-time u-offset u-grib-info u-values)
                 (select-and-read-message index "10u")
               (multiple-value-bind (v-data-time u-offset v-grib-info v-values)
                   (select-and-read-message index "10v")
                 (when (= i 0)
                   (setf (dataset-basetime result) u-data-time)
                   (setf (dataset-grib-info result) u-grib-info))
                 (setf (aref forecasts i)
                       (make-uv :dataset result
                                :cycle u-data-time
                                :offset (* u-offset 60)
                                :step step
                                :u-array u-values
                                :v-array v-values))))))
    (codes-index-delete index)
    (setf (dataset-forecasts result) forecasts)
    (values
     result)))

(defun select-and-read-message (index varname)
  (codes-index-select-string index "shortName" varname)
  (with-handle-from-index (handle index)
    (let ((data-time
           (read-data-datetime handle))
          (offset (codes-get-long handle "forecastTime"))
          (grib-info
           (get-gribinfo handle))
          (values
           (codes-get-double-array handle "values")))
      (values data-time
              offset
              grib-info
              values))))

(defun read-data-datetime (message)
  (let ((date
         (format nil "~a" (codes-get-long message "dataDate")))
        (time
         (codes-get-long message "dataTime")))
    (parse-timestring (format () "~a-~a-~aT~2,'0d:~2,'0d:00+00:00"
                              (subseq date 0 4)
                              (subseq date 4 6)
                              (subseq date 6 8)
                              (/ time 100)
                              (rem time 100)))))


(defun get-gribinfo (message)
  (make-gribinfo
   :grid-size (codes-get-size message "values")
   :step-units (codes-get-long message "stepUnits")
   :lat-points (codes-get-long message "numberOfPointsAlongAMeridian")
   :lon-points (codes-get-long message "numberOfPointsAlongAParallel")
   :lat-start (coerce (codes-get-long message "latitudeOfFirstGridPointInDegrees") 'double-float)
   :lat-end (coerce (codes-get-long message "latitudeOfLastGridPointInDegrees") 'double-float)
   :lon-start (coerce (codes-get-long message "longitudeOfFirstGridPointInDegrees") 'double-float)
   :lon-end (coerce (codes-get-long message "longitudeOfLastGridPointInDegrees") 'double-float)
   :j-inc (codes-get-double message "jDirectionIncrementInDegrees") ; "south to north"
   :i-inc (codes-get-double message "iDirectionIncrementInDegrees") ; "west to east"
   :j-scan-pos (codes-get-long message "jScansPositively")
   :i-scan-neg  (codes-get-long message "iScansNegatively")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
