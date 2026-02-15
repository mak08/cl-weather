;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2026-02-14 22:50:27>

(in-package :cl-weather)

(defun get-messages-from-file (file)
  (with-c-file (fp file "ro")
    (when (null-pointer-p fp)
      (error 'missing-file :filename file))
    (let
        ((n (codes-count-in-file fp)))
      (log2:trace "Messages: ~a~%" n)
      (loop
        :for k :below n :by 2
        :collect (let ((u (codes-grib-handle-new-from-file fp))
                       (v (codes-grib-handle-new-from-file fp)))
                   (with-bindings (((u-data-time u-offset u-message-info u-values)
                                    (read-grib-message u))
                                   ((v-data-time v-offset v-message-info v-values)
                                    (read-grib-message v)))
                     (make-uv :info u-message-info
                              :vars nil
                              :basetime u-data-time
                              :cycle u-data-time
                              :offset (* u-offset 60)
                              :step u-offset
                              :u-array u-values
                              :v-array v-values)))))))

(defun read-grib-message (handle)
  (let ((parameter
          (codes-get-string handle "parameterName"))
        (number
          (codes-get-long handle "parameterNumber"))
        (data-time
          (read-data-datetime handle))
        (offset (codes-get-long handle "forecastTime"))
        (message-info
          (read-message-info handle))
        (values
          (codes-get-double-array handle "values")))
    (log2:trace "Parameter=~a:~a Time=~a:~a" number parameter data-time offset)
    (values data-time
            offset
            message-info
            values)))

(defun get-uv-steps-from-index (index u-and-v-var)
  (let* ((steps 
           (codes-index-get-long index "step"))
         (shortNames
           (codes-index-get-string index "shortName")))
    (log2:trace "Index steps (~a): ~a" (length steps) steps)
    (destructuring-bind (u-var v-var)
        u-and-v-var
      (loop
        :for i :from 0
        :for step :across steps
        :collect
        (progn
          (codes-index-select-long index "step" step)
          (with-bindings (((u-data-time u-offset u-message-info u-values)
                           (select-and-read-message index u-var))
                          ((v-data-time v-offset v-message-info v-values)
                           (select-and-read-message index v-var)))
            (make-uv :info u-message-info
                     :vars u-and-v-var
                     :basetime u-data-time
                     :cycle u-data-time
                     :offset (* u-offset 60)
                     :step step
                     :u-array u-values
                     :v-array v-values)))))))

(defun select-and-read-message (index varname)
  (codes-index-select-string index "shortName" varname)
  (with-handle-from-index (handle index)
    (read-grib-message handle)))

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

(defun read-message-info (message)
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
