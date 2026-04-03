;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2026-04-03 23:16:51>

(in-package :cl-weather)

(defun get-messages-from-file (file)
  (with-c-file (fp file "ro")
    (when (null-pointer-p fp)
      (error 'missing-file :filename file))
    (let
        ((n (codes-count-in-file fp)))
      (log2:trace "Messages: ~a~%" n)
      (loop
        :for k :below n
        :collect (let ((handle (codes-grib-handle-new-from-file fp)))
                   (read-grib-message handle))))))

(defun get-uv-messages-from-file (file)
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
                                    (message-values (read-grib-message u)))
                                   ((v-data-time v-offset v-message-info v-values)
                                    (message-values (read-grib-message v))))
                     (make-uv :info u-message-info
                              :vars nil
                              :basetime u-data-time
                              :cycle u-data-time
                              :offset (* u-offset 60)
                              :step u-offset
                              :u-array u-values
                              :v-array v-values)))))))

(defun message-values (msg)
  (values (gribmessage-basetime msg)
          (gribmessage-step msg)
          (gribmessage-info msg)
          (gribmessage-data msg)))
      
(defun read-grib-message (handle)
  (let ((name
          (codes-get-string handle "parameterName"))
        (discipline
          (codes-get-long handle "discipline"))
        (category
          (codes-get-long handle "parameterCategory"))
        (number
          (codes-get-long handle "parameterNumber"))
        (short
          (codes-get-string handle "shortName"))
        (data-time
          (read-data-datetime handle))
        (step (codes-get-long handle "forecastTime"))
        (message-info
          (read-message-info handle))
        (values
          (codes-get-double-array handle "values")))
    (log2:trace "Parameter=~a Time=~a:~a" name  data-time step)
    (make-gribmessage :parameter (make-parameter :name name
                                                 :short short
                                                 :discipline discipline
                                                 :category category
                                                 :number number)
                      :basetime data-time
                      :step step
                      :info message-info
                      :data values)))

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
                           (message-values
                            (select-and-read-message index (list (list "shortName" u-var)))))
                          ((v-data-time v-offset v-message-info v-values)
                           (message-values
                            (select-and-read-message index (list (list "shortName" v-var))))))
            (make-uv :info u-message-info
                     :vars u-and-v-var
                     :basetime u-data-time
                     :cycle u-data-time
                     :offset (* u-offset 60)
                     :step step
                     :u-array u-values
                     :v-array v-values)))))))

(defun get-messages-from-index (index query)
  (let* ((steps 
           (codes-index-get-long index "step")))
    (log2:trace "Index steps (~a): ~a" (length steps) steps)
    (loop
      :for i :from 0
      :for step :across steps
      :collect
      (progn
        (codes-index-select-long index "step" step)
        (select-and-read-message index query)))))

(defun select-and-read-message (index clauses)
  (dolist (clause clauses)
    (destructuring-bind (parameter value)
        clause
      (typecase value
        (string
         (codes-index-select-string index parameter value))
        (integer
         (codes-index-select-long index parameter value))
        (float 
         (codes-index-select-double index parameter value)))))
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
  (let ((lon-start (coerce (codes-get-double message "longitudeOfFirstGridPointInDegrees") 'double-float))
        (lon-end (coerce (codes-get-double message "longitudeOfLastGridPointInDegrees") 'double-float))
        (j-inc (codes-get-double message "jDirectionIncrementInDegrees")) ; "south to north"
        (i-inc (codes-get-double message "iDirectionIncrementInDegrees")) ; "west to east"
        )
    (when (= 0d0 (mod (incf lon-end i-inc) 360) lon-start)
      (log2:trace "wrap around grib")
      (setf lon-end 360d0))
    (make-gribinfo
     :grid-size (codes-get-size message "values")
     :step-units (codes-get-long message "stepUnits")
     :lat-points (codes-get-long message "numberOfPointsAlongAMeridian")
     :lon-points (codes-get-long message "numberOfPointsAlongAParallel")
     :lat-start (coerce (codes-get-double message "latitudeOfFirstGridPointInDegrees") 'double-float)
     :lat-end (coerce (codes-get-double message "latitudeOfLastGridPointInDegrees") 'double-float)
     :lon-start lon-start
     :lon-end lon-end
     :j-inc j-inc
     :i-inc i-inc
     :j-scan-pos (codes-get-long message "jScansPositively")
     :i-scan-neg  (codes-get-long message "iScansNegatively"))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
