;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2022-11-11 22:06:29>

(in-package :cl-weather)

(declaim (optimize debug safety)
         #+()(ftype (function (double-float double-float) double-float)
            enorm-d))

(defstruct fctile-stack north south east west cycle data)
(defstruct fctile-data offset u-data v-data) 

(defun get-wind-uv (north south west east
                    &key
                      (cycle (available-cycle (now)))
                      (from-forecast 0)
                      (to-forecast from-forecast))
  (make-fctile-stack
   :north north
   :south south
   :east east
   :west west
   :cycle cycle
   :data (loop :for offset :from from-forecast :to to-forecast :by 3
               :collect
               (let*
                   ((dataset (noaa-forecast :cycle cycle :offset offset))
                    (uv (dataset-forecast dataset))
                    (info (dataset-grib-info dataset))
                    (result-u (make-array (list (- east west) (- north south)) :element-type 'double-float))
                    (result-v (make-array (list (- east west) (- north south)) :element-type 'double-float)))
                 (loop :for lat :from north :downto (+ south 1)
                       :for lat0 :from 0
                       :do (loop :for lon :from west :to (- east 1)
                                 :for lon0 :from 0
                                 :for index = (uv-index info lat lon)
                                 :do (multiple-value-bind (u v)
                                         (grib-get-uv uv index)
                                       (setf (aref result-u lat0 lon0) u)
                                       (setf (aref result-v lat0 lon0) v))))
                 (make-fctile-data :offset offset
                                   :u-data result-u
                                   :v-data result-v)))))


(defun test-wind (lat lon &key
                            (time (now))
                            (method :bilinear)
                            (merge-start 6.0d0)
                            (merge-window 0.0d0)
                            (cycle (available-cycle time))
                            (resolution "1p00"))
  (let ((iparams
          (interpolation-parameters time
                                    :method method
                                    :merge-start merge-start
                                    :merge-window merge-window
                                    :cycle cycle
                                    :resolution resolution)))
    (interpolated-prediction lat lon iparams)))


(defun test-forecast-fraction (&key (timestamp (now)) (cycle))
  (let* ((forecast (cycle-forecast cycle timestamp))
         (date)
         (ds0 (noaa-forecast date :cycle cycle :offset forecast))
         (ds1 (noaa-forecast date :cycle cycle :offset (next-forecast forecast)))
         (fc0 (dataset-forecast ds0))
         (fc1 (dataset-forecast ds1)))
    (forecast-fraction fc0 fc1 timestamp)))

(defun get-grib-wind (date cycle offset u v)
  (let* ((filename (noaa-destpath date :cycle cycle :offset offset))
         (command (format () "grib_get ~a -l ~a,~a,1" filename u v))
         (*read-default-float-format* 'double-float)
         (result
          (uiop:run-program command
                            :output (lambda (is)
                                      (loop
                                         :for line = (read-line is nil nil)
                                         :while line
                                         :collect (read-from-string line))))))
    (log2:trace "~a => ~a" command result)
    (values (apply #'angle result)
            (apply #'enorm result))))

(defun probe-wind (time latlng)
  (let ((forecast (get-forecast
                   (get-forecast-bundle 'noaa-bundle)
                   (parse-rfc3339-timestring time))))
    (log2:info "Using ~a" forecast)
    (multiple-value-bind (angle speed)
        (get-wind-forecast forecast latlng)
      (values angle
              (m/s-to-knots speed)))))

(declaim (inline enorm-s))
(defun enorm-s (x y)
  (declare (single-float x y))
  (sqrt (+ (* x x) (* y y))))

(declaim (inline enorm-d))
(defun enorm-d (x y)
  (declare (double-float x y)
           #+()(ftype (function (double-float double-float) double-float)
            enorm-d))
  (sqrt (+ (* x x) (* y y))))

(defun test-r (n u v)
  (let (
        (r))
    (dotimes (k n r)
      (setf r (enorm-s u v)))))

(defun test-d (n u v)
  (declare (fixnum n))
  (let (
        (r 0d0))
    #+()(declare (double-float r)
             (ftype (function (double-float double-float) double-float)
                    enorm-d))
    (dotimes (k n r)
      (setf r (enorm-d u v)))))

(defun test-c (n u v)
  (declare (single-float u v))
  (let ((c (complex u v))
        (r))
    (declare ((complex single-float) c))
    (dotimes (k n r)
      (setf r (abs c)))))
