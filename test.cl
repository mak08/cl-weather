;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2021-05-25 21:34:56>

(in-package :cl-weather)

(declaim (optimize speed (debug 0) (safety 0))
         #+()(ftype (function (double-float double-float) double-float)
            enorm-d))

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
