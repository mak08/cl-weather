;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2019-01-02 18:42:02>

(in-package :cl-weather)

(declaim (optimize speed (debug 0) (safety 0))
         #+()(ftype (function (double-float double-float) double-float)
            enorm-d))

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
