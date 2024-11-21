;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2024-07-25 22:27:10>

(in-package :cl-weather)

(declaim (optimize debug safety)
         #+()(ftype (function (double-float double-float) double-float)
            enorm-d))

(setf cl-weather:*vr-grib-directory* "/home/michael/Wetter/vr/")

#|
(test-wind-n #p[035°02'50"N 034°24'50"E]
             :source :vr
             :method :enorm
             :resolution "0p25"
             :time (parse-timestring "2023-02-20T01:20:00Z")
             :cycle (make-cycle
                     :timestamp (parse-timestring "2023-02-19T12:00:00Z")))
|#

(defun test-wind (latlng
                  &key
                    (time (now))
                    (source :noaa)
                    (method :bilinear)
                    (merge-start 6.0d0)
                    (merge-window 0.0d0)
                    (cycle (available-cycle time))
                    (resolution "1p00"))
  (let* ((lat (latlng-lat latlng))
         (lon (latlng-lng latlng))
         (iparams
           (interpolation-parameters 
            time 
            :source source
            :method method
            :merge-start merge-start
            :merge-duration merge-window
            :cycle cycle
            :resolution resolution)))
    (multiple-value-bind (dir speed)
        (interpolate lat lon iparams)
      (format nil "~6,2f° ~6,2fm/2 ~6,2fkn" dir speed (m/s-to-knots speed)))))

(defun test-wind-old (latlng
                  &key
                    (time (now))
                    (source :noaa)
                    (method :bilinear)
                    (merge-start 6.0d0)
                    (merge-window 0.0d0)
                    (cycle (available-cycle time))
                    (resolution "1p00"))
  (let* ((lat (latlng-lat latlng))
         (lon (latlng-lng latlng))
         (iparams
           (interpolation-parameters time
                                     :method method
                                     :merge-start merge-start
                                     :merge-duration merge-window
                                     :source source
                                     :cycle cycle
                                     :resolution resolution)))
    (multiple-value-bind (dir speed)
        (interpolated-prediction lat lon iparams)
      (format t "~6,2f° ~6,2fkn" dir (m/s-to-knots speed)))))

(defun test-forecast-fraction (&key (timestamp (now)) (cycle))
  (let* ((forecast (cycle-forecast cycle timestamp))
         (ds0 (load-forecast  :cycle cycle :offset forecast))
         (ds1 (load-forecast  :cycle cycle :offset (next-forecast forecast)))
         (fc0 (dataset-forecast ds0))
         (fc1 (dataset-forecast ds1)))
    (forecast-fraction fc0 fc1 timestamp)))

(defun get-grib-wind (source cycle offset u v)
  (let* ((filename (ecase source
                     (:noaa (noaa-destpath :cycle cycle :offset offset))
                     (:vr (vr-destpath :cycle cycle :offset offset))))
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
