;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-03-20 20:55:24>

(in-package "CL-WEATHER")
(setf (log2:log-level "cl-weather") log2:+info+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common in all modules

(defparameter *source-root*
  (make-pathname :directory (pathname-directory #.*compile-file-truename*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific  to this module

(defvar *grib-directory*
  (merge-pathnames (make-pathname :directory '(:relative "gribfiles")) *source-root*))

(defparameter *merge-window* 2.5d0)
(defparameter *merge-start* 4.0d0)

(declaim (inline normalized-lat))
(defun normalized-lat (lat)
  (declare (double-float lat))
  (cond ((> lat 90)
         (- lat 180d0))
        ((< lat -90)
         (+ lat 180d0))
        (t
         lat)))

(declaim (inline normalized-lng))
(defun normalized-lng (lng)
  (declare (double-float lng))
  (cond ((minusp lng)
         (+ lng 360d0))
        ((>= lng 360d0)
         (- lng 360d0))
        (t
         lng)))

(defun uv-index (info lat lon)
  (let*
      ((j-scan-pos-p (eql (gribinfo-j-scan-pos info) 1))
       (i-inc (gribinfo-i-inc info))
       (j-inc (gribinfo-j-inc info))
       (lat0 (gribinfo-lat-start info))
       (lon0 (gribinfo-lon-start info))
       (olat (if j-scan-pos-p (- lat lat0) (- lat0 lat)))
       (olon (- lon lon0))
       (lat-index (floor olat j-inc))
       (lon-index (floor olon i-inc))
       (lonpoints (gribinfo-lon-points info))
       (lat-offset (* lat-index lonpoints))
       (uv-index (+ lat-offset lon-index)))
    ;;(log2:trace "Lat: ~a Lng: ~a Index: ~a" lat lon uv-index)
    uv-index))

(defun dataset-forecast (dataset)
  (aref (dataset-forecasts dataset) 0))

(defun forecast-fraction (fc0 fc1 timestamp)
  (let ((delta (timestamp-difference (uv-forecast-time fc1) (uv-forecast-time fc0)))
        (s0 (timestamp-difference timestamp (uv-forecast-time fc0))))
    (cond
      ((eql delta 0)
       0d0)
      (t
       (assert (not (minusp s0)))
       (let ((fraction (coerce (/ s0 delta) 'double-float)))
         (log2:trace "TS: ~a FC0: ~a FC1: ~a Fraction: ~a" timestamp fc0 fc1 fraction)
         fraction)))))

(declaim (inline grib-get-uv))
(defun grib-get-uv (uv index)
  (let ((u (aref (uv-u-array uv) index))
        (v (aref (uv-v-array uv) index)))
    ;;(log2:trace "~a => ~,2,,'0,f,~,2,,'0,f" index (angle u v) (enorm u v))
    (values u v)))
(declaim (notinline grib-get-uv))

(defun timespec-to-timestamp (date cycle)
  "Make a timestamp from $date=YYYYMMDD, $cycle={0,6,12,18}"
  (parse-timestring (format nil "~a-~a-~aT~2,,,'0@a:00:00"
                            (subseq date 0 4)
                            (subseq date 4 6)
                            (subseq date 6 8)
                            cycle)))

(defun timestamp-to-timespec (timestamp)
  (let ((date (format-timestring nil timestamp :format '((:year 4) (:month 2) (:day 2))))
        (cycle (timestamp-hour timestamp :timezone +utc-zone+)))
    (ecase cycle
      ((0 6 12 18)
       (values date
               cycle)))))

(defun base-time (iparams)
  (params-base-time (iparams-current iparams)))

(defun format-datetime (stream timestamp &key (timezone +utc-zone+))
  (format stream "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
          (timestamp-year timestamp :timezone timezone)
          (timestamp-month timestamp :timezone timezone)
          (timestamp-day timestamp :timezone timezone)
          (timestamp-hour timestamp :timezone timezone)
          (timestamp-minute timestamp :timezone timezone)
          (timestamp-second timestamp :timezone timezone)))


(defmethod print-object ((ts timestamp) stream)
  (format-datetime stream ts))


(defstruct params timestamp base-time forecast next-fc info fc0 fc1 fraction)

(defun prediction-parameters (timestamp &key (date nil) (cycle nil))
  ;; If $date is provided, $cycle must also be provided, and the specified forecast will be used.
  ;; Otherwise, the latest available forecast will be used.
  ;; ### ToDo ### The $next-fc may not be available yet!
  (cond
    (date
     (assert cycle))
    (t
     (multiple-value-setq (date cycle) (available-cycle timestamp))))
  (log2:trace "TS: ~a, date:~a cycle:~a" timestamp date cycle)
  (let* ((forecast (cycle-forecast date cycle timestamp))
         (next-fc (next-forecast forecast))
         (ds0 (noaa-forecast date :cycle cycle :offset forecast))
         (ds1 (noaa-forecast date :cycle cycle :offset next-fc))
         (fc0 (dataset-forecast ds0))
         (fc1 (dataset-forecast ds1))
         (fraction (forecast-fraction fc0 fc1 timestamp))
         (info (dataset-grib-info ds0)))
    (make-params :info info
                 :timestamp timestamp
                 :base-time (timespec-to-timestamp date cycle)
                 :forecast forecast
                 :next-fc next-fc
                 :fc0 fc0
                 :fc1 fc1
                 :fraction fraction)))

(defstruct iparams current previous offset)

(defun interpolation-parameters (timestamp &optional (cycle nil))
  (multiple-value-bind  (date1 cycle1)
      (available-cycle timestamp)
    (when cycle
      (multiple-value-setq (date1 cycle1)
        (timestamp-to-timespec (parse-timestring cycle))))
    (multiple-value-bind (date0 cycle0)
        (previous-cycle date1 cycle1)
      (let* ((current (prediction-parameters timestamp :date date1 :cycle cycle1))
             (c-offset (/ (timestamp-difference (params-timestamp current)
                                                (params-base-time current))
                         3600.0))
             (previous (when  (<= c-offset (+ *merge-start* *merge-window*))
                         (prediction-parameters timestamp :date date0 :cycle cycle0)))
             (p-offset (when  (<= c-offset (+ *merge-start* *merge-window*))
                         (/ (timestamp-difference (params-timestamp previous)
                                                  (params-base-time previous))
                            3600.0))))
        (log2:trace "Current: ~a-~a+~a  previous: ~a" date1 cycle1 (truncate (* c-offset 60)) (not (null previous)))
        (make-iparams :current current
                      :previous previous
                      :offset c-offset)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
