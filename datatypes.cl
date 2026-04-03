;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2026-04-04 00:51:21>

(in-package :cl-weather)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions

(define-condition weather-condition (error)
  ((cycle :initarg :cycle :reader cycle)
   (offset :initarg :offset :reader offset)
   (resolution :initarg :resolution :reader resolution)
   (filename :initarg :filename :reader filename :initform "<not provided>" ))
  (:report
   (lambda (c s)
     (declare (ignore c))
     (format s "weather-condition"))))

(define-condition missing-file (error)
  ((filename :initarg :filename :reader filename :initform "<not provided>" ))
  (:report
   (lambda (c s)
     (format s "Missing file ~a" (filename c)))))

(define-condition missing-forecast (weather-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Missing forecast ~a-~a ~a ~a" (cycle c) (offset c) (resolution c) (filename c)))))

(define-condition incomplete-download (weather-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Incomplete download ~a for ~a ~a ~a"
             (filename c)
             (cycle c)
             (offset c)
             (resolution c)))))

(define-condition latlon-out-of-bounds (condition)
  ((lat :reader lat :initarg :lat)
   (lon :reader lon :initarg :lon))
  (:report
   (lambda (condition stream)
     (format stream "Position ~a,~a out of bounds for GRIB" (lat condition) (lon condition)))))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forecast set
;;;
;;; - The dataset may contain forecast (uv) data from different cycles (during the
;;;   update phase).
;;; - The dataset basetime reflects the forecast time of the first forecast.
;;; - The offsets of the forecasts are recomputed w.r.t. the new basetime when the
;;;   dataset is shifted before the update phase.

;;; TODO: Use struct CYCLE throughout to specify the forecast cycle
(defstruct (cycle (:constructor make-cycle%)) timestamp)

(defun make-cycle (&key (timestamp (now)) (interval (* 6 60 60)) (delay 0))
  "The latest cycle that is or becomes available, assuming 4x6 schedule."
  (let* ((seconds (timestamp-to-unix timestamp))
         (cycle-seconds (* interval (floor (- seconds delay) interval))))
    (make-cycle% :timestamp (unix-to-timestamp cycle-seconds))))

(defun cycle-offset (cycle timestamp)
  (*  3
      (floor
       (truncate (timestamp-difference timestamp
                                       (cycle-timestamp cycle))
                 3600)
       3)))

(defun-t cycle-datestring string ((cycle cycle))
  (format-yyyymmdd nil (cycle-timestamp cycle)))

(defun cycle-string (cycle)
  (format nil "~a-~2,'0d" (cycle-datestring cycle) (cycle-run cycle)))

(defun-t cycle-run fixnum ((cycle cycle))
  (timestamp-hour (cycle-timestamp cycle) :timezone +utc-zone+))

(defun datestring-run-to-timestamp (datestring run)
  (let ((date (format nil "~a-~a-~aT~2,,,'0@a:00:00.000Z"
                      (subseq datestring 0 4)
                      (subseq datestring 4 6)
                      (subseq datestring 6 8)
                      run)))
    date))

(defmethod print-object ((thing cycle) stream)
  (format stream "~a" (cycle-string thing))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpolation parameters

(defstruct iparams current previous offset merge-fraction)
(defun iparams-method (iparams)
  (if (iparams-current iparams)
      (params-method  (iparams-current iparams))
      (params-method  (iparams-previous iparams))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRIB Info

(defstruct gribinfo 
  grid-size                     ; Number of data points, should equal lat-points * lon-points
  step-units                    ; Unit of step (in hours), usually 1
  lat-start lat-end lat-points  ; Start, end and number of points along parallel
  lon-start lon-end lon-points  ; Start, end and number of points along meridian
  i-inc j-inc                   ; Grid increment in degrees 
  i-scan-neg j-scan-pos         ; Value order
  )
(defmethod print-object ((thing gribinfo) stream)
  (format stream "{GribInfo ~apts i-incr:~a j-incr:~a}"
          (gribinfo-grid-size thing)
          (gribinfo-i-inc thing)
          (gribinfo-j-inc thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forecast data

(defstruct gribmessage parameter basetime step info data)
(defmethod print-object ((msg gribmessage) stream)
  (format stream "{param=~a time=~a offset=~a ~a}"
          (gribmessage-parameter msg)
          (gribmessage-basetime msg)
          (gribmessage-step msg)
          (gribmessage-info msg)))

(defstruct parameter name short discipline category number)
(defmethod print-object ((param parameter) stream)
  (format stream "{name=~a short=~a d=~a c=~a n=~a}"
          (parameter-name param)
          (parameter-short param)
          (parameter-discipline param)
          (parameter-category param)
          (parameter-number param)))

(defstruct uv
  info                                  ; units?, borders, incremements, scan directions
  vars                                  ; wind or current
  basetime                              ; cycle time
  (cycle  nil :read-only t)             ; Time when forecast was created
  offset                                ; Offset of forecast data w.r.t. dataset basetime
  step                                  ; Step - for debugging
  u-array                               ; U component
  v-array                               ; V component
  )

(defun uv-forecast-time (uv)
  (adjust-timestamp (uv-basetime uv) (offset :minute (uv-offset uv))))

(defun gribmessage-forecast-time (msg)
  (adjust-timestamp (gribmessage-basetime msg) (offset :hour (gribmessage-step msg))))

(defmethod print-object ((thing uv) stream)
  (format stream "{UV T=~a C=~a n=~a}"
          (format-ddhhmm nil (uv-forecast-time thing))
          (format-timestamp-as-cycle nil (uv-cycle thing))
          (uv-step thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Wind 

(defstruct wind u v a s)
 
(defmethod print-object ((thing wind) stream)
  (format stream "(Speed ~a Dir ~a)"
          (enorm (wind-u thing) (wind-v thing))
          (angle (wind-u thing) (wind-v thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(declaim (inline normalized-lat))
(defun-t normalized-lat double-float ((lat double-float))
  (cond ((> lat 90)
         (- lat 180d0))
        ((< lat -90)
         (+ lat 180d0))
        (t
         lat)))

(declaim (inline normalized-lng))
(defun-t normalized-lng double-float ((lng double-float))
  (cond ((minusp lng)
         (+ lng 360d0))
        ((>= lng 360d0)
         (- lng 360d0))
        (t
         lng)))

(declaim (inline uv-index))
(defun uv-index (info lat lon)
  (let ((lat-start (gribinfo-lat-start info))
        (lat-end (gribinfo-lat-end info))
        (lon-start (gribinfo-lon-start info))
        (lon-end (gribinfo-lon-end info))
        (j-scan-pos-p (eql (gribinfo-j-scan-pos info) 1)))
    (let* ((i-inc (gribinfo-i-inc info))
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
      uv-index)))

(defun dataset-forecast (dataset)
  (aref (dataset-forecasts dataset) 0))

(declaim (inline forecast-fraction))


(defgeneric forecast-fraction (fc0 fc1 timestamp)
  (:documentation
   "Determine fractions (l, 1-l) to combine forecasts for forecast times t0 and t1 at timestamp"))

(defmethod forecast-fraction ((fc0 uv) (fc1 uv) timestamp)
  (let ((fraction
          (duration-fraction (uv-forecast-time fc0)
                             (uv-forecast-time fc1)
                             timestamp)))
    (log2:trace "fc0:~a fc1:~a Fraction: ~a" fc0 fc1 fraction)
    fraction))

(defmethod forecast-fraction ((fc0 gribmessage) (fc1 gribmessage) timestamp)
  (let ((fraction
          (duration-fraction (gribmessage-forecast-time fc0)
                             (gribmessage-forecast-time fc1)
                             timestamp)))
    (log2:trace "fc0:~a fc1:~a Fraction: ~a" fc0 fc1 fraction)
    fraction))
    
(declaim (inline duration-fraction))
(defun duration-fraction (ts0 ts1 ts)
  (let ((delta (timestamp-difference ts1 ts0))
        (s0 (timestamp-difference ts ts0)))
    (cond
      ((eql delta 0)
       0d0)
      (t
       (assert (not (minusp s0)))
       (coerce (/ s0 delta) 'double-float))))) 

(declaim (inline grib-get-uv))
(defun grib-get-uv (uv index)
  (declare (ftype (function (t) (array double-float (*))) uv-u-array uv-v-array))
  (let ((u (aref (uv-u-array uv) index))
        (v (aref (uv-v-array uv) index)))
    ;;(log2:trace "~a => ~,2,,'0,f,~,2,,'0,f" index (angle u v) (enorm u v))
    (values u v)))

(defmethod print-object ((ts local-time:timestamp) stream)
  (format-datetime stream ts))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
