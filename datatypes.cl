;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2021-11-12 00:11:24>

(in-package :cl-weather)


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

(defun make-cycle (&key (timestamp (now)))
  (let* ((seconds (timestamp-to-unix timestamp))
         (cycle-seconds (* 6 3600 (floor seconds (* 6 3600)))))
    (make-cycle% :timestamp (unix-to-timestamp cycle-seconds))))

(defun-t cycle-datestring string ((cycle cycle))
  (format-timestring nil (cycle-timestamp cycle)
                     :format '((:year 4) (:month 2) (:day 2))
                     :timezone +utc-zone+))

(defun-t cycle-as-timestamp timestamp ((cycle cycle))
  (cycle-timestamp cycle))

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
  (format stream "[~a ~a]" (cycle-datestring thing) (cycle-run thing))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defstruct dataset
  basetime                              ; Base time of forecast data
  grib-info                             ; Grid description
  forecasts                             ; Array of uv data
  )

(defun dataset-cycle (dataset)
  (make-cycle :timestamp (dataset-basetime dataset)))

(defmethod print-object ((thing dataset) stream)
  (format stream "{dataset base=~a}"
          (format-datetime nil (dataset-basetime thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forecast parameters
(defstruct params timestamp base-time forecast next-fc method merge-start merge-window info fc0 fc1 fraction)

(defstruct iparams current previous offset)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ==============
;;; Class forecast
;;; ==============
;;; A forecast represents forecast data for a specific point in time.
;;; Wind speed and direction for any point in the covered area can be obtained
;;; from a forecast, but the actual values are computed only on demand.

(defclass forecast ()
  ((fc-dataset :reader fc-dataset :initarg :dataset)
   (fc-time :reader fc-time :initarg :time)
   (fc-offset :reader fc-offset :initarg :offset :documentation "Offset from dataset basetime in minutes")
   (fc-hash :accessor fc-hash% :initform (make-hash-table))))

(defmethod print-object ((thing forecast) stream)
  (let ((cycle (dataset-cycle (fc-dataset thing))))
    (format stream "{Forecast @ ~a, Offset ~a, Cycle ~a|~a}"
            (format-datetime nil (fc-time thing))
            (fc-offset thing)
            (cycle-datestring cycle)
            (cycle-run cycle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forecast data

(defstruct uv
  dataset                               ; Back reference 
  (cycle  nil :read-only t)             ; Time when forecast was created
  offset                                ; Offset of forecast data w.r.t. dataset basetime
  step                                  ; Step - for debugging
  u-array                               ; U component of wind
  v-array                               ; V component of wind 
  )

(defun uv-forecast-time (uv)
  (adjust-timestamp (dataset-basetime (uv-dataset uv)) (offset :minute (uv-offset uv))))

(defmethod print-object ((thing uv) stream)
  (format stream "{forecast cycle=~a offset=~a time=~a step=~a}"
          (format-datetime nil (uv-cycle thing))
          (uv-offset thing)
          (format-datetime nil (uv-forecast-time thing))
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
  (duration-fraction  (uv-forecast-time fc0) (uv-forecast-time fc1) timestamp))

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
  (let ((u (aref (uv-u-array uv) index))
        (v (aref (uv-v-array uv) index)))
    ;;(log2:trace "~a => ~,2,,'0,f,~,2,,'0,f" index (angle u v) (enorm u v))
    (values u v)))
;; (declaim (notinline grib-get-uv))

(defmethod print-object ((ts timestamp) stream)
  (format-datetime stream ts))


(defun base-time (iparams)
  (params-base-time (iparams-current iparams)))

(defun iparams-effective-cycle (iparams)
  (if (iparams-previous iparams)
      (params-base-time (iparams-previous iparams))
      (params-base-time (iparams-current iparams))))

(defun prediction-parameters (timestamp &key
                                          method
                                          merge-start
                                          merge-window
                                          (cycle (available-cycle timestamp)))
  ;; If $date is provided, $cycle must also be provided, and the specified forecast will be used.
  ;; Otherwise, the latest available forecast will be used.
  ;; ### ToDo ### The $next-fc may not be available yet!
  (log2:trace "timestamp:~a cycle:~a" timestamp cycle)
  (let* ((forecast (cycle-forecast cycle timestamp))
         (next-fc (next-forecast forecast))
         (ds0 (noaa-forecast :cycle cycle :offset forecast))
         (ds1 (noaa-forecast :cycle cycle :offset next-fc))
         (fc0 (dataset-forecast ds0))
         (fc1 (dataset-forecast ds1))
         (fraction (forecast-fraction fc0 fc1 timestamp))
         (info (dataset-grib-info ds0)))
    (make-params :info info
                 :timestamp timestamp
                 :base-time (cycle-timestamp cycle)
                 :method method
                 :merge-start merge-start
                 :merge-window merge-window
                 :forecast forecast
                 :next-fc next-fc
                 :fc0 fc0
                 :fc1 fc1
                 :fraction fraction)))

(defun interpolation-parameters (timestamp &key
                                             method
                                             merge-start
                                             merge-window
                                             (cycle (available-cycle timestamp)))
  (log2:trace "T:~a C:~a M:~a S:~a W:~a" timestamp cycle method merge-start merge-window)
  (let* ((cycle1 (or cycle (available-cycle timestamp)))
         (cycle0 (previous-cycle cycle1))
         (current (prediction-parameters timestamp
                                         :method method
                                         :merge-start merge-start
                                         :merge-window merge-window
                                         :cycle cycle1))
         (offset (/ (timestamp-difference (params-timestamp current)
                                          (params-base-time current))
                    3600.0))
         (previous (when  (<= offset (+ merge-start merge-window))
                     (prediction-parameters timestamp
                                            :method method
                                            :merge-start merge-start
                                            :merge-window merge-window
                                            :cycle cycle0))))
    (log2:trace "Current:~a  Previous: ~a" cycle1 cycle0)
    (make-iparams :current current
                  :previous previous
                  :offset offset)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
