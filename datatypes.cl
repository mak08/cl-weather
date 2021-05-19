;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2021-05-02 00:31:21>

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
(defstruct cycle
  :date                                 ; Format: YYYYMMDD
  :hour                                 ; 0, 6, 12, 18 (for NOAA)
  )

(defstruct dataset
  basetime                              ; Base time of forecast data
  grib-info                             ; Grid description
  forecasts                             ; Array of uv data
  )

(defun dataset-cycle (dataset)
  (dataset-basetime dataset))

(defmethod print-object ((thing dataset) stream)
  (format stream "{dataset base=~a}"
          (format-datetime nil (dataset-basetime thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forecast parameters
(defstruct params timestamp base-time forecast next-fc info fc0 fc1 fraction)

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
            (format-timestring nil cycle :format '(:year "-" (:month 2) "-" (:day 2)) :timezone +utc-zone+)
            (timestamp-hour cycle :timezone +utc-zone+))))

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

(defmethod print-object ((ts timestamp) stream)
  (format-datetime stream ts))


(defun base-time (iparams)
  (params-base-time (iparams-current iparams)))

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

(defun interpolation-parameters (timestamp &optional (cycle nil))
  (multiple-value-bind  (date1 cycle1)
    (if cycle
        (timestamp-to-timespec (parse-timestring cycle))
        (available-cycle timestamp))
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
