;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2019-01-02 23:44:29>

(in-package :cl-weather)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forecast set
;;;
;;; - The dataset may contain forecast (uv) data from different cycles (during the
;;;   update phase).
;;  - The dataset basetime reflects the forecast time of the first forecast.
;;; - The offsets of the forecasts are recomputed w.r.t. the new basetime when the
;;;   dataset is shifted before the update phase.

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
;;; Helpers

(defun format-datetime (stream timestamp &key (timezone +utc-zone+))
  (format stream "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
          (timestamp-year timestamp :timezone timezone)
          (timestamp-month timestamp :timezone timezone)
          (timestamp-day timestamp :timezone timezone)
          (timestamp-hour timestamp :timezone timezone)
          (timestamp-minute timestamp :timezone timezone)
          (timestamp-second timestamp :timezone timezone)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
