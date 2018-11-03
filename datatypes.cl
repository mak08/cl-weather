;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-10-17 21:30:35>

(in-package :cl-weather)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct grib

  "Basic / common GRIB data"
  forecast-time                 ; The timestamp (yyyymmdd, hh) of the forecast (identical with the first forecast in the bundle)
  cycle                         ; Forecast cycle/date
  grid-size                     ; Number of data points, should equal lat-points * lon-points
  step-units
  lat-start lat-end lat-points  ; Start, end and number of points along parallel
  lon-start lon-end lon-points  ; Start, end and number of points along meridian
  i-inc j-inc
  i-scan-neg j-scan-pos
  data                          ; Array of forecast data for successive forecast times
  )

(defmethod print-object ((thing grib) stream)
  (format stream "{grib <~a,~a>/<~a,~a> ~a}"
          (grib-lat-start thing)
          (grib-lon-start thing)
          (grib-lat-end thing)
          (grib-lon-end thing)
          (grib-forecast-time thing)))

(defstruct grib-values
  cycle
  offset                 ; 
  u-array
  v-array
  vmax-data
  outdated)

(defmethod print-object ((thing grib-values) stream)
  (format stream "{grib-values ~a ~a}"
          (grib-values-offset thing)
          (array-dimensions
           (grib-values-u-array thing))))


(defstruct wind u v)

(defmethod print-object ((thing wind) stream)
  (format stream "(Speed ~a Dir ~a)"
          (enorm (wind-u thing) (wind-v thing))
          (angle (wind-u thing) (wind-v thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Euclidian Norm

(defun enorm (x y)
  (declare (double-float x y))
  (sqrt (+ (* x x) (* y y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interpolation

(defun bilinear (w a w0 w1 a0 a1 v00 v01 v10 v11)
  ;; Bilinear interpolation at P=(w a) given values f(w0, a0) = v00 etc.
  ;; If a0=a1 (w0=w1) interpolate at the resp. midpoints of v00, v01, v10, v11
  (declare (double-float w a w0 w1 a0 a1 v00 v10 v01 v11))
  (assert (<= w0 w w1))
  (assert (<= a0 a a1))
  (let* ((dw
          (if (= w0 w1) 0.5d0 (/ (- w w0) (- w1 w0))))
         (v0
          (+ v00 (* dw (- v01 v00))))
         (v1
          (+ v10 (* dw (- v11 v10))))
         (da
          (if (= a0 a1) 0.5d0 (/ (- a a0) (- a1 a0))))
         (v
          (+ v0 (* da (- v1 v0)))))
    (declare (double-float dw v0 v1 da v))
    v))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
