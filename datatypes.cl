;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-01-13 23:29:14>

(in-package :cl-weather)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct gribfile
  "Basic / common GRIB data"
  forecast-time                 ; The timestamp (yyyymmdd, hh) of the forecast (identical with the first forecast in the bundle)
  grid-size                     ; Number of data points, should equal lat-points * lon-points
  step-units
  lat-start lat-end lat-points  ; Start, end and number of points along parallel
  lon-start lon-end lon-points  ; Start, end and number of points along meridian
  i-inc j-inc
  i-scan-neg j-scan-pos
  data                          ; Array of forecast data for successive forecast times
  )

(defmethod print-object ((thing gribfile) stream)
  (format stream "{gribfile <~a,~a>/<~a,~a> ~a}"
          (gribfile-lat-start thing)
          (gribfile-lon-start thing)
          (gribfile-lat-end thing)
          (gribfile-lon-end thing)
          (gribfile-forecast-time thing)))

(defstruct grib-filespec
  region
  resolution
  date)

(defstruct grib-values
  forecast-time                 ; 
  u-array
  v-array
  vmax-data)

(defmethod print-object ((thing grib-values) stream)
  (format stream "{grib-values ~a ~a}"
          (grib-values-forecast-time thing)
          (array-dimensions
           (grib-values-u-array thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lat&Lng
;; TODO: A latlng should only be used to represent Google Maps coordinates.

(defstruct latlng
  (lat 0 :read-only t)
  (lng 0 :read-only t)
  latr%
  lngr%)

(defmethod print-object ((thing latlng) stream)
  (format stream "[~3$, ~3$]" (latlng-lat thing) (latlng-lng thing)))

(defun latlng-latr (latlng)
  (or (latlng-latr% latlng)
      (setf (latlng-latr% latlng)
            (rad (latlng-lat latlng)))))

(defun latlng-lngr (latlng)
  (or (latlng-lngr% latlng)
      (setf (latlng-lngr% latlng)
            (rad (latlng-lng latlng)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conversion

(defun rad (x)
  (declare (double-float x))
  (* (* 2d0 pi) (/ x 360d0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Euclidian Norm

(defun enorm (x y)
  (declare (double-float x y))
  (sqrt (+ (* x x) (* y y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Converting GRIB U/V values to DEG

(defun angle (u v)
  (declare (double-float u v))
  (let ((angle
         (+ 180d0 (* 180d0 (/ (atan u v) pi)))))
    angle))

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
