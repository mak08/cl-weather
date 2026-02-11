;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2026-02-11 21:47:18>

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))


(in-package "CL-WEATHER")

(defstruct params-fw fc0 fc1 fraction)

(defun get-params (datasource timestamp
                   &key
                     (cycle (timestamp-cycle datasource timestamp)))
  (handler-case
      (get-params-for-cycle datasource timestamp :cycle cycle)
    (missing-forecast (e)
      (get-params-for-cycle datasource timestamp :cycle (previous-cycle datasource cycle)))))
      
(defun get-params-for-cycle (datasource-classname timestamp &key cycle)
  (let* ((datasource (get-datasource datasource-classname cycle))
         (step-0 (cycle-forecast datasource timestamp))
         (step-1 (next-forecast datasource step-0))
         (fc-0 (load-forecast datasource step-0))
         (fc-1 (load-forecast datasource step-1))
         (fraction (forecast-fraction fc-0 fc-1 timestamp)))
    (make-params-fw :fc0 fc-0 :fc1 fc-1 :fraction fraction)))

(declaim (inline get-values))
(defun get-values (fraction lat lon fc-0 fc-1)
  (let* ((info (uv-info fc-0))
         (i-inc (gribinfo-i-inc info))
         (j-inc (gribinfo-j-inc info))
         (lat0 (normalized-lat (* (ffloor lat j-inc) j-inc)))
         (lon0 (normalized-lng (* (ffloor lon i-inc) i-inc)))
         (lat1 (normalized-lat (+ lat0 j-inc)))
         (lon1 (normalized-lng (+ lon0 i-inc)))
         (i00 (uv-index info lat0 lon0))
         (i01 (uv-index info lat0 lon1))
         (i10 (uv-index info lat1 lon0))
         (i11 (uv-index info lat1 lon1))
         (wlat (/ (- (normalized-lat lat) lat0) j-inc))
         (wlon (/ (- (normalized-lng lon) lon0) i-inc)))
    (declare (double-float lat lon wlat wlon i-inc j-inc lat0 lon0 lat1 lon1)
             (fixnum i00 i01 i10 i11))
    (with-bindings (((u00-0 v00-0) (grib-get-uv fc-0 i00))
                    ((u10-0 v10-0) (grib-get-uv fc-0 i10))
                    ((u01-0 v01-0) (grib-get-uv fc-0 i01))
                    ((u11-0 v11-0) (grib-get-uv fc-0 i11))
                    ((u00-1 v00-1) (grib-get-uv fc-1 i00))
                    ((u10-1 v10-1) (grib-get-uv fc-1 i10))
                    ((u01-1 v01-1) (grib-get-uv fc-1 i01))
                    ((u11-1 v11-1) (grib-get-uv fc-1 i11)))
       (let* ((u-0 (bilinear wlat wlon u00-0 u01-0 u10-0 u11-0))
              (v-0 (bilinear wlat wlon v00-0 v01-0 v10-0 v11-0))
              (u-1 (bilinear wlat wlon u00-1 u01-1 u10-1 u11-1))
              (v-1 (bilinear wlat wlon v00-1 v01-1 v10-1 v11-1))
              (s00-0 (enorm u00-0 v00-0))
              (s01-0 (enorm u01-0 v01-0))
              (s10-0 (enorm u10-0 v10-0))
              (s11-0 (enorm u11-0 v11-0))
              (s-0 (bilinear wlat wlon s00-0 s01-0 s10-0 s11-0))
              (s00-1 (enorm u00-1 v00-1))
              (s01-1 (enorm u01-1 v01-1))
              (s10-1 (enorm u10-1 v10-1))
              (s11-1 (enorm u11-1 v11-1))
              (s-1 (bilinear wlat wlon s00-1 s01-1 s10-1 s11-1))
              (u (linear fraction u-0 u-1))
              (v (linear fraction v-0 v-1))
              (a (angle u v))
              (s (linear fraction s-0 s-1)))
         (values a s u v)))))

(defun valid-position (lat lon info)
  (let ((lat-start (gribinfo-lat-start info))
        (lat-end (gribinfo-lat-end info))
        (lon-start (gribinfo-lon-start info))
        (lon-end (gribinfo-lon-end info))
        (j-scan-pos-p (eql (gribinfo-j-scan-pos info) 1)))
    (and (if j-scan-pos-p
             (<= lat-start lat lat-end)
             (<= lat-end lat lat-start))
         (<= lon-start (mod lon 360) lon-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(declaim (inline interpolate-fw))
(defun interpolate-fw (lat lon params)
  (let ((fc-0 (params-fw-fc0 params))
        (fc-1 (params-fw-fc1 params))
        (fraction (params-fw-fraction params)))
    (cond
      ((valid-position lat lon (uv-info fc-0))
       (get-values fraction lat lon fc-0 fc-1))
      (t
       (values 0d0 0d0)))))

(defun get-wind-fw (datasource timestamp lat lon)
  (multiple-value-bind (d s)
      (interpolate-fw lat lon (get-params datasource timestamp))
    (values d (m/s-to-knots s))))
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
