;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2025-11-12 20:21:09>

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))


(in-package "CL-WEATHER")

(defstruct params-fw fc0 fc1 fraction)

(defun cycle-for-timestamp-fw (timestamp)
  (let* ((now (now))
         (c1 (current-cycle))
         (c0 (previous-cycle c1))
         (c1-elapsed (truncate (timestamp-difference now (cycle-timestamp c1)) 60))
         (ts-elapsed (truncate (timestamp-difference timestamp (cycle-timestamp c1)) 60))
         (step (cycle-forecast c1 timestamp))
         (c1-available (> c1-elapsed (+ 215 (truncate step 3)))))
    (if (and c1-available
             (>= ts-elapsed 360))
        c1
        c0)))

(defun get-params (timestamp)
  (let* ((cycle (cycle-for-timestamp-fw timestamp ))
         (step-0 (cycle-forecast cycle timestamp))
         (step-1 (next-forecast step-0))
         (ds-0 (load-forecast :source :noaa :resolution "0p25" :cycle cycle :offset step-0))
         (ds-1 (load-forecast :source :noaa :resolution "0p25" :cycle cycle :offset step-1))
         (fc-0 (aref (dataset-forecasts ds-0) 0)) 
         (fc-1 (aref (dataset-forecasts ds-1) 0))
         (fraction (forecast-fraction fc-0 fc-1 timestamp)))
    (make-params-fw :fc0 fc-0 :fc1 fc-1 :fraction fraction)))

(declaim (inline get-values))
(defun get-values (fraction lat lon fc-0 fc-1)
  (let* ((dataset (uv-dataset fc-0))
         (info (dataset-grib-info dataset))
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
              (s-1 (bilinear wlat wlon s00-1 s01-1 s10-1 s11-1)))
         (values (linear fraction (angle u-0 v-0) (angle u-1 v-1))
                 (linear fraction s-0 s-1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(defun interpolate-fw (lat lon fc-0 fc-1 fraction)
    (get-values fraction lat lon fc-0 fc-1))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
