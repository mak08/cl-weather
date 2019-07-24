;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2019-07-25 00:42:14>

;;; (declaim (optimize (speed 3) (debug 0) (space 1) (safety 0)))

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes
;;; * Pro:
;;;   - Allow integration of many type of forecasts
;;; * Con:
;;;   - Need to create a fresh FORECAST instance each time a forecast is requested?
;;;     (should be OK for isochrones algorithm)
;;;   - Lots of boilerplate needed for the dummy implementation?

;;; - Interface still incomplete: how to handle spatial interpolation?
;;;   - include access to i-inc/j-inc ?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Coordinate Systems
;;;
;;; Google Maps
;;;    Latitude:    0..90  from Equator to North Pole
;;;                 -90..0 from South Pole to Equator
;;;              == 90..-90 from North Pole to South Pole   
;;;    Longitude:   0..180 from Greenwich Meridien to date line
;;;                 -180..0 from date line to Greenwich Meridien
;;;
;;; GRIB (usually; if i-scans-negatively=0 and j-scans-positively = 1)
;;;    Latitude:    90..-90 from North Pole to South Pole
;;;    Longitude:   0..359 from Greenwich Meridien to EAST 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieving NOAA wind forecasts
;;;    Model: NOAA GFS
;;;    Resolution: 1 degree
;;;    GRIB2 times are UTC
;;;    NOAA GFS forecasts are produced every 6hrs (four cycles per day)
;;;
;;; Wind data
;;;    Wind data is usually stored in two variables
;;;    - U10: Zonal wind (wind from west is positive)
;;;    - V10: Meridonal wind (wind from south is positive)
;;;
;;; Forecast availability
;;;    Cycle nn starts to become available at nn+3:30 UTC.
;;;    The full cycle data is available after 1.5hrs:
;;;
;;;    Cycle  First FC avail    Full FC avail
;;;    00     03:30Z            05:00Z
;;;    06     09:30Z            11:00Z
;;;    12     15:30Z            17:00Z
;;;    18     21:30Z            23:00Z

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API Functions

(defun noaa-prediction (lat lng &key (timestamp (now)) (date) (cycle))
  (let* ((params (prediction-parameters timestamp :date date :cycle cycle)))
    (noaa-prediction% lat lng params)))


(defun simple-estimator (wlat wlng s00 s01 s10 s11 a00 a01 a10 a11 u00 u01 u10 u11 v00 v01 v10 v11)
  (let* ((wind-u (bilinear wlat wlng u00 u01 u10 u11))
         (wind-v (bilinear wlat wlng v00 v01 v10 v11))
         (speed (bilinear wlat wlng s00 s01 s10 s11))
         (angle (angle wind-u wind-v)))
    (values angle
            speed)))

(defun vr-estimator (wlat wlng s00 s01 s10 s11 a00 a01 a10 a11 u00 u01 u10 u11 v00 v01 v10 v11)
  (flet ((avg4 (x1 x2 x3 x4)
           (/ (+ x1 x2 x3 x4) 4))
         (scoeff (a0 a1)
           (abs (sin (- a0 a1)))))
    (let* ((wind-u (bilinear wlat wlng u00 u01 u10 u11))
           (wind-v (bilinear wlat wlng v00 v01 v10 v11))
           (speed-bilinear (bilinear wlat wlng s00 s01 s10 s11))
           (speed-enorm (enorm wind-u wind-v))
           (avg-enorm (enorm (avg4 u00 u10 u01 u11) (avg4 v00 v10 v01 v11)))
           (speed-avg (avg4 s00 s10 s01 s11))
           (speed-ratio (if (> speed-avg 0) (/ avg-enorm speed-avg) 1d0)))
      (multiple-value-bind (c10 c11 c00 c01)
          (cond ((< wlng 0.5d0)
                 (cond ((< wlat 0.5d0)
                        (values (scoeff a10 a00) speed-ratio 1d0 (scoeff a00 a01)))
                       (t
                        (decf wlat 0.5d0)
                        (values 1d0 (scoeff a10 a11) (scoeff a10 a00) speed-ratio))))
                (t
                 (decf wlng 0.5d0)
                 (cond ((< wlat 0.5d0)
                        (values speed-ratio (scoeff a11 a01) (scoeff a00 a01) 1d0))
                       (t
                        (decf wlat 0.5d0)
                        (values (scoeff a10 a11) 1d0 speed-ratio (scoeff a11 a01))))))
        (let* ((factor (if (> speed-bilinear 0)
                           (expt (/ speed-enorm speed-bilinear)
                                 (- 1d0 (expt (bilinear (* wlat 2d0) (* wlng 2d0) c00 c01 c10 c11)
                                              0.7d0)))
                           1))
               (speed (* speed-bilinear factor)))
          (values (angle wind-u wind-v)
                  speed))))))

(defstruct params timestamp base-time info fc0 fc1 fraction (estimator #'simple-estimator))

(defun prediction-parameters (timestamp &key (date nil) (cycle nil) (estimator #'simple-estimator))
  ;; If $data is provided, $cycle must also be provided, and the specified forecast will be used.
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
                 :fc0 fc0
                 :fc1 fc1
                 :fraction fraction
                 :estimator estimator)))

(defstruct iparams current old)

(defun interpolation-parameters (timestamp)
  (multiple-value-bind  (date1 cycle1)
      (available-cycle timestamp)
    (multiple-value-bind (date0 cycle0)
        (previous-cycle date1 cycle1)
      (make-iparams :current (prediction-parameters timestamp :date date1 :cycle cycle1 :estimator #'vr-estimator)
                    :old (prediction-parameters timestamp :date date0 :cycle cycle0 :estimator #'vr-estimator)))))

(defun base-time (iparams)
  (params-base-time (iparams-current iparams)))

(defun interpolated-prediction (lat lng iparams)
  (let ((params-new (iparams-current iparams))
        (params-old (iparams-old iparams)))
    (multiple-value-bind (a1 s1)
        (noaa-prediction% lat lng params-new)
      (let ((delta (/ (timestamp-difference (params-timestamp params-new) (params-base-time params-new)) 3600.0)))
        (cond
          ((< 3.5 delta 8.0)
           (let ((fraction (/ (- delta 3.5d0) 4.5d0))) 
             (multiple-value-bind (a0 s0)
                 (noaa-prediction% lat lng params-old)
               (multiple-value-bind (u0 v0) (p2c (rad a0) s0)
                 (multiple-value-bind (u1 v1) (p2c (rad a1) s1)
                   (let* ((w (interpolate-uv u0 v0 u1 v1 fraction))
                          (a (deg (wind-a w)))
                          (s (wind-s w)))
                     (values a s)))))))
          (t
           (values a1 s1)))))))
           
(defun test-prediction (lat lng &key (timestamp (now)))
  (let ((iparams (interpolation-parameters timestamp)))
    (interpolated-prediction lat lng iparams)))

(defun noaa-prediction% (lat lng params)
  (declare (inline normalized-lat normalized-lng uv-index time-interpolate angle bilinear enorm))
  (let* ((info (params-info params))
         (fc0 (params-fc0 params))
         (fc1 (params-fc1 params))
         (fraction (params-fraction params))
         (i-inc (gribinfo-i-inc info))
         (j-inc (gribinfo-j-inc info))
         (lat0 (normalized-lat (* (ffloor lat j-inc) j-inc)))
         (lng0 (normalized-lng (* (ffloor lng i-inc) i-inc)))
         (lat1 (normalized-lat (+ lat0 j-inc)))
         (lng1 (normalized-lng (+ lng0 i-inc)))
         (w00  (time-interpolate fc0 fc1 fraction (uv-index info lat0 lng0)))
         (w01  (time-interpolate fc0 fc1 fraction (uv-index info lat0 lng1)))
         (w10  (time-interpolate fc0 fc1 fraction (uv-index info lat1 lng0)))
         (w11  (time-interpolate fc0 fc1 fraction (uv-index info lat1 lng1)))
         (wlat (/ (- (normalized-lat lat) lat0) j-inc))
         (wlng (/ (- (normalized-lng lng) lng0) i-inc)))
    (log2:trace "LAT0:~a LAT1:~a WLAT:~a LNG0:~a LNG1:~a WLNG:~a" lat0 lat1 wlat lng0 lng1 wlng)  
    (with-accessors ((s00 wind-s) (a00 wind-a) (u00 wind-u) (v00 wind-v))
        w00
      (with-accessors ((s01 wind-s) (a01 wind-a) (u01 wind-u) (v01 wind-v))
          w01
        (with-accessors ((s10 wind-s) (a10 wind-a) (u10 wind-u) (v10 wind-v))
            w10
          (with-accessors ((s11 wind-s) (a11 wind-a) (u11 wind-u) (v11 wind-v))
              w11
            (funcall (params-estimator params) wlat wlng s00 s01 s10 s11 a00 a01 a10 a11 u00 u01 u10 u11 v00 v01 v10 v11)))))))

(defun format-datetime (stream timestamp &key (timezone +utc-zone+))
  (format stream "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
          (timestamp-year timestamp :timezone timezone)
          (timestamp-month timestamp :timezone timezone)
          (timestamp-day timestamp :timezone timezone)
          (timestamp-hour timestamp :timezone timezone)
          (timestamp-minute timestamp :timezone timezone)
          (timestamp-second timestamp :timezone timezone)))

(defun dataset-forecast (dataset)
  (aref (dataset-forecasts dataset) 0))

(defun forecast-fraction (fc0 fc1 timestamp)
  (let ((delta (timestamp-difference (uv-forecast-time fc1) (uv-forecast-time fc0)))
        (s0 (timestamp-difference timestamp (uv-forecast-time fc0))))
    (assert (plusp delta))
    (assert (not (minusp s0)))
    (let ((fraction (coerce (/ s0 delta) 'double-float)))
      (log2:trace "TS: ~a FC0: ~a FC1: ~a Fraction: ~a" timestamp fc0 fc1 fraction)
      fraction)))

(defun time-interpolate (fc0 fc1 fraction index)
  (declare (inline interpolate-uv))
  (multiple-value-bind (u0 v0)
      (grib-get-uv fc0 index)
      (cond
        ((eql fraction 0)
         (make-wind :u u0 :v v0 :s (enorm u0 v0) :a (angle u0 v0)))
        (t
         (multiple-value-bind (u1 v1)
             (grib-get-uv fc1 index)
           (interpolate-uv u0 v0 u1 v1 fraction))))))

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
    (log2:trace "Lat: ~a Lng: ~a Index: ~a" lat lon uv-index)
    uv-index))

(defun grib-get-uv (uv index)
  (let ((u (aref (uv-u-array uv) index))
        (v (aref (uv-v-array uv) index)))
    (log2:trace "~a => ~a,~a" index (angle u v) (enorm u v))
    (values u v)))

(defun interpolate-uv (u0 v0 u1 v1 fraction)
  (declare (inline enorm p2c linear angle-r))
  (let*
      ((s0 (enorm u0 v0))
       (s1 (enorm u1 v1))
       (s (linear fraction s0 s1))
       (u (linear fraction u0 u1))
       (v (linear fraction v0 v1))
       (a (angle-r u v)))
    (multiple-value-bind (u v)
        (p2c a s)
      (make-wind :u u :v v :a a :s s))))


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


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
