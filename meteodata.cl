;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Meteorological Data
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2019-03-04 00:35:16>

(declaim (optimize (speed 3) (debug 0) (space 1) (safety 1)))

(in-package :cl-weather)

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
;;; ===========
;;; Local files
;;; ===========

(defvar *grib-folder* "/home/michael/Wetter/grib2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ===========
;;; Datasources
;;; ===========

;;; Load current forecast bundle from the datasource (initial load).
;;; Overwrite previously loaded data.
(defgeneric load-dataset (datasource)
  )

;;; Returns the currently loaded forecast bundle from the datasource.
;;; Implicitely load bundle if necessary.
;;; Methods should use EQL-specializers on the dataset class name.
(defgeneric get-dataset (datasource)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get the time of the first forecast in the bundle
(defgeneric dataset-time (dataset)
  )
(defmethod dataset-time ((dataset dataset))
  (dataset-basetime dataset))

(defgeneric dataset-max-offset (dataset)
  )
(defmethod dataset-max-offset ((dataset dataset))
  (let
      ((last-values (find-if-not #'null (dataset-forecasts dataset) :from-end t)))
    (/
     (uv-offset last-values)
     60)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get the forecast for a specified time 
(defgeneric get-forecast (dataset utc-time)
  )

(defmethod get-forecast ((dataset dataset) (utc-time local-time:timestamp))
  ;; New procedure:
  ;; - Round time to minute
  ;; - Check if forecast exists at dataset and is still valid
  (let* ((fc-time (timestamp-minimize-part utc-time :sec))
         (fc-offset (truncate
                     (/ (timestamp-difference fc-time (dataset-basetime dataset))
                        60))))
    (make-instance 'forecast
                   :dataset dataset
                   :time fc-time
                   :offset fc-offset)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Obtain wind forecast data (direction, speed)
;;;
;;; Speed is in m/s,
;;; Direction is in deg; north is 0/360; clockwise.
;;;
;;; TBD: Should this method allow only latlng on grid points or interpolate?
(defgeneric get-wind-forecast (forecast latlng))

(declaim (inline avg4))
(defun avg4 (x1 x2 x3 x4)
  (/ (+ x1 x2 x3 x4) 4))

(declaim (inline scoeff))
(defun scoeff (a0 a1)
  (abs (sin (- a0 a1))))

(defmethod get-wind-forecast ((forecast forecast) (latlng vector))
  ;; New interpolation procedure:
  ;; - Round position to Second / 10Seconds ?
  ;; - Perform bilinear interpolation of the required supporting points (cached) to the fc time
  ;; - Perform bilinear interpolation to the required latlng.
  (declare (inline latlng-lat latlng-lng time-interpolate angle bilinear enorm))
  (let* ((lat (latlng-lat latlng))
         (lng% (latlng-lng latlng))
         (lng   (if (< lng% 0d0)
                    (+ lng% 360d0)
                    lng%))
         (dataset (fc-dataset forecast))
         (offset (fc-offset forecast))
         (index (position offset (dataset-forecasts dataset)
                          :test #'<=
                          ;; Some grib-values may be NULL
                          :key #'(lambda (fc)
                                   (etypecase fc
                                     (null -1)
                                     (blended-forecast
                                      (uv-offset (blended-forecast-new fc)))
                                     (uv
                                      (uv-offset fc)))))))
    (cond
      ((null index)
       (values nil nil))
      (t
       (let*
           ((info (dataset-grib-info dataset))
            (i-inc (gribinfo-i-inc info))
            (j-inc (gribinfo-j-inc info))
            (lat0 (* (ffloor lat j-inc) j-inc))
            (lng0 (* (ffloor lng i-inc) i-inc))
            (lat1 (+ lat0 j-inc))
            (lng1 (+ lng0 i-inc))
            (wlat (/ (- lat lat0) (- lat1 lat0)))
            (wlng (/ (- lng lng0) (- lng1 lng0)))
            (w00  (time-interpolate dataset offset index lat0 lng0))
            (w01  (time-interpolate dataset offset index lat0 lng1))
            (w10  (time-interpolate dataset offset index lat1 lng0))
            (w11  (time-interpolate dataset offset index lat1 lng1)))
         (with-accessors ((s00 wind-s) (a00 wind-a) (u00 wind-u) (v00 wind-v))
             w00
           (with-accessors ((s01 wind-s) (a01 wind-a) (u01 wind-u) (v01 wind-v))
               w01
             (with-accessors ((s10 wind-s) (a10 wind-a) (u10 wind-u) (v10 wind-v))
                 w10
               (with-accessors ((s11 wind-s) (a11 wind-a) (u11 wind-u) (v11 wind-v))
                   w11
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
                               speed)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ================
;;; Periodic updates
;;; ================

(defgeneric update-dataset (dataset &key &allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ====================
;;; Dummy implementation
;;; ====================


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
