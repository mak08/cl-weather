;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2026-04-03 22:42:25>

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))

(in-package "CL-WEATHER")

(declaim (inline get-value))
(defun get-value (fraction lat lon fc-0 fc-1)
  (declare (ftype (function (t) (array double-float (*))) gribmessage-data))
  (let* ((info (gribmessage-info fc-0))
         (data-0 (gribmessage-data fc-0))
         (data-1 (gribmessage-data fc-1))
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
    (let ((u00-0 (aref data-0 i00))
          (u10-0 (aref data-0 i10))
          (u01-0 (aref data-0 i01))
          (u11-0 (aref data-0 i11))
          (u00-1 (aref data-1 i00))
          (u10-1 (aref data-1 i10))
          (u01-1 (aref data-1 i01))
          (u11-1 (aref data-1 i11)))
      (let* ((u-0 (bilinear wlat wlon u00-0 u01-0 u10-0 u11-0))
             (u-1 (bilinear wlat wlon u00-1 u01-1 u10-1 u11-1))
             (u (linear fraction u-0 u-1)))
        (values u)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(declaim (inline interpolate-simple))
(defun interpolate-simple (lat lon params)
  (let ((fc-0 (params-fc0 params))
        (fc-1 (params-fc1 params))
        (fraction (params-fraction params)))
    (cond
      ((valid-position lat lon (gribmessage-info fc-0))
       (get-value fraction lat lon fc-0 fc-1))
      (t
       (values 0d0)))))
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
