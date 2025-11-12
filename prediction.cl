;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2025-11-11 20:50:50>

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))


(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes
;;; Interpolation process of wind at (t, lat, lon): 
;;; -  Componentwise linear interpolation at enclosing grid point of FC1 and FC2
;;;    yields component winds (u_i, v_i) at enclosing grid points for t.
;;; -  Also compute speeds s_i and directions d_i at these points
;;; -  From these compute bilinear interpolation of components (u, v) and speed s_s at (lat, lon).
;;;
;;; -  Compute final direction d from (u, v)
;;;
;;; -  Compute s_uv = enorm(u, v)

(declaim (inline position-interpolate))
(defun position-interpolate (method wlat wlng u00 u01 u10 u11 v00 v01 v10 v11)
  (let* ((wind-u (bilinear wlat wlng u00 u01 u10 u11))
         (wind-v (bilinear wlat wlng v00 v01 v10 v11))
         (s00 (enorm u00 v00))
         (s01 (enorm u01 v01))
         (s10 (enorm u10 v10))
         (s11 (enorm u11 v11)))
    (ecase method
      (:enorm
       (values (angle wind-u wind-v)
               (enorm wind-u wind-v)))
      (:bilinear
       (values (angle wind-u wind-v)
               (bilinear wlat wlng s00 s01 s10 s11))))))

(declaim (inline time-interpolate-index))
(defun time-interpolate-index (index current previous merge-fraction)
  (let* ((fraction (if previous (params-fraction previous) (params-fraction current)))
         (cycle1-fc0 (when current (params-fc0 current)))
         (cycle1-fc1 (when current (params-fc1 current)))
         (cycle0-fc0 (when previous (params-fc0 previous)))
         (cycle0-fc1 (when previous (params-fc1 previous))))
    (cond
      ((and current
            previous)
       (with-bindings (((u00 v00) (grib-get-uv cycle0-fc0 index))
                       ((u10 v10) (grib-get-uv cycle0-fc1 index))
                       ((u01 v01) (grib-get-uv cycle1-fc0 index))
                       ((u11 v11) (grib-get-uv cycle1-fc1 index)))
         (let ((u0 (linear fraction u00 u10))
               (v0 (linear fraction v00 v10))
               (u1 (linear fraction u01 u11))
               (v1 (linear fraction v01 v11)))
           (let* ((uz (linear merge-fraction u0 u1))
                  (vz (linear merge-fraction v0 v1)))
             (values uz vz)))))
      (previous
       (with-bindings (((u0 v0) (grib-get-uv cycle0-fc0 index))
                       ((u1 v1) (grib-get-uv cycle0-fc1 index)))
         (let ((u (linear fraction u0 u1))
               (v (linear fraction v0 v1)))
           (values u v))))
      (current
       (with-bindings (((u0 v0) (grib-get-uv cycle1-fc0 index))
                       ((u1 v1) (grib-get-uv cycle1-fc1 index)))
         (let ((u (linear fraction u0 u1))
               (v (linear fraction v0 v1)))
           (values u v))))
      (T
       (error "Have neither current nor previous")))))

(declaim (inline time-interpolate))
(defun time-interpolate (lat lng info current previous merge-fraction)
  (declare (inline grib-get-uv))
  (let* ((i-inc (gribinfo-i-inc info))
         (j-inc (gribinfo-j-inc info))
         (lat0 (normalized-lat (* (ffloor lat j-inc) j-inc)))
         (lng0 (normalized-lng (* (ffloor lng i-inc) i-inc)))
         (lat1 (normalized-lat (+ lat0 j-inc)))
         (lng1 (normalized-lng (+ lng0 i-inc)))
         (i00 (uv-index info lat0 lng0))
         (i01 (uv-index info lat0 lng1))
         (i10 (uv-index info lat1 lng0))
         (i11 (uv-index info lat1 lng1)))
    (declare (double-float lat lng i-inc j-inc lat0 lng0 lat1 lng1)
             (fixnum i00 i01 i10 i11))
    (with-bindings (((u00 v00) (time-interpolate-index i00 current previous merge-fraction))
                    ((u01 v01) (time-interpolate-index i01 current previous merge-fraction))
                    ((u10 v10) (time-interpolate-index i10 current previous merge-fraction))
                    ((u11 v11) (time-interpolate-index i11 current previous merge-fraction)))
      (values u00 u01 u10 u11
              v00 v01 v10 v11))))

(declaim (inline interpolate%))
(defun interpolate% (lat lng current &optional previous merge-fraction)
  (declare (inline normalized-lat normalized-lng uv-index time-interpolate angle bilinear enorm))
  (let* ((info (if previous (params-info previous) (params-info current)))
         (method (if previous (params-method previous) (params-method current)))
         (i-inc (gribinfo-i-inc info))
         (j-inc (gribinfo-j-inc info))
         (lat0 (normalized-lat (* (ffloor lat j-inc) j-inc)))
         (lng0 (normalized-lng (* (ffloor lng i-inc) i-inc)))
         (wlat (/ (- (normalized-lat lat) lat0) j-inc))
         (wlng (/ (- (normalized-lng lng) lng0) i-inc)))
    (declare (double-float lat lng i-inc j-inc lat0 lng0 wlat wlng))
    (multiple-value-bind (u00 u01 u10 u11 v00 v01 v10 v11)
        (time-interpolate lat lng info current previous merge-fraction)
      (position-interpolate method wlat wlng u00 u01 u10 u11 v00 v01 v10 v11))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API Functions

(defun interpolate (lat lng iparams)
  (let ((params-new (iparams-current iparams))
        (params-old (iparams-previous iparams))
        (merge-fraction (iparams-merge-fraction iparams)))
    (interpolate% lat lng params-new params-old merge-fraction)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
