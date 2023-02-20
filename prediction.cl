;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2023-02-20 16:54:40>

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

(declaim (inline magnitude-factor))
(defun-t magnitude-factor double-float
    ((wlat double-float)
     (wlng double-float)
     (wind-u double-float)
     (wind-v double-float)
     (s00 double-float)
     (s01 double-float)
     (s10 double-float)
     (s11 double-float)
     (u00 double-float)
     (u01 double-float)
     (u10 double-float)
     (u11 double-float)
     (v00 double-float)
     (v01 double-float)
     (v10 double-float)
     (v11 double-float))
  (flet ((avg4 (x1 x2 x3 x4)
           (/ (+ x1 x2 x3 x4) 4))
         (scoeff (a0 a1)
           (abs (sin (- a0 a1)))))
    (let* ((a00 (angle u00 v00))
           (a01 (angle u01 v01))
           (a10 (angle u10 v10))
           (a11 (angle u11 v11))
           (avg-enorm (enorm (avg4 u00 u01 u10 u11) (avg4 v00 v01 v10 v11)))
           (speed-bilinear (bilinear wlat wlng s00 s01 s10 s11))
           (speed-enorm (enorm wind-u wind-v))
           (speed-avg (avg4 s00 s10 s01 s11))
           (speed-ratio (if (> speed-avg 0) (/ avg-enorm speed-avg) 1d0)))
      (multiple-value-bind (c10 c11 c00 c01)
          (cond ((< wlng 0.5d0)
                 (cond ((< wlat 0.5d0)
                        ;; left top
                        (values (scoeff a10 a00) speed-ratio 1d0 (scoeff a00 a01)))
                       (t
                        ;; left bottom
                        (decf wlat 0.5d0)
                        (values 1d0 (scoeff a10 a11) (scoeff a10 a00) speed-ratio))))
                (t
                 (decf wlng 0.5d0)
                 (cond ((< wlat 0.5d0)
                        ;; right top
                        (values speed-ratio (scoeff a11 a01) (scoeff a00 a01) 1d0))
                       (t
                        ;; right bottom
                        (decf wlat 0.5d0)
                        (values (scoeff a10 a11) 1d0 speed-ratio (scoeff a11 a01))))))
        (if (> speed-bilinear 0)
            (expt (/ speed-enorm speed-bilinear)
                  (- 1d0 (expt (bilinear (* wlat 2d0) (* wlng 2d0) c00 c01 c10 c11)
                               0.7d0)))
            1d0)))))

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
               (bilinear wlat wlng s00 s01 s10 s11)))
      (:vr
       (values (angle wind-u wind-v)
               (*
                (bilinear wlat wlng s00 s01 s10 s11)
                (magnitude-factor wlat wlng wind-u wind-v s00 s01 s10 s11 u00 u01 u10 u11 v00 v01 v10 v11)))))))

(declaim (inline time-interpolate-index))
(defun time-interpolate-index (index current offset previous)
  (let* ((fraction (if previous (params-fraction previous) (params-fraction current)))
         (cycle1-fc0 (when current (params-fc0 current)))
         (cycle1-fc1 (when current (params-fc1 current)))
         (cycle0-fc0 (when previous (params-fc0 previous)))
         (cycle0-fc1 (when previous (params-fc1 previous)))
         (merge-start (if previous (params-merge-start previous) (params-merge-start current)))
         (merge-window (if previous (params-merge-window previous) (params-merge-window current))))
    (cond
      ((and current previous)
       (with-bindings (((u00 v00) (grib-get-uv cycle0-fc0 index))
                       ((u10 v10) (grib-get-uv cycle0-fc1 index))
                       ((u01 v01) (grib-get-uv cycle1-fc0 index))
                       ((u11 v11) (grib-get-uv cycle1-fc1 index)))
         (let ((u0 (linear fraction u00 u10))
               (v0 (linear fraction v00 v10))
               (u1 (linear fraction u01 u11))
               (v1 (linear fraction v01 v11))
               (d (/ (- offset merge-start) merge-window)))
           (let* ((uz (linear d u0 u1))
                  (vz (linear d v0 v1)))
             (values uz vz)))))
      ((and previous)
       (with-bindings (((u0 v0) (grib-get-uv cycle0-fc0 index))
                       ((u1 v1) (grib-get-uv cycle0-fc1 index)))
         (let ((u (linear fraction u0 u1))
               (v (linear fraction v0 v1)))
           (values u v))))
      ((and current)
       (with-bindings (((u0 v0) (grib-get-uv cycle1-fc0 index))
                       ((u1 v1) (grib-get-uv cycle1-fc1 index)))
         (let ((u (linear fraction u0 u1))
               (v (linear fraction v0 v1)))
           (values u v))))
      (T
       (error "Have neither current nor previous")))))

(declaim (inline time-interpolate))
(defun time-interpolate (lat lng info current offset-new previous)
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
    (with-bindings (((u00 v00) (time-interpolate-index i00 current offset-new previous))
                    ((u01 v01) (time-interpolate-index i01 current offset-new previous))
                    ((u10 v10) (time-interpolate-index i10 current offset-new previous))
                    ((u11 v11) (time-interpolate-index i11 current offset-new previous)))
      (values u00 u01 u10 u11
              v00 v01 v10 v11))))

(defun vr-prediction% (lat lng current offset-new &optional previous)
  (declare (inline normalized-lat normalized-lng uv-index time-interpolate angle bilinear enorm))
  (let* ((info (if previous (params-info previous) (params-info current)))
         (method (if previous (params-method previous) (params-method current)))
         (i-inc (gribinfo-i-inc info))
         (j-inc (gribinfo-j-inc info))
         (lat0 (normalized-lat (* (ffloor lat j-inc) j-inc)))
         (lng0 (normalized-lng (* (ffloor lng i-inc) i-inc)))
         (wlat (/ (- (normalized-lat lat) lat0) j-inc))
         (wlng (/ (- (normalized-lng lng) lng0) i-inc)))
    (multiple-value-bind (u00 u01 u10 u11 v00 v01 v10 v11)
        (time-interpolate lat lng info current offset-new previous)
      (position-interpolate method wlat wlng u00 u01 u10 u11 v00 v01 v10 v11))))


(defun interpolate% (method lat lng offset p0 p1)
  ;; (assert (not (and p0 p1)))
  (let* ((p (or p0 p1))
         (info (params-info p))
         (i-inc (gribinfo-i-inc info))
         (j-inc (gribinfo-j-inc info))

         (lat0 (normalized-lat (* (ffloor lat j-inc) j-inc)))
         (lng0 (normalized-lng (* (ffloor lng i-inc) i-inc)))
         (lat1 (normalized-lat (+ lat0 j-inc)))
         (lng1 (normalized-lng (+ lng0 i-inc)))

         (wlat (/ (- (normalized-lat lat) lat0) j-inc))
         (wlng (/ (- (normalized-lng lng) lng0) i-inc))
         
         (i00 (uv-index info lat0 lng0))
         (i01 (uv-index info lat0 lng1))
         (i10 (uv-index info lat1 lng0))
         (i11 (uv-index info lat1 lng1))

         (fraction (params-fraction p))
         
         (fc0 (params-fc0 p))
         (fc1 (params-fc1 p)))
    (with-bindings (((u0_00 v0_00) (grib-get-uv fc0 i00))
                    ((u0_10 v0_10) (grib-get-uv fc0 i10))
                    ((u0_01 v0_01) (grib-get-uv fc0 i01))
                    ((u0_11 v0_11) (grib-get-uv fc0 i11))
                    ((u1_00 v1_00) (grib-get-uv fc1 i00))
                    ((u1_10 v1_10) (grib-get-uv fc1 i10))
                    ((u1_01 v1_01) (grib-get-uv fc1 i01))
                    ((u1_11 v1_11) (grib-get-uv fc1 i11)))
      
      (with-bindings
          (((a0 s0) (position-interpolate method wlat wlng u0_00 u0_01 u0_10 u0_11 v0_00 v0_01 v0_10 v0_11))
           ((a1 s1) (position-interpolate method wlat wlng u1_00 u1_01 u1_10 u1_11 v1_00 v1_01 v1_10 v1_11)))
        (with-bindings
            (((u0 v0) (p2c (rad a0) s0))
             ((u1 v1) (p2c (rad a1) s1)))
          (let ((u (linear fraction u0 u1))
                (v (linear fraction v0 v1))
                (s (linear fraction s0 s1)))
            (values (angle u v)
                    s)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API Functions

(defun interpolated-prediction (lat lng iparams)
  (let ((params-new (iparams-current iparams))
        (offset-new (iparams-offset iparams))
        (params-old (iparams-previous iparams)))
    (vr-prediction% lat lng params-new  offset-new params-old)))

(defun interpolate (lat lng iparams)
  (let ((method (iparams-method iparams))
        (offset (iparams-offset iparams))
        (p0 (iparams-current iparams))
        (p1 (iparams-previous iparams)))
    (interpolate% method lat lng offset p0 p1)))

(defun vr-prediction (lat lng  &key (timestamp (now)) (cycle (make-cycle :timestamp timestamp)) (resolution "1p00"))
  (let* ((params (prediction-parameters timestamp :cycle cycle :resolution resolution)))
    (vr-prediction% lat lng params nil)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
