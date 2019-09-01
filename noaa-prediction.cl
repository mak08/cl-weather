;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2019-08-10 01:05:09>

;;; (declaim (optimize (speed 3) (debug 0) (space 1) (safety 0)))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API Functions

(defun interpolated-prediction (lat lng iparams)
  (let ((params-new (iparams-current iparams))
        (params-old (iparams-previous iparams)))
    (vr-prediction% lat lng params-new params-old)))

(defun vr-prediction (lat lng  &key (timestamp (now)) (date) (cycle))
  (let* ((params (prediction-parameters timestamp :date date :cycle cycle)))
    (vr-prediction% lat lng params)))

(defun vr-prediction% (lat lng current &optional (previous))
  (declare (inline normalized-lat normalized-lng uv-index time-interpolate angle bilinear enorm))
  (let* (
         (info (params-info current))
         (i-inc (gribinfo-i-inc info))
         (j-inc (gribinfo-j-inc info))
         (lat0 (normalized-lat (* (ffloor lat j-inc) j-inc)))
         (lng0 (normalized-lng (* (ffloor lng i-inc) i-inc)))
         (wlat (/ (- (normalized-lat lat) lat0) j-inc))
         (wlng (/ (- (normalized-lng lng) lng0) i-inc)))
    (multiple-value-bind (u00 u01 u10 u11 v00 v01 v10 v11 a00 a01 a10 a11 s00 s01 s10 s11)
        (time-interpolate lat lng info current previous)
      (position-interpolate wlat wlng s00 s01 s10 s11 a00 a01 a10 a11 u00 u01 u10 u11 v00 v01 v10 v11))))

(defun time-interpolate (lat lng info current previous)
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
    (with-bindings (((u00 v00) (time-interpolate-index i00 current previous))
                    ((u01 v01) (time-interpolate-index i01 current previous))
                    ((u10 v10) (time-interpolate-index i10 current previous))
                    ((u11 v11) (time-interpolate-index i11 current previous)))
      (values u00 u01 u10 u11
              v00 v01 v10 v11
              (angle u00 v00) (angle u01 v01) (angle u10 v10) (angle u11 v11)
              (enorm u00 v00) (enorm u01 v01) (enorm u10 v10) (enorm u11 v11)))))

(defun time-interpolate-index (index current previous)
  (let ((fraction (params-fraction current))
        (f0c1 (params-fc0 current))
        (f1c1 (params-fc1 current))
        (merge (and previous
                    (eql (params-forecast current) 3)))
        (f0c0 (when previous (params-fc0 previous)))
        (f1c0 (when previous (params-fc1 previous))))
    (log2:trace "Merging: ~a fraction ~a current fc ~a " merge fraction (params-forecast current))
    (with-bindings (((u0 v0) (grib-get-uv f0c1 index))
                    ((u1 v1) (grib-get-uv f1c1 index)))
      (cond
        ((not merge)
         (let ((u (linear fraction u0 u1))
               (v (linear fraction v0 v1)))
           (values u v)))
        (t
         (multiple-value-bind (u2 v2)
             (grib-get-uv f1c0 index)
           (let* ((uz (+ (* (- 1 fraction) u1) (* fraction u2) (- u0)))
                  (vz (+ (* (- 1 fraction) v1) (* fraction v2) (- v0)))
                  (u (+ u0 (* uz fraction)))
                  (v (+ v0 (* vz fraction))))
             (values u v))))))))

(defun position-interpolate (wlat wlng s00 s01 s10 s11 a00 a01 a10 a11 u00 u01 u10 u11 v00 v01 v10 v11)
  (flet ((avg4 (x1 x2 x3 x4)
           (/ (+ x1 x2 x3 x4) 4))
         (scoeff (a0 a1)
           (abs (sin (- a0 a1)))))
    (let* ((wind-u (bilinear wlat wlng u00 u01 u10 u11))
           (wind-v (bilinear wlat wlng v00 v01 v10 v11))
           (speed-bilinear (bilinear wlat wlng s00 s01 s10 s11))
           (speed-enorm (enorm wind-u wind-v))
           (avg-enorm (enorm (avg4 u00 u01 u10 u11) (avg4 v00 v01 v10 v11)))
           (speed-avg (avg4 s00 s10 s01 s11))
           (speed-ratio (if (> speed-avg 0) (/ avg-enorm speed-avg) 1d0)))
      (log2:trace "wlat=~,2,,'0,f wlng=~,2,,'0,f s00=~,2,,'0,f s01=~,2,,'0,f s10=~,2,,'0,f s11=~,2,,'0,f" wlat wlng s00 s01 s10 s11)
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
        (let* ((factor (if (> speed-bilinear 0)
                           (expt (/ speed-enorm speed-bilinear)
                                 (- 1d0 (expt (bilinear (* wlat 2d0) (* wlng 2d0) c00 c01 c10 c11)
                                              0.7d0)))
                           1))
               (speed (* speed-bilinear factor)))
          (log2:trace "speed-bilinear=~a speed=~a" speed-bilinear speed)
          (values (angle wind-u wind-v)
                  speed))))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
