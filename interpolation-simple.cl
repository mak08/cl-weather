;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2026-04-03 22:42:25>

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))

(in-package "CL-WEATHER")

(declaim (inline get-value))
(defun get-value (fraction lat lon fc-0 fc-1 &optional circular)
  "Bilinear/temporal interpolation. With CIRCULAR, values are directions in degrees
and are interpolated via their sine/cosine components."
  (declare (double-float fraction lat lon)
           (type gribmessage fc-0 fc-1))
  (let ((data-0 (gribmessage-data fc-0))
        (data-1 (gribmessage-data fc-1)))
    (multiple-value-bind (i00 i01 i10 i11 wlat wlon)
        (regular-grid-cell (gribmessage-info fc-0) lat lon)
      (unless i00 (return-from get-value 0d0))
      (let ((u00-0 (aref data-0 i00))
            (u10-0 (aref data-0 i10))
            (u01-0 (aref data-0 i01))
            (u11-0 (aref data-0 i11))
            (u00-1 (aref data-1 i00))
            (u10-1 (aref data-1 i10))
            (u01-1 (aref data-1 i01))
            (u11-1 (aref data-1 i11)))
        (if circular
            (flet ((comp (fn)
                     (declare (function fn))
                     (flet ((c (d) (funcall fn (* d (/ pi 180d0)))))
                       (let* ((a-0 (bilinear wlat wlon (c u00-0) (c u01-0) (c u10-0) (c u11-0)))
                              (a-1 (bilinear wlat wlon (c u00-1) (c u01-1) (c u10-1) (c u11-1))))
                         (linear fraction a-0 a-1)))))
              (let ((s (comp #'sin))
                    (c (comp #'cos)))
                (declare (double-float s c))
                (values (mod (* (atan s c) (/ 180d0 pi)) 360d0))))
            (let* ((u-0 (bilinear wlat wlon u00-0 u01-0 u10-0 u11-0))
                   (u-1 (bilinear wlat wlon u00-1 u01-1 u10-1 u11-1))
                   (u (linear fraction u-0 u-1)))
              (values u)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(declaim (inline interpolate-simple))
(defun interpolate-simple (lat lon params)
  (declare (double-float lat lon)
           (type params params))
  (let ((fc-0 (params-fc0 params))
        (fc-1 (params-fc1 params))
        (fraction (params-fraction params)))
    (declare (type gribmessage fc-0 fc-1))
    (get-value fraction lat lon fc-0 fc-1)))

(declaim (inline interpolate-direction))
(defun interpolate-direction (lat lon params)
  "Interpolate a direction field (degrees); PARAMS from GET-DIRECTION-PARAMS."
  (declare (double-float lat lon)
           (type params params))
  (let ((fc-0 (params-fc0 params))
        (fc-1 (params-fc1 params))
        (fraction (params-fraction params)))
    (declare (type gribmessage fc-0 fc-1))
    (get-value fraction lat lon fc-0 fc-1 t)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
