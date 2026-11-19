;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2026-04-04 18:52:54>

(declaim (optimize (speed 3) (debug 1) (space 1) (safety 1)))

(in-package "CL-WEATHER")

(defstruct params
  (fc0 nil :type (or null uv gribmessage))
  (fc1 nil :type (or null uv gribmessage))
  (fraction 0d0 :type double-float))

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
    (make-params :fc0 fc-0 :fc1 fc-1 :fraction fraction)))

(defun get-direction-params (datasource timestamp
                             &key
                               (cycle (timestamp-cycle datasource timestamp)))
  "Like GET-PARAMS, but for the direction field of wave datasources."
  (handler-case
      (get-direction-params-for-cycle datasource timestamp :cycle cycle)
    (missing-forecast ()
      (get-direction-params-for-cycle datasource timestamp
                                      :cycle (previous-cycle datasource cycle)))))

(defun get-direction-params-for-cycle (datasource-classname timestamp &key cycle)
  (let* ((datasource (get-datasource datasource-classname cycle))
         (step-0 (cycle-forecast datasource timestamp))
         (step-1 (next-forecast datasource step-0))
         (fc-0 (load-direction-forecast datasource step-0))
         (fc-1 (load-direction-forecast datasource step-1))
         (fraction (forecast-fraction fc-0 fc-1 timestamp)))
    (make-params :fc0 fc-0 :fc1 fc-1 :fraction fraction)))

(declaim (inline regular-grid-cell))
(defun regular-grid-cell (info lat lon)
  "Return four row-major indices and scan-relative weights, or NIL outside.
Anchor coordinates to the first GRIB node, not a multiple of the increment at
0 degrees. Only global longitude grids wrap their last cell to the first node;
regional grids crossing 0/180 degrees retain finite coverage. Edge nodes and
one-node axes use repeated indices rather than accessing a nonexistent neighbour."
  (declare (double-float lat lon)
           (type gribinfo info))
  (let ((ni (gribinfo-lon-points info)) (nj (gribinfo-lat-points info))
        (dx (gribinfo-i-inc info)) (dy (gribinfo-j-inc info)))
    (unless (and (plusp ni) (plusp nj) (plusp dx) (plusp dy))
      (return-from regular-grid-cell nil))
    (let* ((epsilon 1d-7) ; Grid-cell units, for coordinate roundoff only.
           (row (/ (if (= 1 (gribinfo-j-scan-pos info))
                       (- lat (gribinfo-lat-start info))
                       (- (gribinfo-lat-start info) lat)) dy))
           (offset (mod (if (= 1 (gribinfo-i-scan-neg info))
                            (- (gribinfo-lon-start info) lon)
                            (- lon (gribinfo-lon-start info))) 360d0))
           (column (if (> offset (- 360d0 (* dx epsilon))) 0d0 (/ offset dx)))
           (global-p (>= (* ni dx) (- 360d0 (* dx epsilon)))))
      (unless (and (<= (- epsilon) row (+ (1- nj) epsilon))
                   (or global-p (<= column (+ (1- ni) epsilon))))
        (return-from regular-grid-cell nil))
      (setf row (max 0d0 (min (float (1- nj) 1d0) row)))
      (unless global-p
        (setf column (min (float (1- ni) 1d0) column)))
      (let* ((r0 (floor row)) (c0 (floor column))
             (r1 (min (1+ r0) (1- nj)))
             (c1 (if global-p (mod (1+ c0) ni) (min (1+ c0) (1- ni)))))
        (values (+ (* r0 ni) c0) (+ (* r0 ni) c1)
                (+ (* r1 ni) c0) (+ (* r1 ni) c1)
                (- row r0) (- column c0))))))

(declaim (inline valid-position))
(defun valid-position (lat lon info)
  "Whether the coordinate lies within the regular grid's actual node extent."
  (not (null (regular-grid-cell info lat lon))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
