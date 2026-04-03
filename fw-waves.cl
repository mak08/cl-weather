;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   GRIB data sources
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2026-03-31 21:55:35>

(in-package "CL-WEATHER")

;;; FW  waves

;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/gfswave/20260328/00/20260328_00_0p50_006_to_240_gfswave-combined.grib2
;;;"https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/gfswave//20260328/00_20260328_0p50_006_to_240_gfswave-combined.grib2"

(defclass gfswave (fw-datasource datakind-wave)
  ((name :initform "gfswave")
   (location :initform "https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/gfswave/")
   (maxstep :initform 48)))
(defclass gfswave-combined (gfswave)
  ((name :initform "gfswave-combined")
   (schedule :initform (datasource-schedule 'gfswave-combined))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FW

(defmethod datasource-schedule ((datasource (eql 'gfswave-combined)))
  (make-schedule :runs '(0)
                 :steps '(0)
                 :dissem-start 460
                 :dissem-end 490))

(defmethod current-cycle ((datasource (eql 'gfswave-combined)))
  "Cycle currently used by the router."
  ;; The next cycle becomes available (gradually) starting about 3:30h
  ;; after the forecast computation starts.
  (let ((24h (* 24 60 60)))
    (make-cycle :timestamp (universal-to-timestamp
                            (* 24h (floor (timestamp-to-universal
                                           (adjust-timestamp (now) (offset :hour -12))) 24h))))))

(defmethod latest-complete-cycle ((datasource (eql 'gfswave-combined)) &optional (time (now)))
  ;; Determine the latest cycle that should'be complete (theoretically) at the given time
  (let ((*default-timezone* local-time:+utc-zone+))
    (make-cycle% :timestamp (adjust-timestamp
                                (adjust-timestamp time (offset :minute (- 300)))
                              (set :hour 0) (set :minute 0) (set :sec 0)))))


(defmethod timestamp-cycle ((datasource (eql 'gfswave-combined)) timestamp)
  (let* ((current-cycle (current-cycle datasource)))
    current-cycle))

(defmethod cycle-forecast ((datasource gfswave-combined) timestamp)
  ;; Return the 3-hour-forecast required for $timestamp when using $cycle
  (let* ((cycle (cycle datasource))
         (basetime (cycle-timestamp cycle)) 
         (difference (truncate
                      (timestamp-difference timestamp basetime)
                      3600)))
    (cond
      ((minusp difference)
       (error "~a is in the past of cycle ~a" timestamp cycle))
      ((<= difference (* 28 6))
       (* 6 (truncate difference 6)))
      (t
       (log2:warning "~a is in the future of cycle ~a" timestamp cycle)
       (* 28 6)))))

(defmethod next-forecast ((datasource gfswave-combined) forecast)
  ;; Return the next 6-hour-forecast
  (min (+ forecast 6) (* 28 6)))

(defmethod local-pathname ((datasource gfswave-combined) step &key (relative nil))
  (let* ((srcname (name datasource))
         (cycle (cycle datasource))
         (date (cycle-datestring cycle))
         (file-name
           (format nil "~a.~a" srcname date))
         (file-dir
           (list :relative srcname date))
         (pathname
           (make-pathname :directory file-dir
                          :name file-name
                          :type "grib2")))
    (if relative pathname
        (merge-pathnames pathname
                         (pathname *grib-directory*)))))

(defmethod probe-uris ((datasource gfswave-combined) step)
  (check-uri-exists (data-uri datasource step)))

(defmethod data-uri ((datasource gfswave-combined) step)
  (let* ((cycle (cycle datasource))
         (run (format nil "~2,,,'0@a" (cycle-run cycle)))
         (date (cycle-datestring cycle)))
    ;; 20260328/00/20260328_00_0p50_006_to_240_gfswave-combined.grib2
    (format nil "~a~a/~a/~a_~a_0p50_006_to_240_gfswave-combined.grib2"
            (location datasource) date run date run)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
