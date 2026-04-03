;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   GRIB data sources
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2026-03-31 21:54:42>

(in-package "CL-WEATHER")

;;; FW  Current
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/global_agulhas_20260124.grb2
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/global_east_australia_20260124.grb2
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/ibi_english_channel_20260124.grb2
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/sfbofs_sf_bay_20260124_15z.grb2
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/rtofs_west_atl_20260124_6hourly.grb2

 
(defclass fw-current (fw-datasource datakind-current)
  ((name :initform "fw-current")
   (location :initform "https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/")
   (maxstep :initform 48)))
(defclass fw-current-agulhas (fw-current)
  ((name :initform "fw-current-agulhas")
   (schedule :initform (datasource-schedule 'fw-current-agulhas))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FW

(defmethod datasource-schedule ((datasource (eql 'fw-current-agulhas)))
  (make-schedule :runs '(0)
                 :steps '(0)
                 :dissem-start 460
                 :dissem-end 490))

(defmethod current-cycle ((datasource (eql 'fw-current-agulhas)))
  "Cycle currently used by the router."
  ;; The next cycle becomes available (gradually) starting about 3:30h
  ;; after the forecast computation starts.
  (let ((24h (* 24 60 60)))
    (make-cycle :timestamp (universal-to-timestamp
                            (* 24h (floor (timestamp-to-universal
                                           (adjust-timestamp (now) (offset :hour -12))) 24h))))))

(defmethod latest-complete-cycle ((datasource (eql 'fw-current-agulhas)) &optional (time (now)))
  ;; Determine the latest cycle that should'be complete (theoretically) at the given time
  (let ((*default-timezone* local-time:+utc-zone+))
    (make-cycle% :timestamp (adjust-timestamp
                                (adjust-timestamp time (offset :minute (- 300)))
                              (set :hour 0) (set :minute 0) (set :sec 0)))))


(defmethod timestamp-cycle ((datasource (eql 'fw-current-agulhas)) timestamp)
  (let* ((current-cycle (current-cycle datasource)))
    current-cycle))

(defmethod cycle-forecast ((datasource fw-current) timestamp)
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

(defmethod next-forecast ((datasource fw-current) forecast)
  ;; Return the next 6-hour-forecast
  (min (+ forecast 6) (* 28 6)))

(defmethod local-pathname ((datasource fw-current) step &key (relative nil))
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

(defmethod probe-uris ((datasource fw-current) step)
  (check-uri-exists (data-uri datasource step)))

(defmethod data-uri ((datasource fw-current-agulhas) step)
  (let* ((cycle (cycle datasource))
         (run (format nil "~2,,,'0@a" (cycle-run cycle)))
         (date (cycle-datestring cycle)))
    (format nil "~a~a/global_agulhas_~a.grb2"
            (location datasource) date date)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
