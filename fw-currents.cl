;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   GRIB data sources
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2026-05-17 18:32:48>

(in-package "CL-WEATHER")

;;; FW  Current
;;; Agulhas now uses global_agulhas_ggr_YYYYMMDD.grb2 (verified on /wind, 2026-11-15).
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/ibi_english_channel_20260124.grb2
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/sfbofs_sf_bay_20260124_15z.grb2
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/rtofs_west_atl_20260124_6hourly.grb2

 
(defclass fw-current (fw-datasource datakind-current)
  ((name :initform "fw-current")
   (location :initform "https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/")
   (maxstep :initform 48)))
(defclass fw-current-agulhas (fw-current)
  ((name :initform "global_agulhas")
   (maxstep :initform 120)
   (schedule :initform (datasource-schedule 'fw-current))))
(defclass fw-current-english-channel (fw-current)
  ((name :initform "ibi_english_channel")
   (schedule :initform (datasource-schedule 'fw-current))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FW

(defmethod datasource-schedule ((datasource (eql 'fw-current)))
  (make-schedule :runs '(0) :steps '(0) :dissem-start 460 :dissem-end 490))
(defmethod datasource-schedule ((datasource (eql 'fw-current-agulhas)))
  (make-schedule :runs '(0) :steps '(0) :dissem-start 460 :dissem-end 490))
(defmethod datasource-schedule ((datasource (eql 'fw-current-english-channel)))
  (make-schedule :runs '(0) :steps '(0) :dissem-start 460 :dissem-end 490))

(defmethod current-cycle ((datasource (eql 'fw-current-agulhas)))
  (current-cycle-fw))
(defmethod current-cycle ((datasource (eql 'fw-current-english-channel)))
  (current-cycle-fw))

(defmethod timestamp-cycle ((datasource (eql 'fw-current-agulhas)) timestamp)
  (timestamp-cycle-fw datasource timestamp))
(defmethod timestamp-cycle ((datasource (eql 'fw-current-english-channel)) timestamp)
  (timestamp-cycle-fw datasource timestamp))

(defmethod cycle-forecast ((datasource fw-current) timestamp)
  ;; Select a six-hour frame and hold the final field at the source's own horizon.
  (let* ((cycle (cycle datasource))
         (basetime (cycle-timestamp cycle)) 
         (difference (truncate
                      (timestamp-difference timestamp basetime)
                      3600)))
    (cond
      ((minusp difference)
       (error "~a is in the past of cycle ~a" timestamp cycle))
      (t
       (min (maxstep datasource) (* 6 (truncate difference 6)))))))

(defmethod next-forecast ((datasource fw-current) forecast)
  ;; Do not ask the loader for frames beyond this source's forecast horizon.
  (min (+ forecast 6) (maxstep datasource)))

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

(defmethod data-uri ((datasource fw-current) step)
  (let* ((name (name datasource))
         (cycle (cycle datasource))
         (run (format nil "~2,,,'0@a" (cycle-run cycle)))
         (date (cycle-datestring cycle)))
    (format nil "~a~a/~a_~a.grb2"
            (location datasource) date name date)))

(defmethod data-uri ((datasource fw-current-agulhas) step)
  "Use the GGR filename published by Fairwinds, retaining the existing local cache name."
  (declare (ignore step))
  (let ((date (cycle-datestring (cycle datasource))))
    (format nil "~a~a/global_agulhas_ggr_~a.grb2" (location datasource) date date)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aux methods

(defun current-cycle-fw ()
  ;; The next cycle becomes available (gradually) starting about 3:30h
  ;; after the forecast computation starts.
  (make-cycle :timestamp (universal-to-timestamp
                          (* 24h (floor (timestamp-to-universal
                                         (adjust-timestamp (now) (offset :hour -12))) 24h)))))

(defun timestamp-cycle-fw (datasource timestamp)
  (let* ((current-cycle (current-cycle datasource)))
    current-cycle))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
