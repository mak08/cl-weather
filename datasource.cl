;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   GRIB data sources
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2026-02-15 02:06:57>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example GRIB sources

;;; NOAA RTOFS currents - West Atlantic - steps 25-48 
;;; Index <no index>
;;; Data  https://nomads.ncep.noaa.gov/pub/data/nccf/com/rtofs/prod/rtofs.20260105/rtofs_glo.t00z.f048_west_atl_std.grb2

;;; NOAA GFS wind
;;; Index https://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.20260105/06/atmos/gfs.t06z.pgrb2.0p25.f021.idx
;;; Data  https://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.20260105/06/atmos/gfs.t06z.pgrb2.0p25.f021

;;; NOAA AIGFS wind
;;; Index https://nomads.ncep.noaa.gov/pub/data/nccf/com/aigfs/prod/aigfs.20260105/06/model/atmos/grib2/aigfs.t06z.pres.f024.grib2.idx
;;; Data  https://nomads.ncep.noaa.gov/pub/data/nccf/com/aigfs/prod/aigfs.20260105/06/model/atmos/grib2/aigfs.t06z.pres.f024.grib2

;;; ECMWF IFS wind
;;; Index https://data.ecmwf.int/forecasts/20260104/00z/ifs/0p25/oper/20260104000000-21h-oper-fc.index
;;; Data  https://data.ecmwf.int/forecasts/20260104/00z/ifs/0p25/oper/20260104000000-21h-oper-fc.grib2

;;; ECMWF AIFS wind
;;; Index https://data.ecmwf.int/forecasts/20260105/06z/aifs-single/0p25/oper/20260105060000-18h-oper-fc.index
;;; Data  https://data.ecmwf.int/forecasts/20260105/06z/aifs-single/0p25/oper/20260105060000-18h-oper-fc.grib2

;;; FW  Current
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/global_agulhas_20260124.grb2
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/global_east_australia_20260124.grb2
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/ibi_english_channel_20260124.grb2
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/sfbofs_sf_bay_20260124_15z.grb2
;;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/rtofs_west_atl_20260124_6hourly.grb2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types of datasources. Not all are already supported.


(defparameter *datasource-ht* (make-hash-table :test 'equal))

(defun get-datasource (id cycle)
  (or (gethash (list id (cycle-run cycle) (cycle-datestring cycle))
               *datasource-ht*)
      (setf (gethash (list id (cycle-run cycle) (cycle-datestring cycle))
                     *datasource-ht*)
            (make-instance id :cycle cycle))))

(defclass datakind ()
  ((datakind :reader datakind :allocation :class)
   (uv-variables :allocation :class :reader uv-variables)))

(defclass datakind-wind (datakind)
  ((datakind :initform "wind")
   (uv-variables :initform '("10u" "10v"))))

(defclass datakind-current (datakind)
  ((datakind :initform "current")
   (uv-variables :initform '("ubaro" "vbaro"))))

(defclass datasource ()
  ((name :allocation :class :reader name :initform "gribs"
         :documentation "NAME is used to construct local file path. It should be unique.")
   (location :allocation :class :reader location)
   (schedule :allocation :class :reader schedule)
   (resolution :reader resolution :initarg :resolution :initform "0p25")
   (cycle :reader cycle :initarg :cycle :initform (make-cycle)
          :documentation "The data cycle (an instance of CYCLE)")
   (maxstep :reader maxstep :initform 0)))

(defmethod initialize-instance :after ((instance datasource) &rest initargs &key (cycle))
  (assert (member (cycle-run cycle) (schedule-runs (schedule instance)))))

(defclass noaa-datasource (datasource) ())
(defclass noaa-gfs-wind (noaa-datasource datakind-wind)
  ((name :initform "gfs")
   (location :initform "https://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/")
   (schedule :initform (datasource-schedule 'noaa-gfs-wind))
   (maxstep :initform 384)))

(defclass noaa-aigfs-wind (noaa-datasource datakind-wind)
  ((name :initform "aigfs")
   (location :initform "https://nomads.ncep.noaa.gov/pub/data/nccf/com/aigfs/prod/")))
(defclass noaa-rtofs-current (noaa-datasource datakind-current)
  ((name :initform "rtofs")
   (location :initform "https://nomads.ncep.noaa.gov/pub/data/nccf/com/rtofs/prod/")
   (schedule :initform (datasource-schedule 'noaa-rtofs-current))))

(defclass fw-current (datasource datakind-current)
  ((name :initform "fw-current")
   (location :initform "https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/")
   (maxstep :initform 48)))
(defclass fw-current-agulhas (fw-current)
  ((name :initform "fw-current-agulhas")
   (schedule :initform (datasource-schedule 'fw-current-agulhas))))

(defclass ecmwf-datasource (datasource) ())
(defclass ecmwf-ifs-wind (ecmwf-datasource datakind-wind)
  ((name :initform "ifs")
   (location :initform "https://data.ecmwf.int/forecasts/")
   (schedule :initform (datasource-schedule 'ecmwf-ifs-wind))))

(defclass ecmwf-ifs-wind-aws (ecmwf-ifs-wind)
  ((location :initform "https://ecmwf-forecasts.s3.eu-central-1.amazonaws.com/")))

(defclass ecmwf-aifs-wind (ecmwf-datasource datakind-wind)
  ((name :initform "aifs-single")
   (location :initform "https://data.ecmwf.int/forecasts/")
   (schedule :initform (datasource-schedule 'ecmwf-aifs-wind))))

(defgeneric datasource-schedule (class)
  (:documentation
   "Availability of runs and steps of a datasource.
The cycle-run must be a valid run for the datasource when making a datasource instance."))

(defgeneric latest-complete-cycle (datasource &optional time)
  (:documentation
   "Determine the latest cycle that should'be complete (theoretically) at the given time"))

(defgeneric current-cycle (datasource)
  (:documentation
   "Cycle currently used by the router."))

(defgeneric cycle-forecast (datasource timestamp)
  (:documentation
   "The forecast step in the cycle covering timestamp"))

(defgeneric next-forecast (datasource forecast)
  (:documentation
   "The next step/offset"))

(defgeneric previous-cycle (datasource cycle))

(defgeneric cycle-updating-p (datasource &optional time))

(defgeneric timestamp-cycle (datasource timestamp)
  (:documentation
   "EQL methods on datasource name to determine which cycle to use at timestamp.
Datasource constructors require the cycle as an argument. The datasource can only be instantiated
when the cycle was determined."))

(defgeneric download-datasource (datasource &key)
  (:documentation
   "Download complete cycle as forecasts become available"))

(defgeneric download-step (datasource step &key)
  (:documentation
   "Download relevant GRIB messages to local file. See LOCAL-PATHNAME"))

(defgeneric download-step-1 (datasource step local-path)
  (:documentation
   "Download relevant GRIB messages to local file. See LOCAL-PATHNAME"))

(defgeneric get-grib-file-ranges (datasource step)
  (:documentation "Internal use"))

(defgeneric retrieve-index-file (datasource step)
  (:documentation "Internal use"))

(defgeneric probe-uris (datasource step)
  (:documentation "Check if required source GRIB and Index files exist"))

(defgeneric index-uri (datasource step)
  (:documentation "Internal use"))

(defgeneric data-uri (datasource step)
  (:documentation "Internal use"))

(defgeneric local-pathname (datasource step &key)
  (:documentation
   "Returns a PATHNAME"))

(defgeneric file-step (datasource step)
  (:documentation
   "Translate step/offset (hours) to GRIB file step containing the step"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Datatypes

(defstruct datasource-info datakind name gribpaths)
(defstruct grib-spec resolution date run step path)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schedule - availability of runs and steps

(defstruct schedule runs steps dissem-start dissem-end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Range

(defstruct (range (:constructor create-range (start end)))
  start end)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

(defun datasource-info ()
  "Get pathnames for the current cycle"
  ;; Used by getServerSettings - provides download paths for HTTP client
  (loop
    :for name :in *datasources*
    :for cycle = (current-cycle name)
    :for run = (cycle-run cycle)
    :for date = (cycle-datestring cycle)
    :for schedule = (datasource-schedule name)
    :for datasource = (get-datasource name cycle)
    :when (member (cycle-run cycle)
                  (schedule-runs schedule))
      :collect (make-datasource-info
                :datakind (datakind datasource)
                :name name
                :gribpaths (loop
                             :with schedule = (schedule datasource)
                             :for step :in (schedule-steps schedule)
                             :collect (make-grib-spec
                                       :resolution (resolution datasource)
                                       :date date
                                       :run run
                                       :step step
                                       :path (local-pathname datasource step :relative t))))))

(defun day-minute (&optional (time (now)))
  (+ (*
      (timestamp-hour time :timezone +utc-zone+)
      60)
     (timestamp-minute time :timezone +utc-zone+)))

(defmethod file-step ((datasource datasource) step)
  step)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Default methods

(defmethod previous-cycle ((datasource t) cycle)
  (move-cycle :hours -6))

(defun move-cycle (cycle &key hours)
  (let ((timestamp (cycle-timestamp cycle)))
    (make-cycle :timestamp (adjust-timestamp timestamp (offset :hour hours)))))
  
(defmethod cycle-updating-p ((datasource t) &optional (time (now)))
  (< 210 (mod (day-minute time) 360) 300))

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
  ;; https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20260124/global_agulhas_20260124.grb2
  (let* ((cycle (cycle datasource))
         (run (format nil "~2,,,'0@a" (cycle-run cycle)))
         (date (cycle-datestring cycle)))
    (format nil "~a~a/global_agulhas_~a.grb2"
            (location datasource) date date)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ECMWF

(defmethod datasource-schedule ((datasource (eql 'ecmwf-ifs-wind)))
  (make-schedule :runs '(0 12)
                 :steps (append
                         (loop :for s :below 144 :by 3 :collect s)
                         (loop :for s :from 144 :to 360 :by 6 :collect s))
                 :dissem-start 515
                 :dissem-end 550))

(defmethod datasource-schedule ((datasource (eql 'ecmwf-aifs-wind)))
  (make-schedule :runs '(0 6 12 18)
                 :steps (loop :for s :from 0 :to 360 :by 6 :collect s)
                 :dissem-start 350
                 :dissem-end 550))

(defmethod current-cycle ((datasource (eql 'ecmwf-ifs-wind)))
  "Cycle currently used by the router."
  ;; The next cycle becomes available (gradually) starting about 3:30h
  ;; after the forecast computation starts.
  (make-cycle :interval (* 12 60 60) :delay (* 210 60)))

(defmethod current-cycle ((datasource (eql 'ecmwf-aifs-wind)))
  "Cycle currently used by the router."
  ;; The next cycle becomes available (gradually) starting about 3:30h
  ;; after the forecast computation starts.
  (make-cycle :interval (* 6 60 60) :delay (* 210 60)))

(defmethod latest-complete-cycle ((datasource (eql 'ecmwf-ifs-wind)) &optional (time (now)))
  ;; Determine the latest cycle that should'be complete (theoretically) at the given time
  (make-cycle :interval (* 12 60 60) :delay (* 300 60)))

(defmethod latest-complete-cycle ((datasource (eql 'ecmwf-aifs-wind)) &optional (time (now)))
  ;; Determine the latest cycle that should'be complete (theoretically) at the given time
  (make-cycle :interval (* 6 60 60) :delay (* 300 60)))

(defmethod get-grib-file-ranges ((datasource ecmwf-datasource) step)
  (loop
    :with content = (retrieve-index-file datasource step)
    :with length = (length content)
    :with pos = 0
    :with range
    :while (< pos length)
    :do (multiple-value-setq (range pos)
          (parse-json content pos))
    :when (and (null (joref range "number"))
               (some (lambda (shortname)
                       (string= (joref range "param") shortname))
                     (uv-variables datasource)))
    :collect (create-range (joref range "_offset")
                           (1- (+  (joref range "_offset")
                                   (joref range "_length"))))))

(defmethod index-uri ((datasource ecmwf-datasource) step)
  (ecmwf-ifs-wind-uri datasource step "index"))

(defmethod data-uri ((datasource ecmwf-datasource) step)
  (ecmwf-ifs-wind-uri datasource step "grib2"))

(defun ecmwf-ifs-wind-uri (datasource step type)
  (let* ((cycle (cycle datasource))
         (run (format nil "~2,,,'0@a" (cycle-run cycle)))
         (date (cycle-datestring cycle)))
    (format nil "~a~a/~az/~a/~a/oper/~a~a0000-~ah-oper-fc.~a"
            (location datasource) date run (name datasource) (resolution datasource) date run step type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOAA GFS

(defmethod datasource-schedule ((datasource (eql 'noaa-gfs-wind)))
  (make-schedule :runs '(0 6 12 18)
                 :steps (loop :for s :from 0 :to 384 :by 3 :collect s)
                 :dissem-start 210
                 :dissem-end 350))

(defmethod current-cycle ((datasource (eql 'noaa-gfs-wind)))
  "Cycle currently used by the router."
  ;; The next cycle becomes available (gradually) starting about 3:30h
  ;; after the forecast computation starts.
  (make-cycle :timestamp (adjust-timestamp (now) (offset :minute (- 210)))))

(defmethod previous-cycle ((datasource (eql 'noaa-gfs-wind)) cycle)
  (let ((timestamp (cycle-timestamp cycle)))
    (make-cycle :timestamp (adjust-timestamp timestamp (offset :hour -6)))))
  
(defmethod cycle-updating-p ((datasource (eql 'noaa-gfs-wind)) &optional (time (now)))
  (< 210 (mod (day-minute time) 360) 300))

(defmethod latest-complete-cycle ((datasource (eql 'noaa-gfs-wind)) &optional (time (now)))
  ;; Determine the latest cycle that should'be complete (theoretically) at the given time
  (make-cycle :timestamp (adjust-timestamp time (offset :minute (- 300)))))

(defmethod timestamp-cycle ((datasource (eql 'noaa-gfs-wind)) timestamp)
  (let* ((current-cycle (current-cycle datasource))
         (cycle-time (cycle-timestamp current-cycle))
         (elapsed-minutes (truncate (timestamp-difference timestamp cycle-time) 60))
         (cycle-step (truncate elapsed-minutes 180))
         (now (now))
         (running-minutes (truncate (timestamp-difference now cycle-time) 60))
         (avail-time (+ 220 (/ cycle-step 1.5))))
    (if (and (<= 360 elapsed-minutes)
             (>= running-minutes avail-time))
        current-cycle
        (previous-cycle datasource current-cycle))))

(defmethod get-grib-file-ranges ((datasource noaa-gfs-wind) step)
  ;; The URGD and VRGD messages appear consecutively in the GFS GRIB.
  ;; End offset is start offset of the next entry minus one.
  (let*
      ((content (retrieve-index-file datasource step))
       (u10 (search ":UGRD:10 m above ground:" content))
       (u10-start (1+ (position #\Newline content :from-end t :end u10)))
       (u10-end (1+ (position #\Newline content :from-end nil :start u10)))
       (v10-end (1+ (position #\Newline content :from-end nil :start u10-end)))
       (next-end (1+ (position #\Newline content :from-end nil :start v10-end)))
       (u10-entry (subseq content u10-start (1- u10-end)))
       (next-entry (subseq content v10-end (1- next-end)))
       (u10-range-start
         (second
          (cl-utilities:split-sequence #\: u10-entry)))
       (range-end
         (second
          (cl-utilities:split-sequence #\: next-entry))))
    (let ((start (parse-integer u10-range-start))
          (end (1- (parse-integer range-end))))
      (list (create-range start end)))))

(defmethod cycle-forecast ((datasource noaa-gfs-wind) timestamp)
  ;; Return the 3-hour-forecast required for $timestamp when using $cycle
  (let* ((cycle (cycle datasource))
         (basetime (cycle-timestamp cycle)) 
         (difference (truncate
                      (timestamp-difference timestamp basetime)
                      3600)))
    (cond
      ((minusp difference)
       (error "~a is in the past of cycle ~a" timestamp cycle))
      ((<= difference 384)
       (* 3 (truncate difference 3)))
      (t
       (log2:warning "~a is in the future of cycle ~a" timestamp cycle)
       384))))

(defmethod next-forecast ((datasource noaa-gfs-wind) forecast)
  ;; Return the next 3-hour-forecast
  (min (+ forecast 3) 384))

(defmethod index-uri ((datasource noaa-gfs-wind) step)
  (noaa-gfs-wind-uri datasource step ".idx"))

(defmethod data-uri ((datasource noaa-gfs-wind) step)
  (noaa-gfs-wind-uri datasource step ""))

(defun noaa-gfs-wind-uri (datasource step type)
  (let* ((cycle (cycle datasource))
         (run (format nil "~2,,,'0@a" (cycle-run cycle)))
         (date (cycle-datestring cycle))
         (step (format nil "~3,,,'0@a" step)))
    (format nil "~agfs.~a/~a/atmos/gfs.t~az.pgrb2.~a.f~a~a"
            (location datasource) date run run (resolution datasource) step type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOAA RTOFS

(defmethod datasource-schedule ((datasource (eql 'noaa-rtofs-current)))
  (make-schedule :runs '(0)
                 :steps '(24 48 72 144)
                 :dissem-start 210
                 :dissem-end 350))

(defmethod latest-complete-cycle ((datasource (eql 'noaa-rtofs-current)) &optional (time (now)))
  ;; Determine the latest cycle that should'be complete (theoretically) at the given time
  (let ((*default-timezone* local-time:+utc-zone+))
    (make-cycle% :timestamp (adjust-timestamp
                                (adjust-timestamp time (offset :minute (- 300)))
                              (set :hour 0) (set :minute 0) (set :sec 0)))))
  
(defmethod file-step ((datasource noaa-rtofs-current) step)
  (let ((file-steps (schedule-steps (schedule datasource))))
    (loop
      :for file-step :in file-steps
      :when (>= file-step step)
        :do (return file-step))))

(defmethod get-grib-file-ranges ((datasource noaa-rtofs-current) step)
  (error "No ranges available for NOAA RTOFS"))

(defmethod index-uri ((datasource noaa-rtofs-current) step)
  nil)

(defmethod probe-uris ((datasource noaa-rtofs-current) step)
  (check-uri-exists (data-uri datasource step)))

(defmethod data-uri ((datasource noaa-rtofs-current) step)
  (noaa-rtofs-current-uri datasource step ""))

(defun noaa-rtofs-current-uri (datasource step type)
  (let* ((cycle (cycle datasource))
         (run (format nil "~2,,,'0@a" (cycle-run cycle)))
         (date (cycle-datestring cycle))
         (step (format nil "~3,,,'0@a" step)))
    (format nil "~artofs.~a/rtofs_glo.t~az.f~a_west_atl_std.grb2"
            (location datasource) date run step)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
