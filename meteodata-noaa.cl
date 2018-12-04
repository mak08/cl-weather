;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2018-12-04 00:23:41>

(in-package :cl-weather)

(declaim (optimize speed (debug 1) (space 0) (safety 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieving NOAA wind forecasts
;;;    Model: NOAA GFS
;;;    Resolution: 1 degree
;;;    GRIB2 times are UTC
;;;    NOAA GFS forecasts are produced every 6hrs (four cycles per day)
;;;
;;; Wind data
;;;    Wind data is usually stored in two variables
;;;    - UGRND: Zonal wind (wind from west is positive)
;;;    - VGRND: Meridonal wind (wind from south is positive)
;;;
;;; Forecast availability
;;;    Cycle nn starts to become available at nn+3:30 UTC.
;;;    The full cycle data is available after 1.5hrs:
;;;
;;;    Cycle  First FC avail    Full FC avail
;;;    00     03:30Z            05:00Z
;;;    06     09:30Z            11:00Z
;;;    12     15:30Z            17:00Z
;;;    18     21:30Z            23:00Z

(defvar *noaa-forecast-bundle* nil)

;;; Offset (in minutes) of the forecast used at a given time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API Functions

(defclass noaa-bundle (forecast-bundle)
  ((noaa-data :reader noaa-data :initarg :data)
   (date :reader date :initarg :date)
   (cycle :reader cycle :initarg :cycle)
   (fc-hash :accessor %fc-hash :initform (make-hash-table))))

(defmethod print-object ((thing noaa-bundle) stream)
  (format stream "{NOAA Bundle Time ~a, Date ~a, Cycle ~a}"
          (grib-forecast-time (noaa-data thing))
          (date thing)
          (cycle thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD-INITIAL-DATA
;;;
;;;  The first forecast of a cycle becomes available about 3:30h after the start
;;;  of computation. After 5:00h all forecasts should be available. Sometimes the
;;;  computation does not start or finish on time.

(defmethod load-forecast-bundle ((bundle (eql 'noaa-bundle)))
  (multiple-value-bind (filenames date cycle cycle-start-time)
      (download-complete-bundle)
    (let* ((grib (read-noaa-wind-data filenames))
           (noaa-bundle (make-instance 'noaa-bundle
                                       :date date
                                       :cycle cycle
                                       :data grib)))
      (setf *noaa-forecast-bundle* noaa-bundle)
      (log2:info "Done.")
      (when (cycle-updating-p)
        (update-forecast-bundle bundle)))))

(defmethod get-forecast-bundle ((datasource (eql 'noaa-bundle)))
  (or *noaa-forecast-bundle*
      (setf  *noaa-forecast-bundle*
             (load-forecast-bundle datasource))))

(defmethod fcb-time ((bundle noaa-bundle))
  (let ((grib (noaa-data bundle)))
    (grib-forecast-time grib)))

(defmethod fcb-max-offset ((bundle noaa-bundle))
  (let*
      ((values (grib-data (noaa-data bundle)))
       (last-values (find-if-not #'null values :from-end t)))
    (/
     (grib-values-offset last-values)
     60)))

(defclass noaa-forecast (forecast)
  ((fc-grib :reader fc-grib :initarg :grib)
   (fc-time :reader fc-time :initarg :time)
   (fc-offset :reader fc-offset :initarg :offset :documentation "Offset from first forecast in minutes")
   (fc-hash :accessor fc-hash% :initform (make-hash-table))))

(defmethod print-object ((thing noaa-forecast) stream)
  (let ((cycle (grib-cycle (fc-grib thing))))
    (format stream "{NOAA Forecast @ ~a, Offset ~a, Cycle ~a::~a}"
            (fc-time thing)
            (fc-offset thing)
            (format-timestring nil cycle :format '(:year "-" (:month 2) "-" (:day 2)) :timezone +utc-zone+)
            (timestamp-hour cycle :timezone +utc-zone+))))

(defmethod get-forecast ((bundle noaa-bundle) (utc-time local-time:timestamp))
  ;; New procedure:
  ;; - Round time to minute
  ;; - Check if forecast exists at bundle and is still valid
  (let* ((fc-time (timestamp-minimize-part utc-time :sec))
         (fc-offset (truncate
                     (/ (timestamp-difference fc-time (fcb-time bundle))
                        60))))
    (or (gethash fc-offset (%fc-hash bundle))
        (setf (gethash fc-offset (%fc-hash bundle))
              (make-instance 'noaa-forecast
                             :grib (noaa-data bundle)
                             :time fc-time
                             :offset fc-offset)))))

(defmethod get-wind-forecast ((forecast noaa-forecast) latlng)
  ;; New interpolation procedure:
  ;; - Round position to Second / 10Seconds ?
  ;; - Perform bilinear interpolation of the required supporting points (cached) to the fc time
  ;; - Perform bilinear interpolation to the required latlng.
  (declare (inline angle bilinear enorm))
  (let* ((lat (latlng-lat latlng))
         (lng% (latlng-lng latlng))
         (lng   (if (< lng% 0d0)
                    (+ lng% 360d0)
                    lng%))
         (grib (fc-grib forecast))
         (offset (fc-offset forecast))
         (i-inc (grib-i-inc grib))
         (j-inc (grib-j-inc grib))
         (lat0 (* (ffloor lat j-inc) j-inc))
         (lng0 (* (ffloor lng i-inc) i-inc))
         (lat1 (+ lat0 j-inc))
         (lng1 (+ lng0 i-inc))
         (w00
          (time-interpolate grib offset lat0 lng0))
         (w01
          (time-interpolate grib offset lat0 lng1))
         (w10
          (time-interpolate grib offset lat1 lng0))
         (w11
          (time-interpolate grib offset lat1 lng1)))
    (let* ((u (bilinear lng lat lng0 lng1 lat0 lat1 (wind-u w00) (wind-u w01) (wind-u w10) (wind-u w11)))
           (v (bilinear lng lat lng0 lng1 lat0 lat1 (wind-v w00) (wind-v w01) (wind-v w10) (wind-v w11)))
           (s (bilinear lng lat lng0 lng1 lat0 lat1
                        (enorm (wind-u w00) (wind-v w00))
                        (enorm (wind-u w01) (wind-v w01))
                        (enorm (wind-u w10) (wind-v w10))
                        (enorm (wind-u w11) (wind-v w11)))))
      (values (angle u v)
              s))))

(defmethod update-forecast-bundle ((bundle (eql 'noaa-bundle)) &key)
  (log2:info "Updating forecast")
  (let ((noaa-bundle (get-forecast-bundle bundle)))
    (shift-forecast-bundle noaa-bundle)
    (update-noaa-bundle noaa-bundle)))

;;; API Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOWNLOAD-COMPLETE-BUNDLE
;;;    
;;;   Search backwards from $start to find a complete bundle (93 forecasts)

(defun download-complete-bundle (&optional (start (now)))
  ;; Retry to download starting with the latest available cycle
  ;; going backward in time if necessary.
  ;; Return list of filenames
  (let ((timepoint start))
    (tagbody
      :start
      (multiple-value-bind (date cycle cycle-start-time)
          (latest-complete-cycle timepoint)
        (log2:info "Trying download: ~a-~a" date cycle)
        (let* ((filenames (download-noaa-bundle date cycle))
               (numfiles (length filenames)))
          (cond
            ((< numfiles (length +noaa-forecast-offsets+))
             ;; Move one cycle back in time
             (log2:info "Found ~a files - backing up (incomplete)" numfiles)
             (setf timepoint
                   (adjust-timestamp timepoint (offset :minute (- 360))))
             (let ((delta (truncate (timestamp-difference start timepoint) 3600)))
               (log2:info "Looking back ~ah" delta)
               (if (< delta 48)
                   (go :start)
                   (error "Incomplete downloads"))))
            (t
             (return-from download-complete-bundle
               (values filenames
                       date
                       cycle
                       cycle-start-time)))))))))

(defun latest-complete-cycle (&optional (time (now)))
  ;; Determine the latest cycle that should'be complete (theoretically) at the given time
  (let* ((cycle-start-time 
          (timestamp-minimize-part (adjust-timestamp time (offset :minute (- 300)))
                                   :min
                                   :timezone +utc-zone+))
         (date (format-timestring nil cycle-start-time :format '((:year 4) (:month 2) (:day 2))))
         (cycle (* 6 (truncate (timestamp-hour cycle-start-time :timezone +utc-zone+) 6))))
    (values date
            cycle
            cycle-start-time)))

(defun cycle-updating-p (&optional (time (now)))
  (< 210 (mod (day-minute time) 360) 300))

(defun day-minute (&optional (time (now)))
  (+ (* (timestamp-hour time :timezone +utc-zone+) 60) (timestamp-minute time :timezone +utc-zone+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SHIFT-FORECAST-BUNDLE
;;;
;;;   Move bundle _Base Time_ forward 6hrs, adjusting forecast offsets.
;;:   Discard expired forecasts.
;;;   12h forecasts (f252...f384) are also shifted and their offset adjusted.
;;;   This is not really correct. 

(defmethod shift-forecast-bundle ((bundle noaa-bundle))
  (adjust-timestamp! (grib-forecast-time (noaa-data bundle)) (offset :minute 360))
  ;; cycle timestamp is shared!
  (adjust-timestamp! (grib-cycle (noaa-data bundle)) (:offset :hour 6))
  (let ((new-data (make-array 93 :initial-element nil))
        (old-data (grib-data (noaa-data bundle))))
    (loop
       :for k :below 91 ; 93-2
       :do (progn
             ;; Move forecasts 'left', discarding the oldest two forecasts
             (setf (aref new-data k) 
                   (aref old-data (+ k 2)))
             ;; Adjust cycle & offsets to new bundle base time.
             ;; Rightmost positions remain NULL. Don't attempt to adjust them
             ;; (if we shift again before the positions are re-filled).
             (when (aref new-data k)
               (setf (grib-values-outdated (aref new-data k)) t)
               (decf (grib-values-offset (aref new-data k))
                     360))))
    (setf (grib-data (noaa-data bundle))
          new-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update data

(defun update-noaa-bundle (bundle)
  (multiple-value-bind (date cycle)
      (current-cycle)
    (log2:info "Updating to cycle ~a-~a" date cycle)
    (loop
       :with start-time = (now)
       :for offset :across +noaa-forecast-offsets+
       :for index :from 0
       :do (multiple-value-bind (directory spec destfile)
               (noaa-spec-and-destfile date :cycle cycle :offset offset)
             (when (> (timestamp-difference (now) start-time) (* 60 60 3))
               (log2:error "Giving up at offset ~a" offset)
               (return-from update-noaa-bundle))
             (let ((destpath (format () "~a/~a" *grib-folder* destfile)))
               (cond
                 ((probe-file destpath)
                  (log2:info "File exists: ~a(~a:~a))" destpath date cycle))
                 (t
                  (tagbody
                    :retry
                    (cond
                      ((not (noaa-grib-exists-p date cycle spec))
                       (log2:info "Wait 1min for ~a/~a" date spec)
                       (sleep 60)
                       (go :retry))
                      (t
                       (log2:info "Downloading ~a/~a" date spec)
                       (multiple-value-bind
                             (out error-out status)
                           (download-noaa-file% directory spec destfile)))))))
               (merge-forecasts (noaa-data bundle)
                                index
                                (read-noaa-wind-data (list destpath)))))))
  (log2:info "Done."))


(defun merge-forecasts (target-bundle target-index source-bundle)
  ;; Expect same timestamp
  (assert (timestamp=
           (grib-cycle target-bundle)
           (grib-cycle source-bundle)))
  ;; Expect one forecast in source
  (assert (eql (length (grib-data source-bundle)) 1))
  ;; Determine offset of source forecast
  (let* ((source-fc (aref (grib-data source-bundle) 0))
         (source-offset (grib-values-offset source-fc)))
    (log2:trace "Source offset: ~a" source-offset)
    (case source-offset
      ((0 180) 
       ;; Don't replace in the past
       )
      (360
       (setf (aref (grib-data target-bundle) target-index)
             (interpolate-forecast (aref (grib-data target-bundle) target-index) source-fc 0.5)))
      (otherwise
       (setf (aref (grib-data target-bundle) target-index) source-fc)))))

;; Determine index of forecast in the bundle
(defun fc-index (grib-values)
  (position (/ (grib-values-offset grib-values) 60) +noaa-forecast-offsets+ :test #'eql))

(defun current-cycle ()
  ;; The next cycle becomes available about 3:30h after the forecast computation starts.
  (let* ((avail-time (adjust-timestamp (now) (offset :minute (- 210))))
         (date (format-timestring nil avail-time :format '((:year 4) (:month 2) (:day 2))))
         (cycle (* 6 (truncate (timestamp-hour avail-time :timezone +utc-zone+) 6))))
    (values date cycle)))

(defun interpolate-forecast (old new fraction)
  ;; Interpolate two value sets from different cycles
  (declare (inline enorm))
  (let* ((old-u (grib-values-u-array old))
         (old-v (grib-values-v-array old))
         (new-u (grib-values-u-array new))
         (new-v (grib-values-v-array new))
         (dimensions (array-dimensions old-u))
         (result-u (make-array dimensions :element-type 'double-float))
         (result-v (make-array dimensions :element-type 'double-float)))
    (log2:trace "dim:~a t=~a"
               dimensions
               (+ (grib-values-offset old)
                                        (* fraction (- (grib-values-offset new)
                                                       (grib-values-offset old)))))
    (loop
       :for u0 :across old-u
       :for u1 :across new-u
       :for v0 :across old-v
       :for v1 :across new-v
       :for i :from 0
       :for s0 = (enorm u0 v0)
       :for s1 = (enorm u1 v1)
       :for s = (+ s0 (* fraction (- s1 s0)))
       :for u = (+ u0 (* fraction (- u1 u0)))
       :for v = (+ v0 (* fraction (- v1 v0)))
       :for a = (atan u v)
       :do (multiple-value-bind (u v)
               (p2c a s)
             (setf (aref result-u i)
                   u
                   (aref result-v i)
                   v)))

    (make-grib-values :cycle (grib-values-cycle new)
                      :offset (grib-values-offset new)
                      :u-array result-u
                      :v-array result-v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Download forecasts from NOAA.
;;;    See http://nomads.ncep.noaa.gov/
;;;
;;; Example URL:
;;;    http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_1p00.pl?file=gfs.t12z.pgrb2.1p00.f000&lev_10_m_above_ground=on&var_UGRD=on&var_VGRD=on&leftlon=0&rightlon=360&toplat=90&bottomlat=-90&dir=%2Fgfs.2017091712

(defvar +noaa-forecast-offsets+
  #(0 3 6 9 12 15 18 21 24
    27 30 33 36 39 42 45 48 51
    54 57 60 63 66 69 72 75 78 81
    84 87 90 93 96 99 102 105 108 111
    114 117 120 123 126 129 132 135 138 141
    144 147 150 153 156 159 162 165 168 171
    174 177 180 183 186 189 192 195 198 201
    204 207 210 213 216 219 222 225 228 231
    234 237 240 252 264 276 288 300 312 324
    336 348 360 372 384))

(defun download-noaa-bundle (date cycle)
  ;; Date: yyyymmdd 
  ;; Cycle: 00|06|12|18
  ;; 3hourly forecasts will be downloaded (offsets 0..240)
  (ecase cycle ((or 0 6 12 18)))
  (loop
     :for offset :across +noaa-forecast-offsets+
     :for file = (ignore-errors
                   (download-noaa-file date cycle offset))
     :while file
     :collect file :into files
     :finally (return (values files
                              cycle))))

(defun noaa-spec-and-destfile (date &key (cycle "0") (offset 6) (basename "pgrb2") (resolution "1p00"))
  (let* ((directory
          (format () "~a~2,,,'0@a" date cycle))
         (spec
          (format () "gfs.t~2,,,'0@az.~a.~a.f~3,,,'0@a" cycle basename resolution offset))
         (destfile
          (format () "~a_~a.grib2" date spec)))
    (values directory spec destfile)))

(defun download-noaa-file (date cycle offset)
  (multiple-value-bind (directory spec destfile)
      (noaa-spec-and-destfile date :cycle cycle :offset offset)
    (let ((destpath (format () "~a/~a" *grib-folder* destfile)))
      (if (probe-file destpath)
          (log2:trace "File exists: ~a(~a:~a))" destpath date cycle)
          (progn
            (log2:trace "~a(~a:~a)" destpath date cycle)
            (multiple-value-bind
                  (out error-out status)
                (download-noaa-file% directory spec destfile)
              (case status
                (0
                 (let ((download-size
                        (with-open-file (f (format () "~a/~a" *grib-folder* destfile))
                          (file-length f))))
                   (when (< download-size 50000)
                     (uiop:delete-file-if-exists destpath)
                     (log2:warning  "Short file ~a. Deleting." destpath)
                     (error "Short file. Forecast ~a:~a not available yet?" date spec))))
                (otherwise
                 (error "cURL error ~a" status))))))
      destpath)))

(defun noaa-grib-exists-p (date cycle spec)
  "Retrieve the GRIB file valid at timestamp according to VR rules"
  (let* ((url
          (format nil "http://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.~a~2,,,'0@a/~a" date cycle spec))
         (command
          (format () "curl -sfI ~a" url)))
    (log2:info "~a" command)
    (handler-case 
        (null (uiop:run-program command))
      (uiop/run-program:subprocess-error ()
        nil))))

(defun download-noaa-file% (directory spec destfile &key (resolution "1p00"))
  "Retrieve the GRIB file valid at timestamp according to VR rules"
  (let* ((dest-folder
          *grib-folder*)
         (url
          (concatenate 'string
                       "http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_" resolution ".pl?"
                       "file=" spec
                       "&dir=%2Fgfs." directory
                       "&lev_10_m_above_ground=on"
                       "&var_UGRD=on"
                       "&var_VGRD=on"
                       "&leftlon=0"
                       "&rightlon=360"
                       "&toplat=90"
                       "&bottomlat=-90"))
         (ftp-command
          (format () "curl -n \"~a\" -o ~a/~a" url dest-folder destfile)))
    (log2:trace "~a" ftp-command)
    (uiop:run-program ftp-command)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Accessing GRIB wind data

(defun read-noaa-wind-data (filenames)
  "Read GRIB data into U and V arrays. Assumes the GRIB file contains U-GRD and V-GRD values"
  (when (null filenames)
    (error "No input files"))
  (let ((index (codes-index-new '("step" "shortName"))))
    (dolist (filename filenames)
      (log2:trace "Add file ~a~%" filename)
      (codes-index-add-file index filename))
    (get-values-from-index index)))


(defun time-interpolate (grib offset lat lon)
  (declare (inline enorm))
  (let ((index
         (position offset (grib-data grib)
                   :test #'<=
                   ;; Some grib-values may be NULL
                   :key #'(lambda (values)
                            (or (and values (grib-values-offset values))
                                -1))))
        (array-offset
         (array-offset grib lat lon)))
    (when (null index)
      (error "No data for offset ~a" offset))
    (multiple-value-bind (u1 v1 t1)
        (grib-get-uv grib index array-offset)
      (cond
        ((eql index 0)
         (make-wind :u u1 :v v1))
        (t
         (multiple-value-bind (u0 v0 t0)
             (grib-get-uv grib (1- index) array-offset)
           (let*
               ((fraction (/
                           (-  offset (grib-values-offset t0))
                           (- (grib-values-offset t1) (grib-values-offset t0))))
                (s0 (enorm u0 v0))
                (s1 (enorm u1 v1))
                (s (linear fraction s0 s1))
                (u (linear fraction u0 u1))
                (v (linear fraction v0 v1))
                (a (atan u v)))
             (multiple-value-bind (u v)
                 (p2c a s)
               (make-wind :u u :v v)))))))))

(defun grib-get-uv (grib time-index array-offset)
  (let*
      ((t1 (aref (grib-data grib) time-index))
       (u1 (aref (grib-values-u-array t1) array-offset))
       (v1 (aref (grib-values-v-array t1) array-offset)))
    (values u1 v1 t1)))

(defun array-offset (grib lat lon)
  (let*
      ((j-scan-pos-p (eql (grib-j-scan-pos grib) 1))
       (i-inc (grib-i-inc grib))
       (j-inc (grib-j-inc grib))
       (lat0 (grib-lat-start grib))
       (lon0 (grib-lon-start grib))
       (olat (if j-scan-pos-p (- lat lat0) (- lat0 lat)))
       (olon (- lon lon0))
       (lat-index (floor olat j-inc))
       (lon-index (floor olon i-inc))
       (lonpoints (grib-lon-points grib))
       (lat-offset (* lat-index lonpoints))
       (array-offset (+ lat-offset lon-index)))
    array-offset))

(defun linear (fraction a b)
  (+ a (* fraction (- b a))))

(defun p2c (a r)
  (let ((c (cis a)))
    (values 
     (* r (imagpart c))
     (* r (realpart c)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

