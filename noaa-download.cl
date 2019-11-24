;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2019-11-24 20:57:06>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Download forecasts from NOAA.
;;;    See http://nomads.ncep.noaa.gov/
;;;
;;; Example URL:
;;;    http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_1p00.pl?file=gfs.t12z.pgrb2.1p00.f000&lev_10_m_above_ground=on&var_UGRD=on&var_VGRD=on&leftlon=0&rightlon=360&toplat=90&bottomlat=-90&dir=%2Fgfs.2017091712

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieving NOAA wind forecasts
;;;    Model: NOAA GFS
;;;    Resolution: 1 degree
;;;    GRIB2 times are UTC
;;;    NOAA GFS forecasts are produced every 6hrs (four cycles per day)
;;;
;;; Wind data
;;;    Wind data is usually stored in two variables
;;;    - U10: Zonal wind (wind from west is positive)
;;;    - V10: Meridonal wind (wind from south is positive)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOWNLOAD-CYCLE
;;;    
;;;   Search backwards from $start to find a complete cycle

(defun download-cycle-backtrack (&optional (start (now)))
  ;; Retry to download starting with the latest available cycle
  ;; going backward in time if necessary.
  ;; Return list of filenames
  (let ((timepoint start))
    (tagbody
      :start
      (multiple-value-bind (date cycle cycle-start-time)
          (latest-complete-cycle timepoint)
        (log2:info "Trying download: ~a-~a" date cycle)
        (let* ((count
                (download-cycle date cycle :if-missing :abort)))
          (cond
            ((< count (length +noaa-forecast-offsets+))
             ;; Move one cycle back in time
             (log2:info "Found ~a files (incomplete) - backing up" count)
             (setf timepoint
                   (adjust-timestamp timepoint (offset :minute (- 360))))
             (let ((delta (truncate (timestamp-difference start timepoint) 3600)))
               (log2:info "Looking back ~ah" delta)
               (if (< delta 48)
                   (go :start)
                   (error "Incomplete downloads"))))
            (t
             (return-from download-cycle-backtrack
               (values date
                       cycle
                       cycle-start-time)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOWNLOAD-CURRENT-CYCLE
;;;
;;;   Download current cycle

(defun download-latest-cycle ()
  (multiple-value-bind (date cycle)
      (latest-complete-cycle)
    (download-cycle date cycle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOWNLOAD-CYCLE
;;;
;;;   Download the specified cycle. If a forecast is (still) missing, wait or abort.

(defun download-cycle (date cycle &key (if-missing :wait))
  (ecase cycle ((or 0 6 12 18)))
  (ecase if-missing ((or :wait :abort)))
  (log2:info "Downloading cycle ~a-~a" date cycle)
  (loop
     :with start-time = (now)
     :for offset :across +noaa-forecast-offsets+
     :for count :from 1
     :do (let* ((destpath
                 (noaa-destpath date :cycle cycle :offset offset)))
           (when (> (timestamp-difference (now) start-time) (* 60 60 3))
             (log2:error "Giving up download of ~a/~a at offset ~a" date cycle offset)
             (error "Incomplete cycle ~a/~a" date cycle))
           (cond
             ((probe-file destpath)
              (log2:info "File exists: ~a)" destpath))
             (t
              (tagbody
                :retry
                (cond
                  ((not (noaa-file-exists-p date cycle offset))
                   (ecase if-missing
                     (:wait
                      (log2:info "Wait 1m for ~a/~a/~a" date cycle offset)
                      (sleep 60)
                      (go :retry))
                     (:abort
                      (return-from download-cycle (values count cycle)))))
                  (t
                   (log2:info "Downloading ~a/~a/~a" date cycle offset)
                   (let ((spec
                          (noaa-spec date :cycle cycle :offset offset)))
                     (multiple-value-bind
                           (out error-out status)
                         (download-noaa-file% date cycle spec destpath)))))))))
       :finally (return (values count cycle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check if forecast exists on server

(defun noaa-file-exists-p (date cycle offset)
  "Check if the specified forecast exists. $date='YYYYMMDD', $cycle='CC', $offset may be numeric or string."
  (let* ((spec
          (noaa-spec date :cycle cycle :offset offset))
         (url
          (format nil "https://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.~a/~2,,,'0@a/~a" date cycle spec))
         (command
          (format () "curl -sfI ~a" url)))
    (log2:trace "~a" command)
    (handler-case 
        (null (uiop:run-program command))
      (uiop/run-program:subprocess-error ()
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Download forecast

(defun download-noaa-file (date cycle offset)
  (let* ((spec (noaa-spec date :cycle cycle :offset offset))
         (destpath (noaa-destpath date :cycle cycle :offset offset)))
    (cond
      ((and (probe-file destpath)
            (noaa-file-complete-p destpath))
       (log2:trace "File exists: ~a(~a:~a))" destpath date cycle))
      (t
       (when (probe-file destpath)
         (log2:info "File truncated: ~a" destpath))
       (log2:trace "~a(~a:~a)" destpath date cycle)
       (multiple-value-bind
             (out error-out status)
           (download-noaa-file% date cycle spec destpath)
         (case status
           (0
            (let ((download-size
                   (with-open-file (f destpath)
                     (file-length f))))
              (when (< download-size 50000)
                (log2:warning  "Deleting ~a (short file). Forecast not available yet?" destpath)
                (uiop:delete-file-if-exists destpath)
                (error "Forecast ~a:~a short file, not available yet?" date spec))))
           (otherwise
            (error "cURL error ~a" status))))))
    destpath))

(defun noaa-file-complete-p (destpath)
  (with-open-file (f destpath)
    (let* ((length
            (file-length f))
           (closing-bytes
            (make-array 4)))
      (file-position f (- length 4))
      (read-sequence closing-bytes f)
      (every
       (lambda (c) (eql c #\7))
       closing-bytes))))

(defun download-noaa-file% (date cycle spec destpath &key (resolution "1p00"))
  "Retrieve the GRIB file valid at timestamp according to VR rules"
  (let* ((dest-folder
          *grib-directory*)
         (query
          (format nil "~a&~a&~a&~a&~a&~a&~a&~a&~a"
                  (format nil "file=~a" spec)
                  (format nil "dir=%2Fgfs.~a%2F~2,,,'0@a" date cycle)
                  "lev_10_m_above_ground=on"
                  "var_UGRD=on"
                  "var_VGRD=on"
                  "leftlon=0"
                  "rightlon=360"
                  "toplat=80"
                  "bottomlat=-80"))
         (url
          (format nil
                  "https://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_~a.pl?~a" resolution query))
         (ftp-command
          (format () "curl -n \"~a\" -o ~a" url destpath)))
    (log2:trace "~a" ftp-command)
    (uiop:run-program ftp-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grib filter input file name 

(defun noaa-spec (date &key (cycle 0) (offset 6) (basename "pgrb2") (resolution "1p00"))
  (format () "gfs.t~2,,,'0@az.~a.~a.f~3,,,'0@a" cycle basename resolution offset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local forecast file name

(defun noaa-destpath (date &key (cycle 0) (offset 6) (basename "pgrb2") (resolution "1p00"))
  (let* ((spec
          (noaa-spec date :cycle cycle :offset offset))
         (destfile 
          (format () "~a_~a.grib2" date spec)))
    (merge-pathnames destfile *grib-directory*)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
