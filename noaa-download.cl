;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2022-01-05 17:01:34>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Download forecasts from NOAA.
;;;    See http://nomads.ncep.noaa.gov/
;;;
;;; Example URL:
;;;    http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_1p00.pl?file=gfs.t12z.pgrb2.1p00.f000&lev_10_m_above_ground=on&var_UGRD=on&var_VGRD=on&leftlon=0&rightlon=360&toplat=90&bottomlat=-90&dir=%2Fgfs.2017091712

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GEFS containing UGRD 10m:
;;; https://nomads.ncep.noaa.gov/data/nccf/com/gens/prod/gefs.20200924/12/atmos/pgrb2ap5/gep01.t12z.pgrb2a.0p50.f027.idx

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

(defvar *use-range-query* t)
;; The FTP folder only works with range query, not with a filter URL

(defparameter +ncep-ftpprd+ "https://ftpprd.ncep.noaa.gov/data/nccf/com/gfs/prod/gfs.~a/~2,,,'0@a/atmos")
(defparameter +ncep-nomads+ "https://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.~a/~2,,,'0@a/atmos")
(defparameter *noaa-gfs-path* +ncep-nomads+)

(defun download-source-log-string ()
  (if *use-range-query*
      (format nil "~a range query" (subseq *noaa-gfs-path* 8 28))
      "NOMADS grib filter"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOWNLOAD-CYCLE
;;;    
;;;   Search backwards from $start to find a complete cycle

(defparameter *connect-timeout* "10")
(defparameter *retry-interval* 30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOWNLOAD-CURRENT-CYCLE
;;;
;;;   Download current cycle

(defun download-latest-cycle (&key (resolution '("1p00")) (max-offset 384))
  (let ((cycle
          (latest-complete-cycle)))
    (download-cycle cycle :resolution resolution :max-offset max-offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOWNLOAD-CYCLE
;;;
;;;   Download the specified cycle. If a forecast is (still) missing, wait or abort.

(defun download-cycle (cycle &key (resolution '("1p00")) (max-offset 384) (if-missing :wait))
  (ecase (cycle-run cycle) ((or 0 6 12 18)))
  (ecase if-missing ((or :wait :abort)))
  (log2:info "Downloading cycle ~a using ~a" cycle (download-source-log-string))
  (loop
    :with start-time = (now)
    :for count :from 1
    :for offset :across +noaa-forecast-offsets+
    :while (<= offset max-offset)
    :do (loop :for res :in resolution
              :do (progn
                    (when (download-forecast start-time cycle offset res :if-missing if-missing)
                      (sleep 3))))
    :finally (return (values count cycle))))

(defun download-forecast (start-time cycle offset resolution &key (if-missing :wait))
  (log2:trace "Downloading ~a-~a using ~a" cycle offset (download-source-log-string))
  (let* ((destpath
           (noaa-destpath :cycle cycle :offset offset :resolution resolution)))
    (cond
      ((probe-file destpath)
       (log2:info "File exists: ~a" destpath)
       (values nil))
      (t
       (tagbody
         :retry
         (when (> (timestamp-difference (now) start-time) (* 60 60 3))
           (log2:error "Giving up download of ~a at offset ~a" cycle offset)
           (error "Incomplete cycle ~a" cycle))
         (cond
           ((not (noaa-file-exists-p cycle offset resolution))
            (ecase if-missing
              (:wait
               (log2:info "Wait ~as for ~a-~a" *retry-interval* cycle offset)
               (sleep *retry-interval*)
               (go :retry))
              (:abort
               (return-from download-forecast (values nil)))))
           (t
            (handler-case 
                (download-noaa-file% cycle offset destpath :resolution resolution)
              (uiop/run-program:subprocess-error (e)
                (log2:trace "curl error: ~a" e)
                (go :retry))
              (condition (e)
                (log2:trace "Unexpected condition: ~a" e)
                (go :retry))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check if forecast exists on server

(defun noaa-file-exists-p (cycle offset resolution)
  "Check if the specified forecast exists. $date='YYYYMMDD', $cycle='CC', $offset may be numeric or string."
  (let* ((fc-url
          (noaa-file cycle offset :resolution resolution))
         (idx-url
          (noaa-index-file cycle offset :resolution resolution))
         (check-fc
          (format () "curl -sfI --connect-timeout ~a ~a" *connect-timeout* fc-url))
         (check-idx
          (format () "curl -sfI --connect-timeout ~a ~a" *connect-timeout* idx-url)))
    (log2:trace "Checking ~a + .idx" fc-url)
    (handler-case 
       (and (null (uiop:run-program check-fc))
            (null (uiop:run-program check-idx)))
      (uiop/run-program:subprocess-error (e)
        (log2:trace "curl error: ~a" e)
        nil)
      (condition (e)
        (log2:trace "Unexpected condition: ~a" e)
        nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Download forecast

(defun download-noaa-file (cycle offset &key (resolution "1p00"))
  (let* ((spec (noaa-spec :cycle cycle :offset offset :resolution resolution))
         (destpath (noaa-destpath :cycle cycle :offset offset :resolution resolution)))
    (cond
      ((and (probe-file destpath)
            (noaa-file-complete-p destpath))
       (log2:trace "File exists: ~a ~a" destpath cycle))
      (t
       (when (probe-file destpath)
         (log2:info "File truncated: ~a" destpath))
       (log2:trace "~a ~a" destpath cycle)
       (multiple-value-bind
             (out error-out status)
           (download-noaa-file% cycle offset destpath :resolution resolution)
         (declare (ignore out error-out))
         (case status
           (0
            (unless (noaa-file-complete-p destpath)
              (log2:warning  "Deleting ~a (short file), giving up." destpath)
              (uiop:delete-file-if-exists destpath)
              (error "Forecast ~a:~a short file, not available yet?" cycle spec)))
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


(defun download-noaa-file% (cycle offset destpath &key (resolution "1p00"))
  "Retrieve the GRIB file valid at timestamp according to VR rules"
  (if *use-range-query*
      (grib2-download-file-u-v-10 cycle offset :resolution resolution)
      (download-noaa-file%% cycle offset destpath :resolution resolution)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Download by filter

(defun download-noaa-file%% (cycle offset destpath &key (resolution "1p00"))
  "Retrieve the GRIB file valid at timestamp according to VR rules"
  (log2:info "Downloading ~a-~a to ~a" cycle offset destpath)
  (let* ((spec (noaa-spec :cycle cycle :offset offset :resolution resolution))
         (date (cycle-datestring cycle))
         (run (cycle-run cycle))
         (query
          (format nil "~a&~a&~a&~a&~a&~a&~a&~a&~a"
                  (format nil "file=~a" spec)
                  (format nil "dir=%2Fgfs.~a%2F~2,,,'0@a%2Fatmos" date run)
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
          (format () "curl --connect-timeout ~a -n \"~a\" -o ~a" *connect-timeout* url destpath)))
    (log2:trace "~a" ftp-command)
    (uiop:run-program ftp-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Download by range query
(defvar *grib-index-ht* (make-hash-table :test #'equalp))

(defvar +grib-index-ht-lock+
  (bordeaux-threads:make-lock "grib-index-ht"))

(defun cached-grib-index (key value)
  (bordeaux-threads:with-lock-held (+grib-index-ht-lock+)
    (or (gethash key *grib-index-ht*)
        (setf (gethash key *grib-index-ht*)
              value))))

(defun grib2-download-file-u-v-10 (cycle offset &key (resolution "1p00"))
  (let* ((destpath (noaa-destpath :cycle cycle :offset offset :resolution resolution))
         (spec (format nil "~a~a~a" cycle offset resolution))
         (index (cached-grib-index spec
                                   (grib2-get-index cycle offset :resolution resolution))))
    (log2:info "Downloading ~a-~a to ~a" cycle offset destpath)
    (multiple-value-bind (start end)
        (grib2-get-u-v-10-range index)
      (let* ((path
               (noaa-file cycle offset :resolution resolution))
             (url
              (format nil "~a -H \"Range: bytes=~a-~a\""
                      path
                      start
                      end))
             (command (format () "curl ~a\ -o ~a" url destpath)))
        (log2:trace "Command: ~a" command)
        (uiop:run-program command)))))

(defun grib2-get-index (cycle offset &key (resolution "1p00"))
  (let* ((url
           (noaa-index-file cycle offset :resolution resolution))
         (response
          (http-get url))
         (status-code
          (http-status-code
           (http-response-status response)))
         (status-text
          (http-status-text
           (http-response-status response))))
    (log2:trace "Retrieving index for ~a-~a-~a" cycle offset resolution)
    (cond
      ((= status-code 200)
       (http-response-body response))
      (t
       (error "Unexpected HTTP status ~a ~a for URL ~a" status-code status-text url)))))

(defun grib2-get-u-v-10-range (s)
  (let* ((u10
          (search ":UGRD:10 m above ground:" s))
         (u10-start
          (1+ (position #\Newline s :from-end t :end u10)))
         (u10-end
          (1+ (position #\Newline s :from-end nil :start u10)))
         (v10-end
          (1+ (position #\Newline s :from-end nil :start u10-end)))
         (next-end
          (1+ (position #\Newline s :from-end nil :start v10-end)))
         (u10-entry
          (subseq s u10-start (1- u10-end)))
         (next-entry
          (subseq s v10-end (1- next-end)))
         (u10-range-start
          (second
           (cl-utilities:split-sequence #\: u10-entry)))
         (range-end
          (second
           (cl-utilities:split-sequence #\: next-entry))))
    (values (parse-integer u10-range-start)
            (1- (parse-integer range-end)))))

(defun http-get (url &key (headers ()))
  (let ((ftp-command
         (format () "curl -i ~{ -H ~a~} \"~a\"" headers url))
        (out-stream (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
        (err-stream (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
    (log2:trace "~a" ftp-command)
    (with-output-to-string (out out-stream)
      (with-output-to-string (err err-stream)
        (multiple-value-bind (out-status err-status program-status)
            (uiop:run-program ftp-command :output out :error-output err)
          (declare (ignore out-status err-status))
          (case program-status
            (0
             (parse-http-response out-stream))
            (t
             (error "curl failed with code ~a" program-status))))))))

(defstruct http-response status headers body)
(defun parse-http-response (s)
  (with-input-from-string (f s)
    (let*
        ((status-line
          (parse-status-line (read-line f nil nil)))
         (headers
          (loop
             :for line = (read-line f nil nil)
             :while  (> (length line) 1)
             :collect (parse-http-header line)))
         (content-length
          (parse-integer
           (http-header-value
            (find-if (lambda (h) (string-equal (http-header-name h) "Content-Length"))
                     headers))))
         (buffer
          (make-array content-length :element-type 'character)))
      (read-sequence buffer f)
      (make-http-response :status status-line
                          :headers headers
                          :body buffer))))

(defstruct http-status protocol code text)
(defun parse-status-line (s)
  (log2:trace "~a" s) 
  (let* ((p1 (position #\Space s))
         (p2 (position #\Space s :start (1+ p1)))) 
    (make-http-status :protocol (subseq s 0 p1)
                      :code (parse-integer (subseq s (1+ p1) p2))
                      :text (string-trim " "
                                         (subseq s (1+ p2))))))

(defstruct http-header name value)
(defun parse-http-header (s)
  (log2:trace "~a" s) 
  (let ((p (position #\: s)))
    (make-http-header :name (subseq s 0 p)
                      :value (string-trim " " (subseq s (1+ p))))))
                                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grib filter input file name 

(defun noaa-spec (&key (cycle 0) (offset 6) (basename "pgrb2") (resolution "1p00"))
  (format () "gfs.t~2,,,'0@az.~a.~a.f~3,,,'0@a"
          (cycle-run cycle)
          basename
          resolution
          offset))

(defun noaa-file (cycle offset &key (basename "pgrb2") (resolution "1p00"))
  (format () "~?/~a"
          *noaa-gfs-path*
          (list (cycle-datestring cycle)
                (cycle-run cycle))
          (noaa-spec :cycle cycle :offset offset :basename basename :resolution resolution)))

(defun noaa-index-file (cycle offset &key (basename "pgrb2") (resolution "1p00"))
  (format () "~?/~a.idx"
          *noaa-gfs-path*
          (list (cycle-datestring cycle)
                (cycle-run cycle))
          (noaa-spec :cycle cycle :offset offset :basename basename :resolution resolution)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local forecast file name

(defun noaa-destpath (&key (cycle 0) (offset 6) (basename "pgrb2") (resolution "1p00"))
  (let* ((spec
           (noaa-spec :cycle cycle :offset offset  :basename basename :resolution resolution))
         (destfile 
           (format () "~a_~a.grib2" (cycle-datestring cycle) spec)))
    (merge-pathnames destfile *grib-directory*)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
