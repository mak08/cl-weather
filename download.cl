;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2026-01-04 15:55:37>

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

(defparameter +ncep-nomads+ "https://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.~a/~2,,,'0@a/atmos")
(defparameter +ncep-ftpprd+ "https://ftpprd.ncep.noaa.gov/data/nccf/com/gfs/prod/gfs.~a/~2,,,'0@a/atmos")
(defparameter +gens-ftpprd+ "https://ftpprd.ncep.noaa.gov/data/nccf/com/gens/prod/gefs.~a/~2,,,'0@a/atmos/pgrb2ap5") ;; /gep05.t00z.pgrb2a.0p50.f756.idx

(defparameter *noaa-gfs-path* +ncep-nomads+)


(defparameter +vr-destpath-template+
  "~a.~2,,,'0@a.~3,,,'0@a.~a.grb")

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

(defun download-cycle (cycle &key (resolution '("0p25")) (max-offset 384) (if-missing :wait))
  (ignore-errors
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
     :finally (return (values count cycle)))))

(defun download-forecast (start-time cycle offset resolution &key (if-missing :wait))
  (log2:trace "Downloading ~a-~a using ~a" cycle offset (download-source-log-string))
  (let* ((destpath
           (forecast-destpath :source :noaa :cycle cycle :offset offset :resolution resolution)))
    (ensure-directories-exist destpath) 
    (cond
      ((and (probe-file destpath)
            (grib-file-complete-p destpath))
       (log2:info "File exists: ~a" destpath)
       (values nil))
      (t
       (tagbody
         :retry
         (when (> (timestamp-difference (now) start-time) (* 60 60 3))
           (log2:error "Giving up download of ~a at offset ~a" cycle offset)
           (error "Incomplete cycle ~a" cycle))
         (cond
           ((not (noaa-uris-exist-p cycle offset resolution))
            (ecase if-missing
              (:wait
               (log2:info "Waiting ~as for ~a-~a" *retry-interval* cycle offset)
               (sleep *retry-interval*)
               (go :retry))
              (:abort
               (return-from download-forecast (values nil)))))
           (t
            (handler-case 
                (download-noaa-file% cycle offset destpath :resolution resolution)
              (uiop/run-program:subprocess-error (e)
                (log2:error "curl error: ~a" e)
                (go :retry))
              (condition (e)
                (log2:error "Unexpected condition: ~a" e)
                (go :retry))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Delete cycle

(defun file-archive-path (filename)
  (let* ((file-date-path
           (list (subseq filename 0 4)
                 (subseq filename 4 6)
                 (subseq filename 6 8)))
         (archive-path
           (make-pathname :directory (append (pathname-directory *grib-directory*)
                                             '("archive")
                                             file-date-path))))
    (merge-pathnames archive-path filename)))

(defun cleanup-cycles (&key (dry-run t))
  (log2:info "Deleting old forecasts")
  (let* ((pathnames (append
                     (directory (format nil "~a/1p00/**/**/*.grb" *vr-grib-directory*))
                     (directory (format nil "~a/0p25/**/**/*.grb" *vr-grib-directory*))
                     (directory (format nil "~a/1p00/**/**/*.grib2" *grib-directory*))
                     (directory (format nil "~a/0p25/**/**/*.grib2" *grib-directory*))))
         (yesterday (adjust-timestamp (now) (offset :day -1)))
         (archive-dir (make-pathname
                       :directory (append (pathname-directory *grib-directory*)
                                          '("archive"))))
         (have-archive (ignore-errors
                        (ensure-directories-exist archive-dir))))
    (unless have-archive
      (log2:warning "Directory ~a does not exist, not archiving" archive-dir)
      (return-from cleanup-cycles nil))
    (dolist (path pathnames)
      (log2:trace "Checking ~a" path)
      (when (timestamp< (timestamp-from-path path)
                        yesterday)
        (cond
          ((not (< 0
                   (forecast-offset-from-path path)
                   16))
           (log2:info "Deleting ~a" path)
           (delete-file path))
          (t
           (let* ((archive-path
                    (file-archive-path (format nil "~a.~a"
                                               (pathname-name path)
                                               (pathname-type path)))))
             (log2:info "Archiving ~a to ~a" path archive-path)
             (when (not dry-run)
               (ensure-directories-exist archive-path)
               (rename-file path archive-path))))))))
  (when (not dry-run)
    (log2:info "Deleting empty directories")
    (remove-empty-directories *grib-directory*)
    (remove-empty-directories *vr-grib-directory*)))

(defun timestamp-from-path (path)
  (let*  ((filename (pathname-name path))
          (yyyy (subseq filename 0 4))
          (mm (subseq filename 4 6))
          (dd (subseq filename 6 8)))
    (or
     (ignore-errors
      (let* ((hh (subseq filename 14 16))
             (date-string (format nil "~a-~a-~aT~a:00:00Z" yyyy mm dd hh)))
        (parse-rfc3339-timestring date-string)))
     (ignore-errors
      (let* ((hh (subseq filename 9 10))
             (date-string (format nil "~a-~a-~aT~a:00:00Z" yyyy mm dd hh)))
        (parse-rfc3339-timestring date-string))))))

(defun forecast-offset-from-path (path)
  (let*  ((filename (pathname-name path)))
    (or
     (ignore-errors
      (parse-integer(subseq filename 30 33)))
     (ignore-errors
      (parse-integer(subseq filename 12 15))))))
  
(defun remove-empty-directories (path)
  (let ((contents (append
                   (directory
                    (concatenate 'string (namestring path) "*.*")))))
    (or (null contents)
        (every (lambda (p)
                 (when (and
                        (null  (pathname-name p))
                        (remove-empty-directories p))
                   (log2:info "Deleting ~a" p)
                   (sb-ext:delete-directory p)))
               contents))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check if forecast exists on server

(defun noaa-uris-exist-p (cycle offset resolution)
  "Check if the specified forecast exists. $date='YYYYMMDD', $cycle='CC', $offset may be numeric or string."
  (let* ((fc-url
          (noaa-file cycle offset :resolution resolution))
         (idx-url
           (noaa-index-file cycle offset :resolution resolution))
         (result
           (and (check-uri-exists fc-url)
                (check-uri-exists idx-url))))
    (log2:trace "Checking ~a + .idx ==> ~a" fc-url result)
    result))


(defun check-uri-exists (uri)
  (log2:trace "~a" uri)
  (handler-case
      (let ((response (http-get uri :method :head)))
        (case (http-status-code
               (http-response-status response))
          (200
           t)
          (otherwise
           nil)))
    (uiop/run-program:subprocess-error (e)
      (log2:trace "curl error: ~a" e)
      nil)
    (condition (e)
      (log2:trace "Unexpected condition: ~a" e)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Download forecast

(defun download-noaa-file (cycle offset &key (resolution "1p00"))
  (let* ((spec (noaa-spec :cycle cycle :offset offset :resolution resolution))
         (destpath (forecast-destpath :source :noaa :cycle cycle :offset offset :resolution resolution)))
    (cond
      ((and (probe-file destpath)
            (grrib-file-complete-p destpath))
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
            (unless (grib-file-complete-p destpath)
              (log2:warning  "Deleting ~a (short file), giving up." destpath)
              (uiop:delete-file-if-exists destpath)
              (error "Forecast ~a:~a short file, not available yet?" cycle spec)))
           (otherwise
            (error "cURL error ~a" status))))))
    destpath))

(defun grib-file-complete-p (destpath)
  (with-open-file (f destpath :element-type 'character :external-format :utf-8)
    (let* ((length
            (file-length f))
           (closing-bytes
            (make-array 4)))
      (ignore-errors
        (file-position f (- length 4))
        (read-sequence closing-bytes f))
      (let ((result
             (every
              (lambda (c) (eql c #\7))
              closing-bytes)))
        (log2:trace "Checking complete download ~a ==> ~a" destpath result)
        result))))

(defun download-noaa-file% (cycle offset destpath &key (resolution "1p00"))
  "Retrieve the GRIB file valid at timestamp according to VR rules"
  (if *use-range-query*
      (grib2-range-uv10 cycle offset destpath :resolution resolution)
      (grib2-filter-uv10 cycle offset destpath :resolution resolution))
  (unless (and (probe-file destpath)
               (grib-file-complete-p destpath))
    (error 'incomplete-download :cycle cycle :offset offset :resolution resolution :filename destpath))
  (setf *latest-forecast* (format nil "~a:~a" cycle offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Download by filter

(defun grib2-filter-uv10 (cycle offset destpath &key (resolution "1p00"))
  "Retrieve the GRIB file valid at timestamp according to VR rules"
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
           (format () "curl --max-time 480 --connect-timeout ~a -n \"~a\" -o ~a" *connect-timeout* url destpath)))
    (log2:info "Downloading ~a to ~a" url destpath)
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

(defun grib2-range-uv10 (cycle offset destpath &key (resolution "1p00"))
  (let* ((spec (format nil "~a~a~a" cycle offset resolution))
         (index (cached-grib-index spec
                                   (grib2-get-index cycle offset :resolution resolution))))
    (multiple-value-bind (start end)
        (grib2-get-u-v-10-range index)
      (let* ((path
               (noaa-file cycle offset :resolution resolution))
             (url
              (format nil "~a -H \"Range: bytes=~a-~a\""
                      path
                      start
                      end))
             (download-cmd (format () "curl --max-time 480 --connect-timeout ~a ~a\ -o ~a" *connect-timeout* url destpath))
             (transform-cmd (create-transform-command cycle offset resolution)))
        (log2:info "Downloading ~a to ~a" url destpath)
         (uiop:run-program download-cmd)
        (when *generate-jpeg-compressed-gribs*
          (log2:info "Transform: ~a" transform-cmd)
          (uiop:run-program transform-cmd))))))

(defun create-transform-command (cycle offset resolution)
  (format () "wgrib2 ~a -set_grib_max_bits 7 -set_grib_type jpeg -grib_out ~a"
          (forecast-destpath :source :noaa :cycle cycle :offset offset :resolution resolution)
          (forecast-destpath :source :vr :cycle cycle :offset offset :resolution resolution)))
  
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

(defun http-get (url &key (headers ()) (method :get))
  (ecase method ((:get :head)))
  (let* ((head-flag (if (eq method :head) "-I" ""))
         (ftp-command
           (format () "curl --max-time 3 -i ~a ~{ -H ~a~} \"~a\"" head-flag headers url))
         (out-stream (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t))
         (err-stream (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t)))
    (log2:trace "~a" ftp-command)
    (with-output-to-string (out out-stream)
      (with-output-to-string (err err-stream)
        (multiple-value-bind (out-status err-status program-status)
            (uiop:run-program ftp-command :ignore-error-status t :output out :error-output err)
          (declare (ignore out-status err-status))
          (case program-status
            (0
             (parse-http-response out-stream :fetch-body (not (eq method :head))))
            (3
             (error "curl: error 3 (URL malformed)"))
            (6
             (error "curl: error 6 (Couldn't resolve host)"))
            (7
             (error "curl: error 7 (Failed to connect to host)"))
            (t
             (error "curl: error ~a" program-status))))))))

(defstruct http-response status headers body)
(defun parse-http-response (s &key (fetch-body t))
  (with-input-from-string (f s)
    (let*
        ((status-line
          (parse-status-line (read-line f nil nil)))
         (headers
          (loop
             :for line = (read-line f nil nil)
             :while  (> (length line) 1)
             :collect (parse-http-header line)))
         (body
           (cond
             (fetch-body
              (let* ((content-length-header
                       (find "Content-Length" headers :test #'string-equal :key #'http-header-name))
                     (content-length
                       (if content-length-header
                           (parse-integer
                            (http-header-value content-length-header))
                           0))
                     (buffer
                       (make-array content-length :element-type 'character)))
                (read-sequence buffer f)
                buffer))
             (t
              #()))))
      (make-http-response :status status-line
                          :headers headers
                          :body body))))

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

(defun noaa-spec (&key (cycle (make-cycle)) (offset 6) (basename "pgrb2") (resolution "1p00"))
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

(defun forecast-destpath (&key
                            (source :noaa)
                            (cycle (make-cycle))
                            (offset 6)
                            (basename "pgrb2")
                            (resolution "0p25"))
  (ecase source
    (:noaa
     (let* ((spec
              (noaa-spec :cycle cycle :offset offset  :basename basename :resolution resolution))
            (destfile 
              (format () "~a_~a.grib2" (cycle-datestring cycle) spec))
            (cycle-dir
              (make-pathname :directory (append (pathname-directory *grib-directory*)
                                                (list resolution
                                                      (cycle-datestring cycle)
                                                      (format nil "~2,,,'0@a" (cycle-run cycle)))))))
       (merge-pathnames destfile cycle-dir)))
    (:vr
     (let* ((run (cycle-run cycle))
            (timestamp (cycle-timestamp cycle))
            (date (format-yyyymmdd nil timestamp))
            (destfile 
              (format () +vr-destpath-template+ date run offset resolution))
            (cycle-dir
              (make-pathname :directory (append (pathname-directory *vr-grib-directory*)
                                                (list resolution
                                                      (cycle-datestring cycle)
                                                      (format nil "~2,,,'0@a" (cycle-run cycle)))))))
       (merge-pathnames destfile cycle-dir)))))

(defun noaa-archivepath (&key (cycle 0) (offset 6) (basename "pgrb2") (resolution "1p00"))
  (let* ((spec
           (noaa-spec :cycle cycle :offset offset  :basename basename :resolution resolution))
         (destfile 
           (format () "~a_~a.grib2" (cycle-datestring cycle) spec)))
    (file-archive-path destfile)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
