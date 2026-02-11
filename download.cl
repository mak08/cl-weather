;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2026-02-08 14:49:07>

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
    (remove-empty-directories *grib-directory*)))

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

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
