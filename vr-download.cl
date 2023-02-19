;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2023-02-19 17:52:17>

(in-package "CL-WEATHER")

(defparameter +vr-grib-url-template+
;;;https://static.virtualregatta.com/winds/fine/20230226/20230218.06.186.grb
  "https://static.virtualregatta.com/winds/fine/~a/~a.~2,,,'0@a.~3,,,'0@a.grb")

(defparameter +vr-destpath-template+
  "~a.~2,,,'0@a.~3,,,'0@a.~a.grb")

(defun vr-download-cycle (cycle &key (max-offset 288) (if-missing :retry))
  (ecase (cycle-run cycle) ((or 0 6 12 18)))
  (ecase if-missing ((or :retry :error)))
  (log2:info "Downloading from VR ~a" cycle)
  (loop
    :with start-time = (now)
    :for count :from 1
    :for offset :from 9 :to  max-offset :by 3
    :do (vr-download-forecast cycle offset :if-missing if-missing)
    :finally (return (values count cycle))))

(defun vr-download-forecast (cycle offset &key (if-missing :retry))
  (let* ((destpath (vr-destpath :cycle cycle :offset offset :resolution "0p25"))
         (url (vr-grib-url cycle offset)))
    (ensure-directories-exist destpath)
    (cond
      ((and (probe-file destpath)
            (grib-file-complete-p destpath))
       (log2:info "File exists: ~a" destpath)
       (values nil))
      (t
       (tagbody
         :retry
         (cond
           ((not (check-uri-exists url))
            (ecase if-missing
              (:retry
               (log2:info "Waiting ~as for ~a-~a" *retry-interval* cycle offset)
               (sleep *retry-interval*)
               (when (> (timestamp-difference (now) (cycle-timestamp cycle)) (* 60 60 6))
                 (log2:error "Giving up retrying download of ~a at offset ~a" cycle offset)
                 (error "Incomplete cycle ~a" cycle))
               (go :retry))
              (:error
               (log2:error "Giving up download of ~a at offset ~a" cycle offset)
               (error "Incomplete cycle ~a" cycle))))
           (t
            (handler-case 
                (vr-download-forecast% url destpath)
              (uiop/run-program:subprocess-error (e)
                (log2:trace "curl error: ~a" e)
                (go :retry))
              (condition (e)
                (log2:trace "Unexpected condition: ~a" e)
                (go :retry))))))))))

(defun vr-download-forecast% (url destpath)
  (log2:info "Downloading ~a to ~a" url destpath)
  (let ((command
          (format () "curl --max-time 480 --connect-timeout ~a ~a\ -o ~a" *connect-timeout* url destpath)))
    (uiop:run-program command)))

(defun vr-grib-url (cycle offset)
  (let* ((run (cycle-run cycle))
         (timestamp (cycle-timestamp cycle))
         (date (format-yyyymmdd nil timestamp))
         (dir (format-yyyymmdd nil (adjust-timestamp timestamp (offset :hour offset)))))
    (format nil +vr-grib-url-template+ dir date run offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local forecast filename

(defun vr-destpath (&key (cycle (make-cycle)) (offset 6) (resolution "0p25"))
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
    (merge-pathnames destfile cycle-dir)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
