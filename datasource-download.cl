;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   GRIB data sources
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2026-02-12 19:33:42>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://confluence.ecmwf.int/display/DAC/Dissemination+schedule

(defun schedule-download (datasource-classname &key (async :datasource))
  "Schedule downloads for DATASOURCE."
  (ecase async ((:none :datasource :step)))
  (let* ((schedule (datasource-schedule datasource-classname))
         (hours
           (mapcar
            (lambda (run)
              (+ run (truncate (schedule-dissem-start schedule) 60)))
            (schedule-runs schedule)))
         (minutes
           (list
            (mod (schedule-dissem-start schedule) 60))))
    ;; Kick off download delayed by dissemination delay.
    (log2:info "Scheduling ~a at ~a:~a" datasource-classname hours minutes)
    (timers:add-timer
     (lambda ()
       (let* ((timestamp
                (adjust-timestamp (now)
                  (offset :minute (- (schedule-dissem-start schedule)))))
              (datasource
                (get-datasource datasource-classname
                                 (make-cycle :timestamp timestamp))))
         (download-datasource datasource)))
     :hours hours
     :minutes minutes)))

(defmethod download-datasource (datasource &key (async :none))
  (ecase async ((:none :datasource :step)))
  (flet ((download-fn ()
           (dolist (step (schedule-steps (schedule datasource)))
             (log2:info "Downloading ~a ~a ~a" (name datasource) (cycle-run (cycle datasource)) step)
             (download-step datasource step :async (eql async :step)))))
    (case async
      (:none
       (download-fn))
      (otherwise
       (bordeaux-threads:make-thread #'download-fn :name "DATASOURCE-DOWNLOAD")))))

(defmethod download-step ((datasource datasource) step &key (retries 50) (async nil))
  "Download a single step from the datasource. Signal a MISSING-GRIB
error if the source grib was not available. If RETRY is true, retry
after 30s for max. 90min before giving up. If ASYNC is true, run
asynchronously."
  (let ((local-path (local-pathname datasource step)))
    (when (probe-file local-path)
      (log2:info "File exists: ~a" local-path)
      (return-from download-step (values t)))
    (ensure-directories-exist local-path)
    (dotimes
        (try retries)
      (when (probe-uris datasource step)
        (handler-case
            (progn
              (download-step-1 datasource step local-path)
              (log2:info "Downloaded ~a ==> ~a" (data-uri datasource step) local-path)
              (cond
                ((grib-file-complete-p local-path)
                 (return-from download-step t))
                (t
                 (log2:warning "Incomplete or incorrect download, deleting ~a" local-path)
                 (delete-file local-path))))
          (uiop/run-program:subprocess-error (e)
            (log2:warning "curl error: ~a" e))
          (condition (e)
            (log2:warning "Unexpected condition: ~a" e))))
      (log2:info "Waiting 30s for ~a ~a ~a" (name datasource) (cycle-run (cycle datasource)) step)
      (sleep 30))
    (log2:warning "Final failed download ~a ~a ~a" (name datasource) (cycle-run (cycle datasource)) step)
    (return-from download-step nil)))

(defmethod probe-uris (datasource step)
  (and (check-uri-exists (data-uri datasource step))
       (check-uri-exists (index-uri datasource step))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Datasource-specific methods

(defmethod download-step-1 ((datasource datasource) step local-path)
  (let* ((ranges (get-grib-file-ranges datasource step)))
    ;; AWS only supports a single byte range and returns the full file if multiple ranges are requested.
    ;; We download ranges individually and concatenate the GRIBs into the final file LOCAL-PATH. 
    (when (null ranges)
      (log2:warning "No ranges could be determined for ~a" datasource))
    (let* ((parts
             (loop
               :for part-no :from 1
               :for range :in ranges
               :for part-file-name = (format nil "~a.~a" local-path part-no)
               :do (download-range datasource step range part-file-name)
               :collect part-file-name))
           (cmd   
             (format nil "cat ~{~a~^ ~} > ~a" parts local-path))
           (delcmd
             (format nil "rm ~{~a~^ ~}" parts)))
      (uiop:run-program cmd)
      (uiop:run-program delcmd))))

(defmethod download-step-1 ((datasource noaa-rtofs-current) step local-path)
  ;; RTOFS current gribs don't support ranges (?)
  (download-file datasource step local-path))

(defmethod download-step-1 ((datasource fw-current) step local-path)
  ;; RTOFS current gribs don't support ranges (?)
  (download-file datasource step local-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions

(defun download-file (datasource step local-path)
  (let* ((url
           (data-uri datasource step))
         (cmd
           (format nil "curl ~a -o ~a" url local-path)))
    (log2:trace "~a" cmd)
    (uiop:run-program cmd)))
  
(defun download-range (datasource step range part-file-name)
  (let* ((ranges-header
           (format nil  "\"Range: bytes=~a-~a\"" (range-start range) (range-end range)))
         (url
           (data-uri datasource step))
         (cmd
           (format nil "curl -H ~a ~a -o ~a" ranges-header url part-file-name)))
    (log2:trace "~a" cmd)
    (uiop:run-program cmd)))

(defmethod local-pathname ((datasource datasource) step &key (relative nil))
  (let* ((srcname (name datasource))
         (res (resolution datasource))
         (cycle  (cycle datasource))
         (date (cycle-datestring cycle))
         (run (format nil "~2,,,'0@a" (cycle-run cycle)))
         (step (format nil "~3,,,'0@a" step))
         (file-name
           (format nil "~a.~a.~a.t~az.f~a" srcname res date run step))
         (file-dir
           (list :relative srcname res date run))
         (pathname
           (make-pathname :directory file-dir
                          :name file-name
                          :type "grib2")))
    (if relative pathname
        (merge-pathnames pathname
                         (pathname *grib-directory*)))))

(defmethod retrieve-index-file ((datasource datasource) step)
  (let* ((url (index-uri datasource step))
         (retry 3))
    (tagbody
       :retry
       (let* ((response (http-get url))
              (body (http-response-body response))
              (status-code (http-status-code
                            (http-response-status response)))
              (status-text (http-status-text
                            (http-response-status response))))
         (log2:trace-more "~a => ~a" url body)
         (cond
           ((= status-code 200)
            (return-from retrieve-index-file body))
           ((= status-code 302)
            (when (> retry 0)
              (decf retry)
              (log2:warning "Retry ~a/3 ~a" (- 3 retry) url)
              (sleep 10)
              (go :retry))
            (error "Unexpected HTTP status ~a ~a for URL ~a" status-code status-text url))
           (t
            (error "Unexpected HTTP status ~a ~a for URL ~a" status-code status-text url)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check if forecast exists on server

(defun check-uri-exists (uri)
  (log2:trace "~a" uri)
  (handler-case
      (let ((response (http-get uri :method :head)))
        (case (http-status-code
               (http-response-status response))
          (200
           (log2:trace "~a => T" uri)
           t)
          (otherwise
           (log2:trace "~a => nil" uri)
           nil)))
    (uiop/run-program:subprocess-error (e)
      (log2:trace "curl error: ~a" e)
      nil)
    (condition (e)
      (log2:trace "Unexpected condition: ~a" e)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check downloaded file

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
        (log2:trace "Checking download complete ~a ==> ~a" destpath result)
        result))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
