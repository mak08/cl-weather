;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Access to NOAA forecasts (non-interpolated)
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2026-04-03 01:39:07>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading forecast data

(defvar *forecast-access-context* nil)

(defmacro with-forecast-access-context (() &body body)
  "Keep absent files unavailable throughout BODY, without retaining failures globally.
Each invocation creates a fresh dynamically scoped context. Successful forecasts
remain shared; corrupt data and other access errors are not remembered."
  `(let ((*forecast-access-context* (make-hash-table :test #'equal)))
     ,@body))

(defun forecast-file-path (datasource step)
  "Identify the actual file, including sources storing multiple steps together."
  (namestring (merge-pathnames (local-pathname datasource (file-step datasource step)))))

(defun signal-missing-forecast-file (datasource step filename)
  "Report the current step accurately even when another step first found the file missing."
  (error 'missing-forecast :cycle (cycle datasource) :offset step
         :resolution (resolution datasource) :filename filename))

(defun remember-missing-forecast-file (filename)
  "Remember a confirmed absence once in the active access context."
  (unless (gethash filename *forecast-access-context*)
    (setf (gethash filename *forecast-access-context*) t)
    (log2:warning "Forecast file missing in this access context: ~a" filename)))

(defun load-forecast-file-in-context (datasource step filename)
  "Avoid native missing-file opens; record only absence, including a deletion during loading."
  (unless (probe-file filename)
    (remember-missing-forecast-file filename)
    (signal-missing-forecast-file datasource step filename))
  (handler-case (load-datasource-forecasts datasource step)
    (missing-forecast (e)
      (unless (probe-file filename)
        (remember-missing-forecast-file filename))
      (error e))
    (missing-file (e)
      ;; File-based regional sources use MISSING-FILE rather than MISSING-FORECAST.
      (if (probe-file filename)
          (error e)
          (progn
            (remember-missing-forecast-file filename)
            (signal-missing-forecast-file datasource step filename))))))

(defmethod load-forecast (datasource step)
  "Return requested U/V data from Cache, fill cache if needed."
  (let ((max (maxstep datasource)))
    (when (> step max)
      (log2:warning "Step ~a out of range, using max (~a)" step max)
      (setf step max))
    (let* ((key (get-key datasource step))
           (filename (when *forecast-access-context* (forecast-file-path datasource step))))
      (bordeaux-threads:with-lock-held (+forecast-ht-lock+)
        ;; A different context may have populated the shared cache meanwhile.
        ;; This context must still regard its previously absent file as missing.
        (when (and filename (gethash filename *forecast-access-context*))
          (signal-missing-forecast-file datasource step filename))
        (unless (gethash key *forecast-ht*)
          ;; LOAD-DATASOURCE-FORECASTS loads a file, may load multiple steps.
          ;; Caching is performed by that method.
          (if filename
              (load-forecast-file-in-context datasource step filename)
              (load-datasource-forecasts datasource step)))
        (gethash key *forecast-ht*)))))

(defun get-direction-key (datasource step)
  (let ((key (get-key datasource step)))
    (setf (second key) (concatenate 'string (name datasource) "-dir"))
    key))

(defmethod load-direction-forecast (datasource step)
  "Return wave direction data from cache, fill cache if needed."
  (let ((max (maxstep datasource)))
    (when (> step max)
      (setf step max))
    (load-forecast datasource step)
    (bordeaux-threads:with-lock-held (+forecast-ht-lock+)
      (gethash (get-direction-key datasource step) *forecast-ht*))))

(defun get-key (datasource step)
  (list step
        (name datasource)
        (cycle-run (cycle datasource))
        (cycle-datestring (cycle datasource))
        (resolution datasource)))

(defmethod load-datasource-forecasts ((datasource datasource) step)
  ;; Determine file containing the requested step.
  ;; Load & cache all steps from the file.
  (let* ((file-step (file-step datasource step))
         (filename (namestring (local-pathname datasource file-step)))
         (u-and-v-var (uv-variables datasource)))
    (with-grib-index (index '("step" "shortName"))
      (log2:info "Loading forecast ~a~%" filename)
      (let ((retcode (codes-index-add-file index filename)))
        (case retcode
          (0)
          (-11
           (let ((cycle (cycle datasource))
                 (resolution (resolution datasource)))
             (log2:warning "codes-index-add-file: ~a ~a" filename retcode)
             (error 'missing-forecast :cycle cycle :offset step :resolution resolution :filename filename)))
          (t
           (let ((message (codes-get-error-message retcode)))
             (log2:warning "codes-index-add-file: ~a: ~a" filename retcode)
             (error "eccodes: ~a: ~a" filename message))))
        (let ((uvdata (get-uv-steps-from-index index u-and-v-var (name datasource))))
          (dolist (uv uvdata)
            (let ((step (uv-step uv)))
              (setf (gethash (get-key datasource step) *forecast-ht*) uv))))))))

(defmethod load-datasource-forecasts ((datasource fw-current) step)
  (let* ((file-step (file-step datasource step))
         (filename (namestring (local-pathname datasource file-step)))
         (u-and-v-var (uv-variables datasource)))
    (log2:info "Loading forecast ~a~%" filename)
    (let ((uvdata (get-uv-messages-from-file filename)))
      (dolist (uv uvdata)
        (let ((step (uv-step uv)))
          (log2:trace "loading ~a step ~a" datasource step)
          (setf (gethash (get-key datasource step) *forecast-ht*) uv))))))

(defun clear-missing-current-vectors (field)
  "Turn bitmap-missing ocean components into zero flow before interpolation.
ecCodes represents missing land values as 9999, which must never become speed."
  (when field
      (let ((missing (gribinfo-missing-value (uv-info field)))
            (u (uv-u-array field)) (v (uv-v-array field)))
        (dotimes (i (length u))
          (when (or (= (aref u i) missing) (= (aref v i) missing))
            (setf (aref u i) 0d0 (aref v i) 0d0)))))
  field)

(defmethod load-datasource-forecasts :after ((datasource ecmwf-ifs-current) step)
  "Remove land bitmap sentinels from newly decoded IFS current components."
  (clear-missing-current-vectors (gethash (get-key datasource step) *forecast-ht*)))

(defmethod load-datasource-forecasts ((datasource gfswave-combined) step)
  (let* ((file-step (file-step datasource step))
         (filename (namestring (local-pathname datasource file-step))))
    (log2:info "Loading forecast ~a~%" filename)
    (with-grib-index (index '("step" "discipline" "parameterCategory" "parameterNumber"))
      (let ((retcode (codes-index-add-file index filename)))
        (case retcode
          (0)
          (-11
           (let ((cycle (cycle datasource))
                 (resolution (resolution datasource)))
             (log2:warning "codes-index-add-file: ~a ~a" filename retcode)
             (error 'missing-forecast :cycle cycle :offset step :resolution resolution :filename filename)))
          (t
           (let ((message (codes-get-error-message retcode)))
             (log2:warning "codes-index-add-file: ~a: ~a" filename retcode)
             (error "eccodes: ~a: ~a" filename message))))
        (let* ((query (list (list "discipline" 10)
                            (list "parameterCategory" 0)
                            (list "parameterNumber" 3)))
               (dir-query (list (list "discipline" 10)
                                (list "parameterCategory" 0)
                                (list "parameterNumber" 10)))
               (messages (get-messages-from-index index query))
               (dir-messages (get-messages-from-index index dir-query)))
          (dolist (message messages)
            (let ((step (gribmessage-step message)))
              (setf (gethash (get-key datasource step) *forecast-ht*) message)))
          (dolist (message dir-messages)
            (let ((step (gribmessage-step message)))
              (setf (gethash (get-direction-key datasource step) *forecast-ht*)
                    message))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cleanup forecast HT to avoid memory exhaustion

(defvar *forecast-cleanup-timer*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun start-forecast-ht-cleanup ()
  (log2:info "Starting forecast cleanup thread")
  (setf *forecast-cleanup-timer*
        (timers:add-timer #'forecast-ht-cleanup
                          :id "FORECAST-CLEANUP"
                          :hours '(5 11 17 23)
                          :minutes '(10))))

(defun forecast-ht-cleanup ()
  (let* ((expiry (adjust-timestamp (now) (:offset :hour -12))))
    (bordeaux-threads:with-lock-held (+forecast-ht-lock+)
      (log2:info "Searching hash entries older than ~a" (format-timestring nil expiry))
      (maphash
       (lambda (k v)
         (declare (ignore v))
         (destructuring-bind (offset source run datestring resolution)
             k
           (let* ((time (parse-timestring (datestring-run-to-timestamp datestring run)))
                  (remove (timestamp< time expiry)))
             (log2:trace "~a ~a <FC> ~a-~a-~a <fc-age> ~a <exp-age> ~a => ~:[keep~;remove~]"
                        source
                        resolution
                        datestring run offset
                        (format-datetime nil time)
                        (format-datetime nil expiry)
                        remove)
             (when remove
               (log2:info "Removing ~a" k)
               (remhash k cl-weather::*forecast-ht*)))))
       *forecast-ht*))))
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
