;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Access to NOAA forecasts (non-interpolated)
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2026-02-11 20:46:40>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading forecast data

(defun load-forecast (datasource step)
  "Return requested U/V data. Cached."
  (let ((max (maxstep datasource)))
    (when (> step max)
      (log2:warning "Step ~a out of range, using max (~a)" step max)
      (setf step max))
    (let* ((key (get-key datasource step)))
      (bordeaux-threads:with-lock-held (+forecast-ht-lock+)
        (unless (gethash key *forecast-ht*)
          (load-datasource-forecasts datasource step))
        (gethash key *forecast-ht*)))))

(defun get-key (datasource step)
  (list step
        (name datasource)
        (cycle-run (cycle datasource))
        (cycle-datestring (cycle datasource))
        (resolution datasource)))

(defmethod load-datasource-forecasts ((datasource datasource) step)
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
        (let ((uvdata (get-uv-steps-from-index index u-and-v-var)))
          (dolist (uv uvdata)
            (let ((step (uv-step uv)))
              (setf (gethash (get-key datasource step) *forecast-ht*) uv))))))))

(defmethod load-datasource-forecasts ((datasource fw-current) step)
  (let* ((file-step (file-step datasource step))
         (filename (namestring (local-pathname datasource file-step)))
         (u-and-v-var (uv-variables datasource)))
    (log2:info "Loading forecast ~a~%" filename)
    (let ((uvdata (get-messages-from-file filename)))
      (dolist (uv uvdata)
        (let ((step (uv-step uv)))
          (log2:trace "loading ~a step ~a" datasource step)
          (setf (gethash (get-key datasource step) *forecast-ht*) uv))))))

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
         (destructuring-bind (source datestring run offset resolution)
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
