;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Access to NOAA forecasts (non-interpolated)
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-12-26 20:17:54>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun noaa-forecast (&key (cycle 0) (offset 0) (resolution "1p00"))
  "Read GRIB data into U and V arrays. Assumes the GRIB file contains U-GRD and V-GRD values"
  (let ((key (list (cycle-datestring cycle)
                   (cycle-run cycle)
                   offset
                   resolution)))
    (bordeaux-threads:with-lock-held (+noaa-forecast-ht-lock+)
      (or (gethash key *noaa-forecast-ht*)
          (setf (gethash key *noaa-forecast-ht*)
                (noaa-forecast% :cycle cycle :offset offset :resolution resolution))))))

(defun noaa-forecast% (&key (cycle 0) (offset 0) (resolution "1p00") (load-missing nil))
  (let ((filename (namestring (noaa-destpath :cycle cycle :offset offset :resolution resolution)))
        (index (codes-index-new '("step" "shortName"))))
    (when load-missing
      (setf filename
            (namestring (download-noaa-file cycle offset))))
    (log2:trace "Add file ~a~%" filename)
    (let ((retcode (codes-index-add-file index filename)))
      (case retcode
        (0)
        (-11
         (log2:warning "codes-index-add-file: ~a ~a" filename retcode)
         (error "File ~a not found" filename))
        (t
         (let ((message (codes-get-error-message retcode)))
           (log2:warning "codes-index-add-file: ~a: ~a" filename retcode)
           (error "eccodes: ~a: ~a" filename message))))
      (get-uv-steps-from-index index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cleanup forecast HT to avoid memory exhaustion

(defvar *forecast-cleanup-timer*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun noaa-start-forecast-ht-cleanup ()
  (log2:info "Starting forecast cleanup thread")
  (setf *forecast-cleanup-timer*
        (timers:add-timer #'noaa-forecast-ht-cleanup
                          :id "FORECAST-CLEANUP"
                          :hours '(5 11 17 23)
                          :minutes '(0))))

(defun noaa-forecast-ht-cleanup ()
  (let* ((expiry (adjust-timestamp (now) (:offset :day -2))))
    (bordeaux-threads:with-lock-held (+noaa-forecast-ht-lock+)
      (log2:info "Searching hash entries older than ~a" (format-timestring nil expiry))
      (maphash
       (lambda (k v)
         (declare (ignore v))
         (destructuring-bind (datestring run offset resolution)
             k
           (log2:info "Checking ~a-~a-~a ~a" datestring run offset resolution)
           (let ((time (parse-timestring (datestring-run-to-timestamp datestring run))))
             (when (timestamp<= time expiry)
               (log2:info "Removing ~a" k)
               (remhash k cl-weather::*noaa-forecast-ht*)))))
       *noaa-forecast-ht*))))
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
