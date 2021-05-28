;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Access to NOAA forecasts (non-interpolated)
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-05-28 19:11:12>

(in-package "CL-WEATHER")

(defun noaa-forecast (&key (cycle 0) (offset 0))
  "Read GRIB data into U and V arrays. Assumes the GRIB file contains U-GRD and V-GRD values"
  (let ((key (list (cycle-datestring cycle)
                   (cycle-run cycle)
                   offset)))
    (bordeaux-threads:with-lock-held (+noaa-forecast-ht-lock+)
      (or (gethash key *noaa-forecast-ht*)
          (setf (gethash key *noaa-forecast-ht*)
                (noaa-forecast% :cycle cycle :offset offset))))))

(defun noaa-forecast% (&key (cycle 0) (offset 0) (load-missing nil))
  (let ((filename (namestring (noaa-destpath :cycle cycle :offset offset)))
        (index (codes-index-new '("step" "shortName"))))
    (when load-missing
      (setf filename
            (namestring (download-noaa-file cycle offset))))
    (log2:trace "Add file ~a~%" filename)
    (let ((retcode (codes-index-add-file index filename)))
      (case retcode
        (0)
        (-11
         (log2:warning "codes-index-add-file: -11")
         (error "File ~a not found" filename))
        (t
         (let ((message (codes-get-error-message retcode)))
           (log2:warning "codes-index-add-file: ~a: ~a" filename retcode)
           (error "eccodes: ~a: ~a" filename message))))
      (get-uv-steps-from-index index))))

(defun noaa-forecast-ht-cleanup ()
  (let* ((expiry (adjust-timestamp (now) (:offset :day -2))))
    (bordeaux-threads:with-lock-held (+noaa-forecast-ht-lock+)
      (log2:info "Removing hash entries older than ~a" (format-timestring nil expiry))
      (maphash
       (lambda (k v)
         (declare (ignore v))
         (destructuring-bind (datestring run offset)
             k
           (log2:info "Checking ~a-~a-~a" datestring run offset)
           (let ((time (parse-timestring (datestring-run-to-timestamp datestring run))))
             (when (timestamp<= time expiry)
               (log2:info "Removing ~a-~a-~a" datestring run offset)
               (remhash k cl-weather::*noaa-forecast-ht*)))))
       *noaa-forecast-ht*))))
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
