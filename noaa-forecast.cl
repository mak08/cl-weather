;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Access to NOAA forecasts (non-interpolated)
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-05-23 20:44:42>

(in-package "CL-WEATHER")


(defvar +noaa-forecast-ht-lock+
  (bordeaux-threads:make-lock "noaa-forecast-ht"))

(defvar *noaa-forecast-ht*
  (make-hash-table :test #'equalp))

(defun noaa-forecast (date &key (cycle 0) (offset 0))
  "Read GRIB data into U and V arrays. Assumes the GRIB file contains U-GRD and V-GRD values"
  (let ((key (list date cycle offset)))
    (bordeaux-threads:with-lock-held (+noaa-forecast-ht-lock+)
      (or (gethash key *noaa-forecast-ht*)
          (setf (gethash key *noaa-forecast-ht*)
                (noaa-forecast% date :cycle cycle :offset offset))))))

(defun noaa-forecast% (date &key (cycle 0) (offset 0) (load-missing nil))
  (let ((filename (namestring (noaa-destpath date :cycle cycle :offset offset)))
        (index (codes-index-new '("step" "shortName"))))
    (when load-missing
      (setf filename
            (namestring (download-noaa-file date cycle offset))))
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
      (log2:info "Removing entries older than ~a" (format-timestring nil expiry))
      (maphash
       (lambda (k v)
         (destructuring-bind (date cycle forecast)
             k
           (let ((time (timespec-to-timestamp date cycle)))
             (when (timestamp<= time expiry)
               (remhash k cl-weather::*noaa-forecast-ht*)))))
       cl-weather::*noaa-forecast-ht*))))
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
