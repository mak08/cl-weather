;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Access to NOAA forecasts (non-interpolated)
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2020-11-13 23:41:50>

(in-package "CL-WEATHER")


(defvar *noaa-forecast-ht*
  (make-hash-table :test #'equalp))


(defun noaa-forecast (date &key (cycle 0) (offset 0))
  "Read GRIB data into U and V arrays. Assumes the GRIB file contains U-GRD and V-GRD values"
  (let ((key (list date cycle offset)))
    (or (gethash key *noaa-forecast-ht*)
        (setf (gethash key *noaa-forecast-ht*)
              (noaa-forecast% date :cycle cycle :offset offset)))))

(defun noaa-forecast% (date &key (cycle 0) (offset 0) (load-missing t))
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
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
