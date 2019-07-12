;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Access to NOAA forecasts (non-interpolated)
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2019-07-12 21:29:18>

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
    (log2:trace "Add file ~a~%" filename)
    (when load-missing
      (download-noaa-file date cycle offset))
    (ecase (codes-index-add-file index filename)
      (0)
      (-1
       (log2:warning "codes-index-add-file: -1"))
      (-11
       (log2:warning "codes-index-add-file: -11")
       (error "File ~a not found" filename)))
    (get-uv-steps-from-index index)))
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
