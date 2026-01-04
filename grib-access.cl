;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Access to NOAA forecasts (non-interpolated)
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2025-12-30 21:54:19>

(in-package "CL-WEATHER")


;;; Revised GRIB loading
;;;
;;; LOADING a grib file
;;; - Support loading pairs of u/v latlon grids
;;; - u/v pairs to load must be provided
;;; - File can contain multiple steps
;;; - Each step is cached separately (disuse datased comprising multiple steps)


(defun grib-index (filename)
  (let ((index (codes-index-new '("step" "shortName" "parameterName"))))
    (log2:info "Loading forecast ~a~%" filename)
    (let ((retcode (codes-index-add-file index filename)))
      (case retcode
        (0)
        (-11
         (log2:warning "codes-index-add-file: ~a ~a" filename retcode)
         (error "File not found: ~a" filename))
        (t
         (let ((message (codes-get-error-message retcode)))
           (log2:warning "codes-index-add-file: ~a: ~a" filename retcode)
           (error "eccodes: ~a: ~a" filename message))))
      (get-messages-from-index index))))

(defun get-messages-from-index (index)
  (let* ((steps 
           (codes-index-get-long index "step"))
         (shortNames
           (codes-index-get-string index "shortName"))
         (parameterNames
           (codes-index-get-string index "parameterName")))
    (log2:info "Index steps (~a): ~a" (length steps) steps)
    (log2:info "Index shortNames (~a): ~a" (length shortNames) shortNames)
    (log2:info "Index parameterNames (~a): ~a" (length parameterNames) parameterNames)))

(defun load-grib-file (filename)
  (let ((index (codes-index-new '("step" "shortName"))))
    (log2:info "Loading forecast ~a~%" filename)
    (let ((retcode (codes-index-add-file index filename)))
      (case retcode
        (0)
        (-11
         (log2:warning "codes-index-add-file: ~a ~a" filename retcode)
         (error 'missing-forecast))
        (t
         (let ((message (codes-get-error-message retcode)))
           (log2:warning "codes-index-add-file: ~a: ~a" filename retcode)
           (error "eccodes: ~a: ~a" filename message))))
      (get-uv-steps-from-index index))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
