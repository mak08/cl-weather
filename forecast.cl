;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Access to NOAA forecasts (non-interpolated)
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2024-01-28 22:40:44>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load forecast

(defun load-forecast (&key (source :noaa) (cycle (make-cycle)) (offset 0) (resolution "1p00"))
  "Read GRIB data into U and V arrays. Assumes the GRIB file contains U-GRD and V-GRD values"
  (let ((key (list source
                   (cycle-datestring cycle)
                   (cycle-run cycle)
                   offset
                   resolution)))
    (bordeaux-threads:with-lock-held (+forecast-ht-lock+)
      (or (gethash key *forecast-ht*)
          (setf (gethash key *forecast-ht*)
                (load-forecast% :source source :cycle cycle :offset offset :resolution resolution))))))

(defun load-forecast% (&key (source :noaa) (cycle (make-cycle)) (offset 0) (resolution "1p00"))
  (let ((filename (find-file-for-spec :source source :cycle cycle :offset offset :resolution resolution))
        (index (codes-index-new '("step" "shortName"))))
    (log2:info "Loading forecast ~a~%" filename)
    (let ((retcode (codes-index-add-file index filename)))
      (case retcode
        (0)
        (-11
         (log2:warning "codes-index-add-file: ~a ~a" filename retcode)
         (error 'missing-forecast :cycle cycle :offset offset :resolution resolution :filename filename))
        (t
         (let ((message (codes-get-error-message retcode)))
           (log2:warning "codes-index-add-file: ~a: ~a" filename retcode)
           (error "eccodes: ~a: ~a" filename message))))
      (get-uv-steps-from-index index))))

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

(defun find-file-for-spec (&key source cycle offset resolution)
  (let ((dest-path (ecase source
                     (:noaa (noaa-destpath :cycle cycle :offset offset :resolution resolution))
                     (:vr  (vr-destpath :cycle cycle :offset offset :resolution resolution)))))
    (if (probe-file dest-path)
        (namestring dest-path)
        (let ((archive-path  (noaa-archivepath :cycle cycle :offset offset :resolution resolution)))
          (if (probe-file archive-path)
              (namestring archive-path)
              (error 'missing-forecast :cycle cycle :offset offset :resolution resolution :filename dest-path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get forecast collection for area

(defstruct fcdata north south west east start end increment cycle resolution values) 

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
  (let* ((expiry (adjust-timestamp (now) (:offset :hour -18))))
    (bordeaux-threads:with-lock-held (+forecast-ht-lock+)
      (log2:info "Searching hash entries older than ~a" (format-timestring nil expiry))
      (maphash
       (lambda (k v)
         (declare (ignore v))
         (destructuring-bind (source datestring run offset resolution)
             k
           (log2:info "Checking ~a ~a-~a-~a ~a" source datestring run offset resolution)
           (let ((time (parse-timestring (datestring-run-to-timestamp datestring run))))
             (when (timestamp< time expiry)
               (log2:info "Removing ~a" k)
               (remhash k cl-weather::*forecast-ht*)))))
       *forecast-ht*))))
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
