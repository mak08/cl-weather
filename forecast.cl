;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Access to NOAA forecasts (non-interpolated)
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2026-01-04 12:28:11>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading forecast data

(defun load-forecast (&key (source :noaa)
                        (u-and-v-var '("10u" "10v"))
                        (cycle (make-cycle))
                        (offset 0)
                        (resolution "1p00"))
  "Return requested U/V data from cache.
Read from file if not cached.
Filename is determined by FIND-FILE-FOR-SPEC."
  (let ((key (list source
                   (cycle-datestring cycle)
                   (cycle-run cycle)
                   offset
                   resolution)))
    (bordeaux-threads:with-lock-held (+forecast-ht-lock+)
      (or (gethash key *forecast-ht*)
          (setf (gethash key *forecast-ht*)
                (load-forecast% :source source :u-and-v-var u-and-v-var :cycle cycle :offset offset :resolution resolution))))))

(defun load-forecast% (&key (source :noaa)
                         (u-and-v-var '("10u" "10v"))
                         (cycle (make-cycle))
                         (offset 0)
                         (resolution "1p00"))
  (let ((filename (find-file-for-spec :source source :cycle cycle :offset offset :resolution resolution)))
    (with-grib-index (index '("step" "shortName"))
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
        (get-uv-steps-from-index index u-and-v-var)))))

(defun find-file-for-spec (&key source cycle offset resolution)
  (let ((dest-path (forecast-destpath :source source :cycle cycle :offset offset :resolution resolution)))
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
                          :minutes '(10))))

(defun noaa-forecast-ht-cleanup ()
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
