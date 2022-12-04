;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   Access to NOAA forecasts (non-interpolated)
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2022-11-30 22:44:54>

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load forecast

(defun noaa-forecast (&key (cycle (make-cycle)) (offset 0) (resolution "1p00"))
  "Read GRIB data into U and V arrays. Assumes the GRIB file contains U-GRD and V-GRD values"
  (let ((key (list (cycle-datestring cycle)
                   (cycle-run cycle)
                   offset
                   resolution)))
    (bordeaux-threads:with-lock-held (+noaa-forecast-ht-lock+)
      (or (gethash key *noaa-forecast-ht*)
          (setf (gethash key *noaa-forecast-ht*)
                (noaa-forecast% :cycle cycle :offset offset :resolution resolution))))))

(defun noaa-forecast% (&key (cycle (make-cycle)) (offset 0) (resolution "1p00"))
  (let ((filename (find-file-for-spec :cycle cycle :offset offset :resolution resolution))
        (index (codes-index-new '("step" "shortName"))))
    (log2:trace "Add file ~a~%" filename)
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

(defun find-file-for-spec (&key cycle offset resolution)
  (let ((dest-path (noaa-destpath :cycle cycle :offset offset :resolution resolution)))
    (if (probe-file dest-path)
        (namestring dest-path)
        (let ((archive-path  (noaa-archivepath :cycle cycle :offset offset :resolution resolution)))
          (if (probe-file archive-path)
              (namestring archive-path)
              (error 'missing-forecast :cycle cycle :offset offset :resolution resolution :filename dest-path))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get forecast collection for area

(defstruct fcdata north south west east start end increment cycle resolution values) 

#|
 (with-open-file (f "/home/michael/Repository/VirtualHelm/local/wind.json" :direction :output :if-exists :supersede)
  (json f (get-forecasts 90 0 0 90 :start 0 :end 15 :cycle (previous-cycle (available-cycle (now))))))
|#

(defun get-forecasts (north south west east &key (start 0) (end 384) (increment 1d0) (cycle (available-cycle (now))) (resolution "1p00"))
  (let* ((data (make-array (list (1+ (truncate (- end start) 3))
                                 (1+ (truncate (- north south) increment))
                                 (1+ (truncate (- east west) increment))
                                 2)
                           :element-type 'double-float))
         (result (make-fcdata :north north
                              :south south
                              :east east
                              :west west
                              :start start
                              :end end
                              :increment increment
                              :cycle (format-datetime nil (cycle-timestamp cycle))
                              :resolution resolution
                              :values data)))
    (loop
      :for offset :from start :to end :by 3
      :for ioffset :from 0
      :for dataset = (noaa-forecast :cycle cycle :offset offset :resolution resolution)
      :for forecast = (dataset-forecast dataset)
      :for info = (dataset-grib-info dataset)
      :do (loop
            :for lat :from north :downto south :by increment
            :for ilat :from 0
            :do (loop
                  :for lon :from west :to east :by increment
                  :for ilon :from 0
                  :for index = (uv-index info lat lon)
                  :for (u v) = (multiple-value-list (grib-get-uv forecast index))
            :do (setf (aref data ioffset ilat ilon 0) u
                      (aref data ioffset ilat ilon 1) v))))
    result))

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
    (bordeaux-threads:with-lock-held (+noaa-forecast-ht-lock+)
      (log2:info "Searching hash entries older than ~a" (format-timestring nil expiry))
      (maphash
       (lambda (k v)
         (declare (ignore v))
         (destructuring-bind (datestring run offset resolution)
             k
           (log2:info "Checking ~a-~a-~a ~a" datestring run offset resolution)
           (let ((time (parse-timestring (datestring-run-to-timestamp datestring run))))
             (when (timestamp< time expiry)
               (log2:info "Removing ~a" k)
               (remhash k cl-weather::*noaa-forecast-ht*)))))
       *noaa-forecast-ht*))))
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
