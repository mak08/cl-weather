;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2024-05-04 14:40:29>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ================
;;; Periodic updates
;;; ================

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defvar *noaa-download-timer*)
(defvar *cleanup-timer*)

(defun start-cycle-updates (&key (resolution '("1p00")) (load-previous *load-previous*) (max-offset 384))
  (when load-previous
    ;; Force download of previous cycle - even if the latest available cycle is complete,
    ;; previous cycle may still be needed for interpolation.
    (let ((noaa-update
            (bordeaux-threads:make-thread
             (lambda ()
               (download-cycle (previous-cycle (available-cycle (now))) :max-offset max-offset :resolution resolution))
             :name "NOAA-UPDATE")))
      (bordeaux-threads:join-thread noaa-update)))
  
  ;; Download the latest complete cycle
  (let ((noaa-update
          (bordeaux-threads:make-thread
           (lambda ()
             (download-cycle (latest-complete-cycle) :max-offset max-offset :resolution resolution))
           :name "NOAA-UPDATE")))
    (bordeaux-threads:join-thread noaa-update))
  ;; When a cycle is currently being output, start download immediately
  (when (cycle-updating-p)
    (let ((noaa-update
            (bordeaux-threads:make-thread
             (lambda ()
               (download-cycle (current-cycle) :resolution resolution :max-offset max-offset :if-missing :wait))
             :name "NOAA-UPDATE")))))
  ;; NOW we can leave it to the 4/24 update. 
  (setf *noaa-download-timer*
        (timers:add-timer (lambda ()
                            (download-cycle (current-cycle) :resolution resolution :max-offset max-offset :if-missing :wait))
                          :id (format nil "GFS-~a-UPDATE-NOAA" resolution)
                          :hours '(3 9 15 21)
                          :minutes '(30)))
  (setf *cleanup-timer*
        (timers:add-timer (lambda ()
                            (cleanup-cycles :dry-run nil))
                          :id (format nil "CLEANUP-CYCLES" resolution)
                          :hours '(3 9 15 21)
                          :minutes '(10)))
  (noaa-start-forecast-ht-cleanup))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
