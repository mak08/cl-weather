;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2023-02-19 17:42:52>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ================
;;; Periodic updates
;;; ================

(in-package "CL-WEATHER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(defvar *noaa-download-timer*)
(defvar *vr-download-timer*)
(defvar *cleanup-timer*)

(defun start-cycle-updates (&key (resolution '("1p00")) (max-offset 384))
  ;; Force download of previous cycle - even if the latest available cycle is complete,
  ;; previous cyclemay still be needed for interpolation.
  (download-cycle (previous-cycle (available-cycle (now))) :resolution resolution)
  (vr-download-cycle (previous-cycle (available-cycle (now))) :max-offset 288)
  ;; Download the latest complete cycle
  (let ((noaa-update
          (bordeaux-threads:make-thread
           (lambda ()
             (download-cycle (latest-complete-cycle) :resolution resolution :max-offset max-offset))
           :name "NOAA-UPDATE"))
        (vr-update
          (bordeaux-threads:make-thread
           (lambda ()
             (vr-download-cycle (latest-complete-cycle) :max-offset 288))
                   :name "VR-UPDATE")))
    (bordeaux-threads:join-thread noaa-update)
    (bordeaux-threads:join-thread vr-update))
  ;; When a cycle is currently being output, start download immediately
  (when (cycle-updating-p)
    (let ((noaa-update
            (bordeaux-threads:make-thread
             (lambda ()
               (download-cycle (current-cycle) :resolution resolution :max-offset max-offset :if-missing :wait))
             :name "NOAA-UPDATE"))
          (vr-update
            (bordeaux-threads:make-thread
             (lambda ()
               (vr-download-cycle (current-cycle) :max-offset 288 :if-missing :retry))
             :name "VR-UPDATE")))))
  ;; NOW we can leave it to the 4/24 update. 
  (setf *noaa-download-timer*
        (timers:add-timer (lambda ()
                            (download-cycle (current-cycle) :resolution resolution :max-offset max-offset :if-missing :wait))
                          :id (format nil "GFS-~a-UPDATE-NOAA" resolution)
                          :hours '(3 9 15 21)
                          :minutes '(30)))
  (setf *vr-download-timer*
        (timers:add-timer (lambda ()
                            (vr-download-cycle (current-cycle) :max-offset 288 :if-missing :retry))
                          :id (format nil "GFS-~a-UPDATE-VR" resolution)
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
