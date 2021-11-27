;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-11-01 20:33:26>

(in-package "CL-WEATHER")
(setf (log2:log-level "cl-weather") log2:+info+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common in all modules

(defparameter *source-root*
  (make-pathname :directory (pathname-directory #.*compile-file-truename*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific  to this module

(defvar *grib-directory*
  (merge-pathnames (make-pathname :directory '(:relative "gribfiles")) *source-root*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forecast hashtable

(defvar +noaa-forecast-ht-lock+
  (bordeaux-threads:make-lock "noaa-forecast-ht"))

(defvar *noaa-forecast-ht*
  (make-hash-table :test #'equalp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Merge new and old if we are between 4-6.5h into the new forecast
;; (0.5-3.0h of the cycle)

(defvar *interpolation* :enorm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defun format-datetime (stream timestamp &key (timezone +utc-zone+))
  (format stream "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
          (timestamp-year timestamp :timezone timezone)
          (timestamp-month timestamp :timezone timezone)
          (timestamp-day timestamp :timezone timezone)
          (timestamp-hour timestamp :timezone timezone)
          (timestamp-minute timestamp :timezone timezone)
          (timestamp-second timestamp :timezone timezone)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
