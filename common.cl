;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2024-05-04 14:41:00>

(in-package "CL-WEATHER")
(setf (log2:log-level "cl-weather") log2:+info+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common in all modules

(defparameter *source-root*
  (make-pathname :directory (pathname-directory #.*compile-file-truename*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific  to this module

(defvar *grib-directory*
  (merge-pathnames (make-pathname :directory '(:relative "weather" "noaa")) *source-root*))

(defvar *vr-grib-directory*
  (merge-pathnames (make-pathname :directory '(:relative "weather" "vr")) *source-root*))

(defvar  *load-previous* nil)

(defvar *generate-jpeg-compressed-gribs* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forecast hashtable

(defvar +forecast-ht-lock+
  (bordeaux-threads:make-lock "noaa-forecast-ht"))

(defvar *forecast-ht*
  (make-hash-table :test #'equalp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

(defun format-timestamp-as-cycle (stream timestamp)
  (format-timestring nil timestamp
                     :format '((:day 2) "T" (:hour 2))
                     :timezone +utc-zone+ ))

(defun format-yyyymmdd (stream timestamp)
  (format-timestring stream timestamp
                     :format '((:year 4) (:month 2) (:day 2))
                     :timezone +utc-zone+))

(defun format-ddhhmm (stream timestamp)
  (format-timestring stream timestamp
                     :format '((:day 2)"T" (:hour 2) ":" (:min 2))
                     :timezone +utc-zone+))

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
