;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2019-06-26 22:49:27>

(defsystem "cl-weather"
  :description "Load and prepare weather data"
  :depends-on ("log2" "cl-eccodes" "cl-geomath" "local-time" "cl-map")
  :default-component-class cl-source-file.cl
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "macros")
               (:file "gribfile")
               (:file "datatypes")
               (:file "noaa-cycles")
               (:file "noaa-download")
               (:file "noaa-update")
               (:file "noaa-forecast")
               (:file "noaa-prediction")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
