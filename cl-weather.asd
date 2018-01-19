;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-01-18 19:37:14>

(defsystem "cl-weather"
  :description "Load and prepare weather data"
  :depends-on ("log2" "cl-eccodes" "cl-utilities" "local-time" "cl-map")
  :default-component-class cl-source-file.cl
  :components ((:file "package")
               (:file "macros")
               (:file "gribfile")
               (:file "datatypes")
               (:file "meteodata")
               (:file "meteodata-dwd")
               (:file "meteodata-noaa")
               (:file "forecast-access")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
