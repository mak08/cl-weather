;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-10-17 21:15:47>

(defsystem "cl-weather"
  :description "Load and prepare weather data"
  :depends-on ("log2" "cl-eccodes" "cl-utilities" "local-time" "cl-map")
  :default-component-class cl-source-file.cl
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "gribfile")
               (:file "datatypes")
               (:file "meteodata")
               ;; (:file "meteodata-dwd")
               (:file "meteodata-noaa")
               (:file "forecast-access")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
