;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2023-02-18 23:26:52>

(defsystem "cl-weather"
  :description "Load and prepare weather data"
  :depends-on ("makros" "log2" "cl-eccodes" "cl-geomath" "local-time" "cl-map")
  :default-component-class cl-source-file.cl
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "macros")
               (:file "datatypes")
               (:file "gribfile")
               (:file "noaa-cycles")
               (:file "noaa-download")
               (:file "vr-download")
               (:file "cycle-update")
               (:file "forecast")
               (:file "prediction")
               (:file "tiles")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
