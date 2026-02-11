;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2026-02-11 22:28:59>

(defsystem "cl-weather"
  :description "Load and prepare weather data"
  :depends-on ("makros" "log2" "cl-eccodes" "cl-geomath" "local-time" "cl-map")
  :default-component-class cl-source-file.cl
  :serial t
  :components ((:file "package")
               (:file "common")
               (:file "macros")
               (:file "curl-wrapper")
               (:file "datatypes")
               (:file "gribfile")
               (:file "datasource")
               (:file "datasource-download")
               (:file "download")
               (:file "cycle-update")
               (:file "forecast")
               (:file "prediction")
               (:file "prediction-fw")
               (:file "tiles")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
