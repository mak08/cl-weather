;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2026-04-03 11:56:37>

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
               (:file "fw-datasource")
               (:file "fw-currents")
               (:file "fw-waves")
               (:file "datasource-download")
               (:file "download")
               (:file "cycle-update")
               (:file "forecast")
               (:file "interpolation-params")
               (:file "interpolation-simple")
               (:file "interpolation-uv")
               (:file "tiles")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
