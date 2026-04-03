;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   GRIB data sources
;;; Author        Michael Kappert 2019
;;; Last Modified <michael 2026-03-31 21:57:12>

(in-package "CL-WEATHER")

(defclass fw-datasource (datasource file-download)
  ())

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
