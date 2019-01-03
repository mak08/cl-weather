;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2019-01-02 21:41:22>

(defpackage "CL-WEATHER"
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "CFFI"
        "CL-GEOMATH"
        "CL-ECCODES"
        "LOCAL-TIME"
        "CL-MAP")
  (:export "*GRIB-FOLDER*"

           "NOAA-DATASET"
           "DWD-DATASET"
           "CONSTANT-DATASET"
           
           "GET-DATASET"
           "LOAD-DATASET"
           "UPDATE-DATASET"

           "DATASET-TIME"
           "DATASET-CYCLE"
           "DATASET-MAX-OFFSET"
           
           "GET-FORECAST"
           "GET-WIND-FORECAST"
           "FC-TIME"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
