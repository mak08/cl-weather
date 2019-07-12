;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2019-07-05 23:17:11>

(defpackage "CL-WEATHER"
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "CFFI"
        "CL-GEOMATH"
        "CL-ECCODES"
        "LOCAL-TIME"
        "CL-MAP")
  (:export "*GRIB-DIRECTORY*"
           "NOAA-START-UPDATES"
           "LATEST-COMPLETE-CYCLE"
           "CURRENT-CYCLE"
           "CYCLE-UPDATING-P"
           "DOWNLOAD-CYCLE-BACKTRACK"
           "DOWNLOAD-LATEST-CYCLE"
           "DOWNLOAD-CYCLE"
           "DATASET-BASETIME"
           "DATASET-CYCLE"
           "DATASET-TIME"
           "NOAA-FORECAST"
           "PREDICTION-PARAMETERS"
           "NOAA-PREDICTION%"
           "NOAA-PREDICTION"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
