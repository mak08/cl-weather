;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2021-05-26 21:14:42>

(defpackage "CL-WEATHER"
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "CFFI"
        "MACROS"
        "CL-GEOMATH"
        "CL-ECCODES"
        "LOCAL-TIME"
        "CL-MAP")
  (:export "*GRIB-DIRECTORY*"
           "*USE-RANGE-QUERY*"
           "*INTERPOLATION*"
           "*MERGE-START*"
           "*MERGE-WINDOW*"
           "NOAA-START-UPDATES"
           "LATEST-COMPLETE-CYCLE"
           "AVAILABLE-CYCLE"
           "CURRENT-CYCLE"
           "CYCLE-UPDATING-P"
           "PREVIOUS-CYCLE"
           "DOWNLOAD-LATEST-CYCLE"
           "DOWNLOAD-CYCLE"
           "DATASET-BASETIME"
           "DATASET-CYCLE"
           "DATASET-TIME"
           "NOAA-FORECAST"

           "CYCLE"
           "CYCLE-TIMESTAMP"
           "MAKE-CYCLE"
           "CYCLE-DATE"
           "CYCLE-HOUR"
           "PREDICTION-PARAMETERS"
           "PARAMS-BASE-TIME"
           "PARAMS-TIMESTAMP"
           "VR-PARAMETERS"
           "VR-ESTIMATOR"
           "MAKE-IPARAMS"
           "IPARAMS-CURRENT"
           "BASE-TIME"

           "TIMESPEC-TO-TIMESTAMP"
           "TIMESTAMP-TO-TIMESPEC"
           
           "NOAA-PREDICTION%"
           "NOAA-PREDICTION"
           "VR-PREDICTION%"
           "VR-PREDICTION"
           "INTERPOLATION-PARAMETERS"
           "INTERPOLATED-PREDICTION"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
