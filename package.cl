;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2020-10-25 16:27:21>

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
           "AVAILABLE-CYCLE"
           "CURRENT-CYCLE"
           "CYCLE-UPDATING-P"
           "PREVIOUS-CYCLE"
           "DOWNLOAD-CYCLE-BACKTRACK"
           "DOWNLOAD-LATEST-CYCLE"
           "DOWNLOAD-CYCLE"
           "DATASET-BASETIME"
           "DATASET-CYCLE"
           "DATASET-TIME"
           "NOAA-FORECAST"

           ;; This is a hack - Need to cleanup handling of prediction base time
           "CYCLE"
           "CYCLE-DATE"
           "CYCLE-HOUR"
           "PREDICTION-PARAMETERS"
           "PARAMS-BASE-TIME"
           "PARAMS-TIMESTAMP"
           "VR-PARAMETERS"
           "VR-ESTIMATOR"
           "MAKE-IPARAMS"
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
