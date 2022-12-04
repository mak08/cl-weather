;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2022-11-28 23:52:32>

(defpackage "CL-WEATHER"
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "CFFI"
        "MACROS"
        "CL-GEOMATH"
        "CL-ECCODES"
        "LOCAL-TIME"
        "CL-MAP")
  (:export "*NOAA-GFS-PATH*"
           "+NCEP-NOMADS+"
           "+NCEP-FTPPRD+"
           "*GRIB-DIRECTORY*"
           "*USE-RANGE-QUERY*"
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
           "CLEANUP-CYCLES"
           "DATASET-BASETIME"
           "DATASET-CYCLE"
           "DATASET-TIME"
           "NOAA-FORECAST"

           "CYCLE"
           "CYCLE-TIMESTAMP"
           "MAKE-CYCLE"
           "CYCLE-DATESTRING"
           "CYCLE-STRING"
           "CYCLE-RUN"
           "PREDICTION-PARAMETERS"
           "PARAMS-BASE-TIME"
           "PARAMS-TIMESTAMP"
           "VR-PARAMETERS"
           "VR-ESTIMATOR"
           "MAKE-IPARAMS"
           "IPARAMS-CURRENT"
           "BASE-TIME"
           "IPARAMS-EFFECTIVE-CYCLE"
           
           "TIMESPEC-TO-TIMESTAMP"
           "TIMESTAMP-TO-TIMESPEC"
           
           "NOAA-PREDICTION%"
           "NOAA-PREDICTION"
           "VR-PREDICTION%"
           "VR-PREDICTION"
           "INTERPOLATION-PARAMETERS"
           "INTERPOLATED-PREDICTION"

           "TILE-FILENAME"
           "CREATE-TILES"
           "CREATE-TILE"
           "GET-WIND-UV"

           "PACK"
           "UNPACK"
           ))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
