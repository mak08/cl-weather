;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2025-11-12 20:34:27>

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
           "*LOAD-PREVIOUS*"
           "*GENERATE-JPEG-COMPRESSED-GRIBS*"
           "START-CYCLE-UPDATES"
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
           "LOAD-FORECAST"

           "CYCLE"
           "CYCLE-TIMESTAMP"
           "MAKE-CYCLE"
           "CYCLE-DATESTRING"
           "CYCLE-STRING"
           "CYCLE-RUN"

           "GET-PARAMS"
           "PARAMS-FW-FC0"
           "PARAMS-FW-FC1"
           "PARAMS-FW-FRACTION"

           "TIMESPEC-TO-TIMESTAMP"
           "TIMESTAMP-TO-TIMESPEC"

           "INTERPOLATE-FW"

           "TILE-FILENAME"
           "CREATE-TILES"
           "CREATE-TILE"
           "GET-WIND-UV"

           "PACK"
           "UNPACK"
           ))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
