;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-12-28 11:59:19>

(defpackage "CL-WEATHER"
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "CFFI"
        "CL-GEOMATH"
        "CL-ECCODES"
        "LOCAL-TIME"
        "CL-MAP")
  (:export "*GRIB-FOLDER*"

           "CONSTANT-WIND-BUNDLE"
           "NOAA-BUNDLE"
           "DWD-ICON-BUNDLE"

           "GET-FORECAST-BUNDLE"
           "LOAD-FORECAST-BUNDLE"
           "UPDATE-FORECAST-BUNDLE"

           "FCB-TIME"
           "FCB-MAX-OFFSET"
           "GET-FORECAST"
           "GET-WIND-FORECAST"
           "FC-TIME"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
