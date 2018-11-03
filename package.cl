;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-09-19 15:31:32>

(defpackage "CL-WEATHER"
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "CL-ECCODES"
        "LOCAL-TIME"
        "CFFI"
        "CL-MAP")
  (:export
   "*GRIB-FOLDER*"
   "CONSTANT-WIND-BUNDLE"
   "NOAA-BUNDLE"
   "DWD-ICON-BUNDLE"
   "GET-FORECAST-BUNDLE"
   "LOAD-FORECAST-BUNDLE"
   "FCB-TIME"
   "FCB-MAX-OFFSET"
   "GET-FORECAST"
   "GET-WIND-FORECAST"
   "FC-TIME"
   "UPDATE-FORECAST-BUNDLE"

   "BILINEAR"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
