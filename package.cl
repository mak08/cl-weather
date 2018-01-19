;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-01-19 22:26:43>

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
   "FCB-TIME"
   "FCB-MAX-OFFSET"
   "GET-FORECAST"
   "GET-WIND-FORECAST"
   "FC-TIME"
   "UPDATE-FORECAST-BUNDLE"

   "BILINEAR"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
