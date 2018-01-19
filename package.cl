;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-01-18 19:42:00>

(defpackage "CL-WEATHER"
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "CL-ECCODES"
        "LOCAL-TIME"
        "CFFI"
        "CL-MAP")
  (:export
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
