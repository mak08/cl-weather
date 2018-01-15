;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2018-01-13 23:27:47>

(defpackage "CL-WEATHER"
  (:use "COMMON-LISP"
        #+sbcl "SB-MOP"
        "CL-ECCODES"
        "LOCAL-TIME"
        "CFFI")
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

   "LATLNG"
   "MAKE-LATLNG"
   "COPY-LATLNG"
   "LATLNG-LATR"
   "LATLNG-LNGR"
   "LATLNG-LAT"
   "LATLNG-LNG"

   "BILINEAR"
   "RAD"
   "ANGLE"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
