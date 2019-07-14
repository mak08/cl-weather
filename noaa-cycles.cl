;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2019-07-14 00:20:33>

(in-package "CL-WEATHER")

(defun current-cycle ()
  ;; The next cycle becomes available about 3:30h after the forecast computation starts.
  (let* ((avail-time (adjust-timestamp (now) (offset :minute (- 210))))
         (date (format-timestring nil avail-time :format '((:year 4) (:month 2) (:day 2))))
         (cycle (* 6 (truncate (timestamp-hour avail-time :timezone +utc-zone+) 6))))
    (values date cycle)))

(defun available-cycle (timestamp)
  ;; The next cycle becomes available about 3:30h after the forecast computation starts.
  (let* ((avail-time (adjust-timestamp (now) (offset :minute (- 210))))
         (date (format-timestring nil avail-time :format '((:year 4) (:month 2) (:day 2))))
         (cycle (* 6 (truncate (timestamp-hour avail-time :timezone +utc-zone+) 6)))
         (forecast (cycle-forecast date cycle timestamp))
         (elapsed (truncate (timestamp-difference (now)
                                                  (timespec-to-timestamp date cycle)) 60))
         (available (> elapsed (+ 210 (truncate forecast 3)))))
    (if available
        (values date cycle)
        (previous-cycle date cycle))))

(defun previous-cycle (date cycle)
  (let ((timestamp (timespec-to-timestamp  date cycle)))
    (timestamp-to-timespec (adjust-timestamp timestamp (offset :hour -6)))))

(defun latest-complete-cycle (&optional (time (now)))
  ;; Determine the latest cycle that should'be complete (theoretically) at the given time
  (let* ((cycle-start-time 
          (timestamp-minimize-part (adjust-timestamp time (offset :minute (- 300)))
                                   :min
                                   :timezone +utc-zone+))
         (date (format-timestring nil cycle-start-time :format '((:year 4) (:month 2) (:day 2))))
         (cycle (* 6 (truncate (timestamp-hour cycle-start-time :timezone +utc-zone+) 6))))
    (values date
            cycle
            (timespec-to-timestamp date cycle))))

(defun cycle-updating-p (&optional (time (now)))
  (< 210 (mod (day-minute time) 360) 300))

(defun day-minute (&optional (time (now)))
  (+ (*
      (timestamp-hour time :timezone +utc-zone+)
      60)
     (timestamp-minute time :timezone +utc-zone+)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forecasts


(defvar +noaa-forecast-offsets+
  #(0 3 6 9 12 15 18 21 24
    27 30 33 36 39 42 45 48 51
    54 57 60 63 66 69 72 75 78 81
    84 87 90 93 96 99 102 105 108 111
    114 117 120 123 126 129 132 135 138 141
    144 147 150 153 156 159 162 165 168 171
    174 177 180 183 186 189 192 195 198 201
    204 207 210 213 216 219 222 225 228 231
    234 237 240 252 264 276 288 300 312 324
    336 348 360 372 384))

(defun cycle-forecast (date cycle timestamp)
  (ecase cycle ((0 6 12 18)))
  (let* ((basetime (timespec-to-timestamp date cycle)) 
         (difference (timestamp-difference timestamp basetime)))
    (when (minusp difference)
      (error "~a is in the past of ~a" timestamp basetime))
    (setf difference (truncate difference 3600))
    (cond
      ((<= 0 difference 239)
       (* 3 (truncate difference 3)))
      ((<= 240 difference 384)
       (* 12 (truncate difference 12)))
      (t
       (error "~a is in the future of cycle ~a/~a" timestamp date cycle)))))

(defun next-forecast (forecast)
  (assert (find forecast +noaa-forecast-offsets+ :test #'eql))
  (cond
    ((<= forecast 237)
     (+ forecast 3))
    ((<= forecast 372)
     (+ forecast 12))
    ((= forecast 384)
     384)))
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
