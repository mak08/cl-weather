;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2021-07-04 01:13:43>

(in-package "CL-WEATHER")

(defun current-cycle ()
  ;; The next cycle becomes available (gradually) starting about 3:30h
  ;; after the forecast computation starts.
  (make-cycle :timestamp (adjust-timestamp (now) (offset :minute (- 210)))))

(defun available-cycle (timestamp)
  ;; If $timestamp is in the future:
  ;;   If the forecasts already available from the latest cycle  cover $timestamp,
  ;;   return the current cycle, otherwise retunr the previous cycle.
  ;; If $timestamp is in the past, return the latest cycle that included it.
  (let* ((now (now))
         (diff (timestamp-difference timestamp now))
         (avail-time (adjust-timestamp (if (< diff 0) timestamp now)
                       (offset :minute (- 210))))
         (cycle (make-cycle :timestamp avail-time))
         (elapsed (truncate (timestamp-difference (now)
                                                  (cycle-timestamp cycle)) 60))
         (forecast (cycle-forecast cycle timestamp))
         (available (> elapsed (+ 225 (truncate forecast 3)))))
    (log2:trace "Timestamp: ~a => ~a ~a" timestamp available cycle)
    (if available
        (values cycle)
        (previous-cycle cycle))))

(defun previous-cycle (cycle)
  (let ((timestamp (cycle-timestamp cycle)))
    (make-cycle :timestamp (adjust-timestamp timestamp (offset :hour -6)))))

(defun latest-complete-cycle (&optional (time (now)))
  ;; Determine the latest cycle that should'be complete (theoretically) at the given time
  (make-cycle :timestamp (adjust-timestamp time (offset :minute (- 300)))))

(defun cycle-updating-p (&optional (time (now)))
  (< 210 (mod (day-minute time) 360) 300))

(defun day-minute (&optional (time (now)))
  (+ (*
      (timestamp-hour time :timezone +utc-zone+)
      60)
     (timestamp-minute time :timezone +utc-zone+)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Forecasts


(defparameter +noaa-forecast-offsets+
  #(0   3   6   9   12  15  18  21  24
    27  30  33  36  39  42  45  48  51   54
    57  60  63  66  69  72  75  78  81   84
    87  90  93  96  99  102 105 108 111 114
    117 120 123 126 129 132 135 138 141 144
    147 150 153 156 159 162 165 168 171 174
    177 180 183 186 189 192 195 198 201 204
    207 210 213 216 219 222 225 228 231 234
    237 240 243 246 249 252 255 258 261 264
    267 270 273 276 279 282 285 288 291 294
    297 300 303 306 309 312 315 318 321 324
    327 330 333 336 339 342 345 348 351 354
    357 360 363 366 369 372 375 378 381 384))

;; Return the 3-hour-forecast required for $timestamp when using $cycle
(defun cycle-forecast (cycle timestamp)
  (let* ((basetime (cycle-as-timestamp cycle)) 
         (difference (truncate
                      (timestamp-difference timestamp basetime)
                      3600)))
    (cond
      ((minusp difference)
       (error "~a is in the past of cycle ~a" timestamp cycle))
      ((<= difference 384)
       (* 3 (truncate difference 3)))
      (t
       (log2:warning "~a is in the future of cycle ~a" timestamp cycle)
       384))))

;; Return the next 3-hour-forecast
(defun next-forecast (forecast)
  (assert (or (= forecast 0)
              (find forecast +noaa-forecast-offsets+ :test #'eql)))
  (min (+ forecast 3) 384))
  
;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
