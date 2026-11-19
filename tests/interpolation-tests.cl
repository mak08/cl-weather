;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deterministic tests for forecast selection and AROME interpolation.

(in-package "CL-WEATHER")

(fiveam:def-suite cl-weather-tests)
(fiveam:in-suite cl-weather-tests)

(defclass test-arome-wind (arome-wind)
  ())

(defmethod datasource-schedule ((datasource (eql 'test-arome-wind)))
  (declare (ignore datasource))
  (make-schedule :runs '(0 6 12 18)
                 :steps '(6 9 12 15 18 21 24 27 30 33 36 39 42 45 48)))

(defvar *test-forecast-requests* nil)

(defmethod load-forecast ((datasource test-arome-wind) step)
  (push (list step (local-pathname datasource step :relative t))
        *test-forecast-requests*)
  (let* ((cycle-time (cycle-timestamp (cycle datasource)))
         (info (make-gribinfo :grid-size 4
                              :lat-start 1d0
                              :lat-end 0d0
                              :lat-points 2
                              :lon-start 359d0
                              :lon-end 0d0
                              :lon-points 2
                              :i-inc 1d0
                              :j-inc 1d0
                              :i-scan-neg 0
                              :j-scan-pos 0)))
    (make-uv :info info
             :source "arome"
             :basetime cycle-time
             :offset (* step 60)
             :step step
             :u-array (make-array 4 :element-type 'double-float
                                  :initial-element (float step 1d0))
             :v-array (make-array 4 :element-type 'double-float
                                  :initial-element 0d0))))

(defun test-cycle (timestamp)
  (make-cycle :timestamp (parse-timestring timestamp)))

(defun test-params-for (cycle timestamp)
  (let ((*test-forecast-requests* nil))
    (values (get-params-for-cycle 'test-arome-wind
                                  (parse-timestring timestamp)
                                  :cycle cycle)
            (nreverse *test-forecast-requests*))))

(defun expected-arome-path (date run)
  (make-pathname :directory (list :relative
                                 "arome"
                                 "western_med"
                                 date
                                 run)
                 :name (format nil "arome.t~az.western_med.f006-f048" run)
                 :type "grib2"))

(fiveam:test forecast-selection-at-temporal-borders
  (let ((cycle (test-cycle "2026-09-24T12:00:00Z")))
    (multiple-value-bind (at-start requests)
        (test-params-for cycle "2026-09-24T18:00:00Z")
      (fiveam:is (= 0d0 (params-fraction at-start)))
      (fiveam:is (= 6 (uv-step (params-fc0 at-start))))
      (fiveam:is (= 9 (uv-step (params-fc1 at-start))))
      (fiveam:is (equal '(6 9) (mapcar #'first requests)))
      (fiveam:is (every (lambda (request)
                          (equal (second request)
                                 (expected-arome-path "20260924" "12")))
                        requests))
      (multiple-value-bind (direction speed u v)
          (interpolate-uv 0.5d0 359.5d0 at-start)
        (declare (ignore direction v))
        (fiveam:is (= 6d0 u))
        (fiveam:is (= 6d0 speed))))
    (multiple-value-bind (inside requests)
        (test-params-for cycle "2026-09-24T19:30:00Z")
      (fiveam:is (= 0.5d0 (params-fraction inside)))
      (fiveam:is (equal '(6 9) (mapcar #'first requests)))
      (fiveam:is (every (lambda (request)
                          (equal (second request)
                                 (expected-arome-path "20260924" "12")))
                        requests))
      (multiple-value-bind (direction speed u v)
          (interpolate-uv 0.5d0 359.5d0 inside)
        (declare (ignore direction v))
        (fiveam:is (= 7.5d0 u))
        (fiveam:is (= 7.5d0 speed))))
    (multiple-value-bind (at-next-step requests)
        (test-params-for cycle "2026-09-24T21:00:00Z")
      (fiveam:is (= 0d0 (params-fraction at-next-step)))
      (fiveam:is (= 9 (uv-step (params-fc0 at-next-step))))
      (fiveam:is (= 12 (uv-step (params-fc1 at-next-step))))
      (fiveam:is (equal '(9 12) (mapcar #'first requests))))
    (multiple-value-bind (at-end requests)
        (test-params-for cycle "2026-09-26T12:00:00Z")
      (fiveam:is (= 0d0 (params-fraction at-end)))
      (fiveam:is (= 48 (uv-step (params-fc0 at-end))))
      (fiveam:is (= 48 (uv-step (params-fc1 at-end))))
      (fiveam:is (equal '(48 48) (mapcar #'first requests)))
      (multiple-value-bind (direction speed u v)
          (interpolate-uv 0.5d0 359.5d0 at-end)
        (declare (ignore direction v))
        (fiveam:is (= 48d0 u))
        (fiveam:is (= 48d0 speed))))))

(fiveam:test forecast-path-follows-selected-cycle
  (let ((cycle (test-cycle "2026-09-24T18:00:00Z")))
    (multiple-value-bind (params requests)
        (test-params-for cycle "2026-09-25T00:00:00Z")
      (declare (ignore params))
      (fiveam:is (equal '(6 9) (mapcar #'first requests)))
      (fiveam:is (every (lambda (request)
                          (equal (second request)
                                 (expected-arome-path "20260924" "18")))
                        requests)))))

(fiveam:test ifs-datasource-info-uses-cycle-specific-schedule
  (let* ((short-cycle (test-cycle "2026-09-24T06:00:00Z"))
         (long-cycle (test-cycle "2026-09-24T12:00:00Z"))
         (long-datasource (get-datasource 'ecmwf-ifs-wind long-cycle))
         (short-info (datasource-info-for-cycle 'ecmwf-ifs-wind short-cycle))
         (long-info (datasource-info-for-cycle 'ecmwf-ifs-wind long-cycle))
         (short-paths (datasource-info-gribpaths short-info))
         (long-paths (datasource-info-gribpaths long-info)))
    (fiveam:is (= 49 (length short-paths)))
    (fiveam:is (every (lambda (path) (<= (grib-spec-step path) 144))
                      short-paths))
    (fiveam:is (equal "ifs/0p25/20260924/06/ifs.0p25.20260924.t06z.f000.grib2"
                      (namestring (grib-spec-path (first short-paths)))))
    (fiveam:is (= 85 (length long-paths)))
    (fiveam:is (= 360 (grib-spec-step (car (last long-paths)))))
    (fiveam:is (equal "ifs/0p25/20260924/12/ifs.0p25.20260924.t12z.f360.grib2"
                      (namestring (grib-spec-path (car (last long-paths))))))
    (fiveam:is (equal "https://data.ecmwf.int/forecasts/20260924/12z/ifs/0p25/oper/20260924120000-0h-oper-fc.grib2"
                      (data-uri long-datasource 0)))
    (fiveam:is (equal "https://data.ecmwf.int/forecasts/20260924/12z/ifs/0p25/oper/20260924120000-0h-oper-fc.index"
                      (index-uri long-datasource 0)))))

(fiveam:test ifs-schedules-match-dissemination-windows
  (let* ((long-schedule (datasource-schedule-for-cycle
                         'ecmwf-ifs-wind
                         (test-cycle "2026-09-24T12:00:00Z")))
         (short-schedule (datasource-schedule-for-cycle
                          'ecmwf-ifs-wind
                          (test-cycle "2026-09-24T18:00:00Z"))))
    (fiveam:is (= 345 (schedule-dissem-start long-schedule)))
    ;; Observed completion plus the configured 30-minute safety margin.
    (fiveam:is (= 484 (schedule-dissem-end long-schedule)))
    (fiveam:is (= 360 (car (last (schedule-steps long-schedule)))))
    (fiveam:is (= 345 (schedule-dissem-start short-schedule)))
    (fiveam:is (= 417 (schedule-dissem-end short-schedule)))
    (fiveam:is (= 144 (car (last (schedule-steps short-schedule)))))
    (fiveam:is (member 'ecmwf-ifs-wind *datasources*))))

(fiveam:test aifs-datasource-schedule-and-paths
  (let* ((cycle (test-cycle "2026-09-24T12:00:00Z"))
         (datasource (get-datasource 'ecmwf-aifs-wind cycle))
         (info (datasource-info-for-cycle 'ecmwf-aifs-wind cycle))
         (paths (datasource-info-gribpaths info)))
    (fiveam:is (= 61 (length paths)))
    (fiveam:is (= 360 (grib-spec-step (car (last paths)))))
    (fiveam:is (equal "aifs-single/0p25/20260924/12/aifs-single.0p25.20260924.t12z.f000.grib2"
                      (namestring (grib-spec-path (first paths)))))
    (fiveam:is (equal "https://data.ecmwf.int/forecasts/20260924/12z/aifs-single/0p25/oper/20260924120000-0h-oper-fc.grib2"
                      (data-uri datasource 0)))
    (fiveam:is (equal "https://data.ecmwf.int/forecasts/20260924/12z/aifs-single/0p25/oper/20260924120000-0h-oper-fc.index"
                      (index-uri datasource 0)))
    (fiveam:is (= 6 (next-forecast datasource 0)))
    (fiveam:is (= 24 (cycle-forecast datasource (parse-timestring "2026-09-25T13:00:00Z"))))
    (fiveam:is (member 'ecmwf-aifs-wind *datasources*))))

(fiveam:test aifs-run-specific-dissemination-selection
  ;; All AIFS runs now start at +5h45; completion includes the 30-minute margin.
  (let* ((before-06z (parse-timestring "2026-09-24T11:44:59Z"))
         (after-06z (parse-timestring "2026-09-24T11:45:00Z"))
         (before-12z-complete (parse-timestring "2026-09-24T20:03:59Z"))
         (after-12z-complete (parse-timestring "2026-09-24T20:04:00Z")))
    (fiveam:is (= 0 (cycle-run (latest-ecmwf-cycle
                                'ecmwf-aifs-wind before-06z :availability :start))))
    (fiveam:is (= 6 (cycle-run (latest-ecmwf-cycle
                                'ecmwf-aifs-wind after-06z :availability :start))))
    (fiveam:is (= 6 (cycle-run (latest-ecmwf-cycle
                                'ecmwf-aifs-wind before-12z-complete :availability :end))))
    (fiveam:is (= 12 (cycle-run (latest-ecmwf-cycle
                                 'ecmwf-aifs-wind after-12z-complete :availability :end))))))

(fiveam:test ecmwf-cycle-selection-at-every-scheduled-boundary
  ;; Compare full timestamps to cover day rollover as well as run numbers.
  (dolist (source '(ecmwf-ifs-wind ecmwf-ifs-current ecmwf-aifs-wind))
    (dolist (timestamp '("2026-09-24T00:00:00Z" "2026-09-24T06:00:00Z"
                         "2026-09-24T12:00:00Z" "2026-09-24T18:00:00Z"))
      (let* ((cycle (test-cycle timestamp))
             (previous (previous-cycle source cycle))
             (schedule (datasource-schedule-for-cycle source cycle)))
        (dolist (availability '(:start :end))
          (let* ((minutes (ecase availability
                            (:start (schedule-dissem-start schedule))
                            (:end (schedule-dissem-end schedule))))
                 (boundary (adjust-timestamp (cycle-timestamp cycle) (offset :minute minutes))))
            (dolist (seconds '(-1 0 1))
              (let* ((time (adjust-timestamp boundary (offset :sec seconds)))
                     (expected (cycle-timestamp (if (minusp seconds) previous cycle))))
                (fiveam:is (timestamp= expected
                                      (cycle-timestamp (latest-ecmwf-cycle source time :availability availability))))
                (when (eq availability :end)
                  ;; The public selector must agree with the download schedule.
                  (fiveam:is (timestamp= expected
                                        (cycle-timestamp (latest-complete-cycle source time)))))))))))))

(fiveam:test scheduled-cycles-match-each-datasource-run
  (let ((time (parse-timestring "2026-10-22T19:43:00Z")))
    (dolist (datasource *datasources*)
      (dolist (run (schedule-runs (datasource-schedule datasource)))
        (let ((cycle (cycle-for-run datasource run time)))
          (fiveam:is (= run (cycle-run cycle)))
          (fiveam:is (member run
                             (schedule-runs
                              (schedule (get-datasource datasource cycle))))))))
    (let ((overnight-run (cycle-for-run 'ecmwf-aifs-wind 18
                                        (parse-timestring "2026-10-23T00:27:00Z"))))
      (fiveam:is (= 18 (cycle-run overnight-run)))
      (fiveam:is (equal "20261022" (cycle-datestring overnight-run))))))

(defun make-test-grib-message ()
  (let ((message (make-array 24 :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    (setf (aref message 0) 71
          (aref message 1) 82
          (aref message 2) 73
          (aref message 3) 66
          (aref message 15) 24
          (aref message 20) 55
          (aref message 21) 55
          (aref message 22) 55
          (aref message 23) 55)
    message))

(fiveam:test ecmwf-multipart-range-download-normalization
  (let* ((message (make-test-grib-message))
         (bytes (concatenate 'vector #(13 10) message #(45 45) message #(13 10)))
         (ranges (grib-message-ranges bytes)))
    (fiveam:is (equal '((2 . 24) (28 . 24)) ranges))))

(defun make-spatial-test-uv (u-values &key (bitmap-present 0)
                                            (missing-value 9999d0))
  (make-uv :info (make-gribinfo :grid-size 4
                                :lat-start 1d0
                                :lat-end 0d0
                                :lat-points 2
                                :lon-start 359d0
                                :lon-end 0d0
                                :lon-points 2
                                :i-inc 1d0
                                :j-inc 1d0
                                :i-scan-neg 0
                                :j-scan-pos 0
                                :bitmap-present bitmap-present
                                :missing-value missing-value)
           :source "arome"
           :u-array (make-array 4 :element-type 'double-float
                                :initial-contents u-values)
           :v-array (make-array 4 :element-type 'double-float
                                :initial-element 0d0)))

(defun spatial-test-params (uv)
  (make-params :fc0 uv :fc1 uv :fraction 0d0))

(fiveam:test arome-spatial-border-and-interior-interpolation
  (let ((params (spatial-test-params
                 (make-spatial-test-uv '(1d0 3d0 5d0 7d0)))))
    (multiple-value-bind (direction speed u v)
        (interpolate-uv 1d0 359d0 params)
      (declare (ignore direction v))
      (fiveam:is (= 1d0 u))
      (fiveam:is (= 1d0 speed)))
    (multiple-value-bind (direction speed u v)
        (interpolate-uv 0d0 0d0 params)
      (declare (ignore direction v))
      (fiveam:is (= 7d0 u))
      (fiveam:is (= 7d0 speed)))
    (multiple-value-bind (direction speed u v)
        (interpolate-uv 1d0 0d0 params)
      (declare (ignore direction v))
      (fiveam:is (= 3d0 u))
      (fiveam:is (= 3d0 speed)))
    (multiple-value-bind (direction speed u v)
        (interpolate-uv 0d0 359d0 params)
      (declare (ignore direction v))
      (fiveam:is (= 5d0 u))
      (fiveam:is (= 5d0 speed)))
    (multiple-value-bind (direction speed u v)
        (interpolate-uv 0.5d0 -0.5d0 params)
      (declare (ignore direction v))
      (fiveam:is (= 4d0 u))
      (fiveam:is (= 4d0 speed)))))

(fiveam:test arome-bitmap-missing-points
  (let* ((params (spatial-test-params
                  (make-spatial-test-uv '(9999d0 3d0 5d0 7d0)
                                        :bitmap-present 1)))
         (at-missing-point (multiple-value-list
                            (interpolate-uv 1d0 359d0 params))))
    (fiveam:is (equal '(0d0 0d0 0d0 0d0) at-missing-point))
    (multiple-value-bind (direction speed u v)
        (interpolate-uv 0.5d0 359.5d0 params)
      (declare (ignore direction v))
      (fiveam:is (= 5d0 u))
      (fiveam:is (= 5d0 speed)))))

(defun regional-test-info (&key (i-scan-neg 0) (j-scan-pos 1) (lon-start 348.000710d0))
  "A small non-zero-aligned grid using the actual IBI origin and increments."
  (let ((dx 0.027779d0) (lat 47.027108d0))
    (make-gribinfo :grid-size 9 :lat-points 3 :lon-points 3
                   :lat-start lat :lat-end (+ lat (* (if (= j-scan-pos 1) 2 -2) dx))
                   :lon-start lon-start :lon-end (mod (+ lon-start (* (if (= i-scan-neg 1) -2 2) dx)) 360d0)
                   :i-inc dx :j-inc dx :i-scan-neg i-scan-neg :j-scan-pos j-scan-pos)))

(defun regular-test-params (info &optional (scale 1d0))
  "Synthetic vectors whose node values make row/column mistakes visible."
  (let* ((count (* (gribinfo-lat-points info) (gribinfo-lon-points info)))
         (u (make-array count :element-type 'double-float))
         (v (make-array count :element-type 'double-float)))
    (dotimes (i count)
      (setf (aref u i) (* scale (1+ i)) (aref v i) (* 2d0 scale (1+ i))))
    (spatial-test-params (make-uv :info info :source "regional" :u-array u :v-array v))))

(fiveam:test regional-offset-grid-interpolation-and-zero-outside
  (dolist (east '(0 1))
    (dolist (north '(0 1))
      ;; Exercise signed and unsigned longitudes, crossing Greenwich and 180°.
      (dolist (origin '(348.000710d0 -11.999290d0 359.99d0 179.99d0))
        (let* ((info (regional-test-info :i-scan-neg east :j-scan-pos north :lon-start origin))
               (params (regular-test-params info))
               (dx (* (if (= east 1) -1d0 1d0) (gribinfo-i-inc info)))
               (dy (* (if (= north 1) 1d0 -1d0) (gribinfo-j-inc info)))
               (lat (gribinfo-lat-start info)))
          (labels ((sample (r c)
                     (interpolate-uv (+ lat (* r dy)) (+ origin (* c dx)) params)))
            (dotimes (r 3)
              (dotimes (c 3)
                (fiveam:is (< (abs (- (nth-value 2 (sample r c)) (+ 1 (* r 3) c))) 1d-6))))
            (fiveam:is (< (abs (- (nth-value 2 (sample 0.5d0 0.5d0)) 3d0)) 1d-6))
            (dolist (rc '((-0.01d0 1d0) (2.01d0 1d0) (1d0 -0.01d0) (1d0 2.01d0)))
              (fiveam:is (equal '(0d0 0d0 0d0 0d0)
                                (multiple-value-list (sample (first rc) (second rc)))))))))))
  ;; Temporal interpolation uses the same correctly anchored nodes.
  (let* ((info (regional-test-info))
         (params (regular-test-params info))
         (next (regular-test-params info 2d0)))
    (setf (params-fc1 params) (params-fc0 next) (params-fraction params) 0.5d0)
    (fiveam:is (< (abs (- (nth-value 2 (interpolate-uv (+ 47.027108d0 0.0138895d0)
                                                        (+ 348.000710d0 0.0138895d0) params))
                          4.5d0)) 1d-6))))

(fiveam:test global-seam-poles-and-scalar-regional-fields
  (let* ((info (make-gribinfo :grid-size 12 :lat-points 3 :lon-points 4
                             :lat-start 90d0 :lat-end -90d0 :lon-start 0d0 :lon-end 360d0
                             :i-inc 90d0 :j-inc 90d0))
         (params (regular-test-params info)))
    (fiveam:is (= 2.5d0 (nth-value 2 (interpolate-uv 90d0 315d0 params))))
    (fiveam:is (= 2.5d0 (nth-value 2 (interpolate-uv 90d0 -45d0 params))))
    (fiveam:is (= 4.5d0 (nth-value 2 (interpolate-uv 45d0 135d0 params))))
    (fiveam:is (< (abs (- (nth-value 2 (interpolate-uv 72d0 315d0 params)) 3.3d0)) 1d-10))
    (fiveam:is (= 12d0 (nth-value 2 (interpolate-uv -90d0 270d0 params))))
    (fiveam:is (equal '(0d0 0d0 0d0 0d0) (multiple-value-list (interpolate-uv -91d0 0d0 params)))))
  (let* ((info (regional-test-info :lon-start 359.99d0))
         (uv-params (regular-test-params info))
         (field (make-gribmessage :info info :data (uv-u-array (params-fc0 uv-params))))
         (params (make-params :fc0 field :fc1 field :fraction 0d0)))
    (fiveam:is (< (abs (- (interpolate-simple (+ 47.027108d0 0.0138895d0)
                                              (+ 359.99d0 0.0138895d0) params) 3d0)) 1d-6))
    (fiveam:is (< (abs (- (interpolate-direction 47.027108d0 359.99d0 params) 1d0)) 1d-6))
    (fiveam:is (= 0d0 (interpolate-simple 46d0 0d0 params)))
    (fiveam:is (= 0d0 (interpolate-direction 48d0 0d0 params)))))

(fiveam:test single-node-axes-and-arome-outside
  (let* ((info (make-gribinfo :grid-size 1 :lat-points 1 :lon-points 1
                             :lat-start 50d0 :lat-end 50d0 :lon-start -5d0 :lon-end -5d0
                             :i-inc 1d0 :j-inc 1d0))
         (params (regular-test-params info)))
    (fiveam:is (= 1d0 (nth-value 2 (interpolate-uv 50d0 355d0 params))))
    (fiveam:is (equal '(0d0 0d0 0d0 0d0) (multiple-value-list (interpolate-uv 50d0 -4d0 params)))))
  (let ((params (spatial-test-params (make-spatial-test-uv '(1d0 3d0 5d0 7d0)))))
    (fiveam:is (equal '(0d0 0d0 0d0 0d0) (multiple-value-list (interpolate-uv -1d0 0d0 params))))))

;;; EOF
