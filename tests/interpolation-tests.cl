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
    (fiveam:is (= 454 (schedule-dissem-end long-schedule)))
    (fiveam:is (= 360 (car (last (schedule-steps long-schedule)))))
    (fiveam:is (= 345 (schedule-dissem-start short-schedule)))
    (fiveam:is (= 387 (schedule-dissem-end short-schedule)))
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
  (let* ((before-06z (parse-timestring "2026-09-24T12:20:00Z"))
         (after-06z (parse-timestring "2026-09-24T12:30:00Z"))
         (before-12z-complete (parse-timestring "2026-09-24T19:30:00Z"))
         (after-12z-complete (parse-timestring "2026-09-24T19:40:00Z")))
    (fiveam:is (= 0 (cycle-run (latest-ecmwf-cycle
                                'ecmwf-aifs-wind before-06z :availability :start))))
    (fiveam:is (= 6 (cycle-run (latest-ecmwf-cycle
                                'ecmwf-aifs-wind after-06z :availability :start))))
    (fiveam:is (= 6 (cycle-run (latest-ecmwf-cycle
                                'ecmwf-aifs-wind before-12z-complete :availability :end))))
    (fiveam:is (= 12 (cycle-run (latest-ecmwf-cycle
                                 'ecmwf-aifs-wind after-12z-complete :availability :end))))))

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

;;; EOF
