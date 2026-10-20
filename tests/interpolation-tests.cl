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
