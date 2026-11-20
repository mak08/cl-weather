;;; Startup recovery and dissemination boundaries, without network access or timers.
(in-package :cl-weather)
(fiveam:in-suite cl-weather-tests)

(fiveam:test startup-downloads-cross-nominal-cycle-boundaries
  (dolist (source '(ecmwf-ifs-wind ecmwf-ifs-current ecmwf-aifs-wind))
    (dolist (timestamp '("2026-11-19T00:00:00Z" "2026-11-19T06:00:00Z"
                         "2026-11-19T12:00:00Z" "2026-11-19T18:00:00Z"))
      (let* ((cycle (test-cycle timestamp))
             (base (cycle-timestamp cycle))
             (previous (previous-cycle source cycle))
             (schedule (datasource-schedule-for-cycle source cycle))
             (start (* 60 (schedule-dissem-start schedule)))
             (end (* 60 (schedule-dissem-end schedule))))
        (dolist (seconds (list (* 4 3600) (1- start) start (+ (* 6 3600) 1800) (1- end) end (1+ end)))
          (let* ((time (adjust-timestamp base (offset :sec seconds)))
                 (publishing-p (<= start seconds end))
                 (expected (cond ((< seconds start) (list previous))
                                 ((< seconds end) (list previous cycle))
                                 (t (list cycle))))
                 (actual (startup-download-cycles source time)))
            (fiveam:is (eql publishing-p (cycle-updating-p source time)))
            (fiveam:is (= (length expected) (length actual)))
            (loop for wanted in expected for found in actual do
              (fiveam:is (timestamp= (cycle-timestamp wanted) (cycle-timestamp found))))))))))

(defun call-with-startup-test-functions (bindings thunk)
  "Temporarily stub deployment side effects and restore them even after failure."
  (let ((originals (mapcar (lambda (binding) (cons (car binding) (symbol-function (car binding)))) bindings)))
    (unwind-protect
         (progn
           (dolist (binding bindings) (setf (symbol-function (car binding)) (cdr binding)))
           (funcall thunk))
      (dolist (binding originals) (setf (symbol-function (car binding)) (cdr binding))))))

(fiveam:test startup-launches-recovery-downloads-before-timers
  (let ((*datasources* '(ecmwf-ifs-wind ecmwf-ifs-current ecmwf-aifs-wind))
        (events nil))
    (call-with-startup-test-functions
     (list (cons 'download-datasource
                 (lambda (source &key async)
                   (fiveam:is (eq :datasource async))
                   (push (list :download (class-name (class-of source)) (cycle source)) events)))
           (cons 'schedule-download (lambda (source) (push (list :timer source) events)))
           (cons 'start-forecast-ht-cleanup (lambda () (push (list :cleanup) events))))
     (lambda ()
       ;; 18Z is still publishing after the next nominal run at midnight.
       (start-cycle-updates (parse-timestring "2026-11-20T00:30:00Z"))
       (let* ((ordered (reverse events))
              (downloads (subseq ordered 0 6)))
         (fiveam:is (= 10 (length ordered)))
         (fiveam:is (every (lambda (event) (eq :download (first event))) downloads))
         (loop for source in *datasources* for index from 0 by 2 do
           (let ((complete (nth index downloads)) (publishing (nth (1+ index) downloads)))
             (fiveam:is (eq source (second complete)))
             (fiveam:is (eq source (second publishing)))
             (fiveam:is (timestamp= (cycle-timestamp (third complete))
                                   (parse-timestring "2026-11-19T12:00:00Z")))
             (fiveam:is (timestamp= (cycle-timestamp (third publishing))
                                   (parse-timestring "2026-11-19T18:00:00Z")))))
         (fiveam:is (equal (mapcar #'first (subseq ordered 6)) '(:timer :timer :timer :cleanup))))
       ;; Once all three sources have completed, download each run only once.
       (setf events nil)
       (start-cycle-updates (parse-timestring "2026-11-20T02:05:00Z"))
       (fiveam:is (= 3 (count :download events :key #'first)))))))

(defvar *test-publication-schedule* nil)

(defmethod datasource-schedule ((source (eql 'test-publication-source)))
  "Expose test metadata without source-specific availability code."
  *test-publication-schedule*)

(defun startup-test-cycle-times (time)
  "Extract UTC seconds for the synthetic datasource's planned startup cycles."
  (mapcar (lambda (cycle) (timestamp-to-unix (cycle-timestamp cycle)))
          (startup-download-cycles 'test-publication-source (parse-timestring time))))

(defun startup-test-time (text)
  "Convert an expected ISO timestamp into UTC seconds for value comparisons."
  (timestamp-to-unix (parse-timestring text)))

(fiveam:test shared-publication-metadata-controls-availability
  (let ((*test-publication-schedule* (make-schedule :runs '(0 5 17) :steps '(0) :dissem-start 60 :dissem-end 120)))
    ;; Irregular runs do not lie on a grid defined by the shortest interval.
    (fiveam:is (timestamp= (cycle-timestamp (cycle-for-run 'test-publication-source 5
                                              (parse-timestring "2026-11-20T06:30:00Z")))
                          (parse-timestring "2026-11-20T05:00:00Z")))
    (fiveam:is (equalp (startup-test-cycle-times "2026-11-20T06:30:00Z")
                      (mapcar #'startup-test-time '("2026-11-20T00:00:00Z" "2026-11-20T05:00:00Z"))))
    (fiveam:is (= 1 (length (startup-test-cycle-times "2026-11-20T07:00:00Z"))))
    ;; Changing metadata alone changes both default availability and startup recovery.
    (setf (schedule-dissem-end *test-publication-schedule*) 150)
    (fiveam:is (= 2 (length (startup-test-cycle-times "2026-11-20T07:00:00Z"))))
    (fiveam:is (timestamp= (cycle-timestamp (latest-complete-cycle 'test-publication-source
                                                                                (parse-timestring "2026-11-20T07:00:00Z")))
                          (parse-timestring "2026-11-20T00:00:00Z"))))
  (let ((*test-publication-schedule* (make-schedule :runs '(0) :steps '(0) :dissem-start 1500 :dissem-end 1510)))
    ;; Publication can belong to an earlier day, even for daily sources.
    (fiveam:is (equalp (startup-test-cycle-times "2026-11-20T01:05:00Z")
                      (mapcar #'startup-test-time '("2026-11-18T00:00:00Z" "2026-11-19T00:00:00Z")))))
  (let ((*test-publication-schedule* (make-schedule :runs '(0 6) :steps '(0) :dissem-start 0 :dissem-end 480)))
    ;; Startup recovers all publishing runs when their windows overlap.
    (fiveam:is (equalp (startup-test-cycle-times "2026-11-20T07:00:00Z")
                      (mapcar #'startup-test-time '("2026-11-19T06:00:00Z" "2026-11-20T00:00:00Z"
                                                  "2026-11-20T06:00:00Z"))))))

(fiveam:test every-source-startup-follows-its-publication-window
  (dolist (source (cons 'noaa-rtofs-current *datasources*))
    (dolist (run (schedule-runs (datasource-schedule source)))
      (let* ((cycle (cycle-for-run source run (parse-timestring "2026-11-20T23:59:00Z")))
             (base (cycle-timestamp cycle))
             (schedule (datasource-schedule-for-cycle source cycle))
             (start (adjust-timestamp base (offset :sec (* 60 (schedule-dissem-start schedule)))))
             (end (adjust-timestamp base (offset :sec (* 60 (schedule-dissem-end schedule))))))
        (flet ((contains-cycle (time)
                 (find base (startup-download-cycles source time) :key #'cycle-timestamp :test #'timestamp=)))
          (fiveam:is (null (contains-cycle (adjust-timestamp start (offset :sec -1)))))
          (fiveam:is (not (null (contains-cycle start))))
          (fiveam:is (not (null (contains-cycle end)))))
        (fiveam:is (cycle-updating-p source start))
        (fiveam:is (cycle-updating-p source end))
        (fiveam:is (timestamp= base (cycle-timestamp (latest-complete-cycle source end))))
        (fiveam:is (timestamp= base (cycle-timestamp (latest-scheduled-cycle source end :availability :end))))))))

(fiveam:test recurring-timers-use-the-same-run-metadata
  (let ((*test-publication-schedule* (make-schedule :runs '(0 5 17) :steps '(0) :dissem-start 60 :dissem-end 120))
        (clock (parse-timestring "2026-11-20T00:00:00Z")) (timers nil) (downloads nil))
    (call-with-startup-test-functions
     (list (cons 'timers:add-timer (lambda (fn &key hours minutes)
                                    (push (list hours minutes fn) timers)))
           (cons 'get-datasource (lambda (source cycle) (list source cycle)))
           (cons 'download-datasource (lambda (source &key async) (push (list source async) downloads))))
     (lambda ()
       (schedule-download 'test-publication-source :clock (lambda () clock))
       (fiveam:is (equal '((1) (6) (18)) (mapcar #'first (reverse timers))))
       (fiveam:is (every (lambda (timer) (equal '(0) (second timer))) timers))
       ;; Execute captured callbacks without registering timers or downloading files.
       (setf clock (parse-timestring "2026-11-20T18:00:00Z"))
       (funcall (third (find '(18) timers :key #'first :test #'equal)))
       (fiveam:is (timestamp= (cycle-timestamp (second (first (first downloads))))
                             (parse-timestring "2026-11-20T17:00:00Z")))
       (setf clock (parse-timestring "2026-11-21T01:00:00Z"))
       (funcall (third (find '(1) timers :key #'first :test #'equal)))
       (fiveam:is (timestamp= (cycle-timestamp (second (first (first downloads))))
                             (parse-timestring "2026-11-21T00:00:00Z")))
       (fiveam:is (every (lambda (download) (eq :datasource (second download))) downloads))))))
