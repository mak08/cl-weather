;;; Access-context isolation with synthetic loaders and disposable files; no downloads.
(in-package :cl-weather)
(fiveam:in-suite cl-weather-tests)

(defvar *access-test-directory*)
(defvar *access-test-loads* 0)
(defvar *access-test-mode* :normal)

(defclass access-test-source (ecmwf-ifs-datasource)
  ((name :initform "access-test") (maxstep :initform 9)
   (schedule :initform (datasource-schedule 'access-test-source))))

(defclass access-test-combined (access-test-source)
  ((name :initform "access-test-combined")))

(defmethod datasource-schedule ((source (eql 'access-test-source)))
  "Provide predictable run and step geometry for access-context tests."
  (make-schedule :runs '(0 6 12 18) :steps '(0 3 6 9) :dissem-start 0 :dissem-end 0))

(defmethod datasource-schedule ((source (eql 'access-test-combined)))
  "Use the same forecast steps in a combined-file source."
  (datasource-schedule 'access-test-source))

(defmethod file-step ((source access-test-combined) step)
  "Store every forecast step in one file, as wave and regional sources do."
  (declare (ignore step))
  0)

(defmethod local-pathname ((source access-test-source) step &key relative)
  "Locate a disposable fixture by source, run and physical file step."
  (let ((path (format nil "~a-~a-~a.grib2" (name source) (cycle-string (cycle source)) step)))
    (if relative (pathname path) (merge-pathnames path *access-test-directory*))))

(defmethod load-datasource-forecasts ((source access-test-source) step)
  "Count loader calls, simulate errors, or seed fields from a tiny text fixture."
  (incf *access-test-loads*)
  (let ((path (local-pathname source (file-step source step))))
    (ecase *access-test-mode*
      (:corrupt (error "Synthetic decoding failure"))
      (:missing-but-present (signal-missing-forecast-file source step (namestring path)))
      (:vanished (delete-file path) (signal-missing-forecast-file source step (namestring path)))
      (:vanished-regional (delete-file path) (error 'missing-file :filename path))
      (:normal
       ;; Match the real file-based loader's absence condition outside a context.
       (unless (probe-file path) (error 'missing-file :filename path))
       (let ((value (with-open-file (in path) (read in)))
             (base (cycle-timestamp (cycle source))))
         (dolist (s (if (typep source 'access-test-combined) '(0 3 6 9) (list step)))
           (setf (gethash (get-key source s) *forecast-ht*)
                 (make-uv :cycle base :basetime base :offset (* s 60) :step s
                          :u-array (make-array 1 :element-type 'double-float :initial-element value)
                          :v-array (make-array 1 :element-type 'double-float :initial-element 0d0))
                 (gethash (get-direction-key source s) *forecast-ht*)
                 (make-gribmessage :basetime base :step s))))))))

(defmacro with-access-test-files (&body body)
  "Create an isolated file tree and cache, then remove only these test fixtures."
  `(let ((*access-test-directory*
           (merge-pathnames (format nil "weather-access-~a-~a/" (get-universal-time) (random 1000000000))
                            (uiop:temporary-directory)))
         (*forecast-ht* (make-hash-table :test #'equalp))
         (*access-test-loads* 0) (*access-test-mode* :normal))
     (ensure-directories-exist *access-test-directory*)
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree *access-test-directory* :validate t))))

(defun access-test-source (&optional (run 18) (class 'access-test-source))
  "Construct a synthetic source without consulting the clock."
  (make-instance class :cycle (make-cycle% :timestamp
                              (parse-timestring (format nil "2026-11-19T~2,'0d:00:00Z" run)))))

(defun write-access-test-file (source step)
  "Make a previously missing physical file available to a different context."
  (with-open-file (out (local-pathname source (file-step source step))
                       :direction :output :if-exists :supersede)
    (write 1d0 :stream out)))

(fiveam:test access-context-keeps-absence-before-shared-cache
  (with-access-test-files
    (let ((source (access-test-source)))
      (with-forecast-access-context ()
        (fiveam:signals missing-forecast (load-forecast source 6))
        (write-access-test-file source 6)
        ;; Another context can populate the positive cache immediately.
        (with-forecast-access-context () (fiveam:is (uv-p (load-forecast source 6))))
        (fiveam:is (= 1 *access-test-loads*))
        (dotimes (i 100)
          (fiveam:signals missing-forecast (load-forecast source 6)))
        (fiveam:is (= 1 *access-test-loads*)))
      (with-forecast-access-context () (fiveam:is (uv-p (load-forecast source 6))))
      (fiveam:is (= 1 *access-test-loads*)))))

(fiveam:test access-context-groups-combined-steps-and-direction
  (with-access-test-files
    (let ((source (access-test-source 18 'access-test-combined)))
      (with-forecast-access-context ()
        (fiveam:signals missing-forecast (load-direction-forecast source 3))
        (write-access-test-file source 0)
        (handler-case (progn (load-forecast source 6) (fiveam:fail "Missing combined file became available"))
          (missing-forecast (e) (fiveam:is (= 6 (offset e)))))
        (fiveam:signals missing-forecast (load-direction-forecast source 9))
        (fiveam:is (zerop *access-test-loads*)))
      (with-forecast-access-context ()
        (fiveam:is (uv-p (load-forecast source 6)))
        (fiveam:is (gribmessage-p (load-direction-forecast source 9))))
      (fiveam:is (= 1 *access-test-loads*)))))

(fiveam:test access-context-preserves-fallback-and-cleans-up-on-error
  (with-access-test-files
    (let ((source (access-test-source)) (previous (access-test-source 12))
          (time (parse-timestring "2026-11-20T01:17:00Z"))
          (outer *forecast-access-context*))
      ;; The previous cycle holds its final frame at hour 9 for this fixture.
      (write-access-test-file previous 9)
      (handler-case
          (with-forecast-access-context ()
            (dotimes (i 100)
              (let ((params (get-params 'access-test-source time :cycle (cycle source))))
                (fiveam:is (timestamp= (cycle-timestamp (cycle previous)) (uv-cycle (params-fc0 params))))))
            (error "Synthetic operation failure"))
        (error () nil))
      (fiveam:is (eq outer *forecast-access-context*))
      (fiveam:is (= 1 *access-test-loads*))
      (write-access-test-file source 6)
      (write-access-test-file source 9)
      (with-forecast-access-context ()
        (fiveam:is (timestamp= (cycle-timestamp (cycle source))
                              (uv-cycle (params-fc0 (get-params 'access-test-source time :cycle (cycle source))))))))))

(fiveam:test access-context-remembers-only-actual-absence
  (with-access-test-files
    (let ((source (access-test-source)))
      (dolist (mode '(:corrupt :missing-but-present))
        (setf *access-test-loads* 0)
        (write-access-test-file source 6)
        (let ((*access-test-mode* mode))
          (with-forecast-access-context ()
            (dotimes (i 2) (fiveam:signals error (load-forecast source 6)))))
        (fiveam:is (= 2 *access-test-loads*)))
      (dolist (mode '(:vanished :vanished-regional))
        (setf *access-test-loads* 0)
        (write-access-test-file source 6)
        (let ((*access-test-mode* mode))
          (with-forecast-access-context ()
            (fiveam:signals missing-forecast (load-forecast source 6))
            (write-access-test-file source 6)
            (fiveam:signals missing-forecast (load-forecast source 6))))
        (fiveam:is (= 1 *access-test-loads*))))))

(fiveam:test access-without-context-still-retries-files
  (with-access-test-files
    (let ((*forecast-access-context* nil) (source (access-test-source)))
      (dotimes (i 2) (fiveam:signals missing-file (load-forecast source 6)))
      (fiveam:is (= 2 *access-test-loads*))
      (write-access-test-file source 6)
      (fiveam:is (uv-p (load-forecast source 6))))))

(fiveam:test access-context-isolates-concurrent-operations
  (with-access-test-files
    (let ((source (access-test-source))
          (directory *access-test-directory*) (cache *forecast-ht*) (loaded nil))
      (with-forecast-access-context ()
        (fiveam:signals missing-forecast (load-forecast source 6))
        (write-access-test-file source 6)
        (let ((worker
                (bordeaux-threads:make-thread
                 (lambda ()
                   (handler-case
                       (let ((*access-test-directory* directory) (*forecast-ht* cache)
                             (*access-test-loads* 0) (*access-test-mode* :normal))
                         (with-forecast-access-context () (load-forecast source 6)))
                     (error (e) e))))))
          (setf loaded (bordeaux-threads:join-thread worker)))
        (fiveam:is (uv-p loaded))
        (fiveam:signals missing-forecast (load-forecast source 6)))
      (with-forecast-access-context () (fiveam:is (eq loaded (load-forecast source 6)))))))

(fiveam:test access-context-retains-normal-positive-cache-behavior
  (with-access-test-files
    (let ((source (access-test-source)))
      (write-access-test-file source 6)
      (let ((loaded (load-forecast source 6)))
        (delete-file (local-pathname source 6))
        ;; Absence is recorded only when a cache miss requires actual file access.
        (with-forecast-access-context () (fiveam:is (eq loaded (load-forecast source 6))))
        (fiveam:is (= 1 *access-test-loads*))))))
