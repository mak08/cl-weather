;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test: Arome data-uri generation
;;; Verifies the `data-uri` for `arome-wind` includes the run directory

(in-package :cl-weather)

(defun test-arome-data-uri ()
  (let* ((cycle (make-cycle :timestamp (parse-timestring "2026-06-13T06:00:00Z")))
         (ds (get-datasource 'arome-wind cycle))
         (uri (data-uri ds 6))
         (expected "https://dk7714bfk71nn.cloudfront.net/arome/western_med/20260613/06/arome.t06z.western_med.f006-f048.grib2"))
    (format t "Generated: ~a~%Expected:  ~a~%" uri expected)
    (if (string= uri expected)
        (format t "TEST-PASS: test-arome-data-uri~%")
        (error "TEST-FAIL: Arome data-uri mismatch\nGenerated: ~a\nExpected:  ~a" uri expected))))

;; Run automatically when loaded
(handler-case
    (progn (test-arome-data-uri))
  (error (e) (format t "ERROR running test-arome-data-uri: ~a~%" e)))
