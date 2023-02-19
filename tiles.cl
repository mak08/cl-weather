;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2018
;;; Last Modified <michael 2023-02-18 23:53:54>

(in-package :cl-weather)

(declaim (optimize (speed 1) (space 0) (debug 3) (safety 3)))

(defvar *tile-root-dir* (merge-pathnames (pathname "TileRoot/") *source-root*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Datatypes

(defstruct fctile-stack
  (north 0 :type fixnum)
  (south 0 :type fixnum)
  (east 0 :type fixnum)
  (west 0 :type fixnum)
  cycle
  resolution
  from
  to
  data)

(defstruct fctile-data
  (offset 0 :type fixnum)
  (nlat 0 :type fixnum)
  (nlon 0 :type fixnum)
  u-data
  v-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aux functions
 
(defun tile-filename (cycle resolution fc lat0 lon0 &key (tile-root-dir *tile-root-dir*))
  ;; TILE-FILENAME must match the URI used by the web client!
  ;; Otherwise the URI -> path mapping needs to be reproduced in the try_files directive!
  (merge-pathnames
   (make-pathname
    :directory (list :relative
                     (cycle-string cycle)
                     resolution
                     (format nil "~3,'0d" fc)
                     (format nil "~3,'0d" lat0))
    :name (format nil "~3,'0d.dat" lon0))
   tile-root-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write tile to file

(defmethod write-fctile-stack ((f stream) stack)
  (log2:trace "Writing ~a" (pathname f))
  (write-uint16 f (fctile-stack-north stack))
  (write-uint16 f (fctile-stack-south stack))
  (write-uint16 f (fctile-stack-west stack))
  (write-uint16 f (fctile-stack-east stack))
  (write-sequence (babel:string-to-octets (format-datetime nil (cycle-timestamp (fctile-stack-cycle stack)))
                                          :encoding :utf-16) f)
  (write-uint16 f
                (cond
                  ((equal (fctile-stack-resolution stack) "1p00")
                   100)
                  ((equal (fctile-stack-resolution stack) "0p25")
                   25)
                  (t
                   (error "Unsupported resoltion ~a" (fctile-stack-resolution stack)))))
  (write-uint16 f (length (fctile-stack-data stack)))
  (map nil (lambda (data)
             (write-fctile-data f data))
       (fctile-stack-data stack)))

(defmethod write-fctile-data ((f stream) data)
  (write-uint16 f (fctile-data-offset data))
  (write-uint16 f (fctile-data-nlat data))
  (write-uint16 f (fctile-data-nlon data))
  (write-uint32 f (array-total-size (fctile-data-u-data data)))
  (loop
    :for k :below (array-total-size (fctile-data-u-data data))
    :do (write-uint16 f (pack16 (row-major-aref (fctile-data-u-data data) k))))
  (loop
    :for k :below (array-total-size (fctile-data-v-data data))
    :do (write-uint16 f (pack16 (row-major-aref (fctile-data-v-data data) k)))))

(defmethod write-uint16 ((f stream) n)
  (assert (< n (expt 2 16)))
  ;; (assert (>= n 0))
  ;; Javascript-compatible byte order
  (write-byte (ldb (byte 8 0) n) f)
  (write-byte (ldb (byte 8 8) n) f))

(defmethod write-uint32 ((f stream) n)
  (assert (< n (expt 2 32)))
  ;; (assert (>= n 0))
  ;; Javascript-compatible byte order
  (write-byte (ldb (byte 8 0) n) f)
  (write-byte (ldb (byte 8 8) n) f)
  (write-byte (ldb (byte 8 16) n) f)
  (write-byte (ldb (byte 8 24) n) f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write tile to buffer

(defun fctile-stack-byte-size (tile)
  (let ((n (fctile-stack-north tile))
        (s (fctile-stack-south tile))
        (w (fctile-stack-west tile))
        (e (fctile-stack-east tile))
        (res (cond
               ((equal (fctile-stack-resolution tile) "1p00")
                100)
               ((equal (fctile-stack-resolution tile) "0p25")
                   25)
               (t
                (error "Unsupported resoltion ~a" (fctile-stack-resolution tile)))))
        (from (fctile-stack-from tile))
        (to (fctile-stack-to tile)))
  (+ 8     ; coordinates
     42    ; cycle string with BOM
     2     ; resolution
     2     ; number of forecasts
     (* (1+ (/ (- to from) 3)) ; for each 3-hr forecast
        (+ 2   ; offset
           2   ; # lat values
           2   ; # lon values
           (* 2 ; bytes per value
              2 ; u and v
              (- s n) (/ 100 res)
              (- e w) (/ 100 res)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packing 

(defun pack8 (f)
  (declare ((double-float -200d0 200d0) f))
  (let* ((m (min 1d0 (/ (sqrt (abs f)) 8d0)))
         (b (round (* m (1- (expt 2 7))))))
    (if (< f 0d0)
        (logior b (expt 2 7))
        b)))

(defun unpack8 (b)
  (declare (fixnum b))
  (let ((sign (if (> b (expt 2 7)) -1 1)))
    (setf b (logand b (1- (expt 2 7))))
    (let ((m  (* (/ b (1- (expt 2 7))) 8)))
      (coerce (* sign (expt m 2)) 'double-float))))

(defun pack16 (f)
  (declare ((double-float -200d0 200d0) f))
  (let* ((m (min 1d0 (/ (sqrt (abs f)) 8d0)))
         (b (truncate (* m (1- (expt 2 15))))))
    (if (< f 0d0)
        (logior b (expt 2 15))
        b)))

(defun unpack16 (b)
  (declare (fixnum b))
  (let ((sign (if (> b (expt 2 15)) -1 1)))
    (setf b (logand b (1- (expt 2 15))))
    (let ((m  (* (/ b (1- (expt 2 15))) 8)))
      (coerce (* sign (expt m 2)) 'double-float))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating tile data

(defun get-wind-uv (north south west east
                    &key
                      (cycle (available-cycle (now)))
                      (resolution "1p00")
                      (from-forecast 0)
                      (to-forecast from-forecast))
  (assert (> south north))
  (assert (> east west))
  (make-fctile-stack
   :north north
   :south south
   :east east
   :west west
   :cycle cycle
   :resolution resolution
   :from from-forecast
   :to to-forecast
   :data (loop :for offset :from from-forecast :to to-forecast :by 3
               :with increment = (cond
                                   ((equal resolution "1p00")
                                    1)
                                   ((equal resolution "0p25")
                                    0.25)
                                   (t
                                    (error "Unsupported resolution ~a" resolution)))
               :with numlat = (1+ (truncate (- south north) increment))
               :with numlon = (1+ (truncate (- east west) increment))
               :collect
               (let*
                   ((dataset (load-forecast :cycle cycle :offset offset :resolution resolution))
                    (uv (dataset-forecast dataset))
                    (info (dataset-grib-info dataset))
                    (result-u (make-array (list numlat numlon) :element-type 'double-float))
                    (result-v (make-array (list numlat numlon) :element-type 'double-float)))
                 (loop :for lat :from north :to south :by increment
                       :for lat0 :from 0
                       :do (loop :for lon :from  west :to east :by increment
                                 :for lon0 :from 0
                                 :for index = (uv-index info lat (mod lon 360))
                                 :do (multiple-value-bind (u v)
                                         (grib-get-uv uv index)
                                       (setf (aref result-u lat0 lon0) u)
                                       (setf (aref result-v lat0 lon0) v))))
                 (make-fctile-data :offset offset
                                   :nlat numlat
                                   :nlon numlon
                                   :u-data result-u
                                   :v-data result-v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating binary data tiles

(defun create-tile (filename north south west east 
                    &key
                      (cycle (available-cycle (now)))
                      (resolution "1p00")
                      (from-forecast 0)
                      (to-forecast from-forecast))
  (let ((fctile-stack (get-wind-uv north south west east
                                   :cycle cycle
                                   :resolution resolution
                                   :from-forecast from-forecast
                                   :to-forecast to-forecast)))
    (with-open-file (f filename
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :supersede)
      (write-fctile-stack f fctile-stack))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating tilesets

(defun create-tiles (&key
                       (tile-root-dir *tile-root-dir*)
                       (cycle (available-cycle (now)))
                       (resolution "1p00"))
  (loop
    :for fc :from 0 :to 384 :by 3
    :do (loop
          :for lat0 :from -90 :to 80 :by 10
          :do (loop
                :for lon0 :from 0 :to 350 :by 10
                :do (let ((filename (tile-filename cycle resolution fc lat0 lon0)))
                      (ensure-directories-exist filename)
                      ;; (log2:info "Tile ~a-~a-~a" fc lat0 lon0)
                      (create-wind-tile-binary filename lat0 (+ lat0 10) lon0 (+ lon0 10)
                                               :cycle cycle
                                               :resolution resolution
                                               :from-forecast fc
                                               :to-forecast fc))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test tile

(defun create-test-tile (&key (path "/home/michael/Wetter/tile-025/test.dat"))
  (with-open-file (f path
                     :element-type '(unsigned-byte 8)
                     :direction :output
                     :if-exists :supersede)
    (let ((test-tile
            (make-fctile-stack
             :north 50
             :south 49
             :east 10
             :west 9
             :cycle (make-cycle) 
             :resolution "0p25"
             :from 0
             :to 3
             :data (list
                    (make-fctile-data :offset 0
                                      :u-data (make-array 4
                                                          :initial-contents (list 3d0 -3d0 21.7d0 -21.7d0))
                                      :v-data (make-array 4
                                                          :initial-contents (list 0.3d0 -0.3d0 64.0d0 -64.0d0)))
                    (make-fctile-data :offset 3
                                      :u-data (make-array 4
                                                          :initial-contents (list 100d0 -100d0 100.7d0 -100.7d0))
                                      :v-data (make-array 4
                                                          :initial-contents (list 3d0 -3d0 21.7d0 -21.7d0)))))))
      (write-fctile-stack f test-tile))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
