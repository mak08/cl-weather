;;; Load after cl-weather, then call TEST-FW-CURRENT-URLS.
(in-package :cl-weather)

(defun test-fw-current-urls ()
  "Check the published Agulhas GGR filename, stable cache paths and its 120 h horizon."
  (let* ((time (parse-timestring "2026-11-15T00:00:00Z"))
         (cycle (make-cycle% :timestamp time))
         (agulhas (make-instance 'fw-current-agulhas :cycle cycle))
         (east (make-instance 'fw-current-east-australia :cycle cycle))
         (channel (make-instance 'fw-current-english-channel :cycle cycle)))
    (assert (string= (data-uri agulhas 0)
                     "https://kxdpgsxzhepthjpeghta.supabase.co/storage/v1/object/public/gribs/currents/20261115/global_agulhas_ggr_20261115.grb2"))
    (assert (search "global_agulhas.20261115.grib2" (namestring (local-pathname agulhas 0 :relative t))))
    (assert (= 120 (maxstep agulhas)))
    (assert (= 114 (cycle-forecast agulhas (adjust-timestamp time (:offset :hour 115)))))
    (assert (= 120 (cycle-forecast agulhas (adjust-timestamp time (:offset :hour 150)))))
    (assert (= 120 (next-forecast agulhas 120)))
    (assert (= 48 (next-forecast channel 48)))
    ;; Do not silently point East Australia at the GGR crop, which omits its northern waters.
    (assert (search "/global_east_australia_20261115.grb2" (data-uri east 0)))
    (assert (search "/ibi_english_channel_20261115.grb2" (data-uri channel 0))))
  (format t "~&Fairwinds current URL and horizon tests passed.~%"))
