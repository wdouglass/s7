;;; create the 'digital zipper' effect
;;; a not-very-debonair way to fade out file1 and fade in file2
;;; this is also good if the same file is used twice -- sort of like a CD player gone berserk
;;;
;;; changed 21-Sep-10 to use defgenerator
;;; changed 19-Apr-05 to use def-clm-struct and envelopes (and fixed duration bug in zip-sound)


(provide 'snd-zip.scm)
(if (provided? 'snd)
    (require snd-ws.scm)
    (require sndlib-ws.scm))

(define (safe-srate) (if (pair? (sounds)) (srate) *clm-srate*))

(defgenerator zdata
  (low-start 20)
  (frame-loc 0)
  (cursamples 0)
  (frame0 #f)
  (frame1 #f)
  (frame2 #f)
  (fe #f)
  (rampe #f)
  input1 input2)

(define make-zipper
  (let ((+documentation+ "(make-zipper ramp-env frame-size frame-env) makes a zipper generator.  'ramp-env' is 
an envelope (normally a ramp from 0 to 1) which sets where we are in the zipping process, 
'frame-size' is the maximum frame length during the zip in seconds (defaults to 0.05), and 
'frame-env' is an envelope returning the current frame size during the zip process."))
    (lambda* (ramp-env frame-size frame-env)
      (let ((max-size (+ 1 (ceiling (* (safe-srate) (or frame-size 0.05))))))
	(make-zdata :low-start 20
		    :frame-loc 0
		    :cursamples 0
		    :frame0 (make-float-vector max-size)
		    :frame1 (make-float-vector max-size)
		    :frame2 (make-float-vector max-size)
		    :fe (or frame-env (make-env (list 0 (* (safe-srate) 0.05)) :length (mus-length ramp-env))) ; a bit of a kludge...
		    :rampe ramp-env)))))


(define zipper 
  (let ((+documentation+ "(zipper zip in1 in2) creates the digital zipper sound effect using zipper generator 'zip' and the two samplers 'in1' and 'in2'"))
    (lambda (zp input1 input2)
      (let-set! zp 'input1 input1)
      (let-set! zp 'input2 input2)
      (with-let zp
	(let* ((frame-samples (floor (env fe)))
	       (chunk-len (round (* frame-samples (env rampe)))))
	  (if (<= chunk-len low-start)
	      (begin
		(set! frame-loc 0)
		(read-sample input1))
	      (if (>= chunk-len (- frame-samples low-start))
		  (begin
		    (set! frame-loc 0)
		    (input2))
		  ;; else we're in the ramp phase
		  ;;  read frame if we're within its bounds
		  (begin
		    (when (>= frame-loc cursamples)
		      ;; now get next portion of the ramp
		      (set! frame-loc 0)
		      (set! cursamples frame-samples)
		      (do ((k 0 (+ k 1)))
			  ((= k frame-samples))
			(float-vector-set! frame1 k (read-sample input1)))
		      (do ((k 0 (+ k 1)))
			  ((= k frame-samples))
			(float-vector-set! frame2 k (read-sample input2)))
		      ;; now resample each dependent on location in ramp (samp1 and samp2 are increments)
		      (fill! frame0 0.0)
		      (do ((samp2 (* 1.0 (/ frame-samples chunk-len))) ; this was floor (and also below)?
			   (k 0 (+ k 1))
			   (start-ctr 0.0))
			  ((= k chunk-len))
			(let ((ictr (floor start-ctr)))
			  (let ((y0 (float-vector-ref frame2 ictr))
				(y1 (float-vector-ref frame2 (+ ictr 1))))
			    (float-vector-set! frame0 k (+ y0 (* (- y1 y0) (- start-ctr ictr))))))
			(set! start-ctr (+ start-ctr samp2)))
		      (do ((samp1 (* 1.0 (/ frame-samples (- frame-samples chunk-len))))
			   (k chunk-len (+ k 1))
			   (start-ctr 0.0))
			  ((= k frame-samples))
			(let ((ictr (floor start-ctr)))
			  (let ((y0 (float-vector-ref frame1 ictr))
				(y1 (float-vector-ref frame1 (+ ictr 1))))
			    (float-vector-set! frame0 k (+ y0 (* (- y1 y0) (- start-ctr ictr))))))
			(set! start-ctr (+ start-ctr samp1))))
		    (let ((result (float-vector-ref frame0 frame-loc)))
		      (set! frame-loc (+ frame-loc 1))
		      result)))))))))


;; (zip-sound 0 1 "fyow.snd" "now.snd" '(0 0 1 1) .05)
;; (zip-sound 0 3 "mb.snd" "fyow.snd" '(0 0 1.0 0 1.5 1.0 3.0 1.0) .025)

(define zip-sound 
  (let ((+documentation+ "(zip-sound beg dur file1 file2 ramp-env size) zips the two files and mixes the result into the current sound"))
    (lambda* (beg-in-seconds dur-in-seconds file1 file2 ramp size)
      (let* ((beg (seconds->samples beg-in-seconds))
	     (dur (seconds->samples dur-in-seconds))
	     (zip (make-zipper (make-env (or ramp (list 0 0 1 1)) :length dur)
			       (or size 0.05)
			       (make-env (list 0 (* (srate) (or size 0.05))) :length dur)))
	     (read0 (make-sampler 0 file1))
	     (read1 (make-sampler 0 file2)))
	(map-channel (lambda (y)
		       (+ y (zipper zip read0 read1)))
		     beg dur)))))

#|
(define (ramp-test)
  (let ((data (make-float-vector 10000)))
    (new-sound "new-0.snd")
    (do ((i 0 (+ i 1))) ((= i 10000)) (set! (data i) (* i .0001)))
    (float-vector->channel data 0 10000 0)
    (new-sound "new-1.snd")
    (do ((i 0 (+ i 1))) ((= i 10000)) (set! (data i) (- 1.0 (* i .0001))))
    (float-vector->channel data 0 10000 1)
    (let* ((dur (framples))
	   (zp (make-zipper (make-env '(0 0 1 1) :length dur)
			    0.05
			    (make-env (list 0 (* (safe-srate) 0.05)) :length dur)))
	  (reader0 (make-sampler 0 0 0))
	  (reader1 (make-sampler 0 1 0)))
      (map-channel (lambda (val)
		     (zipper zp reader0 reader1))))))
|#

