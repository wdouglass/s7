;;; fm-violin as a generator (and at end, original instrument using this generator)
;;;
;;; make-fm-violin takes the same args as the instrument version with the following changes
;;;   beg and dur are omitted, also degree, reverb-amount, distance
;;;   all envelopes default to constants (rather than envelopes)
;;;   from the generator's point of view, each envelope is a function called at run time to get its next value,
;;;     very much like "as-needed" input in src or granulate, so the envelopes could actually be any
;;;     arbitrary function you like (see examples at end).
;;;   returns a violin function
;;; fm-violin takes the value returned by make-fm-violin and returns a new sample each time it is called

(provide 'snd-fmv.scm)

(define make-fm-violin 

  (let ((+documentation+ "(make-fm-violin frequency amplitude 
    (fm-index 1.0) (amp-env #f) (periodic-vibrato-rate 5.0) 
    (random-vibrato-rate 16.0) (periodic-vibrato-amplitude 0.0025) 
    (random-vibrato-amplitude 0.005) (noise-amount 0.0) 
    (noise-freq 1000.0) (ind-noise-freq 10.0) (ind-noise-amount 0.0)
    (amp-noise-freq 20.0) (amp-noise-amount 0.0) (gliss-env #f)
    (fm1-env #f) (fm2-env #f) (fm3-env #f) (fm1-rat 1.0) 
    (fm2-rat 3.0)	(fm3-rat 4.0) (fm1-index #f) (fm2-index #f) 
    (fm3-index #f) (base 1.0))
makes a new fm-violin generator.  It is the same as the v.scm version, 
but does not assume it is running within with-sound. In terms of arguments 
beg, dur, degree, reverb-amount, and distance are omitted, 
and all envelopes default to constants (rather than envelopes). 
From the generator's point of view, each envelope is a function called at run time to get its next value, 
very much like 'as-needed' input in src or granulate. 
fm-violin takes the value returned by make-fm-violin and returns a new sample each time it is called: 
  (define (test-v beg dur freq amp)
    (let ((v (make-fm-violin freq amp 
	      :amp-env (let ((e (make-env :envelope '(0 0 1 1 2 0) 
					  :scaler amp :length dur)))
			 (lambda () (env e)))))
	  (data (channel->float-vector beg dur)))
      (do ((i 0 (+ 1 i))) ((= i dur))
	(set! (data i) (+ (data i) (v))))
      (float-vector->channel data beg dur))))"))
		       
    (lambda* (frequency amplitude
			(fm-index 1.0)
			amp-env
			(periodic-vibrato-rate 5.0) 
			(random-vibrato-rate 16.0)
			(periodic-vibrato-amplitude 0.0025) 
			(random-vibrato-amplitude 0.005)
			(noise-amount 0.0) 
			(noise-freq 1000.0)
			(ind-noise-freq 10.0) 
			(ind-noise-amount 0.0)
			(amp-noise-freq 20.0) 
			(amp-noise-amount 0.0)
			gliss-env
			fm1-env
			fm2-env
			fm3-env
			(fm1-rat 1.0) 
			(fm2-rat 3.0)	 
			(fm3-rat 4.0)                    
			fm1-index
			fm2-index
			fm3-index
			(base 1.0))
      
      (let* ((frq-scl (hz->radians frequency))
	     (maxdev (* frq-scl fm-index))
	     (logfreq (log frequency))
	     (modulate (not (zero? fm-index)))
	     (index1 (or fm1-index (min pi (* maxdev (/ 5.0 logfreq)))))
	     (index2 (or fm2-index (min pi (/ (* maxdev 3.0 (- 8.5 logfreq)) (+ 3.0 (* frequency 0.001))))))
	     (index3 (or fm3-index (min pi (* maxdev (/ 4.0 (sqrt frequency))))))
	     (easy-case (and (zero? noise-amount)
			     (or (not fm2-env) (equal? fm1-env fm2-env))
			     (or (not fm3-env) (equal? fm1-env fm3-env))
			     (= fm1-rat (floor fm1-rat))
			     (= fm2-rat (floor fm2-rat))
			     (= fm3-rat (floor fm3-rat))))
	     (carrier (make-oscil frequency))
	     (fmosc1 (and modulate (make-oscil (* fm1-rat frequency))))
	     (fmosc2 (and modulate (or easy-case (make-oscil (* fm2-rat frequency)))))
	     (fmosc3 (and modulate (or easy-case (make-oscil (* fm3-rat frequency)))))
	     
	     (coeffs (and easy-case modulate
			  (partials->polynomial
			   (list (floor fm1-rat) index1
				 (floor (/ fm2-rat fm1-rat)) index2
				 (floor (/ fm3-rat fm1-rat)) index3))))
	     (ampf (or amp-env (lambda () amplitude)))
	     (indf1 (or fm1-env (lambda () (or (and easy-case modulate 1.0) index1))))
	     (indf2 (or fm2-env (lambda () index2)))
	     (indf3 (or fm3-env (lambda () index3)))
	     (pervib (make-triangle-wave periodic-vibrato-rate (* periodic-vibrato-amplitude frq-scl)))
	     (ranvib (make-rand-interp random-vibrato-rate (* random-vibrato-amplitude frq-scl)))
	     (fm-noi (and (not (= 0.0 noise-amount))
			  (make-rand noise-freq (* pi noise-amount))))
	     (amp-noi (and (not (= 0.0 amp-noise-amount))
			   (not (= 0.0 amp-noise-freq))
			   (make-rand-interp amp-noise-freq amp-noise-amount)))
	     (ind-noi (and (not (= 0.0 ind-noise-amount)) 
			   (not (= 0.0 ind-noise-freq))
			   (make-rand-interp ind-noise-freq ind-noise-amount)))
	     (frqf (or gliss-env (lambda () 0.0))))
	
	(lambda ()
	  (let ((vib (+ (frqf) (triangle-wave pervib) (rand-interp ranvib)))
		(fuzz (if (rand? fm-noi) (rand fm-noi) 0.0)))
	    (* (ampf)
	       (if amp-noi (+ 1.0 (rand-interp amp-noi)) 1.0)
	       (oscil carrier 
		      (+ vib 
			 (* (if ind-noi (+ 1.0 (rand-interp ind-noi)) 1.0)
			    (if (not fmosc1)
				0.0
				(if coeffs
				    (* (indf1)
				       (polynomial coeffs (oscil fmosc1 vib)))
				    (+ (* (indf1) (oscil fmosc1 (+ (* fm1-rat vib) fuzz)))
				       (* (indf2) (oscil fmosc2 (+ (* fm2-rat vib) fuzz)))
				       (* (indf3) (oscil fmosc3 (+ (* fm3-rat vib) fuzz))))))))))))))))

#|
(define test-v 
  (lambda (beg dur freq amp amp-env)
    (let ((v (make-fm-violin 
	      freq amp 
	      :amp-env (let ((e (make-env :envelope (or amp-env '(0 0 1 1 2 0)) 
					  :scaler amp 
					  :length dur)))
			 (lambda () (env e)))))
	  (data (channel->float-vector beg dur)))
      (do ((i 0 (+ 1 i)))
	  ((= i dur))
	(set! (data i) (+ (data i) (v))))
      (float-vector->channel data beg dur))))

;;; (with-sound () (test-v 0 10000 440 .1 '(0 0 1 1 2 0)))

(define test-v1
  ;; use oscil as index envelope
  (lambda (beg dur freq amp amp-env)
    (let ((v (make-fm-violin 
	      freq amp 
	      :amp-env (let ((e (make-env :envelope (or amp-env '(0 0 1 1 2 0)) 
					  :scaler amp 
					  :length dur)))
			 (lambda () (env e)))
	      :fm1-env (let ((osc (make-oscil 100.0)))
			 (lambda () (oscil osc)))))
	  (data (channel->float-vector beg dur)))
      (do ((i 0 (+ 1 i)))
	  ((= i dur))
	(set! (data i) (+ (data i) (v))))
      (float-vector->channel data beg dur))))
|#
  
(define fm-violin-ins 
  (let ((+documentation+ "(fm-violin-ins startime dur freq amp degree (reverb-amount 0.0) (distance 1.0) :rest args) 
calls the fm-violin with the given args and mixes the results into the current sound"))
    (lambda* (startime dur freq amp degree (reverb-amount 0.0) (distance 1.0) :rest args)
      (let* ((beg (floor (* startime (srate))))
	     (len (floor (* dur (srate))))
	     (loc (make-locsig :channels (channels) :degree (or degree (random 90.0)) :reverb reverb-amount :distance distance))
	     (out-data (make-float-vector len))
	     (v (apply make-fm-violin freq amp args)))
	(do ((i 0 (+ 1 i)))
	    ((= i len))
	  (set! (out-data i) (v)))
	(if (= (channels) 2)
	    (let ((bsamps (copy out-data)))
	      (mix-float-vector (float-vector-scale! bsamps (locsig-ref loc 1)) beg #f 1 #f)
	      (mix-float-vector (float-vector-scale! out-data (locsig-ref loc 0)) beg #f 0 #f))
	    (mix-float-vector out-data beg #f 0 #f))))))




