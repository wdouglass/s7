;; clean-channel -- noise reduction

(provide 'snd-clean.scm)
(require snd-dsp.scm snd-generators.scm)

(define goertzel-channel
  (let ((+documentation+ "(goertzel-channel freq beg dur snd (chn 0)) returns the amplitude of the 'freq' spectral component"))
    (lambda* (freq (beg 0) dur snd chn)
      (let* ((rfreq (/ (* 2.0 pi freq) (srate snd)))
	     (cs (* 2.0 (cos rfreq))))
	(let ((reader (make-sampler beg snd chn))
	      (len (- (if (number? dur) dur (- (framples snd chn) beg)) 2))
	      (flt (make-two-pole 1.0 (- cs) 1.0)))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (two-pole flt (next-sample reader)))
	  (let ((y1 (two-pole flt (next-sample reader)))
		(y0 (two-pole flt (next-sample reader))))
	    (magnitude (- y0 (* y1 (exp (complex 0.0 (- rfreq))))))))))))


(define* (check-freq freq snd chn)
  (do ((hum 0.0)
       (i 0 (+ i 1))
       (loc 0.0 (+ loc (round (/ (framples snd chn) 5)))))
      ((= i 4)
       (/ hum 4.0))
    (set! hum (+ hum (goertzel-channel freq loc 2048 snd chn)))))


;;; -------- single sample clicks

(define* (remove-single-sample-clicks (jump 8) snd chn)
  (let* ((len (framples snd chn))
	 (block-size (min len 1048576))) ; do edits by blocks rather than sample-at-a-time (saves time, memory etc) 1048576=1024*1024
    (let ((reader (make-sampler 0 snd chn))
	  (mx (make-moving-average 16)) ; local average of abs difference
	  (samp0 0.0)
	  (samp1 0.0)
	  (samp2 0.0)
	  (fixed 0)
	  (block-ctr 0)
	  (block-beg 0)
	  (block (make-float-vector block-size))
	  (block-changed #f))
      (do ((ctr 0 (+ ctr 1)))
	  ((= ctr len))
	(set! samp0 samp1)
	(set! samp1 samp2)
	(set! samp2 (next-sample reader))
	(set! (block block-ctr) samp2)
	(let ((df1 (abs (- samp1 samp0))))
	  (let ((df2 (abs (- samp2 samp1)))
		(df3 (abs (- samp2 samp0)))
		(local-max (moving-average mx df1)))
	    (if (and (> df1 (* jump local-max))
		     (> df2 (* jump local-max))
		     (or (< df3 local-max)
			 (and (< df3 (* 2 local-max))
			      (< (* (- samp2 samp0)
				    (- samp1 samp0))
				 0.0))))
		(begin
		  (set! samp1 (* .5 (+ samp0 samp2)))
		  (set! (block (- block-ctr 1)) samp1)
		  (set! block-changed #t)
		  (set! fixed (+ 1 fixed))))))
	(set! block-ctr (+ 1 block-ctr))
	(if (>= block-ctr block-size)
	    (begin
	      (if block-changed
		  (begin
		    (float-vector->channel block block-beg block-size snd chn)
		    (set! block-changed #f)))
	      (set! block-beg (- (+ block-beg block-size) 1))
	      (set! block-ctr 1)
	      (set! (block 0) samp2))))
      (if block-changed
	  (float-vector->channel block block-beg block-ctr snd chn))
      fixed)))

(define (test-remove-single-clicks)
  (let ((test (new-sound "test.snd")))
    (let ((data (make-float-vector 1001)))
      (do ((i 2 (+ i 30))
	   (val .9 (- val .05)))
	  ((>= i 1000))
	(float-vector-set! data i val))
      (set! (data 1000) .001)
      (float-vector->channel data)
      (remove-single-sample-clicks)
      (let ((mx (maxamp))
	    (loc (maxamp-position)))
	(if (> mx 0.06)
	    (format () "~%;remove-single-sample-clicks 0: ~A (at ~D)" mx loc)))
      (revert-sound)
      (do ((i 0 (+ i 1))
	   (ang 0.0 (+ ang .01)))
	  ((= i 1000))
	(float-vector-set! data i (+ (float-vector-ref data i) (* .2 (sin ang)))))
      (float-vector->channel data)
      (remove-single-sample-clicks)
      (if (fneq (maxamp) .2)
	  (format () "~%;remove-single-sample-clicks sin max: ~A" (maxamp)))
      (let ((cur-data (channel->float-vector 0))
	    (diff 0.0))
	(do ((i 0 (+ i 1))
	     (ang 0.0 (+ ang .01)))
	    ((= i 1000))
	  (set! diff (max diff (abs (- ( cur-data i) (* .2 (sin ang)))))))
	(if (> diff .2)
	    (format () "~%;remove-single-sample-clicks sine max diff: ~A" diff))))
    (close-sound test)))

;;; -------- pops

(define* (smooth-float-vector data beg dur)
  (let ((y0 (data beg))
	(y1 (data (+ beg dur))))
    (do ((angle (if (> y1 y0) pi 0.0)) 
	 (off (* .5 (+ y0 y1))) 
	 (scale (* 0.5 (abs (- y1 y0))))
	 (incr (/ pi dur))
	 (i 0 (+ i 1)))
	((= i dur))
      (set! (data (+ beg i)) (+ off (* scale (cos (+ angle (* i incr)))))))))

(define* (remove-pops (size 8) snd chn)
  (let* ((len (framples snd chn))
	 (pad (* 8 size))
	 (block-size (min (+ len pad) 1048576))) ; 1048576=1024*1024
    (let ((reader (make-sampler 0 snd chn))
	  (dly0 (make-delay (* 4 size)))
	  (dly1 (make-delay (* 5 size)))
	  (mx-ahead (make-moving-average (* 4 size))) ; local average of abs difference ahead of current window
	  (mx-behind (make-moving-average (* 4 size))) ; local average of abs difference behind current window
	  (mx (make-moving-average size)) ; local average of abs difference
	  
	  (last-ahead-samp 0.0)
	  (last-dly0-samp 0.0)
	  (last-dly1-samp 0.0)
	  (last-case 0)
	  (fixed 0)
	  (block-ctr 0)
	  (block-beg 0)
	  (block (make-float-vector block-size))
	  (block-changed #f))
      
      (let ((check-val 0.0)
	    (check-start 0)
	    (checker 0)
	    (checked 0)
	    (checking #f)
	    (moving-start #t))
	(do ((ctr 0 (+ ctr 1)))
	    ((= ctr len))
	  (let* ((ahead-samp (next-sample reader))
		 (avg-ahead (moving-average mx-ahead (abs (- ahead-samp last-ahead-samp))))
		 (dly0-samp (delay dly0 ahead-samp))
		 (cur-diff (abs (- dly0-samp last-dly0-samp)))
		 (cur-avg (moving-average mx cur-diff))
		 (dly1-samp (delay dly1 ahead-samp))
		 (avg-behind (moving-average mx-behind (abs (- dly1-samp last-dly1-samp)))))
	    (set! last-ahead-samp ahead-samp)
	    (set! last-dly0-samp dly0-samp)
	    (set! last-dly1-samp dly1-samp)
	    (set! (block block-ctr) ahead-samp)
	    (if checking
		(begin
		  (set! checked (+ checked 1))
		  (if (or (>= checked (* 2 size))
			  (< cur-avg check-val))
		      (begin
			(set! fixed (+ fixed 1))
			(set! checking #f)
			(smooth-float-vector block (- check-start block-beg) (+ size checker))
			(set! block-changed #t)))
		  (if moving-start
		      (begin
			(set! moving-start (< cur-diff avg-behind))
			(if moving-start
			    (set! check-start (+ check-start 1)))))
		  (if (not moving-start)
		      (set! checker (+ checker 1))))
		(if (and (> (- ctr last-case) (* 2 size))
			 (> cur-avg (* 4 avg-ahead))
			 (> cur-avg (* 4 avg-behind)))
		    ;; possible pop
		    (begin
		      (set! check-start (max 0 (- ctr (* 5 size))))
		      (set! moving-start (< cur-diff avg-behind))
		      (if moving-start
			  (set! check-start (+ check-start 1)))
		      (set! checking #t)
		      (set! check-val cur-avg)
		      (set! checker 0)
		      (set! checked 0)
		      (set! last-case ctr))))
	    
	    (set! block-ctr (+ block-ctr 1))
	    (if (>= (+ block-ctr pad) block-size)
		(begin
		  (if block-changed
		      (begin
			(float-vector->channel block block-beg (- block-ctr pad) snd chn)
			(set! block-changed #f)))
		  (set! block-beg (- (+ block-beg block-ctr) pad))
		  (do ((i 0 (+ i 1))
		       (j (- block-ctr pad) (+ j 1)))
		      ((= i pad))
		    (set! (block i) (block j)))
		  (set! block-ctr pad)))))
	
	(if block-changed
	    (float-vector->channel block block-beg block-ctr snd chn)))
      
      fixed)))

(define (test-remove-pops)
  (new-sound "test.snd")
  (let ((data (make-float-vector 4001)))
    (do ((i 100 (+ i 200)))
	((>= i 3800))
      (let ((size (random 8)))
	(do ((k 0 (+ k 1)))
	    ((= k size))
	  (set! (data (+ i k)) (mus-random 1.0)))))
    (float-vector->channel data)
    (remove-pops)
    (let ((mx (maxamp)))
      (if (> mx .01)
	  (format () "~%;test remove-pops 0 case: ~A" mx)))
    (revert-sound)
    (do ((i 0 (+ i 1))
	 (ang 0.0 (+ ang .01)))
	((= i 4000))
      (set! (data i) (+ (data i)
			(* .2 (sin ang)))))
    (float-vector->channel data)
    (remove-pops)
    (let ((mx (maxamp)))
      (if (fneq mx .2)
	  (format () "~%;test remove-pops sine case: ~A" mx)))
    (close-sound)))
	

;;; -------- hum

(define (test-notch-hum)
  (let ((test (with-sound ("test.snd" :srate 22050)
		(let ((osc (make-oscil 60.0))
		      (e (make-env '(0 0 1 .5 9 .5 10 0) :length 44100)))
		   (do ((i 0 (+ i 1)))
		       ((= i 44100))
		     (outa i (* (env e) (oscil osc))))))))
    
    (notch-channel (list 60.0) #f #f #f #f #f #f #t 8)
    (let ((mx (maxamp)))
      (if (> mx .02)
	  (format () "~%;notch hum 0: ~A" mx)))
    (close-sound (find-sound test)))
  (let ((test (with-sound ("test.snd" :srate 22050)
		(let ((p (make-polywave 20.0 '(2 1 3 1 4 1)))
		      (e (make-env '(0 0 1 .3 9 .3 10 0) :scaler 1/3 :length 44100)))
		   (do ((i 0 (+ i 1)))
		       ((= i 44100))
		     (outa i (* (env e) (polywave p))))))))
    
    (let ((v60 (goertzel 60.0))
	  (v40 (goertzel 40.0))
	  (v80 (goertzel 80.0)))
      (notch-channel (list 60.0) #f #f #f #f #f #f #t 8)
      (let ((e60 (goertzel 60.0))
	    (e40 (goertzel 40.0))
	    (e80 (goertzel 80.0)))
	(if (or (fneq (/ e60 v60) 0.0)
		(fneq (/ e40 v40) 1.0)
		(fneq (/ e80 v80) 1.0))
	    (format () "~%;notch 60: ~A ~A ~A -> ~A ~A ~A" v40 v60 v80 e40 e60 e80))))
    (close-sound (find-sound test)))

  (let ((test (with-sound ("test.snd" :srate 22050)
		(let ((p (make-polywave 5.0 '(11 1 12 1 13 1)))
		      (e (make-env '(0 0 1 .3 9 .3 10 0) :scaler 1/3 :length 44100)))
		   (do ((i 0 (+ i 1)))
		       ((= i 44100))
		     (outa i (* (env e) (polywave p))))))))
    
    (let ((v60 (goertzel 60.0))
	  (v40 (goertzel 55.0))
	  (v80 (goertzel 65.0)))
      (notch-channel (list 60.0) #f #f #f #f #f #f #t 2)
      (let ((e60 (goertzel 60.0))
	    (e40 (goertzel 55.0))
	    (e80 (goertzel 65.0)))
	(if (or (> (/ e60 v60) 0.01)
		(< (/ e40 v40) 0.99)
		(< (/ e80 v80) 0.99))
	    (format () "~%;notch 60 tight: ~A ~A ~A -> ~A ~A ~A" v40 v60 v80 e40 e60 e80))))
    (close-sound (find-sound test))))


;;; -------- DC

(define (test-remove-DC)
  (let ((test (new-sound "test.snd"))
	(data (make-float-vector 4001)))
    (do ((i 0 (+ i 1))
	 (ang 0.0 (+ ang .01)))
	((= i 4000))
      (float-vector-set! data i (+ .1 (mus-random 0.1) (* .2 (sin ang)))))
    (float-vector->channel data)
    (let ((dc (goertzel 0.0))
	  (sig (goertzel 35.0)))
      (let ((dcflt (make-filter 2 #r(1 -1) #r(0 -0.99))))
	(map-channel (lambda (y) (filter dcflt y)))
	(let ((ndc (goertzel 0.0))
	      (nsig (goertzel 35.0)))
	  (if (or (> (/ ndc dc) .1)
		  (< (/ nsig sig) .4))
	      (format () "~%;remove-DC: ~A -> ~A (~A), ~A -> ~A (~A)" dc ndc (/ ndc dc) sig nsig (/ nsig sig))))))
    (close-sound test)))


(define* (tvf-channel snd chn)
  (let ((size (framples snd chn))
	(avg-size 8192))
    (let ((avg-data (make-float-vector size))
	  (ctr 0)
	  (mx (maxamp snd chn))
	  (rd0 (make-sampler 0 snd chn))
	  (xhat 0.0)
	  (frm (make-formant :radius (- 1.0 (/ 500.0 (srate snd))) :frequency 600))
	  (del (make-moving-sum avg-size))
	  (K 0.0)
	  (maxg 0.0)
	  (ming 1000.0)
	  (maxK 0.0)
	  (minK 1000.0))
      
      (do ((i 0 (+ i 1)))
	  ((= i avg-size))
	(moving-sum del (formant frm (rd0))))
      
      (map-channel
       (lambda (datum)
	 (let ((xhatminus xhat)
	       (avg (moving-sum del (formant frm (rd0)))))
	   
	   (set! K (min 1.0 (+ .1 (/ avg 100.0))))
	   ;;	 (set! K .5)
	   
	   (set! (avg-data ctr) K)
	   (set! ctr (+ ctr 1))
	   
	   (set! maxg (max maxg avg))
	   (set! ming (min ming avg))
	   (set! maxK (max maxK K))
	   (set! minK (min minK K))
	   
	   (set! xhat (+ xhatminus
			 (* K (- datum xhatminus))))
	   xhatminus))
       0 size snd chn)

      (scale-channel (/ mx (maxamp snd chn)) snd chn))))


(define* (clean-channel snd chn)

  ;; look for obvious simple clicks
  (let ((clicks (as-one-edit (lambda () (remove-single-sample-clicks 8 snd chn)))))
    (format () (if (> clicks 0)
		   (values "~%; fixed ~D single sample clicks" clicks)
		   "~%; no single-sample clicks found")))

  ;; look for obvious clipping and try to reconstruct
  (let ((mx (maxamp snd chn)))
    (if (>= mx 1.0)
	(let ((clips (unclip-channel snd chn)))
	  (format () (if (eq? clips 'no-clips)
			 "~%; no clipped portions found"
			 (values "~%; reconstructed ~D clipped portions" (list-ref clips 3)))))
	(format () "~%; no obvious clipping (max amp: ~A)" mx)))

  ;; look for pops
  (let ((total-pops 0))
    (call-with-exit
     (lambda (quit)
       (for-each
	(lambda (size)
	  (let ((pops (as-one-edit (lambda () (remove-pops size snd chn)))))
	    (set! total-pops (+ total-pops pops))
	    (if (> pops 0)
		(format () "~%; fixed ~D ~D-sample ~A" pops size (if (= pops 1) "pop" "pops"))
		(quit))))
	'(4 8 16 32))))
    (if (= total-pops 0)
	(format () "~%; no pops found")))

  ;; look for hum
  (let* ((hum60 (check-freq 60.0 snd chn))
	 (hum55 (check-freq 55.0 snd chn))
	 (hum (max hum60 hum55)))
    (if (> hum 30.0)
	(let ((humf (if (> hum60 hum55) 60.0 55.0)))
	  (notch-channel (list humf) 4096 0 (framples snd chn) snd chn #f #t 4)
	  (format () "~%; notch out ~D cycle hum: ~A -> ~A" (floor humf) hum (check-freq humf snd chn)))))

  ;; look for DC
  (let ((dc (check-freq 0.0 snd chn)))
    (if (> dc 30.0)
	(let ((dcflt (make-filter 2 #r(1 -1) #r(0 -0.99))))
	  (map-channel (lambda (y) (filter dcflt y)) 0 (framples snd chn) snd chn)
	  (format () "~%; block DC: ~A -> ~A" dc (check-freq 0.0 snd chn)))))

  ;; time-varying low-pass filter
  (tvf-channel snd chn)
  )


(define* (clean-sound snd)
  (let ((index (or snd (selected-sound) (car (sounds)))))
    (if (not (sound? index))
	(error 'no-such-sound (list "clean-sound" snd))
	(let ((chns (channels index)))
	  (do ((chn 0 (+ chn 1)))
	      ((= chn chns))
	    (clean-channel index chn))))))
