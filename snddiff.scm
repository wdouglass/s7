(provide 'snd-snddiff.scm)


(define (cross-correlate snd0 chn0 snd1 chn1)
  (let ((fftlen (floor (expt 2 (ceiling (log (max (framples snd0 chn0) (framples snd1 chn1)) 2))))))
    (correlate (channel->float-vector 0 fftlen snd1 chn1) 
	       (channel->float-vector 0 fftlen snd0 chn0))))

(define (lag-time snd0 chn0 snd1 chn1)
  ;; returns the probable lagtime between the two sounds (negative means second sound is delayed)
  (let ((corr (cross-correlate snd0 chn0 snd1 chn1)))
    (let ((len (length corr))
	  (lag (cadr (float-vector-peak-and-location corr))))
      (if (= lag -1)
	  0
	  (if (< lag (/ len 2))
	      lag
	      (- lag len))))))
  

(define* (snddiff-1 v0 v1 (maxdiff 0.0))
  (let ((diff (float-vector-subtract! (copy v0) v1)))
    (if (<= (float-vector-peak diff) maxdiff)
	'no-difference
	(do ((diffs 0)
	     (diff-data ())
	     (len (min (length v0) (length v1)))
	     (i 0 (+ i 1)))
	    ((or (> diffs 10)
		 (= i len))
	     (and (< diffs 10)
		  (list 'differences diff-data)))
	  (when (> (abs (diff i)) .00001)
	    (set! diffs (+ diffs 1))
	    (set! diff-data (cons (list i (v0 i) (v1 i)) diff-data)))))))


(define (float-vector-size v)
  (sqrt (dot-product v v)))

(define (unconvolve-1 v0 v1 impulse-response)  ; assume here that v0 is the original and that we're aligned, and both are trimmed at the front
  (let ((pos -1)
	(len (length v0)))

    (do ((i 0 (+ i 1)))
	((or (>= pos 0)
	     (= i len)))
      (if (not (= (v1 i) 0.0))
	  (set! pos i)))
    
    (if (< pos 0) ; if still -1, must be all zero 
	impulse-response
	(let ((scl (/ (v1 pos) (v0 0)))
	      (size (float-vector-size v1)))
	  (float-vector-subtract! 
	   (float-vector-move! v1 0 pos)            ; align new copy with original (todo: move doesn't clear trailing entries)
	   (float-vector-scale! (copy v0) scl)) ; subtract original scaled to fit first none zero point

	  (if (< (float-vector-size v1) size)
	      (unconvolve-1 v0 v1 (cons (list scl pos) impulse-response))
	      impulse-response)))))

(define (unconvolve v0 v1)
  (and (float-vector? v0) 
       (float-vector? v1)
       (let ((trim -1)
	     (len (min (length v0) (length v1))))
	 (do ((i 0 (+ i 1)))
	     ((or (> trim -1)
		  (= i len)))
	   (if (not (and (= (v0 i) 0.0)
			 (= (v1 i) 0.0)))
	       (set! trim i)))
	 (if (> trim 0)
	     (begin
	       (float-vector-move! v0 0 trim)
	       (float-vector-move! v1 0 trim)))
	 (let ((result (unconvolve-1 v0 (copy v1) ())))
	   (and (pair? result)
		(list 'filter (reverse result)))))))
  

(define (snddiff-2 snd0 chn0 snd1 chn1)
  ;; this can currently find initial delays, scaling differences, and scattered individual sample differences
  (let ((len0 (framples snd0 chn0))
	(len1 (framples snd1 chn1)))

    (or (and (= len0 len1)
	     (let ((s0 (channel->float-vector 0 #f snd0 chn0))
		   (s1 (channel->float-vector 0 #f snd1 chn1)))
	       (or (snddiff-1 s0 s1 0.0)
		   (let* ((scl (let ((pos (maxamp-position snd0 chn0)))
				 (/ (sample pos snd1 chn1) (sample pos snd0 chn0))))  ; use actual values to keep possible sign difference
			  (diff (snddiff-1 (float-vector-scale! s0 scl) s1)))
		     (if (eq? diff 'no-difference)
			 (list 'scale scl)
			 (and (list? diff)
			      (list 'scale scl 'differences diff)))))))

	;; align sounds and  zero out any non-overlapping sections, keeping track of whether they are zero beforehand
	(let ((lag (lag-time snd0 chn0 snd1 chn1))
	      (pre0 #f)
	      (pre1 #f))
	  (if (> lag 0)
	      (begin
		(pad-channel 0 lag snd1 chn1)
		(set! pre0 (float-vector-peak (channel->float-vector 0 lag snd0 chn0)))
		(if (> pre0 0.0)
		    (scale-channel 0.0 0 lag snd0 chn0)))
	      (if (< lag 0)
		  (let ((pad (- lag)))
		    (pad-channel 0 pad snd0 chn0)
		    (set! pre1 (float-vector-peak (channel->float-vector 0 pad snd1 chn1)))
		    (if (> pre1 0.0)
			(scale-channel 0.0 0 pad snd1 chn1)))))

	  (set! len0 (framples snd0 chn0))
	  (set! len1 (framples snd1 chn1))
	  (let ((post0 #f)
		(post1 #f))
	    (if (> len0 len1)
		(let ((dur (- len0 len1)))
		  (set! post0 (float-vector-peak (channel->float-vector len1 dur snd0 chn0)))
		  (scale-channel 0.0 len1 dur snd0 chn0))
		(if (> len1 len0)
		    (let ((dur (- len1 len0)))
		      (set! post1 (float-vector-peak (channel->float-vector len0 dur snd1 chn1)))
		      (scale-channel 0.0 len0 dur snd1 chn1))))
	    
	    (let ((s0 (channel->float-vector 0 #f snd0 chn0))
		  (s1 (channel->float-vector 0 #f snd1 chn1)))
	      (or (let ((res (snddiff-1 s0 s1 0.0)))
		    (and res
			 (if (> lag 0)
			     (list 'lag lag res pre0 pre1 post0 post1)
			     (list res pre0 pre1 post0 post1))))
		  (let* ((scl (let ((pos (maxamp-position snd0 chn0)))
				(/ (sample pos snd1 chn1) (sample pos snd0 chn0)))) ; use actual values to keep possible sign difference
			 (diff (snddiff-1 (float-vector-scale! s0 scl) s1 0.0001)))
		    (if (eq? diff 'no-difference)
			(list 'scale scl 'lag lag pre0 pre1 post0 post1)
			(and (list? diff)
			     (list 'scale scl 'differences diff 'lag lag pre0 pre1 post0 post1)))))))
	  ;; align and zero + scaling didn't find a match
	  ))))


(define (snddiff snd0 chn0 snd1 chn1)
  ;; a wrapper for snddiff to put things back the way they were
  (let ((edpos0 (edit-position snd0 chn0))
	(edpos1 (edit-position snd1 chn1)))
    (let ((result (snddiff-2 snd0 chn0 snd1 chn1)))
      (set! (edit-position snd0 chn0) edpos0)
      (set! (edit-position snd1 chn1) edpos1)
      (or result
	  (unconvolve (channel->float-vector 0 #f snd0 chn0) (channel->float-vector 0 #f snd1 chn1))))))

;; for env: slam both to 1 at every peak, check for eq, see if smooth env gives match?
;;   or check spectr for eq leaving out low stuff, then try env?