;;; versions of the Moore-Klingbeil-Trevisani-Edwards phase-vocoder

(provide 'snd-pvoc.scm)

(define make-pvocoder 
  (let ((+documentation+ "(make-pvocoder fftsize overlap interp analyze edit synthesize) makes a new (Scheme-based, not CLM) phase-vocoder generator"))

    (lambda* (fftsize overlap interp analyze edit synthesize)
      (let ((N (or fftsize 512)))
	(let ((N2 (floor (/ N 2)))
	      (D (floor (/ N (or overlap 4)))))
	  
	  ;; basic: fftsize overlap
	  ;;  everything else via closures (interp in particular)
	  ;;  pv: counter ("output" here)
	  ;;      interp
	  ;;      fftsize ("N"), hop ("D")
	  ;;      in-counter ("filptr")
	  ;;      hamming window scaled
	  ;;      slot for in-coming data ("in-data") (created on first call)
	  ;;      float-vectors: ampinc amp freqinc phaseinc phase lastphase
	  ;;      funcs: analysize, edit, resynthesize
	  
	  (list 
	   interp                        ;output
	   interp                        ;interp
	   0                             ;filptr
	   N                             ;N
	   (let ((window (make-fft-window hamming-window fftsize)))
	     (float-vector-scale! window (/ 2.0 (* 0.54 fftsize))) ;den = hamming window integrated
	     window)                     ; window
	   D                             ;D
	   #f                            ;in-data (created in pvocoder gen)
	   (make-float-vector fftsize)            ;ampinc
	   (make-float-vector fftsize)            ;freqs
	   (make-float-vector N2)                 ;amps
	   (make-float-vector N2)                 ;phaseinc
	   (make-float-vector N2)                 ;phases
	   (make-float-vector N2)                 ;lastphaseinc
	   analyze
	   edit
	   synthesize))))))

;;; pvocoder generator: 
;;     input data func
;;     analysis func with fallback
;;     editing func with fallback 
;;     resynthesis func with fallback

(define pvocoder 
  (let ((+documentation+ "(pvocoder pv input) is the phase-vocoder generator associated with make-pvocoder")
	;; pvocoder list accessors
	(pvoc-output (lambda (pv) (pv 0)))
	(set-pvoc-output (lambda (pv val) (set! (pv 0) val)))
	(pvoc-interp (lambda (pv) (pv 1)))
	(pvoc-filptr (lambda (pv) (pv 2)))
	(set-pvoc-filptr (lambda (pv val) (set! (pv 2) val)))
	(pvoc-N (lambda (pv) (pv 3)))
	(pvoc-window (lambda (pv) (pv 4)))
	(pvoc-D (lambda (pv) (pv 5)))
	(pvoc-in-data (lambda (pv) (pv 6)))
	(set-pvoc-in-data (lambda (pv val) (set! (pv 6) val)))
	(pvoc-ampinc (lambda (pv) (pv 7)))
	(pvoc-freqs (lambda (pv) (pv 8)))
	(pvoc-amps (lambda (pv) (pv 9)))
	(pvoc-phaseinc (lambda (pv) (pv 10)))
	(pvoc-phases (lambda (pv) (pv 11)))
	(pvoc-lastphase (lambda (pv) (pv 12)))
	(pvoc-analyze (lambda (pv) (pv 13)))
	(pvoc-edit (lambda (pv) (pv 14)))
	(pvoc-synthesize (lambda (pv) (pv 15)))
	(sine-bank (lambda* (amps phases size)
		     (let ((len (or size (length amps)))
			   (sum 0.0))
		       (do ((i 0 (+ i 1)))
			   ((= i len))
			 (set! sum (+ sum (* (amps i) (sin (phases i))))))
		       sum)))
	(pi2 (* 2.0 pi)))

    (lambda (pv input)
      (when (>= (pvoc-output pv) (pvoc-interp pv))
	;; get next block of amp/phase info
	(let ((N (pvoc-N pv))
	      (D (pvoc-D pv))
	      (amps (pvoc-ampinc pv))
	      (freqs (pvoc-freqs pv))
	      (filptr (pvoc-filptr pv)))
	  
	  (if (pvoc-analyze pv)
	      ((pvoc-analyze pv) pv input)
	      ;; if no analysis func:
	      (begin
		(fill! freqs 0.0)
		(set-pvoc-output pv 0)
		(if (not (pvoc-in-data pv))
		    (begin
		      (set-pvoc-in-data pv (make-float-vector N))
		      (do ((i 0 (+ i 1)))
			  ((= i N))
			(set! ((pvoc-in-data pv) i) (input))))
		    (let ((indat (pvoc-in-data pv)))
		      ;; extra loop here since I find the optimized case confusing (we could dispense with the data move)
		      (float-vector-move! indat 0 D)
		      (do ((i (- N D) (+ i 1)))
			  ((= i N))
			(set! (indat i) (input)))))
		(let ((buf (modulo filptr N)))
		  (if (= buf 0)
		      (begin
			(fill! amps 0.0)
			(float-vector-add! amps (pvoc-in-data pv))
			(float-vector-multiply! amps (pvoc-window pv)))
		      (do ((k 0 (+ k 1)))
			  ((= k N))
			(set! (amps buf) (* ((pvoc-window pv) k) ((pvoc-in-data pv) k)))
			(set! buf (+ 1 buf))
			(if (= buf N) (set! buf 0)))))
		(set-pvoc-filptr pv (+ filptr D))
		(mus-fft amps freqs N 1)
		(rectangular->polar amps freqs)))
	  
	  (if (pvoc-edit pv)
	      ((pvoc-edit pv) pv)
	      (let ((lp (pvoc-lastphase pv))
		    (pscl (/ 1.0 D))
		    (kscl (/ pi2 N))
		    (lim (floor (/ N 2))))
		;; if no editing func:
		(do ((k 0 (+ k 1)))
		    ((= k lim))
		  (let ((phasediff (remainder (- (freqs k) (lp k)) pi2)))
		    (float-vector-set! lp k (freqs k))
		    (if (> phasediff pi) (set! phasediff (- phasediff pi2))
			(if (< phasediff (- pi)) (set! phasediff (+ phasediff pi2))))
		    (set! (freqs k) (+ (* pscl phasediff) (* k kscl)))))))
	  
	  (let ((scl (/ 1.0 (pvoc-interp pv))))
	    (float-vector-subtract! amps (pvoc-amps pv))
	    (float-vector-subtract! freqs (pvoc-phaseinc pv))
	    (float-vector-scale! amps scl)
	    (float-vector-scale! freqs scl)
	    )))
      
      (set-pvoc-output pv (+ 1 (pvoc-output pv)))
      
      (if (pvoc-synthesize pv)
	  ((pvoc-synthesize pv) pv)
	  ;; if no synthesis func:
	  ;; synthesize next sample
	  (begin
	    (float-vector-add! (pvoc-amps pv) (pvoc-ampinc pv))
	    (float-vector-add! (pvoc-phaseinc pv) (pvoc-freqs pv))
	    (float-vector-add! (pvoc-phases pv) (pvoc-phaseinc pv))
	    (sine-bank (pvoc-amps pv) (pvoc-phases pv))))
      )))

#|
(let* ((ind (open-sound "oboe.snd"))
       (pv (make-pvocoder 256 4 64))
       (rd (make-sampler 0)))
  (map-channel (lambda (y) (pvocoder pv (lambda () (rd))))))
|#

#|
;;; ---------------- same thing using phase-vocoder gen

(define test-pv-1
  (lambda (freq)
    (let* ((reader (make-sampler 0))
	   (pv (make-phase-vocoder (lambda (dir) (next-sample reader))
				   512 4 128 1.0
				   #f ;no change to analysis
				   #f ;no change to edits
				   #f ;no change to synthesis
				   )))
      (map-channel (lambda (val) (phase-vocoder pv))))))

(define test-pv-2
  (lambda (freq)
    (let* ((reader (make-sampler 0))
	   (pv (make-phase-vocoder (lambda (dir) (next-sample reader))
				   512 4 128 freq
				   #f ;no change to analysis
				   #f
				   #f ; no change to synthesis
				   )))
      (map-channel (lambda (val) (phase-vocoder pv))))))

(define test-pv-3
  (lambda (time)
    (let* ((reader (make-sampler 0))
	   (pv (make-phase-vocoder (lambda (dir) (next-sample reader))
				   512 4 (floor (* 128 time)) 1.0
				   #f ;no change to analysis
				   #f ;no change to edits
				   #f ;no change to synthesis
				   ))
	   (len (floor (* time (framples))))
	   (data (make-float-vector len)))
      (do ((i 0 (+ i 1)))
	  ((= i len))
	(set! (data i) (phase-vocoder pv)))
      (free-sampler reader)
      (float-vector->channel data 0 len))))

(define test-pv-4
  (lambda (gate)
    (let* ((reader (make-sampler 0))
	   (pv (make-phase-vocoder (lambda (dir) (next-sample reader))
				   512 4 128 1.0
				   #f ;no change to analysis
				   (lambda (v)
				     (let ((N (mus-length v)))
				       (do ((i 0 (+ i 1)))
					   ((= i N))
					 (if (< ((phase-vocoder-amp-increments v) i) gate)
					     (float-vector-set! (phase-vocoder-amp-increments v) i 0.0)))
				       #t))
				   #f ;no change to synthesis
				   )))
      (map-channel (lambda (val) (phase-vocoder pv))))))
|#


;;; -------- another version of the phase vocoder --------

(define pvoc
  (let ((+documentation+ "(pvoc fftsize overlap time pitch gate hoffset snd chn) applies the phase vocoder
  algorithm to the current sound (i.e. fft analysis, oscil bank resynthesis). 'pitch'
  specifies the pitch transposition ratio, 'time' - specifies the time dilation ratio,
  'gate' specifies a resynthesis gate in dB (partials with amplitudes lower than
  the gate value will not be synthesized), 'hoffset is a pitch offset in Hz.")
	(pi2 (* 2.0 pi)))
    
    (lambda* ((fftsize 512) (overlap 4) (time 1.0)
	      (pitch 1.0) (gate 0.0) (hoffset 0.0)
	      (snd 0) (chn 0))
      (let* ((len (framples))
	     (N2 (floor (/ fftsize 2)))
	     (window (make-fft-window hamming-window fftsize))
	     (in-data (channel->float-vector 0 (* fftsize 2) snd chn))
	     (lastamp (make-float-vector N2))
	     (lastfreq (make-float-vector N2))
	     (outlen (floor (* time len)))
	     (interp (* (floor (/ fftsize overlap)) time))
	     (obank (make-oscil-bank lastfreq (make-float-vector N2) lastamp))
	     (filptr 0)
	     (D (floor (/ fftsize overlap)))
	     (syngate (if (= 0.0 gate)    ; take a resynthesis gate specificed in dB, convert to linear amplitude
			  0.0000
			  (expt 10 (/ (- (abs gate)) 20))))
	     (poffset (hz->radians hoffset))
	     (fdr (make-float-vector fftsize))
	     (fdi (make-float-vector fftsize))
	     (lastphase (make-float-vector N2))
	     (ampinc (make-float-vector N2))
	     (freqinc (make-float-vector N2))
	     (fundamental (/ pi2 fftsize))
	     (output interp)
	     (out-data (make-float-vector (max len outlen)))
	     (in-data-beg 0))
	
	(set! window (float-vector-scale! window (/ 2.0 (* 0.54 fftsize)))) ;den = hamming window integrated
	
	(do ((i 0 (+ i 1)))
	    ((>= i outlen))
	  (when (>= output interp) ;; if all the samples have been output then do the next frame
	    (let ((buffix (modulo filptr fftsize)))
					; buffix is the index into the input buffer
					; it wraps around circularly as time increases in the input
	      (set! output 0)       ; reset the output sample counter
	      ;; save the old amplitudes and frequencies
	      (fill! lastamp 0.0)
	      (fill! lastfreq 0.0)
	      (float-vector-add! lastamp fdr)
	      (float-vector-add! lastfreq fdi)
	      (do ((k 0 (+ k 1)))
		  ((= k fftsize))
		;; apply the window and then stuff into the input array
		(set! (fdr buffix) (* (window k) (in-data (- filptr in-data-beg))))
		(set! filptr (+ 1 filptr))
		;; increment the buffer index with wrap around
		(set! buffix (+ 1 buffix))
		(if (>= buffix fftsize) (set! buffix 0)))
	      ;; rewind the file for the next hop
	      (set! filptr (- (+ filptr D) fftsize))
	      (if (> filptr (+ in-data-beg fftsize))
		  (begin
		    (set! in-data-beg filptr)
		    (set! in-data (channel->float-vector in-data-beg (* fftsize 2) snd chn))))
	      ;; no imaginary component input so zero out fdi
	      (fill! fdi 0.0)
	      ;; compute the fft
	      (mus-fft fdr fdi fftsize 1)
	      ;; now convert into magnitude and interpolated frequency
	      (do ((k 0 (+ k 1)))
		  ((= k N2))
		(let ((a (fdr k))
		      (b (fdi k))
		      (phasediff 0))			  
		  (let ((mag (sqrt (+ (* a a) (* b b)))))
		    (set! (fdr k) mag)    ;; current amp stored in fdr
		    ;; mag is always positive
		    ;; if it is zero then the phase difference is zero
		    (if (> mag 0)
			(let ((phase (- (atan b a))))
			  (set! phasediff (- phase (lastphase k)))
			  (set! (lastphase k) phase)
			  ;; frequency wrapping from Moore p. 254
			  (if (> phasediff pi) (do () ((<= phasediff pi)) (set! phasediff (- phasediff pi2))))
			  (if (< phasediff (- pi)) (do () ((>= phasediff (- pi))) (set! phasediff (+ phasediff pi2)))))))
		  ;; current frequency stored in fdi
		  ;; scale by the pitch transposition
		  (set! (fdi k) (* pitch (+ (/ phasediff D) (* k fundamental) poffset))))
		;; resynthesis gating
		(if (< (fdr k) syngate) (set! (fdr k) 0.0))
		;; take (lastamp k) and count up to (fdr k)
		;; interpolating by ampinc
		(set! (ampinc k) (/ (- (fdr k) (lastamp k)) interp))
		;; take (lastfreq k) and count up to (fdi k)
		;; interpolating by freqinc
		(set! (freqinc k) (/ (- (fdi k) (lastfreq k)) interp)))))
	  ;; loop over the partials interpolate frequency and amplitude
	  (float-vector-add! lastamp ampinc)
	  (float-vector-add! lastfreq freqinc)
	  (float-vector-set! out-data i (oscil-bank obank))
	  (set! output (+ 1 output)))
	(float-vector->channel out-data 0 (max len outlen))))))
