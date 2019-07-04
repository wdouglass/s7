;;; a DSP-related grabbag

(provide 'snd-dsp.scm)
(if (provided? 'snd)
    (require snd-ws.scm)
    (require sndlib-ws.scm))
(require snd-env.scm)

(when (provided? 'pure-s7)
  (define (make-polar mag ang)
    (if (and (real? mag) (real? ang))
	(complex (* mag (cos ang)) (* mag (sin ang)))
	(error 'wrong-type-arg "make-polar args should be real"))))


(define binomial
  (let ((+documentation+ "(binomial n k) computes the binomial coefficient C(N,K)"))
    (lambda (n k)
      (let ((mn (min k (- n k))))
	(if (< mn 0)
	    0
	    (if (= mn 0)
		1
		(let ((mx (max k (- n k))))
		  (do ((cnk (+ 1 mx))
		       (i 2 (+ 1 i)))
		      ((> i mn) cnk)
		    (set! cnk (/ (* cnk (+ mx i)) i))))))))))

(define log10
  (let ((+documentation+ "(log10 a) returns the log base 10 of 'a'"))
    (lambda (a)
      (log a 10))))

;;; src-duration (see src-channel in extsnd.html)

(define src-duration
  (let ((+documentation+ "(src-duration envelope) returns the new duration of a sound after using 'envelope' for time-varying sampling-rate conversion"))
    (lambda (e)
      (let ((len (- (length e) 2)))
	(do ((all-x (- (e len) (e 0))) ; last x - first x
	     (dur 0.0)
	     (i 0 (+ i 2)))
	    ((>= i len) dur)
	  (let ((area (let ((x0 (e i))
			    (x1 (e (+ i 2)))
			    (y0 (e (+ i 1))) ; 1/x x points
			    (y1 (e (+ i 3))))
			(if (< (abs (real-part (- y0 y1))) .0001)
			    (/ (- x1 x0) (* y0 all-x))
			    (/ (* (- (log y1) (log y0)) 
				  (- x1 x0)) 
			       (* (- y1 y0) all-x))))))
	    ;; or (* (/ (- (log y1) (log y0)) (- y1 y0)) 
	    ;;    or (/ (log (/ y1 y0)) (- y1 y0))
	    ;;       (/ (- x1 x0) all-x)
	    (set! dur (+ dur (abs area)))))))))

;;; :(src-duration '(0 1 1 2))
;;; 0.69314718055995
;;; :(src-duration #(0 1 1 2))
;;; 0.69314718055995

(define (src-fit-envelope e target-dur)
  (scale-envelope e (/ (src-duration e) target-dur))) ; scale-envelope is in env.scm


;;; -------- Dolph-Chebyshev window
;;; 
;;; formula taken from Richard Lyons, "Understanding DSP"
;;; see clm.c for C version (using either GSL's or GCC's complex trig functions)

(define dolph
  (let ((+documentation+ "(dolph n gamma) produces a Dolph-Chebyshev FFT data window of 'n' points using 'gamma' as the window parameter."))
    (lambda (N gamma)
      (let ((rl (make-float-vector N))
	    (im (make-float-vector N)))
	(let ((alpha (cosh (/ (acosh (expt 10.0 gamma)) N))))
	  (do ((den (/ 1.0 (cosh (* N (acosh alpha)))))
	       (freq (/ pi N))
	       (i 0 (+ i 1))
	       (phase 0.0))
	      ((= i N))
	    (let ((val (* den (cos (* N (acos (* alpha (cos phase))))))))
	      (set! (rl i) (real-part val))
	      (set! (im i) (imag-part val))) ;this is always essentially 0.0
	    (set! phase (+ phase freq))))
	(fft rl im -1)            ;direction could also be 1
	(float-vector-scale! rl (/ 1.0 (float-vector-peak rl)))
	(do ((i 0 (+ i 1))
	     (j (/ N 2)))
	    ((= i N))
	  (set! (im i) (rl j))
	  (set! j (+ j 1))
	  (if (= j N) (set! j 0)))
	im))))


;;; this version taken from Julius Smith's "Spectral Audio..." with three changes
;;;   it does the DFT by hand, and is independent of anything from Snd (fft, float-vectors etc)

(define dolph-1
  (let ((+documentation+ "(dolph-1 n gamma) produces a Dolph-Chebyshev FFT data window of 'n' points using 'gamma' as the window parameter."))
    (lambda (N gamma)
      (let ((vals (make-vector N)))
	(let ((alpha (cosh (/ (acosh (expt 10.0 gamma)) N))))
	  (do ((den (/ 1.0 (cosh (* N (acosh alpha)))))
	       (freq (/ pi N))
	       (mult -1 (- mult))
	       (i 0 (+ i 1))
	       (phase (* -0.5 pi)))
	      ((= i N))
	    (set! (vals i) (* mult den (cos (* N (acos (* alpha (cos phase)))))))
	    (set! phase (+ phase freq))))
	;; now take the DFT
	(let ((pk 0.0)
	      (w (make-vector N)))
	  (do ((i 0 (+ i 1))
	       (sum 0.0 0.0))
	      ((= i N))
	    (do ((k 0 (+ k 1)))
		((= k N))
	      (set! sum (+ sum (* (vals k) (exp (/ (* 2.0 0+1.0i pi k i) N))))))
	    (set! (w i) (magnitude sum))
	    (set! pk (max pk (w i))))
	  ;; scale to 1.0 (it's usually pretty close already, that is pk is close to 1.0)
	  (do ((i 0 (+ i 1)))
	      ((= i N))
	    (set! (w i) (/ (w i) pk)))
	  w)))))


;;; -------- move sound down by n (a power of 2)

(define down-oct
  (let ((+documentation+ "(down-n n) moves a sound down by n-1 octaves"))
    (lambda* (n snd chn)
      ;; I think this is "stretch" in DSP jargon -- to interpolate in the time domain we're squeezing the frequency domain
      ;;  the power-of-2 limitation is based on the underlying fft function's insistence on power-of-2 data sizes
      ;;  see stretch-sound-via-dft below for a general version
      (let* ((len (framples snd chn))
	     (fftlen (floor (expt 2 (ceiling (log len 2))))))
	(let ((fftlen2 (/ fftlen 2))
	      (fft-1 (- (* n fftlen) 1))
	      (fftscale (/ 1.0 fftlen))
	      (rl1 (channel->float-vector 0 fftlen snd chn))
	      (im1 (make-float-vector fftlen)))
	  (fft rl1 im1 1)
	  (float-vector-scale! rl1 fftscale)
	  (float-vector-scale! im1 fftscale)
	  (let ((rl2 (make-float-vector (* n fftlen)))
		(im2 (make-float-vector (* n fftlen))))
	    (copy rl1 rl2 0 fftlen2)
	    (copy im1 im2 0 fftlen2)
	    (do ((k (- fftlen 1) (- k 1))
		 (j fft-1 (- j 1)))
		((= k fftlen2))
	      (float-vector-set! rl2 j (float-vector-ref rl1 k))
	      (float-vector-set! im2 j (float-vector-ref im1 k)))
	    (fft rl2 im2 -1)
	    (float-vector->channel rl2 0 (* n len) snd chn #f (format #f "down-oct ~A" n))))))))
  
(define stretch-sound-via-dft 
  (let ((+documentation+ "(stretch-sound-via-dft factor snd chn) makes the given channel longer ('factor' should be > 1.0) by \
squeezing in the frequency domain, then using the inverse DFT to get the time domain result."))
    (lambda* (factor snd chn)
      ;; this is very slow! factor>1.0
      (let* ((n (framples snd chn))
	     (out-n (round (* n factor))))
	(let ((out-data (make-float-vector out-n))
	      (fr (make-vector out-n 0.0))
	      (freq (/ (* 2 pi) n)))
	  (do ((in-data (channel->float-vector 0 n snd chn))
	       (n2 (floor (/ n 2.0)))
	       (i 0 (+ i 1)))
	      ((= i n))
	    ;; DFT + split
	    (set! (fr (if (< i n2) i (- (+ i out-n) n 1))) 
		  (edot-product (* freq 0.0-1.0i i) in-data)))
	  (set! freq (/ (* 2 pi) out-n))
	  (do ((i 0 (+ i 1)))
	      ((= i out-n))
	    ;; inverse DFT
	    (set! (out-data i) (real-part (/ (edot-product (* freq 0.0+1.0i i) fr) n))))
	  (float-vector->channel out-data 0 out-n snd chn #f (format #f "stretch-sound-via-dft ~A" factor)))))))
  


;;; -------- vibrating-uniform-circular-string
;;;
;;; this is a simplification of the underlying table-filling routine for "scanned synthesis".
;;; To watch the wave, open some sound (so Snd has some place to put the graph), turn off
;;; the time domain display (to give our graph all the window -- to do this in a much more
;;; elegant manner, see snd-motif.scm under scanned-synthesis).

#|
(define old-vibrating-uniform-circular-string
  (lambda (size x0 x1 x2 mass xspring damp)
    (define circle-ref 
      (lambda (v i)
	(if (< i 0)
	    (v (+ size i))
	    (if (>= i size)
		(v (- i size))
		(v i)))))
    (let* ((dm (/ damp mass))
	   (km (/ xspring mass))
	   (denom (+ 1.0 dm))
	   (p1 (/ (+ 2.0 (- dm (* 2.0 km))) denom))
	   (p2 (/ km denom))
	   (p3 (/ -1.0 denom)))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(set! (x0 i) (+ (* p1 (x1 i))
			(* p2 (+ (circle-ref x1 (- i 1)) (circle-ref x1 (+ i 1))))
			(* p3 (x2 i)))))
      (copy x1 x2)
      (copy x0 x1))))

;;; (vibrating-uniform-circular-string 100 (make-float-vector 100) (make-float-vector 100) (make-float-vector 100) .5 .5 .5)
|#

(define (vibrating-uniform-circular-string size x0 x1 x2 mass xspring damp)
  (let ((dm (/ damp mass))
	(km (/ xspring mass)))
    (let ((denom (+ 1.0 dm)))
      (let ((p2 (/ km denom))
	    (s1 (- size 1)))
	(float-vector-scale! x2 (/ -1.0 denom))
	(copy x1 x0)
	(float-vector-scale! x0 (/ (- (+ 2.0 dm) (* 2.0 km)) denom))
	(float-vector-add! x0 x2)
	(set! (x0 0) (+ (x0 0) (* p2 (+ (x1 s1) (x1 1)))))
	(do ((i 1 (+ i 1)))
	    ((= i s1))
	  (float-vector-set! x0 i (+ (float-vector-ref x0 i) (* p2 (+ (float-vector-ref x1 (- i 1)) (float-vector-ref x1 (+ i 1)))))))
	(set! (x0 s1) (+ (x0 s1) (* p2 (+ (x1 (- s1 1)) (x1 0))))))))
  (copy x1 x2)
  (copy x0 x1))

(define (vibrating-string size x0 x1 x2 masses xsprings esprings damps haptics)
  ;; this is the more general form
  (do ((i 0 (+ i 1)))
      ((= i size))
    (let ((mass (masses i)))
      (let ((dm (/ (damps i) mass))
	    (km (/ (xsprings i) mass))
	    (cm (/ (esprings i) mass)))
	(let ((denom (+ 1.0 dm cm)))
	  (let ((p1 (/ (- (+ 2.0 dm) (* 2.0 km)) denom))
		(p2 (/ km denom))
		(p3 (/ -1.0 denom))
		(p4 (/ (haptics i) (* mass denom)))
		(j (if (= i 0) size (- i 1)))
		(k (if (= i (- size 1)) 0 (+ i 1))))
	    (set! (x0 i) (+ (* p1 (x1 i))
			    (* p2 (+ (x1 j) (x1 k)))
			    (* p3 (x2 i))
			    p4)))))))
  (copy x1 x2)
  (copy x0 x1))


;;; -------- "frequency division" -- an effect from sed_sed@my-dejanews.com

(define freqdiv
  (let ((+documentation+ "(freqdiv n snd chn) repeats each nth sample n times (clobbering the intermediate samples): (freqdiv 8)"))
    (lambda* (n snd chn)
      (let* ((data (channel->float-vector 0 #f snd chn))
	     (len (length data)))
	(if (> n 1)
	    (do ((i 0 (+ i n)))
		((>= i len)
		 (float-vector->channel data 0 len snd chn))
	      (let ((val (data i))
		    (stop (min len (+ i n))))
		(do ((k (+ i 1) (+ k 1)))
		    ((= k stop))
		  (float-vector-set! data k val)))))))))


;;; -------- "adaptive saturation" -- an effect from sed_sed@my-dejanews.com
;;;
;;; a more extreme effect is "saturation":
;;;   (map-channel (lambda (val) (if (< (abs val) .1) val (if (>= val 0.0) 0.25 -0.25))))

(define adsat
  (let ((+documentation+ "(adsat size beg dur snd chn) is an 'adaptive saturation' sound effect"))
    (lambda* (size (beg 0) dur snd chn)
      (let* ((len (if (number? dur) dur (- (framples snd chn) beg)))
	     (data (make-float-vector (* size (ceiling (/ len size))))))
	(let ((reader (make-sampler beg snd chn))
	      (mn 0.0)
	      (mx 0.0)
	      (vals (make-float-vector size)))
	  (do ((i 0 (+ i size)))
	      ((>= i len))
	    (do ((k 0 (+ k 1)))
		((= k size))
	      (float-vector-set! vals k (next-sample reader)))
	    (set! mn (float-vector-min vals))
	    (set! mx (float-vector-max vals))
	    (do ((k 0 (+ k 1)))
		((= k size))
	      (float-vector-set! data (+ i k) (if (negative? (float-vector-ref vals k)) mn mx))))
	  (float-vector->channel data beg len snd chn current-edit-position (format #f "adsat ~A ~A ~A" size beg dur)))))))


;;; -------- spike
;;;
;;; makes sound more spikey -- sometimes a nice effect

#|
(define spike
  (let ((+documentation+ "(spike snd chn) multiplies successive samples together to make a sound more spikey"))
    (lambda* (snd chn)
      (let* ((len (framples snd chn))
	     (data (make-float-vector len))
	     (amp (maxamp snd chn))) ; keep resultant peak at maxamp
	(let ((reader (make-sampler 0 snd chn))
	      (x1 0.0)
	      (x2 0.0)
	      (amp1 (/ 1.0 (* amp amp))))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (let ((x0 (next-sample reader)))
	      (float-vector-set! data i (* x0 x1 x2))
	      (set! x2 x1)
	      (set! x1 (abs x0))))
	  (float-vector->channel (float-vector-scale! data amp1) 0 len snd chn current-edit-position "spike"))))))
|#
(define spike
  (let ((+documentation+ "(spike snd chn) multiplies successive samples together to make a sound more spikey"))
    (lambda* (snd chn)
      (let ((len (framples snd chn)))
	(let ((data (channel->float-vector 0 (+ len 2) snd chn))
	      (amp (maxamp snd chn)) ; keep resultant peak at maxamp
	      ;; multiply x[0]*x[1]*x[2]
	      (data1 (make-float-vector (+ len 1))))
	  (copy data data1 1)
	  (float-vector-abs! (float-vector-multiply! data1 data))
	  (float-vector-multiply! data (subvector data1 len 1))
	  (let ((amp1 (/ amp (float-vector-peak data))))
	    (float-vector->channel (float-vector-scale! data amp1) 0 len snd chn current-edit-position "spike")))))))

;;; the more successive samples we include in the product, the more we
;;;   limit the output to pulses placed at (just after) wave peaks


;;; -------- easily-fooled autocorrelation-based pitch tracker 

(define spot-freq
  (let ((+documentation+ "(spot-freq samp snd chn) tries to determine the current pitch: (spot-freq (left-sample))"))
    (lambda* (s0 snd chn)
      (let* ((fftlen (floor (expt 2 (ceiling (log (/ (srate snd) 20.0) 2)))))
	     (data (autocorrelate (channel->float-vector s0 fftlen snd chn)))
	     (cor-peak (float-vector-peak data)))
	(if (= cor-peak 0.0)
	    0.0
	    (call-with-exit
	     (lambda (return)
	       (do ((i 1 (+ i 1)))
		   ((= i (- fftlen 2)) 0)
		 (if (and (< (data i) (data (+ i 1)))
			  (> (data (+ i 1)) (data (+ i 2))))
		     (let ((offset (let ((logla (log10 (/ (+ cor-peak (data i)) (* 2 cor-peak))))
					 (logca (log10 (/ (+ cor-peak (data (+ i 1))) (* 2 cor-peak))))
					 (logra (log10 (/ (+ cor-peak (data (+ i 2))) (* 2 cor-peak)))))
				     (/ (* 0.5 (- logla logra))
					(+ logla logra (* -2.0 logca))))))
		       (return (/ (srate snd)
				  (* 2 (+ i 1 offset))))))))))))))

					;(hook-push graph-hook 
					;	   (lambda (hook)
					;	     (status-report (format #f "~A" (spot-freq (left-sample))))))



;;; -------- chorus (doesn't always work and needs speedup)
(define chorus-size 5)

(define chorus
  (let ((+documentation+ "(chorus) tries to produce the chorus sound effect")
	(flanger (lambda (dly inval)
		   (+ inval 
		      (delay (car dly)
			     inval
			     (rand-interp (cadr dly)))))))
    (lambda ()
      (let ((make-flanger
	     (let ((chorus-time .05)
		   (chorus-amount 20.0)
		   (chorus-speed 10.0))
	       (lambda ()
		 (let ((ri (make-rand-interp :frequency chorus-speed :amplitude chorus-amount))
		       (len (floor (random (* 3.0 chorus-time (srate))))))
		   (list (make-delay len :max-size (+ len chorus-amount 1)) ri)))))
	    (dlys (make-vector chorus-size)))

	(do ((i 0 (+ i 1)))
	    ((= i chorus-size))
	  (set! (dlys i) (make-flanger)))
	(lambda (inval)
	  (do ((sum 0.0)
	       (i 0 (+ i 1)))
	      ((= i chorus-size)
	       (* .25 sum))
	    (set! sum (+ sum (flanger (dlys i) inval)))))))))


;;; -------- chordalize (comb filters to make a chord using chordalize-amount and chordalize-base)
(define chordalize-amount .95)
(define chordalize-base 100)
(define chordalize-chord '(1 3/4 5/4))

(define chordalize
  (let ((+documentation+ "(chordalize) uses harmonically-related comb-filters to bring out a chord in a sound"))
    (lambda ()
      ;; chord is a list of members of chord such as '(1 5/4 3/2)
      (let ((combs (make-comb-bank (apply vector (map (lambda (interval)
							(make-comb chordalize-amount (floor (* chordalize-base interval))))
						      chordalize-chord))))
	    (scaler (/ 0.5 (length chordalize-chord)))) ; just a guess -- maybe this should rescale to old maxamp
	(lambda (x)
	  (* scaler (comb-bank combs x)))))))


;;; -------- zero-phase, rotate-phase
;;; fft games (from the "phazor" package of Scott McNab)

(define zero-phase
  (let ((+documentation+ "(zero-phase snd chn) calls fft, sets all phases to 0, and un-ffts"))
    (lambda* (snd chn)
      (let* ((len (framples snd chn))
	     (fftlen (floor (expt 2 (ceiling (log len 2)))))
	     (rl (channel->float-vector 0 fftlen snd chn)))
	(let ((fftscale (/ 1.0 fftlen))
	      (old-pk (float-vector-peak rl))
	      (im (make-float-vector fftlen)))
	  (if (> old-pk 0.0)
	      (begin
		(fft rl im 1)
		(rectangular->magnitudes rl im)
		(float-vector-scale! rl fftscale)
		(float-vector-scale! im 0.0)
		(fft rl im -1)
		(let ((pk (float-vector-peak rl)))
		  (float-vector->channel (float-vector-scale! rl (/ old-pk pk)) 0 len snd chn #f "zero-phase")))))))))

(define rotate-phase
  (let ((+documentation+ "(rotate-phase func snd chn) calls fft, applies func to each phase, then un-ffts"))
    (lambda* (func snd chn)
      (let* ((len (framples snd chn))
	     (fftlen (floor (expt 2 (ceiling (log len 2)))))
	     (rl (channel->float-vector 0 fftlen snd chn)))
	(let ((fftlen2 (floor (/ fftlen 2)))
	      (fftscale (/ 1.0 fftlen))
	      (old-pk (float-vector-peak rl))
	      (im (make-float-vector fftlen)))
	  (if (> old-pk 0.0)
	      (begin
		(fft rl im 1)
		(rectangular->magnitudes rl im)
		(float-vector-scale! rl fftscale)
		(set! (im 0) 0.0)
		(do ((i 1 (+ i 1))
		     (j (- fftlen 1) (- j 1)))
		    ((= i fftlen2))
		  ;; rotate the fft vector by func, keeping imaginary part complex conjgate of real
		  (set! (im i) (func (im i)))
		  (set! (im j) (- (im i))))
		(polar->rectangular rl im)
		(fft rl im -1)
		(let ((pk (float-vector-peak rl)))
		  (float-vector->channel (float-vector-scale! rl (/ old-pk pk)) 0 len snd chn #f 
					 (format #f "rotate-phase ~A" (procedure-source func)))))))))))

#|
(rotate-phase (lambda (x) 0.0)) is the same as (zero-phase)
(rotate-phase (lambda (x) (random pi))) randomizes phases
(rotate-phase (lambda (x) x)) returns original
(rotate-phase (lambda (x) (- x))) reverses original (might want to write fftlen samps here)
(rotate-phase (lambda (x) (* x 2))) reverb-effect (best with voice)
(rotate-phase (lambda (x) (* x 12)) "bruise blood" effect
|#

(define (signum n)
  ;; in CL this returns 1.0 if n is float
  (if (positive? n) 1
      (if (zero? n) 0
	  -1)))


;;; -------- brighten-slightly

(define brighten-slightly
  (let ((+documentation+ "(brighten-slightly amount snd chn) is a form of contrast-enhancement ('amount' between ca .1 and 1)"))
    (lambda* (amount snd chn)
      (let ((len (framples snd chn)))
	(let ((mx (maxamp))
	      (brt (* 2 pi amount))
	      (data (make-float-vector len))
	      (reader (make-sampler 0 snd chn)))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (float-vector-set! data i (sin (* (next-sample reader) brt))))
	  (float-vector->channel 
	   (float-vector-scale! data (/ mx (float-vector-peak data))) 0 len snd chn current-edit-position (format #f "brighten-slightly ~A" amount)))))))

(define brighten-slightly-1
  (let ((+documentation+ "(brighten-slightly-1 coeffs) is a form of contrast-enhancement: (brighten-slightly-1 '(1 .5 3 1))"))
    (lambda (coeffs)
      (let ((pcoeffs (partials->polynomial coeffs))
	    (mx (maxamp))
	    (len (framples)))
	(let ((data (make-float-vector len))
	      (reader (make-sampler 0)))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (float-vector-set! data i (polynomial pcoeffs (next-sample reader))))
	  (float-vector->channel (float-vector-scale! data (/ mx (float-vector-peak data)))))))))




;;; -------- FIR filters

;;; Snd's (very simple) spectrum->coefficients procedure is:

(define spectrum->coeffs
  (let ((+documentation+ "(spectrum->coeffs order spectr) returns FIR filter coefficients given the filter order and desired spectral envelope (a float-vector)"))
    (lambda (order spectr)
      (let ((coeffs (make-float-vector order))
	    (m (floor (/ (+ order 1) 2)))
	    (am (* 0.5 (+ order 1)))
	    (q (/ (* pi 2.0) order)))
	(if (not (float-vector? spectr))
	    (error 'wrong-type-arg "spectrum->coeffs spectrum argument should be a float-vector: ~A" spectr))
	(do ((j 0 (+ j 1))
	     (jj (- order 1) (- jj 1)))
	    ((= j m) coeffs)
	  (let ((xt (* 0.5 (spectr 0))))
	    (do ((i 1 (+ i 1)))
		((= i m))
	      (set! xt (+ xt (* (spectr i) (cos (* q i (- am j 1)))))))
	    (let ((coeff (* 2.0 (/ xt order))))
	      (set! (coeffs j) coeff)
	      (set! (coeffs jj) coeff))))))))

;; (filter-channel et al reflect around the midpoint, so to match exactly you need to take
;;   the env passed and flip it backwards for the back portion -- that is to say, this function
;;   needs a wrapper to make it act like anyone would expect)


(define fltit-1
  (let ((+documentation+ "(fltit-1 order spectrum) creates an FIR filter from spectrum and order and returns a closure that calls it: \
  (map-channel (fltit-1 10 (float-vector 0 1.0 0 0 0 0 0 0 1.0 0)))"))
    (lambda (order spectr)
      (let ((flt (make-fir-filter order (spectrum->coeffs order spectr))))
	(lambda (x)
	  (fir-filter flt x))))))

					;(map-channel (fltit-1 10 (float-vector 0 1.0 0 0 0 0 0 0 1.0 0)))
					;
					;(let ((notched-spectr (make-float-vector 40)))
					;  (set! (notched-spectr 2) 1.0)  
					;  (set! (notched-spectr 37) 1.0)
					;  (map-channel (fltit-1 40 notched-spectr)))
					;

;;; -------- Hilbert transform

(define make-hilbert-transform
  (let ((+documentation+ "(make-hilbert-transform (len 30)) makes a Hilbert transform filter"))
    (lambda* ((len 30))
      (let ((arrlen (+ 1 (* 2 len))))
	(do ((arr (make-float-vector arrlen))
	     (lim (if (even? len) len (+ 1 len)))
	     (i (- len) (+ i 1)))
	    ((= i lim)
	     (make-fir-filter arrlen arr))
	  (let ((k (+ i len))
		(denom (* pi i))
		(num (- 1.0 (cos (* pi i)))))
	    (set! (arr k)
		  (if (or (= num 0.0) 
			  (= i 0))
		      0.0
		      ;; this is the "ideal" -- rectangular window -- version:
		      ;; (set! (arr k) (/ num denom))
		      ;; this is the Hamming window version:
		      (* (/ num denom) 
			 (+ .54 (* .46 (cos (/ (* i pi) len))))))))))))) ; window

(define hilbert-transform fir-filter)

#|
(let ((h (make-hilbert-transform 15)))
  (map-channel (lambda (y)
		 (hilbert-transform h y))))

;;; this comes from R Lyons:
(define* (sound->amp-env snd chn)
  (let ((hlb (make-hilbert-transform 40))
	(d (make-delay 40)))
    (map-channel
     (lambda (y)
       (let ((hy (hilbert-transform hlb y))
	     (dy (delay d y)))
	 (sqrt (+ (* hy hy) (* dy dy)))))
     0 #f snd chn #f "sound->amp-env")))

(define* (hilbert-transform-via-fft snd chn)
  ;; same as FIR version but use FFT and change phases by hand
  ;; see snd-test.scm test 18 for a faster version
  (let* ((size (framples snd chn))
	 (len (expt 2 (ceiling (log size 2.0))))
	 (rl (make-float-vector len))
	 (im (make-float-vector len))
	 (rd (make-sampler 0 snd chn))
	 (pi2 (* 0.5 pi)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (set! (rl i) (rd)))
    (mus-fft rl im len)
    (do ((i 0 (+ i 1)))
	((= i len))
      (let* ((c (complex (rl i) (im i)))
	     (ph (angle c))
	     (mag (magnitude c)))
	(if (< i (/ len 2))
	    (set! ph (+ ph pi2))
	    (set! ph (- ph pi2)))
	(set! c (make-polar mag ph))
	(set! (rl i) (real-part c))
	(set! (im i) (imag-part c))))
    (mus-fft rl im len -1)
    (float-vector-scale! rl (/ 1.0 len))
    (float-vector->channel rl 0 len snd chn #f "hilbert-transform-via-fft")))
|#

;;; -------- highpass filter 

(define make-highpass
  (let ((+documentation+ "(make-highpass fc (len 30)) makes an FIR highpass filter"))
    (lambda* (fc (len 30))
      (let ((arrlen (+ 1 (* 2 len))))
	(do ((arr (make-float-vector arrlen))
	     (i (- len) (+ i 1)))
	    ((= i len)
	     (make-fir-filter arrlen arr))
	  (let ((k (+ i len))
		(denom (* pi i))
		(num (- (sin (* fc i)))))
	    (set! (arr k)
		  (if (= i 0)
		      (- 1.0 (/ fc pi))
		      (* (/ num denom) 
			 (+ .54 (* .46 (cos (/ (* i pi) len)))))))))))))
	

(define highpass fir-filter)

#|
(let ((hp (make-highpass (* .1 pi))))
  (map-channel (lambda (y)
		 (highpass hp y))))
|#


;;; -------- lowpass filter

(define make-lowpass
  (let ((+documentation+ "(make-lowpass fc (len 30)) makes an FIR lowpass filter"))
    (lambda* (fc (len 30))
      (let ((arrlen (+ 1 (* 2 len))))
	(do ((arr (make-float-vector arrlen))
	     (i (- len) (+ i 1)))
	    ((= i len)
	     (make-fir-filter arrlen arr))
	  (let ((k (+ i len))
		(denom (* pi i))
		(num (sin (* fc i))))
	    (set! (arr k)
		  (if (= i 0)
		      (/ fc pi)
		      (* (/ num denom) 
			 (+ .54 (* .46 (cos (/ (* i pi) len)))))))))))))

(define lowpass fir-filter)

#|
(let ((hp (make-lowpass (* .2 pi))))
  (map-channel (lambda (y)
		 (lowpass hp y))))
|#

;;; -------- bandpass filter

(define make-bandpass
  (let ((+documentation+ "(make-bandpass flo fhi (len 30)) makes an FIR bandpass filter"))
    (lambda* (flo fhi (len 30))
      (let ((arrlen (+ 1 (* 2 len))))
	(do ((arr (make-float-vector arrlen))
	     (i (- len) (+ i 1)))
	    ((= i len)
	     (make-fir-filter arrlen arr))     
	  (let ((k (+ i len))
		(denom (* pi i))
		(num (- (sin (* fhi i)) (sin (* flo i)))))
	    (set! (arr k)
		  (if (= i 0)
		      (/ (- fhi flo) pi)
		      (* (/ num denom) 
			 (+ .54 (* .46 (cos (/ (* i pi) len)))))))))))))

(define bandpass fir-filter)

#|
(let ((hp (make-bandpass (* .1 pi) (* .2 pi))))
  (map-channel (lambda (y)
		 (bandpass hp y))))

;; for more bands, you can add the coeffs:

(define* (make-bandpass-2 flo1 fhi1 flo2 fhi2 (len 30))
  (let* ((f1 (make-bandpass flo1 fhi1 len))
	 (f2 (make-bandpass flo2 fhi2 len)))
    (float-vector-add! (mus-xcoeffs f1) (mus-xcoeffs f2))
    f1))

(let ((ind (new-sound "test.snd")))
  (map-channel (lambda (y) (mus-random 1.0)) 0 10000)
  (let ((f2 (make-bandpass-2 (* .12 pi) (* .15 pi) (* .22 pi) (* .25 pi) 100)))
    (map-channel (lambda (y) (fir-filter f2 y)))
    ))

|#

;;; -------- bandstop filter

(define make-bandstop
  (let ((+documentation+ "(make-bandstop flo fhi (len 30)) makes an FIR bandstop (notch) filter"))
    (lambda* (flo fhi (len 30))
      (let ((arrlen (+ 1 (* 2 len))))
	(do ((arr (make-float-vector arrlen))
	     (i (- len) (+ i 1)))
	    ((= i len)
	     (make-fir-filter arrlen arr))     
	  (let ((k (+ i len))
		(denom (* pi i))
		(num (- (sin (* flo i)) (sin (* fhi i)))))
	    (set! (arr k)
		  (if (= i 0)
		      (- 1.0 (/ (- fhi flo) pi))
		      (* (/ num denom) 
			 (+ .54 (* .46 (cos (/ (* i pi) len)))))))))))))

(define bandstop fir-filter)

#|
(let ((hp (make-bandstop (* .1 pi) (* .3 pi))))
  (map-channel (lambda (y)
		 (bandstop hp y))))
|#

;;; -------- differentiator

(define make-differentiator
  (let ((+documentation+ "(make-differentiator (len 30)) makes an FIR differentiator (highpass) filter"))
    (lambda* ((len 30))
      (let ((arrlen (+ 1 (* 2 len))))
	(do ((arr (make-float-vector arrlen))
	     (i (- len) (+ i 1)))
	    ((= i len)
	     (make-fir-filter arrlen arr))     
	  (let ((k (+ i len)))
	    (set! (arr k) 
		  (if (= i 0)
		      0.0
		      (* (/ (cos (* pi i)) i)
			 (+ .54 (* .46 (cos (/ (* i pi) len)))))))))))))
	
(define differentiator fir-filter)

#|
(let ((hp (make-differentiator)))
  (map-channel (lambda (y)
		 (differentiator hp y))))
|#


;;; -------- IIR filters
;;; see analog-filter.scm for the usual suspects

;;; -------- Butterworth filters (see also further below -- make-butter-lp et al)
;;;
;; translated from CLM butterworth.cl:
;;
;;   Sam Heisz, January 1998
;;   inspired by some unit generators written for Csound by Paris Smaragdis
;;   who based his work on formulas from 
;;   Charles Dodge, Computer music: synthesis, composition, and performance.

(define butter filter)

(define make-butter-high-pass
  (let ((+documentation+ "(make-butter-high-pass freq) makes a Butterworth filter with high pass cutoff at 'freq'"))
    (lambda (fq)
      ;; this is the same as iir-low-pass-2 below with 'din' set to (sqrt 2.0) -- similarly with the others
      (let* ((r (tan (/ (* pi fq) (srate))))
	     (r2 (* r r))
	     (c1 (/ 1.0 (+ 1.0 (* r (sqrt 2.0)) r2))))
	(make-filter 3
		     (float-vector c1 
				   (* -2.0 c1)
				   c1)
		     (float-vector 0.0 
				   (* 2.0 (- r2 1.0) c1)
				   (* (- (+ 1.0 r2) (* r (sqrt 2.0))) c1)))))))

(define make-butter-low-pass
  (let ((+documentation+ "(make-butter-low-pass freq) makes a Butterworth filter with low pass cutoff at 'freq'.  The result \
can be used directly: (filter-sound (make-butter-low-pass 500.0)), or via the 'butter' generator"))
    (lambda (fq)
      (let* ((r (/ 1.0 (tan (/ (* pi fq) (srate)))))
	     (r2 (* r r))
	     (c1 (/ 1.0 (+ 1.0 (* r (sqrt 2.0)) r2))))
	(make-filter 3
		     (float-vector c1 (* 2.0 c1) c1)
		     (float-vector 0.0 
				   (* 2.0 (- 1.0 r2) c1)
				   (* (- (+ 1.0 r2) (* r (sqrt 2.0))) c1)))))))

(define make-butter-band-pass
  (let ((+documentation+ "(make-butter-band-pass freq band) makes a bandpass Butterworth filter with low edge at 'freq' and width 'band'"))
    (lambda (fq bw)
      (let ((c (/ 1.0 (tan (/ (* pi bw) (srate))))))
	(let ((d (* 2.0 (cos (/ (* 2.0 pi fq) (srate)))))
	      (c1 (/ 1.0 (+ 1.0 c))))
	  (make-filter 3
		       (float-vector c1 0.0 (- c1))
		       (float-vector 0.0 
				     (* (- c) d c1)
				     (* (- c 1.0) c1))))))))

(define make-butter-band-reject
  (let ((+documentation+ "(make-butter-band-reject freq band) makes a band-reject Butterworth filter with low edge at 'freq' and width 'band'"))
    (lambda (fq bw)
      (let* ((c (tan (/ (* pi bw) (srate))))
	     (c1 (/ 1.0 (+ 1.0 c)))
	     (c2 (* c1 -2.0 (cos (/ (* 2.0 pi fq) (srate))))))
	(make-filter 3
		     (float-vector c1 c2 c1)
		     (float-vector 0.0 c2 (* (- 1.0 c) c1)))))))

;;; simplest use is (filter-sound (make-butter-low-pass 500.0))
;;; see also effects.scm


;;; from "DSP Filter Cookbook" by Lane et al, Prompt Pubs, 2001
;;; 
;;; use with the filter generator
;;;   (define gen (make-iir-high-pass-2 1000))
;;;   (filter gen 1.0)
;;;   etc

(define make-biquad
  (let ((+documentation+ "(make-biquad a0 a1 a2 b1 b2) returns a biquad filter (use with the CLM filter gen)"))
    (lambda (a0 a1 a2 b1 b2)
      (make-filter 3 
		   (float-vector a0 a1 a2) 
		   (float-vector 0.0 b1 b2)))))

(define (make-local-biquad a0 a1 a2 gamma beta)
  (make-biquad a0 a1 a2 (* -2.0 gamma) (* 2.0 beta)))

(define* (make-iir-low-pass-2 fc din) ; din=(sqrt 2.0) for example (suggested range 0.2.. 10)
  (let* ((theta (/ (* 2 pi fc) *clm-srate*))
	 (beta (let ((d (* (sin theta) (/ (or din (sqrt 2.0)) 2))))
		 (* 0.5 (/ (- 1.0 d) (+ 1.0 d)))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (* 0.5 (- (+ 0.5 beta) gamma))))
    (make-local-biquad alpha (* 2.0 alpha) alpha gamma beta)))

(define* (make-iir-high-pass-2 fc din)
  (let* ((theta (/ (* 2 pi fc) *clm-srate*))
	 (beta (let ((d (* (sin theta) (/ (or din (sqrt 2.0)) 2))))
		 (* 0.5 (/ (- 1.0 d) (+ 1.0 d)))))
	 (gamma (* (+ 0.5 beta) (cos theta)))
	 (alpha (* 0.5 (+ 0.5 beta gamma))))
    (make-local-biquad alpha (* -2.0 alpha) alpha gamma beta)))

(define (make-iir-band-pass-2 f1 f2)
  (let* ((theta (/ (* 2 pi (sqrt (* f1 f2))) *clm-srate*))
	 (beta (let ((t2 (tan (/ theta (* 2 (/ (sqrt (* f1 f2)) (- f2 f1)))))))
		 (* 0.5 (/ (- 1.0 t2) (+ 1.0 t2))))))
    (let ((gamma (* (+ 0.5 beta) (cos theta)))
	  (alpha (- 0.5 beta)))
      (make-local-biquad alpha 0.0 (- alpha) gamma beta))))

(define (make-iir-band-stop-2 f1 f2)
  (let* ((theta (/ (* 2 pi (sqrt (* f1 f2))) *clm-srate*))
	 (beta (let ((t2 (tan (/ theta (* 2 (/ (sqrt (* f1 f2)) (- f2 f1)))))))
		 (* 0.5 (/ (- 1.0 t2) (+ 1.0 t2))))))
    (let ((gamma (* (+ 0.5 beta) (cos theta)))
	  (alpha (+ 0.5 beta)))
      (make-local-biquad alpha (* -2.0 gamma) alpha gamma beta))))

#|
(define* (old-make-eliminate-hum (hum-freq 60.0) (hum-harmonics 5) (bandwidth 10))
  (let ((gen (make-vector hum-harmonics)))
    (do ((i 0 (+ i 1)))
	((= i hum-harmonics))
      (let ((center (* (+ i 1.0) hum-freq))
	    (b2 (* 0.5 bandwidth)))
	(set! (gen i) (make-iir-band-stop-2 (- center b2) (+ center b2)))))
    gen))

(define (old-eliminate-hum gen x0)
  (do ((i 0 (+ i 1)))
      ((= i (length gen)) x0)
    (set! x0 (filter (vector-ref gen i) x0)))) ; "cascade" n filters

;;; (let ((hummer (old-make-eliminate-hum))) (map-channel (lambda (x) (old-eliminate-hum hummer x))))
|#

;;; the new form is creating a function/closure of the form:
;;;  (let ((g0 (make-iir-band-stop-2 c00 c01)) (g1 (make-iir-band-stop-2 c10 c11))) (lambda (y) (filter g1 (filter g0 y))))

(define-macro* (make-eliminate-hum (hum-freq 60.0) (hum-harmonics 5) (bandwidth 10))
  `(let ((body 'y)
	 (closure ()))
     (do ((i 0 (+ i 1)))
	 ((= i ,hum-harmonics))
       (let ((filt (string->symbol (format #f "g~D" i)))
	     (center (* (+ i 1.0) ,hum-freq))
	     (b2 (* 0.5 ,bandwidth)))
	 (set! body (list 'filter filt body))
	 (set! closure (cons (list filt (list 'make-iir-band-stop-2 (- center b2) (+ center b2))) closure))))
     (apply let closure 
	    `((lambda (y) ,body)))))

;;; (map-channel (make-eliminate-hum))


(define (make-peaking-2 f1 f2 m)
  ;; bandpass, m is gain at center of peak
  ;; use map-channel with this one (not clm-channel or filter)
  (let ((flt (let* ((theta (/ (* 2 pi (sqrt (* f1 f2))) *clm-srate*))
		    (beta (let ((t2 (* (/ 4.0 (+ m 1)) (tan (/ theta (* 2 (/ (sqrt (* f1 f2)) (- f2 f1))))))))
			    (* 0.5 (/ (- 1.0 t2) (+ 1.0 t2))))))
	       (let ((gamma (* (+ 0.5 beta) (cos theta)))
		     (alpha (- 0.5 beta)))
		 (make-local-biquad alpha 0.0 (- alpha) gamma beta))))
	(m1 (- m 1.0)))
    (lambda (x) (+ x (* m1 (filter flt x))))))


(define cascade->canonical
  (let ((+documentation+ "(cascade->canonical A) converts a list of cascade coeffs (float-vectors with 3 entries) to canonical form")
	(conv (lambda (M h L x y)        ; x * h -> y
		(do ((n 0 (+ n 1)))
		    ((= n (+ L M)))
		  (let ((sum 0.0)
			(start (max 0 (- n L 1)))
			(end (min n M)))
		    (do ((m start (+ m 1)))
			((> m end))
		      (set! sum (+ sum (* (h m) (x (- n m))))))
		    (set! (y n) sum))))))
    ;; from Orfanidis "Introduction to Signal Processing"
    (lambda (A)
      (let ((K (length A)))
	(let ((d (make-float-vector (+ 1 (* 2 K))))
	      (a1 (make-float-vector (+ 1 (* 2 K)))))
	  (set! (a1 0) 1.0)
	  (do ((i 0 (+ i 1)))
	      ((= i K))
	    (conv 2 (A i) (+ 1 (* 2 i)) a1 d)
	    (copy d a1 0 (+ 3 (* 2 i))))
	  a1)))))


(define make-butter-lp
  (let ((+documentation+ "(make-butter-lp M fc) returns a butterworth low-pass filter; its order is 'M' * 2, 'fc' is the cutoff frequency in Hz"))
    (lambda (M fc)
      (let ((theta (/ (* 2 pi fc) *clm-srate*)))
	(do ((xcoeffs ())
	     (ycoeffs ())
	     (st (sin theta))
	     (ct (cos theta))
	     (k 1 (+ k 1)))
	    ((> k M)
	     (make-filter (+ 1 (* 2 M))
			  (cascade->canonical xcoeffs)
			  (cascade->canonical ycoeffs)))
	  (let* ((beta (let ((d (* st (sin (/ (* pi (- (* 2 k) 1)) (* 4 M))))))
			 (* 0.5 (/ (- 1.0 d) (+ 1.0 d)))))
		 (gamma (* ct (+ 0.5 beta)))
		 (alpha (* 0.25 (- (+ 0.5 beta) gamma))))
	    (set! xcoeffs (cons (float-vector (* 2 alpha) (* 4 alpha) (* 2 alpha)) xcoeffs))
	    (set! ycoeffs (cons (float-vector 1.0 (* -2.0 gamma) (* 2.0 beta)) ycoeffs))))))))

(define make-butter-hp
  (let ((+documentation+ "(make-butter-hp M fc) returns a butterworth high-pass filter; its order is 'M' * 2, 'fc' is the cutoff frequency in Hz"))
    (lambda (M fc)
      (let ((theta (/ (* 2 pi fc) *clm-srate*)))
	(do ((xcoeffs ())
	     (ycoeffs ())
	     (st (sin theta))
	     (ct (cos theta))
	     (k 1 (+ k 1)))
	    ((> k M)
	     (make-filter (+ 1 (* 2 M))
			  (cascade->canonical xcoeffs)
			  (cascade->canonical ycoeffs)))
	  (let* ((beta (let ((d (* st (sin (/ (* pi (- (* 2 k) 1)) (* 4 M))))))
			 (* 0.5 (/ (- 1.0 d) (+ 1.0 d)))))
		 (gamma (* ct (+ 0.5 beta)))
		 (alpha (* 0.25 (+ 0.5 beta gamma))))
	    (set! xcoeffs (cons (float-vector (* 2 alpha) (* -4 alpha) (* 2 alpha)) xcoeffs))
	    (set! ycoeffs (cons (float-vector 1.0 (* -2.0 gamma) (* 2.0 beta)) ycoeffs))))))))

(define make-butter-bp
  (let ((+documentation+ "(make-butter-bp M f1 f2) returns a butterworth band-pass filter; its order is 'M' * 2, 'f1' and 'f2' are the band edge frequencies in Hz"))
    (lambda (M f1 f2)
      (let* ((f0 (sqrt (* f1 f2)))
	     (theta0 (/ (* 2 pi f0) *clm-srate*))
	     (de (let ((Q (/ f0 (- f2 f1))))
		   (/ (* 2 (tan (/ theta0 (* 2 Q)))) (sin theta0)))))
	(do ((xcoeffs ())
	     (ycoeffs ())
	     (de2 (/ de 2))
	     (tn0 (tan (* theta0 0.5)))
	     (i 1 (+ i 1))
	     (k 1)
	     (j 1))
	    ((> i M)
	     (make-filter (+ 1 (* 2 M))
			  (cascade->canonical xcoeffs)
			  (cascade->canonical ycoeffs)))
	  (let* ((Dk (* 2 (sin (/ (* pi (- (* 2 k) 1)) (* 2 M)))))
		 (dk1 (let ((Ak (/ (+ 1 (* de2 de2)) (* Dk de2))))
			(sqrt (/ (* de Dk)
				 (+ Ak (sqrt (- (* Ak Ak) 1)))))))
		 (Wk (let ((Bk (* de2 (/ Dk dk1))))
		       (real-part (+ Bk (sqrt (- (* Bk Bk) 1.0)))))) ; fp inaccuracies causing tiny (presumably bogus) imaginary part here
		 (thetajk (* 2 (if (= j 1) (atan tn0 Wk) (atan (* tn0 Wk)))))
		 (betajk (* 0.5 (/ (- 1.0 (* 0.5 dk1 (sin thetajk)))
				   (+ 1.0 (* 0.5 dk1 (sin thetajk)))))))
	    (let ((gammajk (* (+ 0.5 betajk) (cos thetajk)))
		  (alphajk (let ((wk2 (/ (- Wk (/ 1.0 Wk)) dk1)))
			     (* 0.5 (- 0.5 betajk) (sqrt (+ 1.0 (* wk2 wk2)))))))
	      (set! xcoeffs (cons (float-vector (* 2 alphajk) 0.0 (* -2 alphajk)) xcoeffs))
	      (set! ycoeffs (cons (float-vector 1.0 (* -2.0 gammajk) (* 2.0 betajk)) ycoeffs))))
	  (if (= j 1)
	      (set! j 2)
	      (begin
		(set! k (+ k 1))
		(set! j 1))))))))
  
(define make-butter-bs
  (let ((+documentation+ "(make-butter-bs M f1 f2) returns a butterworth band-stop filter; its order is 'M' * 2, 'f1' and 'f2' are the band edge frequencies in Hz"))
    (lambda (M f1 f2)
      (let* ((f0 (sqrt (* f1 f2)))
	     (theta0 (/ (* 2 pi f0) *clm-srate*))
	     (de (let ((Q (/ f0 (- f2 f1))))
		   (/ (* 2 (tan (/ theta0 (* 2 Q)))) (sin theta0)))))
	(do ((xcoeffs ())
	     (ycoeffs ())
	     (de2 (/ de 2))
	     (ct (cos theta0))
	     (tn0 (tan (* theta0 0.5)))
	     (i 1 (+ i 1))
	     (k 1)
	     (j 1))
	    ((> i M)
	     (make-filter (+ 1 (* 2 M))
			  (cascade->canonical xcoeffs)
			  (cascade->canonical ycoeffs)))
	  (let* ((Dk (* 2 (sin (/ (* pi (- (* 2 k) 1)) (* 2 M)))))
		 (dk1 (let ((Ak (/ (+ 1 (* de2 de2)) (* Dk de2))))
			(sqrt (/ (* de Dk)
				 (+ Ak (sqrt (- (* Ak Ak) 1)))))))
		 (thetajk (let ((Wk (let ((Bk (* de2 (/ Dk dk1))))
				      (real-part (+ Bk (sqrt (- (* Bk Bk) 1.0)))))))
			    (* 2 (if (= j 1) (atan tn0 Wk) (atan (* tn0 Wk))))))
		 (betajk (* 0.5 (/ (- 1.0 (* 0.5 dk1 (sin thetajk)))
				   (+ 1.0 (* 0.5 dk1 (sin thetajk)))))))
	    (let ((gammajk (* (+ 0.5 betajk) (cos thetajk)))
		  (alphajk (/ (* 0.5 (+ 0.5 betajk) (- 1.0 (cos thetajk))) (- 1.0 ct))))
	      (set! xcoeffs (cons (float-vector (* 2 alphajk) (* -4 ct alphajk) (* 2 alphajk)) xcoeffs))
	      (set! ycoeffs (cons (float-vector 1.0 (* -2.0 gammajk) (* 2.0 betajk)) ycoeffs))))
	  (if (= j 1)
	      (set! j 2)
	      (begin
		(set! k (+ k 1))
		(set! j 1))))))))


;;; -------- notch filters

(define* (make-notch-frequency-response cur-srate freqs (notch-width 2))
  (let ((freq-response (list 1.0 0.0)))
    (for-each
     (lambda (i)
       (set! freq-response (cons 1.0 (cons (/ (* 2 (- i notch-width)) cur-srate) freq-response)))       ; left upper y resp hz
       (set! freq-response (cons 0.0 (cons (/ (* 2 (- i (/ notch-width 2))) cur-srate) freq-response))) ; left bottom y resp hz
       (set! freq-response (cons 0.0 (cons (/ (* 2 (+ i (/ notch-width 2))) cur-srate) freq-response))) ; right bottom y resp hz
       (set! freq-response (cons 1.0 (cons (/ (* 2 (+ i notch-width)) cur-srate) freq-response))))      ; right upper y resp hz
     freqs)
    (reverse (cons 1.0 (cons 1.0 freq-response)))))

(define notch-channel
  (let ((+documentation+ "(notch-channel freqs filter-order beg dur snd chn edpos (truncate #t) (notch-width 2)) -> notch filter removing freqs"))
    (lambda* (freqs filter-order beg dur snd chn edpos (truncate #t) (notch-width 2))
      (filter-channel (make-notch-frequency-response (* 1.0 (srate snd)) freqs notch-width)
		      (or filter-order (min (framples snd chn) (expt 2 (floor (log (/ (srate snd) notch-width) 2)))))
		      beg dur snd chn edpos truncate
		      (format #f "notch-channel '~A ~A ~A ~A" freqs filter-order beg dur)))))

(define notch-sound
  (let ((+documentation+ "(notch-sound freqs filter-order snd chn (notch-width 2)) -> notch filter removing freqs"))
    (lambda* (freqs filter-order snd chn (notch-width 2))
      (filter-sound (make-notch-frequency-response (* 1.0 (srate snd)) freqs notch-width)
		    (or filter-order (min (framples snd chn) (expt 2 (floor (log (/ (srate snd) notch-width) 2)))))
		    snd chn #f
		    (format #f "notch-channel '~A ~A 0 #f" freqs filter-order)))))

(define notch-selection
  (let ((+documentation+ "(notch-selection freqs filter-order (notch-width 2)) -> notch filter removing freqs"))
    (lambda* (freqs filter-order (notch-width 2))
      (if (selection?)
	  (filter-selection (make-notch-frequency-response (* 1.0 (selection-srate)) freqs notch-width)
			    (or filter-order (min (selection-framples) (expt 2 (floor (log (/ (selection-srate) notch-width) 2))))))))))
;; apparently I'm using powers of 2 here so that mus_make_fir_coeffs, called from get_filter_coeffs, can use an fft
;;   the others use the fft internally for the fir filter, but not filter-selection


;;; -------- fractional Fourier Transform, z transform
;;;
;;; translated from the fxt package of Joerg Arndt

(define fractional-fourier-transform
  (let ((+documentation+ "(fractional-fourier-transform real imaginary n angle) performs a fractional Fourier transform on data; if angle=1.0, you get a normal Fourier transform"))
    (lambda (fr fi n v)
      ;; this is the slow (dft) form
      ;; v=1 -> normal fourier transform
      (do ((hr (make-float-vector n))
	   (hi (make-float-vector n))
	   (ph0 (/ (* v 2 pi) n))
	   (w 0 (+ 1 w))
	   (sr 0.0 0.0)
	   (si 0.0 0.0))
	  ((= w n)
	   (list hr hi))     
	(do ((k 0 (+ k 1)))
	    ((= k n))
	  (let ((phase (* ph0 k w)))
	    (let ((c (cos phase))
		  (s (sin phase))
		  (x (fr k))
		  (y (fi k)))
	      (let ((r (- (* x c) (* y s)))
		    (i (+ (* y c) (* x s))))
		(set! sr (+ sr r))
		(set! si (+ si i))))))
	(set! (hr w) sr)
	(set! (hi w) si)))))

(define z-transform
  (let ((+documentation+ "(z-transform data n z) performs a Z transform on data; if z=e^2*pi*j/n you get a Fourier transform; complex results in returned vector"))
    (lambda (f n z)
      ;; using vector to allow complex sums (z=e^2*pi*i/n -> fourier transform)
      ;;   (z-transform data n (exp (complex 0.0 (* (/ 2.0 n) pi))))
      (let ((res ((if (float-vector? f) make-float-vector make-vector) n)))
	(do ((w 0 (+ 1 w)))
	    ((= w n))
	  (do ((sum 0.0)
	       (t 1.0)
	       (m (expt z w))
	       ;; -w?? there seems to be confusion here -- slowzt.cc in the fxt package uses +w
	       (k 0 (+ k 1)))
	      ((= k n)
	       (set! (res w) sum))
	    (set! sum (+ sum (* (f k) t)))
	    (set! t (* t m))))
	res))))



;;; -------- slow Hartley transform 

(define dht
  (let ((+documentation+ "(dht data) returns the Hartley transform of 'data'."))
    (lambda (data) 
      ;; taken from Perry Cook's SignalProcessor.m (the slow version of the Hartley transform)
      (let ((len (length data)) )
	(do ((arr (make-float-vector len))
	     (w (/ (* 2.0 pi) len))
	     (i 0 (+ i 1)))
	    ((= i len) arr)
	  (do ((j 0 (+ j 1)))
	      ((= j len))
	    (set! (arr i) (+ (arr i) 
			     (* (data j) 
				(+ (cos (* i j w)) 
				   (sin (* i j w))))))))))))

(define find-sine
  (let ((+documentation+ "(find-sine freq beg dur snd) returns the amplitude and initial-phase (for sin) at freq"))
    (lambda* (freq beg dur snd)
      (let ((incr (/ (* freq 2 pi) (srate snd)))
	    (sw 0.0)
	    (cw 0.0)
	    (reader (make-sampler beg snd))
	    (samp 0.0))
	(do ((i 0 (+ i 1)) ; this could also use edot-product
	     (x 0.0 (+ x incr)))
	    ((= i dur))
	  (set! samp (next-sample reader))
	  (set! sw (+ sw (* samp (sin x))))
	  (set! cw (+ cw (* samp (cos x)))))
	(list (* 2 (/ (sqrt (+ (* sw sw) (* cw cw))) dur))
	      (atan cw sw))))))

;;; this is a faster version of find-sine using the "Goertzel algorithm" taken from R Lyons "Understanding DSP" p 529
;;; it returns the same result as find-sine above if you take (* 2 (/ (goertzel...) dur)) -- see snd-test.scm examples

(define goertzel
  (let ((+documentation+ "(goertzel freq beg dur snd) returns the amplitude of the 'freq' spectral component"))
    (lambda* (freq (beg 0) dur snd)
      (let* ((rfreq (/ (* 2.0 pi freq) (srate snd)))
	     (cs (* 2.0 (cos rfreq))))
	(let ((reader (make-sampler beg snd 0))
	      (len (- (if (number? dur) dur (- (framples snd 0) beg)) 2))
	      (flt (make-two-pole 1.0 (- cs) 1.0)))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (two-pole flt (next-sample reader)))
	  (let ((y1 (two-pole flt (next-sample reader)))
		(y0 (two-pole flt (next-sample reader))))
	    (magnitude (- y0 (* y1 (exp (complex 0.0 (- rfreq))))))))))))

#|
;; old version:
(let ((y2 0.0)
      (y1 0.0)
      (y0 0.0)
      (reader (make-sampler beg snd 0))
      (len (if (number? dur) dur (- (framples snd 0) beg))))
  (do ((i 0 (+ i 1)))
      ((= i len))
    (set! y2 y1)
    (set! y1 y0)
    (set! y0 (+ (- (* y1 cs) y2) (next-sample reader))))
  |#


(define make-spencer-filter
  (let ((+documentation+ "(make-spencer-filter) is a version of make-fir-filter; it returns one of the standard smoothing filters from \
the era when computers were human beings"))
    (lambda ()
      (make-fir-filter 15 (apply float-vector (map (lambda (n) (/ n 320.0)) '(-3 -6 -5 3 21 46 67 74 67 46 21 3 -5 -6 -3)))))))


;;; -------- any-random
;;;
;;; arbitrary random number distributions via the "rejection method"

(define* (any-random amount e)
  (if (= amount 0.0)
      0.0
      (if (not e)
	  (random amount)
	  (let next-random ()
	    (let ((x (random (* 1.0 (e (- (length e) 2)))))
		  (y (random 1.0)))
	      (if (<= y (envelope-interp x e))
		  x
		  (next-random)))))))

(define (gaussian-distribution s)
  (do ((e ())
       (den (* 2.0 s s))
       (i 0 (+ i 1))
       (x 0.0 (+ x .05))
       (y -4.0 (+ y .4)))
      ((= i 21)
       (reverse e))
    (set! e (cons (exp (- (/ (* y y) den))) (cons x e)))))
  
(define (pareto-distribution a)
  (do ((e ())
       (scl (/ (expt 1.0 (+ a 1.0)) a))
       (i 0 (+ i 1))
       (x 0.0 (+ x .05))
       (y 1.0 (+ y .2)))
      ((= i 21)
       (reverse e))
      (set! e (cons (* scl (/ a (expt y (+ a 1.0)))) (cons x e)))))

					;(map-channel (lambda (y) (any-random 1.0 '(0 1 1 1)))) ; uniform distribution
					;(map-channel (lambda (y) (any-random 1.0 '(0 0 0.95 0.1 1 1)))) ; mostly toward 1.0
					;(let ((g (gaussian-distribution 1.0))) (map-channel (lambda (y) (any-random 1.0 g))))
					;(let ((g (pareto-distribution 1.0))) (map-channel (lambda (y) (any-random 1.0 g))))

;;; this is the inverse integration function used by CLM to turn a distribution function into a weighting function

(define* (inverse-integrate dist (data-size 512) (e-size 50))
  (let ((sum (cadr dist))
	(x0 (car dist)))
    (let ((first-sum sum)
	  (data (make-float-vector data-size))
	  (e ())
	  (xincr (/ (- (dist (- (length dist) 2)) x0) e-size)))
      (do ((i 0 (+ i 1))
	   (x x0 (+ x xincr)))
	  ((> i e-size))
	(set! e (cons x (cons sum e)))
	(set! sum (+ sum (envelope-interp x dist))))
      (let ((incr (/ (- (cadr e) first-sum) (- data-size 1))))
	(set! e (reverse e))
	(do ((i 0 (+ i 1))
	     (x first-sum (+ x incr)))
	    ((= i data-size))
	  (set! (data i) (envelope-interp x e)))
	data))))

(define (gaussian-envelope s)
  (do ((e ())
       (den (* 2.0 s s))
       (i 0 (+ i 1))
       (x -1.0 (+ x .1))
       (y -4.0 (+ y .4)))
      ((= i 21)
       (reverse e))       
    (set! e (cons (exp (- (/ (* y y) den))) (cons x e)))))

;;; (make-rand :envelope (gaussian-envelope 1.0))



;;; ---------------- Julius Smith stuff ----------------
;;;
;;; these are from "Mathematics of the DFT", W3K Pubs

(define channel-mean                     ; <f, 1> / n
  (let ((+documentation+ "(channel-mean snd chn) returns the average of the samples in the given channel: <f,1>/n"))
    (lambda* (snd chn)            
      (let ((N (framples snd chn))
	    (reader (make-sampler 0 snd chn))
	    (incr (make-one-pole 1.0 -1.0)))
	(do ((i 0 (+ i 1)))
	    ((= i N))
	  (one-pole incr (next-sample reader)))
	(/ (one-pole incr 0.0) N)))))

(define channel-total-energy             ; <f, f>
  (let ((+documentation+ "(channel-total-energy snd chn) returns the sum of the squares of all the samples in the given channel: <f,f>"))
    (lambda* (snd chn)    
      (let ((data (samples 0 (framples snd chn) snd chn)))
	(dot-product data data)))))

#|
(let ((sum 0.0)
      (N (framples snd chn))
      (reader (make-sampler 0 snd chn)))
  (do ((i 0 (+ i 1)))
      ((= i N))
    (let ((y (next-sample reader)))
      (set! sum (+ sum (* y y)))))
  sum))
|#

(define channel-average-power             ; <f, f> / n
  (let ((+documentation+ "(channel-average-power snd chn) returns the average power in the given channel: <f,f>/n"))
    (lambda* (snd chn)   
      (/ (channel-total-energy snd chn) (framples snd chn)))))

(define channel-rms                       ; sqrt(<f, f> / n)
  (let ((+documentation+ "(channel-rms snd chn) returns the RMS value of the samples in the given channel: sqrt(<f,f>/n)"))
    (lambda* (snd chn)             
      (sqrt (channel-average-power snd chn)))))

(define channel-variance                  ; "sample-variance" might be better, <f, f> - (<f, 1> / n) ^ 2 with quibbles
  (let ((+documentation+ "(channel-variance snd chn) returns the sample variance in the given channel: <f,f>-((<f,1>/ n)^2"))
    (lambda* (snd chn) 
      (let ((mu (let ((N (framples snd chn)))
		  (* (/ N (- N 1)) (channel-mean snd chn))))) ; avoid bias sez JOS
	(- (channel-total-energy snd chn) 
	   (* mu mu))))))

(define channel-norm                      ; sqrt(<f, f>)
  (let ((+documentation+ "(channel-norm snd chn) returns the norm of the samples in the given channel: sqrt(<f,f>)"))
    (lambda* (snd chn)            
      (sqrt (channel-total-energy snd chn)))))

(define channel-lp
  (let ((+documentation+ "(channel-lp p snd chn) returns the Lp norm of the samples in the given channel"))
    (lambda* (p snd chn)
      (let ((incr (make-one-pole 1.0 -1.0))
	    (N (framples snd chn))
	    (reader (make-sampler 0 snd chn)))
	(do ((i 0 (+ i 1)))
	    ((= i N))
	  (one-pole incr (expt (abs (next-sample reader)) p)))
	(expt (one-pole incr 0.0) (/ 1.0 p))))))

(define channel-lp-inf maxamp)
#|
"(channel-lp-inf snd chn) returns the maxamp in the given channel (the name is just math jargon for maxamp)"
(let ((mx 0.0)
      (N (framples snd chn))
      (reader (make-sampler 0 snd chn)))
  (do ((i 0 (+ i 1)))
      ((= i N))
    (set! mx (max mx (abs (next-sample reader)))))
  mx))
|#

(define channel2-inner-product            ; <f, g>
  (let ((+documentation+ "(channel2-inner-product s1 c1 s2 c2) returns the inner-product of the two channels: <f,g>"))
    (lambda (s1 c1 s2 c2)         
      (dot-product (samples 0 (framples s1 c1) s1 c1) (samples 0 (framples s1 c1) s2 c2)))))
#|
(let ((N (framples s1 c1))
      (sum 0.0)
      (r1 (make-sampler 0 s1 c1))
      (r2 (make-sampler 0 s2 c2)))
  (do ((i 0 (+ i 1)))
      ((= i N))
    (set! sum (+ sum (* (r1) (r2)))))
  sum))
|#

(define channel2-angle                    ; acos(<f, g> / (sqrt(<f, f>) * sqrt(<g, g>)))
  (let ((+documentation+ "(channel2-angle s1 c1 s2 c2) treats the two channels as vectors, returning the 'angle' between them: acos(<f,g>/(sqrt(<f,f>)*sqrt(<g,g>)))"))
    (lambda (s1 c1 s2 c2)                 
      (let ((inprod (channel2-inner-product s1 c1 s2 c2))
	    (norm1 (channel-norm s1 c1))
	    (norm2 (channel-norm s2 c2)))
	(acos (/ inprod (* norm1 norm2)))))))

(define channel2-orthogonal?             ; <f, g> == 0
  (let ((+documentation+ "(channel2-orthogonal? s1 c1 s2 c2) returns #t if the two channels' inner-product is 0: <f,g>==0"))
    (lambda (s1 c1 s2 c2)           
      (= (channel2-inner-product s1 c1 s2 c2) 0.0))))

(define channel2-coefficient-of-projection  ; s1,c1 = x, s2,c2 = y, <f, g> / <f, f>
  (let ((+documentation+ "(channel2-coefficient-of-projection s1 c1 s2 c2) returns <f,g>/<f,f>"))
    (lambda (s1 c1 s2 c2) 
      (/ (channel2-inner-product s1 c1 s2 c2)
	 (channel-total-energy s1 c1)))))

;;; -------- end of JOS stuff --------

#|
(define channel-distance-1              ; sqrt(<f - g, f - g>)
  (let ((+documentation+ "(channel-distance s1 c1 s2 c2) returns the euclidean distance between the two channels: sqrt(<f-g,f-g>)"))
    (lambda* ((s1 0) (c1 0) (s2 1) (c2 0))    
      (let ((r1 (make-sampler 0 s1 c1))
	    (r2 (make-sampler 0 s2 c2))
	    (sum 0.0)
	    (N (min (framples s1 c1) (framples s2 c2)))
	    (diff 0.0))
	(do ((i 0 (+ i 1)))
	    ((= i N))
	  (set! diff (- (r1) (r2)))
	  (set! sum (+ sum (* diff diff))))
	(sqrt sum)))))
|#
(define channel-distance              ; sqrt(<f - g, f - g>)
  (let ((+documentation+ "(channel-distance s1 c1 s2 c2) returns the euclidean distance between the two channels: sqrt(<f-g,f-g>)"))
    (lambda* ((s1 0) (c1 0) (s2 1) (c2 0))    
      (let ((N (min (framples s1 c1) (framples s2 c2))))
	(let ((data1 (samples 0 N s1 c1))
	      (data2 (samples 0 N s2 c2)))
	  (float-vector-subtract! data1 data2)
	  (sqrt (dot-product data1 data1)))))))


(define periodogram
  (let ((+documentation+ "(periodogram N) displays an 'N' point Bartlett periodogram of the samples in the current channel"))
    (lambda (N)
      (let ((N2 (* 2 N)))
	(let ((len (framples))
	      (average-data (make-float-vector N))
	      (rd (make-sampler 0))
	      (rl (make-float-vector N2))
	      (im (make-float-vector N2)))
	  (do ((i 0 (+ i N)))
	      ((>= i len))
	    (float-vector-scale! rl 0.0)
	    (float-vector-scale! im 0.0)
	    (do ((k 0 (+ k 1)))
		((= k N))
	      (float-vector-set! rl k (read-sample rd)))
	    (mus-fft rl im)
	    (float-vector-multiply! rl rl)
	    (float-vector-multiply! im im)
	    (float-vector-add! average-data (float-vector-add! rl im)))
	  ;; or add snd-spectrum results
	  (graph (float-vector-scale! average-data (/ 1.0 (ceiling (/ len N))))))))))


;;; -------- ssb-am friends

(define shift-channel-pitch
  (let ((+documentation+ "(shift-channel-pitch freq (order 40) (beg 0) dur snd chn edpos) uses the ssb-am CLM generator to \
shift the given channel in pitch without changing its length.  The higher 'order', the better usually."))
    (lambda* (freq (order 40) (beg 0) dur snd chn edpos)
      ;; higher order = better cancellation
      (let ((gen (make-ssb-am freq order)))
	(map-channel (lambda (y) 
		       (ssb-am gen y)) 
		     beg dur snd chn edpos 
		     (format #f "shift-channel-pitch ~A ~A ~A ~A" freq order beg dur))))))

(define hz->2pi
  (let ((+documentation+ "(hz->2pi freq) is like hz->radians but uses the current sound's srate, not mus-srate"))
    (lambda (freq)
      (/ (* 2 pi freq) (srate)))))

(define* (ssb-bank old-freq new-freq pairs (order 40) (bw 50.0) (beg 0) dur snd chn edpos)
  (let ((ssbs (make-vector pairs))
	(bands (make-vector pairs))
	(factor (/ (- new-freq old-freq) old-freq)))
    (do ((i 1 (+ i 1)))
	((> i pairs))
      (let ((aff (* i old-freq))
	    (bwf (* bw (+ 1.0 (/ i (* 2 pairs))))))
	(set! (ssbs (- i 1)) (make-ssb-am (* i factor old-freq)))
	(set! (bands (- i 1)) (make-bandpass (hz->2pi (- aff bwf)) 
					     (hz->2pi (+ aff bwf)) 
					     order))))
    (let ((data (channel->float-vector beg dur snd chn edpos)))
      (let ((len (length data))
	    (mx (float-vector-peak data)))
	(let ((summer (make-float-vector len)))
	  (set! *output* summer)
	  (do ((i 0 (+ i 1)))
	      ((= i pairs))
	    (let ((gen (vector-ref ssbs i))
		  (filt (vector-ref bands i)))
	      (do ((k 0 (+ k 1)))
		  ((= k len))
		(outa k (ssb-am gen (bandpass filt (ina k data))))))) ; outa adds, (ina i v) is the same as (float-vector-ref v i)
	  (set! *output* #f)
	  (float-vector-scale! summer (/ mx (float-vector-peak summer)))
	  (float-vector->channel summer beg len snd chn current-edit-position
				 (format #f "ssb-bank ~A ~A ~A ~A ~A ~A ~A" old-freq new-freq pairs order bw beg dur)))))))

;;; (let ((ind (open-sound "oboe.snd"))) (ssb-bank 550.0 660.0 10))


(define* (ssb-bank-env old-freq new-freq freq-env pairs (order 40) (bw 50.0) (beg 0) dur snd chn edpos)
  ;; this version adds a frequency envelope
  ;; (ssb-bank-env 557 880 '(0 0 1 100.0) 7)
  (let ((ssbs (make-vector pairs))
	(bands (make-vector pairs))
	(factor (/ (- new-freq old-freq) old-freq))
	(frenvs (make-vector pairs)))
    (do ((i 1 (+ i 1)))
	((> i pairs))
      (let ((aff (* i old-freq))
	    (bwf (* bw (+ 1.0 (/ i (* 2 pairs))))))
	(set! (ssbs (- i 1)) (make-ssb-am (* i factor old-freq)))
	(set! (bands (- i 1)) (make-bandpass (hz->2pi (- aff bwf)) 
					     (hz->2pi (+ aff bwf)) 
					     order))
	(set! (frenvs (- i 1)) (make-env freq-env 
					 :scaler (hz->radians i) 
					 :length (framples)))))
    (let ((data (channel->float-vector beg dur snd chn edpos)))
      (let ((len (length data))
	    (mx (float-vector-peak data)))
	(let ((summer (make-float-vector len)))
	  (set! *output* summer)
	  (do ((i 0 (+ i 1)))
	      ((= i pairs))
	    (let ((gen (vector-ref ssbs i))
		  (filt (vector-ref bands i))
		  (e (vector-ref frenvs i)))
	      (do ((k 0 (+ k 1)))
		  ((= k len))
		(outa k (ssb-am gen (bandpass filt (ina k data)) (env e))))))
	  (set! *output* #f)
	  (float-vector-scale! summer (/ mx (float-vector-peak summer)))
	  (float-vector->channel summer beg len snd chn current-edit-position
				 (format #f "ssb-bank-env ~A ~A '~A ~A ~A ~A ~A ~A" old-freq new-freq freq-env pairs order bw beg dur)))))))

;;; (let ((ind (open-sound "oboe.snd"))) (ssb-bank-env 550 600 '(0 1 1 2) 10))
#|
;;; a "bump function" (Stein and Shakarchi)
(define (bumpy)
  (let* ((x 0.0) 
	 (xi (/ 1.0 (framples)))
	 (start 0)
	 (end 1)
	 (scl (exp (/ 4.0 (- end start))))) ; normalize it
    (map-channel (lambda (y) 
		   (let ((val (if (or (<= x start) ; don't divide by zero
				      (>= x end))
				  0.0
				  (* (exp (/ -1.0 (- x start))) 
				     (exp (/ -1.0 (- end x)))))))
		     (set! x (+ x xi))
		     (* scl val))))))
|#


;;; float-vector|channel|spectral-polynomial

(define (float-vector-polynomial v coeffs)
  ;; Horner's rule applied to entire float-vector
  (let* ((num-coeffs (length coeffs))
	 (new-v (make-float-vector (length v) (coeffs (- num-coeffs 1)))))
    (do ((i (- num-coeffs 2) (- i 1)))
	((< i 0))
      (float-vector-offset! (float-vector-multiply! new-v v) (coeffs i)))
    new-v))


(define* (channel-polynomial coeffs snd chn)
  (let ((len (framples snd chn)))
    (float-vector->channel 
     (float-vector-polynomial 
      (channel->float-vector 0 len snd chn) 
      coeffs) 
     0 len snd chn #f (format #f "channel-polynomial ~A" (float-vector->string coeffs)))))

;;; (channel-polynomial (float-vector 0.0 .5)) = x*.5
;;; (channel-polynomial (float-vector 0.0 1.0 1.0 1.0)) = x*x*x + x*x + x

;;; convolution -> * in freq

(define* (spectral-polynomial coeffs snd chn)
  (let* ((len (framples snd chn))
	 (num-coeffs (length coeffs))
	 (fft-len (if (< num-coeffs 2) 
		      len 
		      (expt 2 (ceiling (log (* (- num-coeffs 1) len) 2))))))
    (let ((sound (channel->float-vector 0 len snd chn))
	  (rl1 (make-float-vector fft-len))
	  (rl2 (make-float-vector fft-len))
	  (newv (make-float-vector fft-len)))
      (if (> (coeffs 0) 0.0)
	  (let ((dither (coeffs 0)))
	    (do ((i 0 (+ i 1)))
		((= i fft-len))
	      (float-vector-set! newv i (mus-random dither)))))
      (if (> num-coeffs 1)
	  (begin
	    (float-vector-add! newv (float-vector-scale! (copy sound) (coeffs 1)))
	    (if (> num-coeffs 2)
		(let ((peak (maxamp snd chn)))
		  (copy sound rl1)
		  (do ((i 2 (+ i 1)))
		      ((= i num-coeffs))
		    (copy sound rl2)
		    (convolution rl1 rl2 fft-len)
		    (float-vector-add! newv (float-vector-scale! (copy rl1) 
								 (/ (* (coeffs i) peak) (float-vector-peak rl1)))))
		  (float-vector-scale! newv (/ peak (float-vector-peak newv)))))))
      (float-vector->channel newv 0 (max len (* len (- num-coeffs 1))) 
			     snd chn #f 
			     (format #f "spectral-polynomial ~A" (float-vector->string coeffs))))))
  

;;; ----------------
;;; SCENTROID
;;;
;;; by Bret Battey
;;; Version 1.0 July 13, 2002
;;; translated to Snd/Scheme Bill S 19-Jan-05
;;;
;;; Returns the continuous spectral centroid envelope of a sound.
;;; The spectral centroid is the "center of gravity" of the spectrum, and it
;;; has a rough correlation to our sense of "brightness" of a sound. 
;;;
;;; [Beauchamp, J., "Synthesis by spectral amplitude and 'brightness' matching
;;; analyzed musical sounds". Journal of Audio Engineering Society 30(6), 396-406]
;;;
;;; The formula used is:
;;;    C = [SUM<n=1toj>F(n)A(n)] / [SUM<n=1toj>A(n)]
;;;    Where j is the number of bins in the analysis, 
;;;    F(n) is the frequency of a given bin,
;;;    A(n) is the magnitude of the given bin.
;;;
;;; If a pitch envelope for the analyzed sound is available, the results
;;; of SCENTROID can be used with the function NORMALIZE-CENTROID, below, 
;;; to provide a "normalized spectral centroid". 
;;;
;;; DB-FLOOR -- Frames below this decibel level (0 dB = max) will be discarded
;;; and returned with spectral centroid = 0
;;;
;;; RFREQ -- Rendering frequency. Number of  measurements per second.
;;;
;;; FFTSIZE -- FFT window size. Must be a power of 2. 4096 is recommended.

(define scentroid 
  (let ((+documentation+ "(scentroid file (beg 0.0) dur (db-floor -40.0) (rfreq 100.0) (fftsize 4096)) returns the spectral centroid envelope of a sound; 'rfreq' is \
the rendering frequency, the number of measurements per second; 'db-floor' is the level below which data will be ignored"))
    (lambda* (file (beg 0.0) dur (db-floor -40.0) (rfreq 100.0) (fftsize 4096))
      (let* ((fsr (srate file))
	     (start (floor (* beg fsr))))
	(let ((incrsamps (floor (/ fsr rfreq)))
	      (end (+ start (if dur (floor (* dur fsr)) (- (framples file) beg)))))
	  (let ((fdr (make-float-vector fftsize))
		(fdi (make-float-vector fftsize))
		(scl (make-float-vector (/ fftsize 2)))
		(ones (make-float-vector (/ fftsize 2) 1.0))
		(results (make-float-vector (+ 1 (floor (/ (- end start) incrsamps)))))
		(fft2 (floor (/ fftsize 2)))
		(binwidth (* 1.0 (/ fsr fftsize)))
		(rd (make-readin file)))
	    (do ((k 0 (+ k 1)))
		((= k fft2))
	      (float-vector-set! scl k (* k binwidth)))
	    (do ((i start (+ i incrsamps))
		 (loc 0 (+ 1 loc)))
		((>= i end))
	      (set! (mus-location rd) i)
	      (do ((j 0 (+ j 1)))
		  ((= j fftsize))
		(float-vector-set! fdr j (readin rd)))
	      (if (>= (linear->db (sqrt (/ (dot-product fdr fdr) fftsize))) db-floor)
		  (begin
		    (fill! fdi 0.0)
		    (mus-fft fdr fdi fftsize)
		    (rectangular->magnitudes fdr fdi)
		    (set! (results loc) (/ (dot-product scl fdr fft2) 
					   (dot-product ones fdr fft2))))))
	    results))))))


;;; ----------------
;;;
;;; invert-filter inverts an FIR filter
;;;
;;; say we previously filtered a sound via (filter-channel (float-vector .5 .25 .125))
;;;   and we want to undo it without using (undo):
;;;   (filter-channel (invert-filter (float-vector .5 .25 .125)))
;;;
;;; there are a million gotchas here.  The primary one is that the inverse filter
;;;   can "explode" -- the coefficients can grow without bound.  For example, any
;;;   filter returned by spectrum->coeffs above will be a problem (it always returns
;;;   a "linear phase" filter).  Could this be used to remove reverb?

(define invert-filter
  (let ((+documentation+ "(invert-filter coeffs) tries to return an inverse filter to undo the effect of the FIR filter coeffs."))
    (lambda (fcoeffs)
      (let* ((flen (length fcoeffs))
	     (coeffs (make-float-vector (+ 32 flen))) ; add room for coeffs to die away
	     (order (length coeffs)))
	(do ((i 0 (+ i 1)))
	    ((= i flen))
	  (set! (coeffs i) (fcoeffs i)))
	(let ((nfilt (make-float-vector order)))
	  (set! (nfilt 0) (/ 1.0 (coeffs 0)))
	  (do ((i 1 (+ i 1)))
	      ((= i order))
	    (do ((sum 0.0)
		 (j 0 (+ j 1))
		 (k i (- k 1)))
		((= j i)
		 (set! (nfilt i) (/ sum (- (coeffs 0)))))
	      (set! sum (+ sum (* (nfilt j) (coeffs k))))))
	  nfilt)))))


;;; ----------------
;;;
;;; Volterra filter
;;;
;;; one of the standard non-linear filters
;;; this version is taken from Monson Hayes "Statistical DSP and Modeling"
;;;   it is a slight specialization of the form mentioned by J O Smith and others

(define make-volterra-filter
  (let ((+documentation+ "(make-volterra-filter acoeffs bcoeffs) returns a list for use with volterra-filter, producing one of the standard non-linear filters"))
    (lambda (acoeffs bcoeffs)
      (list acoeffs 
	    bcoeffs 
	    (make-float-vector (max (length acoeffs) (length bcoeffs)))))))

(define volterra-filter
  (let ((+documentation+ "(volterra-filter flt x) takes 'flt', a list returned by make-volterra-filter, and an input 'x', and returns the (non-linear filtered) result"))
    (lambda (flt x)
      (let ((as (car flt))
	    (bs (cadr flt))
	    (xs (caddr flt)))
	(let ((x1len (length as))
	      (x2len (length bs))
	      (sum 0.0))
	  (let ((xlen (length xs)))
	    (float-vector-move! xs (- xlen 1) (- xlen 2) #t))
	  (set! (xs 0) x)
	  (set! sum (dot-product as xs x1len))
	  (do ((i 0 (+ i 1)))
	      ((= i x2len))
	    (do ((j i (+ j 1)))
		((= j x2len))
	      (set! sum (+ sum (* (bs j) (xs i) (xs j))))))
	  sum)))))

;;; (define flt (make-volterra-filter (float-vector .5 .1) (float-vector .3 .2 .1)))
;;; (map-channel (lambda (x) (volterra-filter flt x)))



;;; ----------------
;;;
;;; harmonicizer (each harmonic is split into a set of harmonics via Chebyshev polynomials)
;;;   obviously very similar to ssb-bank above, but splits harmonics individually, rather than pitch-shifting them

(define harmonicizer 
  (let ((+documentation+ "(harmonicizer freq coeffs pairs (order 40) (bw 50.0) (beg 0) dur snd chn edpos) splits out each harmonic \
and replaces it with the spectrum given in coeffs")
	(startup 40))
	
    (lambda* (freq coeffs pairs (order 40) (bw 50.0) (beg 0) dur snd chn edpos)
      (let ((bands (make-vector pairs))
	    (pcoeffs (partials->polynomial coeffs))
	    (peaks (make-vector pairs))
	    (peaks2 (make-vector pairs))
	    (flt (make-filter 2 #r(1 -1) #r(0 -0.9)))
	    (old-mx (maxamp))
	    (len (- (or dur (framples snd chn edpos)) beg)))
	(let ((summer (make-float-vector len))
	      (indata (channel->float-vector beg len snd chn edpos)))
	  
	  (do ((i 0 (+ i 1)))
	      ((= i pairs))
	    (let ((aff (* (+ i 1) freq))
		  (bwf (* bw (+ 1.0 (/ (+ i 1) (* 2 pairs))))))
	      (set! (peaks i) (make-moving-max 128))
	      (set! (peaks2 i) (make-moving-norm 128))
	      (set! (bands i) (make-bandpass (hz->2pi (- aff bwf)) 
					     (hz->2pi (+ aff bwf)) 
					     order))))
	  ;; ignore startup
	  (do ((k 0 (+ k 1)))
	      ((= k startup))
	    (do ((sum 0.0)
		 (i 0 (+ i 1)))
		((= i pairs)
		 (filter flt sum))
	      (let ((sig (bandpass (vector-ref bands i) (float-vector-ref indata k))))
		(set! sum (+ sum (* (moving-max (vector-ref peaks i) sig)
				    (polynomial pcoeffs (* sig (moving-norm (vector-ref peaks2 i) sig)))))))))

	  (set! *output* summer)
	  (do ((pair 0 (+ pair 1)))
	      ((= pair pairs))
	    (do ((bp (vector-ref bands pair))
		 (pk (vector-ref peaks pair))
		 (pk2 (vector-ref peaks2 pair))
		 (k startup (+ k 1)))
		((= k len))
	      (let ((x (bandpass bp (float-vector-ref indata k))))
		(outa k (* (moving-max pk x) (polynomial pcoeffs (* x (moving-norm pk2 x))))))))

	      ;; we're normalizing the polynomial input so its waveshaping index is more-or-less 1.0
	      ;;   this might work better with len=256, max .1 -- we're assuming a well-behaved signal 
	      
	  (set! *output* #f)
	  (do ((k startup (+ k 1)))
	      ((= k len))
	    (float-vector-set! summer k (filter flt (float-vector-ref summer k))))
	  
	  (let ((nmx (float-vector-peak summer)))
	    (if (> nmx 0.0)
		(float-vector-scale! summer (/ old-mx nmx))))
	  (float-vector->channel summer beg len snd chn))))))

;;; (harmonicizer 550.0 (list 1 .5 2 .3 3 .2) 10)



;;; ----------------
;;;
;;; linear sampling rate conversion

(define linear-src-channel
  (let ((+documentation+ "(linear-src-channel sr snd chn) performs sampling rate conversion using linear interpolation."))
    (lambda* (sr snd chn)
      (let ((tempfile 
	     (with-sound (:output (snd-tempnam) :srate (srate snd) :to-snd #f)
	       (let ((rd (make-sampler 0 snd chn)))
		 (do ((intrp 0.0)
		      (last (rd))
		      (next (rd))
		      (samp 0 (+ samp 1)))
		     ((sampler-at-end? rd))
		   (outa samp
			 (let ((pos intrp))
			   (if (>= pos 1.0)
			       (do ((num (floor pos))
				    (i 0 (+ i 1)))
				   ((= i num)
				    (set! pos (- pos num)))
				 (set! last next)
				 (set! next (read-sample rd))))
			   (set! intrp (+ pos sr))
			   (+ last (* pos (- next last))))))))))
	(set-samples 0 (- (framples tempfile) 1) tempfile snd chn #t "linear-src" 0 #f #t)
	;; first #t=truncate to new length, #f=at current edpos, #t=auto delete temp file
	))))


;;; -------- spectrum displayed in various frequency scales

(define display-bark-fft
  ;; click in lisp-graph to change the tick placement choice
  
  (let ((snd-color-1 (lambda args
		       (and (defined? 'snd-color)
			    (apply snd-color args))))
	(bark (lambda (f) 
		(let ((f2 (/ f 7500))) 
		  (+ (* 13.5 (atan (* .00076 f))) (* 3.5 (atan (* f2 f2)))))))
	(mel (lambda (f) 
	       (* 1127 (log (+ 1.0 (/ f 700.0))))))
	(erb (lambda (f) 
	       (+ 43.0 (* 11.17 (log (/ (+ f 312) (+ f 14675))))))))
	
    (let ((bark-fft-size 0)
	  (bark-tick-function 0)
	  (color1 (snd-color-1 8))  ; selected-data-color
	  (color2 (snd-color-1 2))  ; red
	  (color3 (snd-color-1 4))) ; blue
      
      (define (display-bark-fft-1 hook)
	(let* ((snd (hook 'snd))
	       (chn (hook 'chn))
	       (ls (left-sample snd chn))
	       (fftlen (floor (expt 2 (ceiling (log (- (+ (right-sample snd chn) 1) ls) 2))))))
	  (when (> fftlen 0)
	    (let ((data (channel->float-vector ls fftlen snd chn))
		  (normalized (not (= (transform-normalization snd chn) dont-normalize)))
		  (linear #t))                               ; can't currently show lisp graph in dB 
	      ;; snd-axis make_axes: WITH_LOG_Y_AXIS, but LINEAR currently in snd-chn.c 3250
	      (when (float-vector? data)
		(let ((fftdata (snd-spectrum data              ; returns fftlen / 2 data points
					     (fft-window snd chn) fftlen linear 
					     (fft-window-beta snd chn) #f normalized)))
		  (when (float-vector? fftdata)
		    (let ((sr (srate snd))
			  (data-len (length fftdata))
			  (bark-low (floor (bark 20.0)))
			  (mel-low (floor (mel 20.0)))
			  (erb-low (floor (erb 20.0))))
		      (let ((mx (float-vector-peak fftdata))
			    ;; bark settings
			    (bark-frqscl (/ data-len (- (ceiling (bark (* 0.5 sr))) bark-low)))
			    (bark-data (make-float-vector data-len))
			    
			    ;; mel settings
			    (mel-frqscl (/ data-len (- (ceiling (mel (* 0.5 sr))) mel-low)))
			    (mel-data (make-float-vector data-len))
			    
			    ;; erb settings
			    (erb-frqscl (/ data-len (- (ceiling (erb (* 0.5 sr))) erb-low)))
			    (erb-data (make-float-vector data-len)))
			
			(set! bark-fft-size fftlen)
			
			(do ((i 0 (+ i 1)))
			    ((= i data-len))
			  (let ((val (fftdata i))
				(frq (* sr (/ i fftlen))))
			    (let ((bark-bin (round (* bark-frqscl (- (bark frq) bark-low))))
				  (mel-bin (round (* mel-frqscl (- (mel frq) mel-low))))
				  (erb-bin (round (* erb-frqscl (- (erb frq) erb-low)))))
			      (if (and (>= bark-bin 0)
				       (< bark-bin data-len))
				  (set! (bark-data bark-bin) (+ val (bark-data bark-bin))))
			      (if (and (>= mel-bin 0)
				       (< mel-bin data-len))
				  (set! (mel-data mel-bin) (+ val (mel-data mel-bin))))
			      (if (and (>= erb-bin 0)
				       (< erb-bin data-len))
				  (set! (erb-data erb-bin) (+ val (erb-data erb-bin)))))))
			
			(if normalized
			    (let ((bmx (float-vector-peak bark-data))
				  (mmx (float-vector-peak mel-data))
				  (emx (float-vector-peak erb-data)))
			      (if (> (abs (- mx bmx)) .01)
				  (float-vector-scale! bark-data (/ mx bmx)))
			      (if (> (abs (- mx mmx)) .01)
				  (float-vector-scale! mel-data (/ mx mmx)))
			      (if (> (abs (- mx emx)) .01)
				  (float-vector-scale! erb-data (/ mx emx)))))
			
			(graph (list bark-data mel-data erb-data) 
			       "ignored" 
			       20.0 (* 0.5 sr) 
			       0.0 (if normalized 1.0 (* data-len (y-zoom-slider snd chn)))
			       snd chn 
			       #f show-bare-x-axis))))))))
	  
	  (list color1 color2 color3))) ; tell lisp graph display what colors to use
      
      (define (make-bark-labels hook)
	;; at this point the x axis has no markings, but there is room for labels and ticks
	(let* ((snd (hook 'snd))
	       (chn (hook 'chn))
	       (old-foreground-color (foreground-color snd chn copy-context))
	       (axinfo (axis-info snd chn lisp-graph))
	       (axis-x0 (axinfo 10))
	       (axis-x1 (axinfo 12))
	       (axis-y1 (axinfo 11))
	       (bark-label-font (snd-font 3))
	       (cr ((channel-widgets snd chn) 17))
	       (scale-position (let ((sr2 (* 0.5 (srate snd))))
				 (lambda (scale f)
				   (let ((b20 (scale 20.0)))
				     (round (+ axis-x0 
					       (/ (* (- axis-x1 axis-x0) (- (scale f) b20)) 
						  (- (scale sr2) b20))))))))
	       (bark-position (lambda (f) (scale-position bark f)))
	       (mel-position (lambda (f) (scale-position mel f)))
	       (erb-position (lambda (f) (scale-position erb f)))
	       (draw-bark-ticks
		(let ((minor-tick-len 6)
		      (major-tick-len 12))
		  (let (;; (tick-y0 axis-y1)
			(minor-y0 (+ axis-y1 minor-tick-len))
			(major-y0 (+ axis-y1 major-tick-len))
			(axis-y0 (axinfo 13))
			(bark-numbers-font (snd-font 2)))
		    
		    (lambda (bark-function)
		      (if bark-numbers-font (set! (current-font snd chn copy-context) bark-numbers-font))
		      
		      (draw-line axis-x0 axis-y1 axis-x0 major-y0 snd chn copy-context cr)
		      (let ((i1000 (scale-position bark-function 1000.0))
			    (i10000 (scale-position bark-function 10000.0)))
			
			(draw-line i1000 axis-y1 i1000 major-y0 snd chn copy-context cr)
			(draw-line i10000 axis-y1 i10000 major-y0 snd chn copy-context cr)
			
			(draw-string "20" axis-x0 major-y0 snd chn copy-context cr)
			(draw-string "1000" (- i1000 12) major-y0 snd chn copy-context cr)
			(draw-string "10000" (- i10000 24) major-y0 snd chn copy-context cr)
			
			(draw-string (format #f "fft size: ~D" bark-fft-size) (+ axis-x0 10) axis-y0 snd chn copy-context cr)
			
			(do ((i 100 (+ i 100)))
			    ((= i 1000))
			  (let ((i100 (scale-position bark-function i)))
			    (draw-line i100 axis-y1 i100 minor-y0 snd chn copy-context cr)))
			
			(do ((i 2000 (+ i 1000)))
			    ((= i 10000))
			  (let ((i1000 (scale-position bark-function i)))
			    (draw-line i1000 axis-y1 i1000 minor-y0 snd chn copy-context cr)))))))))
	  
	  (let ((label-pos (round (+ axis-x0 (* .45 (- axis-x1 axis-x0)))))
		(label-height 15)
		(char-width 8))
	    
	    ;; bark label/ticks
	    (set! (foreground-color snd chn copy-context) color1)
	    (if (= bark-tick-function 0) (draw-bark-ticks bark-position))
	    (if bark-label-font (set! (current-font snd chn copy-context) bark-label-font))
	    (draw-string "bark," label-pos (+ axis-y1 label-height) snd chn copy-context cr)
	    
	    ;; mel label/ticks
	    (set! (foreground-color snd chn copy-context) color2)
	    (if (= bark-tick-function 1) (draw-bark-ticks mel-position))
	    (if bark-label-font (set! (current-font snd chn copy-context) bark-label-font))
	    (draw-string "mel," (+ (* char-width 6) label-pos) (+ axis-y1 label-height) snd chn copy-context cr)
	    
	    ;; erb label/ticks
	    (set! (foreground-color snd chn copy-context) color3)
	    (if (= bark-tick-function 2) (draw-bark-ticks erb-position))
	    (if bark-label-font (set! (current-font snd chn copy-context) bark-label-font))
	    (draw-string "erb" (+ (* char-width 11) label-pos) (+ axis-y1 label-height) snd chn copy-context cr))
	  
	  (set! (foreground-color snd chn copy-context) old-foreground-color)))
    
      ;; mouse click = move to next scale's ticks
      (define (choose-bark-ticks hook)
	(if (= (hook 'axis) lisp-graph)
	    (begin
	      (set! bark-tick-function (+ bark-tick-function 1))
	      (if (> bark-tick-function 2)
		  (set! bark-tick-function 0))
	      (update-lisp-graph (hook 'snd) (hook 'chn)))))
      
      ;; user's view of display-bark-fft function
      (lambda* (off col1 col2 col3)
	(if col1 (set! color1 col1))
	(if col2 (set! color2 col2))
	(if col3 (set! color3 col3))
	(if (not off)
	    (begin
	      (hook-push lisp-graph-hook display-bark-fft-1)
	      (hook-push after-lisp-graph-hook make-bark-labels)
	      (hook-push mouse-click-hook choose-bark-ticks)
	      (for-each (lambda (snd)
			  (do ((c 0 (+ 1 c)))
			      ((= c (channels snd)))
			    (update-lisp-graph snd c)))
			(sounds)))
	    (begin
	      (hook-remove lisp-graph-hook display-bark-fft-1)
	      (hook-remove after-lisp-graph-hook make-bark-labels)
	      (hook-remove mouse-click-hook choose-bark-ticks)
	      (for-each (lambda (snd)
			  (do ((c 0 (+ 1 c)))
			      ((= c (channels snd)))
			    (set! (lisp-graph? snd c) #f)))
			(sounds))))))))
  
(define (undisplay-bark-fft) (display-bark-fft #t))



;;; -------- lpc-coeffs, lpc-predict

(define lpc-coeffs
  (let ((+documentation+ "(lpc-coeffs data n m) returns 'm' LPC coeffients (in a vector) given 'n' data points in the float-vector 'data'"))
    (lambda (data n m)
      
      ;; translated and changed to use 0-based arrays from memcof of NRinC
      (let ((d (make-float-vector m))
	    (wk1 (make-float-vector n))
	    (wk2 (make-float-vector n))
	    (wkm (make-float-vector n)))
	(copy data wk1)
	(do ((j 1 (+ j 1))
	     (k 0 (+ k 1)))
	    ((= j n))
	  (float-vector-set! wk2 k (float-vector-ref data j)))
	(do ((k 0 (+ k 1)))
	    ((= k m) d)
	  (let ((end (- n k 1)))
	    (let ((num (dot-product wk1 wk2 end))
		  (denom (+ (dot-product wk1 wk1 end) (dot-product wk2 wk2 end))))
	      (if (not (= denom 0.0))
		  (set! (d k) (/ (* 2.0 num) denom))))
	    (let ((d-k (d k)))
	      (do ((i 0 (+ i 1))
		   (k1 (- k 1) (- k1 1)))
		  ((= i k)) ; first time is skipped presumably
		(float-vector-set! d i (- (float-vector-ref wkm i) (* d-k (float-vector-ref wkm k1))))))
	    (if (< k (- m 1))
		(let ((end (- n k 2)))
		  (copy d wkm 0 (+ k 1))
		  (let ((wkm-k (float-vector-ref wkm k))
			(old-wk1 (copy wk1)))
		    (do ((j 0 (+ j 1)))
			((= j end))
		      (float-vector-set! wk1 j (- (float-vector-ref wk1 j) (* wkm-k (float-vector-ref wk2 j)))))
		    (do ((j 0 (+ j 1))
			 (j1 1 (+ j1 1)))
			((= j end))
		      (float-vector-set! wk2 j (- (float-vector-ref wk2 j1) (* wkm-k (float-vector-ref old-wk1 j1))))))))))))))

(define lpc-predict 
  ;; translated and changed to use 0-based arrays from predic of NRinC
  ;; incoming coeffs are assumed to be in a vector (from lpc-coeffs)
  (let ((+documentation+ "(lpc-predict data n coeffs m nf clipped) takes the output of lpc-coeffs ('coeffs', a float-vector) and the length thereof ('m'), \
'n' data points of 'data' (a float-vector), and produces 'nf' new data points (in a float-vector) as its prediction. If 'clipped' is #t, the new data \
is assumed to be outside -1.0 to 1.0."))
    (lambda* (data n coeffs m nf clipped)
      (let ((future (make-float-vector nf))
	    (reg (make-float-vector m)))
	(do ((i 0 (+ i 1))
	     (j (- n 1) (- j 1)))
	    ((= i m))
	  (set! (reg i) (data j)))
	(do ((j 0 (+ j 1))
	     (sum (dot-product coeffs reg m) (dot-product coeffs reg m)))
	    ((= j nf) future)
	  (do ((k (- m 1) (- k 1)))
	      ((= k 0))
	    (set! (reg k) (reg (- k 1))))
	  ;; added this block
	  (if clipped
	      (set! sum (if (> sum 0.0) (max sum 1.0) (min sum -1.0))))
	  (set! (reg 0) sum)
	  (set! (future j) sum))))))


;;; -------- unclip-channel

(define unclip-channel
  (let ((+documentation+ "(unclip-channel snd chn) looks for clipped portions and tries to reconstruct the original using LPC"))
    (lambda* (snd chn)
      (let ((clips 0)                              ; number of clipped portions * 2
	    (unclipped-max 0.0)
	    (len (framples snd chn))
	    (data (channel->float-vector 0 #f snd chn))
	    (clip-size 1000)
	    (clip-data (make-vector 1000 0)))
	;; count clipped portions
	(let ((in-clip #f)
	      (clip-beg 0))
	  (float-vector-abs! data)
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (if (> (float-vector-ref data i) .9999)                    ; this sample is clipped
		(if (not in-clip)
		    (begin
		      (set! in-clip #t)
		      (set! clip-beg i)))
		(begin                            ; not clipped
		  (set! unclipped-max (max unclipped-max (float-vector-ref data i))) ; this is not the same as (float-vector-peak ...)
		  (if in-clip
		      (begin
			(set! in-clip #f)
			(set! (clip-data clips) clip-beg)
			(set! (clip-data (+ 1 clips)) (- i 1))
			(set! clips (+ clips 2))
			(if (> clips clip-size)
			    (let ((new-clip-data (make-vector (* 2 clip-size) 0)))
			      (copy clip-data new-clip-data)
			      (set! clip-data new-clip-data)
			      (set! clip-size (* 2 clip-size))))))))))
	
	(if (<= clips 0)
	    'no-clips
	    ;; try to restore clipped portions
	    (let ((max-len 0))
	      (as-one-edit
	       (lambda ()
		 (do ((clip 0 (+ clip 2)))               ;   so go through all...
		     ((>= clip clips))
		   (let* ((clip-beg (clip-data clip))  ; clip-beg to clip-end inclusive are clipped
			  (clip-end (clip-data (+ 1 clip)))
			  (clip-len (- (+ clip-end 1) clip-beg))
			  (data-len (max 32 (* clip-len 4))))
		     
		     (set! max-len (max max-len clip-len))
		     
		     (let ((forward-data-len data-len)
			   (backward-data-len data-len)
			   (previous-end (if (= clip 0) 0 (clip-data (- clip 1))))
			   (next-beg (if (< clip (- clips 3)) (clip-data (+ clip 2)) (framples snd chn))))
		       
		       (if (< (- clip-beg data-len) previous-end)  ; current beg - data collides with previous
			   (set! forward-data-len (max 4 (- clip-beg previous-end))))
		       
		       (if (> (+ clip-end data-len) next-beg)    ; current end + data collides with next
			   (set! backward-data-len (max 4 (- next-beg clip-end))))
		       
		       (let ((forward-predict-len (min (max clip-len (floor (/ forward-data-len 2))) forward-data-len))
			     (backward-predict-len (min (max clip-len (floor (/ backward-data-len 2))) backward-data-len)))
			 
			 ;; use LPC to reconstruct going both forwards and backwards
			 
			 (let ((future (let ((data (channel->float-vector (- clip-beg forward-data-len) forward-data-len snd chn)))
					 (lpc-predict 
					  data forward-data-len 
					  (lpc-coeffs data forward-data-len forward-predict-len)
					  forward-predict-len
					  clip-len #f)))
			       (past (let ((rdata (reverse! (channel->float-vector (+ 1 clip-end) backward-data-len snd chn))))
				       (lpc-predict 
					rdata backward-data-len 
					(lpc-coeffs rdata backward-data-len backward-predict-len)
					backward-predict-len
					clip-len #f)))
				(new-data (make-float-vector clip-len)))
			   
			   (if (> clip-len 1)
			       (do ((i 0 (+ i 1))
				    (j (- clip-len 1) (- j 1)))
				   ((= i clip-len))
				 (let ((sn (* 0.5 (+ 1.0 (cos (* pi (/ i (- clip-len 1))))))))
				   (set! (new-data i) (+ (* sn 
							    (future i))
							 (* (- 1.0 sn) 
							    (past j))))))
			       
			       ;; todo perhaps move this mix dependent on data-lens?
			       ;; todo perhaps special case for 2 samps (what if both 1.0 for example?)
			       ;; todo perhaps if multichannel and channels are correlated and one is not clipped -- use
			       ;;   its data to help reconstruct clipped case?
			       
			       (set! (new-data 0) ((if (> (future 0) 0.0) max min) (future 0) (past 0))))
			   
			   ;; write reconstruction
			   (float-vector->channel new-data clip-beg clip-len snd chn))))))))
	      
	      (if (> unclipped-max .95) (set! unclipped-max .999))
	      (scale-channel (/ unclipped-max (maxamp snd chn)) 0 (framples snd chn) snd chn)
	      (list 'max unclipped-max 'clips (/ clips 2) 'max-len max-len)))))))


(define unclip-sound
  (let ((+documentation+ "(unclip-sound snd) applies unclip-channel to each channel of 'snd'."))
    (lambda* (snd)
      (let ((index (or snd (selected-sound) (car (sounds)))))
	(if (not (sound? index))
	    (error 'no-such-sound (list "unclip-sound" snd))
	    (do ((chns (channels index))
		 (chn 0 (+ 1 chn)))
		((= chn chns))
	      (unclip-channel index chn)))))))


(define* (kalman-filter-channel (Q 1.0e-5))
  ;; translated from http://www.scipy.org/Cookbook/KalmanFiltering by Andrew Straw (but "R" here is a signal)
  (let ((size (framples))
	(mx (maxamp))
	(data (channel->float-vector 0))
	(xhat 0.0)
	(P 1.0) ; any non-zero value ok here
	(R 0.01) ; first guess
	(Pminus 0.0)
	(frm (make-formant :radius (- 1.0 (/ 2000.0 (srate))) :frequency 1000))
	(del (make-moving-average 256))
	(K 0.0))
    
    (do ((k 1 (+ k 1)))
	((= k size))
      (let ((datum (data k))
	    (xhatminus xhat))
	
	(let ((avg (moving-average del (abs (formant frm datum)))))
	  (set! R (/ .000001 (+ avg .001))))
	;; K now goes between say .5 if avg large to almost 0 if avg 0 (R is inverse essentially)
	;;   so filter lp effect increases as apparent true signal decreases
	;;   "truth" here is based on vocal resonances
	
	(set! (data k) xhatminus) ; filter output
	
	(set! Pminus (+ P Q))
	(set! K (/ Pminus (+ Pminus R)))
	(set! xhat (+ xhatminus
		      (* K (- datum xhatminus))))
	(set! P (* (- 1.0 K) Pminus))))
    
    (float-vector-scale! data (/ mx (float-vector-peak data)))
    (float-vector->channel data)))


;;; -------- Savitzky-Golay filter coefficients (FIR filter -- returns float-vector of coeffs centered at float-vector midpoint)
;;;
;;; based on Numerical Recipes in C p 652

(define invert-matrix
  (let ((+documentation+ "(invert-matrix matrix b (zero 1.0e-7)) inverts 'matrix'"))
    (lambda* (matrix b (zero 1.0e-7))
      ;; translated from Numerical Recipes (gaussj)
      
					;(format () "~%~%invert-matrix n: ~D, ~S, b: ~A, ~S~%" (length matrix) matrix (and b (length b)) b)
      (call-with-exit
       (lambda (return)
	 (let ((n (car (vector-dimensions matrix))))
	   (let ((cols (make-vector n 0))
		 (rows (make-vector n 0))
		 (pivots (make-vector n 0)))
	     (do ((i 0 (+ i 1))
		  (col 0 0)
		  (row 0 0))
		 ((= i n))
	       (do ((biggest 0.0)
		    (j 0 (+ j 1)))
		   ((= j n)
		    (if (< biggest zero) 
			(return #f))) ; this can be fooled (floats...): (invert-matrix (make-share-vector (float-vector 1 2 3 3 2 1 4 5 6) (list 3 3)))
		 (if (not (= (pivots j) 1))
		     (do ((k 0 (+ k 1)))
			 ((= k n))
		       (if (= (pivots k) 0)
			   (let ((val (abs (matrix j k))))
			     (if (> val biggest)
				 (begin
				   (set! col k)
				   (set! row j)
				   (set! biggest val))))
			   (if (> (pivots k) 1)
			       (return #f))))))
	       (set! (pivots col) (+ (pivots col) 1))
	       (if (not (= row col))
		   (let ((temp (if (sequence? b) (b row) 0.0)))
		     (if (sequence? b)
			 (begin
			   (set! (b row) (b col))
			   (set! (b col) temp)))
		     (do ((k 0 (+ k 1)))
			 ((= k n))
		       (set! temp (matrix row k))
		       (set! (matrix row k) (matrix col k))
		       (set! (matrix col k) temp))))
	       (set! (cols i) col)
	       (set! (rows i) row)
	       ;; round-off troubles here
	       (if (< (abs (matrix col col)) zero)
		   (return #f))
	       (let ((inverse-pivot (/ 1.0 (matrix col col))))
		 (set! (matrix col col) 1.0)
		 (do ((k 0 (+ k 1)))
		     ((= k n))
		   (set! (matrix col k) (* inverse-pivot (matrix col k))))
		 (if b (set! (b col) (* inverse-pivot (b col)))))
	       (do ((k 0 (+ k 1)))
		   ((= k n))
		 (if (not (= k col))
		     (let ((scl (matrix k col)))
		       (set! (matrix k col) 0.0)
		       (do ((m 0 (+ 1 m)))
			   ((= m n))
			 (set! (matrix k m) (- (matrix k m) (* scl (matrix col m)))))
		       (if b (set! (b k) (- (b k) (* scl (b col)))))))))
	     (do ((i (- n 1) (- i 1)))
		 ((< i 0))
	       (if (not (= (rows i) (cols i)))
		   (do ((k 0 (+ k 1)))
		       ((= k n))
		     (let ((temp (matrix k (rows i))))
		       (set! (matrix k (rows i)) (matrix k (cols i)))
		       (set! (matrix k (cols i)) temp)))))
	     (list matrix b))))))))

(define (matrix-solve A b)
  (cond ((invert-matrix A b) => cadr) (else #f)))

(define* (make-savitzky-golay-filter size (order 2)) ;assuming symmetric filter (left = right)
  (if (even? size) 
      (set! size (+ size 1)))
  (let ((n (/ (- size 1) 2))
	(a (make-float-vector (list (+ 1 order) (+ 1 order)))))
    (do ((i 0 (+ i 1)))
	((> i (* order 2)))
      (let ((sum (if (= i 0) 1.0 0.0)))
	(if (even? i)
	    (do ((k 1 (+ k 1)))
		((> k n))
					;(set! sum (+ sum (expt k i) (expt (- k) i))) ; kinda nuts 
	      (set! sum (+ sum (* 2 (expt k i))))))
	(let ((m i))
	  (if (> i (- (* 2 order) i))
	      (set! m (- (* 2 order) i)))
	  (do ((k (- m) (+ k 2)))
	      ((> k m))
	    (set! (a (/ (+ i k) 2) (/ (- i k) 2)) sum)))))
    (let ((b (matrix-solve a (let ((f (make-float-vector (+ order 1))))
			       (set! (f 0) 1.0) ; set others instead for derivative
			       f)))
	  (result (make-float-vector size)))
      (do ((k (- n) (+ k 1))
	   (i 0 (+ i 1)))
	  ((> k n))
	(let ((sum (b 0))
	      (fac 1.0))
	  (do ((m 1 (+ 1 m))) 
	      ((> m order)) 
	    (set! fac (* fac k))
	    (set! sum (+ sum (* (b m) fac))))
	  (set! (result i) sum)))
      (make-fir-filter :order size :xcoeffs result))))

(define savitzky-golay-filter fir-filter)

#|
;; NRinC examples (2nd ed, p651)
:(make-savitzky-golay-filter 5 2)
#<fir-filter: order: 5, xs: [-0.086 0.343 0.486 0.343 -0.086]>
:(make-savitzky-golay-filter 11 2)
#<fir-filter: order: 11, xs: [-0.084 0.021 0.103 0.161 0.196 0.207 0.196 0.161...(0: -0.084, 5: 0.207)]>
:(make-savitzky-golay-filter 11 4)
#<fir-filter: order: 11, xs: [0.042 -0.105 -0.023 0.140 0.280 0.333 0.280 0.140...(1: -0.105, 5: 0.333)]>
:(make-savitzky-golay-filter 25 2)
#<fir-filter: order: 25, xs: [-0.049 -0.027 -0.006 0.012 0.028 0.043 0.055 0.066...(0: -0.049, 12: 0.090)]>
|#


;;; -------- hard and soft clipping
;;;
;;; from Julius Smith's http://ccrma.stanford.edu/~jos/pasp/Cubic_Soft_Clipper.html

(define (hard-clipped x)
  (max -1.0 (min 1.0 x)))

(define (soft-clipped x)
  (max -0.6667 (min 0.6667 (- x (* 0.3333 x x x)))))



;;; -------- parallel FM spectrum calculator
;;;
;;; outside Snd, jn can be accessed via:
;;;   (require libgsl.scm)
;;;   (define jn (*libgsl* 'gsl_sf_bessel_Jn))
;;; or:
;;;   (require libm.scm)
;;;   (define jn (*libm* 'jn))
					;(fm-parallel-component 200 2000.0 (list 2000.0 200.0) (list 0.5 1.0) () () #t)

(define fm-parallel-component 
  (let ((+documentation+ "(fm-parallel-component freq carrier modfreqs indices () () with-sines) returns the amplitude of \"freq\" in \
the multi-modulator FM case described by the list of modulator frequencies and indices"))
    (lambda (freq-we-want wc wms inds ns bs using-sine)
      (if (pair? wms)
	  (let ((index (car inds)))
	    (let ((sum 0.0)
		  (mx (ceiling (* 7 index)))
		  (wm (car wms)))
	      (do ((k (- mx) (+ k 1)))
		  ((>= k mx) sum)
		(set! sum (+ sum (fm-parallel-component freq-we-want (+ wc (* k wm)) (cdr wms) (cdr inds) 
							(append ns (list k)) (append bs (list index)) 
							using-sine))))))
	  (if (>= (abs (- freq-we-want (abs wc))) .1)
	      0.0
	      (let ((bmult 1.0))
		(for-each
		 (lambda (n index)
		   (set! bmult (* bmult (bes-jn n index))))
		 ns bs)
		(if (and using-sine (< wc 0.0)) (set! bmult (- bmult)))
		bmult))))))


;;; this returns the component in FM with complex index (using-sine ignored for now)
;;;   this needs the Bessel functions (gsl or snd-test.scm)

(define (fm-complex-component freq-we-want wc wm a b interp using-sine)
  (let ((sum 0.0)
	(mxa (ceiling (* 7 a)))
	(mxb (ceiling (* 7 b))))
    (do ((k (- mxa) (+ k 1)))
	((>= k mxa))
      (do ((j (- mxb) (+ j 1)))
	  ((>= j mxb))
	(if (< (abs (- freq-we-want wc (* k wm) (* j wm))) 0.1)
	    (let ((curJI (* (bes-jn k a)
			    (bes-in (abs j) b)
			    (expt 0.0+1.0i j))))
	      (set! sum (+ sum curJI))
	      (if (> (magnitude curJI) 0.001)
		  (format () ";fm-complex-component add ~,3f from J~D(~A) = ~,3f and I~D(~A) = ~,3f~%"
			       curJI 
			       k a (bes-jn k a)
			       j b (bes-in (abs j) b)))))))
    (list sum
	  (+ (* (- 1.0 interp) (real-part sum))
	     (* interp (imag-part sum))))))

					;(fm-complex-component 1200 1000 100 1.0 3.0 0.0 #f)
					;(fm-complex-component 1200 1000 100 1.0 4.0 0.0 #f) hits the commentary


(define (fm-cascade-component freq-we-want wc wm1 a wm2 b)
  (let ((sum 0.0)
	(mxa (ceiling (* 7 a)))
	(mxb (ceiling (* 7 b))))
    (do ((k (- mxa) (+ k 1)))
	((>= k mxa))
      (do ((j (- mxb) (+ j 1)))
	  ((>= j mxb))
	(if (< (abs (- freq-we-want wc (* k wm1) (* j wm2))) 0.1)
	    (let ((curJJ (* (bes-jn k a)
			    (bes-jn j (* k b)))))
	      (set! sum (+ sum curJJ))
	      (if (> (magnitude curJJ) 0.001)
		  (format () ";fm-cascade-component add ~,3f from J~D(~A) = ~,3f and J~D(~A) = ~,3f~%"
			       curJJ 
			       k a (bes-jn k a)
			       j b (bes-jn j (* k b))))))))
    sum))
					;(fm-cascade-component 2000 2000 500 1.5 50 1.0)



;;; waveshaping harmonic amplitude at a given index

(define (cheby-hka k a coeffs) ; (coeff 0 = DC)
  (do ((sum 0.0)
       (n (length coeffs))
       (j 0 (+ j 1)))
      ((= j n)
       sum)
    (do ((dsum 0.0)
	 (p (+ k (* 2 j)))
	 (i 0 (+ i 1)))
	((>= (+ p (* 2 i)) n)
	 (set! sum (+ sum (* dsum 
			     (expt a p)
			     (binomial p j)))))
      (set! dsum (+ dsum (* (expt -1 i)
			    (coeffs (+ p (* 2 i)))
			    (+ (binomial (+ p i) i)
			       (binomial (+ p i -1) (- i 1)))))))))
#|
(with-sound ()
  (let ((gen (make-polyshape 1000.0 :partials (list 1 .5  2 .25  3 .125  4 .125))))
    (do ((i 0 (+ i 1)))
	((= i 88200))
      (outa i (* .5 (polyshape gen 0.25))))))

(cheby-hka 1 0.25 (float-vector 0 .5 .25 .125 .125))
|#


;;; find not-so-spikey amps for waveshaping

(define flatten-partials 
  
  (let ((cos-fft-to-max 
	 (let ((size 1024))	 
	   (lambda (n cur-amps)
	     (do ((fft-rl (make-float-vector size))
		  (fft-im (make-float-vector size))
		  (i 0 (+ i 1))
		  (bin 2 (+ bin 2)))
		 ((= i n)
		  (float-vector-peak (mus-fft fft-rl fft-im size -1)))   
	       (set! (fft-rl bin) (cur-amps i)))))))
  
    (lambda* (any-partials (tries 32))
      (let ((topk 0)
	    (DC 0.0))
	(let* ((partials (if (list? any-partials)
			     (apply float-vector any-partials)
			     any-partials))
	       (len (length partials))
	       (original-sum (do ((sum 0.0)
				  (i 0 (+ i 2)))
				 ((>= i len) sum)
			       (let ((hnum (floor (partials i)))
				     (amp (partials (+ i 1))))
				 (if (= hnum 0)
				     (set! DC amp)
				     (begin
				       (set! topk (max topk hnum))
				       (set! sum (+ sum amp)))))))
	       (min-sum original-sum)
	       (original-partials (do ((v (make-float-vector topk))
				       (i 0 (+ i 2)))
				      ((>= i len) v)
				    (let ((hnum (floor (partials i))))
				      (if (not (= hnum 0))
					  (set! (v (- hnum 1)) (partials (+ i 1)))))))
	       (min-partials (copy original-partials)))
	  
	  (if (<= topk (log tries 2))
	      (set! tries (floor (expt 2 (- topk 1)))))
	  
	  (do ((try 0 (+ 1 try)))
	      ((= try tries))
	    (let ((new-partials (copy original-partials)))
	      (do ((k 0 (+ k 1)))
		  ((= k topk))
		(if (> (random 1.0) 0.5)
		    (set! (new-partials k) (- (new-partials k)))))
	      (let ((new-sum (cos-fft-to-max topk new-partials)))
		(if (< new-sum min-sum)
		    (begin
		      (set! min-partials (copy new-partials))
		      (set! min-sum new-sum))))))
	  
	  (let ((new-amps (float-vector-scale! min-partials (/ original-sum min-sum)))
		(new-partials (copy partials)))
	    (do ((i 0 (+ i 2)))
		((>= i len))
	      (let ((hnum (floor (new-partials i))))
		(set! (new-partials (+ i 1)) (if (= hnum 0) DC (new-amps (- hnum 1))))))
	    new-partials))))))
#|
(with-sound (:clipped #f :statistics #t :channels 2)
  (let* ((amps (normalize-partials (list 1 .25 2 .5 3 .25)))
	 (gen1 (make-polywave 400.0 amps))
	 (gen2 (make-polywave 400.0 (flatten-partials amps))))
    (do ((i 0 (+ i 1)))
	((= i 44100))
      (outa i (polywave gen1))
      (outb i (polywave gen2)))))
|#



#|
;;; these are standard FFTs for s7

(define* (fft! rl im n (dir 1))
  
  (if (not im)
      (let ((clear (copy rl)))
	(fill! clear 0.0)
	(set! im clear)))
  
  (if (not n)
      (set! n (length rl)))
  
  (do ((i 0 (+ i 1))
       (j 0))
      ((= i n))
    (if (> j i)
	(let ((tempr (rl j))
	      (tempi (im j)))
	  (set! (rl j) (rl i))
	  (set! (im j) (im i))
	  (set! (rl i) tempr)
	  (set! (im i) tempi)))
    (let ((m (/ n 2)))
      (do () 
	  ((or (< m 2) (< j m)))
	(set! j (- j m))
	(set! m (/ m 2)))
      (set! j (+ j m))))
  
  (let ((ipow (floor (log n 2)))
	(prev 1))
    (do ((lg 0 (+ lg 1))
	 (mmax 2 (* mmax 2))
	 (pow (/ n 2) (/ pow 2))
	 (theta (* pi dir) (* theta 0.5)))
	((= lg ipow))
      (let ((wpr (cos theta))
	    (wpi (sin theta))
	    (wr 1.0)
	    (wi 0.0))
	(do ((ii 0 (+ ii 1)))
	    ((= ii prev))
	  (do ((jj 0 (+ jj 1))
	       (i ii (+ i mmax))
	       (j (+ ii prev) (+ j mmax)))
	      ((>= jj pow))
	    (let ((tempr (- (* wr (rl j)) (* wi (im j))))
		  (tempi (+ (* wr (im j)) (* wi (rl j)))))
	      (set! (rl j) (- (rl i) tempr))
	      (set! (rl i) (+ (rl i) tempr))
	      (set! (im j) (- (im i) tempi))
	      (set! (im i) (+ (im i) tempi))))
	  (let ((wtemp wr))
	    (set! wr (- (* wr wpr) (* wi wpi)))
	    (set! wi (+ (* wi wpr) (* wtemp wpi)))))
	(set! prev mmax))))
  rl)


(define* (cfft! data n (dir 1))
  
  (if (not n) (set! n (length data)))
  
  (do ((i 0 (+ i 1))
       (j 0))
      ((= i n))
    (if (> j i)
	(let ((temp (data j)))
	  (set! (data j) (data i))
	  (set! (data i) temp)))
    (let ((m (/ n 2)))
      (do () 
	  ((or (< m 2) (< j m)))
	(set! j (- j m))
	(set! m (/ m 2)))
      (set! j (+ j m))))
  
  (let ((ipow (floor (log n 2)))
	(prev 1))
    (do ((lg 0 (+ lg 1))
	 (mmax 2 (* mmax 2))
	 (pow (/ n 2) (/ pow 2))
	 (theta (complex 0.0 (* pi dir)) (* theta 0.5)))
	((= lg ipow))
      (let ((wpc (exp theta))
	    (wc 1.0))
	(do ((ii 0 (+ ii 1)))
	    ((= ii prev))
	  (do ((jj 0 (+ jj 1))
	       (i ii (+ i mmax))
	       (j (+ ii prev) (+ j mmax)))
	      ((>= jj pow))
	    (let ((tc (* wc (data j))))
	      (set! (data j) (- (data i) tc))
	      (set! (data i) (+ (data i) tc))))
	  (set! wc (* wc wpc)))
	(set! prev mmax))))
  
  data)


;;; > (cfft! (list 0.0 1+i 0.0 0.0))
;;; (1+1i -1+1i -1-1i 1-1i)
;;; 
;;; > (cfft! (vector 0.0 1+i 0.0 0.0))
;;; #(1+1i -1+1i -1-1i 1-1i)
;;; 
;;; ;; check against built-in FFT
;;; > (let ((rl (float-vector 0.0 1.0 0.0 0.0)) 
;;;         (im (float-vector 0.0 1.0 0.0 0.0))) 
;;;     (mus-fft rl im) 
;;;     (map complex rl im))
;;; (1+1i -1+1i -1-1i 1-1i)

|#
