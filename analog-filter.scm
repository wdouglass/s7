;;; various even order analog filters, based primarily on Anders Johansson's (GPL'd) code
;;;
;;; butterworth-lowpass|highpass|bandstop|bandpass
;;; chebyshev-lowpass|highpass|bandstop|bandpass
;;; inverse-chebyshev-lowpass|highpass|bandstop|bandpass
;;;
;;; if GSL included in Snd:
;;; bessel-lowpass|highpass|bandstop|bandpass
;;; elliptic-lowpass|highpass|bandstop|bandpass
;;;
;;; build Snd with gsl for best results

(provide 'snd-analog-filter.scm)

(define* (analog->digital n num den fz)
  (let ((g 1.0)
	(Q 1.0)
	(wc (tan (* pi fz)))
	(c (make-float-vector (* 2 n)))

	(cascade->canonical 
	 (let ((conv (lambda (M h L x y)
		       ;; x * h -> y
		       (do ((n 0 (+ n 1)))
			   ((= n (+ L M)))
			 (let ((sum 0.0)
			       (start (max 0 (- n L 1)))
			       (end (min n M)))
			   (do ((m start (+ m 1)))
			       ((> m end))
			     (set! sum (+ sum (* (h m) (x (- n m))))))
			   (set! (y n) sum))))))
	   (lambda (A)
	     ;; (cascade->canonical A) converts cascade filter coeffs to canonical form
	     ;; from Orfanidis "Introduction to Signal Processing"
	     (let ((K (length A)))
	       (let ((d (make-float-vector (+ 1 (* 2 K))))
		     (a1 (make-float-vector (+ 1 (* 2 K)))))
		 (set! (a1 0) 1.0)
		 (do ((i 0 (+ i 1)))
		     ((= i K))
		   (conv 2 (A i) (+ 1 (* 2 i)) a1 d)
		   (copy d a1 0 (+ 3 (* 2 i))))
		 a1))))))
    
    (do ((i 0 (+ i 2))
	 (j 0 (+ j 3))
	 (k 0 (+ k 4)))
	((>= i n))
      (let* ((nt0 (/ (num j) (* wc wc)))
	     (nt1 (/ (num (+ j 1)) wc))
	     (nt2 (num (+ j 2)))
	     (dt0 (/ (den j) (* wc wc)))
	     (dt1 (/ (den (+ j 1)) (* wc Q)))
	     (dt2 (den (+ j 2))))
	(let ((kd (+ dt0 dt1 dt2))
	      (kn (+ nt0 nt1 nt2)))
	  (set! (c    k   ) (/ (- (* 2.0 dt2) (* 2.0 dt0)) kd))
	  (set! (c (+ k 1)) (/ (- (+ dt0 dt2) dt1) kd))
	  (set! (c (+ k 2)) (/ (- (* 2.0 nt2) (* 2.0 nt0)) kn))
	  (set! (c (+ k 3)) (/ (- (+ nt0 nt2) nt1) kn))
	  (set! g (* g (/ kn kd))))))
    
    (do ((a ())
	 (b ())
	 (i 0 (+ i 2))
	 (k 0 (+ k 4))) ; c
	((>= i n)
	 (list (float-vector-scale! (cascade->canonical a) g) ; scale entire numerator because this is the convolved form
	       (cascade->canonical b)))
      (set! a (cons (float-vector (c (+ k 3)) (c (+ k 2)) (c (+ k 3))) a))
      (set! b (cons (float-vector 1.0 (c k) (c (+ k 1))) b)))))
      

(define (prototype->highpass n proto)
  (let ((num (car proto))
	(den (cadr proto)))
    (do ((g 1.0)
	 (numt (make-float-vector (length num)))
	 (dent (make-float-vector (length den)))
	 (k 0 (+ k 2))
	 (i 0 (+ i 3)))
	((>= k n)
	 (set! (numt 0) g)
	 (list numt dent))
      (set! g (* g (/ (num (+ i 2)) (den (+ i 2)))))
      (set! (numt    i   ) 1.0)
      (set! (numt (+ i 1)) (/ (num (+ i 1)) (num (+ i 2))))
      (set! (numt (+ i 2)) (/ (num i) (num (+ i 2))))
      (set! (dent    i   ) 1.0)
      (set! (dent (+ i 1)) (/ (den (+ i 1)) (den (+ i 2))))
      (set! (dent (+ i 2)) (/ (den i) (den (+ i 2)))))))


;;; ---------------- Butterworth ----------------

(define (butterworth-prototype n)
  (let ((len (/ (* n 3) 2)))
    (do ((num (make-float-vector len))
	 (den (make-float-vector len))
	 (w 1 (+ w 2))
	 (j 0 (+ j 3)))
	((>= w n)
	 (list num den)) 
      (set! (num j) 0.0)
      (set! (num (+ j 1)) 0.0)
      (set! (num (+ j 2)) 1.0)
      (set! (den j) 1.0)
      (set! (den (+ j 1)) (* 2.0 (cos (/ (* w pi) (* 2.0 n)))))
      (set! (den (+ j 2)) 1.0))))

(define make-butterworth-lowpass
  (let ((+documentation+ "(make-butterworth-lowpass n fc) returns a lowpass Buttterworth filter; n = order, fc = cutoff \
freq (srate = 1.0): (make-butterworth-lowpass 8 .1)"))
    (lambda (n fc)
      ;; identical to make-butter-lp except for fc (freq->1.0) fixup
      (if (odd? n) (set! n (+ n 1)))
      (let ((coeffs (let ((proto (butterworth-prototype n)))
		      (analog->digital n (car proto) (cadr proto) fc))))
	(make-filter :xcoeffs (car coeffs) 
		     :ycoeffs (cadr coeffs))))))

(define make-butterworth-highpass
  (let ((+documentation+ "(make-butterworth-highpass n fc) returns a highpass Butterworth filter; n = order, fc = cutoff \
freq (srate = 1.0): (make-butterworth-highpass 8 .1)"))
    (lambda (n fc)
      (if (odd? n) (set! n (+ n 1)))
      (let ((coeffs (let ((hproto (prototype->highpass n (butterworth-prototype n))))
		      (analog->digital n (car hproto) (cadr hproto) fc))))
	(make-filter :xcoeffs (car coeffs) 
		     :ycoeffs (cadr coeffs))))))

(define make-butterworth-bandpass
  (let ((+documentation+ "(make-butterworth-bandpass n fl fh) returns a bandpass Butterworth filter; n = order, fl and fh \
are (1.0-based) edge freqs: (make-butterworth-bandpass 4 .1 .2)"))
    (lambda (n fl fh)
      (if (odd? n) (set! n (+ n 1)))
      (let ((lp (make-butterworth-lowpass n fh))
	    (hp (make-butterworth-highpass n fl)))
	(lambda (y) 
	  (filter lp (filter hp y)))))))

(define (make-bandstop-filter lp hp)
  (lambda (y) 
    (+ (filter lp y) (filter hp y))))
  
(define make-butterworth-bandstop
  (let ((+documentation+ "(make-butterworth-bandstop n fl fh) returns a bandstop Butterworth filter; n = order, fl and fh \
are (1.0-based) edge freqs: (make-butterworth-bandstop 4 .1 .2)"))
    (lambda (n fl fh)
      (if (odd? n) (set! n (+ n 1)))
      (make-bandstop-filter (make-butterworth-lowpass n fl)
			    (make-butterworth-highpass n fh)))))


;;; ---------------- Chebyshev ---------------- 

(define* (chebyshev-prototype n (ripple 1.0)) ; ripple in dB (positive)
  (let ((len (/ (* n 3) 2)))
    (do ((v0 (let ((e (sqrt (- (expt 10.0 (* 0.1 ripple)) 1.0))))
	       (/ (asinh (/ 1.0 e)) n)))
	 (num (make-float-vector len))
	 (den (make-float-vector len))
	 (k 1.0 (+ k 2.0))
	 (j 0 (+ j 3)))
	((>= k n)
	 (set! (num 2) (/ (expt 2.0 (- 2 n))
			  (expt 3.2 (log ripple 10.0)))) ; whatever works...
	 (list num den))
      (let ((u (- (* (sinh v0) (sin (/ (* k pi) (* 2.0 n))))))
	    (w (* (cosh v0) (cos (/ (* k pi) (* 2.0 n))))))
	(set! (num    j   ) 0.0)
	(set! (num (+ j 1)) 0.0)
	(set! (num (+ j 2)) 1.0)
	(set! (den    j   ) 1.0)
	(set! (den (+ j 1)) (* -2.0 u))
	(set! (den (+ j 2)) (+ (* u u) (* w w)))))))

(define make-chebyshev-lowpass 
  (let ((+documentation+ "(make-chebyshev-lowpass n fc (ripple 1.0)) returns a lowpass Chebyshev filter; n = order, \
fc = cutoff freq (srate = 1.0): (make-chebyshev-lowpass 8 .1)"))
    (lambda* (n fc (ripple 1.0))
      (if (odd? n) (set! n (+ n 1)))
      (let ((coeffs (let ((proto (chebyshev-prototype n ripple)))
		      (analog->digital n (car proto) (cadr proto) fc))))
	(make-filter :xcoeffs (car coeffs) 
		     :ycoeffs (cadr coeffs))))))

(define make-chebyshev-highpass 
  (let ((+documentation+ "(make-chebyshev-highpass n fc (ripple 1.0)) returns a highpass Chebyshev filter; n = order, \
fc = cutoff freq (srate = 1.0): (make-chebyshev-highpass 8 .1 .01)"))
    (lambda* (n fc (ripple 1.0))
      (if (odd? n) (set! n (+ n 1)))
      (let ((coeffs (let ((hproto (prototype->highpass n (chebyshev-prototype n ripple))))
		      (analog->digital n (car hproto) (cadr hproto) fc))))
	(make-filter :xcoeffs (car coeffs) 
		     :ycoeffs (cadr coeffs))))))

(define make-chebyshev-bandpass 
  (let ((+documentation+ "(make-chebyshev-bandpass n fl fh (ripple 1.0)) returns a bandpass Chebyshev filter; n = order, \
fl and fh = edge freqs (srate = 1.0): (make-chebyshev-bandpass 4 .1 .2)"))
    (lambda* (n fl fh (ripple 1.0))
      (if (odd? n) (set! n (+ n 1)))
      (let ((lp (make-chebyshev-lowpass n fh ripple))
	    (hp (make-chebyshev-highpass n fl ripple)))
	(lambda (y) 
	  (filter lp (filter hp y)))))))

(define make-chebyshev-bandstop 
  (let ((+documentation+ "(make-chebyshev-bandstop n fl fh (ripple 1.0)) returns a bandstop Chebyshev filter; n = order, \
fl and fh = edge freqs (srate = 1.0): (make-chebyshev-bandstop 8 .1 .4 .01)"))
    (lambda* (n fl fh (ripple 1.0))
      (if (odd? n) (set! n (+ n 1)))
      (make-bandstop-filter (make-chebyshev-lowpass n fl ripple)
			    (make-chebyshev-highpass n fh ripple)))))



;;; ---------------- inverse Chebyshev ---------------- 

(define* (inverse-chebyshev-prototype n (loss-dB 60.0)) ; stopband loss
  (let ((len (/ (* n 3) 2)))
    (do ((v0 (let ((e (sqrt (/ 1.0 (- (expt 10.0 (* 0.1 loss-dB)) 1.0)))))
	       (/ (asinh (/ 1.0 e)) n)))
	 (num (make-float-vector len))
	 (den (make-float-vector len))
	 (pl 0.0)
	 (L 1.0 (+ L 2.0))
	 (j 0 (+ j 3)))
	((>= L n)
	 (list num den
	       (expt 1.122 (- loss-dB)))) ; argh
      (let ((u (- (* (sinh v0) (sin (/ (* L pi) (* 2.0 n))))))
	    (w (* (cosh v0) (cos (/ (* L pi) (* 2.0 n)))))
	    (t (/ 1.0 (sin (/ (* (+ L pl) pi) (* 2.0 n))))))
	(set! (num    j   ) 1.0)
	(set! (num (+ j 1)) 0.0)
	(set! (num (+ j 2)) (* t t))
	(set! (den    j   ) 1.0)
	(set! (den (+ j 1)) (/ (* -2.0 u) (+ (* u u) (* w w))))
	(set! (den (+ j 2)) (/ 1.0 (+ (* u u) (* w w))))))))

(define make-inverse-chebyshev-lowpass 
  (let ((+documentation+ "(make-inverse-chebyshev-lowpass n fc (loss-dB 60.0)) returns a lowpass inverse-Chebyshev filter; n = order, \
fc = cutoff freq (srate = 1.0): (make-inverse-chebyshev-lowpass 10 .4 120)"))
    (lambda* (n fc (loss-dB 60.0))
      (if (odd? n) (set! n (+ n 1)))
      (let* ((proto (inverse-chebyshev-prototype n loss-dB))
	     (coeffs (analog->digital n (car proto) (cadr proto) fc)))
	(make-filter :xcoeffs (float-vector-scale! (car coeffs) (caddr proto)) 
		     :ycoeffs (cadr coeffs))))))

(define make-inverse-chebyshev-highpass 
  (let ((+documentation+ "(make-inverse-chebyshev-highpass n fc (loss-dB 60.0)) returns a highpass inverse-Chebyshev filter; n = order, \
fc = cutoff freq (srate = 1.0): (make-inverse-chebyshev-highpass 10 .1 120)"))
    (lambda* (n fc (loss-dB 60.0))
      (if (odd? n) (set! n (+ n 1)))
      (let* ((proto (inverse-chebyshev-prototype n loss-dB))
	     (coeffs (let ((hproto (prototype->highpass n proto)))
		       (analog->digital n (car hproto) (cadr hproto) fc))))
	(make-filter :xcoeffs (float-vector-scale! (car coeffs) (caddr proto)) 
		     :ycoeffs (cadr coeffs))))))

(define make-inverse-chebyshev-bandpass 
  (let ((+documentation+ "(make-inverse-chebyshev-bandpass n fl fh (loss-dB 60.0)) returns a bandpass inverse-Chebyshev filter; n = order, \
fl and fh are edge freqs (srate=1.0): (make-inverse-chebyshev-bandpass 8 .1 .4)"))
    (lambda* (n fl fh (loss-dB 60.0))
      (if (odd? n) (set! n (+ n 1)))
      (let ((lp (make-inverse-chebyshev-lowpass n fh loss-dB))
	    (hp (make-inverse-chebyshev-highpass n fl loss-dB)))
	(lambda (y) (filter lp (filter hp y)))))))

(define make-inverse-chebyshev-bandstop 
  (let ((+documentation+ "(make-inverse-chebyshev-bandstop n fl fh (loss-dB 60.0)) returns a bandstop inverse-Chebyshev filter; n = order, \
fl and fh are edge freqs (srate=1.0): (make-inverse-chebyshev-bandstop 8 .1 .4 90)"))
    (lambda* (n fl fh (loss-dB 60.0))
      (if (odd? n) (set! n (+ n 1)))
      (make-bandstop-filter (make-inverse-chebyshev-lowpass n fl loss-dB)
			    (make-inverse-chebyshev-highpass n fh loss-dB)))))



;;; ---------------- Bessel (-Thompson) ---------------- 

(define bessel-prototype 
  (let ((bessel-i 
	 (let ((fact (lambda (n)
		       (do ((x 1)
			    (i 2 (+ i 1)))
			   ((> i n) x)
			 (set! x (* x i))))))
	   ;; this form overflows if we don't have bignums
	   ;;  (define (bessel-i n)
	   ;;    (let ((cs (make-float-vector (+ n 1))))
	   ;;      (do ((i 0 (+ i 1)))
	   ;;	  ((> i n))
	   ;;	(set! (cs i) (/ (fact (- (* 2 n) i))
	   ;;			(* (expt 2 (- n i))
	   ;;			   (fact i)
	   ;;			   (fact (- n i))))))
	   ;;      cs))
	   (lambda (n)
	     (do ((cs (make-float-vector (+ n 1)))
		  (i 0 (+ i 1)))
		 ((> i n) cs)
	       (do ((val (/ 1.0 (* (fact i) (expt 2 (- n i)))))
		    (k 1 (+ k 1))
		    (f (- n i -1) (+ f 1)))  ; (f (+ 1 (- n i)) (+ 1 f))
		   ((> k n)
		    (set! (cs i) val))   
		 (set! val (* val f))))))))
    (lambda (n)  
      (let ((len (/ (* n 3) 2)))
	(let ((num (make-float-vector len))
	      (den (make-float-vector len))
	      (b2 (bessel-i n)))
	  (let ((p (gsl-roots (copy b2 (make-vector (length b2))))))
	    (do ((i 0 (+ i 1)))
		((= i n))
	      (set! (p i) (/ (p i) (expt (b2 0) (/ 1.0 n)))))
	    (do ((j 0 (+ j 3))
		 (i 0 (+ i 2)))
		((>= i n))
	      (set! (num    j   ) 0.0)
	      (set! (num (+ j 1)) 0.0)
	      (set! (num (+ j 2)) 1.0)
	      (set! (den    j   ) 1.0)
	      (set! (den (+ j 1)) (* -2.0 (real-part (p i))))
	      (set! (den (+ j 2)) (real-part (* (p i) (p (+ i 1)))))))
	  (list num den))))))

(define make-bessel-lowpass 
  (let ((+documentation+ "(make-bessel-lowpass n fc) returns a lowpass Bessel filter; n = order, fc = cutoff freq (srate = 1.0): (make-bessel-lowpass 4 .1)"))
    (lambda (n fc)
      (if (odd? n) (set! n (+ n 1)))
      (let ((coeffs (let ((proto (bessel-prototype n)))
		      (analog->digital n (car proto) (cadr proto) fc))))
	(make-filter :xcoeffs (car coeffs) 
		     :ycoeffs (cadr coeffs))))))

(define make-bessel-highpass 
  (let ((+documentation+ "(make-bessel-highpass n fc) returns a highpass Bessel filter; n = order, fc = cutoff freq (srate = 1.0): (make-bessel-highpass 8 .1)"))
    (lambda* (n fc)
      (if (odd? n) (set! n (+ n 1)))
      (let ((coeffs (let ((hproto (prototype->highpass n (bessel-prototype n))))
		      (analog->digital n (car hproto) (cadr hproto) fc))))
	(make-filter :xcoeffs (car coeffs) 
		     :ycoeffs (cadr coeffs))))))

(define make-bessel-bandpass 
  (let ((+documentation+ "(make-bessel-bandpass n fl fh) returns a bandpass Bessel filter; n = order, fl and fh are edge freqs (srate=1.0): (make-bessel-bandpass 4 .1 .2)"))
    (lambda* (n fl fh)
      (if (odd? n) (set! n (+ n 1)))
      (let ((lp (make-bessel-lowpass n fh))
	    (hp (make-bessel-highpass n fl)))
	(lambda (y) 
	  (filter lp (filter hp y)))))))

(define make-bessel-bandstop 
  (let ((+documentation+ "(make-bessel-bandstop n fl fh) returns a bandstop Bessel filter; n = order, fl and fh are edge freqs (srate=1.0): (make-bessel-bandstop 8 .1 .2)"))
    (lambda* (n fl fh)
      (if (odd? n) (set! n (+ n 1)))
      (make-bandstop-filter (make-bessel-lowpass n fl)
			    (make-bessel-highpass n fh)))))


;;; ---------------- Elliptic ---------------- 

(define* (elliptic-prototype n (ripple 1.0) (loss-dB 60.0))
  (let ((minimize-function 
	 (lambda* (f xmin xmax arg1 arg2)
	   (let* ((n 20)
		  (x (make-float-vector n))
		  (fx (f xmin arg1 arg2)))
	     (do ((i 0 (+ i 1)))
		 ((= i n))
	       (do ((step (/ (- xmax xmin) (- n 1.0)))
		    (j 0 (+ j 1))
		    (s xmin))
		   ((= j (- n 1)))
		 (float-vector-set! x j s)
		 (set! s (+ s step)))
	       (set! (x (- n 1)) xmax)
	       (do ((j 0 (+ j 1)))
		   ((= j n))
		 (let ((ft (f (x j) arg1 arg2)))
		   (if (< ft fx)
		       (begin
			 (set! fx ft)
			 (set! xmax (x (if (< j (- n 1)) (+ j 1) (- n 1))))
			 (set! xmin (x (if (> j 0) (- j 1) 0))))))))
	     (/ (+ xmax xmin) 2.0))))
	
	(e (sqrt (- (expt 10.0 (* 0.1 ripple)) 1.0))))
    
    (let ((k1 (/ e (sqrt (- (expt 10.0 (* 0.1 loss-dB)) 1.0))))
	  (len (/ (* n 3) 2)))
      (let ((k1p (sqrt (- 1.0 (* k1 k1))))
	    (m 0.0)
	    (num (make-float-vector len))
	    (den (make-float-vector len))
	    (g 1.0))
	(let ((eps 0.0000001)
	      (kr 0.0))
	  (if (> (abs (- 1.0 (* k1p k1p))) eps)
	      (set! kr (* n (/ (gsl-ellipk (* k1 k1)) (gsl-ellipk (* k1p k1p))))))
	  (set! m (minimize-function 
		   (lambda (m arg1 arg2)
		     (abs (- (/ (gsl-ellipk m) 
				(gsl-ellipk (- 1.0 m)))
			     arg1)))
		   0.001 0.999 kr)))
	(let ((k (gsl-ellipk m))
	      (cv (make-float-vector (floor (* 0.5 3 (+ n 1))))))
	  (do ((i 0 (+ i 2))
	       (j 0 (+ j 3)))
	      ((>= i n))
	    (let ((vals (gsl-ellipj (/ (* (+ i 1) k) (* 1.0 n)) m)))
	      (let ((sn (car vals))
		    (cn (cadr vals))
		    (dn (caddr vals)))
		(set! (cv    j   ) sn)
		(set! (cv (+ j 1)) cn)
		(set! (cv (+ j 2)) dn)
		(let* ((z (/ 0.0-i (* (sqrt m) sn)))
		       (pz (real-part (* z (complex (real-part z) (- (imag-part z)))))))
		  (set! g (/ g pz))
		  (set! (num    j   ) 1.0)
		  (set! (num (+ j 1)) (* -2.0 (real-part z)))
		  (set! (num (+ j 2)) pz)))))
	  (let ((vals (let ((v0 (let ((minf (minimize-function
					     (lambda (u arg1 arg2)
					       (let ((vals (gsl-ellipj u arg1)))
						 (abs (- arg2 (/ (car vals) (cadr vals))))))
					     0.0 (/ 1.0 e) (* k1p k1p) (/ 1.0 e))))
				  (/ (* k minf)
				     (* n (gsl-ellipk (* k k1)))))))
			(gsl-ellipj v0 (- 1.0 m)))))
	    (do ((sn (car vals))
		 (cn (cadr vals))
		 (dn (caddr vals))
		 (i 0 (+ i 2))
		 (j 0 (+ j 3)))
		((>= i n))
	      (let* ((p (/ (- (+ (* (cv (+ j 1)) (cv (+ j 2)) sn cn)
				 (* 0.0+i (cv j) dn)))
			   (- 1.0 (* (cv (+ j 2)) sn
				     (cv (+ j 2)) sn))))
		     (pp (real-part (* p (complex (real-part p) (- (imag-part p)))))))
		(set! g (* g pp))
		(set! (den    j   ) 1.0)
		(set! (den (+ j 1)) (* -2.0 (real-part p)))
		(set! (den (+ j 2)) pp)))))
	(list num den (abs (/ g (sqrt (+ 1.0 (* e e))))))))))

(define make-elliptic-lowpass 
  (let ((+documentation+ "(make-elliptic-lowpass n fc (ripple 1.0) (loss-dB 60.0)) returns a lowpass elliptic filter; n = order, \
fc = cutoff freq (srate = 1.0): (make-elliptic-lowpass 8 .25 .01 90)"))
    (lambda* (n fc (ripple 1.0) (loss-dB 60.0))
      (if (odd? n) (set! n (+ n 1)))
      (let* ((proto (elliptic-prototype n ripple loss-dB))
	     (coeffs (analog->digital n (car proto) (cadr proto) fc)))
	(make-filter :xcoeffs (float-vector-scale! (car coeffs) (caddr proto)) 
		     :ycoeffs (cadr coeffs))))))

(define make-elliptic-highpass 
  (let ((+documentation+ "(make-elliptic-highpass n fc (ripple 1.0) (loss-dB 60.0)) returns a highpass elliptic filter; n = order, \
fc = cutoff freq (srate = 1.0): (make-elliptic-highpass 8 .25 .01 90)"))
    (lambda* (n fc (ripple 1.0) (loss-dB 60.0))
      (if (odd? n) (set! n (+ n 1)))
      (let* ((proto (elliptic-prototype n ripple loss-dB))
	     (coeffs (let ((hproto (prototype->highpass n proto)))
		       (analog->digital n (car hproto) (cadr hproto) fc))))
	(make-filter :xcoeffs (float-vector-scale! (car coeffs) (caddr proto)) 
		     :ycoeffs (cadr coeffs))))))

(define make-elliptic-bandpass 
  (let ((+documentation+ "(make-elliptic-bandpass n fl fh (ripple 1.0) (loss-dB 60.0)) returns a bandpass elliptic filter; n = order, \
fl and fh are edge freqs (srate=1.0): (make-elliptic-bandpass 6 .1 .2 .1 90)"))
    (lambda* (n fl fh (ripple 1.0) (loss-dB 60.0))
      (if (odd? n) (set! n (+ n 1)))
      (let ((lp (make-elliptic-lowpass n fh ripple loss-dB))
	    (hp (make-elliptic-highpass n fl ripple loss-dB)))
	(lambda (y) 
	  (filter lp (filter hp y)))))))

(define make-elliptic-bandstop 
  (let ((+documentation+ "(make-elliptic-bandstop n fl fh (ripple 1.0) (loss-dB 60.0)) returns a bandstop elliptic filter; n = order, \
fl and fh are edge freqs (srate=1.0): (make-elliptic-bandstop 6 .1 .2 .1 90)"))
    (lambda* (n fl fh (ripple 1.0) (loss-dB 60.0))
      (if (odd? n) (set! n (+ n 1)))
      (make-bandstop-filter (make-elliptic-lowpass n fl ripple loss-dB)
			    (make-elliptic-highpass n fh ripple loss-dB)))))
