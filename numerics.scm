(provide 'snd-numerics.scm)

(when (provided? 'pure-s7)
  (define (make-polar mag ang)
    (if (and (real? mag) (real? ang))
	(complex (* mag (cos ang)) (* mag (sin ang)))
	(error 'wrong-type-arg "make-polar args should be real"))))

;;; random stuff I needed at one time or another while goofing around...
;;;   there are a lot more in snd-test.scm

(define factorial
  (let* ((num-factorials 128)
	 (factorials (let ((temp (make-vector num-factorials 0)))
		       (set! (temp 0) 1) ; is this correct?
		       (set! (temp 1) 1)
		       temp)))
    (lambda (n)
      (if (> n num-factorials)
	  (let ((old-num num-factorials)
		(old-facts factorials))
	    (set! num-factorials n)
	    (set! factorials (make-vector num-factorials 0))
	    (do ((i 0 (+ i 1)))
		((= i old-num))
	      (set! (factorials i) (old-facts i)))))
      (if (zero? (factorials n))
	  (set! (factorials n) (* n (factorial (- n 1)))))
      (factorials n))))

;; (factorial 3) 6

#|
;;; maxima uses this:

;; From Richard Fateman's paper, "Comments on Factorial Programs",
;; http://www.cs.berkeley.edu/~fateman/papers/factorial.pdf
;;
;; k(n,m) = n*(n-m)*(n-2*m)*...
;;
;; (k n 1) is n!
;;
;; This is much faster (3-4 times) than the original factorial
;; function.

(define (factorial n)
  (define (k n m)
    (if (<= n m)
	n
	(* (k n (* 2 m))
	   (k (- n m) (* 2 m)))))
  (if (zero? n)
      1
      (k n 1)))
|#

(define (binomial-direct n m) 
  (/ (factorial n)
     (* (factorial m) (factorial (- n m)))))

(define n-choose-k 
  (let ((+documentation+ "(n-choose-k n k) computes the binomial coefficient C(N,K)"))
    (lambda (n k)
      (let ((mn (min k (- n k))))
	(if (< mn 0)
	    0
	    (if (= mn 0)
		1
		(let ((mx (max k (- n k))))
		  (do ((cnk (+ 1 mx))
		       (i 2 (+ i 1)))
		      ((> i mn) cnk)
		    (set! cnk (/ (* cnk (+ mx i)) i))))))))))
 
;; (n-choose-k 10 6) 210
;; (n-choose-k 10 9) 10
;; same for binomial-direct
    

;;; --------------------------------------------------------------------------------
;;; from Numerical Recipes
(define (plgndr L m x)			;Legendre polynomial P m/L (x), m and L integer
					;0 <= m <= L and -1<= x <= 1 (x real)
  (if (or (not (<= 0 m L)) 
	  (> (abs x) 1.0))
      (error 'wrong-type-arg "invalid arguments to plgndr: ~A ~A ~A" L m x)
      (let ((pmm 1.0)
	    (fact 0.0) 
	    (somx2 0.0))
	(if (> m 0)
	    (begin
	      (set! somx2 (sqrt (* (- 1.0 x) (+ 1.0 x))))
	      (set! fact 1.0)
	      (do ((i 1 (+ i 1)))
		  ((> i m))
		(set! pmm (* (- pmm) fact somx2))
		(set! fact (+ fact 2.0)))))
	(if (= L m) 
	    pmm
	    (let ((pmmp1 (* x pmm (+ (* 2 m) 1))))
	      (if (= L (+ m 1)) 
		  pmmp1
		  (do ((pk 0.0) ; NR used "ll" which is unreadable
		       (k (+ m 2) (+ k 1)))
		      ((> k L) pk)
		    (set! pk (/ (- (* x (- (* 2 k) 1) pmmp1) 
				   (* (+ k m -1) pmm)) 
				(- k m)))
		    (set! pmm pmmp1)
		    (set! pmmp1 pk))))))))


;;; A&S (bessel.lisp)
(define (legendre-polynomial a x) ; sum of weighted polynomials (m=0)
  (let ((n (- (length a) 1)))
    (if (= n 0) 
	(a 0)
	(let ((r x)
	      (s 1.0)
	      (h 0.0)
	      (sum (a 0)))
	  (do ((k 1 (+ k 1)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (a k))))
	    (set! r (/ (- (* r x (+ (* 2 k) 1))  (* s k)) (+ k 1)))
	    (set! s h))
	  (+ sum (* r (a n)))))))

(define (legendre n x)
  (legendre-polynomial (let ((v (make-vector (+ 1 n) 0.0)))
			 (set! (v n) 1.0)
			 v)
		       x))

;;; (with-sound (:scaled-to 0.5) (do ((i 0 (+ i 1)) (x 0.0 (+ x .1))) ((= i 10000)) (outa i (legendre 20 (cos x)))))
;; (legendre 3 1.0) 1.0

#|
;; if l odd, there seems to be sign confusion:
(with-sound (:channels 2 :scaled-to 1.0)
  (do ((i 0 (+ i 1))
       (theta 0.0 (+ theta 0.01)))
      ((= i 10000))
    (outa i (plgndr 1 1 (cos theta)))
    (let ((x (sin theta)))
      (outb i (- x)))))

;; this works:
(with-sound (:channels 2 :scaled-to 1.0)
  (do ((i 0 (+ i 1))
       (theta 0.0 (+ theta 0.01)))
      ((= i 10000))
    (let ((x (cos theta)))
      (outa i (plgndr 3 0 x))
      (outb i (* 0.5 x (- (* 5 x x) 3))))))
|#


(define* (gegenbauer n x (alpha 0.0))
  (set! alpha (max alpha -0.5))
  (cond ((= n 0)       1.0)
	((= alpha 0.0) (* (/ 2.0 n) (cos (* n x))))           ; maxima and A&S 22.3.14 (gsl has bogus values here)
	(else
	 (case n
	   ((1)       (* 2 alpha x))                          ; G&R 8.93(2)
	   ((2)       (- (* 2 alpha (+ alpha 1) x x) alpha))  ; G&R 8.93(3)
	   (else
	    (let ((fn1 (* 2 x alpha))
		  (fn 0.0000)
		  (fn2 1.0000))
	      (if (= n 1)
		  fn1
		  (do ((k 2 (+ k 1))
		       (k0 2.0 (+ k0 1.0)))
		      ((> k n) fn)
		    (set! fn (/ (- (* 2 x fn1 (+ k alpha -1.0)) (* fn2 (+ k (* 2 alpha) -2.0))) k0))
		    (set! fn2 fn1)
		    (set! fn1 fn)))))))))

;;; (with-sound (:scaled-to 0.5) (do ((i 0 (+ i 1)) (x 0.0 (+ x .1))) ((= i 10000)) (outa i (gegenbauer 15 (cos x) 1.0))))
;; (gegenbauer 3 1.0) -0.6599949977336302

#|
(with-sound (:scaled-to 0.5)
  (do ((i 0 (+ i 1))
       (theta 0.0 (+ theta 0.05)))
      ((= i 10000))
    (let ((x (cos theta)))
      (outa i (gegenbauer 20 x)))))
|#


(define* (chebyshev-polynomial a x (kind 1))
  (let ((n (- (length a) 1)))
    (if (= n 0) 
	(a 0)
	(let ((r (* kind x))
	      (s 1.0)
	      (h 0.0)
	      (sum (a 0)))
	  (do ((k 1 (+ k 1)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (a k))))
	    (set! r (- (* 2 r x) s))
	    (set! s h))
	  (+ sum (* r (a n)))))))

(define* (chebyshev n x (kind 1))
  (let ((a (make-vector (+ 1 n) 0.0)))
    (set! (a n) 1.0)
    (chebyshev-polynomial a x kind)))

;; (chebyshev 3 1.0) 1.0


(define (hermite-polynomial a x)
  (let ((n (- (length a) 1)))
    (if (= n 0) 
	(a 0)
	(let ((r (* 2 x))
	      (s 1.0)
	      (h 0.0)
	      (sum (a 0)))
	  (do ((k 1 (+ k 1))
	       (k2 2 (+ k2 2)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (a k))))
	    (set! r (- (* 2 r x) (* k2 s)))
	    (set! s h))
	  (+ sum (* r (a n)))))))

(define* (hermite n x)
  (let ((a (make-vector (+ 1 n) 0.0)))
    (set! (a n) 1.0)
    (hermite-polynomial a x)))

;; (hermite 3 1.0) -4.0


(define* (laguerre-polynomial a x (alpha 0.0))
  (let ((n (- (length a) 1)))
    (if (= n 0) 
	(a 0)
	(let ((r (- (+ alpha 1.0) x))
	      (s 1.0)
	      (h 0.0)
	      (sum (a 0)))
	  (do ((k 1 (+ k 1)))
	      ((= k n))
	    (set! h r)
	    (set! sum (+ sum (* r (a k))))
	    (set! r (/ (- (* r (- (+ (* 2 k) 1 alpha) x)) 
			  (* s (+ k alpha)))
		       (+ k 1)))
	    (set! s h))
	  (+ sum (* r (a n)))))))

(define* (laguerre n x (alpha 0.0))
  (let ((a (make-vector (+ 1 n) 0.0)))
    (set! (a n) 1.0)
    (laguerre-polynomial a x alpha)))

;; (laguerre 3 1.0) -0.6666666666666666


#|
;;; --------------------------------------------------------------------------------
;;; 
;;; just for my amusement -- apply a linear-fractional or Mobius transformation to the fft data (treated as complex)
;;; 
;;; (automorph 1 0 0 1) is the identity
;;; (automorph 2 0 0 1) scales by 2
;;; (automorph 0.0+1.0i 0 0 1) rotates 90 degrees (so 4 times = identity)
;;; most cases won't work right because we're assuming real output and so on

(define* (automorph a b c d snd chn)
  (let* ((len (framples snd chn))
	 (pow2 (ceiling (log len 2)))
	 (fftlen (floor (expt 2 pow2)))
	 (fftlen2 (/ fftlen 2))
	 (fftscale (/ 1.0 fftlen))
	 (rl (channel->float-vector 0 fftlen snd chn))
	 (im (make-float-vector fftlen)))
    (fft rl im 1)
    (float-vector-scale! rl fftscale)
    (float-vector-scale! im fftscale)
    ;; handle 0 case by itself
    (let* ((c1 (complex (rl 0) (im 0)))
	   (val (/ (+ (* a c1) b)
		   (+ (* c c1) d)))
	   (rval (real-part val))
	   (ival (imag-part val)))
      (set! (rl 0) rval)
      (set! (im 0) ival))
    (do ((i 1 (+ i 1))
	 (k (- fftlen 1) (- k 1)))
	((= i fftlen2))
      (let* ((c1 (complex (rl i) (im i)))
	     (val (/ (+ (* a c1) b)      ; (az + b) / (cz + d)
		     (+ (* c c1) d)))
	     (rval (real-part val))
	     (ival (imag-part val)))
	(set! (rl i) rval)
	(set! (im i) ival)
	(set! (rl k) rval)
	(set! (im k) (- ival))))
    (fft rl im -1)
    (float-vector->channel rl 0 len snd chn #f (format #f "automorph ~A ~A ~A ~A" a b c d))))
|#


#|
;;; --------------------------------------------------------------------------------
;;; these are in snd-xen.c??

(define (bes-i1 x)				;I1(x)
  (if (< (abs x) 3.75)
      (let ((y (expt (/ x 3.75) 2)))
	(* x (+ 0.5
		(* y (+ 0.87890594
			(* y (+ 0.51498869
				(* y (+ 0.15084934
					(* y (+ 0.2658733e-1
						(* y (+ 0.301532e-2
							(* y 0.32411e-3))))))))))))))
      (let* ((ax (abs x))
	     (y (/ 3.75 ax))
	     (ans1 (+ 0.2282967e-1
		      (* y (+ -0.2895312e-1
			      (* y (+ 0.1787654e-1 
				      (* y -0.420059e-2)))))))
	     (ans2 (+ 0.39894228
		      (* y (+ -0.3988024e-1
			      (* y (+ -0.362018e-2
				      (* y (+ 0.163801e-2
					      (* y (+ -0.1031555e-1 (* y ans1)))))))))))
	     (sign (if (< x 0.0) -1.0 1.0)))
	(* (/ (exp ax) (sqrt ax)) ans2 sign))))

(define (bes-in n x)			;return In(x) for any integer n, real x
  (if (= n 0) 
      (bes-i0 x)
      (if (= n 1) 
	  (bes-i1 x)
	  (if (= x 0.0) 
	      0.0
	      (let* ((iacc 40)
		     (bigno 1.0e10)
		     (bigni 1.0e-10)
		     (ans 0.0)
		     (tox (/ 2.0 (abs x)))
		     (bip 0.0)
		     (bi 1.0)
		     (m (* 2 (+ n (truncate (sqrt (* iacc n))))))
		     (bim 0.0))
		(do ((j m (- j 1)))
		    ((= j 0))
		  (set! bim (+ bip (* j tox bi)))
		  (set! bip bi)
		  (set! bi bim)
		  (if (> (abs bi) bigno)
		      (begin
			(set! ans (* ans bigni))
			(set! bi (* bi bigni))
			(set! bip (* bip bigni))))
		  (if (= j n) (set! ans bip)))
		(if (and (< x 0.0) (odd? n)) (set! ans (- ans)))
		(* ans (/ (bes-i0 x) bi)))))))
|#


;;; --------------------------------------------------------------------------------

(define (aux-f x)			;1<=x<inf
  (let ((x2 (* x x)))
    (/ (+ 38.102495 (* x2 (+ 335.677320 (* x2 (+ 265.187033 (* x2 (+ 38.027264 x2)))))))
       (* x (+ 157.105423 (* x2 (+ 570.236280 (* x2 (+ 322.624911 (* x2 (+ 40.021433 x2)))))))))))

(define (aux-g x)
  (let ((x2 (* x x)))
    (/ (+ 21.821899 (* x2 (+ 352.018498 (* x2 (+ 302.757865 (* x2 (+ 42.242855 x2)))))))
       (* x2 (+ 449.690326 (* x2 (+ 1114.978885 (* x2 (+ 482.485984 (* x2 (+ 48.196927 x2)))))))))))

(define (Si x) 
  (if (>= x 1.0)
      (- (/ pi 2) (* (cos x) (aux-f x)) (* (sin x) (aux-g x)))
      (do ((sum x)
	   (fact 2.0)
	   (one -1.0)
	   (xs x)
	   (x2 (* x x))
	   (err .000001)
	   (unhappy #t)
	   (i 3.0 (+ i 2.0)))
	  ((not unhappy) sum)
	(set! xs (/ (* one x2 xs) (* i fact)))
	(set! one (- one))
	(set! fact (+ 1 fact))
	(set! xs (/ xs fact))
	(set! unhappy (> (abs xs) err))
	(set! sum (+ sum xs)))))

(define (Ci x) 
  (if (>= x 1.0)
      (- (* (sin x) (aux-f x)) (* (cos x) (aux-g x)))
      (do ((g .5772156649)
	   (sum 0.0)
	   (fact 1.0)
	   (one -1.0)
	   (xs 1.0)
	   (x2 (* x x))
	   (err .000001)
	   (unhappy #t)
	   (i 2.0 (+ i 2.0)))
	  ((not unhappy) 
	   (+ g (log x) sum))
	(set! xs (/ (* one x2 xs) (* i fact)))
	(set! one (- one))
	(set! fact (+ 1 fact))
	(set! xs (/ xs fact))
	(set! unhappy (> (abs xs) err))
	(set! sum (+ sum xs)))))

;; (Si 1.0) 0.9460830708394717
;; (Ci 1.0) 0.3374039233633503



;;; --------------------------------------------------------------------------------

(define bernoulli3
  (let ((saved-values (copy #(1 -1/2 1/6 0 -1/30 0 1/42 0 -1/30 0 5/66 0 -691/2730
			      0 7/6 0 -3617/510 0 43867/798 0 -174611/330 0
			      854513/138 0 -236364091/2730 0 8553103/6 0
			      -23749461029/870 0 8615841276005/14322 0)
			    (make-vector 100 #f))))
    (lambda (n)
      (if (number? (saved-values n))
	  (saved-values n)
	  (let ((value (if (odd? n) 
			   0.0
			   (let ((sum2 0.0)
				 (itmax 1000)
				 (tol 5.0e-7)
				 (close-enough #f))
			     (do ((i 1 (+ i 1)))
				 ((or close-enough
				      (> i itmax)))
			       (let ((term (/ 1.0 (expt i n))))
				 (set! sum2 (+ sum2 term))
				 (set! close-enough (or (< (abs term) tol)
							(< (abs term) (* tol (abs sum2)))))))
			     (/ (* 2.0 sum2 (factorial n)
				   (if (= (modulo n 4) 0) -1 1))
				(expt (* 2.0 pi) n))))))
	    (set! (saved-values n) value))))))

(define (bernoulli-poly n x)
  (let ((fact 1.0)
	(value (bernoulli3 0)))
    (do ((i 1 (+ i 1)))
	((> i n) value)
      (set! fact (* fact (/ (- (+ n 1) i) i)))
      (set! value (+ (* value x)
		     (* fact (bernoulli3 i)))))))

#|
;; (bernoulli-poly 1 1.0) 0.5

(with-sound (:clipped #f :channels 2)
  (let ((x 0.0)
	(incr (hz->radians 100.0))
	(N 2))
    (do ((i 0 (+ i 1)))
	((= i 44100))
      (outa i (* (expt -1 (- N 1)) 
		 (/ 0.5 (factorial N))
		 (expt (* 2 pi) (+ (* 2 N) 1))
		 (bernoulli-poly (+ (* 2 N) 1) (/ x (* 2 pi)))))
      (outb i (* (expt -1 (- N 1)) 
		 (/ 0.5 (factorial N))
		 (expt (* 2 pi) (+ (* 2 N) 1))
		 (bernoulli-poly (+ (* 2 N) 0) (/ x (* 2 pi)))))
      (set! x (+ x incr))
      (if (> x (* 2 pi)) (set! x (- x (* 2 pi)))))))
|#


;;; --------------------------------------------------------------------------------

(define (sin-m*pi/n m1 n1)

  ;; this returns an expression giving the exact value of sin(m*pi/n), m and n integer
  ;;   if we can handle n -- currently it can be anything of the form 2^a 3^b 5^c 7^d 11^h 13^e 17^f 257^g
  ;;   so (sin-m*pi/n 1 60) returns an exact expression for sin(pi/60).

  (let ((m (numerator (/ m1 n1)))
	(n (denominator (/ m1 n1))))

    (set! m (modulo m (* 2 n)))
    ;; now it's in lowest terms without extra factors of 2*pi

    (cond ((zero? m) 0)

	  ((zero? n) (error 'divide-by-zero "divide by zero (sin-m*pi/n n = 0)"))

	  ((= n 1) 0)

	  ((negative? n)
	   (let ((val (sin-m*pi/n m (- n))))
	     (and val (list '- val))))

	  ((> m n) 
	   (let ((val (sin-m*pi/n (- m n) n)))
	     (and val (list '- val))))

	  ((= n 2) (if (= m 0) 0 1))

	  ((= n 3) `(sqrt 3/4))

	  ((> m 1)
	   (let ((m1 (sin-m*pi/n (- m 1) n))
		 (n1 (sin-m*pi/n 1 n))
		 (m2 (sin-m*pi/n (- m 2) n)))
	     (and m1 m2 n1
		  `(- (* 2 ,m1 (sqrt (- 1 (* ,n1 ,n1)))) ,m2))))

	  ((= n 5) `(/ (sqrt (- 10 (* 2 (sqrt 5)))) 4))

	  ((= n 7) `(let ((A1 (expt (+ -7/3456 (sqrt -49/442368)) 1/3))
			  (A2 (expt (- -7/3456 (sqrt -49/442368)) 1/3)))
		      (sqrt (+ 7/12 (* -1/2 (+ A1 A2)) (* 1/2 0+i (sqrt 3) (- A1 A2))))))

	  ((= n 17) `(let* ((A1 (sqrt (- 17 (sqrt 17))))
			    (A2 (sqrt (+ 17 (sqrt 17))))
			    (A3 (sqrt (+ 34 (* 6 (sqrt 17)) (* (sqrt 2) (- (sqrt 17) 1) A1) (* -8 (sqrt 2) A2)))))
		       (* 1/8 (sqrt 2) (sqrt (- 17 (sqrt 17) (* (sqrt 2) (+ A1 A3)))))))


	  ((= n 11) `(let* ((SQRT5 (* 1/2 (- (sqrt 5) 1)))
			    (B5 (sqrt (+ 2 (* 1/2 (+ (sqrt 5) 1)))))
			    (B6 (+ SQRT5 (* 0+i B5)))
			    (B6_2 (* B6 B6))
			    (B6_3 (* B6 B6 B6))
			    (B6_4 (* B6 B6 B6 B6)) 
			    (D1 (+ 6 (* 3/2 B6) (* 3/4 B6_2)))
			    (D2 (+ SQRT5 (* 0+i B5) (* 1/4 B6_2)))
			    (D3 (* 1/2 (- (+ 1 (* 0+i (sqrt 11))))))
			    (D4 (* 1/2 (- (* 0+i (sqrt 11)) 1)))
			    (D6 (+ 1 (* 1/4 B6_2) (* 1/8 B6_3)))
			    (D7 (+ (* 1/2 B6) (* 1/16 B6_4)))
			    (D8 (+ 2 (* 1/2 B6)))
			    (D9 (+ 13 (* 21 B6) (* 67/4 B6_2) (* 21/4 B6_3) (* 2 B6_4)))
			    (D11 (+ 129 (* 109/2 B6) (* 59/4 B6_2) (* 9/8 B6_3) (* 9/16 B6_4)))
			    (D13 (+ (* 3 B6) B6_2))
			    (D14 (+ (* 3 B6) (* 3/4 B6_2) (* 3/8 B6_3)))
			    (D15 (+ 79 (* 27 B6) (* 39/4 B6_2) (* 37/4 B6_3) (* 21/4 B6_4)))
			    (D16 (+ D11 (* D3 D9) (* D4 D15)))
			    (D17 (+ D11 (* D4 D9) (* D3 D15)))
			    (D30 (/ (* B6_2 (+ (* D3 D6) (* D4 D7))) (* 4 (expt D17 1/5))))
			    (D32 (* 1/2 (+ 1 (* 0+i (sqrt 11)))))
			    (D33 (/ (* B6_3 (+ (* D4 D6) (* D3 D7))) (* 8 (expt D16 1/5))))
			    (D34 (* 1/4 B6_2 (expt D16 1/5)))
			    (D35 (+ 2 (* 1/8 B6_4) (* 1/2 B6 D8) (* 1/8 B6_3 D2)))
			    (D36 (+ SQRT5 (* 0+i B5) (* 1/2 B6_2) (* 1/4 B6_3) (* 1/2 B6 D2) (* 1/4 B6_2 (+ (* 1/4 B6_3) (* 1/16 B6_4)))))
 			    (D38 (* 1/2 (+ -1 (* 0+i (sqrt 11)))))
			    (D39 (* 1/8 B6_3 (expt D17 1/5)))
			    (D40 (+ 3 (* 3/2 B6) (* 9/4 B6_2) (* 7/8 B6_3) (* 3/16 B6_4) (* 1/2 B6 (+ 6 (* 2 B6)))
				    (* 1/16 B6_4 D1) (* 1/4 B6_2 D13) (* 1/8 B6_3 (+ 3 (* 3/4 B6_3) (* 3/16 B6_4)))))
			    (D41 (+ (* 1/4 B6_2 D1) (* 1/8 B6_3 D13) (* 1/16 B6_4 D14) (* 1/2 B6 (+ 4 (* 3/8 B6_4)))))
			    (D42 (+ 3 (* 3/4 B6_3) (* 3/16 B6_4) (* 1/8 B6_3 D1) (* 1/4 B6_2 D14) 
				    (* 1/2 B6 (+ (* 3/2 B6_2) (* 3/8 B6_3) (* 3/16 B6_4))) (* 1/16 B6_4 (+ 3 (* 3/2 B6) (* 3/8 B6_4)))))
			    (D43 (+ (* 1/4 B6_3) (* 1/16 B6_4) (* 1/8 B6_3 D8) (* 1/4 B6_2 D2) 
				    (* 1/2 B6 (+ (* 1/2 B6_2) (* 1/8 B6_3))) (* 1/16 B6_4 (+ 1 (* 1/8 B6_4)))))
			    (D44 (/ (* B6_4 (+ D42 (* D4 D40) (* D3 D41))) (* 16 (expt D16 3/5))))
			    (D45 (/ (* B6 (+ D42 (* D3 D40) (* D4 D41))) (* 2 (expt D17 3/5))))
			    (D48 (/ (* B6 (+ D43 (* D3 D35) (* D4 D36))) (* 2 (expt D16 2/5))))
			    (D49 (/ (* B6_4 (+ D43 (* D4 D35) (* D3 D36))) (* 16 (expt D17 2/5)))))
		       (* -1/2 0+i 
			  (+ (* 1/5 (- D32 D33 D34 D48 D44))
			     (* 1/5 (+ D38 D30 D39 D49 D45))))))
	   
	  ((= n 13)
	   `(let* ((A1 (/ (- -1 (sqrt 13)) 2))
		   (A2 (/ (+ -1 (sqrt 13)) 2))
		   (A3 (/ (+ -1 (* 0+i (sqrt 3))) 2))
		   (A4 (+ -1 (* 0+i (sqrt 3))))
		   (A5 (* 0+i (sqrt (+ 7 (sqrt 13) A2))))
		   (A6 (* 0+i (sqrt (+ 7 (- (sqrt 13)) A1))))
		   (A8  (* 1/2 (- A2 A6)))
		   (A9  (* 1/2 (+ A1 A5)))
		   (A11 (* 1/2 (+ A2 A6)))
		   (A12 (* 1/2 (- A1 A5)))
		   (A13 (* 3/2 A4 A8))
		   (A14 (* 3/2 A4 A11))
		   (A15 (* 3/4 A4 A4 A11))
		   (A16 (* 3/4 A4 A4 A8))
		   (A17 (+ A3 (* 1/4 A4 A4))))
	      (* -1/6 0+i (+ (- A9 A12)
			     (* A4 (+ (/ (+ A8 (* A17 A12)) (* 2 (expt (+ 6 A13 A15 A9) 1/3)))
				      (/ (+ A11 (* A17 A9)) (* -2 (expt (+ 6 A16 A14 A12) 1/3)))
				      (* 1/4 A4 (- (expt (+ 6 A13 A15 A9) 1/3) (expt (+ 6 A16 A14 A12) 1/3)))))))))
	  
	  ((= n 257)
	   `(let* ((A1 (sqrt (- 514 (* 2 (sqrt 257)))))
		   (A2 (- 257 (* 15 (sqrt 257))))
		   (A3 (+ 257 (* 15 (sqrt 257))))
		   (A4 (- 257 (sqrt 257)))
		   (A5 (+ (sqrt 257) 257))
		   (A7 (+ 257 (* 9 (sqrt 257))))
		   (A8 (- 514 (* 18 (sqrt 257))))
		   (AA (sqrt (* 2 A5)))
		   (A9 (sqrt (+ A2 (* 8 A1) (* -7 AA))))
		   (A10 (sqrt (+ A2 (* -8 A1) (* 7 AA))))
		   (A11 (sqrt (+ A3 (* 7 A1) (* 8 AA))))
		   (A12 (sqrt (+ A3 (* -7 A1) (* -8 AA))))
		   (A13 (sqrt (+ A8 (* 6 A1) (* 8 A9) (* -24 A10) (* 12 A11))))
		   (A14 (* 4 (sqrt (+ A8 (* 6 A1) (* -8 A9) (* 24 A10) (* -12 A11)))))
		   (A15 (* 4 (sqrt (+ A8 (* -6 A1) (* -12 A12) (* 24 A9) (* 8 A10)))))
		   (A16 (* 4 (sqrt (* 2 (+ (- 257 (* 9 (sqrt 257))) (* -3 A1) (* 6 A12) (* -12 A9) (* -4 A10))))))
		   (A17 (sqrt (* 2 (+ A7 (* -3 AA) (* -4 A12) (* 6 A9) (* 12 A11)))))
		   (A18 (sqrt (* 2 (+ A7 (* 3 AA) (* 12 A12) (* -6 A10) (* 4 A11)))))
		   (A19 (* 4 (sqrt (* 2 (+ A7 (* 3 AA) (* -12 A12) (* 6 A10) (* -4 A11))))))
		   (A20 (* 4 (sqrt (* 2 (+ A7 (* -3 AA) (* 4 A12) (* -6 A9) (* -12 A11))))))
		   (A22 (+ A4 (* 3 A1) (* -4 AA) (* -4 A12) (* 4 A9) (* -4 A10) (* 2 A11)))
		   (A23 (+ A5 (* -4 A1) (* -3 AA) (* -4 A12) (* 2 A9) (* 4 A10) (* 4 A11)))
		   (A24 (+ A5 (* 4 A1) (* 3 AA) (* 4 A12) (* 4 A9) (* -2 A10) (* 4 A11)))
		   (A26 (sqrt (+ A22 (+ (- A16) (- A19) (* 4 A17) (* -6 A13)))))
		   (A27 (sqrt (+ A22 (+ A16 A19 (* -4 A17) (* 6 A13)))))
		   (A28 (+ 257 (* 7 (sqrt 257)) (* 3 A1) (* -4 A9) (* 4 A10) (* 6 A11)))
		   (A29 (* 8 (sqrt (+ A24 A15 (- A20) (* -6 A18) (* -4 A13)))))
		   (A30 (+ A28 (* -4 A18) (* -4 A17) (* -2 A13)))
		   (A31 (+ (* 8 (sqrt (+ A23 A15 A14 (* 4 A18) (* -6 A17)))) (* 4 A26) A29 (* -8 A27))))
	      (* 1/16 (sqrt (* 1/2 (+ A4 (- A1) (* -2 A11) (* -2 A13) (* -4 A26)
				      (* -4 (sqrt (* 2 (+ A30 (- A31)))))
				      (* -8 (sqrt (+ A4 (- A1) (* -2 A11) (* 6 A13) (* -4 A26) (* -8 A27)
						     (* 4 (sqrt (* 2 (+ A30 A31))))
						     (* -8 (sqrt (* 2 (+ A28 (* 4 A18) (* 4 A17) (* 2 A13) (* -8 A26) (* -4 A27)
									 (* -8 (sqrt (+ A23 (- A15) (- A14) (* -4 A18) (* 6 A17))))
									 (* -8 (sqrt (+ A24 (- A15) A20 (* 6 A18) (* 4 A13)))))))))))))))))
	   
	  ((or (= (modulo n 2) 0) (= (modulo n 3) 0) (= (modulo n 5) 0) (= (modulo n 7) 0) 
	       (= (modulo n 17) 0) (= (modulo n 13) 0) (= (modulo n 257) 0) (= (modulo n 11) 0))
	   (let* ((divisor (cond ((= (modulo n 2) 0) 2)
				 ((= (modulo n 3) 0) 3)
				 ((= (modulo n 5) 0) 5)
				 ((= (modulo n 7) 0) 7)
				 ((= (modulo n 17) 0) 17)
				 ((= (modulo n 13) 0) 13)
				 ((= (modulo n 11) 0) 11)
				 (else 257)))
		  (val (sin-m*pi/n 1 (/ n divisor))))
	     (and val
		  `(let ((ex ,val))
		     (/ (- (expt (+ (sqrt (- 1 (* ex ex))) (* 0+i ex)) (/ 1 ,divisor))
			   (expt (- (sqrt (- 1 (* ex ex))) (* 0+i ex)) (/ 1 ,divisor)))
			0+2i)))))
	  (else #f))))

#|
(let ((maxerr 0.0)
      (max-case #f)
      (cases 0))
  (do ((n 1 (+ n 1)))
      ((= n 10000))
    (do ((m 1 (+ m 1)))
	((= m 4))
      (let ((val (sin (/ (* m pi) n)))
	    (expr (sin-m*pi/n m n)))
	(if expr 
	    (let ((err (magnitude (- val (eval expr)))))
	      (set! cases (+ cases 1))
	      (if (> err maxerr)
		  (begin
		    (set! maxerr err)
		    (set! max-case (/ m n)))))))))
  (format () "sin-m*pi/n (~A cases) max err ~A at ~A~%" cases maxerr max-case))

:(sin (/ pi (* 257 17)))
0.00071906440440859
:(eval (sin-m*pi/n 1 (* 17 257)))
0.00071906440440875

|#


;;; --------------------------------------------------------------------------------

(define show-digits-of-pi-starting-at-digit
  ;; piqpr8.c
  ;;
  ;;    This program implements the BBP algorithm to generate a few hexadecimal
  ;;    digits beginning immediately after a given position id, or in other words
  ;;    beginning at position id + 1.  On most systems using IEEE 64-bit floating-
  ;;    point arithmetic, this code works correctly so long as d is less than
  ;;    approximately 1.18 x 10^7.  If 80-bit arithmetic can be employed, this limit
  ;;    is significantly higher.  Whatever arithmetic is used, results for a given
  ;;    position id can be checked by repeating with id-1 or id+1, and verifying 
  ;;    that the hex digits perfectly overlap with an offset of one, except possibly
  ;;    for a few trailing digits.  The resulting fractions are typically accurate 
  ;;    to at least 11 decimal digits, and to at least 9 hex digits.  
  ;;
  ;;  David H. Bailey     2006-09-08
  ;;
  ;; translated to Scheme 29-Dec-08

  (let ((ihex (lambda (x nhx chx)
		;; This returns, in chx, the first nhx hex digits of the fraction of x.
		(do ((y (abs x))
		     (hx "0123456789ABCDEF")
		     (i 0 (+ i 1)))
		    ((= i nhx) chx)
		  (set! y (* 16.0 (- y (floor y))))
		  (set! (chx i) (hx (floor y))))))
	(series (lambda (m id)
		  ;; This routine evaluates the series  sum_k 16^(id-k)/(8*k+m) using the modular exponentiation technique.
		  (let ((expm (let ((ntp 25))
				(let ((tp1 0)
				      (tp (make-vector ntp)))
				  (lambda (p ak)
				    ;; expm = 16^p mod ak.  This routine uses the left-to-right binary exponentiation scheme.
				    
				    ;; If this is the first call to expm, fill the power of two table tp.
				    (if (= tp1 0)
					(begin
					  (set! tp1 1)
					  (set! (tp 0) 1.0)
					  (do ((i 1 (+ i 1)))
					      ((= i ntp))
					    (set! (tp i) (* 2.0 (tp (- i 1)))))))
				    
				    (if (= ak 1.0)
					0.0
					(let ((pl -1))
					  ;;  Find the greatest power of two less than or equal to p.
					  (do ((i 0 (+ i 1)))
					      ((or (not (= pl -1)) 
						   (= i ntp)))
					    (if (> (tp i) p)
						(set! pl i)))
					  
					  (if (= pl -1) (set! pl ntp))
					  (let ((pt (tp (- pl 1)))
						(p1 p)
						(r 1.0))
					    ;;  Perform binary exponentiation algorithm modulo ak.
					    
					    (do ((j 1 (+ j 1)))
						((> j pl) r)
					      (if (>= p1 pt)
						  (begin
						    (set! r (* 16.0 r))
						    (set! r (- r (* ak (floor (/ r ak)))))
						    (set! p1 (- p1 pt))))
					      (set! pt (* 0.5 pt))
					      (if (>= pt 1.0)
						  (begin
						    (set! r (* r r))
						    (set! r (- r (* ak (floor (/ r ak)))))))))))))))
			(eps 1e-17)
			(s 0.0))
		    (do ((k 0 (+ k 1)))
			((= k id))
		      (let* ((ak (+ (* 8 k) m))
			     (t (expm (- id k) ak)))
			(set! s (+ s (/ t ak)))
			(set! s (- s (floor s)))))
		    
		    ;; Compute a few terms where k >= id.
		    (do ((happy #f)
			 (k id (+ k 1)))
			((or (> k (+ id 100)) happy) s)
		      (let ((t (/ (expt 16.0 (- id k)) (+ (* 8 k) m))))
			(set! happy (< t eps))
			(set! s (+ s t))
			(set! s (- s (floor s)))))))))
    (lambda (id)  
      ;; id is the digit position.  Digits generated follow immediately after id.
      (let ((chx (make-string 17))
	    (pid (let ((s1 (series 1 id))
		       (s2 (series 4 id))
		       (s3 (series 5 id))
		       (s4 (series 6 id)))
		   (- (+ (* 4.0 s1) (* -2.0 s2)) s3 s4))))
	(set! pid (- (+ 1.0 pid) (floor pid)))
	(ihex pid 10 chx)
	(format #f " position = ~D~% fraction = ~,15F~% hex digits =  ~S~%" id pid chx)))))
  
#|
(show-digits-of-pi-starting-at-digit 0)
" position = 0
 fraction = 1.141592653589793
 hex digits =  \"243F6A8885       \"
"
(show-digits-of-pi-starting-at-digit 1000)
" position = 1000
 fraction = 1.288845098351256
 hex digits =  \"49F1C09B07       \"
"
|#

#|
;;; from the CL bboard, perhaps written by Justin Grant
;;;   requires gmp (bignums)

(define (machin-pi digits)

  (define (arccot-minus xsq n xpower)
    (let ((term (floor (/ xpower n))))
      (if (= term 0)
        0
        (- (arccot-plus xsq (+ n 2) (floor (/ xpower xsq)))
           term))))       

  (define (arccot-plus xsq n xpower)
    (let ((term (floor (/ xpower n))))
      (if (= term 0)
        0
        (+ (arccot-minus xsq (+ n 2) (floor (/ xpower xsq)))
           term))))

  (define (arccot x unity)
    (let ((xpower (floor (/ unity x))))
      (arccot-plus (* x x) 1 xpower)))

  (let* ((unity (expt 10 (+ digits 10)))
         (thispi (* 4 (- (* 4 (arccot 5 unity)) (arccot 239 unity)))))
    (floor (/ thispi (expt 10 10)))))
|#



;;; --------------------------------------------------------------------------------

(define* (sin-nx-peak n (err 1e-12))
  ;; return the min peak amp and its location for sin(x)+sin(nx+a)
  (let ((size (* n 100)))
    (let ((incr (/ (* 2 pi) size))
	  (peak 0.0)
	  (location 0.0)
	  (offset (if (= (modulo n 4) 3) 0 pi)))
      (do ((i 0 (+ i 1))
	   (x 0.0 (+ x incr)))
	  ((= i size))
	(let ((val (abs (+ (sin x) (sin (+ offset (* n x)))))))
	  (if (> val peak)
	      (begin
		(set! peak val)
		(set! location x)))))
      ;; now narrow it by zigzagging around the peak
      (do ((x location)
	   (zig-size (* incr 2) (/ zig-size 2)))
	  ((< zig-size err)
	   (list (abs (+ (sin x) (sin (+ (* n x) offset)))) x))   
	(let ((cur (abs (+ (sin x) (sin (+ offset (* n x))))))
	      (left (abs (+ (sin (- x zig-size)) (sin (+ (* n (- x zig-size)) offset))))))
	  (if (< left cur)
	      (let ((right (abs (+ (sin (+ x zig-size)) (sin (+ (* n (+ x zig-size)) offset))))))
		(if (> right cur)
		    (set! x (+ x zig-size))))
	      (set! x (- x zig-size))))))))

;; (sin-nx-peak 100) (1.999876644816418 1.555089933857112)
;; (sin-nx-peak 1) (5.551115123125783e-16 2.576105496457603)

	

;;; --------------------------------------------------------------------------------
#|
;;; built-in as an experiment
(define (exptmod a b n) ; from the net somewhere: (modulo (expt a b) n)
  (cond ((zero? b) 1)
        ((even? b) (exptmod (modulo (* a a) n) (quotient b 2) n))
        (else (modulo (* a (exptmod (modulo (* a a) n) (quotient b 2) n)) n))))

;; (exptmod 3 100 5) 1
;; (exptmod 3 101 5) 3
;; (exptmod 3 100 3) 0
|#
