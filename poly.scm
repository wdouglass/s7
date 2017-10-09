;;; polynomial-related stuff
;;;
;;; poly+ poly* poly/ poly-gcd poly-reduce poly-roots poly-derivative poly-resultant poly-discriminant

(provide 'snd-poly.scm)

(when (provided? 'pure-s7)
  (define (make-polar mag ang)
    (if (and (real? mag) (real? ang))
	(complex (* mag (cos ang)) (* mag (sin ang)))
	(error 'wrong-type-arg "make-polar args should be real"))))

(define (vector->float-vector v) (copy v (make-float-vector (length v))))
(define (float-vector->vector v) (copy v (make-vector (length v) 0.0)))


;;; using lists and vectors internally for complex intermediates

(define vector-add! 
  (let ((+documentation+ "(vector-add! p1 p2) adds (elementwise) the vectors p1 and p2"))
    (lambda (p1 p2)
      (do ((len (min (length p1) (length p2)))
	   (i 0 (+ i 1)))
	  ((= i len))
	(set! (p1 i) (+ (p1 i) (p2 i))))
      p1)))

(define vector-scale! 
  (let ((+documentation+ "(vector-scale! p1 scl) scales each element of the vector p1 by scl"))
    (lambda (p1 scl)
      (do ((len (length p1))
	   (i 0 (+ i 1)))
	  ((= i len))
	(set! (p1 i) (* scl (p1 i))))
      p1)))

(define poly-as-vector-eval 
  (let ((+documentation+ "(poly-as-vector-eval v x) treats 'v' as a vector of polynomial coefficients, returning the value of the polynomial at x"))
    (lambda (v x)
      (let* ((top (- (length v) 1))
	     (sum (v top)))
	(do ((i (- top 1) (- i 1)))
	    ((< i 0) sum)
	  (set! sum (+ (* sum x) (v i))))))))


(define poly-as-vector-reduce 
  (let ((+documentation+ "(poly-as-vector-reduce p1) removes trailing (high-degree) zeros from the vector p1"))
    (lambda (p1)
      ;; always return at least a 0 coeff (rather than return #f=0 polynomial)
      (let ((new-len (do ((i (- (length p1) 1) (- i 1)))
			 ((or (= i 0)
			      (not (= (p1 i) 0.0)))
			  (+ i 1)))))
	(if (= new-len (length p1))
	    p1
	    (copy p1 (make-vector new-len)))))))

(define poly-reduce 
  (let ((+documentation+ "(poly-reduce p1) removes trailing (high-degree) zeros from the float-vector p1"))
    (lambda (p1)
      (if (= (p1 (- (length p1) 1)) 0.0)
	  (vector->float-vector (poly-as-vector-reduce (float-vector->vector p1)))
	  p1))))

;;; (poly-reduce (float-vector 1 2 3)) -> #<float-vector[len=3]: 1.000 2.000 3.000>
;;; (poly-reduce (float-vector 1 2 3 0 0 0)) -> #<float-vector[len=3]: 1.000 2.000 3.000>
;;; (poly-reduce (float-vector 0 0 0 0 1 0)) -> #<float-vector[len=5]: 0.000 0.000 0.000 0.000 1.000>


(define poly-as-vector+ 
  (let ((+documentation+ "(poly-as-vector+ p1 p2) adds vectors p1 and p2"))
    (lambda (p1 p2)
      (if (vector? p1)
	  (if (vector? p2)
	      (if (> (length p1) (length p2))
		  (vector-add! (copy p1) p2)
		  (vector-add! (copy p2) p1))
	      (let ((v (copy p1)))
		(set! (v 0) (+ (v 0) p2))
		v))
	  (let ((v (copy p2)))
	    (set! (v 0) (+ (v 0) p1))
	    v)))))

(define poly+ 
  (let ((+documentation+ "(poly+ p1 p2)  adds vectors or float-vectors p1 and p2"))
    (lambda (p1 p2) 
      (vector->float-vector 
       (poly-as-vector+ 
	(if (float-vector? p1) (float-vector->vector p1) p1) 
	(if (float-vector? p2) (float-vector->vector p2) p2))))))

;;; (poly+ (float-vector .1 .2 .3) (float-vector 0.0 1.0 2.0 3.0 4.0)) -> #<float-vector[len=5]: 0.100 1.200 2.300 3.000 4.000>
;;; (poly+ (float-vector .1 .2 .3) .5) -> #<float-vector[len=3]: 0.600 0.200 0.300>
;;; (poly+ .5 (float-vector .1 .2 .3)) -> #<float-vector[len=3]: 0.600 0.200 0.300>


(define poly-as-vector* 
  (let ((+documentation+ "(poly-as-vector* p1 p2) multiplies (as polynomials) the vectors p1 and p2"))
    (lambda (p1 p2)
      (if (not (vector? p1))
	  (vector-scale! (copy p2) p1)
	  (if (not (vector? p2))
	      (vector-scale! (copy p1) p2)
	      (let ((p1len (length p1))
		    (p2len (length p2)))
		(do ((m (make-vector (+ p1len p2len) 0))
		     (i 0 (+ i 1)))
		    ((= i p1len) m)
		  (do ((j 0 (+ j 1)))
		      ((= j p2len))
		    (set! (m (+ i j)) (+ (m (+ i j)) (* (p1 i) (p2 j))))))))))))

(define poly* 
  (let ((+documentation+ "(poly* p1 p2) multiplies the polynomials (float-vectors or vectors) p1 and p2"))
    (lambda (p1 p2)
      (vector->float-vector 
       (poly-as-vector* 
	(if (float-vector? p1) (float-vector->vector p1) p1) 
	(if (float-vector? p2) (float-vector->vector p2) p2))))))

;;; (poly* (float-vector 1 1) (float-vector -1 1)) -> #<float-vector[len=4]: -1.000 0.000 1.000 0.000>
;;; (poly* (float-vector -5 1) (float-vector 3 7 2)) -> #<float-vector[len=5]: -15.000 -32.000 -3.000 2.000 0.000>
;;; (poly* (float-vector -30 -4 2) (float-vector 0.5 1)) -> #<float-vector[len=5]: -15.000 -32.000 -3.000 2.000 0.000>
;;; (poly* (float-vector -30 -4 2) 0.5) -> #<float-vector[len=3]: -15.000 -2.000 1.000>
;;; (poly* 2.0 (float-vector -30 -4 2)) -> #<float-vector[len=3]: -60.000 -8.000 4.000>


(define poly-as-vector/ 
  (let ((+documentation+ "(poly-as-vector/ p1 p2) divides the polynomial p1 by p2 (both vectors)"))
    (lambda (p1 p2)
      (if (not (vector? p1))
	  (list (vector 0) p2)
	  (if (not (vector? p2))
	      (list (poly-as-vector* p1 (/ p2)) (vector 0))
	      ;; Numerical Recipes poldiv
	      (let ((p1len (length p1))
		    (p2len (length p2)))
		(if (> p2len p1len)
		    (list (vector 0) p2)
		    (let ((len (max p1len p2len)))
		      (let ((r (make-vector len 0))
			    (q (make-vector len 0)))
			(do ((i 0 (+ i 1)))
			    ((= i len))
			  (set! (r i) (p1 i)))
			(let ((n (- p1len 1))
			      (nv (- p2len 1)))
			  (do ((k (- n nv) (- k 1)))
			      ((< k 0))
			    (set! (q k) (/ (r (+ nv k)) (p2 nv)))
			    (do ((j (+ nv k -1) (- j 1)))
				((< j k))
			      (set! (r j) (- (r j) (* (q k) (p2 (- j k)))))))
			  (do ((j nv (+ j 1)))
			      ((> j n))
			    (set! (r j) 0))
			  (list q r)))))))))))

(define poly/ 
  (let ((+documentation+ "(poly/ p1 p2) divides p1 by p2, both polynomials either float-vectors or vectors"))
    (lambda (p1 p2)
      (map vector->float-vector (poly-as-vector/ (if (float-vector? p1) (float-vector->vector p1) p1) 
						 (if (float-vector? p2) (float-vector->vector p2) p2))))))

;;; (poly/ (float-vector -1.0 -0.0 1.0) (vector 1.0 1.0)) -> (#<float-vector[len=3]: -1.000 1.000 0.000> #<float-vector[len=3]: 0.000 0.000 0.000>)
;;; (poly/ (float-vector -15 -32 -3 2) (vector -5 1)) -> (#<float-vector[len=4]: 3.000 7.000 2.000 0.000> #<float-vector[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (float-vector -15 -32 -3 2) (vector 3 1)) -> (#<float-vector[len=4]: -5.000 -9.000 2.000 0.000> #<float-vector[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (float-vector -15 -32 -3 2) (vector .5 1)) -> (#<float-vector[len=4]: -30.000 -4.000 2.000 0.000> #<float-vector[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (float-vector -15 -32 -3 2) (vector 3 7 2)) -> (#<float-vector[len=4]: -5.000 1.000 0.000 0.000> #<float-vector[len=4]: 0.000 0.000 0.000 0.000>)
;;; (poly/ (float-vector -15 -32 -3 2) 2.0) -> (#<float-vector[len=4]: -7.500 -16.000 -1.500 1.000> #<float-vector[len=1]: 0.0>)


(define poly-as-vector-derivative 
  (let ((+documentation+ "(poly-as-vector-derivative p1) returns the derivative or polynomial p1 (as a vector)"))
    (lambda (p1)
      (let ((len (- (length p1) 1)))
	(do ((v (make-vector len))
	     (i (- len 1) (- i 1))
	     (j len (- j 1)))
	    ((< i 0) v)
	  (set! (v i) (* j (p1 j))))))))

(define poly-derivative 
  (let ((+documentation+ "(poly-derivative p1) returns the derivative of p1, either a float-vector or vector"))
    (lambda (p1) 
      (vector->float-vector 
       (poly-as-vector-derivative 
	(float-vector->vector p1))))))

;;; (poly-derivative (float-vector 0.5 1.0 2.0 4.0)) -> #<float-vector[len=3]: 1.000 4.000 12.000>

;;; poly-antiderivative with random number as constant? or max of all coeffs? -- 0.0 seems like a bad idea -- maybe an optional arg 
;;; then poly-integrate: (let ((integral (poly-antiderivative p1))) (- (poly-as-vector-eval integral end) (poly-as-vector-eval integral beg)))


(define (submatrix mx row col)
  (let ((old-n (car (vector-dimensions mx))))
    (do ((nmx (let ((new-n (- old-n 1)))
		(make-float-vector (list new-n new-n))))
	 (i 0 (+ i 1))
	 (ni 0))
	((= i old-n) nmx)
      (unless (= i row)
	(do ((j 0 (+ j 1))
	     (nj 0))
	    ((= j old-n))
	  (if (not (= j col))
	      (begin
		(set! (nmx ni nj) (mx i j))
		(set! nj (+ nj 1)))))
	(set! ni (+ 1 ni))))))

(define (determinant mx)
  (if (not (float-vector? mx))
      (error 'wrong-type-arg "determinant argument should be a float-vector")
      (let ((n (car (vector-dimensions mx))))
	(case n
	  ((1) (mx 0 0))
	  ((2) (- (* (mx 0 0) (mx 1 1)) (* (mx 0 1) (mx 1 0))))
	  ((3) (- (+ (* (mx 0 0) (mx 1 1) (mx 2 2))
		     (* (mx 0 1) (mx 1 2) (mx 2 0))
		     (* (mx 0 2) (mx 1 0) (mx 2 1)))
		  (* (mx 0 0) (mx 1 2) (mx 2 1))
		  (* (mx 0 1) (mx 1 0) (mx 2 2))
		  (* (mx 0 2) (mx 1 1) (mx 2 0))))
	  (else   
	   (do ((sum 0.0)
		(sign 1)
		(i 0 (+ i 1)))
	       ((= i n) sum)
	     (let ((mult (mx 0 i)))
	       (if (not (= mult 0.0))
		   (set! sum (+ sum (* sign mult (determinant (submatrix mx 0 i))))))
	       (set! sign (- sign)))))))))

(define (poly-as-vector-resultant p1 p2)
  (if (not (and (vector? p1)
		(vector? p2)))
      (error 'wrong-type-arg "poly-as-vector-resultant arguments should be vectors")
      (let* ((m (length p1))
	     (n (length p2))
	     (mat (let ((d (+ n m -2)))
		    (make-float-vector (list d d)))))
	;; load matrix with n-1 rows of m's coeffs then m-1 rows of n's coeffs (reversed in sense), return determinant
	(do ((i 0 (+ i 1)))
	    ((= i (- n 1)))
	  (do ((j 0 (+ j 1)))
	      ((= j m))
	    (set! (mat i (+ i j)) (p1 (- m j 1)))))
	(do ((i 0 (+ i 1)))
	    ((= i (- m 1)))
	  (do ((j 0 (+ j 1)))
	      ((= j n))
	    (set! (mat (+ i n -1) (+ i j)) (p2 (- n j 1)))))
	(determinant mat))))

(define poly-resultant 
  (let ((+documentation+ "(poly-resultant p1 p2) returns the resultant of polynomials p1 and p2 (float-vectors or vectors)"))
    (lambda (p1 p2) 
      (poly-as-vector-resultant 
       (if (float-vector? p1) (float-vector->vector p1) p1)
       (if (float-vector? p2) (float-vector->vector p2) p2)))))


(define poly-as-vector-discriminant 
  (let ((+documentation+ "(poly-as-vector-discriminant p1) returns the discriminant of polynomial p1 (a vector)"))
    (lambda (p1)
      (poly-as-vector-resultant p1 (poly-as-vector-derivative p1)))))

(define poly-discriminant 
  (let ((+documentation+ "(poly-discriminant p1) returns the discriminant of polynomial p1 (either a float-vector or a vector)"))
    (lambda (p1)
      (poly-as-vector-discriminant 
       (if (float-vector? p1) (float-vector->vector p1) p1)))))


;;; (poly-as-vector-resultant (vector -1 0 1) (vector 1 -2 1))  0.0 (x=1 is the intersection)
;;; (poly-as-vector-resultant (vector -1 0 2) (vector 1 -2 1))   1.0
;;; (poly-as-vector-resultant (vector -1 0 1) (vector 1 1))      0.0 (x=-1 is the intersection)
;;; (poly-as-vector-resultant (vector -1 0 1) (vector 2 1))      3.0

;;; (poly-as-vector-discriminant (vector -1 0 1)) -4.0
;;; (poly-as-vector-discriminant (vector 1 -2 1)) 0.0

;;; (poly-discriminant (poly-reduce (poly* (poly* (float-vector -1 1) (float-vector -1 1)) (float-vector 3 1)))) 0.0
;;; (poly-discriminant (poly-reduce (poly* (poly* (poly* (float-vector -1 1) (float-vector -1 1)) (float-vector 3 1)) (float-vector 2 1)))) 0.0
;;; (poly-discriminant (poly-reduce (poly* (poly* (poly* (float-vector 1 1) (float-vector -1 1)) (float-vector 3 1)) (float-vector 2 1)))) 2304
;;; (poly-discriminant (poly-reduce (poly* (poly* (poly* (float-vector 1 1) (float-vector -1 1)) (float-vector 3 1)) (float-vector 3 1)))) 0.0



(define poly-roots-epsilon 1.0e-7)

(define simplify-complex 
  (let ((+documentation+ "(simplify-complex a) sets to 0.0 real or imaginary parts of 'a' that are less than poly-roots-epsilon"))
    (lambda (a)
      (if (< (abs (imag-part a)) poly-roots-epsilon)
	  (if (< (abs (real-part a)) poly-roots-epsilon)
	      0.0
	      (real-part a))
	  (if (< (abs (real-part a)) poly-roots-epsilon)
	      (complex 0.0 (imag-part a))
	      a)))))


(define poly-gcd 
  (let ((+documentation+ "(poly-gcd p1 p2) returns the GCD of polynomials p1 and p2 (both float-vectors)"))
    (lambda (p1 p2)
      (if (< (length p1) (length p2))
	  (float-vector 0.0)
	  (let ((qr (map poly-reduce (poly/ p1 p2))))
	    (if (= (length (cadr qr)) 1)
		(if (= (float-vector-ref (cadr qr) 0) 0.0)
		    p2
		    (float-vector 0.0))
		(apply poly-gcd qr)))))))

(define poly-as-vector-gcd 
  (let ((+documentation+ "(poly-as-vector-gcd p1 p2) returns the GCD of polynomials p1 and p2 (both vectors)"))
    (lambda (p1 p2)
      (if (< (length p1) (length p2))
	  (vector 0)
	  (let ((qr (map poly-as-vector-reduce (poly-as-vector/ p1 p2))))
	    (if (= (length (cadr qr)) 1)
		(if (= ((cadr qr) 0) 0.0)
		    p2
		    (vector 0))
		(apply poly-as-vector-gcd qr)))))))

;;; (poly-gcd (poly-reduce (poly* (float-vector 2 1) (float-vector -3 1))) (float-vector 2 1)) -> #<float-vector[len=2]: 2.000 1.000>
;;; (poly-gcd (poly-reduce (poly* (float-vector 2 1) (float-vector -3 1))) (float-vector 3 1)) -> #<float-vector[len=1]: 6.000>
;;; (poly-gcd (poly-reduce (poly* (float-vector 2 1) (float-vector -3 1))) (float-vector -3 1)) -> #<float-vector[len=2]: -3.000 1.000>
;;; (poly-gcd (poly-reduce (poly* (float-vector 8 1) (poly* (float-vector 2 1) (float-vector -3 1)))) (float-vector -3 1)) -> #<float-vector[len=2]: -3.000 1.000>
;;; (poly-gcd (poly-reduce (poly* (float-vector 8 1) (poly* (float-vector 2 1) (float-vector -3 1)))) (poly-reduce (poly* (float-vector 8 1) (float-vector -3 1)))) -> #<float-vector[len=3]: -24.000 5.000 1.000>
;;; (poly-gcd (float-vector -1 0 1) (float-vector 2 -2 -1 1)) -> #<float-vector[len=1]: 0.000>
;;; (poly-gcd (float-vector 2 -2 -1 1) (float-vector -1 0 1)) -> #<float-vector[len=2]: 1.000 -1.000>
;;; (poly-gcd (float-vector 2 -2 -1 1) (float-vector -2.5 1)) -> #<float-vector[len=1]: 0.000>


(define poly-as-vector-roots
  (let ((linear-root          ; ax + b
	 (lambda (a b) 
	   (list (/ (- b) a))))
	
	(quadratic-roots      ; ax^2 + bx + c
	 (lambda (a b c) 
	   (let ((d (sqrt (- (* b b) (* 4 a c)))))
	     (list (/ (- d b) (* 2 a))
		   (/ (- (+ d b)) (* 2 a))))))
	
	(cubic-roots          ; ax^3 + bx^2 + cx + d
	 (lambda (a b c d) 
	   ;; Abramowitz & Stegun 3.8.2
	   (let ((a0 (/ d a))
		 (a1 (/ c a))
		 (a2 (/ b a)))
	     (let* ((r (- (/ (- (* a1 a2) (* 3 a0)) 6) (/ (* a2 a2 a2) 27)))
		    (sq3r2 (let ((q (- (/ a1 3) (/ (* a2 a2) 9))))
			     (sqrt (+ (* q q q) (* r r))))))
	       (let ((r1 (expt (+ r sq3r2) 1/3))
		     (r2 (expt (- r sq3r2) 1/3))
		     (incr (/ (* 2 pi 0+i) 3)))
		 (call-with-exit
		  (lambda (return)
		    (do ((i 0 (+ i 1)))   ; brute force! this can almost certainly be optimized
			((= i 3))
		      (do ((j 0 (+ j 1)))
			  ((= j 3))
			(let* ((s1 (* r1 (exp (* i incr))))
			       (s2 (* r2 (exp (* j incr))))
			       (z1 (simplify-complex (- (+ s1 s2) (/ a2 3)))))
			  (if (< (magnitude (poly-as-vector-eval (vector a0 a1 a2 1) z1)) poly-roots-epsilon)
			      (let ((z2 (simplify-complex (+ (* -0.5 (+ s1 s2))
							     (/ a2 -3) 
							     (* (- s1 s2) 0.5 (sqrt -3))))))
				(if (< (magnitude (poly-as-vector-eval (vector a0 a1 a2 1) z2)) poly-roots-epsilon)
				    (let ((z3 (simplify-complex (+ (* -0.5 (+ s1 s2)) 
								   (/ a2 -3) 
								   (* (- s1 s2) -0.5 (sqrt -3))))))
				      (if (< (magnitude (poly-as-vector-eval (vector a0 a1 a2 1) z3)) poly-roots-epsilon)
					  (return (list z1 z2 z3))))))))))
		    #f)))))))
	
	(quartic-roots          ; ax^4 + bx^3 + cx^2 + dx + e
	 (lambda (a b c d e)    ; Weisstein, "Encyclopedia of Mathematics"
	   (call-with-exit
	    (lambda (return)
	      (let ((a0 (/ e a))
		    (a1 (/ d a))
		    (a2 (/ c a))
		    (a3 (/ b a)))
		(let ((yroot (poly-as-vector-roots (vector (- (* 4 a2 a0) (* a1 a1) (* a3 a3 a0))
							   (- (* a1 a3) (* 4 a0))
							   (- a2)
							   1.0))))
		  (when (and (pair? yroot)
			     (= (length yroot) 4))
		    (do ((i 0 (+ i 1)))
			((= i 3))
		      (let* ((y1 (yroot i))
			     (R (sqrt (- (+ (* 0.25 a3 a3) y1) a2))))
			(let ((D (sqrt (if (= R 0)
					   (+ (* 0.75 a3 a3) (* -2 a2) (* 2 (sqrt (- (* y1 y1) (* 4 a0)))))
					   (- (+ (* 0.75 a3 a3) (* -2 a2) (/ (* 0.25 (- (+ (* 4 a3 a2) (* -8 a1)) (* a3 a3 a3))) R)) (* R R)))))
			      (E (sqrt (if (= R 0)
					   (+ (* 0.75 a3 a3) (* -2 a2) (* -2 (sqrt (- (* y1 y1) (* 4 a0)))))
					   (- (+ (* 0.75 a3 a3) (* -2 a2) (/ (* -0.25 (- (+ (* 4 a3 a2) (* -8 a1)) (* a3 a3 a3))) R)) (* R R))))))
			  (let ((z1 (+ (* -0.25 a3) (* 0.5 R) (* 0.5 D)))
				(z2 (+ (* -0.25 a3) (* 0.5 R) (* -0.5 D)))
				(z3 (+ (* -0.25 a3) (* -0.5 R) (* 0.5 E)))
				(z4 (+ (* -0.25 a3) (* -0.5 R) (* -0.5 E))))
			    
			    (if (< (magnitude (poly-as-vector-eval (vector e d c b a) z1)) poly-roots-epsilon)
				(return (list z1 z2 z3 z4))))))))
		  #f))))))
	
	(nth-roots           ; ax^n + b
	 (lambda (a b deg) 
	   (do ((n (expt (/ (- b) a) (/ 1.0 deg)))
		(incr (/ (* 2 pi 0+i) deg))
		(roots ())
		(i 0 (+ i 1)))
	       ((= i deg) roots)
	     (set! roots (cons (simplify-complex (* n (exp (* i incr)))) roots))))))
    
    ;; poly-as-vector-roots
    (lambda (p1)
      (let ((deg (- (length p1) 1)))
	
	(cond ((= deg 0)                          ; just constant
	       ())
	      
	      ((not (= (p1 0) 0.0))               ; constant=0.0, divide through by x, recurse on new
	       (case deg
		 ((1)                             ; ax + b -> -b/a
		  (linear-root (p1 1) (p1 0)))
		 
		 ((2)                             ; ax^2 + bx + c -> -b +/- sqrt(b^2 - 4ac) / 2a
		  (quadratic-roots (p1 2) (p1 1) (p1 0)))
		 
		 (else
		  (or (and (= deg 3)
			   ;; it may be better to fall into Newton's method here
			   (cubic-roots (p1 3) (p1 2) (p1 1) (p1 0)))
		      
		      (and (= deg 4)
			   (quartic-roots (p1 4) (p1 3) (p1 2) (p1 1) (p1 0)))
		      
		      ;; degree>4 (or trouble above), use Newton's method unless some simple case pops up
		      (let ((ones 0))
			(do ((i 1 (+ i 1)))
			    ((> i deg))
			  (if (not (= (p1 i) 0.0))
			      (set! ones (+ 1 ones))))
			
			(cond ((= ones 1)                  ; x^n + b -- "linear" in x^n
			       (nth-roots (p1 deg) (p1 0) deg))
			      
			      ((and (= ones 2)
				    (even? deg)
				    (not (= (p1 (/ deg 2)) 0.0)))
			       (let ((roots ())       ; quadratic in x^(n/2)
				     (n (/ deg 2)))
				 (for-each
				  (lambda (r)
				    (set! roots (append roots (nth-roots 1.0 (- r) n))))
				  (poly-as-vector-roots (vector (p1 0) 
								(p1 (/ deg 2)) 
								(p1 deg))))
				 roots))
			      
			      ((and (> deg 3)
				    (= ones 3)
				    (= (modulo deg 3) 0)
				    (not (= (p1 (/ deg 3)) 0.0))
				    (not (= (p1 (/ (* 2 deg) 3)) 0.0)))
			       (let ((roots ())   ; cubic in x^(n/3)
				     (n (/ deg 3)))
				 (for-each
				  (lambda (r)
				    (set! roots (append roots (nth-roots 1.0 (- r) n))))
				  (poly-as-vector-roots (vector (p1 0) 
								(p1 (/ deg 3)) 
								(p1 (/ (* 2 deg) 3))
								(p1 deg))))
				 roots))
			      
			      (else 
			       ;; perhaps get derivative roots, plug in main -- need to get nth derivative to be safe in this
			       ;; from Cohen, "Computational Algebraic Number Theory"
			       (let ((roots ())
				     (q (copy p1))
				     (n deg)
				     (x 1.3+0.314159i))
				 (let ((pp (poly-as-vector-derivative p1)))
				   (let ((happy #f)
					 (qp (copy pp))
					 (dx 0.0)
					 (v (poly-as-vector-eval q x))
					 (last-dx 1.0)) ; guard against infinite loop
				     (do ((m (* (magnitude v) (magnitude v))))
					 (happy)
				       (set! dx (/ v (poly-as-vector-eval qp x)))
				       (if (or (<= (magnitude dx) poly-roots-epsilon)
					       (= dx last-dx))
					   (set! happy #t)
					   (begin
					     (set! last-dx dx)
					     (do ((c 0 (+ 1 c))
						  (step3 #f))
						 ((or (>= c 20)
						      step3
						      (<= (magnitude dx) poly-roots-epsilon)))
					       (let* ((y (- x dx))
						      (v1 (poly-as-vector-eval q y))
						      (m1 (* (magnitude v1) (magnitude v1))))
						 (if (< m1 m)
						     (begin
						       (set! x y)
						       (set! v v1)
						       (set! m m1)
						       (set! step3 #t))
						     (set! dx (/ dx 4.0)))))))))
				   (set! x (- x (/ (poly-as-vector-eval p1 x) (poly-as-vector-eval pp x))))
				   (set! x (- x (/ (poly-as-vector-eval p1 x) (poly-as-vector-eval pp x)))))
				 (if (< (imag-part x) poly-roots-epsilon)
				     (begin
				       (set! x (real-part x))
				       (set! q (poly-as-vector/ q (vector (- x) 1.0)))
				       (set! n (- n 1)))
				     (begin
				       (set! q (poly-as-vector/ q (vector (magnitude x) 0.0 1.0)))
				       (set! n (- n 2))))
				 (set! roots (cons x roots))
				 (if (> n 0) 
				     (set! roots (append (poly-as-vector-roots (poly-as-vector-reduce (car q))) roots)))
				 roots))))))))
	      
	      ((= deg 1)
	       (list 0.0))
	      
	      (else 
	       (do ((pnew (make-vector deg))
		    (i 1 (+ i 1)))
		   ((> i deg)
		    (cons 0.0 (poly-as-vector-roots pnew)))
		 (set! (pnew (- i 1)) (p1 i)))))))))

  
(define poly-roots 
  (let ((+documentation+ "(poly-roots p1) returns the roots of polynomial p1"))
    (lambda (p1) 
      (let* ((v1 (float-vector->vector (poly-reduce p1)))
	     (roots (poly-as-vector-roots v1)))
	(for-each
	 (lambda (q)
	   (let ((dx (magnitude (poly-as-vector-eval v1 q))))
	     (if (> dx poly-roots-epsilon) 
		 (format () ";poly.scm 502: (poly-roots ~A) numerical trouble (polynomial root is not very good): ~A at ~A: ~A" p1 v1 q dx))))
	 roots)
	roots))))

#|
(do ((i 0 (+ i 1))) ((= i 10)) 
  (poly-as-vector-roots (vector (complex (mus-random 1.0) (mus-random 1.0)) 
				(complex (mus-random 1.0) (mus-random 1.0)))))
(do ((i 0 (+ i 1))) ((= i 10)) 
  (poly-as-vector-roots (vector (complex (mus-random 1.0) (mus-random 1.0)) 
				(complex (mus-random 1.0) (mus-random 1.0))
				(complex (mus-random 1.0) (mus-random 1.0)))))

(do ((i 0 (+ i 1))) ((= i 10)) 
  (poly-roots (float-vector (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0))))

(do ((i 0 (+ i 1))) ((= i 10)) 
  (poly-as-vector-roots (vector (complex (mus-random 1.0) (mus-random 1.0)) 
				(complex (mus-random 1.0) (mus-random 1.0))
				(complex (mus-random 1.0) (mus-random 1.0))
				(complex (mus-random 1.0) (mus-random 1.0)))))

(do ((i 0 (+ i 1))) ((= i 10)) 
  (poly-roots (float-vector (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0))))

(do ((i 0 (+ i 1))) ((= i 10)) 
  (poly-as-vector-roots (vector (complex (mus-random 1.0) (mus-random 1.0)) 
				(complex (mus-random 1.0) (mus-random 1.0))
				(complex (mus-random 1.0) (mus-random 1.0))
				(complex (mus-random 1.0) (mus-random 1.0))
				(complex (mus-random 1.0) (mus-random 1.0)))))

(do ((i 3 (+ i 1))) ((= i 20)) 
  (let ((v (make-float-vector i 0.0)))
    (set! (v 0) (mus-random 1.0))
    (set! (v (- i 1)) 1.0)
    (poly-roots v)))

(do ((i 3 (+ i 2))) ((= i 21)) 
  (let ((v (make-float-vector i 0.0)))
    (set! (v 0) (mus-random 1.0))
    (set! (v (- i 1)) 1.0)
    (set! (v (/ (- i 1) 2)) 1.0)
    (poly-roots v)))

;;; these can be off by a lot!
(do ((i 0 (+ i 1))) ((= i 10)) 
  (poly-roots (float-vector (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0) (mus-random 1.0))))

(poly-roots (poly* (poly* (poly* (float-vector -1 1) (float-vector 1 1)) (poly* (float-vector -2 1) (float-vector 2 1))) (poly* (float-vector -3 1) (float-vector 3 1)))) -> (-3.0 3.0 -1.0 1.0 -2.0 2.0)

;;; numerical trouble: 
(poly-roots (float-vector 1000 .01 0 1))

;; failed to find a root within poly-roots-epsilon -- get the best available
(let ((s1 backup-s1)
      (s2 backup-s2))
  (list (simplify-complex (- (+ s1 s2) (/ a2 3.0)))
	(simplify-complex (+ (* -0.5 (+ s1 s2))
			     (/ a2 -3.0) 
			     (* (- s1 s2) 0.5 (sqrt -3.0))))
	(simplify-complex (+ (* -0.5 (+ s1 s2)) 
			     (/ a2 -3.0) 
			     (* (- s1 s2) -0.5 (sqrt -3.0))))))))))
|#

