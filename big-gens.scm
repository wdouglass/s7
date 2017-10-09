(provide 'snd-big-gens.scm)

(require snd-generators.scm)


;;; -------- conversions --------

(define (big-hz->radians hz)
  (/ (* hz 2 pi) *clm-srate*))

(define (big-radians->hz rad)
  (/ (* rad *clm-srate*) 2 pi))

(define (big-db->linear x)
  (expt 10.0 (/ x 20.0)))

(define (big-linear->db x)
  (if (> x 0.0) (* 20.0 (log x 10)) -100.0))

(define (big-degrees->radians deg)
  (* (/ pi 180) deg))

(define (big-radians->degrees rad)
  (/ (* rad 180) pi))

(define (big-seconds->samples secs)
  (round (* secs *clm-srate*)))

(define (big-samples->seconds samps)
  (/ samps *clm-srate*))

(define (big-rectangular->polar rl im)
  (do ((len (length rl))
       (i 0 (+ i 1)))
      ((= i len))
    (let ((rl1 (rl i))
	  (im1 (im i)))
      (set! (rl i) (sqrt (+ (* rl1 rl1) (* im1 im1))))
      (set! (im i) (- (atan im1 rl1))))))

(define (big-polar->rectangular mag ang)
  (do ((len (length mag))
       (i 0 (+ i 1)))
      ((= i len))
    (let ((mag1 (mag i))
	  (ang1 (- (ang i))))
      (set! (mag i) (* mag1 (cos ang1)))
      (set! (ang i) (* mag1 (sin ang1))))))


;;; -------- arrays (vectors in this context) --------

(define (big-array-clear v)
  (fill! v 0.0))

(define (big-array-normalize v)
  (let ((len (length v))
	(pk 0.0))
    (do ((i 0 (+ i 1)))
	((= i len))
      (set! pk (max pk (abs (v i)))))
    (if (not (member pk '(0.0 1.0) =))
	(do ((i 0 (+ i 1)))
	    ((= i len))
	  (set! (v i) (/ (v i) pk))))
    v))

(define (big-array-interp wave x n)
  (let* ((xx (modulo x n))
	 (ipart (floor xx))
	 (fpart (- xx ipart)))
    (if (zero? fpart)
	(wave ipart)
	(+ (wave ipart)
	   (* fpart (- (wave (modulo (+ ipart 1) n)) 
		       (wave ipart)))))))



;;; -------- polynomial --------

(define (big-polynomial coeffs x)
  (let* ((top (- (length coeffs) 1))
	 (sum (coeffs top)))
    (do ((i (- top 1) (- i 1)))
	((< i 0) sum)
      (set! sum (+ (* x sum)
		   (coeffs i))))))


;;; -------- dot-product --------

(define (big-dot-product v1 v2)
  (do ((len (min (length v1) (length v2)))
       (sum 0.0)
       (i 0 (+ i 1)))
      ((= i len) sum)
    (set! sum (+ sum (* (v1 i) (v2 i))))))


;;; -------- ring-modulate --------

(define big-ring-modulate *)


;;; -------- amplitude-modulate --------

(define (big-amplitude-modulate carrier in1 in2)
  (* in1 (+ carrier in2)))


;;; -------- contrast-enhancement --------

(define* (big-contrast-enhancement in1 (index 1.0))
  (sin (+ (* in1 (/ pi 2))
	  (* index (sin (* 2 pi in1))))))


;;; -------- oscil --------

(defgenerator (big-oscil 
  :make-wrapper 
    (lambda (g) (set! (g 'frequency) (big-hz->radians (g 'frequency))) g))
  (frequency 0.0) 
  (angle 0.0))

(define* (big-oscil gen (fm 0.0) (pm 0.0))
  (let ((x (gen 'angle)))
    (set! (gen 'angle) (+ fm x (gen 'frequency)))
    (sin (+ x pm))))

#|
(with-sound (:statistics #t :clipped #f) 
  (let ((g (make-big-oscil 440.0)))
    (do ((i 0 (+ i 1))) 
	((= i 22050))
      (outa i (* .5 (big-oscil g))))))
|#



;;; -------- ncos --------

(defgenerator (big-ncos
  :make-wrapper
   (lambda (g)
     (if (<= (g 'n) 0)
	 (set! (g 'n) 1))
     (set! (g 'r) (/ (g 'n)))
     (set! (g 'frequency) (big-hz->radians (g 'frequency)))
     g))
   (frequency 0.0) 
   (n 1) 
   (angle 0.0)
   (r 1.0))

(define* (big-ncos gen (fm 0.0))
  (let* ((n (gen 'n))
	 (x (gen 'angle))
	 (scl (gen 'r))
	 (den (* 2 (sin (/ x 2)))))
    (set! (gen 'angle) (+ fm x (gen 'frequency)))
    (if (= den 0.0)
	1.0
	(min 1.0 (* scl (- (/ (sin (* (+ n 1/2) x)) den) 1/2))))))

#|
(with-sound (:statistics #t :clipped #f) 
  (let ((g (make-big-ncos 100.0 10)))
    (do ((i 0 (+ i 1))) 
	((= i 22050))
      (outa i (* .5 (big-ncos g))))))
|#



;;; -------- nsin --------

(defgenerator (big-nsin
	       :make-wrapper (letrec ((ns (lambda (x n)
					    (let* ((a2 (/ x 2))
						   (den (sin a2)))
					      (if (= den 0.0)
						  0.0
						  (/ (* (sin (* n a2))
							(sin (* (+ n 1) a2)))
						     den)))))
				      (find-scaler (lambda (n lo hi)
						     (let ((mid (/ (+ lo hi) 2))
							   (ylo (ns lo n))
							   (yhi (ns hi n)))
						       (if (< (abs (- yhi ylo)) 1e-12)
							   (ns mid n)
							   (find-scaler n (if (> ylo yhi)
									      (values lo mid)
									      (values mid hi))))))))
			       (lambda (g)
				 (if (<= (g 'n) 0)
				     (set! (g 'n) 1))
				 (set! (g 'r) (/ 1.0 (find-scaler (g 'n) 0.0 (/ pi (+ (g 'n) 1/2)))))
				 (set! (g 'frequency) (big-hz->radians (g 'frequency)))
				 g)))
  (frequency 0.0) 
  (n 1) 
  (angle 0.0) 
  (r 1.0))

(define* (big-nsin gen (fm 0.0))
  (let* ((n (gen 'n))
	 (x (gen 'angle))
	 (a2 (/ x 2))
	 (scl (gen 'r))
	 (den (sin a2)))
    (set! (gen 'angle) (+ fm x (gen 'frequency)))
    (if (= den 0.0)
	0.0
	(/ (* scl (sin (* n a2)) (sin (* (+ n 1) a2))) den))))

#|
(with-sound (:statistics #t :clipped #f) 
  (let ((g (make-big-nsin 100.0 10)))
    (do ((i 0 (+ i 1))) 
	((= i 22050))
      (outa i (* .5 (big-nsin g))))))
|#


;;; -------- table-lookup --------

(defgenerator (big-table-lookup
  :make-wrapper
    (lambda (g)
      (if (g 'wave)
	  (set! (g 'size) (length (g 'wave)))
	  (set! (g 'wave) (make-vector (g 'size) 0.0)))
      (set! (g 'frequency) (/ (* (g 'frequency) (g 'size)) *clm-srate*))
      (set! (g 'angle) (/ (* (g 'angle) (g 'size)) (* 2 pi)))
      g))
  (frequency 0.0) 
  (angle 0.0) 
  (wave #f) 
  (size *clm-table-size*))

(define* (big-table-lookup gen (fm 0.0))
  (let ((x (gen 'angle))
	(w (gen 'wave))
	(n (gen 'size)))
    (set! (gen 'angle) (+ x (gen 'frequency) (/ (* fm n) 2 pi)))
    (big-array-interp w x n)))
      
#|
(with-sound (:statistics #t :clipped #f) 
  (let ((g (make-big-table-lookup 100.0 0.0 (let ((w (make-vector 32)))
					      (do ((i 0 (+ i 1)))
						  ((= i 32) w)
						(set! (w i) (sin (/ (* i pi) 16))))))))
    (do ((i 0 (+ i 1))) 
	((= i 22050))
      (outa i (* .5 (big-table-lookup g))))))
|#



;;; -------- one-zero --------

(defgenerator big-one-zero (a0 1.0) (a1 0.0) (x1 0.0))

(define* (big-one-zero gen x)
  (let ((val (+ (* x (gen 'a0))
		(* (gen 'x1) (gen 'a1)))))
    (set! (gen 'x1) x)
    val))

		  
;;; -------- one-pole --------

(defgenerator big-one-pole (a0 1.0) (b1 0.0) (y1 0.0))

(define* (big-one-pole gen x)
  (let ((val (- (* x (gen 'a0))
		(* (gen 'y1) (gen 'b1)))))
    (set! (gen 'y1) val)))


