;;; vector timing tests

(set! (*s7* 'heap-size) (* 2 1024000))

(define size 300000)
(define size/10 (/ size 10))


(define (f1)
  (let ((v (make-vector size 1))
	(sum 0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (floor (vector-ref v i)))))))

(unless (= (f1) size)
  (format *stderr* "f1: ~S~%" (f1)))


(define (f2)
  (let ((v (make-int-vector size 1))
	(sum 0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (int-vector-ref v i))))))

(unless (= (f2) size)
  (format *stderr* "f2: ~S~%" (f2)))


(define (f3)
  (let ((v (make-byte-vector size 1))
	(sum 0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (byte-vector-ref v i))))))

(unless (= (f3) size)
  (format *stderr* "f3: ~S~%" (f3)))


(define (f4)
  (let ((v (make-float-vector size 1.0))
	(sum 0.0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (float-vector-ref v i))))))

(unless (= (f4) size)
  (format *stderr* "f4: ~S~%" (f4)))


(define (f5)
  (let ((v (make-vector size 1 integer?))
	(sum 0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (vector-ref v i))))))

(unless (= (f5) size)
  (format *stderr* "f5: ~S~%" (f5)))


(define (f6)
  (let ((v (make-vector size #t boolean?))
	(sum 0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (if (vector-ref v i) 1 0))))))

(unless (= (f6) size)
  (format *stderr* "f6: ~S~%" (f6)))


;;; --------------------------------
(define (f11)
  (let ((v (make-vector (list 10 size/10) 1))
	(sum 0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (round (vector-ref v k i))))))))

(unless (= (f11) size)
  (format *stderr* "f11: ~S~%" (f11)))


(define (f12)
  (let ((v (make-int-vector (list 10 size/10) 1))
	(sum 0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (int-vector-ref v k i)))))))

(unless (= (f12) size)
  (format *stderr* "f12: ~S~%" (f12)))


(define (f13)
  (let ((v (make-byte-vector (list 10 size/10) 1))
	(sum 0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (byte-vector-ref v k i)))))))

(unless (= (f13) size)
  (format *stderr* "f13: ~S~%" (f13)))


(define (f14)
  (let ((v (make-float-vector (list 10 size/10) 1.0))
	(sum 0.0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (float-vector-ref v k i)))))))

(unless (= (f14) size)
  (format *stderr* "f14: ~S~%" (f14)))


(define (f15)
  (let ((v (make-vector (list 10 size/10) 1 integer?))
	(sum 0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (vector-ref v k i)))))))

(unless (= (f15) size)
  (format *stderr* "f15: ~S~%" (f15)))


(define (f16)
  (let ((v (make-vector (list 10 size/10) 'a symbol?))
	(sum 0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (if (eq? 'a (vector-ref v k i)) 1 0)))))))

(unless (= (f16) size)
  (format *stderr* "f16: ~S~%" (f15)))



;;; --------------------------------------------------------------------------------

(define (g1)
  (let ((v (make-vector size 1))
	(sum 0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (ceiling (v i)))))))

(unless (= (g1) size)
  (format *stderr* "g1: ~S~%" (g1)))


(define (g2)
  (let ((v (make-int-vector size 1))
	(sum 0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (v i))))))

(unless (= (g2) size)
  (format *stderr* "g2: ~S~%" (g2)))


(define (g3)
  (let ((v (make-byte-vector size 1))
	(sum 0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (v i))))))

(unless (= (g3) size)
  (format *stderr* "g3: ~S~%" (g3)))


(define (g4)
  (let ((v (make-float-vector size 1.0))
	(sum 0.0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (v i))))))

(unless (= (g4) size)
  (format *stderr* "g4: ~S~%" (g4)))


(define (g5)
  (let ((v (make-vector size 1 integer?))
	(sum 0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (v i))))))

(unless (= (g5) size)
  (format *stderr* "g5: ~S~%" (g5)))


(define (g6)
  (let ((v (make-vector size 0+i complex?))
	(sum 0))
    (do ((i 0 (+ i 1)))
	((= i size) sum)
      (set! sum (+ sum (if (zero? (real-part (vector-ref v i))) 1 0)))))) ; faster is (imag-part...)

(unless (= (g6) size)
  (format *stderr* "g6: ~S~%" (g6)))


;;; --------------------------------

(define (g11)
  (let ((v (make-vector (list 10 size/10) 1))
	(sum 0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (floor (v k i))))))))

(unless (= (g11) size)
  (format *stderr* "g11: ~S~%" (g11)))


(define (g12)
  (let ((v (make-int-vector (list 10 size/10) 1))
	(sum 0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (v k i)))))))

(unless (= (g12) size)
  (format *stderr* "g12: ~S~%" (g12)))


(define (g13)
  (let ((v (make-byte-vector (list 10 size/10) 1))
	(sum 0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (v k i)))))))

(unless (= (g13) size)
  (format *stderr* "g13: ~S~%" (g13)))


(define (g14)
  (let ((v (make-float-vector (list 10 size/10) 1.0))
	(sum 0.0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (v k i)))))))

(unless (= (g14) size)
  (format *stderr* "g14: ~S~%" (g14)))


(define (g15)
  (let ((v (make-vector (list 10 size/10) 1 integer?))
	(sum 0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (v k i)))))))

(unless (= (g15) size)
  (format *stderr* "g15: ~S~%" (g15)))


(define (g16)
  (let ((v (make-vector (list 10 size/10) #\a char?))
	(sum 0))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! sum (+ sum (if (char=? (vector-ref v k i) #\a) 1 0)))))))

(unless (= (g16) size)
  (format *stderr* "g16: ~S~%" (g16)))


;;; --------------------------------------------------------------------------------

(define (h1)
  (let ((v (make-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size) (vector-ref v 0))
      (vector-set! v i 2))))

(unless (= (h1) 2)
  (format *stderr* "h1: ~S~%" (h1)))


(define (h2)
  (let ((v (make-int-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size) (int-vector-ref v 0))
      (int-vector-set! v i 2))))

(unless (= (h2) 2)
  (format *stderr* "h2: ~S~%" (h2)))


(define (h3)
  (let ((v (make-byte-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size) (byte-vector-ref v 0))
      (byte-vector-set! v i 2))))

(unless (= (h3) 2)
  (format *stderr* "h3: ~S~%" (h3)))


(define (h4)
  (let ((v (make-float-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size) (float-vector-ref v 0))
      (float-vector-set! v i 2.0))))

(unless (= (h4) 2.0)
  (format *stderr* "h4: ~S~%" (h4)))


(define (h5)
  (let ((v (make-vector size 1 integer?)))
    (do ((i 0 (+ i 1)))
	((= i size) (vector-ref v 0))
      (vector-set! v i 2))))

(unless (= (h5) 2)
  (format *stderr* "h5: ~S~%" (h5)))


(define (h6)
  (let ((v (make-vector size #(1 0) vector?)))
    (do ((i 0 (+ i 1)))
	((= i size) (vector-ref v 0 0))
      (vector-set! v i #(2 3)))))

(unless (= (h6) 2)
  (format *stderr* "h6: ~S~%" (h6)))


(define (h7)
  (let ((v (make-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size) (vector-ref v 0))
      (list-values (vector-set! v i 2)))))

(unless (= (h7) 2)
  (format *stderr* "h7: ~S~%" (h7)))


;;; --------------------------------

(define (h11)
  (let ((v (make-vector (list 10 size/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) (vector-ref v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(vector-set! v k i 2)))))

(unless (= (h11) 2)
  (format *stderr* "h11: ~S~%" (h11)))


(define (h12)
  (let ((v (make-int-vector (list 10 size/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) (int-vector-ref v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(int-vector-set! v k i 2)))))

(unless (= (h12) 2)
  (format *stderr* "h12: ~S~%" (h12)))


(define (h13)
  (let ((v (make-byte-vector (list 10 size/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) (byte-vector-ref v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(byte-vector-set! v k i 2)))))

(unless (= (h13) 2)
  (format *stderr* "h13: ~S~%" (h13)))


(define (h14)
  (let ((v (make-float-vector (list 10 size/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) (float-vector-ref v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(float-vector-set! v k i 2.0)))))

(unless (= (h14) 2.0)
  (format *stderr* "h14: ~S~%" (h14)))


(define (h15)
  (let ((v (make-vector (list 10 size/10) 1 integer?)))
    (do ((k 0 (+ k 1)))
	((= k 10) (vector-ref v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(vector-set! v k i 2))))) ; this calls int-vector-set!

(unless (= (h15) 2)
  (format *stderr* "h15: ~S~%" (h15)))


(define (h16)
  (let ((v (make-vector (list 10 size/10) :a keyword?)))
    (do ((k 0 (+ k 1)))
	((= k 10) (vector-ref v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(vector-set! v k i :b)))))

(unless (eq? (h16) :b)
  (format *stderr* "h16: ~S~%" (h16)))


(define (h17)
  (let ((v (make-vector (list 10 size/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) (vector-ref v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(list-values (vector-set! v k i 2))))))

(unless (= (h17) 2)
  (format *stderr* "h17: ~S~%" (h17)))


;;; --------------------------------------------------------------------------------


(define (j1)
  (let ((v (make-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size) (v 0))
      (set! (v i) 2))))

(unless (= (j1) 2)
  (format *stderr* "j1: ~S~%" (j1)))


(define (j2)
  (let ((v (make-int-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size) (v 0))
      (set! (v i) 2))))

(unless (= (j2) 2)
  (format *stderr* "j2: ~S~%" (j2)))


(define (j3)
  (let ((v (make-byte-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size) (v 0))
      (set! (v i) 2))))

(unless (= (j3) 2)
  (format *stderr* "j3: ~S~%" (j3)))


(define (j4)
  (let ((v (make-float-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size) (v 0))
      (set! (v i) 2.0))))

(unless (= (j4) 2.0)
  (format *stderr* "j4: ~S~%" (j4)))


(define (j5)
  (let ((v (make-vector size 1 integer?)))
    (do ((i 0 (+ i 1)))
	((= i size) (v 0))
      (set! (v i) 2))))

(unless (= (j5) 2)
  (format *stderr* "j5: ~S~%" (j5)))


(define (j6)
  (let ((v (make-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size) (v 0))
      (list-values (set! (v i) 2)))))

(unless (= (j6) 2)
  (format *stderr* "j6: ~S~%" (j6)))

;;; --------------------------------

(define (j11)
  (let ((v (make-vector (list 10 size/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) (v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! (v k i) 21)))))

(unless (= (j11) 21)
  (format *stderr* "j11: ~S~%" (j11)))

(define (j12)
  (let ((v (make-int-vector (list 10 size/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) (v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! (v k i) 22)))))

(unless (= (j12) 22)
  (format *stderr* "j12: ~S~%" (j12)))


(define (j13)
  (let ((v (make-byte-vector (list 10 size/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) (v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! (v k i) 23)))))

(unless (= (j13) 23)
  (format *stderr* "j13: ~S~%" (j13)))


(define (j14)
  (let ((v (make-float-vector (list 10 size/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) (v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! (v k i) 2.0)))))

(unless (= (j14) 2.0)
  (format *stderr* "j14: ~S~%" (j14)))


(define (j15)
  (let ((v (make-vector (list 10 size/10) 1 integer?)))
    (do ((k 0 (+ k 1)))
	((= k 10) (v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(set! (v k i) 24)))))

(unless (= (j15) 24)
  (format *stderr* "j15: ~S~%" (j15)))


(define (j16)
  (let ((v (make-vector (list 10 size/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) (v 0 0))
      (do ((i 0 (+ i 1)))
	  ((= i size/10))
	(list-values (set! (v k i) 2))))))

(unless (= (j16) 2)
  (format *stderr* "j16: ~S~%" (j16)))


;;; --------------------------------------------------------------------------------

(define ssize/10 30000)
(define ssize/10.0 (* 1.0 ssize/10))

(define (h111)
  (let ((v (make-vector (list 10 ssize/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) v)
      (do ((i 0 (+ i 1)))
	  ((= i ssize/10))
	(vector-set! v k i (+ i (* k ssize/10)))))))

(define (sum-h111)
  (let ((sum 0)
	(v (h111)))
    (do ((k 0 (+ k 1)))
	((= k 10) sum) ; (n-1) * (n/2) since we're starting at 0
      (do ((i 0 (+ i 1)))
	  ((= i ssize/10))
	(set! sum (+ sum (floor (vector-ref v k i))))))))
    
(unless (= (sum-h111) 44999850000)
  (format *stderr* "h111: ~S~%" (sum-h111)))


(define (h121)
  (let ((v (make-int-vector (list 10 ssize/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) v)
      (do ((i 0 (+ i 1)))
	  ((= i ssize/10))
	(int-vector-set! v k i (+ i (* k ssize/10)))))))

(define (sum-h121)
  (let ((sum 0)
	(v (h121)))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i ssize/10))
	(set! sum (+ sum (int-vector-ref v k i)))))))

(unless (= (sum-h121) 44999850000)
  (format *stderr* "h121: ~S~%" (sum-h121)))


(define (h131)
  (let ((v (make-byte-vector (list 10 ssize/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) v)
      (do ((i 0 (+ i 1)))
	  ((= i ssize/10))
	(byte-vector-set! v k i (modulo (+ i (* k ssize/10)) 256))))))

(define (sum-h131)
  (let ((sum 0)
	(v (h131)))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i ssize/10))
	(set! sum (+ sum (byte-vector-ref v k i)))))))

(unless (= (sum-h131) 38246416)
  (format *stderr* "h131: ~S~%" (sum-h131)))


(define (h141)
  (let ((v (make-float-vector (list 10 ssize/10))))
    (do ((k 0 (+ k 1)))
	((= k 10) v)
      (do ((i 0 (+ i 1)))
	  ((= i ssize/10))
	(float-vector-set! v k i (+ i (* k ssize/10.0)))))))

(define (sum-h141)
  (let ((sum 0.0)
	(v (h141)))
    (do ((k 0 (+ k 1)))
	((= k 10) sum)
      (do ((i 0 (+ i 1)))
	  ((= i ssize/10))
	(set! sum (+ sum (float-vector-ref v k i)))))))

(unless (= (sum-h141) 44999850000.0)
  (format *stderr* "h141: ~S~%" (sum-h141)))


;;; --------------------------------------------------------------------------------

(define size3 50)

(define (h1111)
  (let ((v (make-vector (list size3 size3 size3))))
    (do ((k 0 (+ k 1)))
	((= k size3) v)
      (do ((i 0 (+ i 1)))
	  ((= i size3))
	(do ((n 0 (+ n 1)))
	    ((= n size3))
	  (vector-set! v k i n (+ (* i size3) (* k size3 size3) n)))))))

(define (sum-h1111)
  (let ((sum 0)
	(v (h1111)))
    (do ((k 0 (+ k 1)))
	((= k size3) sum) 
      (do ((i 0 (+ i 1)))
	  ((= i size3))
	(do ((n 0 (+ n 1)))
	    ((= n size3))
	  (set! sum (+ sum (floor (vector-ref v k i n)))))))))

(unless (= (sum-h1111) 7812437500)
  (format *stderr* "h1111: ~S~%" (sum-h1111)))


(define (i1111)
  (let ((v (make-int-vector (list size3 size3 size3))))
    (do ((k 0 (+ k 1)))
	((= k size3) v)
      (do ((i 0 (+ i 1)))
	  ((= i size3))
	(do ((n 0 (+ n 1)))
	    ((= n size3))
	  (int-vector-set! v k i n (+ (* i size3) (* k size3 size3) n)))))))

(define (sum-i1111)
  (let ((sum 0)
	(v (i1111)))
    (do ((k 0 (+ k 1)))
	((= k size3) sum) 
      (do ((i 0 (+ i 1)))
	  ((= i size3))
	(do ((n 0 (+ n 1)))
	    ((= n size3))
	  (set! sum (+ sum (int-vector-ref v k i n))))))))

(unless (= (sum-i1111) 7812437500)
  (format *stderr* "i1111: ~S~%" (sum-i1111)))


(define (f1111)
  (let ((v (make-float-vector (list size3 size3 size3))))
    (do ((k 0 (+ k 1)))
	((= k size3) v)
      (do ((i 0 (+ i 1)))
	  ((= i size3))
	(do ((n 0 (+ n 1)))
	    ((= n size3))
	  (float-vector-set! v k i n (+ 0.0 (* i size3) (* k size3 size3) n)))))))

(define (sum-f1111)
  (let ((sum 0.0)
	(v (f1111)))
    (do ((k 0 (+ k 1)))
	((= k size3) sum) 
      (do ((i 0 (+ i 1)))
	  ((= i size3))
	(do ((n 0 (+ n 1)))
	    ((= n size3))
	  (set! sum (+ sum (float-vector-ref v k i n))))))))

(unless (= (sum-f1111) 7812437500.0)
  (format *stderr* "f1111: ~S~%" (sum-f1111)))

(define (vcop a b n)
  (let ((c (do ((i (- n 1) (- i 1)))
	       ((< i 0) b)
	     (vector-set! b i (vector-ref a i)))))
    (do ((i 0 (+ i 1)))
	((= i n) a)
      (vector-set! a i (vector-ref b i)))))

(define (tvcop)
  (do ((k 0 (+ k 1)))
      ((= k 1000))
    (vcop (make-vector 1000 1) (make-vector 1000 0) 1000)))

(tvcop)

(exit)

