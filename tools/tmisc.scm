(define size 500000)

;;; let-temporarily
(define (w1 x)
  (let ((y x))
    (do ((j 0 (+ j 1)))
	((= j 1))
      (do ((i 0 (+ i 1)))
	  ((= i size))
	(let-temporarily ((y 32))
	  (unless (= y 32)
	    (format *stderr* "temp y: ~A~%" y)))
	(unless (= y x)
	  (format *stderr* "y: ~A~%" y))))))

(define (w2)
  (let ((x 1))
    (let ((y (let-temporarily ((x 32))
	       (+ x 1))))
      (+ x y))))

(define (w3)
  (let ((x 1)
	(y 2))
    (let ((z (let-temporarily ((x 6) (y 7))
	       (+ x y))))
      (+ x y z))))

(define (w4)
  (let ((y (let-temporarily (((*s7* 'print-length) 32))
	     (*s7* 'print-length))))
    (+ y 1)))

(define (wtest)
  (w1 3)
  (unless (= (w2) 34) (format *stderr* "w2 got ~S~%" (w2)))
  (unless (= (w3) 16) (format *stderr* "w3 got ~S~%" (w3)))
  (do ((i 0 (+ i 1)))
      ((= i size))
    (w2)
    (w3)))


;;; =>
(define (f1)
  (cond (-2 => abs)))

(define (x+1 x) 
  (+ x 1))

(define (f2)
  (cond (32 => x+1)))

(define* (x+y x (y 2))
  (+ x y))

(define (f3 z)
  (cond ((if z 1 3) => x+y)))

(define (f4)
  (cond ((random 1) => "asdf")))

(define (xs)
  (values 1 2 3))

(define (f5)
  (do ((i 0 (+ i 1))) ((xs) => +)))

(define (f6 x)
  (case x ((1) 2) (else => abs)))

(define (ftest)
  (unless (= (f1) 2) (format *stderr* "f1 got ~S~%" (f1)))
  (unless (= (f2) 33) (format *stderr* "f2 got ~S~%" (f2)))
  (unless (= (f3 #t) 3) (format *stderr* "(f3 #t) got ~S~%" (f3 #t)))
  (unless (= (f3 #f) 5) (format *stderr* "(f3 #f) got ~S~%" (f3 #f)))
  (unless (char=? (f4) #\a) (format *stderr* "(f4) got ~S~%" (f4)))
  (unless (= (f5) 6) (format *stderr* "(f5) got ~S~%" (f5)))
  (unless (= (f6 -2) 2) (format *stderr* "(f6 -2) got ~S~%" (f6 -2)))

  (do ((i 0 (+ i 1)))
      ((= i size))
    (f1)
    (f2)
    (f3 #t)
    (f4)
    (f5)
    (f6 -2)))

(ftest)
(wtest)


(exit)
