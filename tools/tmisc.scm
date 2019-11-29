(set! (*s7* 'heap-size) (* 2 1024000))

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

(define-constant (w2)
  (let ((x 1))
    (let ((y (let-temporarily ((x 32))
	       (+ x 1))))
      (+ x y))))

(define-constant (w3)
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
(define-constant (f1)
  (cond (-2 => abs)))

(define-constant (x+1 x) 
  (+ x 1))

(define-constant (f2)
  (cond (32 => x+1)))

(define* (x+y x (y 2))
  (+ x y))

(define-constant (f3 z)
  (cond ((if z 1 3) => x+y)))

(define-constant (f4)
  (cond ((random 1) => "asdf")))

(define (xs)
  (values 1 2 3))

(define-constant (f5)
  (do ((i 0 (+ i 1))) ((xs) => +)))

(define-constant (f6 x)
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


;;; mv
(define (mv1)
  (+ (values 1 2 3)))
(define (mv2)
  (+ 1 (values 2 3)))
(define (mv3)
  (+ (values 1 2) 3))
(define (mv4 x)
  (+ x (values x x)))
(define (mv5 x)
  (+ (values x x) x))
(define (mv-clo1 x y)
  (+ x y))
(define (mv6 x)
  (mv-clo1 (values x 1)))
(define (mv-clo2 . args)
  (apply + args))
(define (mv7 x)
  (mv-clo2 (values x 1)))
(define (mv8)
  (+ (values 1 2 3) (values 3 -2 -1)))
(define (mv9)
  (+ (abs -1) (values 2 3 4) -4))
(define (mv10)
  (+ (values 1 2 3)))
(define (mv11)
  (+ (abs -1) (values -1 2 4)))
(define (mv12 x y)
  (+ x y (values 2 3 4)))

;;; pair_sym: (mv-clo (values x 1)), h_c_aa: (values x 1), splice_eval_args2 ([i] 1), eval_arg2->apply mv-clo! (loop below is safe_dotimes_step_p
;;;   not enough args for mv-clo1? 
;;; mv-clo2: closure_s_p -> pair_sym ->h_c_aa etc as above!
;;;   perhaps apply_[safe_]closure?

(define (mvtest)
  (unless (= (mv1) 6) (format *stderr* "mv1: ~S~%" (mv1)))
  (unless (= (mv2) 6) (format *stderr* "mv2: ~S~%" (mv2)))
  (unless (= (mv3) 6) (format *stderr* "mv3: ~S~%" (mv3)))
  (unless (= (mv4 2) 6) (format *stderr* "(mv4 2): ~S~%" (mv4 2)))
  (unless (= (mv5 2) 6) (format *stderr* "(mv5 2): ~S~%" (mv5 2)))
  (unless (= (mv6 5) 6) (format *stderr* "(mv6 5): ~S~%" (mv6 5)))
  (unless (= (mv7 5) 6) (format *stderr* "(mv7 5): ~S~%" (mv7 5)))
  (unless (= (mv8) 6) (format *stderr* "mv8: ~S~%" (mv8)))
  (unless (= (mv9) 6) (format *stderr* "mv9: ~S~%" (mv9)))
  (unless (= (mv10) 6) (format *stderr* "mv10: ~S~%" (mv10)))
  (unless (= (mv11) 6) (format *stderr* "mv11: ~S~%" (mv11)))
  (unless (= (mv12 -1 -2) 6) (format *stderr* "(mv12 -1 -2): ~S~%" (mv12 -1 -2)))
  (do ((i 0 (+ i 1)))
      ((= i 50000))
    (mv1)
    (mv2)
    (mv3)
    (mv4 i)
    (mv5 i)
    (mv6 i)
    (mv7 i)
    (mv8)
    (mv9)
    (mv10)
    (mv11)
    (mv12 -2 -1)
    ))

(mvtest)


;;; unlet
;;; incrementally set all globals to 42 -- check that unlet exprs return the same results

(let* ((syms (symbol-table))
       (num-syms (length syms))
       (orig-x (*s7* 'print-length)))
  
  (define (unlet-test i)
    (with-let (unlet)
      (catch #t
	(lambda ()
	  (eval `(define ,(syms i) 42))
	  (when (procedure? (symbol->value (syms i) (rootlet)))
	    (with-let (unlet)
	      (eval `(set! ,(syms i) 42) (rootlet)))))
	(lambda (type info)
	  ;(format *stderr* "~S unchanged: ~S~%" (syms i) (apply format #f info))
	  #f)))
    
    (with-let (unlet)
      (do ((k 0 (+ k 1)))
	  ((= k 1000))
	(catch #t
	  (lambda ()
	    (let ((x (+ k (*s7* 'print-length))))
	      (unless (eqv? x (+ k orig-x))
		(format *stderr* "sym: ~S, x: ~S, orig: ~S~%" (syms i) x (+ k orig-x)))))
	  (lambda (type info)
	    (format *stderr* "sym: ~S, error: ~S~%" (syms i) (apply format #f info)))))))
  
  (do ((i 0 (#_+ i 1))) ; "do" is not a procedure (see above) so it's not in danger here
      ((#_= i num-syms))
    (unlet-test i)))


(#_exit) ; we just clobbered exit above
