;;; tletr

;;; --------------------------------------------------------------------------------
;;; or/and

(define big-list (make-list 10000 1))

(define (and-1 lst)
  (let loop ((x lst))
    (and (pair? x)
	 (or (symbol? (car x))
	     (loop (cdr x))))))

(define (and-2 lst)
  (let loop ((x lst))
    (or (null? x)
	(and (integer? (car x))
	     (loop (cdr x))))))

(define (and-3 lst)
  (let loop ((x lst) (count 10000))
    (and (positive? count)
	 (or (null? x)
	     (loop (cdr x) (- count 1))))))

(define (and-4 lst)
  (let loop ((x lst) (count 10000))
    (or (null? x)
	(and (positive? count)
	     (loop (cdr x) (- count 1))))))

(define (and*-1 lst)
  (let* loop ((x lst))
    (and (pair? x)
	 (or (symbol? (car x))
	     (loop (cdr x))))))

(define (and-f)
  (if (and-1 big-list) (format #t "and-1 returned #t\n"))
  ;(if (and*-1 big-list) (format #t "and*-1 returned #t\n"))
  (if (not (and-2 big-list)) (format #t "and-2 returned #f\n"))
  (if (and-3 big-list) (format #t "and-3 returned #t\n"))
  (if (not (and-4 big-list)) (format #t "and-4 returned #f\n")))


;;; --------

(define (pand-1 x)             ; rclo_and_a_or_a_la
  (and (pair? x)
       (or (symbol? (car x))
	   (pand-1 (cdr x)))))

(define (dpand-1 lst)
  (define (dpand-11 x)
    (and (pair? x)
	 (or (symbol? (car x))
	     (dpand-11 (cdr x)))))
  (dpand-11 lst))

(define (pand-2 x)             ; rclo_or_a_and_a_la
  (or (null? x)
      (and (integer? (car x))
	   (pand-2 (cdr x)))))

(define (pand-3 x count)
  (and (positive? count)
       (or (null? x)
	   (pand-3 (cdr x) (- count 1)))))

(define (pand-4 x count)
  (or (null? x)
      (and (positive? count)
	   (pand-4 (cdr x) (- count 1)))))

(define (pand-f)
  (if (pand-1 big-list) (format #t "pand-1 returned #t\n"))
  (if (dpand-1 big-list) (format #t "dpand-1 returned #t\n"))
  (if (not (pand-2 big-list)) (format #t "pand-2 returned #f\n"))
  (if (pand-3 big-list 10000) (format #t "pand-3 returned #t\n"))
  (if (not (pand-4 big-list 10000)) (format #t "pand-4 returned #f\n")))


;;; --------------------------------------------------------------------------------
;;; if

(define (sum-1 n)
  (let loop ((i n) (sum 0))
    (if (< i 0)
        sum
        (loop (- i 1) (+ i sum)))))

(define (sum*-1 n)
  (let* loop ((i n) (sum 0))
    (if (< i 0)
        sum
        (loop (- i 1) (+ i sum)))))

(define (sum-2 n)
  (let loop ((i n) (sum 0.0))
    (if (< i 0.0)
        sum
        (loop (- i 1.0) (+ i sum)))))

(define (esum-2 n)
  (let loop ((i n) (sum 0))
    (if (< i 0.0)
        sum
        (loop (- i 1.0) (+ i sum)))))

(define (sum-3 n)
  (let loop ((i n) (sum 0))
    (if (>= i 0)
        (loop (- i 1) (+ i sum))
        sum)))

(define (sum-4 n)
  (let loop ((i n) (sum 0.0))
    (if (>= i 0.0)
        (loop (- i 1.0) (+ i sum))
        sum)))

(define (sum-5 n)
  (let loop ((i n) (sum 0))
    (if (< i -1)
	-2
	(if (< i 0)
	    sum
	    (loop (- i 1) (+ sum i))))))

(define big-str (make-string 10000 #\b))

(define (sum-6 str)
  (let loop ((i (- (length str) 1)))
    (if (< i 0)
	#f
	(if (char=? (string-ref str i) #\a)
	    #\?
	    (loop (- i 1))))))

(define (llet-1 x y)             ; rclo_let_if_a_laa
  (let loop ((x x) (y y))
    (let ((z (+ x y)))
      (if (< z 0)
	  z
	  (loop (- x 1) (- y 1))))))

(define (sum-f)
  (let ((n (sum-1 10000)))
    (if (not (= n 50005000))
	(format #t ";sum1: ~A~%" n)))
#|
  (let ((n (sum*-1 10000)))
    (if (not (= n 50005000))
	(format #t ";sum*-1: ~A~%" n)))
|#
  (let ((result (sum-2 10000.0)))
    (if (not (equal? result 50005000.0))
	(format #t ";sum-2: ~A~%" result)))
#|
  (let ((result (esum-2 10000)))
    (if (or (not (float? result))
	    (not (equal? result 50005000.0)))
	(format #t ";esum-2: ~A~%" result)))
|#
  (let ((n (sum-3 10000)))
    (if (not (= n 50005000))
	(format #t ";sum-3: ~A~%" n)))

  (let ((result (sum-4 10000.0)))
    (if (not (equal? result 50005000.0))
	(format #t ";sum-4: ~A~%" result)))

  (let ((result (sum-5 10000)))
    (if (not (= result 50005000))
	(format #t ";sum-5: ~A~%" n)))

  (let ((result (sum-6 big-str)))
    (if result
	(format #t ";sum-6: ~A~%" result)))

  (let ((result (llet-1 10000 10001)))
    (if (not (= result -1))
	(format #t ";llet-1: ~A~%" result))))

;;; --------

(define (psum-1 i sum)
  (if (< i 0)
      sum
      (psum-1 (- i 1) (+ i sum))))

(define (dpsum-1 n s)
  (define* (dpsum-11 i sum)
    (if (< i 0)
	sum
	(dpsum-11 (- i 1) (+ i sum))))
  (dpsum-11 n s))

(define (npsum-1 i sum)
  (if (not (< i 0))
      (psum-1 (- i 1) (+ i sum))
      sum))

(define (psum-2 i sum)
  (if (< i 0.0)
      sum
      (psum-2 (- i 1.0) (+ i sum))))

(define dpsum-2 
  (lambda (i sum)
    (if (< i 0.0)
	sum
	(dpsum-2 (- i 1.0) (+ i sum)))))

(define (psum-3 i sum)
  (if (>= i 0)
      (psum-3 (- i 1) (+ i sum))
      sum))

(define (psum-4 i sum)
  (if (>= i 0.0)
      (psum-4 (- i 1.0) (+ i sum))
      sum))

(define (psum-5 i sum)
  (if (< i -1)
      -2
      (if (< i 0)
	  sum
	  (psum-5 (- i 1) (+ sum i)))))

(define (psum-6 i)
  (if (< i 0)
      #f
      (if (char=? (string-ref big-str i) #\a)
	  #\?
	  (psum-6 (- i 1)))))

(define (plet-1 x y)             ; rclo_let_if_a_laa
  (let ((z (+ x y)))
    (if (< z 0)
	z
	(plet-1 (- x 1) (- y 1)))))

(define (psum-f)
  (let ((n (psum-1 10000 0)))
    (if (not (= n 50005000))
	(format #t ";psum-1: ~A~%" n)))
#|
  (let ((n (dpsum-1 10000 0)))
    (if (not (= n 50005000))
	(format #t ";dpsum-1: ~A~%" n)))
|#
  (let ((n (npsum-1 10000 0)))
    (if (not (= n 50005000))
	(format #t ";npsum-1: ~A~%" n)))

  (let ((result (psum-2 10000.0 0.0)))
    (if (not (equal? result 50005000.0))
	(format #t ";psum-2: ~A~%" result)))

  (let ((result (dpsum-2 10000.0 0.0)))
    (if (not (equal? result 50005000.0))
	(format #t ";dpsum-2: ~A~%" result)))

  (let ((n (psum-3 10000 0)))
    (if (not (= n 50005000))
	(format #t ";psum-3: ~A~%" n)))

  (let ((result (psum-4 10000.0 0.0)))
    (if (not (equal? result 50005000.0))
	(format #t ";psum-4: ~A~%" result)))

  (let ((result (psum-5 10000 0)))
    (if (not (= result 50005000))
	(format #t ";psum-5: ~A~%" result)))

  (let ((result (psum-6 9999)))
    (if result
	(format #t ";psum-6: ~A~%" result)))

  (let ((result (plet-1 10000 10001)))
    (if (not (= result -1))
	(format #t ";plet-1: ~A~%" result))))


;;; --------------------------------------------------------------------------------
(define (tests)
  (do ((i 0 (+ i 1)))
      ((= i 100))
    (sum-f)
    (and-f)
    (psum-f)
    (pand-f)
    ))

(tests)

(exit)
