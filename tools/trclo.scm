;;; tletr

;;; --------------------------------------------------------------------------------
;;; or/and

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

(define big-list (make-list 10000 1))

(define (and-f)
  (if (and-1 big-list) (format #t "and-1 returned #t\n"))
  (if (not (and-2 big-list)) (format #t "and-2 returned #f\n"))
  (if (and-3 big-list) (format #t "and-3 returned #t\n"))
  (if (not (and-4 big-list)) (format #t "and-4 returned #f\n")))


;;; --------

(define (pand-1 x)             ; rclo_and_a_or_a_la
  (and (pair? x)
       (or (symbol? (car x))
	   (pand-1 (cdr x)))))

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

(define (sum-2 n)
  (let loop ((i n) (sum 0.0))
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

(define (sum-f)
  (let ((n (sum-1 10000)))
    (if (not (= n 50005000))
	(format #t ";sum1: ~A~%" n)))

  (let ((result (sum-2 10000.0)))
    (if (not (equal? result 50005000.0))
	(format #t ";sum2: ~A~%" result)))

  (let ((n (sum-3 10000)))
    (if (not (= n 50005000))
	(format #t ";sum3: ~A~%" n)))

  (let ((result (sum-4 10000.0)))
    (if (not (equal? result 50005000.0))
	(format #t ";sum4: ~A~%" result))))

;;; --------

(define (psum-1 i sum)
  (if (< i 0)
      sum
      (psum-1 (- i 1) (+ i sum))))

(define (psum-2 i sum)
  (if (< i 0.0)
      sum
      (psum-2 (- i 1.0) (+ i sum))))

(define (psum-3 i sum)
  (if (>= i 0)
      (psum-3 (- i 1) (+ i sum))
      sum))

(define (psum-4 i sum)
  (if (>= i 0.0)
      (psum-4 (- i 1.0) (+ i sum))
      sum))

(define (psum-f)
  (let ((n (psum-1 10000 0)))
    (if (not (= n 50005000))
	(format #t ";psum1: ~A~%" n)))

  (let ((result (psum-2 10000.0 0.0)))
    (if (not (equal? result 50005000.0))
	(format #t ";psum2: ~A~%" result)))

  (let ((n (psum-3 10000 0)))
    (if (not (= n 50005000))
	(format #t ";psum3: ~A~%" n)))

  (let ((result (psum-4 10000.0 0.0)))
    (if (not (equal? result 50005000.0))
	(format #t ";psum4: ~A~%" result))))


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
