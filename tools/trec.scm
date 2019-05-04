(set! (*s7* 'heap-size) (* 4 1024000))

(define-constant (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(let ((f32 (fib 32)))
  (unless (= f32 2178309) ;3524578)
    (display f32) 
    (newline)))


#|
(define* (tfib n (a 1) (b 1)) ; tail-call version
   (if (= n 0)
       a
       (if (= n 1)
           b
           (tfib (- n 1) b (+ a b)))))

(let ((f32 (tfib 31)))
  (unless (= f32 2178309)
    (display f32) 
    (newline)))

(define (dfib Z)             ; do-loop equivalent to tfib
  (do ((a 1 b)
       (b 1 (+ a b))
       (n Z (- n 1)))
      ((< n 2)
       (if (= n 0) a b))))

(define (dofib n)            ; another do-loop, faster in s7 (twice as fast as tfib)
  (if (< n 2)
      1
      (do ((a 1)
	   (b 1)
	   (c 0)
	   (i 1 (+ i 1)))
	  ((= i n) b)
	(set! c b)
	(set! b (+ a b))
	(set! a c))))
|#


(define-constant (trib n)
  (if (< n 3)
      1
      (+ (trib (- n 1))
         (trib (- n 2))
         (trib (- n 3)))))

(let ((f32 (trib 26)))
  (unless (= f32 3311233)
    (display f32) 
    (newline)))

;; tc is much faster:
(define* (ttrib n (a 1) (b 1) (c 1))
  (if (= n 0)
      a
      (if (= n 1)
	  b
	  (if (= n 2)
	      c
	      (ttrib (- n 1) b c (+ a b c))))))

(let ((f32 (ttrib 26)))
  (unless (= f32 3311233)
    (display f32) 
    (newline)))


(define all-coins '(50 25 10 5 1)) 

(define-constant (cc amount kinds-of-coins) 
  (cond ((= amount 0) 1) 
        ((or (< amount 0) (null? kinds-of-coins)) 0) 
        (else (+ (cc amount (cdr kinds-of-coins)) 
                 (cc (- amount (car kinds-of-coins)) kinds-of-coins))))) 

(define (count-change amount) 
  (cc amount all-coins)) 

(let ((coins (count-change 400)))
  (unless (= coins 26517)
    (display coins)
    (newline)))


(define (add lst)
  (let loop ((p lst)
	     (sum 0))
    (if (pair? p)
	(loop (cdr p) (+ sum (car p)))
	sum)))

(define big-list (make-list 10000 1))

(define (more-add)
  (let ((lst big-list))
    (do ((i 0 (+ i 1)))
	((= i 1000))
      (add lst))))
(more-add)


(define-constant adder 
  (lambda* (lst (sum 0))
    (if (pair? lst)
	(adder (cdr lst) (+ sum (car lst)))
	sum)))

(define (more-adder)
  (let ((lst big-list))
    (do ((i 0 (+ i 1)))
	((= i 1000))
      (adder lst 0))))
(more-adder)


(define-constant (got-symbol lst)
  (and (pair? lst)
       (or (symbol? (car lst))
	   (got-symbol (cdr lst)))))

(define (more-symbol)
  (let ((lst big-list))
    (do ((i 0 (+ i 1)))
	((= i 1000))
      (got-symbol lst))))
(more-symbol)

(set! big-list #f)

;;; add local-slot do cases to s7test

(define-constant (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(let ((n (ack 3 8)))
  (unless (= n 2045)
    (display n)
    (newline)))

(define-constant (tree-eq? a b)
  (if (pair? a)
      (and (pair? b)
	   (tree-eq? (car a) (car b))
	   (tree-eq? (cdr a) (cdr b)))
      (eq? a b)))

(define tree '((a b) (c d e) (f) () (g h i j) (k (l m (n o)) p) (q ((r) s) (((t (u) v) w) x) y) z))
(define (more-eq)
  (do ((i 0 (+ i 1)))
      ((= i 100000))
    (tree-eq? tree tree)))
(more-eq)

(s7-version)
(exit)
