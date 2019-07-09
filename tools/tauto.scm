(set! (hook-functions *unbound-variable-hook*) ())
(set! (*s7* 'print-length) 6)
;(set! (*s7* 'gc-stats) #t)

(if (provided? 'snd)
    (begin
      (format *stderr* "this won't work in Snd!~%")
      (exit)))

;(load "stuff.scm")
;(load "r7rs.scm")
;(require mockery.scm)
;(load "s7test-block.so" (sublet (curlet) (cons 'init_func 'block_init)))
(define max-args 3)
(define-constant one 1)

;(define mock-number (*mock-number* 'mock-number))
;(define mock-pair (*mock-pair* 'mock-pair))
;(define mock-string (*mock-string* 'mock-string))
;(define mock-char (*mock-char* 'mock-char))
;(define mock-vector (*mock-vector* 'mock-vector))
;(define mock-symbol (*mock-symbol* 'mock-symbol))
;(define mock-hash-table (*mock-hash-table* 'mock-hash-table))

;(define np (list 0 1 2 3 4))
;(define mp (mock-pair '(0 1 2 3 4)))
;(define nv (vector 0 1 2 3 4))
;(define mv (mock-vector 0 1 2 3 4))
;(define ns "01234")
;(define ms (mock-string #\0 #\1 #\2 #\3 #\4))

(define-constant auto-constants (list #f #t () #\a (/ (*s7* 'most-positive-fixnum)) (/ -1 (*s7* 'most-positive-fixnum)) 1.5+i
			"hi455" :key hi: 'hi (list 1) (list 1 2) (cons 1 2) (list (list 1 2)) (list (list 1)) (list ()) #() 
			1/0+i 0+0/0i 0+1/0i 1+0/0i 0/0+0i 0/0+0/0i 1+1/0i 0/0+i cons ''2 
			1+i 1+1e10i 1e15+1e15i 0+1e18i 1e18 #\xff (string #\xff) 1e308 
			(*s7* 'most-positive-fixnum) (*s7* 'most-negative-fixnum) (- (*s7* 'most-positive-fixnum) 1) (+ (*s7* 'most-negative-fixnum) 1)
			-1 0 0.0 1 1.5 1.0-1.0i 3/4 #\null -63 (make-hash-table) (hash-table '(a . 2) '(b . 3))
			'((1 2) (3 4)) '((1 (2)) (((3) 4))) "" (list #i(1) "1") '(1 2 . 3) (list (cons 'a 2) (cons 'b 3))
			#i(1 2) (vector 1 '(3)) (let ((x 3)) (lambda (y) (+ x y))) abs 'a 'b one
			(lambda args args) (lambda* ((a 3) (b 2)) (+ a b)) (lambda () 3)
			(sublet () 'a 1) ;(rootlet)
			*load-hook*  *error-hook* (random-state 123)
			quasiquote macroexpand begin let letrec* if case cond (call-with-exit (lambda (goto) goto))
			(with-baffle (call/cc (lambda (cc) cc)))
			(string #\a #\null #\b) #2d((1 2) (3 4)) (inlet 'a 2 'b 3)
			#<undefined> #<unspecified> (make-int-vector 3) (make-float-vector 3 -1.4)
			(make-vector '(2 3) "hi") #("hiho" "hi" "hoho") (subvector (make-int-vector '(2 3) 1) 6)
			(subvector (subvector (make-float-vector '(2 3) 1.0) 6) '(2 2))
			(vector-ref #2d((#i(1 2 3)) (#i(3 4 5))) 0 0) (define-macro (m a) `(+ ,a 1))
			(c-pointer 0) (c-pointer -1) :readable :else (define-bacro* (m (a 1)) `(+ ,a 1))
			(byte-vector 0 1 2) (byte-vector) (byte-vector 255 0 127) (make-iterator #((a . 2)))
			(lambda (dir) 1.0) (float-vector) (make-float-vector '(2 32)) 
			'((a . 1)) #i(1) '((((A . B) C . D) (E . F) G . H) ((I . J) K . L) (M . N) O . P)

			#u(0 1 2) (openlet (inlet 'abs (lambda (x) (- x))))
			(make-iterator (list 1 2 3)) (make-iterator "1") #<eof> #r2d((.1 .2) (.3 .4))
			(dilambda (lambda () 1) (lambda (a) a))
			(gensym)
#|
			(make-block 2) (block 1.0 2.0 3.0) (block) 
			(mock-number 0) (mock-number 1-i) (mock-number 4/3) (mock-number 2.0)
			(mock-string #\h #\o #\h #\o)
			(mock-pair '(2 3 4))
			(mock-char #\b)
			(mock-symbol 'c)
			(mock-vector 1 2 3 4)
			(mock-hash-table 'b 2)
			np mp nv mv ns ms
|#			
			))
      
(define car-auto-constants (car auto-constants))
(define-constant cdr-auto-constants (cdr auto-constants))

(define low 0)
(define-constant auto-arglists (vector (make-list 1) (make-list 2) (make-list 3) (make-list 4) (make-list 5) (make-list 6)))

(define-constant (autotest func args args-now args-left sig)
  ;; args-left is at least 1, args-now starts at 0, args starts at ()
  ;; (format *stderr* "~A: ~D ~D (~D ~D): ~A~%" func (length args) args-now low args-left args)
  ;; (if (pair? args) (format *stderr* "~A " (car args)))

  (call-with-exit
   (lambda (quit)
     (if (>= args-now low)
	 (catch #t 
	   (lambda () 
	     ;(format *stderr* "args: ~A~%" args)
	     (apply func args))
	   (lambda (type info)
	     (if (and (positive? args-now)
		      (memq type '(wrong-type-arg wrong-number-of-args out-of-range syntax-error io-error
				   division-by-zero format-error missing-method error invalid-escape-function)))
		 (quit)))))

     (let ((c-args (vector-ref auto-arglists args-now)))
       (copy args c-args)

       (let ((p (list-tail c-args args-now))
	     (checker (and (pair? sig) (car sig)))) ; see map-values
	 
	 (define (call-func1 c)
	   (when (checker c)
	     (catch #t 
	       (lambda () 
		 (set-car! p c)
		 ;(format *stderr* "c-args: ~A~%" c-args)
		 (apply func c-args))
	       (lambda any 
		 'error))))

	 (define (call-func2 c)
	   (catch #t 
	     (lambda () 
	       (set-car! p c)
	       ;(format *stderr* "c-args: ~A~%" c-args)
	       (apply func c-args))
	     (lambda any 
	       'error)))

	 (if (= args-left 1)
	     (call-with-exit
	      (lambda (quit)
		(set-car! p car-auto-constants)
		(catch #t
		  (lambda ()
		    ;(format *stderr* "c-args: ~A~%" c-args)
		    (apply func c-args))
		  (lambda (type info)
		    (if (or (memq type '(wrong-number-of-args out-of-range syntax-error io-error
					 division-by-zero format-error error missing-method invalid-escape-function))
			    (and (eq? type 'wrong-type-arg)
				 (pair? (cdr info))
				 (pair? (cddr info))
				 (integer? (caddr info)) ; if just 1 arg, arg num can be omitted
				 (< (caddr info) low)))
			(quit))))
		 
		(if checker ; map-values -> function here
		    (for-each call-func1 cdr-auto-constants)
		    (for-each call-func2 cdr-auto-constants))))

	     (let ((sig1 (if (pair? sig) (cdr sig) ()))
		   (c-args1 c-args)
		   (args-now1 (+ args-now 1))
		   (args-left1 (- args-left 1)))
	       (if checker
		   (for-each
		    (lambda (c)
		      (when (checker c)
			(set-car! p c)
			(autotest func c-args1 args-now1 args-left1 sig1)))
		    auto-constants)
		   (for-each
		    (lambda (c)
		      (set-car! p c)
		      (autotest func c-args1 args-now1 args-left1 sig1))
		    auto-constants)))))))))

(define safe-fill!
  (let ((+signature+ '(#t sequence? #t)))
    (lambda (obj arg)
      (if (not (let? obj))
	  (fill! obj arg)))))

(define (map-values lst)
  (do ((lst lst (cdr lst)))
      ((or (not (pair? lst))
	   (not (car lst))
	   (procedure? (car lst))) 
       lst)
    (set-car! lst
	      (if (symbol? (car lst))
		  (symbol->value (car lst))
		  (and (pair? (car lst))
		       (apply lambda '(x) (list (list 'or (list (caar lst) 'x) (list (cadar lst) 'x)))))))))

(define baddies '(exit emergency-exit abort autotest s7-optimize
		  all delete-file system set-cdr! stacktrace test-sym
		  cutlet varlet gc cond-expand reader-cond
		  openlet coverlet eval vector list cons values
		  symbol-table load throw error openlets coverlets
		  global-environment current-environment make-rectangular hash-table* make-keyword ;morally-equal?
		  copy fill! hash-table-set! vector-set! let-set! list-values apply-values immutable!
		  *unbound-variable-hook* *load-hook* *rootlet-redefinition-hook* *missing-close-paren-hook* *read-error-hook*
		  tree-count ; signature is kinda silly here
		  c-define-1 apropos map-values
		  ;outlet-member make-method make-object
		  ))

(define (test-sym sym)
  (if (and (not (memq sym baddies))
	   (defined? sym))
      (let* ((f (symbol->value sym))
	     (argn (and (or (procedure? f) (let? f)) (arity f))))
	(if argn
	    (let ((bottom (car argn))
		  (top (min (cdr argn) max-args))
		  (strname (symbol->string sym)))
	      (if (not (memv (strname 0) '(#\{ #\[ #\()))
		  (begin
		    ;(format *stderr* "~A~%" sym)
		    (if (< top bottom)
			(format *stderr* ";~A (bottom: ~A, top: ~A)...~%" sym bottom top))
			;(format *stderr* ";~A...~%" sym))
		    (set! low bottom)
		    (if (positive? (cdr argn))
			(let ((sig (cond ((eq? sym 'append)
					  (let ((lst (list 'list?)))
					    (set-cdr! lst lst)))
					 ((signature f) => copy))))
			  (map-values sig)
			  (autotest f () 0 top (if (pair? sig) (cdr sig) ())))))))))))

(define (all)
  (let ((st (symbol-table)))
    (for-each test-sym st)
    ;(do ((i 0 (+ i 1)) (len (length st))) ((= i 1000)) (test-sym (st (random len))))
    ;(test-sym 'object->string)
    ;(test-sym 'for-each)
    (format *stderr* "~%all done~%")
    ))

;(test-sym 'write)
(all)

(exit)
