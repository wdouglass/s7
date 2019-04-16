(let ((new-env (sublet (curlet) (cons 'init_func 'block_init)))) ; load calls init_func if possible
  (load "s7test-block.so" new-env))

(load "mockery.scm")

(define-constant one 1)

(define mock-number (*mock-number* 'mock-number))
(define mock-pair (*mock-pair* 'mock-pair))
(define mock-string (*mock-string* 'mock-string))
(define mock-char (*mock-char* 'mock-char))
(define mock-vector (*mock-vector* 'mock-vector))
(define mock-symbol (*mock-symbol* 'mock-symbol))
(define mock-hash-table (*mock-hash-table* 'mock-hash-table))

;;(openlet (inlet 'i 0 'list-set! (lambda (l . args) (apply #_list-set! l ((car args) 'i) (cdr args))))))

(define-constant constants (vector #f #t () #\a (/ most-positive-fixnum) (/ -1 most-positive-fixnum) 1.5+i 
			  "hi455" "\n  \t\x65;" :key hi: 'hi (list 1) (list 1 2) (cons 1 2) (list (list 1 2)) (list (list 1)) (list ()) #() 
			  1/0+i 0+0/0i 0+1/0i 1+0/0i 0/0+0i 0/0+0/0i 1+1/0i 0/0+i cons ''2 
			  1024 -1024 10000 10001 30000 512 -512 
			  1+i 1+1e10i 1e15+1e15i 0+1e18i 1e18 #\xff (string #\xff) 1e308 
			  most-positive-fixnum most-negative-fixnum (- most-positive-fixnum 1) (+ most-negative-fixnum 1)
			  -1 0 0.0 1 1.5 1.0-1.0i 3/4 #\null -63 (make-hash-table) (hash-table '(a . 2) '(b . 3))
			  '((1 2) (3 4)) '((1 (2)) (((3) 4))) "" (list #(1) "1") '(1 2 . 3) (list (cons 'a 2) (cons 'b 3))
			  #(1 2) (vector 1 '(3)) (let ((x 3)) (lambda (y) (+ x y))) abs 'a 'b one apply 
			  (lambda args args) (lambda* ((a 3) (b 2)) (+ a b)) (lambda () 3)
			  (sublet () (cons 'a 1)) ;(rootlet)
			  *load-hook*  *error-hook* (random-state 123)
			  quasiquote macroexpand cond-expand begin let letrec* if case cond (call-with-exit (lambda (goto) goto))
			  (with-baffle (call/cc (lambda (cc) cc)))
			  (string #\a #\null #\b) #2d((1 2) (3 4)) (inlet 'a 2 'b 3)
			  #<undefined> #<unspecified> (make-int-vector 3) (make-float-vector 3 -1.4)
			  (make-vector '(2 3) "hi") #("hiho" "hi" "hoho") (subvector (make-int-vector '(2 3) 1) 6)
			  (subvector (subvector (make-float-vector '(2 3) 1.0) 6) '(2 2))
			  (vector-ref #2d((#(1 2 3)) (#(3 4 5))) 0 0) (define-macro (m a) `(+ ,a 1))
			  (c-pointer 0) (c-pointer -1) :readable *s7* else (define-bacro* (m (a 1)) `(+ ,a 1))
			  (byte-vector 0 1 2) (byte-vector) (byte-vector 255 0 127) (make-iterator (vector '(a . 2)))
			  (lambda (dir) 1.0) (float-vector) (make-float-vector '(2 2)) (int-vector 1 2 3) (int-vector)
			  (inlet 'value 1 '+ (lambda args 1)) (inlet) (make-iterator (inlet 'a 1 'b 2) (cons #f #f))
			  (make-iterator "123456") (make-iterator '(1 2 3)) (make-iterator (hash-table 'a 1 'b 2) (cons #f #f))
			  (open-input-string "123123") (open-input-file "/home/bil/cl/4.aiff")
			  (open-output-file "test.test") (open-output-string)
			  
			  ;(mock-number 0) (mock-number 2) (mock-number 1-i) (mock-number 4/3) (mock-number 2.0)
			  (mock-string #\h #\o #\h #\o)
			  (mock-pair '(2 3 4))
			  (mock-char #\b)
			  (mock-symbol 'c)
			  (mock-vector 1 2 3 4)
			  (mock-hash-table 'b 2)
			  
			  (make-block 4) (block) (make-iterator (block 1 2 3 4))
			  ))
(define-constant constants-len (length constants))

(define-constant ctrl-chars (string ;#\A #\S #\C #\F #\E #\G #\O #\D #\B #\X #\W
		    #\, #\{ #\} #\@ #\P #\*
		    #\a #\s #\c #\f #\e #\g #\o #\d #\b #\x #\p #\n #\w
		    #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
		    #\~ #\T #\& #\% #\^ #\|
		    #\~ #\~ #\~ #\~ 
		    #\, #\, #\, #\, #\" #\" #\\ #\'
		    #\+ #\- #\@ #\. #\/ #\; #\:
		    ))
(define-constant ctrl-chars-len (length ctrl-chars))

(define (test-calls ctrl-str tries size1 op)
  (let ((x #f) (y #f) (z #f) (pos 0)
	(cs constants)
	(cs-len constants-len))
    (do ((i 0 (+ i 1)))
	((= i tries))
      (do ((j 1 (+ j 1)))
	  ((= j size1))
	(string-set! ctrl-str j (string-ref ctrl-chars (random ctrl-chars-len))))
      
      (set! x (vector-ref cs (random cs-len)))
      (set! y (vector-ref cs (random cs-len)))
      (set! z (vector-ref cs (random cs-len)))
      
      (object->string x)
      (display x op)
      
      (catch #t (lambda () (format #f "~{~^~S ~} ~{~|~S ~} ~W" x y z)) (lambda arg 'error))
      (catch #t (lambda () (format #f ctrl-str)) (lambda arg 'error))
      (catch #t (lambda () (format #f ctrl-str x)) (lambda arg 'error))
      (catch #t (lambda () (format #f ctrl-str y)) (lambda arg 'error))
      (catch #t (lambda () (format #f ctrl-str z)) (lambda arg 'error))
      (set! pos (char-position #\~ ctrl-str 1))
      (when pos
	(catch #t (lambda () (format #f ctrl-str x z)) (lambda arg 'error))
	(catch #t (lambda () (format #f ctrl-str x y)) (lambda arg 'error))
	(catch #t (lambda () (format #f ctrl-str y z)) (lambda arg 'error))
	(catch #t (lambda () (format #f ctrl-str z x)) (lambda arg 'error))
	(when (char-position #\~ ctrl-str (+ pos 1))
	  (catch #t (lambda () (format #f ctrl-str x y z)) (lambda arg 'error))
	  (catch #t (lambda () (format #f ctrl-str z y x)) (lambda arg 'error)))))))

(define (test-chars)
  (let ((op (open-output-string)))
    (do ((size 2 (+ size 1))
	 (size1 3 (+ size1 1))
	 (tries 4000 (+ tries 2000))
	 (ctrl-str (make-string 16 #\space)))
	((= size 15))
      (format *stderr* "~D " size)
      (string-set! ctrl-str size1 #\null)
      (string-set! ctrl-str 0 #\~)
      (test-calls ctrl-str tries size1 op)
      (get-output-string op #t))
    (close-output-port op)))

(test-chars)

;(do ((i 0 (+ i 1))) ((= i 1000)) (test-chars))

(s7-version)
(exit)
