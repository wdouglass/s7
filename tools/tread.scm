(load "write.scm")
(load "s7test-block.so" (sublet (curlet) (cons 'init_func 'block_init)))

(set! (*s7* 'print-length) 8) ; :readable should ignore this

(define (tester)
  (do ((baddies 0)
       (size 3 (+ size 1)))
      ((= size 4))
    (format *stderr* "~%-------- ~D --------~%" size)
    
    (do ((tries (* 2000 (expt 3 size)))
	 (k 0 (+ k 1)))
	((or (= k tries)
	     (> baddies 1)))
      
      (let ((cp-lst (make-list 3 #f))
	    (it-lst (make-list 3 #f)))
	(let ((bases (vector (make-list 3 #f)
			     (make-vector 3 #f)
			     (make-cycle #f)
			     (hash-table* 'a 1 'b 2 'c 3)
			     (inlet 'a 1 'b 2 'c 3)
			     (make-iterator it-lst)
			     (c-pointer 1 cp-lst)))
	      (sets ()))
	  
	  (do ((blen (length bases))
	       (i 0 (+ i 1)))
	      ((= i size))
	    (let ((r1 (random blen))
		  (r2 (random blen))
		  (loc (random 3)))
	      (let ((b1 (bases r1))
		    (b2 (bases r2)))
		(case (type-of b1)
		  ((pair?)
		   (if (> (random 10) 3)
		       (begin
			 (set! (b1 loc) b2)
			 (set! sets (cons (list r1 loc r2) sets)))
		       (begin
			 (set-cdr! (list-tail b1 2) (case loc ((0) b1) ((1) (cdr b1)) (else (cddr b1))))
			 (set! sets (cons (list r1 (+ loc 3) r2) sets)))))

		  ((vector?)
		   (set! (b1 loc) b2)
		   (set! sets (cons (list r1 loc r2) sets)))

		  ((c-object?)
		   (set! (b1 0) b2)
		   (set! sets (cons (list r1 0 r2) sets)))

		  ((hash-table? let?)
		   (let ((key (#(a b c) loc)))
		     (set! (b1 key) b2)
		     (set! sets (cons (list r1 key r2) sets))))

		  ((c-pointer?)
		   (set! (cp-lst loc) b2)
		   (set! sets (cons (list r1 loc r2) sets)))

		  ((iterator?)
		   (set! (it-lst loc) b2)
		   (set! sets (cons (list r1 loc r2) sets)))))))
	  (let ((bi 0))
	    (for-each 
	     (lambda (x)
	       (let ((str (object->string x :readable)))
		 (unless (morally-equal? x (catch #t (lambda () (eval-string str)) (lambda (type info) 'error)))
		   (set! baddies (+ baddies 1))
		   (format *stderr* "x: ~S~%" x)
		   (format *stderr* "ex: ~S~%" (catch #t (lambda () (eval-string str)) (lambda (type info) (apply format #f info))))
		   (format *stderr* "sets: ~S~%" (reverse sets))
		   (format *stderr* "str: ~S~%" str)
		   (pretty-print (with-input-from-string str read) *stderr* 0)
		   (format *stderr* "~%~%")
		   
		   (format *stderr* "
                     (let ((p (make-list 3 #f))
                           (v (make-vector 3 #f))
                           (cy (make-cycle #f))
                           (h (hash-table* 'a 1 'b 2 'c 3))
                           (e (inlet 'a 1 'b 2 'c 3))
                           (it (make-iterator (make-list 3 #f)))
                           (cp (c-pointer 1 (make-list 3 #f))))
                           ")
		   (for-each
		    (lambda (set)
		      (cond ((and (zero? (car set))
				  (> (cadr set) 2))
			     (format *stderr* "  (set-cdr! (list-tail p 2) ~A)~%" (#("p" "(cdr p)" "(cddr p)") (- (cadr set) 3))))
			    ((< (car set) 5)
			     (format *stderr* "  (set! (~A ~A) ~A)~%" 
				     (#(p v cy h e) (car set))
				     (case (car set) 
				       ((0 1) (cadr set))
				       ((2) 0)
				       ((3) (format #f "~W" (cadr set)))
				       ((4) (symbol->keyword (cadr set))))
				     (#(p v cy h e it cp) (caddr set))))
			      ((= (car set) 5)
			       (format *stderr* "  (set! ((iterator-sequence it) ~A) ~A)~%" (cadr set) (#(p v cy h e it cp) (caddr set))))
			      (else (format *stderr* "  (set! (((object->let cp) 'c-type) ~A) ~A)~%" (cadr set) (#(p v cy h e it cp) (caddr set))))))
		    sets)
		   (format *stderr* "  ~A)~%" (#(p v cy h e it cp) bi)))
		 (set! bi (+ bi 1))))
	     
	     bases)))))))

(tester)

  
  
(s7-version)
(exit)
