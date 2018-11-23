;;; check signatures

(set! (hook-functions *unbound-variable-hook*) ())
(set! (*s7* 'print-length) 6)
;(set! (*s7* 'gc-stats) #t)

(if (provided? 'snd)
    (begin
      (format *stderr* "this won't work in Snd!~%")
      (exit)))

(define max-args 3)
(define-constant one 1)

(define-constant auto-constants (list #f #t () #\a (/ most-positive-fixnum) (/ -1 most-positive-fixnum) 1.5+i
			"hi455" :key hi: 'hi (list 1) (list 1 2) (cons 1 2) (list (list 1 2)) (list (list 1)) (list ()) #() 
			1/0+i 0+0/0i 0+1/0i 1+0/0i 0/0+0i 0/0+0/0i 1+1/0i 0/0+i cons ''2 
			1+i 1+1e10i 1e15+1e15i 0+1e18i 1e18 #\xff (string #\xff) 1e308 
			most-positive-fixnum most-negative-fixnum (- most-positive-fixnum 1) (+ most-negative-fixnum 1)
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
			(make-vector '(2 3) "hi") #("hiho" "hi" "hoho") (subvector (make-int-vector '(2 3) 1) '(6))
			(subvector (subvector (make-float-vector '(2 3) 1.0) '(6)) '(2 2))
			(vector-ref #2d((#i(1 2 3)) (#i(3 4 5))) 0 0) (define-macro (m a) `(+ ,a 1))
			(c-pointer 0) (c-pointer -1) :readable :else (define-bacro* (m (a 1)) `(+ ,a 1))
			(byte-vector 0 1 2) (byte-vector) (byte-vector 255 0 127) (make-iterator #((a . 2)))
			(lambda (dir) 1.0) (float-vector) (make-float-vector '(2 32)) 
			'((a . 1)) #i(1) '((((A . B) C . D) (E . F) G . H) ((I . J) K . L) (M . N) O . P)
			#u(0 1 2) (openlet (inlet 'abs (lambda (x) (- x))))
			(make-iterator (list 1 2 3)) (make-iterator "1") #<eof> #r2d((.1 .2) (.3 .4))
			(dilambda (lambda () 1) (lambda (a) a))
			(gensym)
			))
      
(define car-auto-constants (car auto-constants))
(define-constant cdr-auto-constants (cdr auto-constants))

(define low 0)
(define-constant auto-arglists (vector (make-list 1) (make-list 2) (make-list 3) (make-list 4) (make-list 5) (make-list 6)))

(define (any? f sequence)
  (and (pair? sequence)
       (or (f (car sequence))
	   (any? f (cdr sequence)))))

(define (sig-ok? val typer)
  (if (pair? typer)
      (any? (lambda (ok?) 
	      (or (memq ok? '(#t values))
		  ((symbol->value ok?) val)))
	    typer)
      (or (memq typer '(#t values))
	  ((symbol->value typer) val))))

(define last-name1 "")
(define last-name2 "")
(define last-name3 "")
(define last-name4 "")

(define match-cases (list
	caar cdar cadr cddr
	caaar cdaar cddar cadar cdddr caadr cdadr caddr
	caaaar caadar cadaar caddar cdaaar cdadar cddaar cdddar cddddr caaddr cadadr cddar cdaadr caaadr cddadr cadddr cdaddr
	string->symbol ; "" as arg
	list->string   ; '(1) -- 1 is not a char
	make-hash-table make-weak-hash-table ; pair of procedures
	sublet          ; pair of symbol/value
	object->string  ; wrong key
	atan            ; x real if y
	make-int-vector make-float-vector make-vector make-byte-vector ; improper pair for dims
	setter
	call-with-output-string with-output-to-string call/cc ; cons etc
	inlet           ; pair but not (sym . val)
	reverse!        ; improper-list but its generic
	inexact->exact  ; NaN
	))

(define mismatch-cases (list setter))
(define doc-cases (list any? sig-ok?))

(define-constant (autotest name func args args-now args-left sig)
  (unless (negative? args-left)
    (let ((c-args (vector-ref auto-arglists args-now)))
      (copy args c-args)
      (let ((p (list-tail c-args args-now)))
	(call-with-exit
	 (lambda (quit)
	   (catch #t 
	     (lambda () 
	       (let ((val (apply func args)))
		 (when (pair? sig)
		   (unless (string=? name last-name3)
		     (unless (sig-ok? val (car sig))
		       (set! last-name3 name)
		       (format *stderr* "got ~S from (~A~{~^ ~S~}), (sig: ~S)~%" val name args sig)))
		   (unless (or (string=? name last-name2)
			       (memq func mismatch-cases))
		     (for-each 
		      (lambda (arg typer)
			(unless (sig-ok? arg typer)
			  (set! last-name2 name)
			  (format *stderr* "mismatch? ~S in (~A~{~^ ~S~}), (sig: ~S in ~S)~%" arg name args typer sig)))
		      args (cdr sig))))))
	     (lambda (type info)
	       (let ((err (apply format #f info)))
		 (unless (string=? name last-name4)
		   (unless (string-position name err)
		     (set! last-name4 name)
		     (format *stderr* "~S ~S ~S~%     ~S from ~S~%" name type (apply format #f info) info args))))
	       (when (and (not (string=? name last-name1))
			  (not (memq func match-cases))
			  (pair? args)
			  (pair? sig)
			  (eq? type 'wrong-type-arg))
		 (let ((pos (- (length args) 1)))
		   (when (sig-ok? (list-ref args pos) (list-ref (cdr sig) pos))
		     (set! last-name1 name)
		     (format *stderr* "incorrect match? ~S in (~A~{~^ ~S~}), (sig: ~S in ~S)~%" 
			     (list-ref args pos) name args (list-ref (cdr sig) pos) sig)))
		 (quit))
	       (if (and (positive? args-now)
			(memq type '(wrong-type-arg wrong-number-of-args out-of-range syntax-error io-error
				     division-by-zero format-error missing-method error invalid-escape-function)))
		   (quit))))
	   (let ((args-now1 (+ args-now 1))
		 (args-left1 (- args-left 1)))
	     (for-each
	      (lambda (c)
		(set-car! p c)
		(autotest name func c-args args-now1 args-left1 sig))
	      auto-constants))))))))

(define baddies '(exit emergency-exit abort autotest s7-optimize
		  all delete-file system set-cdr! stacktrace test-sym
		  cutlet varlet gc cond-expand reader-cond
		  openlet coverlet eval vector list cons hash-table* hash-table values
		  symbol-table load 
		  global-environment current-environment make-rectangular
		  copy fill! hash-table-set! vector-set! let-set! list-values apply-values immutable!
		  mock-number mock-pair mock-string mock-char mock-vector mock-hash-table*
		  mock-symbol mock-port mock-hash-table m mock->string make-local-method
		  *mock-number* *mock-pair* *mock-string* *mock-char* *mock-vector*
		  *mock-symbol* *mock-port* *mock-hash-table* mp ms mv
		  *unbound-variable-hook* *load-hook* *rootlet-redefinition-hook* *missing-close-paren-hook* *read-error-hook*
		  tree-count ; signature is kinda silly here
		  error throw call-with-current-continuation call/cc *s7*
		  / call-with-exit read-string *error-hook* make-hook hook-functions symbol vector-append append apply
		  c-define-1 apropos map-values
		  outlet-member make-method make-object))

(define (test-sym sym)
  (if (and (not (memq sym baddies))
	   (defined? sym))
      (let* ((f (symbol->value sym))
	     (strname (symbol->string sym))
	     (argn (and (or (procedure? f) (let? f)) (arity f))))
	(when argn
	  (unless (memq f doc-cases)
	    (let ((doc (documentation f)))
	      (if (or (not doc)
		      (string=? doc ""))
		  (format *stderr* "~S documentation: ~S~%" sym doc)
		  (unless (string-position strname doc)
		    (format *stderr* "~S: doc: ~S~%" sym doc)))))
	  (let ((bottom (car argn))
		(top (min (cdr argn) max-args)))
	    (if (not (memv (strname 0) '(#\{ #\[ #\()))
		(begin
		  (format *stderr* "-------- ~A --------~%" sym)
		  (if (< top bottom)
		      (format *stderr* ";~A (bottom: ~A, top: ~A)...~%" sym bottom top))
					;(format *stderr* ";~A...~%" sym))
		  (set! low bottom)
		  (if (positive? (cdr argn))
		      (autotest strname f () 0 top (signature f))))))))))

(define (all)
  (let ((st (symbol-table)))
    (for-each test-sym st)
    ;(test-sym 'object->string)
    (format *stderr* "~%all done~%")
    ))

(all)

(s7-version)
(exit)
