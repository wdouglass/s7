;;; reactive.scm
;;;
;;; reimplementation of code formerly in stuff.scm

;;; symbol-setter will soon be just setter (I need to combine them in s7)

(define *gc-counter* 0)

(set! (hook-functions *after-gc-hook*)
      (cons (lambda (hook)
	      (set! *gc-counter* (+ *gc-counter* 1)))
	    (hook-functions *after-gc-hook*)))


(define (gather-symbols expr ce lst ignore)
  (cond ((symbol? expr)
	 (if (or (memq expr lst)
		 (memq expr ignore)
		 (procedure? (symbol->value expr ce))
		 (eq? (let symbol->let ((sym expr)
					(ce ce))
			(if (defined? sym ce #t)
			    ce
			    (and (not (eq? ce (rootlet)))
				 (symbol->let sym (outlet ce)))))
		      (rootlet)))
	     lst
	     (cons expr lst)))

	((not (pair? expr)) lst)

	((not (and (pair? (cdr expr)) (pair? (cddr expr))))
	 (gather-symbols (cdr expr) ce (gather-symbols (car expr) ce lst ignore) ignore))

	((pair? (cadr expr))
	 (gather-symbols (case (car expr)
			   ((let let* letrec letrec* do)
			    (values (cddr expr) ce lst (append ignore (map car (cadr expr)))))
			   ((lambda) 
			    (values (cddr expr) ce lst (append ignore (cadr expr))))
			   ((lambda*)
			    (values (cddr expr) ce lst (append ignore (map (lambda (a) (if (pair? a) (car a) a)) (cadr expr)))))
			   (else
			    (values (cdr expr) ce (gather-symbols (car expr) ce lst ignore) ignore)))))

	((and (eq? (car expr) 'lambda)
	      (symbol? (cadr expr)))
	 (gather-symbols (cddr expr) ce lst (append ignore (list (cadr expr)))))

	(else 
	 (gather-symbols (cdr expr) ce (gather-symbols (car expr) ce lst ignore) ignore))))


(define (setter-update cp)            ; cp: (c-pointer 0 var expr env)
  (let ((var (c-pointer-type cp))
	(env (c-pointer-unmarked cp))
	(counter *gc-counter*))
    (when (and (let? env)
	       (type-of (symbol->value var env))) ; not #f == free
      (let ((expr (c-pointer-info cp)))
	(let ((new-val (eval expr env)))
	  (when (or (= counter *gc-counter*)
		    (and (let? env)
			 (type-of (symbol->value var env))))
	    (let-set! env var new-val)))))))


(define (cp-equal? cp1 cp2)
  (and (eq? (c-pointer-type cp1) (c-pointer-type cp2))
       (eq? (c-pointer-unmarked cp1) (c-pointer-unmarked cp2))))

(define (setter-remove cp lst)
  (if (null? lst)
      ()
      (if (cp-equal? cp (car lst))
	  (cdr lst)
	  (cons (car lst)
		(setter-remove cp (cdr lst))))))


(define-macro (with-immutable sym . body)
  (let ((mut (gensym)))
    `(let ((,mut (immutable? ,sym)))
       (dynamic-wind
	   (lambda () 
	     (immutable! ,sym))
	   (lambda ()
	     ,@body)
	   (lambda ()
	     (unless ,mut
	       (mutable! ,sym))))))) ; mutable! will go away someday (when with-immutable moves to s7)


(define setter-print #f)

(define* (make-setter var env (followers ()) (setters ()) (expr ()))
  (if setter-print (format *stderr* " -------- make-setter ~S ~S ~S ~S ~S~%" var env followers setters expr))
  (let ((followers followers)
	(setters setters)
	(cp (c-pointer 0 var expr env)))
    (lambda (sym val)
      (if setter-print (format *stderr* " -------- setter ~S ~S~%" sym val))
      (with-let (sublet env :sym sym :val val :followers followers)
	(let ((result
	       (let-temporarily (((symbol-setter sym) #f))
		 (let-set! (outlet (curlet)) sym val)    ; set new value without retriggering the setter
		 (with-immutable sym       ; block circular dependencies
		   (for-each setter-update followers))
		 val)))
	  (if setter-print (format *stderr* " -------- return ~S from ~S~%" val (symbol-setter sym)))
	  result)))))


(define-bacro (reactive-set! place value)            ; or maybe macro* with traling arg: (e (outlet (curlet)))??
  (with-let (inlet 'place place                      ; with-let here gives us control over the names
		   'value value 
		   'e (outlet (curlet)))             ; the run-time (calling) environment
    `(let ((old-followers ())
	   (old-setter (symbol-setter ',place)))

       ;; if previous set expr, remove it from setters' followers lists
       (when old-setter
	 (set! old-followers ((funclet old-setter) 'followers))
	 (for-each (lambda (setter)
		     (let ((setter-followers (let-ref (funclet (symbol-setter setter)) 'followers)))
		       (let-set! (funclet (symbol-setter setter))
				 'followers 
				 (setter-remove (c-pointer 0 ',place 0 ,e) setter-followers)))
		     (let-ref (funclet old-setter) 'setters))))

       ;; set up new setter
       (let ((setters (gather-symbols ',value ,e () ()))
	     (expr (copy ',value :readable)))
	 (let ((cp (c-pointer 0 ',place expr ,e)))
	   (set! (symbol-setter ',place) 
		 (make-setter ',place ,e
			      :followers old-followers
			      :setters setters
			      :expr expr))
       
	   ;; add (place . e) to the followers symbol-setter list of each variable in expr
	   (for-each (lambda (setter)
		       (unless (symbol-setter setter)
			 (set! (symbol-setter setter) (make-setter setter ,e)))
		       (let ((setter-followers (let-ref (funclet (symbol-setter setter)) 'followers)))
			 (unless (member cp setter-followers cp-equal?)
			   (let-set! (funclet (symbol-setter setter))
				     'followers
				     (cons cp setter-followers)))))
		     setters)))
       (if setter-print (format *stderr* " -------- set~%"))
       (set! ,place ,value))))


(let ()
(define a 2)
(define b 1)
(define x 0)
(if setter-print (format *stderr* " -------- reactive-set...~%"))
(reactive-set! x (+ a b))

(set! a 3)
(format *stderr* "x: ~A~%" x)
(set! b 4)
(format *stderr* "x: ~A~%" x)

(format *stderr* "x setter: ~S~%" (symbol-setter 'x))
(format *stderr* "a setter: ~S~%" (symbol-setter 'a))
)

#|
;;; save/restore setters -- can let-temporarily be used for this?
(define-macro (with-accessors vars . body)
  `(let ((accessors ()))
     (dynamic-wind
	 (lambda ()
	   (set! accessors (map symbol-setter ',vars)))
	 (lambda ()
	   ,@body)
	 (lambda ()
	   (for-each
	    (lambda (var accessor)
	      (set! (symbol-setter var) accessor))
	    ',vars accessors)))))
|#
