;;; reactive.scm
;;;
;;; reimplementation of code formerly in stuff.scm

(define setter-print #f)


(define (gather-symbols expr ce lst ignore)
  ;; collect settable variables in expr
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


;;; c-pointer used to hold symbol+let info so that the lets can be a "weak references"
(define slot-symbol c-pointer-type)
(define slot-expr c-pointer-info)
(define slot-env c-pointer-weak1)
(define slot-expr-env c-pointer-weak2)
(define (slot symbol expr env expr-env) (c-pointer 0 symbol expr env expr-env))


(define (symbol->let symbol env)
  ;; return let in which symbol lives (not necessarily curlet)
  (if (not (let? env))
      #<undefined>
      (if (defined? symbol env)
	  env
	  (symbol->let symbol (outlet env)))))


(define (setter-update cp)            ; cp: (slot var expr env expr-env)
  ;; when var set, all other vars dependent on it need to be set also, watching out for GC'd followers
  (if setter-print (format *stderr* " -------- setter-update ~S~%" cp))
  (let ((var (slot-symbol cp))
	(env (slot-env cp))
	(expr (slot-expr cp)))
    (when (and (let? (slot-env cp))                  ; when slot-env is GC'd, the c-pointer field is set to #f (by the GC)
	       (let? (slot-expr-env cp)))
      (let ((new-val (eval expr (slot-expr-env cp))))
	(when (let? (slot-env cp))
	  (if setter-print (format *stderr* " -------- let-set ~S ~S~%" var new-val))
	  (let-set! env var new-val))))))


(define (slot-equal? cp1 cp2)
  (and (eq? (slot-symbol cp1) (slot-symbol cp2))
       (eq? (slot-env cp1) (slot-env cp2))))

(define (setter-remove cp lst)
  ;; if reactive-set! called again on a variable, its old setters need to remove the now obsolete set of that variable
  (if (null? lst)
      ()
      (if (slot-equal? cp (car lst))
	  (cdr lst)
	  (cons (car lst)
		(setter-remove cp (cdr lst))))))


(define* (make-setter var env (followers ()) (setters ()) (expr ()) expr-env)
  ;; return a new setter with closure containing the followers and setters of var, and the c-pointer holding its name, environment, and expression
  (if setter-print (format *stderr* " -------- make-setter ~S ~S ~S ~S ~S~%" var env followers setters expr))
  (let ((followers followers)
	(setters setters)
	(cp (slot var expr env expr-env)))
    (lambda (sym val)
      (if setter-print (format *stderr* " -------- setter ~S ~S ~S~%" sym val (*s7* 'stack-top)))
      ;(if (> (*s7* 'stack-top) 30) (abort))
      (let-temporarily (((setter (slot-symbol cp) (slot-env cp)) #f))
	(if setter-print (format *stderr* " -------- let-set ~S ~S: ~S in ~S~%" sym val (setter sym) (slot-env cp)))
	(let-set! (slot-env cp) (slot-symbol cp) val) ; set new value without retriggering the setter
	(for-each setter-update followers)            ; set any variables dependent on var
	val))))


(define-bacro (reactive-set! place value)             ; or maybe macro* with traling arg: (e (outlet (curlet)))??
  (with-let (inlet 'place place                       ; with-let here gives us control over the names
		   'value value 
		   'e (outlet (curlet)))              ; the run-time (calling) environment
    `(let ((old-followers ())
	   (old-setter (setter ',place))
	   (lt (symbol->let ',place ,e)))

       ;; if previous set expr, remove it from setters' followers lists
       (when old-setter
	 (set! old-followers ((funclet old-setter) 'followers))
	 (for-each (lambda (s)
		     (when (setter s)
		       (let ((setter-followers (let-ref (funclet (setter s)) 'followers)))
			 (let-set! (funclet (setter s))
				   'followers 
				   (setter-remove (slot ',place 0 lt ,e) setter-followers)))))
		   (let-ref (funclet old-setter) 'setters)))

       ;; set up new setter
       (let ((setters (gather-symbols ',value ,e () ()))
	     (expr (copy ',value :readable)))
	 (let ((cp (slot ',place expr lt ,e)))
	   (set! (setter ',place lt)
		 (make-setter ',place lt old-followers setters expr ,e))
       
	   ;; add the slot to the followers setter list of each variable in expr
	   (for-each (lambda (s)
		       (unless (setter s)
			 (set! (setter s) (make-setter s (symbol->let s ,e))))
		       (let ((setter-followers (let-ref (funclet (setter s)) 'followers)))
			 (unless (member cp setter-followers slot-equal?)
			   (let-set! (funclet (setter s))
				     'followers
				     (cons cp setter-followers)))))
		     setters)))
       (set! ,place ,value))))


;; --------------------------------------------------------------------------------
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

(format *stderr* "x setter: ~S ~S~%" (setter 'x) (funclet (setter 'x)))
(format *stderr* "a setter: ~S ~S~%" (setter 'a) (funclet (setter 'a)))
;; x setter: #<lambda (sym val)> (inlet 'followers () 'setters (b a) 'cp #<x (nil)>)
;; a setter: #<lambda (sym val)> (inlet 'followers (#<x (nil)>) 'setters () 'cp #<a (nil)>)

(reactive-set! a (* b 2))
(set! b 5)
(format *stderr* "x: ~A, a: ~A, b: ~A~%" x a b)
;; x: 15, a: 10, b: 5
)

(let ((x 0))
  (do ((i 0 (+ i 1)))
      ((= i 100))
    (let ((a 1))
      (reactive-set! x (* 2 a)))
    (let ((a 3))
      (set! a 2))
    (if (zero? (modulo i 10))
	(gc))))

(define-macro (test a b)
  ;(display a) (newline)
  `(if (not (equal? ,a ,b))
       (format *stderr* "~S -> ~S?~%" ',a ,b)))


(test (let ((a 1) (b 2) (c 3)) (reactive-set! a (+ b c)) (set! b 4) (set! c 5) a) 9)
(test (let ((a 1) (b 2) (c 3)) (reactive-set! b (+ c 4)) (reactive-set! a (+ b c)) (set! c 5) a) 14)
(test (let ((expr 21) (symbol 1)) (reactive-set! expr (* symbol 2)) (set! symbol 3) expr) 6)
(test (let ((a 21) (b 1)) (reactive-set! a (* b 2)) (set! b 3) a) 6)
(test (let ((s 21) (v 1)) (reactive-set! s (* v 2)) (set! v 3) s) 6)
(test (let ((a 21) (v 1)) (reactive-set! a (* v 2)) (set! v 3) a) 6)
(test (let ((symbol 21) (nv 1)) (reactive-set! symbol (* nv 2)) (set! nv 3) symbol) 6)
(test (let ((outer 0)) (let ((nv 21) (sym 1)) (let ((inner 1)) (reactive-set! nv (* sym 2)) (set! sym 3) nv))) 6)
(test (let ((a 1) (b 2)) (reactive-set! b (+ a 4)) (let ((a 10)) (set! a (+ b 5)) (list a b))) '(10 5))
(test (let ((a 1) (b 2)) (reactive-set! b (+ a 4)) (list (let ((b 10)) (set! a (+ b 5)) a) b)) '(15 19))

(test (let ((a 1) (b 2) (c 3)) (reactive-set! b (+ c 4)) (let ((a 0)) (reactive-set! a (+ b c)) (set! c 5) a)) 14)
(test (let ((a 1) (b 2) (c 3)) (reactive-set! a (reactive-set! b (+ c 4))) (list a b c)) '(7 7 3))
(test (let ((a 1) (b 2) (c 3)) (reactive-set! a (+ 1 (reactive-set! b (+ c 4)))) (list a b c)) '(8 7 3))

(test (let ((a 1) (x 0)) (reactive-set! x (* a 2)) (reactive-set! a (* x 2)) (set! x 2) a) 4)
(test (let ((a 1)) (let ((b 0) (c 0)) (reactive-set! b (* a 2)) (reactive-set! c (* a 3)) (let ((x 0)) (reactive-set! x (+ a b c)) (set! a 2) x))) 12)
(test (let ((x 0)) (let ((a 1)) (reactive-set! x (* 2 a)) (set! a 2)) x) 4)

;;; (define-macro (with-setters vars . body) `(let-temporarily (,(map (lambda (var) `((setter ',var) #f)) vars)) ,@body))

(let ((x 0))
  (do ((i 0 (+ i 1)))
      ((= i 100))
    (let ((a 1))
      (reactive-set! a (* 2 x))
      (set! x 2)
      (if (zero? (modulo i 10))
	  (gc)))))

(let ((x 0))
  (do ((i 0 (+ i 1)))
      ((= i 100))
    (let ((a 1))
      (reactive-set! x (* 2 a))
      (set! a 2))))

;; for this we need to store the old-setter (how to tell it is not one of ours?)
;  (test (let ((a 21) (b 1)) (set! (setter 'b) (lambda (x y) (* 2 y))) (reactive-set! a (* b 2)) (set! b 3) a) 12)
;  (test (let ((a 21) (b 1)) (set! (setter 'b) (lambda (x y) (* 2 y))) (let ((b 2)) (reactive-set! a (* b 2)) (set! b 3) a)) 6)

;; also place as generalized set: (reactive-set! (v 0) (* a 2)) -- does v get the setter?
