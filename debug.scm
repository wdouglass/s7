(provide 'debug.scm)

(let-temporarily (((*s7* 'debug) 0)) ; trace-in call should not be added to trace-in!
  
  (define trace-in 
    (let ((*debug-spaces* 0)
	  (*debug-max-spaces* (*s7* 'max-format-length))
	  (*debug-port* *stderr*)
	  (*debug-start-output* (lambda (p) #f))
	  (*debug-end-output* newline)
	  (*debug-stack* #f)
	  (*debug-function* #f)
	  (*debug-breaks* ())
	  (*debug-repl* (lambda (call e) #f))
	  (*debug-curlet* #f)
	  (report-return #f))
      
      (set! (setter '*debug-spaces*) integer?)
      (set! (setter '*debug-max-spaces*) integer?)
      (set! (setter '*debug-breaks*) list?)
      (set! (setter '*debug-start-output*) procedure?)
      (set! (setter '*debug-end-output*) procedure?)

      (set! (setter '*debug-port*) 
	    (lambda (s v) 
	      (if (or (output-port? v) (not v))
		  v 
		  (error 'wrong-type-arg "~S can't be set! to ~S" s v))))

      (set! (setter '*debug-stack*) 
	    (lambda (s v) 
	      (if (or (vector? v) (not v)) 
		  v 
		  (error 'wrong-type-arg "~S can't be set! to ~S" s v))))

      (set! (setter '*debug-function*) 
	    (lambda (s v)
	      (if (or (not v)
		      (and (procedure? v)
			   (= (car (arity v)) 3)))
		  v
		  (error 'wrong-type-arg "~S must be #f or a procedure of 3 arguments" s))))
      
      (define (trace-out e val)     ; report value returned
	(let-temporarily (((*s7* 'history-enabled) #f))
	  (set! *debug-spaces* (max 0 (- *debug-spaces* 2)))
	  ;(let-temporarily (((openlets) #f)) ; val local object->string might cause an infinite loop
	    (format *debug-port* "~NC  -> ~S" (min *debug-spaces* *debug-max-spaces*) #\space val);)
	    (*debug-end-output* *debug-port*)
	  val))

      (lambda (e)                   ; trace-in, report function call
	(let-temporarily (((*s7* 'history-enabled) #f))

	  (let* ((func (if (funclet? e) 
			   (with-let e __func__) 
			   (and (funclet? (outlet e))
				(with-let (outlet e) __func__))))
		 (funcname (if (pair? func) 
			       (car func)
			       (if (symbol? func)
				   func
				   #<lambda>)))
		 (args (let-temporarily (((*s7* 'debug) 0)) ; keep s7 from adding trace-in to this map function
			 (map (lambda (x) 
				(if (and (symbol? funcname)
					 (pair? (cdr x))
					 (equal? (arity (symbol->value funcname e)) (cons 0 536870912)))
				    (apply values (cdr x))
				    (if (and (or (pair? (cdr x))
						 (and (symbol? (cdr x))
						      (not (keyword? (cdr x)))))
					     (not (and func (macro? (symbol->value funcname e)))))
					(list 'quote (cdr x))
					(cdr x))))
			     e)))

		 (call (let-temporarily (((openlets) #f))  ; ignore local object->string methods
			 (format #f "(~S~{~^ ~S~})" 
				 funcname
				 args))))
	    
	    (set! *debug-curlet* e)

	    (if (vector? *debug-stack*)
		(vector-set! *debug-stack* (max 0 (min (- (length *debug-stack*) 1) (/ *debug-spaces* 2))) call))
	    
	    (set! report-return (or func (> (*s7* 'debug) 2)))
	    (when report-return
	      (cond ((and *debug-function*
			  (*debug-function* func args e))) ; if it returns #f, try rest of possibilities
			  
		    ((memq funcname *debug-breaks*)
		     (*debug-repl* call e))
			  
		    (else
		     (*debug-start-output* *debug-port*)
		     (format *debug-port* "~NC~A" (min *debug-spaces* *debug-max-spaces*) #\space call)
		     (if (pair? func)
			 (format *debug-port* "    ; ~S: ~A[~D]" (car func) (cadr func) (caddr func)))
		     (if (and (> (port-line-number) 0)
			      (not (string=? (port-filename) "*stdin*")))
			 (format *debug-port* "~A called from ~A[~D]"
				 (if (pair? func) "" "    ;")
				 (port-filename)
				 (port-line-number)))
		     (newline *debug-port*)))
	      (set! *debug-spaces* (+ *debug-spaces* 2)))))

	;; report value returned -- this needs to be at the top level in this function
	(when report-return
	  (dynamic-unwind trace-out e)))

      )))


(define debug-port (dilambda 
		    (lambda () 
		      ((funclet trace-in) '*debug-port*))
		    (lambda (new-port)
		      (set! ((funclet trace-in) '*debug-port*) new-port))))


;;; -------- trace
(define-bacro* (trace (func :unset))
  ;; a bacro because we can be called anywhere (repl top-level-let), we're being asked to trace something in that let probably,
  ;;   but outlet(trace), if trace is a macro, is (rootlet) if (load "debug.scm"), so trace thinks func is undefined.
  ;;   a macro might work if the entire body were returned, and a second quasiquote level used for the set!?
  (cond ((eq? func :unset)           ; (trace)
	 (set! (*s7* 'debug) 3))

	((if (symbol? func)          ; (trace clamp)
	     (not (defined? func))
	     (not (and (pair? func)  ; (trace (*lint* 'remove-if)
		       (pair? (cdr func))
		       (pair? (cadr func))
		       (eq? (caadr func) 'quote)
		       (defined? (cadadr func) (symbol->value (car func))))))
	 (error 'unbound-variable "~S is undefined\n" func))

	((let ((func-val (eval func)))
	   (not (or (procedure? func-val)
		    (macro? func-val))))
	 (error 'wrong-type-arg "~S is not a procedure or macro" func))

	(else
	 (let ((source (procedure-source (eval func))))                    ; (lambda (x) (+ x 1))
	   (if (pair? source)
	       (unless (and (pair? (caddr source))
			    (eq? (caaddr source) 'trace-in))
		 (let ((new-source (cons (car source)                      ; lambda
					 (cons (cadr source)               ; args
					       (cons '(trace-in (curlet))  ; (trace-in (curlet))
						     (cddr source))))))    ; body
		   `(set! ,func (let () 
				  (define ,(if (symbol? func) func (cadadr func))
				    ,new-source)))))
	       ;; we need to use define to get the function name saved for __func__ later, but
	       ;;   we also need to clobber the old definition (not just shadow it) so that existing calls
	       ;;   will be traced.  So, define it in a let returning its new value, setting the current one.
	       (let ((old-func (gensym)))
		 `(set! ,func (let () 
				(define ,(if (symbol? func) func (cadadr func))
				  (let ((,old-func ,func))
				    (lambda args 
				      (trace-in (curlet))
				      (apply ,old-func args))))))))))))

(define-bacro* (untrace (func :unset))
  (if (eq? func :unset)
      (set! (*s7* 'debug) 0)
      (let ((source (procedure-source (eval func))))
	(if (pair? source)
	    (when (and (pair? (caddr source))
		       (eq? (caaddr source) 'trace-in))
	      (let ((new-source (cons (car source)
				      (cons (cadr source)
					    (cdddr source)))))
		`(set! ,func (let () 
			       (define ,(if (symbol? func) func (cadadr func))
				 ,new-source)))))
	    `(set! ,func (let () 
			   (define ,func (with-let (unlet)
					   ,func))))))))


;;; -------- break
(define-macro (break func)
  `(if (defined? ',func)
       (begin
	 (trace ,func)
	 (set! ((funclet trace-in) '*debug-breaks*) (cons ',func ((funclet trace-in) '*debug-breaks*))))
       (error 'unbound-variable "~S is undefined\n" ',func)))

(define-macro* (unbreak (func :unset))
  (if (eq? func :unset)
      (set! ((funclet trace-in) '*debug-breaks*) ())
      `(set! ((funclet trace-in) '*debug-breaks*)
	     (let remove ((lst ((funclet trace-in) '*debug-breaks*)) (new-lst ()))
	       (if (null? lst)
		   (reverse new-lst)
		   (remove (cdr lst) 
			   (if (eq? (car lst) ',func) 
			       new-lst 
			       (cons (car lst) new-lst))))))))


;; -------- watch
(define-macro (watch var)   ; notification if var set!
  `(set! (setter ',var) 
      (lambda (s v e)
	(format (debug-port) "~S set! to ~S~A~%" s v 
                (let ((func (with-let e __func__)))
                  (if (eq? func #<undefined>) "" (format #f ", ~S" func))))
	v)))

(define-macro (unwatch var)
  `(set (setter ',var) #f))


(define (debug-stack)
  (let ((stack ((funclet trace-in) '*debug-stack*))
	(depth ((funclet trace-in) '*debug-spaces*)))
    (when stack
      (format (debug-port) "~NCstack:\n" depth #\space)
      (do ((i 0 (+ i 1))) 
	  ((or (= i (length stack))
	       (>= i (/ ((funclet trace-in) '*debug-spaces*) 2))
	       (not (string? (vector-ref stack i)))))
	(format (debug-port) "~NC~A~%" (+ depth 2) #\space (vector-ref stack i)))
      (newline (debug-port)))))


(define (frame n)
  (do ((p ((funclet trace-in) '*debug-curlet*) (outlet p))
       (i 0 (+ i 1)))
      ((or (= i n) (not (let? p)))
       (format (debug-port) "~S~%" p))))

#|
;; turn on the stack:
(set! ((funclet trace-in) '*debug-stack*) (make-vector 64 #f))

;; turn off the stack:
(set! ((funclet trace-in) '*debug-stack*) #f)

;; trace with a function to call at the trace point, rather than trace-in's code
(set! ((funclet trace-in) '*debug-function*) 
      (lambda (func args e) ...)) ; return non-#f 

;; trace just one function specially:
(set! ((funclet trace-in) '*debug-function*) 
      (lambda (func args e)
        (and (eq? func desired-func)
             ...)))

;; break one function specially, or break-if
;; use above (*debug-repl* call e))

;; log trace info
(call-with-output-file "test.log"
  (lambda (port)
    (let-temporarily ((((funclet trace-in) '*debug-port*) port))
      ...)))


;;; --------
;;; TODO:

;; untrace, break, unbreak, watch, and unwatch need to accept (*lib* 'name) func names
;;   also check dilambdas (protect setter) and (with-let l v) (not cadadr)
;;   watch should notice pre-exisiting setter and incorporate it

;; need repl indication of break depth

;; debug-stack in s7_error if debug.scm loaded, debug>1 and stack exists

;; unbound-var: check if len diff < cutoff (1-char name = no check) and first chars same, and include rootlet/syntax names, maybe transpose *-* and try again?

;; s7_error/ow! need to use sl_history not error-history, or perhaps cull_history

;; s7test for debug.scm
;; and s7.html all these cases, and how to tie into other code (repl.scm, Snd)
|#


(when (defined? 'debug.scm-init)
  ((symbol->value 'debug.scm-init)))
