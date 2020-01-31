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
	  (*debug-curlet* #f) ; currently just for the frame macro
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
	  (let-temporarily (((openlets) #f)) ; val local object->string might cause an infinite loop
	    (format *debug-port* "~NC  -> ~S" (min *debug-spaces* *debug-max-spaces*) #\space val))
	  (*debug-end-output* *debug-port*)
	  val))

      (lambda (e)                   ; trace-in, report function call
	(let-temporarily (((*s7* 'history-enabled) #f))

	  (let* ((func (if (funclet? e) 
			   (with-let e __func__) 
			   (and (funclet? (outlet e))
				(with-let (outlet e) __func__))))
		 (funcname (if (pair? func) (car func) func))
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
				 (or funcname '<lambda>)
				 args))))
	    
	    (set! *debug-curlet* e)

	    (if (vector? *debug-stack*)
		(vector-set! *debug-stack* (max 0 (min (- (length *debug-stack*) 1) 
						       (/ *debug-spaces* 2)))
			     call))
	    
	    (set! report-return (or func (> (*s7* 'debug) 2)))

	    (when report-return
	      (cond ((and *debug-function*
			  (*debug-function* func call e))) ; if it returns #f, try rest of possibilities
			  
		    ((and funcname 
			  (memq funcname *debug-breaks*))
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
  (if (eq? func :unset)
      (set! (*s7* 'debug) 3)
      (catch #t
	(lambda ()

	  (let ((func-val (eval func)))         ; func can be arbitrarily complex
	   (if (not (or (procedure? func-val)
			(macro? func-val)))
	       (format (debug-port) "trace: ~S is not a procedure or macro" func)

	       (let ((func-name (object->string func-val))
		     ;; the name is usually a string internally, so this isn't as wasteful as it appears
		     (source (procedure-source func-val))                        ; (lambda (x) (+ x 1))
		     (setf (setter func-val)))                                   ; preseve possible setter
		 (set! func-name (if (char=? (func-name 0) #\#) '<lambda> (symbol func-name)))
		 
		 (if (pair? source)
		     (unless (and (pair? (caddr source))
				  (eq? (caaddr source) 'trace-in))
		       (let ((new-source (cons (car source)                      ; lambda
					       (cons (cadr source)               ; args
						     (cons '(trace-in (curlet))  ; (trace-in (curlet))
							   (cddr source)))))     ; body
			     (out (outlet (funclet func-val))))                  ; preserve possible local closure
			 (if setf
			     `(set! ,func (with-let ,out (dilambda (let () (define ,func-name ,new-source)) ,setf)))
			     `(set! ,func (with-let ,out (let () (define ,func-name ,new-source)))))))
		     ;; we need to use define to get the function name saved for __func__ later, but
		     ;;   we also need to clobber the old definition (not just shadow it) so that existing calls
		     ;;   will be traced.  So, use a redundant define in a let returning its new value, setting the current one.
		     
		     (let ((old-func (gensym)))
		       (if setf
			   `(set! ,func (dilambda 
					 (let () 
					   (define ,func-name
					     (let ((,old-func ,func))
					       (lambda args 
						 (trace-in (curlet))
						 (apply ,old-func args)))))
					 ,setf))
			   `(set! ,func (let () 
					  (define ,func-name
					    (let ((,old-func ,func))
					      (lambda args 
						(trace-in (curlet))
						(apply ,old-func args)))))))))))))
	(lambda (type info)
	  (format (debug-port) "can't trace ~S: ~S~%" func (apply format #f info))))))
	  

;;; -------- untrace
(define-bacro* (untrace (func :unset))
  (if (eq? func :unset)
      (set! (*s7* 'debug) 0)

      (catch #t
	(lambda ()

	  (let ((func-val (eval func)))         ; func can be arbitrarily complex
	   (if (not (or (procedure? func-val)
			(macro? func-val)))
	       (format (debug-port) "untrace: ~S is not a procedure or macro" func)

	       (let ((func-name (symbol (object->string func-val)))
		     (source (procedure-source func-val))
		     (setf (setter func-val)))
		 
		 (if (pair? source)
		     (when (and (pair? (caddr source))
				(eq? (caaddr source) 'trace-in))
		       (let ((cdddr-source (cdddr source)))
			 (if (and (eq? (caar cdddr-source) 'apply)
				  (gensym? (cadar cdddr-source)))
			     (let ((orig-func (symbol->value (cadar cdddr-source) (funclet func-val))))
			       (if setf
				   `(set! ,func (dilambda ,orig-func ,setf))
				   `(set! ,func ,orig-func)))
			     (let ((new-source (cons (car source)
						     (cons (cadr source)
							   cdddr-source)))
				   (out (outlet (funclet func-val))))
			       (if setf
				   `(set! ,func (with-let ,out
						  (dilambda (let () 
							      (define ,func-name
								,new-source))
							    ,setf)))
				   `(set! ,func (with-let ,out
						  (let () 
						    (define ,func-name
						      ,new-source))))))))))))))
	(lambda (type info)
	  (format (debug-port) "can't untrace ~S: ~S~%" func (apply format #f info))))))


;;; -------- break
(define-bacro (break func) 
  (let ((func-val (eval func)))
    (if (not (or (procedure? func-val)
		 (macro? func-val)))
	(format (debug-port) "break: ~S is not a procedure or macro" func)
	(let ((func-name (symbol (object->string func-val))))
	  (unless (memq func-name ((funclet trace-in) '*debug-breaks*))
	    `(begin
	       (trace ,func)
	       (set! ((funclet trace-in) '*debug-breaks*) (cons ',func-name ((funclet trace-in) '*debug-breaks*)))))))))

;;; -------- unbreak
(define-bacro* (unbreak (func :unset))
  (if (eq? func :unset)
      (set! ((funclet trace-in) '*debug-breaks*) ())
      (let ((func-name (symbol (object->string (eval func)))))
	`(set! ((funclet trace-in) '*debug-breaks*)
	       (let remove ((lst ((funclet trace-in) '*debug-breaks*)) (new-lst ()))
		 (if (null? lst)
		     (reverse new-lst)
		     (remove (cdr lst) 
			     (if (eq? (car lst) ',func-name) 
				 new-lst 
				 (cons (car lst) new-lst)))))))))


;;; -------- watch
(define-macro (watch var)   ; notification if var set!
  (if (pair? var)
      `(with-let ,(car var) 
	 (set! (setter ,(cadr var))
	       (lambda (s v)
		 (format (debug-port) "let-set! ~S to ~S~%" s v)
		 v)))
      `(set! (setter ',var) 
	     (lambda (s v e)
	       (format (debug-port) "~S set! to ~S~A~%" s v
		       (if (let? e) ; might be (rootlet) == ()
			   (let ((func (with-let e __func__)))
			     (if (eq? func #<undefined>) "" (format #f ", ~S" func)))
			   ""))
	       v))))

;;; -------- unwatch
(define-macro (unwatch var)
  (if (pair? var)
      `(with-let ,(car var) 
	 (set! (setter ,(cadr var)) #f))
      `(set! (setter ',var) #f)))


;;; -------- stack
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


;;; -------- frame
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
      (lambda (func call e) ...)) ; return non-#f 

;; trace one function specially:
(set! ((funclet trace-in) '*debug-function*) 
      (lambda (func call e)             ; if this returns #f, trace-in goes on as normally
        (and (eq? func desired-func)
             ... -> #t)))

;; break one function specially, or break-if ; this is as above but (*debug-repl* call e)
;; use above (*debug-repl* func call e))

;; log trace info
(call-with-output-file "test.log"
  (lambda (port)
    (let-temporarily ((((funclet trace-in) '*debug-port*) port))
      ...)))


;;; --------
;;; TODO:

;; unbreak should undo trace if it added the trace?
;; watch should notice pre-exisiting setter and incorporate it (as in trace/untrace)
;; (watch (*s7* 'debug))? this is let-set-fallback for *s7* [can't trace/break these either]

;; need repl indication of break depth: let *debug-repl* track this?

;; debug-stack in s7_error if debug.scm loaded, debug>1 and stack exists
;; if sc->debug>1, we know trace-in is loaded, so closure_let(symbol->value(sc, make_symbol(sc, "trace-in"))) has *debug-stack* etc

;; s7_error/ow! need to use sl_history not error-history, or perhaps cull_history

;; s7test for debug.scm -- t253, or maybe just append here with a switch for testing
;; and s7.html all these cases, and how to tie into other code (repl.scm, Snd)

;; how to trace expansions?
;;   if debug>3, expansion in eval could insert (if ((*s7* 'debug) > 0) (format (debug-port) "line ~D, ~S expanded to ~S" (port-line-number) etc)
;;   op_expansion
;;   or add this code to the expansion body as with a macro? currently if does, but then fails, (see t264)
;;       expansion name is missing and code is not fully expanded: * argument 2, (+ 2 1), is a pair but should be a number
;; macros also fail, same error [op_eval_macro and op_finish_expansion]
;; these seem to be ok if handled by s7, not by the trace macro?? but s7 does not annotate expansions itself 
;; need to test define*/define-macro* and bacros
;; expansion needs trace-in in the expanded code, whereas a macro is ok if precedes it

;; see func8: trace displays (func8 1 2 3) as (func8 1 '(2 3))
;; need t725-style tests of these macros
;; t264 -> s7test [arity etc]
;; trace mac should copy pair_macro at least, (define mac2 (macro ...)) should fixup the info?
;; s7.html: macro et al, also index bacro?
;; op_macro_unchecked?
|#


(when (defined? 'debug.scm-init)
  ((symbol->value 'debug.scm-init)))
