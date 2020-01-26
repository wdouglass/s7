(provide 'debug.scm)

(let-temporarily (((*s7* 'debug) 0)) ; trace-in call should not be added to trace-in!
  
  (define trace-in 
    (let ((*debug-spaces* 0)
	  (*debug-max-spaces* (*s7* 'max-format-length))
	  (*debug-port* *stderr*)
	  (*debug-stack* #f)
	  (*debug-function* #f)
	  (*debug-breaks* ())
	  (*debug-repl* (lambda (call e) #f)))
      
      (set! (setter '*debug-spaces*) integer?)
      (set! (setter '*debug-max-spaces*) integer?)
      (set! (setter '*debug-breaks*) list?)

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
      
      (define (trace-out e val)     ; report value returned if debug>1
	(let-temporarily (((*s7* 'history-enabled) #f))
	  (set! *debug-spaces* (max 0 (- *debug-spaces* 2)))
	  (let-temporarily (((openlets) #f)) ; val local object->string might cause an infinite loop
	    (format *debug-port* "~NC  -> ~S~%" (min *debug-spaces* *debug-max-spaces*) #\space val))
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
					     (or (not func)
						 (not (macro? (symbol->value funcname e)))))
					(list 'quote (cdr x))
					(cdr x))))
			     e)))

		 (call (let-temporarily (((openlets) #f))  ; ignore local object->string methods
			 (format #f "(~S~{~^ ~S~})" 
				 funcname
				 args))))
	    
	    (when (or func (> (*s7* 'debug) 2))
	      (cond (*debug-function*
		     (*debug-function* func args e))
			  
		    ((memq funcname *debug-breaks*)
		     (*debug-repl* call e))
			  
		    (else
		     (format *debug-port* "~NC~A" (min *debug-spaces* *debug-max-spaces*) #\space call)
		     (if (pair? func)
			 (format *debug-port* "    ; ~S: ~A[~D]" (car func) (cadr func) (caddr func)))
		     (if (and (> (port-line-number) 0)
			      (not (string=? (port-filename) "*stdin*")))
			 (format *debug-port* "~A called from ~A[~D]"
				 (if (pair? func) "" "    ;")
				 (port-filename)
				 (port-line-number)))
		     (newline *debug-port*))))

	    (if *debug-stack*
		(vector-set! *debug-stack* (max 0 (min (- (length *debug-stack*) 1) (/ *debug-spaces* 2))) call))))

	(when (> (*s7* 'debug) 1)  ; report value returned, this needs to be at the top level in this function
	  (set! *debug-spaces* (+ *debug-spaces* 2))
	  (dynamic-unwind trace-out e))

	))))

(when (defined? 'debug.scm-init)
  ((symbol->value 'debug.scm-init)))

#|
;; debug=1 trace named function entry, debug=2 add return value, debug=3 add unnamed functions

;; set the output port:
(set! ((funclet trace-in) '*debug-port*) new-port)

;; turn on the stack:
(set! ((funclet trace-in) '*debug-stack*) (make-vector 64 #f))

;; turn off the stack:
(set! ((funclet trace-in) '*debug-stack*) #f)

;; display the stack:
(let ((stack ((funclet trace-in) '*debug-stack*)))
  (when stack
    (do ((i 0 (+ i ))) 
        ((or (= i (length stack))
             (>= i (/ *debug-spaces 2))
             (not (string? (vector-ref stack i)))))
      (display (vector-ref stack i))
      (newline))))

;; trace with a function to call at the trace point (passed func+loc? args? curlet), rather than trace-in's code
(set! ((funclet trace-in) '*debug-function*) (lambda (func args e) ...))

;; add a function to the break-list (function is a symbol here)
(set! ((funclet trace-in) '*debug-breaks*) (cons function ((funclet trace-in) '*debug-breaks*)))

;; turn off all breaks
(set! ((funclet trace-in) '*debug-breaks*) ())

;; log trace info
(call-with-output-file "test.log"
  (lambda (port)
    (let-temporarily ((((funclet trace-in) '*debug-port*) port))
      ...)))

;; (trace func) -- trace one function (debug=0)
(define-macro (trace func)
  ;; to get the return value: (let-temporarily (((*s7* 'debug) 2)) (func))
  (let ((source (procedure-source func)))
    (if (pair? source)
	(unless (and (pair? (car source))
		     (eq? (caar source) 'trace-in))
	  (let ((new-source (cons (car source) 
				  (cons (cadr source)
					(cons '(trace-in (curlet))
					      (cddr source))))))
	    `(set! ,func ,new-source)))
	(let ((old-func (gensym)))
	  `(define ,func 
	     (let ((,old-func ,func))
	       (lambda args 
		 (trace-in (curlet))
		 (apply ,old-func args))))))))

;; watch var (notification if var set!)
(define-macro (watch var)
  `(set! (setter ',var) 
      (lambda (s v e)
	(format *stderr* "~S set! to ~S~A~%" s v 
          (let ((func (with-let e __func__)))
            (if (eq? func #<undefined>) "" (format #f ", ~S" func))))
	v)))



;;; --------
;;; TODO:

;; tie into Snd repl
;;   this requires set-prompt at least, and the hello/goodbye message
;;   in Snd I think that's all: just set prompt, how to see C-q=play current channel?
;;      maybe (go)? (break-out)?
;;   also how to set Snd eval environment (for break)?
;;      g_set_top_level_let in snd-glistener|motif = (top-level-let) in scheme
;;   so Snd break-out=go?
;;   see debug.scm-init repl.scm -- need Snd version

;; step: save old begin_hook, set to drop into repl as above, restore on exiting stepper (CM-q?)
;;   no way to get the true current code? cur_code is set just before the begin_hook call, but we don't see it in time!
;;   how to use begin_hook from scheme? (isn't sc->code correct at that point? or cur_code I think if called in C)
;;   so *begin-hook* would grab sc->code, call hook funcs with that arg, drop into debug-repl until next step
;;   void debug_begin_hook(s7_scheme *sc, bool *res)
;;     s7_pointer call = sc->code; /* or current_code(sc) perhaps */
;;     call hook code
;;     return(true); I think

;; break-if via *debug_function*
|#
