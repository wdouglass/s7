(provide 'debug.scm)

(let-temporarily (((*s7* 'debug) 0)) ; trace-in call should not be added to trace-in!
  
  (define trace-in 
    (let ((*debug-spaces* 0)
	  (*debug-max-spaces* (*s7* 'max-format-length))
	  (*debug-port* *stderr*)
	  (*debug-stack* #f)
	  (*debug-function* #f)
	  (*debug-breaks* ())
	  (*debug-repl* (lambda (call e) #f))
	  (report-return #f))
      
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
      
      (define (trace-out e val)     ; report value returned
	(let-temporarily (((*s7* 'history-enabled) #f))
	  (set! *debug-spaces* (max 0 (- *debug-spaces* 2)))
	  ;(let-temporarily (((openlets) #f)) ; val local object->string might cause an infinite loop
	    (format *debug-port* "~NC  -> ~S~%" (min *debug-spaces* *debug-max-spaces*) #\space val);)
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
	    
	    (if *debug-stack*
		(vector-set! *debug-stack* (max 0 (min (- (length *debug-stack*) 1) (/ *debug-spaces* 2))) call))
	    
	    (set! report-return (or func (> (*s7* 'debug) 2)))
	    (when report-return
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
		     (newline *debug-port*)))
	      (set! *debug-spaces* (+ *debug-spaces* 2)))))

	;; report value returned -- this needs to be at the top level in this function
	(when report-return
	  (dynamic-unwind trace-out e)))

      )))

(define* (step (n 1))
  (set! (*s7* 'step-function) 
	(lambda (code) 
	  (format *debug-port* "step: ~S~%" code) 
	  (*debug-repl* code (curlet))))
  (set! (*s7* 'stepping) n)
  (set! ((*repl* 'repl-let) 'all-done) #t)) ; TODO: need this for each repl


(define-macro (break func)
  `(set! ((funclet trace-in) '*debug-breaks*) (cons ',func ((funclet trace-in) '*debug-breaks*))))

(define (clear-breaks)
  ;; turn off all breaks
  (set! ((funclet trace-in) '*debug-breaks*) ()))


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


(define (set-debug-port new-port)
  (set! ((funclet trace-in) '*debug-port*) new-port))


(when (defined? 'debug.scm-init)
  ((symbol->value 'debug.scm-init)))



#|
;; debug=1 trace every named function or macro entry, debug=3 also trace unnamed functions

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

;; log trace info
(call-with-output-file "test.log"
  (lambda (port)
    (let-temporarily ((((funclet trace-in) '*debug-port*) port))
      ...)))


;;; --------
;;; TODO:

;; tie into Snd repl
;;   this requires set-prompt at least, and the hello/goodbye message
;;   in Snd I think that's all: just set prompt, how to see C-q=play current channel?
;;      maybe (go)? (break-out)?
;;   also how to set Snd eval environment (for break)?
;;      g_set_top_level_let in snd-glistener|motif = (top-level-let) in scheme
;;   so Snd break-out=go?

;; step doesn't connect yet, or maybe my test didn't hit begin_hook
;; (frame n): move outlet n-from-bottom, set as curlet (but remember the starting curlet!)

;;   (continue): (set! stepping 0) or break
;;      but while in repl, eval should turn off the step and break 

;; break-if via *debug_function*
;; how to omit (let-temp debug 0) from history: if enabled changes to 0, look for let-temp preceding and backup cur_code?
;; (untrace) -- if debug<0?
|#
