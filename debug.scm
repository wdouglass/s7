(provide 'debug.scm)

(let-temporarily (((*s7* 'debug) 0)) ; trace-in call should not be added to trace-in!

  (define trace-in 
    (let ((*debug-spaces* 0))

      (define (trace-out e val)     ; report value returned if debug>1
	(set! *debug-spaces* (max 0 (- *debug-spaces* 2)))
	(let-temporarily (((openlets) #f)) ; val local object->string might cause an infinite loop
	  (format *stderr* "~NC  -> ~S~%" *debug-spaces* #\space val))
	val)

      (lambda (e)                   ; trace-in, report function call
	(let-temporarily (((*s7* 'debug) 0)             ; needed to protect internal call-time functions (map below) from adding trace-in
			  ((*s7* 'history-enabled) #f)) ; so we have a chance of finding the calling expression
	  (let ((func (if (funclet? e) 
			  (with-let e __func__) 
			  (if (funclet? (outlet e))
			      (with-let (outlet e) __func__)
			      #<lambda>)))
		(args (map (lambda (x) 
			     (if (or (pair? (cdr x))
				     (and (symbol? (cdr x))
					  (not (keyword? (cdr x)))))
				 (list 'quote (cdr x))
				 (cdr x)))
			   e)))
	    
	    (let ((outer-call #f)           ; look for the expression that called this function
		  (callee (if (pair? func) (car func) func)))
	      (let ((history (*s7* 'history)))
		(do ((call (cdr history) (cdr call)))
		    ((eq? call history))
		  (when (and (pair? (car call))
			     (eq? (caar call) callee))
		    (set! outer-call (car call)))))
	      
	      (let-temporarily (((openlets) #f))
		(format *stderr* "~NC(~S~{~^ ~S~})" *debug-spaces* #\space (if (pair? func) (car func) func) args))
	      (if (pair? func)
		  (format *stderr* "    ; ~S: ~A[~D]" (car func) (cadr func) (caddr func)))
	      (if (and (pair? outer-call)
		       (pair-line-number outer-call))
		  (format *stderr* "~A called from ~A[~D]" 
			  (if (pair? func) "" "    ;")
			  (pair-filename outer-call) 
			  (pair-line-number outer-call)))
	      (newline *stderr*))))
	
	(when (> (*s7* 'debug) 1)  ; report value returned
	  (set! *debug-spaces* (min (*s7* 'max-format-length) (+ *debug-spaces* 2)))
	  (dynamic-unwind trace-out e)))
      )))

