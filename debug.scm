(provide 'debug.scm)

(let-temporarily (((*s7* 'debug) 0)) ; trace-in call should not be added to trace-in!

  (define trace-in 
    (let ((*debug-spaces* 0)
	  (*debug-port* *stderr*))

      (define (trace-out e val)     ; report value returned if debug>1
	(set! *debug-spaces* (max 0 (- *debug-spaces* 2)))
	(let-temporarily (((openlets) #f)) ; val local object->string might cause an infinite loop
	  (format *debug-port* "~NC  -> ~S~%" *debug-spaces* #\space val))
	val)

      (lambda (e)                   ; trace-in, report function call
	(let-temporarily (((*s7* 'history-enabled) #f)) ; so we have a chance of finding the calling expression

	  (let ((func (if (funclet? e) 
			  (with-let e __func__) 
			  (if (funclet? (outlet e))
			      (with-let (outlet e) __func__)
			      #<lambda>)))
		(args (let-temporarily (((*s7* 'debug) -1)) ; keep s7 from adding trace-in to this map function
			(map (lambda (x) 
			       (if (or (pair? (cdr x))
				       (and (symbol? (cdr x))
					    (not (keyword? (cdr x)))))
				   (list 'quote (cdr x)) 
				   (cdr x)))
			     e))))
	    
	    (let ((outer-call #f))           ; look for the expression that called this function
	      (let ((callee (if (pair? func) (car func) func))
		    (history (*s7* 'history)))
		(do ((call (cdr history) (cdr call)))
		    ((eq? call history))
		  (when (and (pair? (car call))
			     (eq? (caar call) callee))
		    (set! outer-call (car call)))))
	      
	      (let-temporarily (((openlets) #f))  ; ignore local object->string methods, as above
		(format *debug-port* "~NC(~S~{~^ ~S~})" 
			(min (*s7* 'max-format-length) *debug-spaces*) #\space 
			(if (pair? func) (car func) func) 
			args))
	      (if (pair? func)
		  (format *debug-port* "    ; ~S: ~A[~D]" (car func) (cadr func) (caddr func)))
	      (if (and (pair? outer-call) 
		       (pair-line-number outer-call))
		  (format *debug-port* "~A called from ~A[~D]" 
			  (if (pair? func) "" "    ;")
			  (pair-filename outer-call)
			  (pair-line-number outer-call)))
	      (newline *debug-port*))))

	(when (> (*s7* 'debug) 1)  ; report value returned
	  (set! *debug-spaces* (min (*s7* 'max-format-length) (+ *debug-spaces* 2)))
	  (dynamic-unwind trace-out e)))
      )))

#|
(define (set-debug-port new-port)
  (let ((old-port ((funclet trace-in) '*debug-port)))
    (set! ((funclet trace-in) '*debug-port) new-port)
    old-port))
|#
