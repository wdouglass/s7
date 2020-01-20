(provide 'debug.scm)


;(set! (*s7* 'max-stack-size) 4096)


(let-temporarily (((*s7* 'debug) 0))     ; so no trace-in call will be added to trace-in!

  (define (trace-in e)
    (let-temporarily (((*s7* 'debug) 0)) ; needed to protect internal call-time functions (map below) from adding trace-in
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

					;(format *stderr* "func: ~S, args: ~S, curlet: ~S, outlet: ~S~%" func args e (outlet e))
					;(format *stderr* "local: ~S ~S~%~%" (funclet? e) (funclet? (outlet e)))
					;(set! (*s7* 'history) (cons (if (pair? func) (car func) func) args))

	(format *stderr* "~S~%" func)
	;(format *stderr* "(~S~{~^ ~S~})~%" (if (pair? func) (car func) func) args)
	)))

  )
