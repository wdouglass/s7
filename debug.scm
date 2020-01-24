(provide 'debug.scm)

(let-temporarily (((*s7* 'debug) 0)) ; trace-in call should not be added to trace-in!
  
  (define trace-in 
    (let ((*debug-spaces* 0)
	  (*debug-max-spaces* (*s7* 'max-format-length))
	  (*debug-port* *stderr*)
	  (*debug-stack* #f)
	  (*debug-function* #f)
	  (*debug-breaks* ()))
      
      (set! (setter '*debug-spaces*) integer?)
      (set! (setter '*debug-max-spaces*) integer?)
      (set! (setter '*debug-breaks*) list?)
      (set! (setter '*debug-port*) (lambda (s v) 
				     (if (or (output-port? v) (not v))
					 v 
					 (error 'wrong-type-arg "~S can't be set! to ~S" s v))))
      (set! (setter '*debug-stack*) (lambda (s v) 
				      (if (or (vector? v) (not v)) 
					  v 
					  (error 'wrong-type-arg "~S can't be set! to ~S" s v))))
      (set! (setter '*debug-function*) (lambda (s v)
					 (if (or (not v)
						 (and (procedure? v)
						      (= (car (arity v)) 3)))
					     v
					     (error 'wrong-type-arg "~S must be #f or a procedure of 3 arguments" s))))
      
      (define (drop-into-repl call e)
	(let ((C-q (integer->char 17)))      ; C-q to exit repl
	  (let-temporarily ((((*repl* 'keymap) C-q) (let-temporarily (((*s7* 'debug) 0)) 
						      (lambda (c) 
							(set! ((*repl* 'repl-let) 'all-done) #t))))
			    ((*repl* 'top-level-let) e)
			    ((*repl* 'prompt) (let-temporarily (((*s7* 'debug) 0)) 
						(lambda (num) 
						  (with-let (*repl* 'repl-let)
						    (set! prompt-string "break> ") 
						    (set! prompt-length (length prompt-string)))))))
	    (with-let (*repl* 'repl-let)
	      (set! cur-line "")
	      (set! red-par-pos #f)
	      (set! cursor-pos 0)
	      (set! prompt-string "break> ") 
	      (set! prompt-length (length prompt-string)))
	    (format *debug-port* "break: ~A, C-q to exit break~%" call)
	    ((*repl* 'run)))))

      (define (trace-out e val)     ; report value returned if debug>1
	(let-temporarily (((*s7* 'history-enabled) #f))
	  (set! *debug-spaces* (max 0 (- *debug-spaces* 2)))
	  (let-temporarily (((openlets) #f)) ; val local object->string might cause an infinite loop
	    (format *debug-port* "~NC  -> ~S~%" (min *debug-spaces* *debug-max-spaces*) #\space val))
	  val))

      (lambda (e)                   ; trace-in, report function call
	(let-temporarily (((*s7* 'history-enabled) #f))

	  (let ((func (if (funclet? e) 
			  (with-let e __func__) 
			  (if (funclet? (outlet e))
			      (with-let (outlet e) __func__)
			      #<lambda>)))
		(args (let-temporarily (((*s7* 'debug) 0)) ; keep s7 from adding trace-in to this map function
			(map (lambda (x) 
			       (if (or (pair? (cdr x))
				       (and (symbol? (cdr x))
					    (not (keyword? (cdr x)))))
				   (list 'quote (cdr x)) 
				   (cdr x)))
			     e))))
		(let ((call (let-temporarily (((openlets) #f))  ; ignore local object->string methods
			      (format #f "(~S~{~^ ~S~})" 
				      (if (pair? func) (car func) func)
				      args))))

		  (cond (*debug-function*
			 (*debug-function* func args e))

			((memq (if (pair? func) (car func) func) *debug-breaks*)
			 (require repl.scm)
			 (drop-into-repl call e))

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

		  (if *debug-stack*
		      (vector-set! *debug-stack* (max 0 (min (- (length *debug-stack*) 1) (/ *debug-spaces* 2))) call)))))

	(when (> (*s7* 'debug) 1)  ; report value returned, this needs to be at the top level in this function
	  (set! *debug-spaces* (+ *debug-spaces* 2))
	  (dynamic-unwind trace-out e))

	))))

#|
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

;; (trace func) -- trace func (debug=0)
(define-macro (trace func)
  ;; to get the return value: (let-temporarily (((*s7* 'debug) 2)) (func))
  (require debug.scm)
  (let* ((source (procedure-source func))
         (new-source (cons (car source) 
			   (cons (cadr source)
				 (cons '(trace-in (curlet))
				       (cddr source))))))
    `(set! ,func ,new-source)))


;;; --------
;;; TODO:

;; some way to trace just top-level functions etc
;; use of exisiting repl (we're assuming repl.scm above, but Snd has 2 others etc)
;; show stack (of outlets) and move around in it in repl
;; show error (if any)
  
;; step: save old begin_hook, set to drop into repl as above, restore on exiting stepper (CM-q?)

;; to watch var, (set! (setter var) (lambda (s v) (format... "~S set! to ~S" s v))) -- but need position info -- perhaps setter -> cur_code?
;;   would need scheme access to current_code(sc)? but even that is unreliable

;; truncated: (let ((str (format...))) (if (< (length str) width) str (append (substring str 0 width) "...")))
;;   maybe add ~N to ~A ~S in format

;; object->let?
;; show source?
|#
