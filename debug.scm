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


;;; -------- trace
;; (trace func) -- trace one function (debug=0)
(define-macro (trace func)
  (let ((source (procedure-source func)))                           ; (lambda (x) (+ x 1))
    (if (pair? source)
	(unless (and (pair? (caddr source))
		     (eq? (caaddr source) 'trace-in))
	  (let ((new-source (cons (car source)                      ; lambda
				  (cons (cadr source)               ; args
					(cons '(trace-in (curlet))  ; (trace-in (curlet))
					      (cddr source))))))    ; body
	    `(define ,func ,new-source)))
	(let ((old-func (gensym)))
	  `(define ,func 
	     (let ((,old-func ,func))
	       (lambda args 
		 (trace-in (curlet))
		 (apply ,old-func args))))))))

(define-macro (untrace func)
  (let ((source (procedure-source func)))
    (if (pair? source)
	(when (and (pair? (caddr source))
		   (eq? (caaddr source) 'trace-in))
	  (let ((new-source (cons (car source)
				  (cons (cadr source)
					(cdddr source)))))
	    `(define ,func ,new-source)))
	`(define ,func (with-let (unlet) ,func)))))


;;; -------- break
(define-macro (break func)
  `(begin
     (trace ,func)
     (set! ((funclet trace-in) '*debug-breaks*) (cons ',func ((funclet trace-in) '*debug-breaks*)))))

(define-macro (unbreak func)
  `(set! ((funclet trace-in) '*debug-breaks*)
	 (let remove ((lst ((funclet trace-in) '*debug-breaks*)) (new-lst ()))
	   (if (null? lst)
	       (reverse new-lst)
	       (remove (cdr lst) 
		       (if (eq? (car lst) ',func) 
			   new-lst 
			   (cons (car lst) new-lst)))))))

(define (clear-breaks)
  (set! ((funclet trace-in) '*debug-breaks*) ()))


;; -------- watch
(define-macro (watch var)   ; notification if var set!
  `(set! (setter ',var) 
      (lambda (s v e)
	(format *stderr* "~S set! to ~S~A~%" s v 
                (let ((func (with-let e __func__)))
                  (if (eq? func #<undefined>) "" (format #f ", ~S" func))))
	v)))


(define (set-debug-port new-port)
  (set! ((funclet trace-in) '*debug-port*) new-port))


(define* (step (n 1))
  (set! (*s7* 'step-function) 
	(lambda (code) 
	  (format ((funclet trace-in) '*debug-port*) "step: ~S~%" code)
	  (((funclet trace-in) '*debug-repl*) code (curlet))))
  (set! (*s7* 'stepping) n)
  (set! ((*repl* 'repl-let) 'all-done) #t)) ; TODO: need this for each repl


(define (debug-stack)
  (let ((stack ((funclet trace-in) '*debug-stack*)))
    (when stack
      (let ((port ((funclet trace-in) '*debug-port*)))
	(format port "stack:\n")
	(do ((i 0 (+ i 1))) 
	    ((or (= i (length stack))
		 (>= i (/ ((funclet trace-in) '*debug-spaces*) 2))
		 (not (string? (vector-ref stack i)))))
	  (format port "  ~A~%" (vector-ref stack i)))
	(newline port)))))


(when (defined? 'debug.scm-init)
  ((symbol->value 'debug.scm-init)))


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

;; debug=0 trace-in off, =1 trace-in on (no additions), =2 trace-in named funcs, =3 trace-in all

;; step does hit begin_hook but cur_code looks strange

;; (frame n): move outlet n-from-bottom, set as curlet (but remember the starting curlet!)

;;   (continue): (set! stepping 0) or break
;;      but while in repl, eval should turn off the step and break 

;; tie into Snd repl
;;   this requires set-prompt at least, and the hello/goodbye message
;;      maybe (go)? (break-out)?
;;   also how to set Snd eval environment (for break)?
;;      g_set_top_level_let in snd-glistener|motif = (top-level-let) in scheme
;; snd-glistener.c:  result = s7_eval_c_string_with_environment(s7, text, top_level_let);
;; snd-motif.c:        form = s7_eval_c_string_with_environment(s7, str, top_level_let);
;; snd-no-gui.c: ??
;;   so in Snd we need to set top-level-let (it's a dilambda)
;;   drop-into-repl sets (*repl* 'top-level-let
;;   at that point:
#4  0x00005555559f7776 in eval (sc=0x555555f81ee0, first_op=422) at s7.c:89091
#5  0x00005555557f1747 in s7_eval (sc=0x555555f81ee0, code=0x555556704ac8, e=0x555555f85640) at s7.c:50764
#6  0x00005555556fbca7 in s7_eval_c_string_with_environment (sc=0x555555f81ee0, str=0x55555675ef60 "(ga 3)", e=0x555555f85640) at s7.c:27008
#7  0x0000555555c6f619 in listener_return (w=0x55555659ef00, last_prompt=149) at snd-motif.c:23349
;; in repl.scm we set local all-done #t which causes repl run to exit (we called it in the trace-in debug-repl function)
>(break ga)
(ga)
>(ga 3)  -> 4
4
>(curlet)
(inlet 'x 3)
;; so it has already returned

;; how to omit (let-temp debug 0) from history: if enabled changes to 0, look for let-temp preceding and backup cur_code?

;; unbound variable could look for close match in (curlet) via levenshtein
;;   there's a version in snd-help.c and repl.scm
|#
