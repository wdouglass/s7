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
				(if (and (or (pair? (cdr x))
					     (and (symbol? (cdr x))
						  (not (keyword? (cdr x)))))
					 (or (not func)
					     (not (macro? (symbol->value funcname e)))))
				   (list 'quote (cdr x))
				   (cdr x)))
			     e)))

		 (call (let-temporarily (((openlets) #f))  ; ignore local object->string methods
			 (format #f "(~S~{~^ ~S~})" 
				 funcname
				 args))))

	    (when (or func (> (*s7* 'debug) 2))
	      (cond (*debug-function*
		     (*debug-function* func args e))
			  
		    ((memq funcname *debug-breaks*)
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
		     (newline *debug-port*))))

	    (if *debug-stack*
		(vector-set! *debug-stack* (max 0 (min (- (length *debug-stack*) 1) (/ *debug-spaces* 2))) call))))

	(when (> (*s7* 'debug) 1)  ; report value returned, this needs to be at the top level in this function
	  (set! *debug-spaces* (+ *debug-spaces* 2))
	  (dynamic-unwind trace-out e))

	))))

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
  (require debug.scm)
  (let* ((source (procedure-source func))
         (new-source (cons (car source) 
			   (cons (cadr source)
				 (cons '(trace-in (curlet))
				       (cddr source))))))
    `(set! ,func ,new-source)))


;;; --------
;;; TODO:

;; use existing repl (we're assuming repl.scm above, but Snd has 2 others etc)
;;   this requires set-prompt at least, and the hello/goodbye message
;;   in Snd I think that's all: just set prompt, how to see C-q=play current channel?
;;      maybe (go)?
;;   also need to set *debug-port*, but how to write to the listener in Snd scheme?
;;   *listener-port* in Snd, using append_listener_text (snd-glistener.c and glistener.c: glistener_append_text)
;;      and append_listener_text snd-motif.c, put soft port in snd-listener.c, or glistener.c and export?
;;   also how to set Snd eval environment (for break)?
;;      g_set_top_level_let in snd-glistener|motif = (top-level-let) in scheme

  (define *listener-port* (openlet 
   (inlet 
	  :format (lambda (p str . args) (listener-write (apply format #f str args)))
	  :write (lambda (obj p)         (listener-write (object->string obj #t)))
	  :display (lambda (obj p)       (listener-write (object->string obj #f)))
	  :write-string (lambda (str p)  (listener-write str))
	  :write-char (lambda (ch p)     (listener-write (string ch)))
	  :newline (lambda (p)           (listener-write (string #\newline)))
	  :close-output-port (lambda (p) #f)
	  :flush-output-port (lambda (p) #f))))
   so Snd needs listener-write=append_listener_text + this in eval_c_string + break-out=go?

;; step: save old begin_hook, set to drop into repl as above, restore on exiting stepper (CM-q?)
;;   no way to get the true current code? cur_code is set just before the begin_hook call, but we don't see it in time!
;;   how to use begin_hook from scheme?

;; to watch var, (set! (setter var) (lambda (s v) (format... "~S set! to ~S" s v))) -- but need position info
;;   perhaps e could have code/file/line but this needs to be limited, and scheme-accessible

;; (trace abs) = trace above, but use #_ form if in unlet, or save before set, also trace needs to be a no-op if func already tracing
|#
