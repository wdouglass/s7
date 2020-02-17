(provide 'profile.scm)

(let-temporarily (((*s7* 'profile) 0))

  (define show-profile 
    (let ((*profile-port* *stderr*))

      (set! (setter '*profile-port*)
	    (lambda (s v)
	      (if (or (output-port? v) (not v))
		  v
		  (error 'wrong-type-arg "~S can't be set! to ~S" s v))))

      (lambda* ((n 100))
	(let ((info (*s7* 'profile-info))) ; a list: '(vector-of-function-names int-vector-of-profile-data ticks-per-second)
	  (if (not info)
	      (format *profile-port* "no profiling data!~%")
	      
	      (let* ((funcs (car info))
		     ;; function names (symbols)
		     (data (cadr info))
		     ;; each entry in the data vector is a block of 5 integers: 
		     ;;   calls <ignore> <ignore> inclusive-time exclusive-time
		     (ticks/sec (* 1.0 (caddr info)))
		     ;;   divide by ticks/sec to turn the times into seconds
		     (entries (length funcs))
		     (vect (make-vector entries)))
		(do ((i 0 (+ i 1)))
		    ((= i entries))
		  (vector-set! vect i (list (/ (data (+ (* i 5) 3)) ticks/sec)    ; inclusive timing
					    (funcs i)                             ; function name
					    (data (* i 5))                        ; calls
					    (/ (data (+ (* i 5) 4)) ticks/sec)))) ; exclusive timing
		
		(set! vect (sort! vect (lambda (a b)               ; sort by inclusive time
					 (> (car a) (car b)))))
		(let ((name-len 0)                                 ; decide the data column
		      (name-max 0)
		      (end (min n entries))
		      (call-max 0))
		  (do ((i 0 (+ i 1)))
		      ((= i end))
		    (let ((len (length (symbol->string (cadr (vector-ref vect i))))))
		      (set! name-len (+ name-len len))
		      (set! name-max (max name-max len)))
		    (set! call-max (max call-max (caddr (vector-ref vect i)))))
		  (set! name-max (max (round (/ name-len entries)) (floor (* .9 name-max))))
		  (set! call-max (+ 1 (ceiling (log call-max 10))))

		  (format *profile-port* "info:\n")
		  (do ((i 0 (+ i 1))
		       (excl 0.0))
		      ((= i end)

		       (format *profile-port* "  ")
		       (when (< end entries)
			 (format *profile-port* "the rest (~D entries): ~,4F, "
				 (- entries end) 
				 (- (car (vector-ref vect 0)) excl)))
		       (format *profile-port* "cells allocated: ~A~%" 
			       (let ((num (with-let *s7* 
					    (+ (- heap-size free-heap-size) gc-total-freed))))
				 (cond ((< num 1000) (format #f "~D" num))
				       ((< num 1000000) (format #f "~,1Fk" (/ num 1000.0)))
				       ((< num 1000000000) (format #f "~,1FM" (/ num 1000000.0)))
				       (else (format #f "~,1FG" (/ num 1000000000.0)))))))

		    (let ((entry (vector-ref vect i)))
		      (format *profile-port* "  ~S:~NTcalls ~S, ~NTtime ~,4F ~NT~,4F~%" 
			      (cadr entry)
			      (+ name-max 5)
			      (caddr entry)
			      (+ name-max 5 6 call-max)
			      (car entry)
			      (+ name-max 5 6 call-max 8 6)
			      (max 0.0 (cadddr entry)))
		      (set! excl (+ excl (cadddr entry))))))))))))
	      
  (define (clear-profile)
    (set! (*s7* 'profile-info) #f))

  (define profile-port (dilambda
			(lambda ()
			  ((funclet show-profile) '*profile-port*))
			(lambda (new-port)
			  (set! ((funclet show-profile) '*profile-port*) new-port))))
  )
