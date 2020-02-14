(provide 'profile.scm)

;;; new version -- work in progress

(let-temporarily (((*s7* 'profile) 0))

  (define profile-in
    (let ((*profile-info* (make-hash-table)); 8 eq? (cons symbol? vector?)))
	  (funcname #f))

      (set! (setter '*profile-info*) hash-table?)

      (define (profile-out f val)
	(let ((end-time (*s7* 'cpu-time))
	      (v (hash-table-ref *profile-info* f)))
	  (vector-set! v 3 (- (vector-ref v 3) 1))    ; if recursion, time only the outer call
	  (when (zero? (vector-ref v 3))
	    (vector-set! v 1 (+ (vector-ref v 1) (- end-time (vector-ref v 2)))))
	  val))

      (lambda (e)          ; from s7 profile-in call added to func
	(set! funcname (*function* e :name))
	(when (symbol? funcname)
	  (let ((v (hash-table-ref *profile-info* funcname)))
	    (unless v
	      (set! v (vector 0 0.0 0.0 0))
	      (hash-table-set! *profile-info* funcname v))
	    (when (zero? (vector-ref v 3))
	      (vector-set! v 2 (*s7* 'cpu-time)))
	    (vector-set! v 0 (+ (vector-ref v 0) 1))
	    (vector-set! v 3 (+ (vector-ref v 3) 1)))
	  (dynamic-unwind profile-out funcname)))))
  
  (define* (show-profile (n 100))
    (let ((info ((funclet profile-in) '*profile-info*)))
      (if (zero? (hash-table-entries info))
	  (format *stderr* "no profiling data!~%")

	  (let* ((entries (hash-table-entries info))
		 (vect (make-vector entries)))
	    (copy info vect)
	    (set! vect (sort! vect (lambda (a b)  ; sort by calls
				     (> (vector-ref (cdr a) 1) (vector-ref (cdr b) 1)))))
	    (let ((name-len 0)
		  (name-max 0)
		  (end (min n entries)))

	      (do ((i 0 (+ i 1)))
		  ((= i end))
		(let ((len (length (symbol->string (car (vector-ref vect i))))))
		  (set! name-len (+ name-len len))
		  (set! name-max (max name-max len))))
	      (set! name-max (max (round (/ name-len entries)) (floor (* .9 name-max))))

	      (format *stderr* "info:\n")
	      (do ((i 0 (+ i 1)))
		  ((= i end))
		(let ((entry (vector-ref vect i)))
		  (format *stderr* "  ~S:~NTcalls ~S, time ~,4G~%" 
			  (car entry)
			  (+ name-max 5)
			  (vector-ref (cdr entry) 0)
			  (vector-ref (cdr entry) 1))))
	      )))))

  (define (clear-profile)
    (fill! ((funclet profile-in) '*profile-info*) #f))

  (define profile-info (dilambda (lambda () 
				   ((funclet profile-in) '*profile-info*))
				 (lambda (v)
				   (set! ((funclet profile-in) '*profile-info*) v))))
  )
