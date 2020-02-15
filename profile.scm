(provide 'profile.scm)

;;; new version -- work in progress

(let-temporarily (((*s7* 'profile) 0))

  (define* (show-profile (n 100))
    (let ((info (*s7* 'profile-info)))
;      (format *stderr* "info: ~S~%" info)
      (if (not info)
	  (format *stderr* "no profiling data!~%")

	  (let* ((funcs (car info))
		 (data (cadr info))
		 (ticks/sec (* 1.0 (caddr info)))
		 (entries (length funcs))
		 (vect (make-vector entries)))
	    (do ((i 0 (+ i 1)))
		((= i entries))
	      (vector-set! vect i (list (/ (data (+ (* i 5) 3)) ticks/sec)
					(funcs i)
					(data (* i 5)))))
	    
;	    (format *stderr* "~D: ~S~%" entries funcs)
	    (set! vect (sort! vect (lambda (a b)  ; sort by time
				     (> (car a) (car b)))))
	    (let ((name-len 0)
		  (name-max 0)
		  (end (min n entries)))

	      (do ((i 0 (+ i 1)))
		  ((= i end))
		(let ((len (length (symbol->string (cadr (vector-ref vect i))))))
		  (set! name-len (+ name-len len))
		  (set! name-max (max name-max len))))
	      (set! name-max (max (round (/ name-len entries)) (floor (* .9 name-max))))

	      (format *stderr* "info:\n")
	      (do ((i 0 (+ i 1)))
		  ((= i end))
		(let ((entry (vector-ref vect i)))
		  (format *stderr* "  ~S:~NTcalls ~S, time ~,4G~%" 
			  (cadr entry)
			  (+ name-max 5)
			  (caddr entry)
			  (car entry)))))))))

  (define (clear-profile)
    (set! (*s7* 'profile-info) #f))

  )
