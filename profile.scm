(provide 'profile.scm)

;;; new version -- work in progress

(define profile-in
  (let ((*profile-info* (make-hash-table)))
    (lambda (e) ; from s7 profile-in call added to func
      (let* ((func (if (funclet? e)
		       (with-let e __func__)
		       (and (funclet? (outlet e))
			    (with-let (outlet e) __func__))))
	     (funcname (if (pair? func) (car func) func)))

	;; add to profile-info hash (key=funcname): calls time cells
	;;   need to track depth also for recursion

	(dynamic-unwind (lambda (f)
			  #f)
			funcname)))))

;;; then when profiling done

(define* (show-profile (n 100))
  (let ((info ((funclet profile-in) '*profile-info*)))
    (if (null? info)
	(format *stderr* "no profiling data!~%")
	(let* ((entries (hash-table-entries info))
	       (vect (make-vector entries)))

	  (copy info vect)
	  (set! vect (sort! vect (lambda (a b) (> (cadr a) (cadr b))))) ; time? or calls?
	  
	  (set! n (min n entries))
	  (do ((i 0 (+ i 1)))
	      ((= i n) 
	       (newline *stderr*))
	    (let* ((data (vect i))
		   (func (car data))
		    ... 
		    )))))))
