(if (not (provided? 'snd-peak-phases.scm)) (load "peak-phases.scm"))
(load "primes.scm")

(define (get-best choice n)
  (let ((val (vector-ref (case choice 
			   ((:all) noid-min-peak-phases)
			   ((:odd) nodd-min-peak-phases)
			   ((:even) neven-min-peak-phases)
			   (else primoid-min-peak-phases))
			 (cond ((<= n 128) (- n 1))
			       ((= n 256) 128)
			       ((= n 512) 129)
			       ((= n 1024) 130)
			       (else 131)))))
    (let ((a-val (vector-ref val 1))
	  (a-len (length val))
	  (a-data (vector-ref val 2)))
      (do ((k 3 (+ k 1)))
	  ((= k a-len)
	   (list a-val a-data))
	(when (and (real? (vector-ref val k))
		   (< (vector-ref val k) a-val))
	  (set! a-val (vector-ref val k))
	  (set! a-data (vector-ref val (+ k 1))))))))

(define (write-best-cases)
  (let ((file (open-output-file "best.data" "w")))
    (for-each
     (lambda (choice)
       (format file "  static mus_float_t ~A_mins[128] = {" (keyword->symbol choice))
       (do ((i 1 (+ i 1)))
	   ((= i 128))
	 (format file "~1,4F, " (car (get-best choice i))))
       (format file "~1,4F};~%~%" (car (get-best choice 128))))
     '(:all :odd :prime :even))

    (format file "  static mus_float_t min_8[4] = {")
    (for-each
     (lambda (choice)
       (format file "~1,4F, " (car (get-best choice 256))))
     '(:all :odd :even))
    (format file "~1,4F};~%" (car (get-best :prime 256)))

    (format file "  static mus_float_t min_9[4] = {")
    (for-each
     (lambda (choice)
       (format file "~1,4F, " (car (get-best choice 512))))
     '(:all :odd :even))
    (format file "~1,4F};~%" (car (get-best :prime 512)))

    (format file "  static mus_float_t min_10[4] = {")
    (for-each
     (lambda (choice)
       (format file "~1,4F, " (car (get-best choice 1024))))
     '(:all :odd :even))
    (format file "~1,4F};~%" (car (get-best :prime 1024)))

    (format file "  static mus_float_t min_11[4] = {")
    (for-each
     (lambda (choice)
       (format file "~1,4F, " (car (get-best choice 2048))))
     '(:all :odd :even))
    (format file "~1,4F};~%" (car (get-best :prime 2048)))

    (close-output-port file)))

(define (min-peak choice n)
  (let ((val (vector-ref (case choice 
			   ((:all) noid-min-peak-phases)
			   ((:odd) nodd-min-peak-phases)
			   ((:even) neven-min-peak-phases)
			   (else primoid-min-peak-phases))
			 (cond ((<= n 128) (- n 1))
			       ((= n 256) 128)
			       ((= n 512) 129)
			       ((= n 1024) 130)
			       (else 131)))))
    (let ((a-val (vector-ref val 1))
	  (a-len (length val)))
      (do ((k 2 (+ k 1)))
	  ((>= k a-len))
	(let* ((v (vector-ref val k))
	       (len (and (vector? v) (length v))))
	  (if (and (vector? v)
		   (not (= n len)))
	      (let-temporarily ((*s7* 'print-length) 4)
		(format () "~A ~D[~D]: vector length = ~D (~A ~A)~%" 
			choice n k 
			len
			(vector-ref val (- k 1))
			v)))
	  (if (not (or (number? v)
		       (float-vector? v)))
	      (format () "~A ~D[~D]: bad entry: ~A (a-len: ~A)~%" choice n k v a-len))
	  (if (float-vector? v)
	      (do ((i 0 (+ i 1)))
		  ((= i len))
		(if (> (abs (float-vector-ref v i)) 2.0)
		    (format () "~A ~D[~D][~D]: needs mod: ~A~%" choice n k i (float-vector-ref v i))))
	      (if (and (real? v)
		       (< v a-val))
		  (set! a-val v)))))
      a-val)))

(define (get-worst-overall choice choices)
  (let ((diffs (make-vector 116))
	(total 0.0)
	(this 0.0)
	(last 0.0)
	(next 0.0)
	(choice-list ())
	(first (car (get-best choice 11))))
    (set! this first)
    (set! next (car (get-best choice 12)))
    (do ((i 12 (+ i 1)))
	((= i 128))
      (set! last this)
      (set! this next)
      (set! next (car (get-best choice (+ i 1))))
      (set! (diffs (- i 12)) (cons i (- this (* 0.5 (+ last next)))))
      (set! total (+ total (abs (- this last)))))
    (sort! diffs (lambda (a b)
		   (> (cdr a) (cdr b))))
    (do ((i (- choices 1) (- i 1)))
	((< i 0))
      (set! choice-list (cons (diffs i) choice-list)))

    (list choice-list (- (+ total first) this)))) ; first to this would be a straight line


(define (get-worst-jump choice choices)
  (let ((start 40))
    (let ((+diffs (make-vector (- 128 start) #f))
	  (-diffs (make-vector (- 128 start) #f))
	  (last 0.0)
	  (+choice-list ())
	  (-choice-list ())
	  (this (car (get-best choice (- start 1)))))
      (do ((i start (+ i 1)))
	  ((= i 128))
	(set! last this)
	(set! this (car (get-best choice i)))
	(set! (+diffs (- i start)) (cons i (- this last)))
	(set! (-diffs (- i start)) (cons i (- last this))))
      (sort! +diffs (lambda (a b)
		      (> (cdr a) (cdr b))))
      (sort! -diffs (lambda (a b)
		      (> (cdr a) (cdr b))))
      (do ((i (- choices 1) (- i 1)))
	  ((< i 0))
	(set! +choice-list (cons (+diffs i) +choice-list))
	(set! -choice-list (cons (-diffs i) -choice-list)))
      (list +choice-list -choice-list))))
  
  
(define (base-case)
  (let ((all-lgs (list (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0)))
	(odd-lgs (list (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0)))
	(even-lgs (list (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0)))
	(prime-lgs (list (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0) (list 1.0 :all 256 120.0)))
	(sum 0.0)
	(even-sqrts 0)
	(odd-sqrts 0)
	(all-sqrts 0)
	(prime-sqrts 0)
	(all-dist 0.0)
	(odd-dist 0.0)
	(even-dist 0.0)
	(prime-dist 0.0)
	(all-under 0.0)
	(odd-under 0.0)
	(even-under 0.0)
	(prime-under 0.0)
					;(upper-limit 0.504)
	(escape #\escape))
    (let ((red-text (format #f "~C[31m" escape))
	  (normal-text (format #f "~C[0m" escape))
	  (bold-text (format #f "~C[1m" escape))
	  (unbold-text (format #f "~C[22m" escape)))
      
      (do ((i 2 (+ i 1)))
	  ((> i 128))
	(for-each
	 (lambda (choice)
	   (let* ((mn (min-peak choice i))
		  (lg (log mn i)))
	     (case choice 
	       ((:all)
		(if (< mn (sqrt i)) (set! all-sqrts (+ all-sqrts 1)))
		(set! all-lgs (cons (list lg choice i mn) all-lgs)))

	       ((:odd)
		(if (< mn (sqrt i)) (set! odd-sqrts (+ odd-sqrts 1)))
		(set! odd-lgs (cons (list lg choice i mn) odd-lgs)))

	       ((:prime)
		(if (< mn (sqrt i)) (set! prime-sqrts (+ prime-sqrts 1)))
		(set! prime-lgs (cons (list lg choice i mn) prime-lgs)))

	       ((:even)
		(if (< mn (sqrt i)) (set! even-sqrts (+ even-sqrts 1)))
		(set! even-lgs (cons (list lg choice i mn) even-lgs))))
	     
	     (set! sum (+ sum mn))
	     (when (> i 6)
	       (if (> mn (sqrt i))
		   (case choice
		     ((:all) (set! all-dist (+ all-dist (- mn (sqrt i)))))
		     ((:odd) (set! odd-dist (+ odd-dist (- mn (sqrt i)))))
		     ((:even) (set! even-dist (+ even-dist (- mn (sqrt i)))))
		     (else (set! prime-dist (+ prime-dist (- mn (sqrt i))))))
		   (case choice
		     ((:all) (set! all-under (+ all-under (- (sqrt i) mn))))
		     ((:odd) (set! odd-under (+ odd-under (- (sqrt i) mn))))
		     ((:even) (set! even-under (+ even-under (- (sqrt i) mn))))
		     (else (set! prime-under (+ prime-under (- (sqrt i) mn)))))))
	     ))
	 '(:all :odd :prime :even)))
      
      (format () "~%sum: ~A, sqrts: ~A ~A ~A ~A (~1,4F)~%~%" sum all-sqrts odd-sqrts prime-sqrts even-sqrts (+ all-dist odd-dist))
      
      (for-each
       (lambda (i)
	 (for-each
	  (lambda (choice)
	    (let* ((mn (min-peak choice i))
		   (lg (and (number? mn) (log mn i))))
	      (if mn
		  (case choice
		    ((:all)
		     (if (< mn (sqrt i)) (set! all-sqrts (+ all-sqrts 1)))
		     (set! all-lgs (cons (list lg choice i mn) all-lgs)))

		    ((:odd)
		     (if (< mn (sqrt i)) (set! odd-sqrts (+ odd-sqrts 1)))
		     (set! odd-lgs (cons (list lg choice i mn) odd-lgs)))

		    ((:prime)
		     (if (< mn (sqrt i)) (set! prime-sqrts (+ prime-sqrts 1)))
		     (set! prime-lgs (cons (list lg choice i mn) prime-lgs)))

		    ((:even)
		     (if (< mn (sqrt i)) (set! even-sqrts (+ even-sqrts 1)))
		     (set! even-lgs (cons (list lg choice i mn) even-lgs))))
		  (format () "no min? ~A ~A~%" choice i))))
	  '(:all :odd :prime :even)))
       '(256 512 1024 2048))
      
      (set! all-lgs (sort! all-lgs (lambda (a b) (< (car a) (car b)))))
      (set! odd-lgs (sort! odd-lgs (lambda (a b) (< (car a) (car b)))))
      (set! even-lgs (sort! even-lgs (lambda (a b) (< (car a) (car b)))))
      (set! prime-lgs (sort! prime-lgs (lambda (a b) (< (car a) (car b)))))
      
      (for-each
       (lambda (lst name)
	 (let ((last 0))
	   (do ((n (- (length lst) 1) (- n 1)))
	       ((and (> (caddr (list-ref lst n)) 20)
		     (<= (caddr (list-ref lst n)) 128))
		(set! last n)))
	   (format () "    ~A ~1,4F (~A) to ~1,4F (~A), dist: ~1,4F, ~1,4F~%" 
		   name
		   (caar lst) (caddar lst) 
		   (car (list-ref lst last)) (caddr (list-ref lst last))
		   (case name 
		     ((all) all-dist)
		     ((odd) odd-dist)
		     ((even) even-dist)
		     (else prime-dist))
		   (case name
		     ((all) all-under)
		     ((odd) odd-under)
		     ((even) even-under)
		     (else prime-under)))))
       (list all-lgs odd-lgs even-lgs prime-lgs)
       '(all odd even prime))
      
      (format () "~%")
      (format () "~A~96,'-T~A~%" red-text normal-text)
      (format () "~A~12Tall~37Todd~62Teven~86Tprime~A~%" bold-text unbold-text)
      (format () "~A~96,'-T~A~%" red-text normal-text)
      
      (do ((i 0 (+ i 1)))
	  ((= i 131))
	(let* ((all (list-ref all-lgs i)) ; '(exp :all n peak)
	       (odd (list-ref odd-lgs i))
	       (even (list-ref even-lgs i))
	       (prime (list-ref prime-lgs i))
	       (bold-all (<= (car all) 0.5))
	       (bold-odd (<= (car odd) 0.5)))
	  (format () (if bold-all
			 (if bold-odd
			     "~A~D~11T~1,3F~20T~1,4F~A~33T| ~A~D~45T~1,3F~53T~1,4F~A~67T| ~D~74T~1,3F~82T~1,4F~91T| ~D~99T~1,3F~107T~1,4F~%"
			     "~A~D~11T~1,3F~20T~1,4F~A~33T| ~*~D~41T~1,3F~49T~1,4F~*~58T| ~D~65T~1,3F~73T~1,4F~82T| ~D~90T~1,3F~98T~1,4F~%")
			 (if bold-odd
			     "~*~D~7T~1,3F~16T~1,4F~*~24T| ~A~D~36T~1,3F~44T~1,4F~A~58T| ~D~65T~1,3F~73T~1,4F~82T| ~D~90T~1,3F~98T~1,4F~%"
			     "~*~D~7T~1,3F~16T~1,4F~*~24T| ~*~D~32T~1,3F~40T~1,4F~*~49T| ~D~56T~1,3F~64T~1,4F~73T| ~D~81T~1,3F~89T~1,4F~%"))
		  bold-text
		  (caddr all) (cadddr all) (car all)
		  unbold-text      
		  bold-text
		  (caddr odd) (cadddr odd) (car odd)
		  unbold-text
		  (caddr even) (cadddr even) (car even)
		  (caddr prime) (cadddr prime) (car prime)
		  )))))
  
  (write-best-cases)
  
  (for-each
   (lambda (choice)
     (let* ((data (get-worst-overall choice 7))
	    (choices (car data))
	    (total (cadr data)))
       (format () "~%~A ~,2F:~14T" choice total)
       (do ((i 0 (+ i 1)))
	   ((= i 7))
	 (format () "(~A ~,2F)~A " 
		 (car (list-ref choices i)) 
		 (cdr (list-ref choices i))
		 (if (>= (car (list-ref choices i)) 100) "" " ")))))
   '(:all :odd :even :prime))
  (format () "~%~%")
  )


(define (checks)
  ;; now check for stupid mistakes...
  
  (let ((previous-version (if (file-exists? "/home/bil/cl_copy/peak-phases.scm")
			      "/home/bil/cl_copy/peak-phases.scm"
			      "/home/bil/dist/snd/peak-phases.scm")))
					;(format *stderr* "check ~S~%" previous-version)
    (let ((alls (make-vector 129))
	  (odds (make-vector 129))
	  (evens (make-vector 129))
	  (primes (make-vector 129)))
      (do ((i 2 (+ i 1)))
	  ((> i 128))
	(vector-set! alls i (min-peak :all i))
	(vector-set! odds i (min-peak :odd i))
	(vector-set! evens i (min-peak :even i))
	(vector-set! primes i (min-peak :prime i)))

      (load previous-version)
      (do ((i 2 (+ i 1)))
	  ((> i 128))
	(let ((old-all (min-peak :all i))
	      (old-odd (min-peak :odd i))
	      (old-even (min-peak :even i))
	      (old-prime (min-peak :prime i)))
	  (if (< old-all (vector-ref alls i)) (format () ":all ~D: ~A to ~A~%" i old-all (alls i)))
	  (if (< old-odd (vector-ref odds i)) (format () ":odd ~D: ~A to ~A~%" i old-odd (odds i)))
	  (if (< old-even (vector-ref evens i)) (format () ":even ~D: ~A to ~A~%" i old-even (evens i)))
	  (if (< old-prime (vector-ref primes i)) (format () ":prime ~D: ~A to ~A~%" i old-prime (primes i))))))))


(base-case)
(checks)

(s7-version)
(exit)
