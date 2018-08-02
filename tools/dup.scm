
(define (dups size file alloc-lines)
  ;; find match of "size" successive lines in "file" (ignoring empty lines and leading/trailing whitespace)
  (let ((lines (make-vector alloc-lines ""))
	(original-lines (make-vector alloc-lines ""))
	(lens (make-int-vector alloc-lines 0))
	(linenums (make-int-vector alloc-lines 0)))

    (define (all-positive? v start end)
      (do ((i start (+ i 1)))
	  ((or (= i end)
	       (not (positive? (int-vector-ref v i))))
	   i)))
	  
    (call-with-input-file file
      (lambda (p)
	;; get lines, original and trimmed
	(let ((total-lines 
	       (do ((i 0 (+ i 1))
		    (j 0)
		    (line (read-line p) (read-line p)))
		   ((eq? line #<eof>) j)
		 ;; save original lines
		 (vector-set! original-lines i line)
		 (let ((len (length line)))
		   (when (> len 0)
		     ;; trim leading whitespace
		     (do ((k 0 (+ k 1)))
			 ((or (= k len)
			      (not (char-whitespace? (string-ref line k))))
			  (when (> k 0)
			    (set! line (substring line k))
			    (set! len (- len k)))))
		     ;; trim trailing whitespace
		     (when (> len 0)
		       (do ((j (- len 1) (- j 1)))
			   ((or (< j 0)
				(not (char-whitespace? (string-ref line j))))
			    (when (not (= j (- len 1)))
			      (set! line (substring line 0 (+ j 1)))
			      (set! len (+ j 1))))))
		     (when (> len 0)
		       (int-vector-set! linenums j i)
		       (vector-set! lines j line)
		       (int-vector-set! lens j (length line))
		       (set! j (+ j 1))))))))

	  ;; mark unmatchable strings
	  (let ((sortv (make-vector total-lines)))
	    (do ((i 0 (+ i 1)))
		((= i total-lines))
	      (vector-set! sortv i (cons (vector-ref lines i) i)))
	    (set! sortv (sort! sortv (lambda (a b)
				       (string<=? (car a) (car b)))))
	    (let ((unctr -1))
	      (do ((i 0))
		  ((= i total-lines))
		(let ((current (car (vector-ref sortv i))))
		  (do ((k (+ i 1) (+ k 1)))
		      ((or (= k total-lines)
			   (not (string=? current (car (vector-ref sortv k)))))
		       (when (= i (- k 1))
			 (int-vector-set! lens (cdr (vector-ref sortv i)) unctr)
			 (set! unctr (- unctr 1)))
		       (set! i k)))))))

	  ;; look for matches
	  (do ((i 0 (+ i 1))
	       (first #t #t)
	       (last-line (- total-lines size)))
	      ((= i last-line))
	    (let ((j (all-positive? lens i (+ i size))))   ; is a match possible?
	      (if (= j (+ i size))
		  (let ((lenseq (subvector lens size i))
			(lineseq (subvector lines size i)))
		    (do ((k (+ i 1) (+ k 1)))
			((>= k last-line))
		      (let ((jk (all-positive? lens k (+ k size))))
			(if (= jk (+ k size))
			    (when (and (equal? lenseq (subvector lens size k))
				       (equal? lineseq (subvector lines size k)))
			      (if first
				  (let ((first-line (int-vector-ref linenums i)))
				    (format *stderr* "~NC~%~{~A~%~}~%  lines ~D ~D" 8 #\- ; lineseq 
					    (subvector original-lines (- (int-vector-ref linenums (+ i size)) first-line) first-line)
					    first-line
					    (int-vector-ref linenums k))
				    (set! first #f))
				  (format *stderr* " ~D" (int-vector-ref linenums k))))
			    (set! k jk))))
		    (unless first
		      (format *stderr* "~%")))
		  (set! i j)))))))))

(dups 16 "s7.c" 88000)
;(dups 12 "ffitest.c" 2000)
  


#|
  (let ((levenshtein 
	 (lambda (s1 s2)
	   (let ((L1 (length s1))
		 (L2 (length s2)))
	     (cond ((zero? L1) L2)
		   ((zero? L2) L1)
		   (else (let ((distance (make-vector (list (+ L2 1) (+ L1 1)) 0)))
			   (do ((i 0 (+ i 1)))
			       ((> i L1))
			     (set! (distance 0 i) i))
			   (do ((i 0 (+ i 1)))
			       ((> i L2))
			     (set! (distance i 0) i))
			   (do ((i 1 (+ i 1)))
			       ((> i L2))
			     (do ((j 1 (+ j 1)))
				 ((> j L1))
			       (let ((c1 (+ (distance i (- j 1)) 1))
				     (c2 (+ (distance (- i 1) j) 1))
				     (c3 (if (char=? (s2 (- i 1)) (s1 (- j 1)))
					     (distance (- i 1) (- j 1))
					     (+ (distance (- i 1) (- j 1)) 1))))
				 (set! (distance i j) (min c1 c2 c3)))))
			   (distance L2 L1)))))))

repl uses (< distance (floor (log str 2))) for a match, so if len diff > min2 don't check?

same code as above but string=? -> fuzzy (and sorter use check above)
|#
