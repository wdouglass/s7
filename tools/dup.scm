;;; dup.scm
;;; (dups size file alloc-lines): 
;;;    find all matches of "size" successive lines in "file" ignoring empty lines and leading/trailing whitespace
;;;    "alloc-lines" is any number bigger than the number of lines in "file"
;;;    (dups 16 "s7.c" 89000) finds all 16-line matches in s7.c which has less than 89000 lines in all

(define dups 
  (let ()

    (define-constant (all-positive? v start end)
      (let ((j end))
	(do ((i start (+ i 1)))
	    ((= i end) j)
	  (if (< (int-vector-ref v i) 0)
	      (set! j i)))))

    (lambda (size file alloc-lines)
      (let ((lines (make-vector alloc-lines ""))
	    (original-lines (make-vector alloc-lines ""))
	    (lens (make-int-vector alloc-lines 0))
	    (linenums (make-int-vector alloc-lines 0)))
	
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
	      
	      (set! size (min size total-lines))

	      ;; mark unmatchable strings
	      (let ((sortv (make-vector total-lines)))
		(do ((i 0 (+ i 1)))
		    ((= i total-lines))
		  (vector-set! sortv i (cons (vector-ref lines i) i)))
		(set! sortv (sort! sortv (lambda (a b)
					   (string<? (car a) (car b)))))
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
		  ((>= i last-line)) ; >= because i is set below
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
				  (let ((full-size size))
				    (do ((nk jk (+ nk 1))
					 (ni j (+ ni 1)))
					((or (= nk total-lines)
					     (not (= (int-vector-ref lens ni) (int-vector-ref lens nk)))
					     (not (string=? (vector-ref lines ni) (vector-ref lines nk))))
					 (set! full-size (+ size (- nk jk)))))
				    (if first
					(let ((first-line (int-vector-ref linenums i)))
					  (format *stderr* "~NC~%~{~A~%~}~%  lines ~D ~D" 8 #\- ; lineseq 
						  (subvector original-lines (- (int-vector-ref linenums (+ i size)) first-line) first-line)
						  first-line
						  (int-vector-ref linenums k))
					  (set! first #f))
					(format *stderr* " ~D" (int-vector-ref linenums k)))
				    (set! i (+ i full-size))
				    (when (< size full-size)
				      (format *stderr* "[~D]" full-size))))
				(set! k jk))))
			(unless first
			  (format *stderr* "~%")))
		      (set! i j)))))))))))

(dups 16 "s7.c" 90000)
;(dups 6 "s7.c" 90000)
;(dups 12 "ffitest.c" 2000)
;(dups 8 "ffitest.c" 2000)
