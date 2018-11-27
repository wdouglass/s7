;;; various standard benchmarks 
;;; --------------------------------------------------------------------------------

(define (fannkuch n)
  (call-with-exit
   (lambda (return)
     (let ((perm (make-int-vector n))
	   (perm1 (make-int-vector n))
	   (count (make-int-vector n))
	   (subs (make-vector n))
	   (subs1 (make-vector n))
	   (maxFlipsCount 0)
	   (checksum 0)
	   (r n)
	   (perm0 0))
       
       (do ((i 0 (+ i 1)))
	   ((= i n))
	 (int-vector-set! perm1 i i)
	 (vector-set! subs i (subvector perm (+ i 1)))
	 (vector-set! subs1 i (subvector perm1 i 1)))
       
       (do ((permCount #t (not permCount)))
	   (#f)
	 (do ()
	     ((= r 1))
	   (int-vector-set! count (- r 1) r)
	   (set! r (- r 1)))
	 (copy perm1 perm)
	 
	 (do ((flipsCount 0 (+ flipsCount 1))
	      (k (int-vector-ref perm 0) (int-vector-ref perm 0)))
	     ((= k 0)
	      (set! maxFlipsCount (max maxFlipsCount flipsCount))
	      (if permCount 
		  (set! checksum (+ checksum flipsCount)) 
		  (set! checksum (- checksum flipsCount))))
	   (reverse! (vector-ref subs k)))
	 
	 (do ((done #f))
	     (done)
	   (when (= r n)
	     (return (cons checksum maxFlipsCount)))
	   
	   (set! perm0 (int-vector-ref perm1 0))
	   (copy (vector-ref subs1 r) perm1)
	   (int-vector-set! perm1 r perm0)

	   (if (positive? (int-vector-set! count r (- (int-vector-ref count r) 1)))
	       (set! done #t)
	       (set! r (+ r 1)))))))))

;; (fannkuch 7): (228 . 16), 8: (1616 . 22), 9: (8629 . 30), 10: (73196 . 38), 11: (556355 . 51), 12: (3968050 . 65)
(display (fannkuch 7)) (newline)
;; (fannkuch 12) takes around 5 minutes (297 secs)

;;; --------------------------------------------------------------------------------

(define (wc)
  (let ((newk 0)
	(nl 0)
	(nw 0)
	(nc 0)
	(inword #f)
	(port (open-input-file "/home/bil/test/scheme/bench/src/bib")))
    (do ((str (read-line port) (read-line port))
	 (nl 0 (+ nl 1)))
	((eof-object? str)
	 (close-input-port port)
	 (list nl (+ nw nl) nc))
      (set! nc (+ nc 1 (string-length str)))
      (set! newk 0)
      (do ((k (char-position #\space str) (char-position #\space str (+ k 1))))
	  ((not k))
	(if (not (= k newk))
	    (set! nw (+ nw 1)))
	(set! newk (+ k 1))))))

(display (wc)) (newline) ; '(31102 851820 4460056)

;;; --------------------------------------------------------------------------------

(define (cat)
  (let ((inport (open-input-file "/home/bil/test/scheme/bench/src/bib"))
	(outport (open-output-file "foo"))) 
    (catch #t
      (lambda ()
	(do () (#f) 
	  (write-string (read-line inport #t) outport)))
      (lambda args
	(close-input-port inport)
	(close-output-port outport)))))

(cat)

;;; --------------------------------------------------------------------------------

(define (string-cat n)
  (let ((s "abcdef"))
    (do ((i 0 (+ i 1)))
	((= i 10) 
	 (string-length s))
      (set! s "abcdef")
      (do ()
	  ((> (string-length s) n))
	(set! s (string-append "123" s "456" s "789"))
	(set! s (string-append
		 (substring s (quotient (string-length s) 2) (string-length s))
		 (substring s 0 (+ 1 (quotient (string-length s) 2)))))))))

(display (string-cat 500000)) (newline) ; 524278

;;; --------------------------------------------------------------------------------

(define mbrot
  (let ()
    (define (count cr ci)
      (let ((max-count 64)
	    (radius^2 16.0)
	    (tzr 0.0)
	    (zr cr)
	    (zi ci))
	(do ((zr^2 (* zr zr) (* zr zr))
	     (zi^2 (* zi zi) (* zi zi))
	     (c 0 (+ c 1)))
	    ((or (= c max-count)
		 (> (+ zr^2 zi^2) radius^2))
	     c)
	  (set! tzr (+ (- zr^2 zi^2) cr))
	  (set! zi (+ (* 2.0 zr zi) ci))
	  (set! zr tzr))))

    (define (mb matrix r i step n)
      (do ((x 0 (+ x 1)))
	  ((= x n))
	(do ((y 0 (+ y 1)))
	    ((= y n))
	  (vector-set! matrix x y (count (+ r (* step x)) (+ i (* step y)))))))

    (lambda (n)
      (let ((matrix (make-vector (list n n))))
	(mb matrix -1.0 -0.5 0.005 n)
	(if (not (= (matrix 0 0) 5))
	    (format #t ";mbrot: ~A~%" n))))))

(mbrot 75)

;;; --------------------------------------------------------------------------------



;;; --------------------------------------------------------------------------------

(s7-version)
(exit)
