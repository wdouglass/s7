;;; various lint-like checks

(define (find-if pred lst)
  (cond ((null? lst) #f)
	((pred (car lst)) (car lst))
	(else (find-if pred (cdr lst)))))
  
(define (va)
  (for-each
   (lambda (func)
     (system (format #f "fgrep ~A *.c > vahi" func))
     (call-with-input-file "vahi"
       (lambda (file)
	 (let loop ((line (read-line file #t)))
	   (or (eof-object? line)
	       (let ((len (length line))
		     (precount 0)
		     (ok #f)
		     (count 0)
		     (flen (length func)))
		 ;; look for * miscounts
		 (call-with-exit
		  (lambda (return)
		    (do ((i 0 (+ i 1)))
			((= i len))
		      (case (string-ref line i)
			((#\*)
			 (set! count (+ 1 count)))
			((#\=)
			 (set! count 0))
			(else
			 (cond ((and (< i (- len 2))
				     (string=? (substring line i (+ i 2)) "/*"))
				(return #f))
			       ((and (< i (- len flen))
				     (string=? (substring line i (+ i flen)) func))
				(set! precount count)
				(set! count 0))
			       ((and (< i (- len 6))
				     (string=? (substring line i (+ i 6)) "sizeof"))
				(set! ok #t)
				(set! count 0))))))))
		 (if (and ok
			  (not (= precount count 0))
			  (not (= count (- precount 1))))
		     (format () "calloc ~D->~D: ~A~%" precount count line))
		 (loop (read-line file #t))))))))
   '("calloc" "malloc" "realloc"))
  )

(va)


#|
(for-each
 (lambda (filename)
   (call-with-input-file filename
     (lambda (file)
       (let ((line-number 0)
	     (last-name ""))
	 (let loop ((line (read-line file #t)))
	   (or (eof-object? line)
	       (let ((len (length line)))
		 (set! line-number (+ line-number 1))
		 (if (> len 0)
		     (let ((start #f))
		       (do ((i 0 (+ i 1)))
			   ((>= i len))
			 (let ((chr (line i)))
			   (if (not (char-whitespace? chr))
			       (if (not start)
				   (set! start i))
			       (if start
				   (let* ((name (substring line start i))
					  (name-len (length name)))
				     (if (and (> name-len 0)
					      (char-alphabetic? (name 0))
					      (string=? name last-name))
					 (format () ";~A[~D]: ~A repeats in ~A~%" filename line-number name line))
				     (set! last-name name)
				     (set! start #f))))))))
		 (loop (read-line file #t)))))))))
 (list
  "snd.html"
  "sndscm.html"
  "grfsnd.html"
  "extsnd.html"
  "sndclm.html"
  "fm.html"
  "s7.html"
  "sndlib.html"
  ))
|#


#|
(format () "--------------------------------------------------------------------------------~%")
(let ((png-files (directory->list "/home/bil/cl/pix"))
      (baddies ()))
  (for-each
   (lambda (file)
     (if (and (not (directory? file))
	      (not (zero? (system (format #f "fgrep ~A *.html" file)))))
	 (set! baddies (cons file baddies))))
   png-files)
  (if (not (null? baddies))
      (begin
	(format () "--------------------------------------------------------------------------------~%")
	(format () ";unused pix/png: ~{~A ~}~%" baddies)
	(format () "--------------------------------------------------------------------------------~%"))))
|#

(exit)


