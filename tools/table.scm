(define (no-dashes-or-cr str)
  (do ((len (length str))
       (newstr "")
       (last-ch #\-)
       (i 0 (+ i 1)))
      ((= i (- len 1)) 
       newstr)
    (let ((ch (string-ref str i)))
      (if (and (or (not (char=? ch #\-))
		   (char-alphabetic? last-ch))
	       (not (char=? ch #\newline)))
	  (set! newstr (string-append newstr (make-string 1 ch))))
      (set! last-ch ch))))

(let ((ctr 0))
  (call-with-input-file 
      "snd-test.scm"
    (lambda (file)
      (let loop ((line (read-line file #t)))
	(set! ctr (+ ctr 1))
	(or (eof-object? line)
	    (let ((len (length line)))
	      (if (and (> len 30)
		       (string=? ";;; ---------------- test "
				 (substring line 0 26)))
		  (format () "~A ~48,1T[~D]~%" (no-dashes-or-cr line) ctr))
	      (loop (read-line file #t))))))))

(exit)
