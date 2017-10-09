
(define (sed in-file out-file source-text dest-text)
  (let ((source-len (length source-text)))
    (call-with-output-file out-file
      (lambda (p)
	(call-with-input-file in-file
	  (lambda (file)
	    (let loop ((line (read-line file)))
	      (or (eof-object? line)
		  (let ((pos (string-position source-text line)))
		    (if pos
			(format p "~A~A~A~%" (substring line 0 pos) dest-text (substring line (+ pos source-len)))
			(format p "~A~%" line))
		    (loop (read-line file)))))))))
    (exit)))



