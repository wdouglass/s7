(define fsize 100000)
(define ssize 500000)

(define (wis)
  (with-input-from-string "asdf"
    (lambda ()
      (unless (eqv? (read-char) #\a)
	(format *stderr* "read-char trouble\n"))
      (unless (eqv? (read-char) #\s)
	(format *stderr* "read-char trouble\n"))
      (unless (eqv? (read-char (current-input-port)) #\d)
	(format *stderr* "current-input-port trouble\n")))))

(define (call-wis) 
  (do ((i 0 (+ i 1)))
      ((= i ssize))
    (wis)))

(define (cwis)
  (let ((a (char->integer #\a))
	(s (char->integer #\s)))
    (call-with-input-string "asdf"
      (lambda (p)
	(if (port-closed? p)
	     (format *stderr* "cwis port closed\n"))
	(unless (eqv? (read-byte p) a)
	  (format *stderr* "call read-char trouble\n"))
	(unless (eqv? (read-byte p) s)
	  (format *stderr* "call read-char trouble\n"))
	(unless (eqv? (port-position p) 2)
	  (format *stderr* "cwis position: ~A~%" (port-position p)))))))

(define (call-cwis) 
  (do ((i 0 (+ i 1)))
      ((= i ssize))
    (cwis)))

(define (wif)
  (with-input-from-file "t923.scm"
    (lambda ()
      (unless (string=? (read-line) "asdf")
	(format *stderr* "file read-char trouble\n"))
      (unless (eq? (read-line) #<eof>)
	(format *stderr* "file read-line eof trouble\n")))))

(define (call-wif) 
  (do ((i 0 (+ i 1)))
      ((= i fsize))
    (wif)))

(define (cwif)
  (call-with-input-file "t923.scm"
    (lambda (p)
      (unless (string=? (port-filename p) "t923.scm")
	(format *stderr* "cxif port filename: ~S~%" (port-filename p)))
      (unless (string=? (read-string 5 p) "asdf\n")
	(format *stderr* "file read-string trouble\n"))
      (unless (eq? (read-string 1 p) #<eof>)
	(format *stderr* "file read-string eof trouble\n")))))

(define (call-cwif) 
  (do ((i 0 (+ i 1)))
      ((= i fsize))
    (cwif)))


(define (wos)
  (with-output-to-string
    (lambda ()
      (write-char #\a)
      (write-char #\b)
      (flush-output-port (current-output-port))
      (unless (string=? (get-output-string (current-output-port)) "ab")
	(format *stderr* "write-char trouble\n")))))

(define (call-wos) 
  (do ((i 0 (+ i 1)))
      ((= i ssize))
    (wos)))

(define (cwos)
  (call-with-output-string
    (lambda (p)
      (write-string "asdf" p)
      (flush-output-port p)
      (unless (string=? (get-output-string p) "asdf")
	(format *stderr* "call write-string trouble\n")))))

(define (call-cwos) 
  (do ((i 0 (+ i 1)))
      ((= i ssize))
    (cwos)))

(define (wof)
  (with-output-to-file "/dev/null"
    (lambda ()
      (write-byte 100)
      (write-byte 101))))

(define (call-wof) 
  (do ((i 0 (+ i 1)))
      ((= i fsize))
    (wof)))

(define (cwof)
  (call-with-output-file "/dev/null"
    (lambda (p)
      (newline p))))

(define (call-cwof) 
  (do ((i 0 (+ i 1)))
      ((= i fsize))
    (cwof)))

(define (op1)
  (let ((port (open-input-string "(+ 1 2)")))
    (unless (input-port? port)
      (format *stderr* "op1 trouble\n"))
    (unless (char=? (peek-char port) #\()
      (format *stderr "op1 peek-char trouble\n"))
    (unless (equal? (read port) '(+ 1 2))
      (format *stderr* "op1 trouble\n"))
    (close-input-port port)))

(define (call-op1) 
  (do ((i 0 (+ i 1)))
      ((= i ssize))
    (op1)))

(define (op2)
  (let ((port (open-output-string)))
    (unless (output-port? port)
      (format *stderr* "op2 trouble\n"))
    (display "(+ 1 2)" port)
    (newline port)
    (flush-output-port port)
    (unless (string=? (get-output-string port) "(+ 1 2)\n")
      (format *stderr* "op2 trouble: ~S\n" (get-output-string port)))
    (close-output-port port)))

(define (call-op2) 
  (do ((i 0 (+ i 1)))
      ((= i ssize))
    (op2)))

(define (op3)
  (let ((port (open-input-file "t923.scm")))
    (unless (char=? (peek-char port) #\a)
      (format *stderr "op3 peek-char trouble\n"))
    (unless (string=? (read-line port) "asdf")
      (format *stderr* "op3 trouble\n"))
    (unless (string=? (read-line port) "fdsa")
      (format *stderr* "op3 2 trouble\n"))
    (unless (eqv? (port-position port) 10)
      (format *stderr* "op3 pos: ~S~%" (port-position port)))
    (unless (eqv? (port-line-number port) 3) ; 1-based??
      (format *stderr* "op3 line: ~S~%" (port-line-number port)))
    (close-input-port port)))

(define (call-op3) 
  (do ((i 0 (+ i 1)))
      ((= i fsize))
    (op3)))

(define (op4)
  (let ((port (open-output-file "/dev/null")))
    (write "asdf" port)
    (close-output-port port)))

(define (call-op4) 
  (do ((i 0 (+ i 1)))
      ((= i fsize))
    (op4)))

#|
(define (funcs)
  (let ((ip (open-input-function
	     (lambda (choice)
	       (case choice
		 ((read peek-char) #\?)
		 ((char-ready?) #f)
		 ((read-char) #\a)
		 ((read-line) "a line"))))))
    (unless (string=? (read-line ip) "a line")
      (format *stderr* "input-function trouble\n"))
    (close-input-port ip))
  (let* ((str ())
	 (op (open-output-function
	      (lambda (c)
		(set! str (cons c str))))))
    (display #\a op)
    (close-output-port op)
    (unless (equal? str '(97))
      (format *stderr* "output-function trouble: ~S\n" str))))

(define (call-funcs) 
  (do ((i 0 (+ i 1)))
      ((= i fsize))
    (funcs)))
|#

(call-with-output-file "t923.scm"
  (lambda (p)
    (display "asdf" p)
    (newline p)))

(call-wis)
(call-cwis)
(call-wif)
(call-cwif)
(call-wos)
(call-cwos)
(call-wof)
(call-cwof)

(call-with-output-file "t923.scm"
  (lambda (p)
    (display "asdf\n" p)
    (display "fdsa" p)
    (newline p)))

(call-op1)
(call-op2)
(call-op3)
(call-op4)

;(call-funcs)

;(when (> (*s7* 'profile) 0) (show-profile 200))
(exit)

;; this is dominated by fopen, fwrite, and fclose -- mallocs everywhere!, so I multiplied the string ports by 5
