;;; try each s7test test macro (using repl, not snd)

(define aux-counter 0)
(system "make-repl")

(for-each
 (lambda (test-case)
   (let ((aux-file (format #f "t101-aux-~D.scm" (set! aux-counter (+ aux-counter 1)))))
     (call-with-output-file aux-file
       (lambda (p)
	 (format p "(define-macro (test tst expected) ~A)~%" test-case)
	 (format p "(define-macro (num-test tst expected) ~A)~%" (string-append "`(nok? " (substring test-case 5)))
	 (format p "(load \"s7test.scm\")~%(exit)~%")))
     (format *stderr* "~%~NC~%test: ~S~%" 80 #\- test-case)
     (system (string-append "./repl " aux-file))))
 (list 
  "`(ok? ',tst (lambda () (eval ',tst)) ,expected)"
  "`(ok? ',tst (lambda () ,tst) ,expected)"
  "`(ok? ',tst (let () (define (_s7_) ,tst)) ,expected)"
  "`(ok? ',tst (let () (define (_s7_) ,tst) (define (_call_) (_s7_))) ,expected)"
  "`(ok? ',tst (lambda () (let ((_s7_ #f)) (set! _s7_ ,tst))) ,expected)"
  "`(ok? ',tst (lambda () (let ((_s7_ ,tst)) _s7_)) ,expected)"
  "`(ok? ',tst (catch #t (lambda () (lambda* ((!a! ,tst)) !a!)) (lambda any (lambda () 'error))) ,expected)"
  "`(ok? ',tst (lambda () (do ((!a! ,tst)) (#t !a!))) ,expected)"
  "`(ok? ',tst (lambda () (do ((__i__ 0 (+ __i__ 1))) ((= __i__ 1) ,expected) ,tst)) ,expected)"
  "`(ok? ',tst (lambda () (let ((__x__ #f)) (define (__f__) (do ((__i__ 0 (+ __i__ 1))) ((= __i__ 1) __x__) (set! __x__ ,tst))) (__f__))) ,expected)"
  "`(ok? ',tst (lambda () (define (!f!) (let ((!v! (vector #f))) (do ((!i! 0 (+ !i! 1))) ((= !i! 1) (!v! 0)) (vector-set! !v! 0 ,tst)))) (!f!)) ,expected)"
  "`(ok? ',tst (lambda () (define (!f!) (let ((!v! #f)) (do ((!i! 0 (+ !i! 1))) ((= !i! 1) !v!) (set! !v! ,tst)))) (!f!)) ,expected)"
  "`(ok? ',tst (lambda () (define (!f!) (let ((!x! (map (lambda (!a!) ,tst) '(0)))) (car !x!))) (!f!)) ,expected)"
  "`(ok? ',tst (lambda () (call-with-exit (lambda (!a!) (!a! ,tst)))) ,expected)"
  "`(ok? ',tst (lambda () (call/cc (lambda (!a!) (!a! ,tst)))) ,expected)"
  "`(ok? ',tst (lambda () (values ,tst)) ,expected)"
  "`(ok? ',tst (lambda () ((lambda (!a! !b!) !b!) (values #f ,tst))) ,expected)"
  "`(ok? ',tst (lambda () (define (_s7_ !a!) !a!) (_s7_ ,tst)) ,expected)"
  "`(ok? ',tst (lambda () (let ((___x #f)) (set! ___x ,tst))) ,expected)"
  "`(ok? ',tst (lambda () (let ((___x #(#f))) (set! (___x 0) ,tst))) ,expected)"
  "`(ok? ',tst (lambda () (let ((___x #(#f))) (vector-set! ___x 0 ,tst))) ,expected)"
  "`(ok? ',tst (lambda () (dynamic-wind (lambda () #f) (lambda () ,tst) (lambda () #f))) ,expected)"
  "`(ok? ',tst (lambda () (caadr (catch 'receive (lambda () (throw 'receive ,tst)) (lambda any any)))) ,expected)"
  "`(ok? ',tst (lambda () (stacktrace (- (random 100) 50) (- (random 100) 50) (- (random 100) 50) (- (random 100) 50) (> (random 100) 50)) ,tst) ,expected)"
  "`(ok? ',tst (lambda () (let ((__val__ (s7-optimize '(,tst)))) (if (eq? __val__ #<undefined>) ,tst __val__)))	,expected)"
  "`(ok? ',tst (lambda () (let ((!x 0)) (set! (setter '!x) (lambda (_A _B) ,tst)) (set! !x 1))) ,expected)"
  "`(ok? ',tst (lambda () (define* (fgh1 (!x ,tst)) !x) (fgh1)) ,expected)"
  "`(ok? ',tst (lambda () (define !f (let ((!x ,tst)) (lambda () !x))) (!f)) ,expected)"
  "`(ok? ',tst (lambda () (define !f (make-iterator (let ((+iterator+ #t)) (lambda () ,tst)))) (iterate !f)) ,expected)"
  "`(ok? ',tst (lambda () (let ((!str (object->string ,tst :readable))) (eval-string !str))) ,expected)"
  "`(ok? ',tst (lambda () (catch 'xyzzy (lambda () (throw 'xyzzy ,tst)) (lambda (type info) (car info)))) ,expected)"
  "`(ok? ',tst (lambda () (let ((!x 0)) (let-temporarily ((!x #f)) ,tst))) ,expected)"
  "`(ok? ',tst (lambda () (let () (define h! (make-hook '!x)) (set! (hook-functions h!) (list (lambda (!h) (set! (!h 'result) ,tst)))) (h!))) ,expected)"
  ))

(format *stderr* "~NC ffitest ~NC~%" 20 #\- 20 #\-)
(if (provided? 'linux)
    (begin
      (system "gcc -o ffitest ffitest.c -g -Wall s7.o -lm -I. -ldl")
      (system "ffitest"))
    (if (provided? 'freebsd)
	(begin
	  (system "cc -o ffitest ffitest.c -g -Wall s7.o -lm -I. -ldl")
	  (system "ffitest"))
	(if (provided? 'osx)
	    (begin
	      (system "gcc -o ffitest ffitest.c -g -Wall s7.o -lm -I.")
	      (system "ffitest"))
	    )))
    

(format *stderr* "~%~NC lint ~NC~%" 20 #\- 20 #\-)
(catch #t (lambda () (lint "s7test.scm" #f)) (lambda args #f))

;; lint clobbers reader-cond
(define-expansion (reader-cond . clauses)
  (call-with-exit
   (lambda (return)
     (for-each
      (lambda (clause)
	(let ((val (eval (car clause))))
	  (if val
	      (if (null? (cdr clause)) (return val)
		  (if (null? (cddr clause)) (return (cadr clause))
		      (return (apply values (map quote (cdr clause)))))))))
      clauses)
     (values))))

(format *stderr* "~%~NC local s7test ~NC~%" 20 #\- 20 #\-)
(system "./snd -e '(let () (catch #t (lambda () (load \"s7test.scm\" (curlet))) (lambda args #f)) (exit))'")

(format *stderr* "~NC s7test ~NC~%" 20 #\- 20 #\-)
(system "./snd s7test.scm")

(format *stderr* "~NC tpeak ~NC~%" 20 #\- 20 #\-)
(system "./snd tpeak.scm")

(format *stderr* "~NC lt ~NC~%" 20 #\- 20 #\-)
(system "./snd lt.scm")

(format *stderr* "~NC tcopy ~NC~%" 20 #\- 20 #\-)
(system "./repl tcopy.scm")

(format *stderr* "~NC tmap ~NC~%" 20 #\- 20 #\-)
(system "./repl tmap.scm")

(format *stderr* "~NC teq ~NC~%" 20 #\- 20 #\-)
(system "./repl teq.scm")

(format *stderr* "~NC titer ~NC~%" 20 #\- 20 #\-)
(system "./repl titer.scm")

(format *stderr* "~%~NC tform ~NC~%" 20 #\- 20 #\-)
(system "./repl tform.scm")

(format *stderr* "~%~NC tread ~NC~%" 20 #\- 20 #\-)
(system "./repl tread.scm")

(format *stderr* "~%~NC thash ~NC~%" 20 #\- 20 #\-)
(system "./repl thash.scm")

(format *stderr* "~NC tauto ~NC~%" 20 #\- 20 #\-)
(system "./repl tauto.scm")

(format *stderr* "~NC tset ~NC~%" 20 #\- 20 #\-)
(system "./repl tset.scm")

(format *stderr* "~NC index ~NC~%" 20 #\- 20 #\-)
(system "./snd make-index.scm")

(format *stderr* "~NC makexg ~NC~%" 20 #\- 20 #\-)
(system "./snd makexg.scm")

(format *stderr* "~NC makegl ~NC~%" 20 #\- 20 #\-)
(system "./snd makegl.scm")

(format *stderr* "~NC tgen ~NC~%" 20 #\- 20 #\-)
(system "./snd tgen.scm")

(format *stderr* "~NC tall ~NC~%" 20 #\- 20 #\-)
(system "./snd tall.scm")

(format *stderr* "~NC snd-test ~NC~%" 20 #\- 20 #\-)
(system "./snd -l snd-test.scm")

(format *stderr* "~NC bench ~NC~%" 20 #\- 20 #\-)
(system "(cd /home/bil/test/scheme/bench/src ; /home/bil/cl/snd test-all.scm)")

;(format *stderr* "~NC lg ~NC~%" 20 #\- 20 #\-)
;(system "./repl lg.scm")

(exit)
