;;; this is an extension of tauto.scm, an auto-tester

;(set! (hook-functions *load-hook*) (list (lambda (hook) (format () "loading ~S...~%" (hook 'name)))))

(load "stuff.scm")
;(load "write.scm")
;(load "mockery.scm")

(set! (*s7* 'safety) 1)

(set! (*s7* 'max-stack-size) 32768)
(set! (*s7* 'max-heap-size) (ash 1 23)) ; 8M -- 560000000 is about 8G
;(set! (*s7* 'gc-stats) 6)
(set! (*s7* 'print-length) 1000)
(set! (*s7* 'max-string-length) 100000)
(set! (*s7* 'max-list-length) 10000)
(set! (*s7* 'max-vector-length) 10000)
(set! (*s7* 'max-vector-dimensions) 10)
(set! (*s7* 'autoloading?) #f)
(set! (*s7* 'morally-equal-float-epsilon) 1.0e-6)
(set! (current-output-port) #f)

(define ostr "")
(define nostr "")

(define __var__ #f)
(define __var1__ #f)
(define error-type #f)
(define error-info #f)
(define false #f)
(define _undef_ (car (with-input-from-string "(#_asdf 1 2)" read)))
(define kar car)
;(set! (setter kar) (lambda (x) car))
(set! (setter kar) (lambda (x) (error 'oops "kar not settable: ~A" ostr)))
(define-constant _1234_ 1234)
(define _dilambda_ (dilambda (lambda (x) (+ x 1)) (lambda (x y) (+ x y))))
(define __var2__ 3)
(set! (symbol-setter '__var2__) (lambda (s v) (if (integer? v) v 3)))

(define (free1) (set! x (- (+ x 1) 1)))
(define (free2) (x i))
(define (free3) (local-func 0))
(define (_vals_) (values #f 1 2))
(define (finite? n) (not (or (nan? n) (infinite? n))))

(define (s7-print-length) (*s7* 'print-length))
(define (s7-max-string-length) (*s7* 'max-string-length))
(define (s7-max-list-length) (*s7* 'max-list-length))
(define (s7-max-vector-length) (*s7* 'max-vector-length))
(define (s7-max-vector-dimensions) (*s7* 'max-vector-dimensions))
(define (s7-default-hash-table-length) (*s7* 'default-hash-table-length))
(define (s7-initial-string-port-length) (*s7* 'initial-string-port-length))
(define (s7-safety) (*s7* 'safety))
(define (s7-set-safety val) (set! (*s7* 'safety) val))
#|
(define (s7-undefined-identifier-warnings) (*s7* 'undefined-identifier-warnings))
(define (s7-autoloading?) (*s7* 'autoloading?))
(define (s7-max-stack-size) (*s7* 'max-stack-size))
(define (s7-stacktrace-defaults) (*s7* 'stacktrace-defaults))
(define (s7-gc-stats) (*s7* 'gc-stats))

(define (s7-set-print-length x) 
  ;(format *stderr* "(~A ~A)~%" 'print-length (object->string x #t 16)) 
  (set! (*s7* 'print-length) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'print-length))
  )
(define (s7-set-max-string-length x) 
  ;(format *stderr* "(~A ~A)~%" 'max-string-length (object->string x #t 16)) 
  (set! (*s7* 'max-string-length) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'max-string-length))
  )
(define (s7-set-max-list-length x) 
  ;(format *stderr* "(~A ~A)~%" 'max-list-length (object->string x #t 16)) 
  (set! (*s7* 'max-list-length) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'max-list-length))
  )
(define (s7-set-max-vector-length x)
  ;(format *stderr* "(~A ~A)~%" 'max-vector-length (object->string x #t 16)) 
  (set! (*s7* 'max-vector-length) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'max-vector-length))
  )
(define (s7-set-max-vector-dimensions x)
  ;(format *stderr* "(~A ~A)~%" 'max-vector-dimensions (object->string x #t 16)) 
  (set! (*s7* 'max-vector-dimensions) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'max-vector-dimensions))
  )
(define (s7-set-default-hash-table-length x)
  ;(format *stderr* "(~A ~A)~%" 'default-hash-table-length (object->string x #t 16)) 
  (set! (*s7* 'default-hash-table-length) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'default-hash-table-length))
  )
(define (s7-set-initial-string-port-length x)
  ;(format *stderr* "(~A ~A)~%" 'initial-string-port-length (object->string x #t 16)) 
  (set! (*s7* 'initial-string-port-length) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'initial-string-port-length))
  )
(define (s7-set-undefined-identifier-warnings x)
  ;(format *stderr* "(~A ~A)~%" 'undefined-identifier-warnings (object->string x #t 16))
  (set! (*s7* 'undefined-identifier-warnings) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'undefined-identifier-warnings))
  )
(define (s7-set-autoloading? x)
  ;(format *stderr* "(~A ~A)~%" 'autoloading? (object->string x #t 16))
  (set! (*s7* 'autoloading?) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'autoloading?))
  )
(define (s7-set-max-stack-size x)
  ;(format *stderr* "(~A ~A)~%" 'max-stack-size (object->string x #t 16))
  (set! (*s7* 'max-stack-size) x)
  ;(format *stderr* "  -> ~A~%" *s7* 'max-stack-size)
  )
(define (s7-set-stacktrace-defaults x) 
  ;(format *stderr* "(~A ~A)~%" 'stacktrace-defaults (object->string x #t 16))
  (set! (*s7* 'stacktrace-defaults) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'stacktrace-defaults))
  )
(define (s7-set-gc-stats x) 
  ;(format *stderr* "(~A ~A)~%" 'gc-stats (object->string x #t 16))
  (set! (*s7* 'gc-stats) x)
  ;(format *stderr* "  -> ~A~%" *s7* 'gc-stats)
  )
(define (s7-set-default-rationalize-error x)
  ;(format *stderr* "(~A ~A)~%" 'default-rationalize-error (object->string x #t 16)) 
  (set! (*s7* 'default-rationalize-error) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'default-rationalize-error))
  )

(define (s7-profile-info) (*s7* 'profile-info))
(define (s7-catches) (*s7* 'catches))
(define (s7-exits) (*s7* 'exits))
(define (s7-c-types) (*s7* 'c-types))
(define (s7-stack-top) (*s7* 'stack-top))
(define (s7-stack-size) (*s7* 'stack-size))
(define (s7-stack) (*s7* 'stack))
(define (s7-symbol-table) (*s7* 'symbol-table))
(define (s7-rootlet-size) (*s7* 'rootlet-size))
(define (s7-heap-size) (*s7* 'heap-size))
(define (s7-free-heap-size) (*s7* 'free-heap-size))
(define (s7-gc-freed) (*s7* 'gc-freed))
(define (s7-gc-protected-objects) (*s7* 'gc-protected-objects))
(define (s7-memory-usage) (*s7* 'memory-usage))
|#
(define (s7-history) (*s7* 'history))
(define (s7-history-size) (*s7* 'history-size))

(define (s7-set-morally-equal-float-epsilon x)
  ;(format *stderr* "(~A ~A)~%" 'morally-equal-float-epsilon (object->string x #t 16)) 
  (set! (*s7* 'morally-equal-float-epsilon) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'morally-equal-float-epsilon))
  )
(define (s7-set-hash-table-float-epsilon x)
  ;(format *stderr* "(~A ~A)~%" 'hash-table-float-epsilon (object->string x #t 16))
  (set! (*s7* 'hash-table-float-epsilon) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'hash-table-float-epsilon))
  )
(define (s7-set-bignum-precision x)
  ;(format *stderr* "(~A ~A)~%" 'bignum-precision (object->string x #t 16)) 
  (set! (*s7* 'bignum-precision) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'bignum-precision))
  )
(define (s7-set-float-format-precision x)
  ;(format *stderr* "(~A ~A)~%" 'float-format-precision (object->string x #t 16)) 
  (set! (*s7* 'float-format-precision) x)
  ;(format *stderr* "  -> ~A~%" (*s7* 'float-format-precision))
  )

(define (s7-default-rationalize-error) (*s7* 'default-rationalize-error))
(define (s7-morally-equal-float-epsilon) (*s7* 'morally-equal-float-epsilon))
(define (s7-hash-table-float-epsilon) (*s7* 'hash-table-float-epsilon))
(define (s7-bignum-precision) (*s7* 'bignum-precision))
(define (s7-float-format-precision) (*s7* 'float-format-precision))
(define (s7-default-random-state) (*s7* 'default-random-state))
(define (s7-cpu-time) (*s7* 'cpu-time))
(define (s7-file-names) (*s7* 'file-names))

(define-macro (_mac_ x) `(+ ,x 1))
(define-macro* (_mac*_ (x 1)) `(+ ,x 1))
(define-bacro (_bac_ x) `(+ ,x 1))
(define-bacro* (_bac*_ (x 1)) `(+ ,x 1))
(define (_fnc_ x) (+ x 1))
(define* (_fnc*_ (x 1)) (+ x 1))
(define (_fnc1_ x) (apply + (list x 1)))
(define (_fnc2_ x) (- x 1))
(define (_fnc3_ x) (* x 2.0))
(define (_fnc4_ x) (/ x))
(define (_fnc5_ x) (not (pair? x)))

(define (checked-eval code)
  (and (null? (cyclic-sequences code))
       (eval code)))

(define (checked-hash-table* . args)
  (let ((_h_ (make-hash-table (*s7* 'default-hash-table-length) morally-equal?)))
    (do ((key/value args (cddr key/value)))
	((null? key/value) _h_)
      (if (not (pair? (cdr key/value)))
	  (error 'wrong-number-of-args "no value")
	  (set! (_h_ (car key/value)) (cadr key/value))))))

(define (checked-hash-table . args)
  (let ((_h_ (make-hash-table (*s7* 'default-hash-table-length) morally-equal?)))
    (do ((key/value args (cdr key/value)))
	((null? key/value) _h_)
      (if (not (pair? key/value))
	  (error 'wrong-type-args "not a pair")
	  (set! (_h_ (car key/value)) (cdr key/value))))))


(load "s7test-block.so" (sublet (curlet) (cons 'init_func 'block_init)))

(define-expansion (_dw_ . args)
  `(dynamic-wind (lambda () #f) (lambda () ,@args) (lambda () #f)))

(define-expansion (_dw_string_ . args)
  `(let ((_port_ #f)
	 (_old_port_ #f))
     (dynamic-wind
	 (lambda ()
	   (set! _old_port_ (current-input-port))
	   (set! _port_ (open-input-string "1234"))
	   (set-current-input-port _port_))
	 (lambda ()
	   ,@args)
	 (lambda ()
	   (close-input-port _port_)
	   (set-current-input-port _old_port_)))))

(define-expansion (_dw_out_ . args)
  `(let ((_port_ #f)
	 (_old_port_ #f))
     (dynamic-wind
	 (lambda ()
	   (set! _old_port_ (current-output-port))
	   (set! _port_ (open-output-string))
	   (set-current-output-port _port_))
	 (lambda ()
	   ,@args
	   (flush-output-port _port_)
	   (get-output-string _port_))
	 (lambda ()
	   (close-output-port _port_)
	   (set-current-output-port _old_port_)))))

(define-expansion (_cw_ . args)
  `(call-with-exit (lambda (_x_) (_x_ ,@args))))

(define-expansion (_ct1_ . args)
  `(catch #t (lambda () (call-with-exit (lambda (goto) (values ,@args)))) (lambda args 'error)))

(define-expansion (_ct2_ . args)
  `(catch #t (lambda () (call-with-exit (lambda (goto) (goto ,@args)))) (lambda args 'error)))

(define-expansion (_mem1_ . args)
  `(member 1 (list 3 2) (lambda (a b) ,@args)))

(define-expansion (_mem2_ . args)
  `(assoc 1 (list (list 3 2) (list 2)) (lambda (a b) ,@args)))

(define-expansion (_ft1_ . args)
  `(let ((_f_ (lambda () ,@args))) (_f_) (_f_)))

(define-expansion (_ft2_ . args)
  `(let () (define (_f_) ,@args) (define (g) (_f_)) (g) (g)))

(define-expansion (_fa1_ . args)
  `(let ((_y_ (begin ,@args))) (define (f x) (x)) (f _y_)))

(define-expansion (_fa2_ . args)
  `((lambda (x) (apply x ())) (begin ,@args)))

(define-expansion (_lt2_ . args)
  `(let ((mx max)) ((lambda* ((max min) (min mx)) ,@args))))

(define-expansion (_rf1_ . args)
  `(let ((y 0)) (define (_rf11_ i x) (if (> i 0) (_rf11_ (- i 1) x) (x))) (_rf11_ 1 (lambda () ,@args))))

(define-expansion (_rf2_ . args) 
  `(let () (define (_rf22_ i x) (if (> i 0) (_rf22_ (- i 1) x) (x i))) (_rf22_ 1 (lambda (y) (begin ,@args)))))

(define-expansion (_do1_ . args)
  `(with-output-to-string 
     (lambda () 
       (do ((i 0 (+ i 1)))
	   ((= i 1))
	 ,@(map (lambda (x)
		  `(display ,x))
		args)))))

(define-expansion (_do2_ . args)
  `(with-output-to-string 
     (lambda () 
       ,@(map (lambda (x)
		`(display ,x))
	      args))))

(define-expansion (_do3_ . args)
  `(let ((exiter (vector #f))) (do ,(car args) ((vector-ref exiter 0) 1) ,@(cdr args) (vector-set! exiter 0 #t))))

(define-expansion (_cop1_ . args)
  `(let ((x begin ,@args))
     x))

(define-expansion (_cop2_ . args)
  `(let ((x begin ,@args))
     (copy x)))

(define-expansion (_rd1_ . args)
  `(let ((port #f))
     (dynamic-wind
	 (lambda ()
	   (set! port (open-input-string (format #f "~{~W~^ ~}" (list ,@args)))))
	 (lambda ()
	   (read port))
	 (lambda ()
	   (close-input-port port)))))

(define-expansion (_rd2_ . args)
  `(with-input-from-string
       (object->string (car (list ,@args)) :readable)
     read))


(define ims (immutable! (string #\a #\b #\c)))
(define imbv (immutable! (byte-vector 0 1 2)))
(define imv (immutable! (vector 0 1 2)))
(define imiv (immutable! (int-vector 0 1 2)))
(define imfv (immutable! (float-vector 0 1 2)))
(define imi (immutable! (inlet 'a 1 'b 2)))
(define imb (immutable! (block 0.0 1.0 2.0)))
(define imh (immutable! (hash-table* 'a 1 'b 2)))
;(define imp (immutable! (cons 0 (immutable! (cons 1 (immutable! (cons 2 ())))))))

(set! (hook-functions *unbound-variable-hook*) ())
(define x 0)
(define max-stack (*s7* 'stack-top))

;;; TODO: ip/op str IO -- read checked etc, nested call/exit str/nstr?, reorder num args? check eqops?
;;;   cycles injected here, check via tree_cyclic in s7 if safety>0, check setting *s7* fields

(let ((functions (vector 'not '= '+ 'cdr 'real? 'rational? 'number? '> '- 'integer? 'apply 
			  'catch 'length 'eq? 'car '< 'assq 'complex? 'vector-ref 
			  'abs '* 'null? 'imag-part '/ 'vector-set! 'equal? 'magnitude 'real-part 'pair? 'max 'nan? 'string->number 'list
			  'negative? 'cons 'string-set! 'list-ref 'eqv? 'positive? '>= 'expt 'number->string 'zero? 'floor 'denominator 'integer->char 
			  'string? 'min '<= 'char->integer 'cos 'rationalize 'cadr 'sin 'char=? 'map 'list-set! 'defined? 'memq 'string-ref 'log 
			  'for-each 'round 'ceiling 'truncate 'string=? 'atan 'eof-object? 'numerator 'char? 'cosh 'member 'vector 
			  ;;'read-char 'read-byte 'read-line 'read-string 'read ; stdin=>hangs
			  'even? 'string-append 'char-upcase 'sqrt 'make-string
			  'char-alphabetic? 'odd? 'call-with-exit 'tanh 'copy 'sinh 'make-vector
			  'string 'char-ci=? 'caddr 'tan 'reverse 'cddr 'append 'vector? 'list? 'exp 'acos 'asin 'symbol? 'char-numeric? 'string-ci=? 
			  'char-downcase 'acosh 'vector-length 'asinh 'format 'make-list 
			  'sort! 'atanh 'modulo 'make-polar 'gcd 'angle 'remainder 'quotient 'lcm 
			  'char-whitespace? 'assoc 'procedure? 'char<? 
			  'inexact->exact 'vector->list 'boolean? 'undefined? 'unspecified?
			  'caar 'ash 'list-tail 'symbol->string 'string->symbol 'exact->inexact 
			  'object->string 'char>? 'symbol->value 'cadar 'integer-decode-float 'string-copy 'cdddr 'logand 'cadddr 
			  'with-input-from-string 'substring 'string->list 'char-upper-case? 
			  'hash-table-set! 'cddddr 'string<? 'dynamic-wind 'call-with-input-file 'error 
			  ;;'close-output-port 
			  'lognot 'cdar 'char-ci>=? 'string>=? 
			  'dilambda 'string-ci<? 'char<=? 'logior 'char-ci<=? 'assv 
			  'string>? 'char-ci>? 'char-lower-case? 'string-ci>=? 'string-ci>? 'string<=? 'caadr 'char-ci<? 'reverse! 
			  'string-ci<=? 'cadadr 'cdadr 'provided? 'caaaar 'caaddr 'caddar 'cdaaar 'cdaadr 'cdaddr 'cddar 
			  'fill!
			  'hash-table-ref 'list->vector 'caaadr 'caaar 'caadar 'cadaar 'cdadar 'cdddar 'string-fill! 'cdaar 'cddaar 'cddadr 
			  'keyword? 'memv 'char-ready? 
			  'symbol->keyword 'logxor 
			  'exact? 'integer-length 'port-filename 'char>=? 
			  'string-length 'list->string 'inexact? 
			  'with-input-from-file 'type-of
			  'vector-fill! 
			  'symbol 'peek-char 'make-hash-table 
			  'close-input-port ;'current-error-port ;-- spurious output
			  'macro? ;'load
			  'quasiquote 
			  'immutable? 'char-position 'string-position
			  'infinite? 
			  'vector-dimensions 'get-output-string 'sublet
			  'string->keyword 'keyword->symbol 
			  'call-with-input-string 'documentation 
			  'continuation? 'hash-table? 'port-closed? 
			  'output-port? 'input-port? 
			  'provide 'call-with-output-string 
			  ;; 'hash-table 
			  ;; 'checked-hash-table 'checked-hash-table*
			  ;;'current-output-port 
			  'with-output-to-string 
			  ;;'current-input-port -- too many (read...)
			  'symbol-setter ;'unlet -- spurious diffs
			  's7-version 
			  'dilambda?
			  'hook-functions 
#|
			  ;; -------- naming funcs
			  'make-hook 
			  'let 'let* 'letrec 
			  'lambda 'lambda*
			  'multiple-value-bind 'call-with-values
			  'inlet 
			  'object->let
			  ;; --------
|#

                          ;'pair-line-number 
			  'open-input-string 'open-output-string 
			  'open-input-file 
			  'define
			  'newline
			  ;'funclet
			  ;'random 
			  'random-state ;;'port-line-number -- too many spurious diffs
			  'gensym
			  ;'quote
			  'if 'begin 
			  
			  'cond 'case 'or 'and 'do 'with-let 'with-baffle 'when 'unless
			  
			  'let-temporarily
			  'byte-vector-set! 'make-byte-vector 
			  'write-char 'call/cc 'write-byte 'write-string 
			  'file-mtime
			  'letrec*
			  'write 'display 
			  'outlet 
			  'directory->list 
			  'define* 'define-macro 'define-macro* 'define-bacro 'define-bacro*
			  'set! 'set-car! ;'set-cdr!
			  'call-with-output-file 'with-output-to-file 
			  ;'cutlet 
			  ;'set-current-error-port ;-- too many bogus eq? complaints
			  ;'stacktrace ; -- with eval-string, causes stack to grow continuously? (length (stacktrace)) 
			  'signature ; -- circular lists cause infinite loops with (e.g.) for-each??
			  ;'define-constant 
			  ;'curlet ; (length (curlet)) too many times
			  ;'rootlet  ; cyclic-sequences oddness
 			  ;'open-output-file
			  ;'delete-file 'set-current-output-port 
			  ;'autoload ;-- possibly causes stack growth
			  ;'varlet ;-- error exits, chaos in rootlet
			  ;'tree-count 'tree-leaves 'tree-memq 'tree-set-memq ;-- no cycle checks and we have signature creating circular lists
			  ;'eval ; -- can't use if signature (circular program)
			  ;'immutable!
			  ;'procedure-source -- appears to be the culprit when hook body becomes (apply) after optimization
			  'eval-string 
			  ;;'owlet ;too many uninteresting diffs
			  ;'gc
			  ;'reader-cond -- cond test clause can involve unbound vars: (null? i) for example
                          'require ;'help -- snd goes crazy
			  'else '_mac_ '_mac*_ '_bac_ '_bac*_ 
			  '_fnc_ '_fnc*_ '_fnc1_ '_fnc2_ '_fnc3_ '_fnc4_ '_fnc5_
			  'block 'make-block 'block?
			  
			  'constant?
			  'openlet 
			  
			  ;;'cond-expand 
			  '*unbound-variable-hook* '*load-hook* '*rootlet-redefinition-hook* '*missing-close-paren-hook* '*read-error-hook* 
			  '*autoload*
			  ;;'*error-hook*

			  'sequence? 'directory? 'hash-table-entries 
			  ;;'hash-table* -- handled as morally equal
			  'arity 'logbit? 
			  'random-state? 'throw 'float-vector-set! 'make-iterator 'complex 
			  'let-ref 'int-vector 'aritable? 'gensym? 'syntax? 'iterator-at-end? 'let? 
			  'make-shared-vector 'float-vector 'iterator-sequence 'getenv 'float-vector-ref 
			  'cyclic-sequences 'let->list 
			  
			  'setter 'int-vector? 
			  'int-vector-set! 'c-object? 'proper-list? 'symbol->dynamic-value 'vector-append 
			  ;'random-state->list ;'pair-filename 
			  'flush-output-port 'c-pointer 'make-float-vector 
			  'iterate 'float-vector? 
			  'apply-values 'values
			  'byte-vector-ref 'file-exists? 'make-int-vector 'string-downcase 'string-upcase 
			  'byte-vector 'morally-equal? 
			  ;;'let-set! -- rootlet troubles?
			  'c-pointer? 'int-vector-ref ;
			  ;;'coverlet -- blocks block's morally-equal?
			  'float? 
			  'list-values 'byte-vector? 'openlet? 'iterator? 
			  'string->byte-vector

#|
			  's7-profile-info 
			  's7-undefined-identifier-warnings 
                          ;'s7-autoloading? 
			  's7-catches 's7-exits 
			  's7-stack-top 's7-stack 
			  's7-symbol-table 
			  's7-gc-protected-objects

			  's7-set-print-length 
			  's7-set-max-string-length 
			  's7-set-max-list-length 
			  's7-set-max-vector-length 
			  's7-set-max-vector-dimensions
			  's7-set-default-hash-table-length
			  's7-set-initial-string-port-length
			  's7-set-undefined-identifier-warnings 's7-set-autoloading? 's7-set-max-stack-size
			  's7-set-stacktrace-defaults
			  's7-set-gc-stats
			  ;;'s7-set-default-rationalize-error
			  ;;'s7-set-morally-equal-float-epsilon 
			  ;;'s7-set-hash-table-float-epsilon 
			  ;;'s7-set-bignum-precision 
			  ;;'s7-set-float-format-precision
|#
			  's7-memory-usage 
			  ;;'s7-safety 's7-set-safety ;-- these need work...
			  's7-c-types 
			  ;;'s7-history ;-- causes stack to grow?
			  's7-print-length 's7-max-string-length 's7-max-list-length 's7-max-vector-length 's7-max-vector-dimensions 's7-default-hash-table-length
			  's7-initial-string-port-length 's7-history-size
			  's7-default-rationalize-error 's7-morally-equal-float-epsilon
			  's7-hash-table-float-epsilon 's7-bignum-precision 
			  's7-float-format-precision 
			  ;'s7-default-random-state 
			  ;'s7-cpu-time
			  's7-file-names
			  's7-autoloading?
			  's7-rootlet-size 's7-heap-size 's7-free-heap-size 's7-gc-freed 's7-stack-size 's7-max-stack-size 's7-gc-stats
			  's7-stacktrace-defaults

			  ;'macroexpand ;-- uninteresting objstr stuff
			  'block-reverse! 'subblock 'local-symbol? 'unquote 'block-append ;'block-let

			  ;'subsequence 
			  'empty? 'indexable? 'first
			  ;'copy-tree ; cycles cause stack overflow
			  'adjoin 'cdr-assoc
			  ;'progv ;'value->symbol -- correctly different values sometimes, progv localizes
			  'and-let* 'string-case 'hash-table->alist 'concatenate
			  'union '2^n? 'lognor 'ldb 'clamp 
			  'sequence->string 
			  ;'*s7*->list ; reverse! etc 
			  'log-n-of ; stack grows if n < 0?
			  'pp

			  'kar '_dilambda_ '_vals_
			  'free1 'free2 'free3

			  'tree-cyclic?
			  ))
	 
      (args (vector "-123" "1234" "-3/4" "-1" "(expt 2 32)" "4294967297" "1001" "10001" "(+ a 1)" "(- a 1)" "(logand (ash 1 b) a)"
		    "(make-block 2)" "(block 1.0 2.0 3.0)" "(block)"
		    "\"ho\"" ":ho" "'ho" "':go" "(list 1)" "(list 1 2)" "(cons 1 2)" "'()" "(list (list 1 2))" "(list (list 1))" "(list ())" "=>" 
		    "#f" "#t" "()" "#()" "\"\"" "'#()" ":readable" ":rest" ":allow-other-keys" ":a" ;"__func__"
		    "1/0+i" "0+0/0i" "0+1/0i" "1+0/0i" "0/0+0/0i" "0/0+i" 
		    "cons" "''2" "\"ra\""
		    "(make-hook)" "(make-hook '__x__)"
		    "1+i" "0+i" "(ash 1 43)" 
		    "(integer->char 255)" "(string (integer->char 255))" "(string #\\null)" "(byte-vector 0)"
		    ;;"most-positive-fixnum" "most-negative-fixnum"
		    "pi" "nan.0" "inf.0"
		    "(list)" "(string)" "#r()" "#u8()" "(vector)" "#i()" "(make-iterator #(10 20))" "#i(1)"
		    "0" "1" "4" "1.0" "-1.0" "1.0+123.0i" "3/4" "(make-vector 3)" "(make-string 3)" "(make-vector '(2 3))"
		    "'((111 2222) (3 4))" "'((1 (2)) (((3) 4)))" "(byte-vector 255)" 
		    "#(123 223)" "(vector 1 '(3))" "(let ((x 3)) (lambda (y) (+ x y)))" "abs" "(lambda sym-args sym-args)" "#u8(0 1)"
		    "(dilambda (lambda () 1) (lambda (a) a))" "quasiquote" "macroexpand" "(lambda* ((a 1) (b 2)) (+ a b))" 
		    "((lambda (a) (+ a 1)) 2)" "((lambda* ((a 1)) (+ a 1)) 1)" "(lambda (a) (values a (+ a 1)))" "((lambda (a) (values a (+ a 1))) 2)"
		    "(define-macro (_m1_ a) `(+ ,a 1))" "(define-bacro (_b1_ a) `(* ,a 2))"
		    "(string #\\c #\\null #\\b)" "#2d((100 200) (3 4))" "#r(0 1)" "#i2d((101 201) (3 4))" "#r2d((.1 .2) (.3 .4))" "#i1d(15 25)"
		    "(values 1 2)" "(values)" "(values #\\c 3 1.2)" "(values \"ho\")"
		    "`(x)" "`(+ x 1)" "`(x 1)" "`((x))" "`((+ x 1))" "`(((+ x 1)))" "`((set! x (+ x 1)) (* x 2))" "`((x 1))" "`(((x 1))) "
		    "`(x . 1)" "`((x . 1))" "`(1)" "`((1))" "`((1) . x)" "'(- 1)" "(+ i 1)"
		    ;;"'((X . 1) . 2)" "'((x 1) . 2)" "'((x 1) (y . 2))" "'((x 1) y . 2)" "'((x 1) (y) . 2)" "'((x 1 . 2) . 3)" "'((x 1) 2)" "'(((x 1) 2) 3)" 
		    "'(())" "'((()))" "(random-state 1234)" 
		    "(c-pointer 0 'integer?)" "(c-pointer -1)" "(c-pointer 1234)"
		    "(inlet 'integer? (lambda (f) #f))" "(inlet 'a 1)" "(openlet (inlet 'abs (lambda (x) (- x))))" 
		    "'(15 26 . 36)" 
		    " . "
		    "((i 0 (+ i 1)))" "(null? i)" 
		    "(= i 2)" "(zero? i)" "((null? i) i)" "(#t ())" 
		    "(x => y)" "((0 1) ())" "(- i 1)" "(if x y)" "(or x y)"
		    ;;"(f x) i" "x y z" "1 2"
		    "`(+ ,a ,@b)" "`(+ ,a ,b)" "`(+ ,a ,b ,@c)" "`(+ ,a b ,@c ',d)"
		    "_definee_" "(_definee_ __var__)" "(_definee_ x)" 
		    "(hash-table* 'a 1)" "(hash-table)" 
		    "(make-iterator (list 11 22 33))" "(make-iterator (vector 1 2 3))" "(make-iterator (string #\\1))" "(make-iterator x)" 
		    "#<eof>" "#<undefined>" "#<unspecified>"
		    "#o123" "#b101" "#\\newline" "#\\alarm" "#\\delete" "#_cons" "#x123.123" "#\\x65" ;"_1234_" "kar"
		    
		    "(call-with-exit (lambda (goto) goto))"
		    "(with-baffle (call/cc (lambda (cc) cc)))"
		    "(symbol->string 'x)" "(symbol \"a b\")" "(symbol \"(\\\")\")"
		    ;;"(setter _definee_)"
		    "(setter car)" "(setter kar)"
		    "(call-with-exit (lambda (return) (let ((x 1) (y 2)) (return x y))))"
		    "(call/cc (lambda (return) (let ((x 1) (y 2)) (return x y))))"
		    "(let ((x 1)) (dynamic-wind (lambda () (set! x 2)) (lambda () (+ x 1)) (lambda () (set! x 1))))"

		    "(let ((x 1)) (free1))" "(let ((x #(1)) (i 0)) (free2))" "(let () (define (local-func x) x) (free3))"

		    "1+1e10i" "1e15+1e15i" "0+1e18i" "1e18" 
		    ;;"(real-part (random 0+i))" -- (cond (real-part...))!
		    ;;"(random 1.0)" ; number->string so lengths differ
		    "(random 1)"
		    ;;"(else ())" "(else (f x) B)"
		    "(else)"
		    "else" "x" "(+ x 1)" "(+ 1/2 x)" "(abs x)" "(+ x 1 2+i)" "(* 2 x 3.0 4)" "((x 1234))" "((x 1234) (y 1/2))" "'x" "(x 1)"
		    "_undef_" "(begin |undef1|)"
		    "+signature+" "+documentation+" "+setter+" 
		    "__var2__"
		    ;; "\"~S~%\"" "\"~A~D~X\"" "\"~{~A~^~}~%\"" "\"~NC~&\"" -- creates files by these names!
		    "ims" "imbv" "imv" "imiv" "imfv" "imi" "imb" "imh" ;;"imp"--many ways to cloober this
		    ;"(immutable! (string #\\a #\\b #\\c))" "(immutable! (byte-vector 0 1 2))" 
		    ;"(immutable! (vector 0 1 2))" "(immutable! (int-vector 0 1 2))" "(immutable! (float-vector 0 1 2))" 
		    ;"(immutable! (inlet 'a 1 'b 2))" 
		    ;"(immutable! (block 0.0 1.0 2.0))" 
		    ;"(immutable! (hash-table* 'a 1 'b 2))"
		    ;"(immutable! (cons 0 (immutable! (cons 1 (immutable! (cons 2 ()))))))"
		    ;"(immutable! 'x)"

		    ;;"(make-list 16 0)" "(make-vector 16 0)" "(make-int-vector 16 0)" "(make-float-vector 16 0)" "(make-byte-vector 16 0)"
		    ;;"(make-string 16 #\\0)"
		    ;;"(let ((hash (make-hash-table))) (do ((i 0 (+ i 1))) ((= i 16) hash) (hash-table-set! hash i 0)))"
		    ;;"(let ((lt (inlet))) (do ((i 0 (+ i 1))) ((= i 16) lt) (varlet lt (symbol \"i\" (number->string i)) 0)))"
		    
		    ;;" #| a comment |# "
		    "(make-shared-vector (vector 0 1 2 3 4) 3)" "(substring \"0123\" 2)"
		    "(vector-dimensions (block))" 
		    "(append (block) (block))"
		    "(let-temporarily ((x 1234)) (+ x 1))"
		    "(error 'oops \"an error!\")"

		    ;;"(catch #t 1 (lambda (a b) b))" "(catch #t (lambda () (fill! (rootlet) 1)) (lambda (type info) info))"

		    "#xfeedback" "#_asdf"
		    ;"quote" "'"
		    "if" "begin" "cond" "case" "when" "unless" "letrec" "letrec*" "or" "and" "let-temporarily"
		    "lambda*" "lambda"
		    ;;"let" "let*" "do" "set!" "with-let" ;"define" "define*" "define-macro" "define-macro*" "define-bacro" "define-bacro*"

		    "(begin (string? (stacktrace)))"

		    "(let ((<1> (vector #f))) (set! (<1> 0) <1>) <1>)"
		    "(let ((<1> (inlet :a #f))) (set! (<1> :a) <1>) <1>)"
		    "(let ((<1> (hash-table*))) (set! (<1> 'a) <1>) <1>)"
		    "(let ((<1> #f) (<2> (vector #f))) (set! <1> (make-iterator <2>)) (set! (<2> 0) <1>) <1>)"
		    "(let ((<1> (list 1 #f))) (set! (<1> 1) (let ((<L> (list #f 3))) (set-car! <L> <1>) <L>)) <1>)"

		    ))

      (codes (vector 
	      (list "(do ((x 0) (i 0 (+ i 1))) ((= i 1) x) (set! x " "(let ((x 0) (i 0)) (set! x ")
	      ;;(list "(let () (let () " "((lambda () ")
	      (list "((lambda x " "((lambda* ((x ())) ")
	      ;;(list "((lambda* ((x 1)) " "(let* ((_aaa_ 1) (x _aaa_)) (begin ")
	      (list "(cond (else " "(case x (else ")
	      (list "(case false ((#f) " "(case false ((1) #t) (else ")
	      (list "(call-with-exit (lambda (_x_) " "(call/cc (lambda (_x_) ")
	      (list "(if (not x) (begin " "(if x #<unspecified> (begin ")
	      (list "(cond ((not false) " "(unless false (begin ")
	      (list "(let () (let-temporarily ((x 1)) " "(let () (let ((x 1)) ")
	      (list "(let-temporarily ((x 1)) (call-with-exit (lambda (go) "
		    "(call-with-exit (lambda (go) (let-temporarily ((x 1)) ")
	      (list "(begin (_dw_ " "((lambda () ")
	      (list "(begin (append " "(apply append (list ")
	      (list "(begin (with-let (inlet 'i 0) " "(with-let (inlet) (let ((i 0)) ")
	      (list "(list (_cw_ " "(list (values ")
	      (list "(set! __var1__ (list " "(list (let () ")
	      (list "(do () ((not false) " "(begin (when (not false) ")
	      ;;(list "((define-macro (_m_) " "((define-bacro (_m_) ") ; circular source if signature in body
	      ;;(list "(let ((mx max)) (let ((max min) (min mx)) " "(begin (_lt2_ ") ; loops?
	      ;;(list "(begin (letrec ((x 1)) " "(begin (letrec* ((x 1)) ")
	      (reader-cond ((not (provided? 'pure-s7)) (list "(with-input-from-string \"1234\" (lambda () " "(begin (_dw_string_ ")))
	      (list "(map abs (begin " "(map (lambda (x) (if (>= x 0.0) x (- x))) (begin ")
	      (list "(for-each display (list " "(for-each (lambda (x) (display x)) (list ")
	      (list "(begin (_ct1_ " "(begin (_ct2_ ")
	      (list "(begin (_mem1_ " "(begin (_mem2_ ")
	      (list "(begin (_ft1_ " "(begin (_ft2_ ")
	      (list "(begin (_fa1_ " "(begin (_fa2_ ")
	      (list "(with-output-to-string (lambda () " "(begin (_dw_out_ ")
	      (list "(begin (_rf1_ " "(begin (_rf2_ ")
	      (list "(let () (_do1_ " "(let () (_do2_ ")
	      (list "(let () (let-temporarily ((x 1234)) (call-with-exit (lambda (goto) (goto 1))) "
		    "(let () (let-temporarily ((x 1234)) (call/cc (lambda (goto) (goto 1))) ")
	      (list "(let ((x 1)) (immutable! 'x) (begin " "((lambda* ((x 1)) (immutable! 'x) ")
	      (list "(do ((i 0 (+ i 1))) ((= i 1)) (do ((j 0 (+ j 1))) ((= j 1)) "
		    "(do ((i 0 (+ i 1))) ((= i 1)) (let ((j 0)) ")
	      (list "(or (_cop1_ " "(and (_cop2_ ")
	      (list "(values (_rd1_ " "(append (_rd2_ ")
	      ))
      
      (chars (vector #\( #\( #\) #\space))) ; #\/ #\# #\, #\` #\@ #\. #\:))  ; #\\ #\> #\space))
  (let ((clen (length chars))
	(flen (length functions))
	(alen (length args))
	(codes-len (length codes)))

    ;(for-each (lambda (x) (if (not (symbol? x)) (format *stderr* "~A " x))) functions)
    ;(for-each (lambda (x) (if (not (string? x)) (format *stderr* "~A " x))) args)
    ;(do ((p (vector->list functions) (cdr p))) ((null? p)) (if (memq (car p) (cdr p)) (format *stderr* "~A repeats~%" (car p))))
    ;(do ((p (vector->list args) (cdr p))) ((null? p)) (if (member (car p) (cdr p) string=?) (format *stderr* "~A repeats~%" (car p))))

    ;;(let ((st (symbol-table))) (for-each (lambda (x) (if (and (procedure? (symbol->value x)) (not (memq x (vector->list functions)))) (format *stderr* "~A~%" x))) st))

    (define (fix-op op)
      (case op
	((set!) "set! __var__")
	((let) "let ()")
	((let*) "let* ()")
	((do) "_do3_")
	((call-with-output-file) "call-with-output-file \"/dev/null\" ")
	((with-output-to-file) "with-output-to-file \"/dev/null\" ")
	((define define* define-macro define-macro* define-bacro define-bacro*) (format #f "~A _definee_ " op))
	((eval) "checked-eval")
	(else => symbol->string)))

    (define make-expr
      (let ((parens 1)
	    (dqs 0)
	    (j 1)
	    (str (make-string 2048 #\space)))
	(lambda (size)
	  (set! parens 1)
	  (set! dqs 0)
	  (set! j 1)
	  (fill! str #\space)
	  (set! (str 0) #\()

	  (let ((opstr (fix-op (functions (random flen)))))
	    (do ((oplen (length opstr))
		 (n 0 (+ n 1))
		 (k j (+ k 1)))
		((= n oplen) 
		 (set! j k))
	      (string-set! str k (string-ref opstr n))))
	  
	  (set! (str j) #\space)
	  (set! j (+ j 1))
	  
	  (do ((k 1 (+ k 1)))
	      ((= k size))
	    
	    (set! (str j) (chars (random clen)))
	    (if (= dqs 1)
		(if (and (char=? (str j) #\")
			 (or (= j 0)
			     (not (char=? (str (- j 1)) #\\))))
		    (set! dqs 0))
		
		;; else not in a string constant
		(case (str j)
		  ((#\()
		   (set! parens (+ parens 1))
		   (let* ((op (functions (random flen)))
			  (opstr (fix-op op)))
		     (do ((oplen (length opstr))
			  (n 0 (+ n 1))
			  (k (+ j 1) (+ k 1)))
			 ((= n oplen) 
			  (set! j k))
		       (string-set! str k (string-ref opstr n))))
		   (set! j (+ j 1))
		   (set! (str j) #\space))
		  
		  ((#\))
		   (set! parens (- parens 1))
		   (when (negative? parens)
		     (set! (str j) #\space)
		     (set! parens 0)))
		  
		  ((#\space)
		   (let ((nargs (random 5)))
		     (do ((n 0 (+ n 1)))
			 ((= n nargs))
		       (let ((argstr (args (random alen))))
			 (do ((arglen (length argstr))
			      (n 0 (+ n 1))
			      (k (+ j 1) (+ k 1)))
			     ((= n arglen)
			      (set! j k))
			   (string-set! str k (string-ref argstr n))))
		       (set! j (+ j 1))
		       (set! (str j) #\space))))
		  
		  ((#\")
		   (set! dqs 1))))
	    
	    (set! j (+ j 1)))
	  
	  (if (= dqs 1)
	      (begin
		(set! (str j) #\")
		(set! j (+ j 1))))
	  
	  (if (> parens 0)
	      (do ((k parens (- k 1))
		   (n j (+ n 1)))
		  ((= k 0)
		   (set! j n))
		(string-set! str n #\))))
	  
					;(format #t "~A~%" (substring str 0 j))
	  (substring str 0 j))))

    (define (same-type? val1 val2 val3 val4 str str1 str2 str3 str4)
      (if (and (eq? (type-of val1) (type-of val2))
	       (eq? (type-of val1) (type-of val3))
	       (eq? (type-of val1) (type-of val4)))
	  (unless (or (openlet? val1)
		      (string-position "(set!" str1)
		      (string-position "gensym" str1))

	    (cond ((symbol? val1)
		   (if (gensym? val1)
		       (unless (and (gensym? val2)
				    (gensym? val3)
				    (gensym? val4))
			 (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%   ~S ~S ~S ~S~%" 
				 str str1 str2 str3 str4 
				 val1 val2 val3 val4))
		       (unless (and (eq? val1 val2)
				    (eq? val1 val3)
				    (eq? val1 val4))
			 (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%   ~S ~S ~S ~S~%" 
				 str str1 str2 str3 str4 
				 val1 val2 val3 val4))))

		  ((sequence? val1)
		   (let ((len1 (length val1)))
		     (unless (or (let? val1)
				 (and (eqv? len1 (length val2))
				      (eqv? len1 (length val3))
				      (eqv? len1 (length val4))))
		       (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%    ~S~%    ~S~%    ~S~%    ~S~%~%" 
			       str str1 str2 str3 str4 
			       val1 val2 val3 val4))
		     (if (and (string? val1)
			      (not (and (eq? (byte-vector? val1) (byte-vector? val2))
					(eq? (byte-vector? val1) (byte-vector? val3))
					(eq? (byte-vector? val1) (byte-vector? val4)))))
			 (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%    ~S~%    ~S~%    ~S~%    ~S~%~%" 
				 str str1 str2 str3 str4 
				 val1 val2 val3 val4))))

		  ((number? val1)
		   (if (or (and (nan? val1)
				(not (and (nan? val2) (nan? val3) (nan? val4))))
			   (and (infinite? val1)
				(not (and (infinite? val2) (infinite? val3) (infinite? val4))))
			   (and (finite? val1)
				(not (and (finite? val2) (finite? val3) (finite? val4))))
			   (and (real? val1) (real? val2) (real? val3) (real? val4) 
				(or (and (negative? val1) (or (positive? val2) (positive? val3) (positive? val4)))
				    (and (positive? val1) (or (negative? val2) (negative? val3) (negative? val4))))))
		       (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%    ~S~%    ~S~%    ~S~%    ~S~%~%" 
			       str str1 str2 str3 str4 
			       val1 val2 val3 val4)))

		  ((or (boolean? val1)
		       (syntax? val1)
		       (unspecified? val1)
		       (char? val1)
		       (eof-object? val1)
		       (null? val1))
		   (unless (and (eq? val1 val2)
				(eq? val1 val3)
				(eq? val1 val4))
		     (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%   ~S ~S ~S ~S~%" 
			     str str1 str2 str3 str4 
			     val1 val2 val3 val4)))

		  ((or (undefined? val1)
		       (c-object? val1))
		   (unless (and (equal? val1 val2)
				(equal? val1 val3)
				(equal? val1 val4))
		     (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%   ~S ~S ~S ~S~%" 
			     str str1 str2 str3 str4 
			     val1 val2 val3 val4)))
#|
		  ((iterator? val1) ; hash input block let 
		   (unless (or (memq (type-of (iterator-sequence val1)) '(hash-table? let?))
			       (and (morally-equal? val1 val2)
				    (morally-equal? val1 val3)
				    (morally-equal? val1 val4)))
		     (format *stderr* "~%~%~S~%~S~%~S~%~S~%~S~%   ~S ~S ~S ~S~%" 
			     str str1 str2 str3 str4 
			     val1 val2 val3 val4)))
|#
		  ))
	  (begin
	    (format *stderr* "~%~%~S~%~S~%~S~%~S~%    ~S~%    ~S~%    ~S~%    ~S~%" 
		    str1 str2 str3 str4 
		    val1 val2 val3 val4)
	    (if (or (eq? val1 'error)
		    (eq? val2 'error)
		    (eq? val3 'error)
		    (eq? val4 'error))
		(format *stderr* "    ~S ~S~%" 
			error-type error-info))
	    ;(format *stderr* "~S~%~%" (stacktrace))
	    )))

    (define (eval-it str) ;(format #t "~A~%" str)
      ;(format *stderr* "~S~%" str)
      (catch #t 
	(lambda ()
	  (car (list (eval-string str))))
	(lambda (type info)
	  (set! error-type type)
	  (set! error-info info)
          'error)))

    (define (try-both str)
      (set! ostr str)

      (catch #t 
	(lambda () 
	  (s7-optimize (list (catch #t 
			       (lambda ()
				 (with-input-from-string str read))
			       (lambda args ())))))
	(lambda arg 'error))
#|
      (catch #t
	(lambda ()
	  (let ((val1 (car (list (eval-string str)))))
	    (let ((newstr (object->string val1 :readable)))
	      (let ((val2 (eval-string newstr)))
		(if (not (morally-equal? val1 val2))
		    (format *stderr* "~%~S~%~S~%    ~W -> ~W~%" str newstr val1 val2))))))
	(lambda arg 'error))
|#
#|
      (catch #t
	(lambda ()
	  (let ((val1 (list (eval-string str))))
	    (let ((val2 (list (eval (with-input-from-string str read)))))
	      (call-with-exit 
	       (lambda (ok)
		 (for-each
		  (lambda (x y)
		    (unless (and (morally-equal x y)
				 (not (gensym? x)))
		      (format *stderr* "eval/string ~%~S~%    ~W -> ~W~%" str val1 val2)
		      (ok)))
		  val1 val2))))))
	(lambda arg 'error))
|#
#|
      (catch #t
	(lambda ()
	  (pretty-print (with-input-from-string str read)))
	(lambda arg 'error))
|#	    
      (let* ((outer (codes (random codes-len)))	     
	     (str1 (string-append "(let ((x #f) (i 0)) " (car outer) str ")))"))
	     (str2 (string-append "(let () (define (func) " str1 ") (define (hi) (func)) (hi))"))
	     (str3 (string-append "(let ((x #f) (i 0)) " (cadr outer) str ")))"))
	     (str4 (string-append "(let () (define (func) " str3 ") (define (hi) (func)) (hi))")))
	(let ((val1 (eval-it str1))
	      (val2 (eval-it str2))
	      (val3 (eval-it str3))
	      (val4 (eval-it str4)))
	  (same-type? val1 val2 val3 val4 str str1 str2 str3 str4)))

      (let ((nstr (make-expr (+ 1 (random 4)))))
	(set! nostr nstr)
	(catch #t 
	  (lambda () 
	    (s7-optimize (list (catch #t 
				 (lambda ()
				   (with-input-from-string nstr read))
				 (lambda args ())))))
	  (lambda arg 'error))
	(let ((str5 (string-append "(let ((_y_ (begin " nstr "))) (define (f x) " str ") (define (g) (f _y_)) (g))"))
	      (str6 (string-append "((lambda (x) " str ") (begin " nstr "))"))
	      (str7 (string-append "(let ((x (begin " nstr "))) " str ")"))
	      (str8 (string-append "(do ((x (begin " nstr "))) (#t " str "))")))
	  (let ((val5 (eval-it str5))
		(val6 (eval-it str6))
		(val7 (eval-it str7))
		(val8 (eval-it str8)))
	    (same-type? val5 val6 val7 val8 nstr str5 str6 str7 str8)))
	(let ((str5 (string-append "(let ((x 0)) (cond ((not ((lambda args (car args)) (let () " nstr "))) #<unspecified>) (else " str ")))"))
	      (str6 (string-append "(let ((x 0)) (when (let () " nstr ") " str "))"))
	      (str7 (string-append "(let ((x 0)) (unless (not ((lambda args (car args)) (let () " nstr "))) " str "))"))
	      (str8 (string-append "(let ((x 0)) (cond ((let () " nstr ") " str ")))")))
	  (let ((val5 (eval-it str5))
		(val6 (eval-it str6))
		(val7 (eval-it str7))
		(val8 (eval-it str8)))
	    (same-type? val5 val6 val7 val8 nstr str5 str6 str7 str8)))
#|
	(let ((mstr (make-expr (+ 1 (random 4)))))
	  (let* ((str5 (string-append "(let () (if (begin " mstr " ) (begin " nstr ") (begin " str ")))"))
		 (str6 (string-append "(let () (define (func) " str5 ") (define (hi) (func)) (hi))"))
		 (str7 (string-append "(let () (cond ((begin " mstr ") " nstr ") (else " str ")))"))
		 (str8 (string-append "(let () (define (func) " str7 ") (define (hi) (func)) (hi))")))
	    (let ((val5 (eval-it str5))
		  (val6 (eval-it str6))
		  (val7 (eval-it str7))
		  (val8 (eval-it str8)))
	      (same-type? val5 val6 val7 val8 nstr str5 str6 str7 str8))))
|#
	))

    (define dots (vector "." "-" "+" "-"))
    (define (test-it)
      (do ((m 0 (+ m 1))
	   (n 0))
	  ((= m 100000000) 
	   (format *stderr* "reached end of loop??~%"))
	
	(when (zero? (modulo m 100000))
	  (set! m 0)
	  (set! n (+ n 1))
	  (if (= n 4) (set! n 0))
	  (format *stderr* "~A" (vector-ref dots n)))

	(try-both (make-expr (+ 1 (random 4)))) ; min 1 here not 0
	(set! __var__ #f)
	))

    (test-it)))

;;; arity/signature checks? -- need access to returning caller

