;;; this is an extension of tauto.scm, an auto-tester

;(set! (hook-functions *load-hook*) (list (lambda (hook) (format () "loading ~S...~%" (hook 'name)))))

(load "stuff.scm")

(set! (*s7* 'max-stack-size) 32768)
(set! (*s7* 'gc-stats) 6)
(define ostr "")
(set! (*s7* 'max-string-length) 100000)
(set! (*s7* 'max-list-length) 100000)
(set! (*s7* 'max-vector-length) 100000)
(set! (*s7* 'max-vector-dimensions) 10)
(set! (current-output-port) #f)
(define __var__ #f)
(define __var1__ #f)
(define error-type #f)
(define error-info #f)
(define false #f)

(define (s7-print-length) (*s7* 'print-length))
(define (s7-max-string-length) (*s7* 'max-string-length))
(define (s7-max-list-length) (*s7* 'max-list-length))
(define (s7-max-vector-length) (*s7* 'max-vector-length))
(define (s7-max-vector-dimensions) (*s7* 'max-vector-dimensions))
(define (s7-default-hash-table-length) (*s7* 'default-hash-table-length))
(define (s7-initial-string-port-length) (*s7* 'initial-string-port-length))
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
(define-bacro (_mac_ x) `(+ ,x 1))
(define-bacro* (_mac*_ (x 1)) `(+ ,x 1))
(define (_fnc_ x) (+ x 1))
(define* (_fnc*_ (x 1)) (+ x 1))
(define (_fnc1_ x) (apply + (list x 1)))

(define (checked-eval code)
  (and (null? (cyclic-sequences code))
       (eval code)))

(define (checked-cutlet e . args)
  (or (eq? e (curlet))
      (eq? e (rootlet))
      (null? e)
      (apply cutlet e args)))

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

(define-expansion (_cw_ . args)
  `(call-with-exit (lambda (_x_) (_x_ ,@args))))

;(define 1- #f)
;(define 1+ #f)
(set! (hook-functions *unbound-variable-hook*) ())

(define max-stack (*s7* 'stack-top))

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
			  'inexact->exact 'vector->list 'boolean? 
			  'caar 'ash 'list-tail 'symbol->string 'string->symbol 'exact->inexact 
			  'object->string 'char>? 'symbol->value 'cadar 'integer-decode-float 'string-copy 'cdddr 'logand 'cadddr 
			  'with-input-from-string 'substring 'string->list 'char-upper-case? 
			  'hash-table-set! 'cddddr 'string<? 'dynamic-wind 'call-with-input-file 'error 
			  ;;'close-output-port 
			  'lognot 'cdar 'char-ci>=? 'string>=? 
			  'dilambda 'string-ci<? 'char<=? 'logior 'char-ci<=? 'assv 
			  'string>? 'char-ci>? 'char-lower-case? 'string-ci>=? 'string-ci>? 'string<=? 'caadr 'char-ci<? 'reverse! 
			  'string-ci<=? 'cadadr 'cdadr 'provided? 'caaaar 'caaddr 'caddar 'cdaaar 'cdaadr 'cdaddr 'cddar 'fill!
			  'hash-table-ref 'list->vector 'caaadr 'caaar 'caadar 'cadaar 'cdadar 'cdddar 'string-fill! 'cdaar 'cddaar 'cddadr 
			  'keyword? 'memv 'char-ready? 
			  'symbol->keyword 'logxor 
			  'exact? 'integer-length 'port-filename 'char>=? 
			  'string-length 'list->string 'inexact? 
			  'with-input-from-file 'type-of
			  'vector-fill! 
			  'symbol 'peek-char 'make-hash-table 
			  'close-input-port 'current-error-port 'macro? ;'load
			  'quasiquote 
			  'immutable? 'char-position 'string-position
			  'infinite? 
			  'vector-dimensions 'get-output-string 'sublet
			  'string->keyword 'keyword->symbol 
			  'call-with-input-string 'documentation 
			  'continuation? 'hash-table? 'port-closed? 
			  'output-port? 'input-port? 
			  'provide 'call-with-output-string 'hash-table 
			  ;;'current-output-port 
			  'with-output-to-string 
			  ;;'current-input-port -- too many (read...)
			  'symbol-setter 'unlet 
			  's7-version 
			  'dilambda?
			  'hook-functions 'make-hook 
			  'open-input-string 'open-output-string 
			  'open-input-file 
			  'define
			  'newline
			  'funclet
			  ;'random 
			  'random-state 'port-line-number 'gensym
			  'quote 'if 'begin 'let 'let* 'letrec 'cond 'case 'or 'and 'do 'with-let 'with-baffle 'when 'unless
			  'lambda 'lambda* 'let-temporarily
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
			  'cutlet 
			  ;'set-current-error-port -- too many bogus eq? complaints
			  ;'stacktrace ; -- with eval-string, causes stack to grow continuously? (length (stacktrace)) 
			  'signature ; -- circular lists cause infinite loops with (e.g.) for-each??
			  ;'define-constant 
			  ;'curlet ; (length (curlet)) too many times
			  ;'rootlet  ; cyclic-sequences oddness
 			  ;'open-output-file
			  ;'delete-file 'set-current-output-port 
			  ;'autoload ;-- possibly causes stack growth
			  ;'varlet ;-- error exits
			  ;'tree-count 'tree-leaves 'tree-memq 'tree-set-memq ;-- no cycle checks and we have signature creating circular lists
			  ;'eval ; -- can't use if signature (circular program)
			  ;'immutable!
			  ;'procedure-source -- appears to be the culprit when hook body becomes (apply) after optimization
			  'eval-string 
			  ;;'owlet ;too many uninteresting diffs
			  ;'gc
			  'openlet 
			  ;'reader-cond -- cond test clause can involve unbound vars: (null? i) for example
                          'require 'help 
			  'else '_mac_ '_mac*_ '_bac_ '_bac*_ 
			  '_fnc_ '_fnc*_ '_fnc1_
			  'block 'make-block 'block? 'empty
			  'reactive-let

			  'constant?
			  'openlet 'multiple-value-bind 'call-with-values
			  'cond-expand 
			  '*unbound-variable-hook* '*load-hook* '*rootlet-redefinition-hook* '*missing-close-paren-hook* '*read-error-hook* 
			  '*autoload*
			  ;;'*error-hook*

			  'sequence? 'directory? 'hash-table-entries 'hash-table* 'arity 'logbit? 
			  'random-state? 'throw 'float-vector-set! 'make-iterator 'complex 
			  'let-ref 'int-vector 'aritable? 'gensym? 'syntax? 'iterator-at-end? 'let? 
			  'make-shared-vector 'float-vector 'iterator-sequence 'getenv 'float-vector-ref 
			  'cyclic-sequences 'let->list 'inlet 'setter 'int-vector? 
			  'int-vector-set! 'dilambda 'c-object? 'proper-list? 'symbol->dynamic-value 'vector-append 
			  'random-state->list 'pair-filename 'flush-output-port 'c-pointer 'make-float-vector 
			  'object->let 'pair-line-number 'iterate 'float-vector? 
			  'apply-values 'values
			  'byte-vector-ref 'file-exists? 'make-int-vector 'string-downcase 'string-upcase 
			  'byte-vector 'morally-equal? 'let-set! 'c-pointer? 'int-vector-ref 'coverlet 'float? 
			  'list-values 'random-state 'byte-vector? 'openlet? 'iterator? 
			  'string->byte-vector

			  's7-memory-usage
#|
			  ;'s7-print-length 's7-max-string-length 's7-max-list-length 's7-max-vector-length 's7-max-vector-dimensions 's7-default-hash-table-length
			  ;'s7-initial-string-port-length 's7-history-size 
			  's7-profile-info 
			  's7-undefined-identifier-warnings 
                          ;'s7-autoloading? 
			  's7-history
			  's7-catches 's7-exits 's7-c-types 
			  's7-stack-top 's7-stack 's7-stacktrace-defaults 
			  's7-symbol-table 
			  's7-gc-protected-objects
			  ;'s7-rootlet-size 's7-heap-size 's7-free-heap-size 's7-gc-freed 's7-stack-size 's7-max-stack-size 
			  ;'s7-gc-stats
			  

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
|#
			  ;;'s7-history -- causes stack to grow?
			  's7-print-length 's7-max-string-length 's7-max-list-length 's7-max-vector-length 's7-max-vector-dimensions 's7-default-hash-table-length
			  's7-initial-string-port-length 's7-history-size
			  's7-default-rationalize-error 's7-morally-equal-float-epsilon
			  's7-hash-table-float-epsilon 's7-bignum-precision 
			  ;;'s7-float-format-precision 
			  's7-default-random-state 
			  ;'s7-cpu-time
			  's7-file-names
			  's7-autoloading?
			  's7-rootlet-size 's7-heap-size 's7-free-heap-size 's7-gc-freed 's7-stack-size 's7-max-stack-size 's7-gc-stats

			  ;;'s7-set-default-rationalize-error
			  ;;'s7-set-morally-equal-float-epsilon 
			  ;;'s7-set-hash-table-float-epsilon 
			  ;;'s7-set-bignum-precision 
			  ;;'s7-set-float-format-precision

			  'macroexpand 'block-reverse! 'subblock 'local-symbol? 'unquote 'unspecified? 'block-append 'undefined? 

			  ;'subsequence 
			  'empty? 'indexable? 'first 'cdr* 
			  ;'copy-tree ; cycles cause stack overflow
			  'adjoin 'cdr-assoc
			  'progv ;'value->symbol -- correctly different values sometimes
			  'and-let* 'string-case 'hash-table->alist 'concatenate
			  'union '2^n? 'lognor 'ldb 'clamp 
			  'sequence->string 
			  ;'*s7*->list ; reverse! etc 
			  'log-n-of ; stack grows if n < 0?

			  ))
	 
      (args (vector "-123" "1234" "-3/4" "-1" "(expt 2 32)" "4294967297" "(+ a 1)" "(- a 1)" "(logand (ash 1 b) a)"
		    "(make-block 2)" "(block 1.0 2.0 3.0)" "(block)"
		    "\"ho\"" ":ho" "'ho" "':go" "(list 1)" "(list 1 2)" "(cons 1 2)" "'()" "(list (list 1 2))" "(list (list 1))" "(list ())" "=>" 
		    "#f" "#t" "()" "#()" "\"\"" "'#()" ":readable" ":rest" ":allow-other-keys" ":a" ;"__func__"
		    ;;"1/0+i" "0+0/0i" "0+1/0i" "1+0/0i" "0/0+0/0i" "0/0+i" 
		    "cons" "''2" 
		    "(make-hook)" "(make-hook '__x__)"
		    "1+i" "0+i" 
		    "(integer->char 255)" "(string (integer->char 255))" 
		    ;;"most-positive-fixnum" "most-negative-fixnum"
		    "pi" "nan.0" "inf.0"
		    "(list)" "(string)" "#r()" "#u8()" "(vector)" "#i()" "(make-iterator #(1 2))" "#i(1)"
		    "0" "1" "1.0" "-1.0" "1.0+123.0i" "3/4" "(make-vector 3)" "(make-string 3)" "(make-vector '(2 3))"
		    "'((1 2) (3 4))" "'((1 (2)) (((3) 4)))" "(byte-vector 255)" 
		    "#(1 2)" "(vector 1 '(3))" "(let ((x 3)) (lambda (y) (+ x y)))" "abs" "(lambda sym-args sym-args)" "#u8(0 1)"
		    "(dilambda (lambda () 1) (lambda (a) a))" "quasiquote" "macroexpand" "(lambda* ((a 1) (b 2)) (+ a b))" 
		    "((lambda (a) (+ a 1)) 2)" "((lambda* ((a 1)) (+ a 1)) 1)" "(lambda (a) (values a (+ a 1)))" "((lambda (a) (values a (+ a 1))) 2)"
		    "(define-macro (_m1_ a) `(+ ,a 1))"
		    "(string #\\c #\\null #\\b)" "#2d((1 2) (3 4))" "#r(0 1)" "#i2d((1 2) (3 4))" "#r2d((.1 .2) (.3 .4))" "#i1d(1 2)"
		    "(values 1 2)" "(values)" "(values #\\c 3 1.2)" "(values \"ho\")"
		    "`(x)" "`(+ x 1)" "`(x 1)" "`((x))" "`((+ x 1))" "`(((+ x 1)))" "`((set! x (+ x 1)) (* x 2))" "`((x 1))" "`(((x 1))) "
		    "`(x . 1)" "`((x . 1))" "`(1)" "`((1))" "`((1) . x)" "`(x 1)" "'(- 1)" "(+ i 1)"
		    ;;"'((X . 1) . 2)" "'((x 1) . 2)" "'((x 1) (y . 2))" "'((x 1) y . 2)" "'((x 1) (y) . 2)" "'((x 1 . 2) . 3)" "'((x 1) 2)" "'(((x 1) 2) 3)" 
		    "'(())" "'((()))" "(random-state 1234)" 
		    "(c-pointer 0 'integer?)" "(c-pointer -1)" "(c-pointer 1234)"
		    "'(1 2 . 3)" " . "
		    "((i 0 (+ i 1)))" "(null? i)" "(= i 2)" "(zero? i)" "((null? i) i)" "(#t ())" 
		    "(x => y)" "((0 1) ())" "(- i 1)" "(if x y)" "(A (f x) B)" 
		    "(begin (f x) B)" 
		    "(f x) i" "x y z" "1 2" "`(+ ,a ,@b)" "`(+ ,a ,b)" "`(+ ,a ,b ,@c)" "`(+ ,a b ,@c ',d)"
		    "_definee_" ;; "(_definee_ wxyz)"
		    "(inlet 'integer? (lambda (f) #f))" "(inlet 'a 1)" "(openlet (inlet 'abs (lambda (x) (- x))))" 
		    "(hash-table* 'a 1)" "(hash-table)" 
		    "(make-iterator (list 1 2 3))" "(make-iterator (vector 1 2 3))" "(make-iterator (string #\\1))" "(make-iterator x)" 
		    "#<eof>" "#<undefined>" "#<unspecified>" 
		    "#o123" "#b101" "#\\newline" "#_cons" "#x123.123"
		    
		    "(call-with-exit (lambda (goto) goto))"
		    "(with-baffle (call/cc (lambda (cc) cc)))"
		    ;;"(symbol->string (gensym))"
		    "(setter _definee_)" "(setter x)" "(setter car)"
		    "(call-with-exit (lambda (return) (let ((x 1) (y 2)) (return x y))))"
		    "(call/cc (lambda (return) (let ((x 1) (y 2)) (return x y))))"
		    "(let ((x 1)) (dynamic-wind (lambda () (set! x 2)) (lambda () (+ x 1)) (lambda () (set! x 1))))"
		    ;;"(apply inlet (gensym) 1/0 ())"

		    "1+1e10i" "1e15+1e15i" 
		    "0+1e18i" "1e18" 
		    ;;"(else ())" 
		    ;;"(else)"
		    ;;"(else (f x) B)"
		    "else" "x" "(+ x 1)" "(+ 1/2 x)" "(abs x)" "(+ x 1 2+i)" "(* 2 x 3.0 4)" "((x 1234))" "((x 1234) (y 1/2))" "'x" "(x 1)"
		    "if" "begin" "cond" "case" "when" "unless"
		    "letrec" "letrec*" "or" "and" 
		    "let-temporarily"
		    ;;"+signature+" "+documentation+" "+setter+"
		    "~S~%" "~A~D~X" "~{~A~^~}~%" "~NC~&"
		    
		    "quote" "'"
		    "lambda*" "lambda"
		    ; "let" "let*" "do" "set!" "with-let" "define" "define*" "define-macro" "define-macro*" "define-bacro" "define-bacro*"
		    ))

      (codes (vector (list "(do ((i 0 (+ i 1))) ((= i 1) x) (set! x " "(let ((i 0)) (set! x ")
		     (list "(let () (let () " "((lambda () ")
		     (list "((lambda x " "((lambda* ((x ())) ")
		     (list "((lambda* ((x 1)) " "(let* ((_aaa_ 1) (x _aaa_)) (begin ")
		     (list "(cond (else " "(case x (else ")
		     (list "(case false ((#f) " "(case false ((1) #t) (else ")
		     (list "(call-with-exit (lambda (_x_) " "(call/cc (lambda (_x_) ")
		     (list "(if (not x) (begin " "(if x #<unspecified> (begin ")
		     (list "(cond ((not false) " "(unless false (begin ")
		     (list "(list (let-temporarily ((x 1)) " "(list (let ((x 1)) ")
		     (list "(begin (_dw_ " "((lambda () ")
		     (list "(begin (vector " "(apply vector (list ")
		     (list "(begin (with-let (inlet 'i 0) " "(with-let (inlet) (let ((i 0)) ")
		     (list "(list (_cw_ " "(list (values ")
		     (list "(set! __var1__ (list " "(list (let () ")
		     (list "(do () ((not false) " "(begin (when (not false) ")
		     (list "((define-macro () " "((define-bacro () ")
		     (list "(begin (let ((max 1) (min 3)) " "((lambda* ((max 1) (min 3)) ")
		     (list "(list (letrec ((x 1)) " "(list (letrec* ((x 1)) ")
		     (reader-cond ((not (provided? 'pure-s7)) (list "(with-input-from-string \"1234\" (lambda () " "(begin (_dw_string_ ")))
		     ))
      
      (chars (vector #\( #\( #\space #\space))) ; #\/ #\# #\, #\` #\@ #\. #\:))  ; #\\ #\> #\space))
  (let ((clen (length chars))
	(flen (length functions))
	(alen (length args))
	(codes-len (length codes))
	)
    
    (for-each (lambda (x) (if (not (symbol? x)) (format *stderr* "~A " x))) functions)
    (for-each (lambda (x) (if (not (string? x)) (format *stderr* "~A " x))) args)
    ;; (let ((st (symbol-table))) (for-each (lambda (x) (if (not (memq x functions)) (format *stderr* "~A " x))) st))
    
    (define (fix-op op)
      (case op
	((set!) "set! __var__")
	;((set-cdr!) "set-cdr! __var2__")
	((let) "let ()")
	((let*) "let* ()")
	((do) "do ((i 0 (+ i 1))) ((= i 1) 1)")
	((call-with-output-file) "call-with-output-file \"/dev/null\" ")
	((with-output-to-file) "with-output-to-file \"/dev/null\" ")
	((newline) "newline #f")
	((define define* define-macro define-macro* define-bacro define-bacro*)
	 (format #f "~A _definee_ 0" op))
	;(format #f "~A _definee_ (_its_arg_)" op))
	((format) "format #t ")
	((eval) "checked-eval")
	((cutlet) "checked-cutlet")
	(else (symbol->string op))))


    (define (make-expr size)
      (let ((parens 1)
	    (dqs 0)
	    (j 1)
	    (str (make-string 2048)))
	(fill! str #\space)
	(set! (str 0) #\()
	(let ((op (functions (random flen))))
	  (let ((opstr (fix-op op)))
	    (let ((oplen (length opstr)))
	      (do ((n 0 (+ n 1))
		   (k j (+ k 1)))
		  ((= n oplen) 
		   (set! j k))
		(string-set! str k (string-ref opstr n))))))
		
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
	      
	      ;; else not in a string
	      (if (char=? (str j) #\()
		  (begin
		    (set! parens (+ parens 1))
		    
		    (let ((op (functions (random flen))))
		      (let ((opstr (fix-op op)))
			(let ((oplen (length opstr)))
			  (do ((n 0 (+ n 1))
			       (k (+ j 1) (+ k 1)))
			      ((= n oplen) 
			       (set! j k))
			    (string-set! str k (string-ref opstr n))))))
		    
		    (set! j (+ j 1))
		    (set! (str j) #\space))
		  
		  (if (char=? (str j) #\))
		      (begin
			(set! parens (- parens 1))
			(if (negative? parens)
			    (begin
			      (set! (str j) #\space)
			      (set! parens 0))))
		      
		      (if (char=? (str j) #\space)
			  (let ((nargs (+ 0 (random 5))))
			    (do ((n 0 (+ n 1)))
				((= n nargs))
			      (let ((argstr (args (random alen))))
				(let ((arglen (length argstr)))
				  (do ((n 0 (+ n 1))
				       (k (+ j 1) (+ k 1)))
				      ((= n arglen)
				       (set! j k))
				    (string-set! str k (string-ref argstr n)))))
			      
			      (set! j (+ j 1))
			      (set! (str j) #\space)))
			  
			  (if (char=? (str j) #\")
			      (set! dqs 1))))))
	  
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
	(substring str 0 j)))

    (define (eval-it str) ;(format #t "~A~%" str)
      (catch #t 
	(lambda ()
	  ((lambda* args (car args)) (eval-string str)))
	(lambda (type info)
	  (set! error-type type)
	  (set! error-info info)
          'error)))

    (define (try-both str)
      ;(format *stderr* "~S~%" str)
      (set! ostr str)

      (catch #t 
	(lambda () 
	  (s7-optimize (list (catch #t 
			       (lambda ()
				 (with-input-from-string str read))
			       (lambda args ())))))
	(lambda arg 'error))

      (let* ((outer (codes (random codes-len)))
	     (str1 (string-append "(let ((x #f) (i 0)) " (car outer) str ")))"))
	     (str2 (string-append "(let () (define (func) " str1 ") (define (hi) (func)) (hi))"))
	     (str3 (string-append "(let ((x #f) (i 0)) " (cadr outer) str ")))"))
	     (str4 (string-append "(let () (define (func) " str3 ") (define (hi) (func)) (hi))")))
	(let ((val1 (eval-it str1))
	      (val2 (eval-it str2))
	      (val3 (eval-it str3))
	      (val4 (eval-it str4)))
	  (unless (and (eq? (type-of val1) (type-of val2))
		       (eq? (type-of val1) (type-of val3))
		       (eq? (type-of val1) (type-of val4)))
	    (format *stderr* "~%~S~%~S~%~S~%~S~%    ~S~%    ~S~%    ~S~%    ~S~%" str1 str2 str3 str4 val1 val2 val3 val4)
	    (if (or (eq? val1 'error)
		    (eq? val2 'error)
		    (eq? val3 'error)
		    (eq? val4 'error))
		(format *stderr* "    ~S ~S~%" error-type error-info))
	    (format *stderr* "~%")))))

    (define (test-it)
      (do ((m 0 (+ m 1)))
	  ((= m 100000000) 
	   (format *stderr* "reached end of loop??~%"))
	
	(when (zero? (modulo m 100000))
	  (set! m 0)
	  (format *stderr* "."))
	
	(catch #t
	  (lambda ()
	    (try-both (make-expr (+ 1 (random 6))))) ; min 1 here not 0
	  (lambda (type info)
	    (format *stderr* "~A: ~A ~A ~A~%" type (apply format #f info) ostr (owlet))))))

    (test-it)))
