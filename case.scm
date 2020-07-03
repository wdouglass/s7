;;; case extension including pattern matching
;;;
;;;   any key not a list, vector, or pattern descriptor (see below) is matched with equivalent?
;;;   pattern descriptors are of the form #< >
;;;     #<>           any expr matches
;;;     #<func>       expr matches if (func expr)
;;;     #<label:func> expr matches as above, expr is saved under "label"
;;;     #<label:>     any expr matches, and is saved under "label"
;;;     #<label>      expr must match value saved under "label"
;;;     #<...>        skip exprs covered by the ellipsis (currently just one per key)
;;;   sequences are currently handled:
;;;     lists and vectors are matched item by item
;;;     others are currently matched directly via equivalent?
;;;
;;;   (case* x ((3.14) 'pi)) returns 'pi if x is 3.14
;;;   (case* x ((#<symbol?>))) returns #t if x is a symbol
;;;   (case* x (((+ 1 #<symbol>)))) matches if x is any list of the form '(+ 1 x) or any other symbol is place of "x"

(define case* 
  (let ((case*-helper
	 (with-let (unlet)
	   (define labels (make-hash-table))

	   (define (undefined->function undef e)   ; handle the pattern descriptor ("undef") of the form #<...>, "e" = caller's curlet
	     ;(format *stderr* "undef: ~S ~S~%" undef e)
	     (let* ((str1 (object->string undef))
		    (str1-len (length str1)))
	       (if (not (char=? (str1 (- str1-len 1)) #\>))
		   (error 'wrong-type-arg "pattern descriptor does not end in '>': ~S" str1))
	       (let ((str (substring str1 2 (- str1-len 1))))
		 ;(format *stderr* "str: ~S~%" str)
		 (if (= (length str) 0)                                        ; #<> = accept anything
		     (lambda (x) #t)
		     (let ((colon (char-position #\: str)))
		       (if colon                                               ; #<label:...> might be #<label:> or #<labelLfunc>
			   (let* ((label (substring str 0 colon))              ; str is label:...
				  (func (substring str (+ colon 1)))           ; func might be ""
				  (label-item (labels func)))                  ; see if we already have saved something under this label
			     (if label-item
				 (lambda (sel)                                 ;   if so, return function that will return an error
				   (error "label ~S is defined twice: old: ~S, new: ~S~%" label label-item sel))
				 ;; otherwise the returned function needs to store the current sel-item under label in labels
				 (if (zero? (length func))
				     (lambda (x)
				       (set! (labels label) x)                 ; set label, accept anything
				       #t)
				     (let ((func-val (symbol->value (string->symbol func) e)))
				       ;(format *stderr* "func-val: ~S ~S~%" func func-val)
				       (if (undefined? func-val)
					   (error 'unbound-variable "function ~S is undefined\n" func)
					   (if (not (procedure? func-val))
					       (error 'wrong-type-arg "~S is not a function" func)
					       (lambda (x)                     ; set label and call func
						 (set! (labels label) x)
						 (func-val x))))))))
			   ;; if no colon either #<label> or #<func> -- label means match its saved label-item, func = call func
			   (let ((saved (labels str)))
			     (if saved                                          ; #<label>
				 (lambda (x) (equivalent? x saved))
				 (symbol->value (string->symbol str) e))))))))) ; #<func> using curlet=e passed in above

	   (define (ellipsis-pair-position pos pat)
	     (if (null? pat)
		 #f
		 (if (equal? (car pat) #<...>)
		     pos
		     (ellipsis-pair-position (+ pos 1) (cdr pat)))))

	   (define (ellipsis-vector-position pat vlen)
	     (let loop ((pos 0))
	       (and (< pos vlen)
		    (if (equal? (pat pos) #<...>)
			pos
			(loop (+ pos 1))))))

	   (define (splice-out-ellipsis sel pat pos)
	     (let ((sel-len (length sel))
		   (pat-len (length pat)))
	       ;(format *stderr* "sel: ~S, pat: ~S, pos: ~S~%" sel pat pos)
	       (if (pair? pat)
		   (cond ((= pos 0)
			  (values (list-tail sel (- sel-len pat-len -1))
				  (cdr pat)))
			 ((= pos (- pat-len 1))
			  (values (copy sel (make-list (- pat-len 1)))
				  (copy pat (make-list (- pat-len 1)))))
			 (else
			  (let ((new-pat (make-list (- pat-len 1)))
				(new-sel (make-list (- pat-len 1))))
			    (copy pat new-pat 0 pos)
			    (copy pat (list-tail new-pat pos) (+ pos 1))
			    (copy sel new-sel 0 pos)
			    (copy sel (list-tail new-sel pos) (- sel-len pos))
			    (values new-sel new-pat))))
		   (cond ((= pos 0)
			  (values (subvector sel (- pat-len 1) (max 0 (- sel-len pat-len 1)))
				  (subvector pat (- pat-len 1) 1)))
			 ((= pos (- pat-len 1))
			  (values (subvector sel (- pat-len 1))
				  (subvector pat (- pat-len 1))))
			 (else
			  (let ((new-pat (make-vector (- pat-len 1)))
				(new-sel (make-vector (- pat-len 1))))
			    (copy pat new-pat 0 pos)
			    ;(format *stderr* "pos: ~D, pat ~S -> ~S" pos pat new-pat)
			    (copy pat (subvector new-pat (- pat-len pos 1) pos) (+ pos 1))
			    ;(format *stderr* "  -> ~S~%" new-pat)
			    (copy sel new-sel 0 pos)
			    ;(format *stderr* "sel ~S -> ~S" sel new-sel)
			    (copy sel (subvector new-sel (- pat-len pos 1) pos) (- sel-len pos))
			    ;(format *stderr* "  -> ~S~%" new-sel)
			    (values new-sel new-pat)))))))
	   
	   (define (handle-sequence pat e)
	     (lambda (sel)
	       (and (eq? (type-of sel) (type-of pat))
		    (begin
		      (if (or (pair? pat) 
			      (vector? pat))
			  (let ((pos (if (pair? pat)
					 (ellipsis-pair-position 0 pat)
					 (ellipsis-vector-position pat (length pat)))))
			    (when (and pos
				       (>= (length sel) (- (length pat) 1))) ; else pat without ellipsis is too long for sel
			      ((lambda (new-sel new-pat)
				 ;(format *stderr* "sel: ~S -> ~S, pat: ~S -> ~S~%" sel new-sel pat new-pat)
				 (set! sel new-sel)
				 (set! pat new-pat))
			       (splice-out-ellipsis sel pat pos)))))
		      (and (= (length sel) (length pat))
			   (call-with-exit
			    (lambda (return)
			      (for-each 
			       (lambda (sel-item pat-item)
				 (or (equivalent? sel-item pat-item)
				     (and (undefined? pat-item)
					  (not (eq? pat-item '#<undefined>))
					  (let ((func (undefined->function pat-item e)))
					    (if (undefined? func)
						(error 'unbound-variable "function ~S is undefined\n" pat-item))
					    (if (not (procedure? func))
						(error 'wrong-type-arg "~S is not a function" func))
					    (func sel-item)))
				     (return #f)))
			       sel pat)
			      #t)))))))

	   (lambda (select clauses e)
	     ;(format *stderr* "~S ~S e: ~S~%" select clauses e)
	     (call-with-exit
	      (lambda (return)
		(for-each
		 (lambda (clause)
		   (let ((keys (car clause))
			 (body (cdr clause)))
		     
		     (define (handle-body select)
		       (if (null? body)
			   (return select)
			   (if (eq? (car body) '=>)
			       (return (eval `(,(cadr body) ,select) e))
			       (return (eval `(begin ,@body) e)))))
		     
		     (fill! labels #f)                                   ; clear previous labels
		     (if (memq keys '(else #t))                          ; (else...) or (#t...)
			 (return (eval `(begin ,@body) e))
			 (for-each
			  (lambda (key)
			    (if (and (undefined? key)                     ; #<...>
				     (not (eq? key '#<undefined>)))
				(if (equal? key #<...>)
				    (error 'wrong-type-arg "~S makes no sense outside a sequence" key)
				    (let ((func (undefined->function key e)))
				      (if (undefined? func)
					  (error 'unbound-variable "function ~S is not defined"
						 (let ((str (object->string key)))
						   (substring str 2 (- (length str) 1)))))
				      (if (not (procedure? func))
					  (error 'wrong-type-arg "~S is not a function" func))
				      (if (func select)
					  (handle-body select))))
				(if (and (sequence? key)
					 ((handle-sequence key e) select))
				    (handle-body select)
				    (if (equivalent? key select)
					(handle-body select)))))
			  keys))))
		 clauses)
		#<unspecified>))))))
    ;; case*
    (#_macro (selector . clauses)
      `(((#_funclet 'case*) 'case*-helper) ,selector ',clauses (#_curlet)))))


;;; --------------------------------------------------------------------------------

(define-macro (test expr res)
  `(let ((value ,expr))
     (unless (equivalent? value ,res)
       (format *stderr* "~S, expected: ~S, got: ~S~%" ',expr ,res value))))

(define (scase x)
 (case* x
   ((a b) 'a-or-b)
   ((1 2/3 3.0) => (lambda (a) (* a 2)))
   ((#_pi) 1 123)
   (("string1" "string2"))
   ((#<symbol?>) 'symbol!)
   (((+ x #<symbol?>)) 'got-list)
   ((#(1 x 3)) 'got-vector)
   (((+ #<>)) 'empty)
   (((* #<x:symbol?> #<x>)) 'got-label)
   (((#<> #<x:> #<x>)) 'repeated)
   (((#<symbol?> #<symbol?>)) 'two)
   (((#<x:> #<x>)) 'pair)
   ((#(#<x:> #<x>)) 'vector)
   ((#(#<symbol?> #<...> #<number?>)) 'vectsn)
   ((#(#<...> #<number?>)) 'vectstart)
   ((#(#<string?> #<char-whitespace?> #<...>)) 'vectstr)
   (else 'oops)))

(test (scase 3.0) 6.0)
(test (scase pi) 123)
(test (scase "string1") "string1")
(test (scase "string3") 'oops)
(test (scase 'a) 'a-or-b)
(test (scase 'abc) 'symbol!)
(test (scase #()) 'oops)
(test (scase '(+ x z)) 'got-list)
(test (scase #(1 x 3)) 'got-vector)
(test (scase '(+ x 3)) 'oops)
(test (scase '(+ x)) 'empty)
(test (scase '(* z z)) 'got-label)
(test (scase '(* z x)) 'oops)
(test (scase '(+ (abs x) (abs x))) 'repeated)
(test (scase '(+ (abs x) (abs y))) 'oops)
(test (scase '(a b)) 'two)
(test (scase '(1 1)) 'pair)
(test (scase '(1 1 2)) 'oops)
(test (scase #(1 1)) 'vector)
(test (scase #(a b c 3)) 'vectsn)
(test (scase #(1 b 2)) 'vectstart)
(test (scase #("asdf" #\space #<eof> +nan.0)) 'vectstr)
(test (scase #(a 3)) 'vectsn)
(test (scase #(1)) 'vectstart)
(test (scase #("asdf" #\space)) 'vectstr)
(test (scase #("asdf")) 'oops)

(define (scase5 x)
  (case* x
    (((#<symbol?> #<...> #<symbol?>)) 'ok)))
(test (scase5 '(a)) #<unspecified>)

(let ((local-func (lambda (key) (eqv? key 1))))
  (define (scase1 x)
    (case* x
      ((2 3 a) 'oops)
      ((#<local-func>) 'yup)))
  (test (scase1 2) 'oops)
  (test (scase1 32) #<unspecified>)
  (test (scase1 1) 'yup))

(define scase2
  (let ((local-func (lambda (key) (eqv? key 1))))
    (lambda (x)
      (case* x
	     ((2 3 a) 'oops)
	     ((#<local-func>) 'yup)))))
(test (scase2 2) 'oops)
(test (scase2 32) #<unspecified>)
(test (scase2 1) 'yup)

(define (scase3 x)
  (let ((local-func (lambda (key) (eqv? key 1))))
    (case* x
	   ((2 3 a) 'oops)
	   ((#<local-func>) 'yup))))
(test (scase3 2) 'oops)
(test (scase3 32) #<unspecified>)
(test (scase3 1) 'yup)

(define (scase4 x)
  (let ((local-func (lambda (key) (eqv? key 1))))
    (case* x
	   ((2 3 a) 'oops)
	   ((#<local-func) 'yup)))) ; lost > is deliberate
(test (catch #t (lambda () (scase4 1)) (lambda (type info) type)) 'wrong-type-arg)

(define (ecase x)
  (case* x
    (((#<symbol?> #<...> #<symbol?>)) 'both-symbol)
    (((#<symbol?> #<...>)) 'car-symbol)
    (((#<...> #<symbol?> #<symbol?>)) 'two-symbols)
    (((#<...> #<symbol?>)) 'end-symbol)
    (else #f)))

(test (ecase '(a b 1)) 'car-symbol)
(test (ecase '(1 2 c)) 'end-symbol)
(test (ecase '(a 1 2 3 c)) 'both-symbol)
(test (ecase '(1 2 3 b c)) 'two-symbols)

(define (scase6 x)
  (case* x
    (((#<x:> #<x> #<...>)) 'ok)
    (else 'oops)))
(test (scase6 '(a a)) 'ok)
(define (scase7 x)
  (case* x
    (((#<...> #<x:> #<x>)) 'ok)
    (else 'oops)))
(test (scase7 '(a a)) 'ok)
(define (scase8 x)
  (case* x
    (((#<x:> #<...> #<x>)) 'ok)
    (else 'oops)))
(test (scase8 '(a a)) 'ok)
(define (scase9 x)
  (case* x
    ((#<y>) 1)
    (else 'oops)))
(test (catch #t (lambda () (scase9 1)) (lambda (type info) type)) 'unbound-variable)
(define (scase10 x)
  (case* x
    ((#<x>) 1)
    (else 'oops)))
(test (catch #t (lambda () (scase10 1)) (lambda (type info) type)) 'wrong-type-arg)
(define (scase11 x)
  (case* x
    ((#<...>) 1)
    (else 'oops)))
(test (catch #t (lambda () (scase11 1)) (lambda (type info) type)) 'wrong-type-arg)
(define (scase12 x)
  (case* x
    (((#<y:> #<zzz>)) 1)
    (else 'oops)))
(test (catch #t (lambda () (scase12 '(1 2))) (lambda (type info) type)) 'unbound-variable)
(define (scase13 x)
  (case* x
    ((#<>) 'ok) ; matches anything! as does #<label:>
    (else 'oops)))
(test (scase13 '(a a)) 'ok)
(test (scase13 1+i) 'ok)
(test (scase13 #(1 2 3)) 'ok)

(define (uniquify lst)
  (let ((not-pair? (lambda (x) 
		     (not (pair? x)))))
    (define (uniq-1 lst new-lst)
      (case* lst
	((()) 
	 (reverse new-lst))
	(((#<>))
	 (reverse (cons (car lst) new-lst)))
	(((#<x:> #<x> #<...>))
	 (uniq-1 (cdr lst) new-lst))
	(else
	 (uniq-1 (cdr lst) (cons (car lst) new-lst)))))
    (uniq-1 lst ())))

(test (uniquify '(a a b b b b a a c c)) '(a b a c))
(test (uniquify '((+ a 1) (+ a 1) (* b 2) (* b 2) c a a)) '((+ a 1) (* b 2) c a))
(test (uniquify '(a b b c)) '(a b c))
(test (uniquify '(a)) '(a))
(test (uniquify ()) ())
;(test (uniquify '(1 2 . 2)) '(1 2 . 2))         ; TODO: need dotted list check

(let ((x '(+ 2 3)))
  (test (case* x
	  (((+ #<> #<>)) (apply + (cdr x)))
	  (else (error 'out-of-range "unimplemented")))
	5))

(define (palindrome? x)
  (case* x
    ((() (#<>)) #t)
    (((#<x:> #<...> #<x>)) 
     (palindrome? (copy (cdr x) (make-list (- (length x) 2)))))
    (else #f)))

(test (palindrome? '(a b a)) #t)
(test (palindrome? '(a b c a)) #f)
(test (palindrome? '(a b c b a)) #t)
(test (palindrome? '(a)) #t)
(test (palindrome? ()) #t)

#|

(#<symbol?> . #<symbol?>)  ; i.e a simple pair -- maybe (lambda (x) (not (pair? (cdr x))))
(#<> . #<pair?>) #<proper-list?>?

|#
