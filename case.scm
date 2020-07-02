;;; case extension including pattern matching
;;;
;;;   any key not a sequence or pattern descriptor (see below) is matched with equivalent?
;;;   pattern descriptors are of the form #<...>
;;;     #<>       any expr matches
;;;     #<func>   expr matches if (func expr)
;;;     #<label:func> expr matches as above, expr is saved under "label"
;;;     #<label:> any expr matches, and is saved under "label"
;;;     #<label>  expr must match value saved under "label"
;;;     #<...>    skip exprs covered by the ellipsis (currently just one per key)
;;;   sequences are currently handled:
;;;     lists and vectors matched item by item
;;;     others are currently matched directly via equivalent?
;;;
;;;   (case* x ((3.14) 'pi)) matches if x is 3.14
;;;   (case* x ((#<symbol?>))) matches if x is a symbol
;;;   (case* x (((+ 1 #<symbol>)))) matches if x is any list of the form '(+ 1 x) or any other symbol is place of "x"

(define case* 
  (let ((case*-helper
	 (with-let (unlet)                         ; we are completely isolated from the surrounding code
	   (define labels (make-hash-table))

	   (define (undefined->function undef e)   ; handle the pattern descriptor ("undef") of the form #<...>, "e" = caller's curlet
	     (let* ((str1 (object->string undef))
		    (str (substring str1 2 (- (length str1) 1))))
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
				     (if (undefined? func-val)
					 (error 'unbound-variable "function ~S is undefined\n" func)
					 (lambda (x)                         ; set label and call func
					   (set! (labels label) x)
					   (func-val x)))))))
			 ;; if no colon either #<label> or #<func> -- label means match its saved label-item, func = call func
			 (let ((saved (labels str)))
			   (if saved                                         ; #<label>
			       (lambda (x) (equivalent? x saved))
			       (symbol->value (string->symbol str) e)))))))) ; #<func> using curlet=e passed in above

	   (define (splice-out-ellipsis sel lst)
	     ;; assume lst has #<...>, (member #<...> lst)
	     (let splicer ((new-sel ()) (new-lst ()) (sel sel) (lst lst))
	       (if (or (null? lst)
		       (null? sel))
		   (values new-sel new-lst)
		   (if (not (equal? (car lst) #<...>))
		       (splicer (cons (car sel) new-sel) (cons (car lst) new-lst) (cdr sel) (cdr lst))
		       (values
			(append (reverse new-sel) (list-tail sel (- (length sel) (length lst) -1)))
			(append (reverse new-lst) (cdr lst)))))))
	   
	   (define (handle-sequence lst e)
	     (lambda (sel)
	       (and (eq? (type-of sel) (type-of lst))
		    (or (and (pair? lst)
			     (member #<...> lst)
			     ((lambda (new-sel new-lst)
				(set! sel new-sel)
				(set! lst new-lst))
			      (splice-out-ellipsis sel lst)))
			(= (length sel) (length lst)))
		    (call-with-exit
		     (lambda (return)
		       (for-each 
			(lambda (sel-item lst-item)
			  (or (equivalent? sel-item lst-item)
			      (and (undefined? lst-item)
				   (not (eq? lst-item '#<undefined>))
				   (let ((func (undefined->function lst-item e)))
				     (if (undefined? func)
					 (error 'unbound-variable "function ~S is undefined\n" func))
				     (func sel-item)))
			      (return #f)))
			sel lst)
		       #t)))))
	   
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
				(let ((func (undefined->function key e)))
				  (if (undefined? func)
				      (error 'unbound-variable "function ~S is not defined"
					     (let ((str (object->string key)))
					       (substring str 2 (- (length str) 1)))))
				  (if (func select)
				      (handle-body select)))
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
      `(((#_funclet 'case*) 'case*-helper) 
	(#_eval ',selector) ',clauses (#_curlet)))))


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
   (else 'oops)))

(test (scase 3.0) 6.0)
(test (scase pi) 123)
(test (scase "string1") "string1")
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

(let ((local-func (lambda (key) (eqv? key 1))))
  (define (scase1 x)
    (case* x
      ((2 3 a) 'oops)
      ((#<local-func>) 'yup)))
  (test (scase1 2) 'oops)
  (test (scase1 32) #<unspecified>)
  (test (scase1 1) 'yup))

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


(define (uniquify lst)
  (let ((not-pair? (lambda (x) 
		     (not (pair? x)))))
    (define (uniq-1 lst new-lst)
      (case* lst
	(((#<>))
	 (reverse (cons (car lst) new-lst)))
	(((#<x:> #<x>))                          ; TODO: should be unneeded? (empty #<...> below?
	 (reverse (cons (car lst) new-lst)))
	(((#<x:> #<x> #<...>))
	 (uniq-1 (cdr lst) new-lst))
	(else
	 (uniq-1 (cdr lst) (cons (car lst) new-lst)))))
    (uniq-1 lst ())))

(test (uniquify '(a a b b b b a a c c)) '(a b a c))
(test (uniquify '((+ a 1) (+ a 1) (* b 2) (* b 2) c a a)) '((+ a 1) (* b 2) c a))
;(test (uniquify '(1 2 . 2)) '(1 2 . 2))         ; TODO: need dotted list check

(let ((x '(+ 2 3)))
  (test (case* x
	  (((+ #<> #<>)) (apply + (cdr x)))
	  (else (error 'out-of-range "unimplemented")))
	5))

#|

;;; TODO: need labelled ellipsis?? maybe any number of  #<...> = skip?, #<func ...>?
;;;   so (#<x:...> #<x>) means any length list of two equal pieces?
;;;   can ... portion be empty?

;;; TODO: error here
(define (palindrome? x)
  (case* x
    ((() (#<x>)) #t)
    (((#<x:> #<...> #<x>)) 
     (palindrome? (copy (cdr x) (make-list (- (length x) 2))))) ; (cdr (reverse (cdr (reverse x))))
    (else #f)))

(test (palindrome? '(a b a)) #t)


(#<symbol?> . #<symbol?>)  ; i.e a simple pair -- maybe (lambda (x) (not (pair? (cdr x))))
(#<> . #<pair?>) #<proper-list?>?
  ...

How to insist on unlet symbol? -- currently #<symbol?> looks in curlet
  maybe check unlet first or (funclet 'case*)
  also caller can always:
  (with-let (unlet) (case*...))

|#
