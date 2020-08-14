;;; case extension including pattern matching
;;;
;;;   The nomenclature of r7rs is (case key ((datum...) expression...)),
;;;     but I prefer "selector" to "key", "target" to "datum", and "body" to "expression".
;;;
;;;   any target not a list, vector, or pattern descriptor (see below) is matched with equivalent?
;;;   pattern descriptors are of the form #< whatever >
;;;
;;;     #<>           any expr matches
;;;     #<func>       expr matches if (func expr)
;;;     #<label:func> expr matches as above, expr is saved under "label"
;;;     #<label:>     any expr matches, and is saved under "label"
;;;     #<label>      expr must match value saved under "label"
;;;     #<...>        skip exprs covered by the ellipsis
;;;     #<label:...>  skip as above, saved skipped exprs under "label" as a quoted list
;;;                   a pattern can have any number of labelled ellipses overall, but just one unnamed ellipsis, and only one ellipsis per pair or vector
;;;     #<label,func:...> labelled ellipsis which matches if (func expr) -- expr is the ellipsis list
;;;                   label is not optional in this case
;;;     #<"regexp">   pattern is a regular expression to be matched against a string
;;;     #<label:"regexp"> labelled string that matches regexp
;;;
;;;   lists and vectors are matched item by item, other sequences are matched directly via equivalent?
;;;   if a label occurs in the result body, the expression it labelled is substituted for it
;;;   the labels and case*'s matching function can be used anywhere -- see below, "match?" etc
;;;
;;;   (case* x ((3.14) 'pi)) returns 'pi if x is 3.14
;;;   (case* x ((#<symbol?>))) returns #t if x is a symbol
;;;   (case* x (((+ 1 #<symbol?>)))) matches if x is any list of the form '(+ 1 x) or any other symbol in place of "x"
;;;   (case* x (((#<symbol?> #<e1:...> (+ #<e2:...>))) (append #<e1> #<e2>))), passed '(a b c d (+ 1 2)), returns '(b c d 1 2)

(provide 'case.scm)
(unless (provided? 'windows) 
  (require libc.scm))

(define case* 
  (let ((case*-labels (lambda (label)
			(let ((labels ((funclet ((funclet 'case*) 'case*-helper)) 'labels)))
			  (labels (symbol->string label))))) ; if ellipsis, this has been quoted by case*
	
	(case*-match? (lambda* (matchee pattern (e (curlet)))
			(let ((matcher ((funclet ((funclet 'case*) 'case*-helper)) 'handle-sequence)))
			  (or (equivalent? matchee pattern)
			      (and (or (pair? matchee) 
				       (vector? matchee))
				   (begin
				     (fill! ((funclet ((funclet 'case*) 'case*-helper)) 'labels) #f) ; clear labels
				     ((matcher pattern e) matchee)))))))
	(case*-helper
	 (with-let (unlet)
	   (define labels (make-hash-table))

	   (define (ellipsis? pat)
	     (and (undefined? pat)
		  (or (equal? pat #<...>)
		      (let ((str (object->string pat)))
			(and (char-position #\: str)
			     (string=? "...>" (substring str (- (length str) 4))))))))
	     
	   (define (ellipsis-pair-position pos pat)
	     (and (pair? pat)
		  (if (ellipsis? (car pat))
		      pos
		      (ellipsis-pair-position (+ pos 1) (cdr pat)))))

	   (define (ellipsis-vector-position pat vlen)
	     (let loop ((pos 0))
	       (and (< pos vlen)
		    (if (ellipsis? (pat pos))
			pos
			(loop (+ pos 1))))))

	   (define (splice-out-ellipsis sel pat pos e)
	     (let ((sel-len (length sel))
		   (new-pat-len (- (length pat) 1))
		   (ellipsis-label (and (not (eq? (pat pos) #<...>))              
					(let* ((str (object->string (pat pos)))
					       (colon (char-position #\: str)))
					  (and colon
					       (substring str 2 colon))))))
	       (let ((func (and (string? ellipsis-label)
				(let ((comma (char-position #\, ellipsis-label)))
				  (and comma
				       (let ((str (substring ellipsis-label (+ comma 1))))
					 (set! ellipsis-label (substring ellipsis-label 0 comma))
					 (let ((func-val (symbol->value (string->symbol str) e)))
					   (if (undefined? func-val)
					       (error 'unbound-variable "function ~S is undefined\n" func))
					   (if (not (procedure? func-val))
					       (error 'wrong-type-arg "~S is not a function\n" func))
					   func-val)))))))
		 (if (pair? pat)
		     (cond ((= pos 0)               ; ellipsis at start of pattern
			    (if ellipsis-label
				(set! (labels ellipsis-label) 
				      (list 'quote (copy sel (make-list (- sel-len new-pat-len))))))
			    (values (list-tail sel (- sel-len new-pat-len))
				    (cdr pat)
				    (or (not func)
					(func (cadr (labels ellipsis-label)))))) ; value is (quote ...) and we want the original list here
			   
			   ((= pos new-pat-len)     ; ellipsis at end of pattern
			    (if ellipsis-label
				(set! (labels ellipsis-label) 
				      (list 'quote (copy sel (make-list (- sel-len pos)) pos))))
			    (values (copy sel (make-list pos))
				    (copy pat (make-list pos))
				    (or (not func) 
					(func (cadr (labels ellipsis-label))))))
			   
			   (else                    ; ellipsis somewhere in the middle
			    (let ((new-pat (make-list new-pat-len))
				  (new-sel (make-list new-pat-len)))
			      (if ellipsis-label
				  (set! (labels ellipsis-label) 
					(list 'quote (copy sel (make-list (- sel-len new-pat-len)) pos))))
			      (copy pat new-pat 0 pos)
			      (copy pat (list-tail new-pat pos) (+ pos 1))
			      (copy sel new-sel 0 pos)
			      (copy sel (list-tail new-sel pos) (- sel-len pos))
			      (values new-sel new-pat
				      (or (not func) 
					  (func (cadr (labels ellipsis-label))))))))
		     
		     (cond ((= pos 0)
			    (if ellipsis-label
				(set! (labels ellipsis-label) 
				      (list 'quote (copy sel (make-list (- sel-len new-pat-len))))))
			    (values (subvector sel (max 0 (- sel-len new-pat-len)) sel-len) ; was new-pat-len (max 0 (- sel-len new-pat-len))
				    (subvector pat 1 (+ new-pat-len 1))                     ;     new-pat-len 1
				    (or (not func) 
					(func (cadr (labels ellipsis-label))))))
			   
			   ((= pos new-pat-len)
			    (if ellipsis-label
				(set! (labels ellipsis-label) 
				      (list 'quote (copy sel (make-list (- sel-len new-pat-len)) pos))))
			    (values (subvector sel 0 new-pat-len)
				    (subvector pat 0 new-pat-len)
				    (or (not func) 
					(func (cadr (labels ellipsis-label))))))
			   
			   (else
			    (let ((new-pat (make-vector new-pat-len))
				  (new-sel (make-vector new-pat-len)))
			      (if ellipsis-label
				  (set! (labels ellipsis-label) 
					(list 'quote (copy sel (make-list (- sel-len new-pat-len)) pos))))
			      (copy pat new-pat 0 pos)
			      (copy pat (subvector new-pat pos new-pat-len) (+ pos 1))       ; (- new-pat-len pos) pos)   copy: (+ pos 1))
			      (copy sel new-sel 0 pos)
			      (copy sel (subvector new-sel pos new-pat-len) (- sel-len pos))
			                                                                     ; (- new-pat-len pos) pos)  copy: (- sel-len pos))
			      (values new-sel new-pat
				      (or (not func) 
					  (cadr (func (labels ellipsis-label))))))))))))
	     
	   (define (handle-regex reg e)
	     (lambda (x)
	       (and (string? x)
		    (with-let (sublet (symbol->value '*libc* e)
				:x x
				:regexp (substring reg 1 (- (length reg) 1)))
		      (let* ((rg (regex.make))
			     (res (regcomp rg regexp 0)))
			(unless (zero? res)
			  (error 'regex-error "~S~%" (regerror res rg)))
			(set! res (regexec rg x 0 0))
			(regfree rg)
			(regex.free rg)
			(zero? res))))))

	   (define (undefined->function undef e)   ; handle the pattern descriptor ("undef") of the form #< whatever >, "e" = caller's curlet
	     (let* ((str1 (object->string undef))
		    (str1-end (- (length str1) 1)))
	       (if (not (char=? (str1 str1-end) #\>))
		   (error 'wrong-type-arg "pattern descriptor does not end in '>': ~S\n" str1))
	       (let ((str (substring str1 2 str1-end)))
		 (if (= (length str) 0)                                           ; #<> = accept anything
		     (lambda (x) #t)
		     (let ((colon (char-position #\: str)))
		       (cond (colon                                               ; #<label:...> might be #<label:> or #<label:func>
			      (let ((label (substring str 0 colon))               ; str is label:...
				    (func (substring str (+ colon 1))))           ; func might be ""
				(cond ((labels label)                             ; see if we already have saved something under this label
				       (lambda (sel)                              ;   if so, return function that will return an error
					 (error 'syntax-error "label ~S is defined twice: old: ~S, new: ~S~%" label (labels label) sel)))
				      
				      ;; otherwise the returned function needs to store the current sel-item under label in labels
				      ((zero? (length func))
				       (lambda (x)
					 (set! (labels label) x)                  ; #<label:>, set label, accept anything
					 #t))
				      
				      ((char=? (func 0) #\")                      ; labelled regex, #<label:"regexp">
				       (lambda (x)
					 (set! (labels label) x)
					 (handle-regex func e)))
				      
				      (else                                       ; #<label:func>
				       (let ((func-val (symbol->value (string->symbol func) e)))
					 (if (undefined? func-val)
					     (error 'unbound-variable "function ~S is undefined\n" func)
					     (if (not (procedure? func-val))
						 (error 'wrong-type-arg "~S is not a function\n" func)
						 (lambda (x)                     ; set label and call func
						   (set! (labels label) x)
						   (func-val x)))))))))
			     
			     ;; if no colon either #<label> or #<func> or #<"regexp"> -- label means match its saved expr, func = call func
			     ((char=? (str 0) #\")
			      (handle-regex str e))
			     
			     (else                                                ; #<label> or #<func>
			      (let ((saved (labels str)))
				(if saved                                         ; #<label>
				    (lambda (x) (equivalent? x saved))
				    (symbol->value (string->symbol str) e)))))))))) ; #<func> using curlet=e passed in above
	   
	   (define (handle-pattern sel-item pat-item e)
	     (and (undefined? pat-item)      ; turn #<func> into func and call it on the current selector element
		  (not (eq? pat-item #<undefined>))
		  (let ((func (undefined->function pat-item e)))
		    (if (undefined? func)
			(error 'unbound-variable "function ~S is undefined\n" pat-item))
		    (if (not (procedure? func))
			(error 'wrong-type-arg "~S is not a function\n" func))
		    (func sel-item))))

	   (define (handle-sequence pat e)
	     (lambda (sel)
	       ;(format *stderr* "~S ~S~%" sel pat)
	       (and (eq? (type-of sel) (type-of pat))
		    (let ((func-ok #t))

		      (when (or (pair? pat)                           ; look for ellipsis
				(vector? pat))
			(if (pair? (cyclic-sequences pat))
			    (error 'wrong-type-arg "case* pattern is cyclic: ~S~%" pat))
			(let ((pos (if (pair? pat)
				       (ellipsis-pair-position 0 pat)
				       (ellipsis-vector-position pat (length pat)))))
			  (when (and pos
				     (>= (length sel) (- (length pat) 1))) ; else pat without ellipsis is too long for sel
			    (let ((new-vars (list (splice-out-ellipsis sel pat pos e))))
			      (set! sel (car new-vars))
			      (set! pat (cadr new-vars))
			      (set! func-ok (caddr new-vars))))))

		      (and (= (length sel) (length pat))             ; march through selector and current target matching elements
			   func-ok
			   (call-with-exit
			    (lambda (return)
			      (for-each 
			       (lambda (sel-item pat-item)
				 (or (equivalent? sel-item pat-item) ; items match

				     (and (or (pair? pat-item)       ; recursive check (* (+ #<symbol?> 1) 2), pat-item: (+ #symbol?> 1)
					      (vector? pat-item))    ; pat-item, not sel-item here so pat-item can cover anything (a list for example)
					  ((handle-sequence pat-item e) sel-item))

				     (handle-pattern sel-item pat-item e)

				     (return #f)))                   ; else give up (selector does not match target)
			       sel pat)
			      
			      ;; dotted list, check final cdr
			      (unless (or (not (pair? sel)) 
					  (proper-list? sel))
				(let ((sel-item (list-tail sel (abs (length sel))))
				      (pat-item (list-tail pat (abs (length pat)))))
				  (return (or (equivalent? sel-item pat-item)
					      (handle-pattern sel-item pat-item e)))))
				      
			      #t)))))))

	   (define (find-labelled-pattern tree)
	     ;; walk body looking for a labelled pattern
	     (or (undefined? tree)
		 (and (pair? tree)
		      (or (find-labelled-pattern (car tree))
			  (find-labelled-pattern (cdr tree))))
		 (and (vector? tree)
		      (let vector-walker ((pos 0))
			(and (< pos (length tree))
			     (or (undefined? (tree pos))
				 (and (pair? (tree pos))
				      (find-labelled-pattern (tree pos)))
				 (and (vector? (tree pos))
				      (vector-walker (tree pos)))
				 (vector-walker (+ pos 1))))))))

	   (define (handle-body select body return e)
	     (if (null? body)
		 (return select))

	     (when (find-labelled-pattern body) ; if labelled, remake the body substituting the labelled-exprs for the labels
	       (set! body (let pair-builder ((tree body))
			    (cond ((undefined? tree)
				   (let ((label (let ((str (object->string tree)))
						  (substring str 2 (- (length str) 1)))))
				     (or (labels label) tree)))
				  
				  ((pair? tree)
				   (cons (pair-builder (car tree))
					 (pair-builder (cdr tree))))
				  
				  ((vector? tree)
				   (vector (map pair-builder tree)))
				  
				  (else tree)))))
		 
	     ;; evaluate the result (case* expands into a call on case*-helper; we need to evaluate the result expressions ourselves)
	     (return (eval (if (null? (cdr body))
			       (car body)
			       (if (eq? (car body) '=>)
				   (list (cadr body) select)
				   (cons 'begin body)))
			   e)))
		
	   ;; case*-helper
	   (lambda (select clauses e)
	     (call-with-exit
	      (lambda (return)
		(for-each
		 (lambda (clause)                                        ;((target...) body...)
		   (let ((targets (car clause))
			 (body (cdr clause)))
		     (fill! labels #f)                                   ; clear previous labels
		     (if (memq targets '(else #t))                       ; (else...) or (#t...)
			 (return (eval (cons 'begin body) e))
			 (for-each
			  (lambda (target)
			    (if (or (equivalent? target select)
				    (and (undefined? target)              ; #<...>
					 (not (eq? target #<undefined>))
					 (let ((func (undefined->function target e)))
					   ;(format *stderr* "func: ~S~%" func)
					   ;; (if (undefined? func) (error 'unbound-variable "function ~A is undefined\n" str))
					   ;; not the above check because we want to be able to pass patterns as selectors! (scase37 in s7test)
					   ;;    this seems like a mistake: #<symbol?> won't work? 
					   (and (procedure? func)
						(func select))))
				    (and (sequence? target)
					 ((handle-sequence target e) select)))
				(handle-body select body return e)))
			  targets))))
		 clauses)))))))
    ;; case*
    (#_macro (selector . clauses)
      `(((#_funclet 'case*) 'case*-helper) ,selector ',clauses (#_curlet)))))


;;; --------------------------------------------------------------------------------

#|
;;; there are more tests in s7test.scm

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
(test (scase #("asdf" #\space +nan.0 #<eof>)) 'vectstr)
(test (scase #(a 3)) 'vectsn)
(test (scase #(1)) 'vectstart)
(test (scase #("asdf" #\space)) 'vectstr)
(test (scase #("asdf")) 'oops)

(define (scase3 x)
  (let ((local-func (lambda (target) (eqv? target 1))))
    (case* x
	   ((2 3 a) 'oops)
	   ((#<local-func>) 'yup))))
(test (scase3 2) 'oops)
(test (scase3 32) #<unspecified>)
(test (scase3 1) 'yup)

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

(define (palindrome? x) ; x can be a list or a vector
  (case* x
    ((() (#<>) #() #(#<>)) 
     #t)
    (((#<x:> #<middle:...> #<x>) #(#<x:> #<middle:...> #<x>))
     (palindrome? #<middle>))
    (else #f)))

(test (palindrome? '(a b a)) #t)
(test (palindrome? '(a b c a)) #f)
(test (palindrome? '(a b c b a)) #t)
(test (palindrome? #(a b a)) #t)
(test (palindrome? #(a b c a)) #f)
(test (palindrome? #(a b c b a)) #t)

(define (scase15 x)
  (case* x
    (((+ #<x:> #<x>)) (* 2 #<x>))
    (((#<x:> #<y:>)) (list #<y> #<x>))
    (else 'oops)))
(test (scase15 '(1 2)) '(2 1))
(test (scase15 '(+ 1 1)) 2)
(test (scase15 '(+ (* 2 3) (* 2 3))) 12)

(define (scase16 x)
  (case* x
   (((+ (* #<symbol?> 2) 3)) 0)
   (else 1)))
(test (scase16 '(+ (* y 2) 3)) 0)
(test (scase16 '(+ (* y 1) 3)) 1)

(define (scase17 x)
  (let ((a1 3))
    (case* x
      (((+ #<add1:symbol?> (* #<mul1:number?> 2))) (+ #<mul1> (* #<add1> 2)))
      (else 'oops))))
(test (scase17 '(+ a1 (* 5 2))) 11)

(define (case-reverse x) ; maybe the least efficient reverse ever
  (case* x
    (((#<>) ()) x)
    (((#<first:> #<rest:...>))
     (append (case-reverse #<rest>)
	     (list (quote #<first>))))))
(test (case-reverse '(a b c)) '(c b a))
(test (case-reverse '(a b)) '(b a))

(define (scase19 x)
  (case* x
    (((#<integer?> . #<symbol?>)) 'ok)
    (else #f)))
(test (scase19 (cons 1 'a)) 'ok)
(test (scase19 (list 1 'a)) #f)

(define (scase20 x)
  (case* x
   ((#(+ (* #<symbol?> 2) 3)) 0)
   (else 1)))
(test (scase20 #(+ (* y 2) 3)) 0)
(test (scase20 #(+ (* y 1) 3)) 1)

(define (scase21 x)
  (let ((pair2? (lambda (p) 
		  (= (length p) 2))))
    (case* x
      (((+ #<pair2?> 3)) #t)
      (else #f))))
(test (scase21 '(+ (abs x) 3)) #t)
(test (scase21 '(+ (* 2 x) 3)) #f)

(define (scase22 x)
  (letrec ((symbols? 
	    (lambda (x)
	      (or (null? x)
		  (and (pair? x)
                       (symbol? (car x))
		       (symbols? (cdr x)))))))
  (case* x
    ((#<symbols?>) #t)
    (else #f))))
(display (scase22 '(+ a b c))) (newline)
(display (scase22 '(+ a b 3))) (newline)

(define (scase23 x)
  (let ((numeric-op? (lambda (x)
		       (let ((func (symbol->value x)))
			 (and (signature func)
			      (memq (car (signature func)) '(number? complex? real? float? rational? integer? byte?)))))))
    (case* x
      (((#<numeric-op?> #<number?>)
	(#<numeric-op?> #<number?> #<number?>)) #t)
      (else #f))))
(display (scase23 '(+ 1 2))) (newline)
(display (scase23 '(floor 32.1))) (newline)
(display (scase23 '(abs))) (newline)

(define (scase24 x)
  (case* x
    (((+ #<rest:...>))
     (+ (apply values #<rest>)))
    (else 'oops)))
(display (scase24 '(+ 1 2 3))) (newline)
(display (let ((a 1) (b 2) (c 3)) (scase24 `(+ ,a ,b ,c)))) (newline)

(define (scase25 x)
  (case* x
    (((#<symbol?> #<ellip1:...> (+ #<ellip2:...>))) (append #<ellip1> #<ellip2>))
    (else #f)))
(display (scase25 '(a b c d (+ 1 2)))) (newline)

(define (scase26 x)
  (case* x
    (((if (not #<test:>) (begin #<body:...>))) (cons 'unless (cons '#<test> #<body>)))
    (((if (not #<test:>) #<body:>))            (cons 'unless (list '#<test> '#<body>)))
    (((if #<test:> (begin #<body:...>)))       (cons 'when (cons '#<test> #<body>)))
    (((if #<test:> #<body:>))                  (cons 'when (list '#<test> '#<body>)))))

(display (scase26 '(if (not (> i 3)) (display i)))) (newline)                   ; '(unless (> i 3) (display i))
(display (scase26 '(if (not (> i 3)) (begin (display i) (newline))))) (newline) ; '(unless (> i 3) (display i) (newline))
(display (scase26 '(if (> i 3) (display i)))) (newline)                         ; '(when (> i 3) (display i))
(display (scase26 '(if (> i 3) (begin (display i) (newline))))) (newline)       ; '(when (> i 3) (display i) (newline))

(define (scase27 x)
  (let ((efunc? (lambda (x)
		  (and (pair? x)
		       (number? (car x))))))
    (case* x
      (((#<label,efunc?:...>)) #t)
      (else #f))))
(display (scase27 '(1 2 3))) (newline)
(display (scase27 '(a 2 3))) (newline)
(display (scase27 '(3))) (newline)
(display (scase27 ())) (newline)

(define (scase29 x)
  (let ((match? ((funclet 'case*) 'case*-match?)))
    (let ((multiplier? (lambda (x)
			 (or (match? x '(* 1 #<integer?>))
			     (match? x '(* 2 #<integer?>))))))
    (case* x
      (((+ #<integer?> #<multiplier?> #<integer?>)) #t)
      (else #f)))))

(display (scase29 '(+ 1 (* 1 2) 3))) (newline)
(display (scase29 '(+ 1 (* 3 2) 3))) (newline)

(define (scase30 x)
  (let ((match? ((funclet 'case*) 'case*-match?)))
    (match? x '(+ #<symbol?> 1))))

(display (scase30 '(+ a 1))) (newline)
(display (scase30 '(+ 1 1))) (newline)

(define* (scase31 x (e (curlet)))
  (let ((match? ((funclet 'case*) 'case*-match?))
        (labels ((funclet 'case*) 'case*-labels)))
    (and (match? x '(#<symbol?> #<ellip1:...> (+ #<ellip2:...>)))
         (append (cadr (labels 'ellip1)) (cadr (labels 'ellip2))))))

(display (scase31 '(a b c d (+ 1 2)))) (newline)

(define (scase32 x)
  (let ((match? ((funclet 'case*) 'case*-match?))
        (labels ((funclet 'case*) 'case*-labels)))
    (if (match? x '(if #<test:> (begin #<body:...>)))
	(cons 'when (cons (labels 'test) (cadr (labels 'body)))))))

(display (scase32 '(if (> i 3) (begin (display i) (newline))))) (newline)
(display (scase32 '(if 32/15 (begin (display i) (newline))))) (newline)

(define (scase33 x)
  (case* x
    ((#<"a.b">) #t)
    (else #f)))

(display (scase33 "a1b")) (newline)
(display (scase33 "abc")) (newline)
(display (scase33 "a123b")) (newline)
(display (scase33 'a1b)) (newline)

(define (scase34 x)
  (case* x
    ((#<reg:"a.b">) #<reg>)
    (else #f)))

(display (scase34 "a1b")) (newline)

(define (scase35 x)
  (let ((quotes? (lambda (x)
		   (char-position #\" x))))
    (case* x
      ((#<"^dog">) 'dog0)
      ((#<"gray\|grey">) 'graey) ; basic regex so it needs \, apparently doesn't work in OSX?
      ((#<"h\(a\|e\)y">) 'haey) 
      ((#<"p[ae]y">) 'paey)
      ((#<"b[aeiou]bble">) 'bxbble)
      ((#<"z\{3,6\}">) 'zzz)
      ((#<"\d">) 'digit)
      ((#<"<>">) 'brackets)
      ((#<quotes?>) 'quotes)
      ((#<"[^i*&2@]">) 'not-i)
      (else #f))))

(display (scase35 "dog")) (newline)
(display (scase35 "i7+")) (newline)
(display (scase35 "gray")) (newline)
(display (scase35 "hay")) (newline)
(display (scase35 "pay")) (newline)
(display (scase35 "bubble")) (newline)
(display (scase35 "ab0d")) (newline)
(display (scase35 "+-<>-+")) (newline)
(display (scase35 "zzzz")) (newline)
(display (scase35 (string #\a #\"))) (newline)

;;; for other types:
(define (hlt x)
  (case* (with-input-from-string (object->string x) read)
    (((hash-table 'a #<integer?>)) 'hash-table)
    (((inlet 'a #<integer?>)) 'inlet)
    (else #f)))

(display (hlt (inlet 'a 1))) (newline)
(display (hlt (hash-table 'a 1))) (newline)

(append (list 'float-vector) (vector->list #r(1 2 3))): (float-vector 1.0 2.0 3.0)

|#
