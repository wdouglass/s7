;;; case extension including pattern matching
;;;
;;;   any key not a list, vector, or pattern descriptor (see below) is matched with equivalent?
;;;   pattern descriptors are of the form #< whatever >
;;;     #<>           any expr matches
;;;     #<func>       expr matches if (func expr)
;;;     #<label:func> expr matches as above, expr is saved under "label"
;;;     #<label:>     any expr matches, and is saved under "label"
;;;     #<label>      expr must match value saved under "label"
;;;     #<...>        skip exprs covered by the ellipsis (currently just one per key)
;;;   sequences are currently handled:
;;;     lists and vectors are matched item by item
;;;     others are matched directly via equivalent?
;;;   if a label occurs in the result expressions, the expression it labelled is substituted for it
;;;
;;;   (case* x ((3.14) 'pi)) returns 'pi if x is 3.14
;;;   (case* x ((#<symbol?>))) returns #t if x is a symbol
;;;   (case* x (((+ 1 #<symbol>)))) matches if x is any list of the form '(+ 1 x) or any other symbol is place of "x"

(provide 'case.scm)

(define case* 
  (let ((case*-helper
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

	   (define (splice-out-ellipsis sel pat pos)
	     (let ((sel-len (length sel))
		   (new-pat-len (- (length pat) 1))
		   (ellipsis-label (and (not (eq? (pat pos) #<...>))
					(let* ((str (object->string (pat pos)))
					       (colon (char-position #\: str)))
					  (and colon
					       (substring str 2 colon))))))
	       (if (pair? pat)
		   (cond ((= pos 0)               ; ellipsis at start of pattern
			  (if ellipsis-label
			      (set! (labels ellipsis-label) (copy sel (make-list (- sel-len new-pat-len)))))
			  (values (list-tail sel (- sel-len new-pat-len))
				  (cdr pat)))

			 ((= pos new-pat-len)     ; ellipsis at end of pattern
			  (if ellipsis-label
			      (set! (labels ellipsis-label) (copy sel (make-list (- sel-len pos)) pos)))
			  (values (copy sel (make-list pos))
				  (copy pat (make-list pos))))

			 (else                    ; ellipsis somewhere in the middle
			  (let ((new-pat (make-list new-pat-len))
				(new-sel (make-list new-pat-len)))
			    (if ellipsis-label
				(set! (labels ellipsis-label) (copy sel (make-list (- sel-len new-pat-len)) pos)))
			    (copy pat new-pat 0 pos)
			    (copy pat (list-tail new-pat pos) (+ pos 1))
			    (copy sel new-sel 0 pos)
			    (copy sel (list-tail new-sel pos) (- sel-len pos))
			    (values new-sel new-pat))))
		   
		   ;; subvector args are confusing -- (subvector vect dims/len offset)
		   (cond ((= pos 0)
			  (if ellipsis-label
			      (set! (labels ellipsis-label) (copy sel (make-list (- sel-len new-pat-len)))))
			  (values (subvector sel new-pat-len (max 0 (- sel-len new-pat-len)))
				  (subvector pat new-pat-len 1)))

			 ((= pos new-pat-len)
			  (if ellipsis-label
			      (set! (labels ellipsis-label) (copy sel (make-list (- sel-len new-pat-len)) pos)))
			  (values (subvector sel new-pat-len)
				  (subvector pat new-pat-len)))

			 (else
			  (let ((new-pat (make-vector new-pat-len))
				(new-sel (make-vector new-pat-len)))
			    (if ellipsis-label
				(set! (labels ellipsis-label) (copy sel (make-list (- sel-len new-pat-len)) pos)))
			    (copy pat new-pat 0 pos)
			    (copy pat (subvector new-pat (- new-pat-len pos) pos) (+ pos 1))
			    (copy sel new-sel 0 pos)
			    (copy sel (subvector new-sel (- new-pat-len pos) pos) (- sel-len pos))
			    (values new-sel new-pat)))))))
	   
	   (define (undefined->function undef e)   ; handle the pattern descriptor ("undef") of the form #< whatever >, "e" = caller's curlet
	     (let* ((str1 (object->string undef))
		    (str1-end (- (length str1) 1)))
	       (if (not (char=? (str1 str1-end) #\>))
		   (error 'wrong-type-arg "pattern descriptor does not end in '>': ~S" str1))
	       (let ((str (substring str1 2 str1-end)))
		 (if (= (length str) 0)                                        ; #<> = accept anything
		     (lambda (x) #t)
		     (let ((colon (char-position #\: str)))
		       (if colon                                               ; #<label:...> might be #<label:> or #<labelLfunc>
			   (let* ((label (substring str 0 colon))              ; str is label:...
				  (func (substring str (+ colon 1)))           ; func might be ""
				  (label-item (labels label)))                 ; see if we already have saved something under this label
			     (if label-item
				 (lambda (sel)                                 ;   if so, return function that will return an error
				   (error 'syntax-error "label ~S is defined twice: old: ~S, new: ~S~%" label label-item sel))
				 ;; otherwise the returned function needs to store the current sel-item under label in labels
				 (if (zero? (length func))
				     (lambda (x)
				       (set! (labels label) x)                 ; set label, accept anything
				       #t)
				     (let ((func-val (symbol->value (string->symbol func) e)))
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

	   (define (handle-sequence pat e)
	     (lambda (sel)
	       (and (eq? (type-of sel) (type-of pat))
		    (begin
		      (if (or (pair? pat)                            ; look for ellipsis
			      (vector? pat))
			  (let ((pos (if (pair? pat)
					 (ellipsis-pair-position 0 pat)
					 (ellipsis-vector-position pat (length pat)))))
			    (when (and pos
				       (>= (length sel) (- (length pat) 1))) ; else pat without ellipsis is too long for sel
			      ((lambda (new-sel new-pat)
				 (set! sel new-sel)
				 (set! pat new-pat))
			       (splice-out-ellipsis sel pat pos)))))

		      (and (= (length sel) (length pat))             ; march through selector and current key matching elements
			   (call-with-exit
			    (lambda (return)
			      (for-each 
			       (lambda (sel-item pat-item)
				 (or (equivalent? sel-item pat-item) ; items match

				     (and (or (pair? pat-item)       ; recursive check (* (+ #<symbol?> 1) 2), pat-item: (+ #symbol?> 1)
					      (vector? pat-item))    ; pat-item, not sel-item here so pat-item can cover anything (a list for example)
					  ((handle-sequence pat-item e) sel-item))

				     (and (undefined? pat-item)      ; turn #<func> into func and call it on the current selector element
					  (not (eq? pat-item #<undefined>))
					  (let ((func (undefined->function pat-item e)))
					    (if (undefined? func)
						(error 'unbound-variable "function ~S is undefined\n" pat-item))
					    (if (not (procedure? func))
						(error 'wrong-type-arg "~S is not a function" func))
					    (func sel-item)))

				     (return #f)))                   ; else give up (selector does not match key)
			       sel pat)
			      
			      ;; ugly repetition for dotted list
			      (unless (or (not (pair? sel)) 
					  (proper-list? sel))
				(let ((sel-item (list-tail sel (abs (length sel))))
				      (pat-item (list-tail pat (abs (length pat)))))
				  (return (or (equivalent? sel-item pat-item)
					      (and (undefined? pat-item)
						   (not (eq? pat-item #<undefined>))
						   (let ((func (undefined->function pat-item e)))
						     (if (undefined? func)
							 (error 'unbound-variable "function ~S is undefined\n" pat-item))
						     (if (not (procedure? func))
							 (error 'wrong-type-arg "~S is not a function" func))
						     (func sel-item)))))))
				      
			      #t)))))))

	   ;; case*-helper
	   (lambda (select clauses e)
	     (call-with-exit
	      (lambda (return)
		(for-each
		 (lambda (clause)
		   (let ((keys (car clause))
			 (body (cdr clause)))
		     
		     (define (handle-body select)
		       (if (null? body)
			   (return select)
			   ;; walk body looking for a labelled pattern
			   (let ((labelled 
				  (let pair-walker ((tree body))
				    (or (undefined? tree)
					(and (pair? tree)
					     (or (pair-walker (car tree))
						 (pair-walker (cdr tree))))
					(and (vector? tree)
					     (let vector-walker ((pos 0))
					       (and (< pos (length tree))
						    (or (undefined? (tree pos))
							(and (pair? (tree pos))
							     (pair-walker (tree pos)))
							(and (vector? (tree pos))
							     (vector-walker (tree pos)))
							(vector-walker (+ pos 1))))))))))

			     ;; if labelled, remake the body substituting the labelled-exprs for the labels
			     (when labelled
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

			     (return (eval (if (eq? (car body) '=>)
					       (list (cadr body) select)
					       (cons 'begin body))
					   e)))))
		     
		     (fill! labels #f)                                   ; clear previous labels
		     (if (memq keys '(else #t))                          ; (else...) or (#t...)
			 (return (eval (cons 'begin body) e))
			 (for-each
			  (lambda (key)
			    (cond ((and (undefined? key)                    ; #<...>
					(not (eq? key #<undefined>)))
				   (if (equal? key #<...>)
				       (error 'wrong-type-arg "~S makes no sense outside of a sequence" key)
				       (let ((func (undefined->function key e)))
					 (if (undefined? func)
					     (error 'unbound-variable "function ~S is not defined"
						    (let ((str (object->string key)))
						      (substring str 2 (- (length str) 1)))))
					 (if (not (procedure? func))
					     (error 'wrong-type-arg "~S is not a function" func))
					 (if (func select)
					     (handle-body select)))))

				((or (and (sequence? key)
					  ((handle-sequence key e) select))
				     (equivalent? key select))
				 (handle-body select))))
			  keys))))
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
  (let ((local-func (lambda (key) (eqv? key 1))))
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
    ((() (#<>) 
      #() #(#<>)) 
     #t)
    (((#<x:> #<middle:...> #<x>) 
      #(#<x:> #<middle:...> #<x>))
     (palindrome? (quote #<middle>)))
    (else #f)))

(test (palindrome? '(a b a)) #t)
(test (palindrome? '(a b c a)) #f)
(test (palindrome? '(a b c b a)) #t)
(test (palindrome? '(a)) #t)
(test (palindrome? ()) #t)

(test (palindrome? #(a b a)) #t)
(test (palindrome? #(a b c a)) #f)
(test (palindrome? #(a b c b a)) #t)
(test (palindrome? #(a)) #t)
(test (palindrome? #()) #t)

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
     (append (case-reverse (quote #<rest>)) 
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
|#


#|
;;; pattern in pattern func: 
(define (scase19 x)
  (let ((multiplier? (lambda (x)
		       (and (pair? x)
			    (let ((handle-sequence ((funclet ((funclet 'case*) 'case*-helper)) 'handle-sequence)))
			      (or ((handle-sequence '(* 1 #<integer?>) (curlet)) x)
				  ((handle-sequence '(* 2 #<integer?>) (curlet)) x)))))))
    (case* x
      (((+ #<integer?> #<multiplier?> #<integer?>)) #t) ; (#<multiplier...?)> perhaps?
      (else #f))))

(display (scase19 '(+ 1 (* 1 2) 3))) (newline)
(display (scase19 '(+ 1 (* 3 2) 3))) (newline)

;;; which works, but curlet looks wrong, and we need to export handle-sequence = match?
;;;   or maybe export the for-each lambda in handle-sequence
|#
