(provide 'mockery.scm)

(define (outlet-member obj e)
  (or (eq? obj e)
      (and (not (eq? obj (rootlet)))
	   (outlet-member (outlet obj) e))))

(define (make-method f accessor)
  (lambda args
    (if (null? args)
	(f)
	(if (let? (car args))
	    (apply f (accessor (car args)) (cdr args))
	    (if (or (null? (cdr args))
		    (not (let? (cadr args))))
		(apply f (map (lambda (arg)
				(if (openlet? arg)
				    (coverlet arg)
				    arg))
			      args))
		(apply f (car args) (accessor (cadr args)) (cddr args)))))))

(define (make-object . args)
  (openlet
   (apply inlet args)))


(define (mock->string obj . args)
  (if (let? obj)
      (format #f (if (or (null? args) (car args)) "~S" "~A") (obj 'value))
      "??"))

(define (make-local-method f)
  (make-method f (lambda (obj) (obj 'value))))


;;; one tricky thing here is that a mock object can be the let of with-let: (with-let (mock-port ...) ...)
;;;   so a mock object's method can be called even when no argument is a mock object

;;; --------------------------------------------------------------------------------

(define *mock-vector*
  (let ((mock-vector? #f)
	(no-mock-vectors #f)
	(->value #f))
    (let ((mock-vector-class
	  (inlet 'equivalent?        (make-local-method #_equivalent?) ; see comment below
		 
		 'local-set!         (lambda (obj i val)          ; reactive-vector uses this as a hook into vector-set!
				       (if (mock-vector? i)
					   (error 'wrong-type-arg "stray mock-vector? ~S" i))
				       (#_vector-set! (->value obj) i val))

		 'vector-set!        (lambda (obj i val) ((obj 'local-set!) obj i val) val)

		 'let-set-fallback   (lambda (obj i val) 
				       (if (and (integer? i)
						(defined? 'value obj))
					   (begin
					     ((obj 'local-set!) obj i val) 
					     val)
					   (error 'out-of-range "unknown field: ~S" i)))
		 
		 'vector-ref         (lambda (obj i) 
				       (if (mock-vector? i)
					   (error 'wrong-type-arg "stray mock-vector? ~S" i))
				       (#_vector-ref (->value obj) i))
		 
		 'let-ref-fallback   (lambda (obj i) 
				       (if (and (integer? i)
						(defined? 'value obj))
					   (#_vector-ref (obj 'value) i)   ; the implicit case
					   (error 'out-of-range "unknown field: ~S" i)))

		 'vector-length      (lambda (obj) 
				       (#_length (->value obj)))

		 'reverse            (lambda (obj) 
				       (#_reverse (->value obj)))

		 'sort!              (lambda (obj f)
				       (if (mock-vector? f)
					   (error 'wrong-type-arg "sort! mock-vector as sort-function: ~S?" f))
				       (#_sort! (->value obj) f))
					   
		 'make-iterator      (lambda (obj) 
				       (#_make-iterator (->value obj)))

		 'arity              (lambda (obj) 
				       (#_arity (->value obj)))

		 'object->string     (lambda args 
				       (let ((w (or (null? (cdr args)) (cadr args))))
					 (copy (if (eq? w :readable) "*mock-vector*" "#<mock-vector-class>"))))

		 'vector-dimensions  (lambda (obj) 
				       (#_vector-dimensions (->value obj)))

		 'fill!              (lambda (obj val) 
				       (#_fill! (->value obj) (->value val)))
		 
		 'vector-fill!       (lambda (obj . args)
				       (apply #_fill! (->value obj) args))
		 
		 'vector->list       (lambda (obj . args)
				       (map values (->value obj)))
		 
		 'subvector          (lambda (obj . args)
				       (apply #_subvector (->value obj) (no-mock-vectors args)))
		 
		 'copy               (lambda* (source dest . args)
				       (if dest
					   (apply #_copy 
						  (->value source)
						  (->value dest)
						  (no-mock-vectors args))
					   (if (not (mock-vector? source))
					       (error 'wrong-type-arg "copy: stray mock-vector? ~A" args)
					       (let ((nobj (dynamic-wind
							       (lambda () (coverlet source))
							       (lambda () (openlet (#_copy source)))
							       (lambda () (openlet source)))))
						 (set! (nobj 'value) (#_copy (source 'value)))
						 nobj))))

		 'vector?            (lambda (obj) #t)

		 'length             (lambda (obj) 
				       (#_length (->value obj)))

		 'vector-append      (lambda args
				       (apply #_vector-append (map ->value args)))

		 'append             (lambda args
				       (apply #_append (map ->value args)))

		 'class-name         '*mock-vector*)))
    
    (define* (make-mock-vector len (init #<unspecified>))
      (openlet 
       (sublet mock-vector-class 
	 'value (#_make-vector len init)
	 'object->string mock->string)))
    
    (define (mock-vector . args)
      (let ((v (make-mock-vector 0)))
	(set! (v 'value) (apply #_vector args))
	v))
    
    (set! mock-vector? (lambda (obj)
			 (and (openlet? obj)
			      (outlet-member obj mock-vector-class))))
    
    (set! no-mock-vectors (lambda (lst)
			    (for-each
			     (lambda (c)
			       (if (mock-vector? c)
				   (error 'wrong-type-arg "stray mock-vector? ~A" lst)))
			     lst)
			    lst))

    (set! ->value (lambda (obj)
      (if (mock-vector? obj)
	  (obj 'value)
	  obj)))

    (curlet))))


#|
;;; the equivalent? method should track circles (via cyclic-sequences?)
;;;   as it is now, this code cores up indefinitely:
(let ((v1 ((*mock-vector* 'mock-vector) 1 2 3 4))
      (v2 ((*mock-vector* 'mock-vector) 2 3 4))
      (v3 #(1 2 3 4))
      (v4 #(2 3 4)))
  (vector-set! v2 0 v1)
  (vector-set! v1 0 v2)
  (vector-set! v4 0 v3)
  (vector-set! v3 0 v4)
  (equivalent? v1 v3))
;;; but how to make this cooperate with the built-in method -- we should call cyclic-sequences just once
|#

#|
;; vector that grows to accommodate vector-set!

(define stretchable-vector-class
  (let ((local-ref (lambda (obj index)
		     (if (>= index (length (obj 'value)))
			 (obj 'initial-element)
			 (#_vector-ref (obj 'value) index))))
	(local-set! (lambda (obj index val)
		      (if (>= index (length (obj 'value)))
			  (set! (obj 'value) (copy (obj 'value) (make-vector (+ index 8) (obj 'initial-element)))))
		      (#_vector-set! (obj 'value) index val))))
    (openlet
     (sublet (*mock-vector* 'mock-vector-class)
       'value (vector)
       'object->string mock->string
       'initial-element #f
       'vector-ref local-ref
       'let-ref-fallback local-ref
       'vector-set! local-set!
       'let-set-fallback local-set!))))
|#


;;; --------------------------------------------------------------------------------

(define *mock-hash-table*
  (let ((mock-hash-table? #f)
	(no-mock-hash-tables #f)
	(->value #f))
    (let ((mock-hash-table-class
	  (inlet 'equivalent?        (lambda (x y) 
				       (#_equivalent? (x 'value) y))

		 'hash-table-ref     (lambda (obj key) 
				       (#_hash-table-ref (->value obj) (->value key)))

		 'hash-table-set!    (lambda (obj key val) 
				       (#_hash-table-set! (->value obj) (->value key) val))

		 'hash-table-entries (lambda (obj) 
				       (#_hash-table-entries (->value obj)))

		 'make-iterator      (lambda (obj) 
				       (#_make-iterator (->value obj)))
		 
		 'let-ref-fallback   (lambda (obj key)
				       (if (defined? 'value obj)
					   (#_hash-table-ref (obj 'value) key)))

		 'let-set-fallback  (lambda (obj key val)  
				      (if (defined? 'value obj)
					  (#_hash-table-set! (obj 'value) key val)))
		 
		 ;; the fallbacks are needed because hash-tables and lets use exactly the same syntax in implicit indexing:
		 ;;   (x 'y) but s7 can't tell that in this one case, we actually want the 'y to be a key not a field.
		 ;;   So, to avoid infinite recursion in let-ref (implicit index), if let-ref can't find the let field,
		 ;;   and the let has 'let-ref|set!-fallback, let-ref|set! passes the argument to that function rather than
		 ;;   return #<undefined>.
		 ;;
		 ;; (round (openlet (inlet 'round (lambda (obj) (#_round (obj 'value))) 'let-ref-fallback (lambda args 3)))) -> 3
		 
		 'fill!              (lambda (obj val) 
				       (#_fill! (->value obj) (->value val)))

		 'reverse            (lambda (obj) 
				       (#_reverse (->value obj)))

		 'object->string     (lambda args 
				       (let ((w (or (null? (cdr args)) (cadr args))))
					 (copy (if (eq? w :readable) "*mock-hash-table*" "#<mock-hash-table-class>"))))

		 'arity              (lambda (obj)          
				       (#_arity (obj 'value)))

		 'copy               (lambda* (source dest . args)
				       (if dest
					   (apply #_copy 
						  (if (mock-hash-table? source) (source 'value) source)
						  (if (mock-hash-table? dest) (dest 'value) dest)
						  (no-mock-hash-tables args))
					   (if (not (mock-hash-table? source))
					       (error 'wrong-type-arg "copy: stray mock-hash-table? ~A" args)
					       (let ((nobj (dynamic-wind
							       (lambda () (coverlet source))
							       (lambda () (openlet (#_copy source)))
							       (lambda () (openlet source)))))
						 (set! (nobj 'value) (#_copy (source 'value)))
						 nobj))))

		 'hash-table?        (lambda (obj) #t)

		 'length             (lambda (obj) 
				       (#_length (->value obj)))

		 'append             (make-local-method #_append)

		 'class-name         '*mock-hash-table*)))
    
    (define* (make-mock-hash-table (len 511))
      (openlet 
       (sublet mock-hash-table-class 
	 'value (#_make-hash-table len)
	 
	 ;; object->string here is a problem -- don't set any value to the object itself!
	 'object->string (lambda (obj . args) ; can't use mock->string because the value is not in the 'value field
			   (format #f "~S" (obj 'value))))))
    
    (define (mock-hash-table . args)
      (let ((v (make-mock-hash-table)))
	(set! (v 'value) (apply #_hash-table args))
	v))
    
    (set! mock-hash-table? (lambda (obj)
			     (and (openlet? obj)
				  (outlet-member obj mock-hash-table-class))))
    
    (set! no-mock-hash-tables (lambda (lst)
				(for-each
				 (lambda (c)
				   (if (mock-hash-table? c)
				       (error 'wrong-type-arg "stray mock-hash-table? ~A" lst)))
				 lst)
				lst))

    (set! ->value (lambda (obj)
      (if (mock-hash-table? obj)
	  (obj 'value)
	  obj)))

    (curlet))))


#|
;; hash-table that returns a special identifier when key is not in the table

(define gloomy-hash-table
  (openlet
   (sublet (*mock-hash-table* 'mock-hash-table-class) ; ideally this would be a separate (not copied) gloomy-hash-table-class 
     'value #f
     'false (gensym)
     'not-a-key #f
     'hash-table-ref (lambda (obj key)
		       (let ((val (#_hash-table-ref (obj 'value) key)))
			 (if (eq? val (obj 'false))
			     #f
			     (or val (obj 'not-a-key)))))
     'hash-table-key? (lambda (obj key)
			(#_hash-table-ref (obj 'value) key)))))

(define (hash-table-key? obj key) 
  ((obj 'hash-table-key?) obj key))

(define* (make-gloomy-hash-table (len 511) not-a-key)
  (let ((ht (copy gloomy-hash-table)))
    (set! (ht 'value) (make-hash-table len))
    (set! (ht 'not-a-key) not-a-key)
    ht))
|#



;;; --------------------------------------------------------------------------------

(define *mock-string*
  (let ((mock-string? #f)
	 (no-mock-strings #f)
	 (->value #f))
    (let ((mock-string-class
	  (inlet 'equivalent?            (lambda (x y) 
					   (#_equivalent? (->value x) (->value y)))

		 'reverse                (lambda (obj) 
					   (#_reverse (->value obj)))

		 'object->string         (lambda args 
					   (let ((w (or (null? (cdr args)) (cadr args))))
					     (copy (if (eq? w :readable) "*mock-string*" "#<mock-string-class>"))))

		 'arity                  (lambda (obj) 
					   (#_arity (->value obj)))

		 'make-iterator          (lambda (obj . args) 
					   (if (pair? args)
					       (error 'wrong-number-of-args "extra trailing args to make-iterator: ~S~%" args))
					   (#_make-iterator (->value obj)))

		 'let-ref-fallback       (lambda (obj i) 
					   (if (and (integer? i)
						    (defined? 'value obj))
					       (#_string-ref (obj 'value) i)           ; these are the implicit cases
					       (error 'out-of-range "unknown field: ~S" i)))

		 'let-set-fallback       (lambda (obj i val) 
					   (if (and (integer? i)
						    (defined? 'value obj))
					       (#_string-set! (obj 'value) i val)
					       (error 'out-of-range "unknown field: ~S" i)))

		 'string-length          (lambda (obj) 
					   (#_length (->value obj)))

		 'string-append          (make-local-method #_string-append)

		 'string-copy            (lambda (obj) 
					   (#_copy (->value obj)))

		 'string=?               (make-local-method #_string=?)
		 'string<?               (make-local-method #_string<?)
		 'string>?               (make-local-method #_string>?)
		 'string<=?              (make-local-method #_string<=?)
		 'string>=?              (make-local-method #_string>=?)

		 'string-downcase        (lambda (obj) (#_string-downcase (->value obj)))
		 'string-upcase          (lambda (obj) (#_string-upcase (->value obj)))
		 'string->symbol         (lambda (obj) (#_string->symbol (->value obj)))
		 'symbol                 (lambda (obj) (#_symbol (->value obj)))
		 'gensym                 (lambda args
					   (if (pair? args)
					       (#_gensym (->value (car args)))
					       (#_gensym)))
		 'string->keyword        (lambda (obj) (#_string->keyword (->value obj)))
		 'open-input-string      (lambda (obj) (#_open-input-string (->value obj)))

		 'call-with-input-string (lambda (obj f) 
					   (if (mock-string? f)
					       (error 'wrong-type-arg "call-with-input-string: stray mock string? ~S" f))
					   (#_call-with-input-string (->value obj) f))

		 'with-input-from-string (lambda (obj f) 
					   (if (mock-string? f)
					       (error 'wrong-type-arg "with-input-from-string: stray mock string? ~S" f))
					   (#_with-input-from-string (->value obj) f))

		 'directory?             (lambda (obj) (#_directory? (->value obj)))
		 'file-exists?           (lambda (obj) (#_file-exists? (->value obj)))
		 'getenv                 (lambda (obj) (#_getenv (->value obj)))
		 'delete-file            (lambda (obj) (#_delete-file (->value obj)))

		 'system                 (lambda* (obj cap) 
					   (if (mock-string? cap)
					       (error 'wrong-type-arg "system: stray mock string? ~S" cap))
					   (#_system (->value obj) cap))

		 'string->byte-vector    (lambda (obj) (#_string->byte-vector (->value obj)))

		 'load                   (lambda* (obj (e (curlet))) 
					   (if (mock-string? e)
					       (error 'wrong-type-arg "load: stray mock string? ~S" e))
					   (#_load (->value obj) e))

		 'eval-string            (lambda* (obj (e (curlet))) 
					   (if (mock-string? e)
					       (error 'wrong-type-arg "eval-string: stray mock string? ~S" e))
					   (#_eval-string (->value obj) e))

		 'char-position          (make-local-method #_char-position)

		 'bignum                 (lambda (obj)
					   (if (provided? 'gmp)
					       (#_bignum (->value obj))
					       (error 'wrong-type-arg "no bignums in this version of s7")))

		 'format                 (lambda (port ctrl . args)
					   (if (mock-string? port)
					       (error 'wrong-type-arg "format port arg is a mock-string: ~A" port))
					   (if (mock-string? ctrl)
					       (apply #_format port (ctrl 'value) args)
					       (#_format port ctrl (->value (car args)))))

		 'string-fill!           (lambda* (obj val (start 0) end) 
					   (if (or (mock-string? val)
						   (mock-string? start)
						   (mock-string? end))
					       (error 'wrong-type-arg "string-fill! stray mock-string? ~A ~A ~A" val start end))
					   (#_string-fill! (->value obj) val start (or end (#_string-length (->value obj)))))
		 
		 'fill!                  (lambda (obj val) 
					   (unless (char? val)
					     (error 'wrong-type-arg "string-fill!: fill value is not a character"))
					   (#_fill! (->value obj) val))
		 
		 'copy                   (lambda (obj . args)
					   (apply #_copy (->value obj) (no-mock-strings args)))
		 
		 'substring              (lambda (obj . args) 
					   (apply #_substring (->value obj) (no-mock-strings args)))
		 
		 'string->number         (lambda* (obj (r 10))
					   (unless (integer? r)
					     (error 'wrong-type-arg "string-ref: stray mock-string?"))
					   (#_string->number (->value obj) r))
		 
		 'write-string           (lambda (obj . args) 
					   (apply #_write-string (->value obj) (no-mock-strings args)))
		 
		 'string-position        (lambda* (s1 s2 (start 0))
					   (if (mock-string? s1)
					       (#_string-position (s1 'value) s2 start)
					       (if (mock-string? s2)
						   (#_string-position s1 (s2 'value) start)
						   (error 'wrong-type-arg "write-string ~S ~S ~S" s1 s2 start))))

		 'string-ref             (lambda (obj i)
					   (unless (integer? i)
					     (error 'wrong-type-arg "string-ref: index is not an integer"))
					   (#_string-ref (->value obj) i))
		 
		 'string-set!            (lambda (obj i val) 
					   (unless (integer? i)
					     (error 'wrong-type-arg "string-set!: index is not an integer"))
					   (unless (char? val)
					     (error 'wrong-type-arg "string-set!: value is not a character"))
					   (#_string-set! (->value obj) i val))
		 
		 'string->list           (if (provided? 'pure-s7)
					     (lambda (obj) (#_map #_values obj))
					     (make-local-method #_string->list))
		 'string-ci=?            (if (provided? 'pure-s7)
					     (lambda strs (apply #_string=? (#_map #_string-upcase strs)))
					     (make-local-method #_string-ci=?))
		 'string-ci<?            (if (provided? 'pure-s7)
					     (lambda strs (apply #_string<? (#_map #_string-upcase strs)))
					     (make-local-method #_string-ci<?))
		 'string-ci>?            (if (provided? 'pure-s7)
					     (lambda strs (apply #_string>? (#_map #_string-upcase strs)))
					     (make-local-method #_string-ci>?))
		 'string-ci<=?           (if (provided? 'pure-s7)
					     (lambda strs (apply #_string<=? (#_map #_string-upcase strs)))
					     (make-local-method #_string-ci<=?))
		 'string-ci>=?           (if (provided? 'pure-s7)
					     (lambda strs (apply #_string>=? (#_map #_string-upcase strs)))
					     (make-local-method #_string-ci>=?))
		 
		 'string?                (lambda (obj) #t)
		 'length                 (lambda (obj) 
					   (#_string-length (->value obj)))
		 'append                 (make-local-method #_append)
		 'class-name             '*mock-string*)))
    
    (define* (make-mock-string len (init #\null))
      (openlet 
       (sublet mock-string-class 
	 'value (#_make-string len init)
	 'object->string mock->string)))
    
    (define (mock-string . args)
      (let ((v (make-mock-string 0)))
	(set! (v 'value) 
	      (if (string? (car args))
		  (car args)
		  (apply #_string args)))
	v))
    
    (set! mock-string? (lambda (obj)
			 (and (openlet? obj)
			      (outlet-member obj mock-string-class))))
    
    (set! no-mock-strings (lambda (lst)
			    (for-each
			     (lambda (c)
			       (if (mock-string? c)
				   (error 'wrong-type-arg "stray mock-string?")))
			     lst)
			    lst))

    (set! ->value (lambda (obj)
      (if (mock-string? obj)
	  (obj 'value)
	  obj)))

    (curlet))))

#|
;; string that is always the current time of day
(require libc.scm)

(define time-string
  (let ((daytime (lambda args
		   (with-let (sublet *libc*)
		     (let ((timestr (make-string 64))) 
		       (let ((len (strftime timestr 64 "%a %d-%b-%Y %H:%M %Z"
					    (localtime 
					     (time.make (time (c-pointer 0)))))))
			 (substring timestr 0 len)))))))
    (openlet
     (sublet (*mock-string* 'mock-string-class) ; the mock-string isn't really needed here
       'let-ref-fallback daytime
       'object->string daytime))))

;; similarly ("JIT data"):
(define ? (openlet 
	   (inlet 'object->string (lambda (obj . args) 
				    (apply #_object->string (owlet) args)))))

|#


;;; --------------------------------------------------------------------------------

(define *mock-char*
  (let ((mock-char? #f)
	(no-mock-chars #f)
	(->value #f))
    (let ((mock-char-class
	  (inlet 'equivalent?        (lambda (x y) (#_equivalent? (->value x) (->value y)))
		 'char-upcase        (lambda (obj) (#_char-upcase (->value obj)))
		 'char-downcase      (lambda (obj) (#_char-downcase (->value obj)))
		 'char->integer      (lambda (obj) (#_char->integer (->value obj)))
		 'char-upper-case?   (lambda (obj) (#_char-upper-case? (->value obj)))
		 'char-lower-case?   (lambda (obj) (#_char-lower-case? (->value obj)))
		 'char-alphabetic?   (lambda (obj) (#_char-alphabetic? (->value obj)))
		 'char-numeric?      (lambda (obj) (#_char-numeric? (->value obj)))
		 'char-whitespace?   (lambda (obj) (#_char-whitespace? (->value obj)))
		 'char=?             (make-local-method #_char=?)
		 'char<?             (make-local-method #_char<?)
		 'char>?             (make-local-method #_char>?)
		 'char<=?            (make-local-method #_char<=?)
		 'char>=?            (make-local-method #_char>=?)
		 'char-ci=?          (make-local-method #_char-ci=?)
		 'char-ci<?          (make-local-method #_char-ci<?)
		 'char-ci>?          (make-local-method #_char-ci>?)
		 'char-ci<=?         (make-local-method #_char-ci<=?)
		 'char-ci>=?         (make-local-method #_char-ci>=?)
		 'string             (make-local-method #_string)

		 'object->string     (lambda args 
				       (let ((w (or (null? (cdr args)) (cadr args))))
					 (copy (if (eq? w :readable) "*mock-char*" "#<mock-char-class>"))))

		 'format             (lambda (port ctrl . args)
				       (if (mock-char? port)
					   (error 'wrong-type-arg "format port arg is a mock-char: ~A" port))
				       (if (mock-char? ctrl)
					   (error 'wrong-type-arg "format control-string arg is a mock-char: ~A" ctrl))
				       (#_format port ctrl (->value (car args))))

		 'arity              (lambda (obj) (#_arity (->value obj)))
		 'make-string        (make-local-method #_make-string)
		 'char-position      (make-local-method #_char-position)
		 
		 'write-char         (lambda (obj . args) 
				       (apply #_write-char (->value obj) (no-mock-chars args)))
		 
		 'string-set!        (lambda (obj ind val)
				       (if (and (string? obj)
						(integer? ind))
					   (#_string-set! obj ind (->value val))
					   (error 'wrong-type-arg "string-set! ~S ~S ~S" obj ind val)))

		 'string-fill!       (lambda (obj chr . args)
				       (if (string? obj)
					   (apply #_string-fill! obj (->value chr) (no-mock-chars args))
					   (error 'wrong-type-arg "string-fill! ~S ~S ~S" obj chr args)))
		 
		 'copy               (lambda (src . args) 
				       (apply #_copy (->value src) (no-mock-chars args)))

		 'char?              (lambda (obj) #t)
		 'class-name         '*mock-char*
		 'length             (lambda (obj) #f))))
    
    (define (mock-char c) 
      (if (and (char? c)
	       (not (let? c)))
	  (immutable!
	   (openlet
	    (sublet (*mock-char* 'mock-char-class)
	      'value c
	      'object->string mock->string)))
	  (error 'wrong-type-arg "mock-char ~S is not a char" c)))
    
    (set! mock-char? (lambda (obj)
		       (and (openlet? obj)
			    (outlet-member obj mock-char-class))))
    
    (set! no-mock-chars (lambda (lst)
			  (for-each
			   (lambda (c)
			     (if (mock-char? c)
				 (error 'wrong-type-arg "stray mock-char?")))
			   lst)
			  lst))

    (set! ->value (lambda (obj)
      (if (mock-char? obj)
	  (obj 'value)
	  obj)))

    (curlet))))

#|
;;; eventually I'll conjure up unichars like (define lambda (byte-vector #xce #xbb)) via mock-char,
;;;   then combine those into unistring via mock-string
;;;
;;; (string-length obj)->g_utf8_strlen etc 
;;;   (g_unichar_isalpha (g_utf8_get_char (byte-vector #xce #xbb))) -> #t
;;;   (g_utf8_strlen (byte-vector #xce #xbb #xce #xba) 10) -> 2
;;;   (g_utf8_normalize (byte-vector #xce #xbb #xce #xba) 4 G_NORMALIZE_DEFAULT)
;;;   but the ones that return gunichar (toupper) currently don't return a byte-vector or a string
;;;   maybe gunichar->byte-vector?
;;;   need glib.scm, or unicode.scm to load the stuff
|#



;;; --------------------------------------------------------------------------------

(define *mock-number*
  (let ((mock-number? #f)
	(no-mock-numbers #f)
	(vref #f)
	(->value #f)
	(with-bounds #f))
    
    (let ((mock-number-class
	   (inlet 
	   'equivalent?      (lambda (x y) (#_equivalent? (->value x) (->value y)))
	   'object->string   (lambda args 
			       (let ((w (or (null? (cdr args)) (cadr args))))
				 (unless (or (boolean? w) (eq? w :readable))
				   (error 'wrong-type-arg "~A should be boolean of :readable" w))
				 (copy (if (eq? w :readable) "*mock-number*" "#<mock-number-class>"))))
	   'arity            (lambda (obj) (#_arity (->value obj)))
	   'real-part        (lambda (obj) (#_real-part (->value obj)))
	   'imag-part        (lambda (obj) (#_imag-part (->value obj)))
	   'numerator        (lambda (obj) (#_numerator (->value obj)))
	   'denominator      (lambda (obj) (#_denominator (->value obj)))
	   'even?            (lambda (obj) (#_even? (->value obj)))
	   'odd?             (lambda (obj) (#_odd? (->value obj)))
	   'zero?            (lambda (obj) (#_zero? (->value obj)))
	   'positive?        (lambda (obj) (#_positive? (->value obj)))
	   'negative?        (lambda (obj) (#_negative? (->value obj)))
	   'infinite?        (lambda (obj) (#_infinite? (->value obj)))
	   'nan?             (lambda (obj) (#_nan? (->value obj)))
	   'append           (lambda args (apply #_append (map ->value args)))
	   'make-polar       (if (provided? 'pure-s7)
				 (lambda (mag ang) (#_complex (* mag (cos ang)) (* mag (sin ang))))
				 (make-local-method #_make-polar))
	   'make-rectangular (make-local-method #_complex)
	   'complex          (make-local-method #_complex)
	   'random-state     (make-local-method #_random-state)
	   'magnitude        (lambda (obj) (#_magnitude (->value obj)))
	   'angle            (lambda (obj) (#_angle (->value obj)))
	   'rationalize      (make-local-method #_rationalize)
	   'abs              (lambda (obj) (#_abs (->value obj)))
	   'exp              (lambda (obj) (#_exp (->value obj)))
	   'log              (make-local-method #_log)
	   'sin              (lambda (obj) (#_sin (->value obj)))
	   'cos              (lambda (obj) (#_cos (->value obj)))
	   'tan              (lambda (obj) (#_tan (->value obj)))
	   'asin             (lambda (obj) (#_asin (->value obj)))
	   'acos             (lambda (obj) (#_acos (->value obj)))
	   'atan             (make-local-method #_atan)
	   'sinh             (lambda (obj) (#_sinh (->value obj)))
	   'cosh             (lambda (obj) (#_cosh (->value obj)))
	   'tanh             (lambda (obj) (#_tanh (->value obj)))
	   'asinh            (lambda (obj) (#_asinh (->value obj)))
	   'acosh            (lambda (obj) (#_acosh (->value obj)))
	   'atanh            (lambda (obj) (#_atanh (->value obj)))
	   'sqrt             (lambda (obj) (#_sqrt (->value obj)))
	   'expt             (make-local-method #_expt)
	   'floor            (lambda (obj) (#_floor (->value obj)))
	   'ceiling          (lambda (obj) (#_ceiling (->value obj)))
	   'truncate         (lambda (obj) (#_truncate (->value obj)))
	   'round            (lambda (obj) (#_round (->value obj)))
	   'integer->char    (lambda (obj) (#_integer->char (->value obj)))
	   'inexact->exact   (lambda (obj) (#_inexact->exact (->value obj)))
	   'exact->inexact   (lambda (obj) (#_exact->inexact (->value obj)))
	   'integer-length   (lambda (obj) (#_integer-length (->value obj)))
	   'integer-decode-float (lambda (obj) (#_integer-decode-float (->value obj)))
	   'number?          (lambda (obj) (#_number? (->value obj))) ; why not #t? currently (mock-number (mock-number ...)) is an error
	   'integer?         (lambda (obj) (#_integer? (->value obj)))
	   'real?            (lambda (obj) (#_real? (->value obj)))
	   'complex?         (lambda (obj) (#_complex? (->value obj)))
	   'rational?        (lambda (obj) (#_rational? (->value obj)))
	   'exact?           (lambda (obj) (#_exact? (->value obj)))
	   'inexact?         (lambda (obj) (#_inexact? (->value obj)))
	   'ash              (lambda (x y) (ash (->value x) (->value y)))
	   'logbit?          (lambda (x y) (logbit? (->value x) (->value y)))
	   'number->string   (lambda args (apply #_number->string (map ->value args)))
	   'random           (lambda* (range state)
			       (if state
				   (if (random-state? state)
				       (#_random (->value range) state)
				       (error 'wrong-type-arg "~S is not a random-state" state))
				   (#_random (->value range))))
	   'quotient         (lambda (x y) (quotient (->value x) (->value y)))
	   'remainder        (lambda (x y) (remainder (->value x) (->value y)))
	   'modulo           (lambda (x y) (modulo (->value x) (->value y)))
	   'lognot           (lambda (obj) (#_lognot (->value obj)))
	   'logior           (lambda args (apply #_logior (map ->value args)))
	   'logxor           (lambda args (apply #_logxor (map ->value args)))
	   'logand           (lambda args (apply #_logand (map ->value args)))
	   ;; any object that has lcm or gcd also needs rational?
	   'lcm              (lambda args (apply #_lcm (map ->value args)))
	   'gcd              (lambda args (apply #_gcd (map ->value args)))
	   '+                (lambda args (apply #_+ (map ->value args)))
	   '-                (lambda args (apply #_- (map ->value args)))
	   '*                (lambda args (apply #_* (map ->value args)))
	   '/                (lambda args (apply #_/ (map ->value args)))
	   'max              (lambda args (apply #_max (map ->value args)))
	   'min              (lambda args (apply #_min (map ->value args)))
	   '=                (lambda args (apply #_= (map ->value args)))
	   '<                (lambda args (apply #_< (map ->value args)))
	   '>                (lambda args (apply #_> (map ->value args)))
	   '<=               (lambda args (apply #_<= (map ->value args)))
	   '>=               (lambda args (apply #_>= (map ->value args)))

	   'write-byte       (lambda* (byte port)
			       (if (mock-number? port)
				   (error 'wrong-type-arg "write-byte: stray mock-number? ~A ~A" byte port))
			       (#_write-byte (->value byte) (or port *stdout*)))
	   
	   'make-list        (lambda (ind . args) (apply #_make-list (->value ind) (map ->value args)))
	   'make-vector      (make-local-method #_make-vector)
	   'make-float-vector(make-local-method #_make-float-vector)
	   'make-hash-table  (lambda args
			       (if (pair? args)
				   (let ((size (car args)))
				     (apply #_make-hash-table (->value size) (no-mock-numbers (cdr args))))
				   (#_make-hash-table)))
	   'make-byte-vector  (make-local-method #_make-byte-vector)
	   
	   'format           (lambda (port ctrl . args)
			       (if (mock-number? port)
				   (error 'wrong-type-arg "format port arg is a mock-number: ~A" port))
			       (if (mock-number? ctrl)
				   (error 'wrong-type-arg "format control-string arg is a mock-number: ~A" ctrl))
			       (#_format port ctrl (->value (car args))))

	   'vector->list     (lambda* (v (start 0) end)
			       (with-bounds vector->list 'vector v start end))

	   'string->list     (lambda* (v (start 0) end)
			       (with-bounds string->list 'string v start end))
	   
	   'substring        (lambda* (v (start 0) end)
			       (with-bounds substring 'string v start end))
	   
	   'vector-fill!     (lambda* (v val (start 0) end)
			       (if (mock-number? v)
				   (error 'wrong-type-arg "~S is a mock-number, not a vector" v)
				   (if (mock-number? val)
				       (vector-fill! v (val 'value) start end)
				       (let ((i1 (->value start))
					     (i2 (if (mock-number? end)
						     (end 'value)
						     (or end (length v)))))
					 (if (and (integer? i1) (integer? i2))
					     (vector-fill! v val i1 i2)
					     (error 'wrong-type-arg "start and end should be integers: ~S ~S" start end))))))
	   
	   'string-fill!     (lambda* (v val (start 0) end)
			       (if (mock-number? v)
				   (error 'wrong-type-arg "~S is a mock-number, not a string" v)
				   (if (mock-number? val)
				       (error 'wrong-type-arg "~S is a mock-number, not a character" v)
				       (let ((i1 (->value start))
					     (i2 (if (mock-number? end)
						     (end 'value)
						     (or end (length v)))))
					 (if (and (integer? i1) (integer? i2))
					     (string-fill! v val i1 i2)
					     (error 'wrong-type-arg "start and end should be integers: ~S ~S" start end))))))
	   
	   'copy             (lambda* (v val (start 0) end)
			       (if (mock-number? v)
				   (#_copy (v 'value))
				   (let ((i1 (->value start))
					 (i2 (if (mock-number? end)
						 (end 'value)
						 (or end (length v)))))
				     (if (and (integer? i1) (integer? i2))
					 (#_copy v val i1 i2)
					 (error 'wrong-type-arg "start and end should be integers: ~S ~S" start end)))))
	   
	   'make-string      (lambda (ind . args) 
			       (apply #_make-string (->value ind) (no-mock-numbers args)))

	   'string-ref       (lambda (str ind) 
			       (if (string? str)
				   (#_string-ref str (ind 'value))
				   (error 'wrong-type-arg "make-string ~S ~S" str ind)))
	   
	   'string-set!      (lambda (str ind val) 
			       (if (string? str)
				   (#_string-set! str (ind 'value) val)
				   (error 'wrong-type-arg "string-set! ~S ~S ~S" str ind val)))
	   
	   'string->number   (lambda* (str (radix 10))
			       (if (string? str)
				   (#_string->number str (radix 'value))
				   (error 'wrong-type-arg "string->number ~S ~S" str radix)))
	   
	   'list-ref         (lambda (lst ind) 
			       (if (pair? lst)
				   (#_list-ref lst (ind 'value))
				   (error 'wrong-type-arg "list-ref ~S ~S" lst ind)))
	   
	   'list-set!        (lambda (lst ind . rest)
			       (if (pair? lst)
				   (if (mock-number? ind)
				       (apply #_list-set! lst (ind 'value) rest)
				       (let loop ((ok-inds ())
						  (inds rest))
					 (if (null? (cdr inds))
					     (apply #_list-set! lst ind (append (reverse ok-inds) inds))
					     (if (mock-number? (car inds))
						 (apply #_list-set! lst ind (append (reverse ok-inds) (cons ((car inds) 'value) (cdr inds))))
						 (loop (cons ok-inds (car inds))
						       (cdr inds))))))
				   (error 'wrong-type-arg "list-set! ~S ~S~{~^ ~S~}" lst ind rest)))
	   
	   'list-tail        (lambda (lst ind) 
			       (if (pair? lst)
				   (#_list-tail lst (ind 'value))
				   (error 'wrong-type-arg "list-tail ~S ~S" ind args)))
	   
	   'vector-ref       (lambda (vec ind . indices) (apply vref #_vector-ref vec ind indices))
	   'float-vector-ref (lambda (vec ind . indices) (apply vref #_float-vector-ref vec ind indices))
	   'int-vector-ref   (lambda (vec ind . indices) (apply vref #_int-vector-ref vec ind indices))
	   'byte-vector-ref  (lambda (vec ind . indices) (apply vref #_byte-vector-ref vec ind indices))

	   'vector-set!      (lambda (vec ind . rest) (apply vref #_vector-set! vec ind rest))
	   'float-vector-set! (lambda (vec ind . rest) (apply vref #_float-vector-set! vec ind rest))
	   'int-vector-set!  (lambda (vec ind . rest) (apply vref #_int-vector-set! vec ind rest))
	   'byte-vector-set! (lambda (vec ind . rest) (apply vref #_byte-vector-set! vec ind rest))
	   
	   'float-vector     (lambda args (apply #_float-vector (map ->value args)))
	   'int-vector       (lambda args (apply #_int-vector (map ->value args)))
	   'byte-vector      (lambda args (apply #_byte-vector (map ->value args)))

	   'subvector        (lambda* (obj dims (offset 0))
			       (if (mock-number? obj)
				   (error 'wrong-type-arg "subvector first arg is not a vector: ~S" obj))
			       (#_subvector obj (->value dims) (->value offset)))
	   
	   'read-string     (lambda* (k port)
			      (if (input-port? port)
				  (#_read-string (k 'value) port)
				  (error 'wrong-type-arg "read-string ~S ~S" k port)))

	   'length           (lambda (obj) #f)
	   'number?          (lambda (obj) #t)
	   'class-name       '*mock-number*)))
    
    (define (mock-number x)
      (if (and (number? x)
	       (not (let? x)))
	  (immutable!
	   (openlet
	    (sublet (*mock-number* 'mock-number-class)
	      'value x
	      'object->string (lambda args
				(apply #_object->string (map ->value args))))))
	  (error 'wrong-type-arg "mock-number ~S is not a number" x)))
    
    (set! mock-number? (lambda (obj)
			 (and (openlet? obj)
			      (outlet-member obj mock-number-class))))
    
    (set! no-mock-numbers (lambda (lst)
			    (for-each
			     (lambda (c)
			       (if (mock-number? c)
				   (error 'wrong-type-arg "stray mock-number?")))
			     lst)
			    lst))
    (set! vref (lambda (ref vec ind . indices)
      (if (vector? vec)
	  (if (mock-number? ind)
	      (apply ref vec (ind 'value) indices)
	      (let loop ((ok-inds ())
			 (inds indices))
		(if (mock-number? (car inds))
		    (apply ref vec ind (append (reverse ok-inds) (cons ((car inds) 'value) (cdr inds))))
		    (loop (cons ok-inds (car inds))
			  (cdr inds)))))
	  (error 'wrong-type-arg "~S: ~S ~S~{~^ ~S~}" ref vec ind indices))))

    (set! ->value (lambda (obj)
      (if (mock-number? obj)
	  (obj 'value)
	  obj)))

    (set! with-bounds (lambda (func type v start end)
      (if (mock-number? v)
	  (error 'wrong-type-arg "~S is a mock-number, not a ~S" v type)
	  (let ((i1 (->value start))
		(i2 (if (mock-number? end)
			(end 'value)
			(or end (length v)))))
	    (if (and (integer? i1) (integer? i2))
		(func v i1 i2)
		(error 'wrong-type-arg "start and end should be integers: ~S ~S" start end))))))

    (curlet))))


#|
;;; fuzzy number

(define fuzzy-number
  (let ((fuzz (lambda (fx)
		(#_* fx (#_- 1.05 (#_random .1))))))
    (lambda (fx)
      (openlet 
       (sublet 
	   (*mock-number* 'mock-number-class)
	 'let-ref-fallback (lambda (obj sym) (fuzz fx))
	 'object->string (lambda (obj . args) (#_number->string (fuzz fx))))))))


;;; interval arithmetic 
;;; 
;;; from Wikipedia:
;;; x + y =	[a+c, b+d]
;;; x - y =	[a-d, b-c]
;;; x × y =	[min(ac, ad, bc, bd), max(ac, ad, bc, bd)]
;;; x / y =	[min(a/c, a/d, b/c, b/d), max(a/c, a/d, b/c, b/d)]

(define *interval*
  (let* ((make-interval #f)
	 (low (lambda (z) (z 'low)))
	 (high (lambda (z) (z 'high)))
	 (interval-class 
	  (openlet (sublet (*mock-number* 'mock-number-class)
		     
		     '+ (lambda args
			  (let ((lo 0)
				(hi 0))
			    (for-each
			     (lambda (z)
			       (if (let? z)
				   (begin
				     (set! lo (+ lo (low z)))
				     (set! hi (+ hi (high z))))
				   (begin
				     (set! lo (+ lo z))
				     (set! hi (+ hi z)))))
			     args)
			    (make-interval lo hi)))
		     
		     '* (lambda args
			  (let ((lo 1)
				(hi 1))
			    (for-each
			     (lambda (z)
			       (let ((zlo (if (let? z) (low z) z))
				     (zhi (if (let? z) (high z) z)))
				 (let ((ac (* lo zlo))
				       (ad (* lo zhi))
				       (bc (* hi zlo))
				       (bd (* hi zhi)))
				   (set! lo (min ac ad bc bd))
				   (set! hi (max ac ad bc bd)))))
			     args)
			    (make-interval lo hi)))
		     
		     '- (lambda args
			  (let ((z (car args)))
			    (if (null? (cdr args)) ; negate (must be let? else how did we get here?)
				(make-interval (- (high z)) (- (low z)))
				(let ((lo (low z))
				      (hi (high z)))
				  (for-each
				   (lambda (z)
				     (if (let? z)
					 (begin
					   (set! lo (- lo (high z)))
					   (set! hi (- hi (low z))))
					 (begin
					   (set! lo (- lo z))
					   (set! hi (- hi z)))))
				   (cdr args))
				  (make-interval lo hi)))))
		     
		     '/ (lambda args
			  (let ((z (car args)))
			    (if (null? (cdr args)) ; invert
				(make-interval (/ (high z)) (/ (low z)))
				(let ((lo (low z))
				      (hi (high z)))
				  (for-each
				   (lambda (z)
				     (let ((zlo (if (let? z) (low z) z))
					   (zhi (if (let? z) (high z) z)))
				       (let ((ac (/ lo zlo))
					     (ad (/ lo zhi))
					     (bc (/ hi zlo))
					     (bd (/ hi zhi)))
					 (set! lo (min ac ad bc bd))
					 (set! hi (max ac ad bc bd)))))
				   (cdr args))
				  (make-interval lo hi)))))
		     
		     'abs (lambda (z)
			    (if (positive? (low z))
				(make-interval (low z) (high z))
				(if (negative? (high z))
				    (make-interval (abs (high z)) (abs (low z)))
				    (make-interval 0 (max (abs (low z)) (abs (high z)))))))
		     
		     'object->string (lambda (obj . args) 
				       (format #f "#<interval: ~S ~S>" (low obj) (high obj)))
		     ))))
    
    (set! make-interval (lambda (low high)
			  (if (> low high) (format *stderr* "~A ~A~%" low high))
			  (openlet (sublet interval-class 'low low 'high high))))
    
    (curlet)))

(define x ((*interval* 'make-interval) 3.0 4.0))
|#



;;; --------------------------------------------------------------------------------

(define *mock-pair*
  (let ((mock-pair? #f)
	(no-mock-pairs #f)
	(->value #f))
    (let ((mock-pair-class
	  (inlet 'equivalent?      (lambda (x y) (#_equivalent? (->value x) (->value y)))
		 'pair-line-number (lambda (obj) (#_pair-line-number (->value obj)))
		 'list->string     (lambda (obj) (#_list->string (->value obj)))
		 'object->string   (lambda args 
				     (let ((w (or (null? (cdr args)) (cadr args))))
				       (copy (if (eq? w :readable) "*mock-pair*" "#<mock-pair-class>"))))
		 'list?            (lambda (obj) (#_list? (->value obj)))
		 'car              (lambda (obj) (#_car (->value obj)))
		 'cdr              (lambda (obj) (#_cdr (->value obj)))
		 'set-car!         (lambda (obj val) (#_set-car! (->value obj) val))
		 'set-cdr!         (lambda (obj val) (#_set-cdr! (->value obj) val))
		 'caar             (lambda (obj) (#_caar (->value obj)))
		 'cadr             (lambda (obj) (#_cadr (->value obj)))
		 'cdar             (lambda (obj) (#_cdar (->value obj)))
		 'cddr             (lambda (obj) (#_cddr (->value obj)))
		 'caaar            (lambda (obj) (#_caaar (->value obj)))
		 'caadr            (lambda (obj) (#_caadr (->value obj)))
		 'cadar            (lambda (obj) (#_cadar (->value obj)))
		 'cdaar            (lambda (obj) (#_cdaar (->value obj)))
		 'caddr            (lambda (obj) (#_caddr (->value obj)))
		 'cdddr            (lambda (obj) (#_cdddr (->value obj)))
		 'cdadr            (lambda (obj) (#_cdadr (->value obj)))
		 'cddar            (lambda (obj) (#_cddar (->value obj)))
		 'caaaar           (lambda (obj) (#_caaaar (->value obj)))
		 'caaadr           (lambda (obj) (#_caaadr (->value obj)))
		 'caadar           (lambda (obj) (#_caadar (->value obj)))
		 'cadaar           (lambda (obj) (#_cadaar (->value obj)))
		 'caaddr           (lambda (obj) (#_caaddr (->value obj)))
		 'cadddr           (lambda (obj) (#_cadddr (->value obj)))
		 'cadadr           (lambda (obj) (#_cadadr (->value obj)))
		 'caddar           (lambda (obj) (#_caddar (->value obj)))
		 'cdaaar           (lambda (obj) (#_cdaaar (->value obj)))
		 'cdaadr           (lambda (obj) (#_cdaadr (->value obj)))
		 'cdadar           (lambda (obj) (#_cdadar (->value obj)))
		 'cddaar           (lambda (obj) (#_cddaar (->value obj)))
		 'cdaddr           (lambda (obj) (#_cdaddr (->value obj)))
		 'cddddr           (lambda (obj) (#_cddddr (->value obj)))
		 'cddadr           (lambda (obj) (#_cddadr (->value obj)))
		 'cdddar           (lambda (obj) (#_cdddar (->value obj)))
		 'assoc            (lambda (val obj . args) (apply #_assoc val (->value obj) (no-mock-pairs args)))
		 'assq             (lambda (val obj) (#_assq val (->value obj)))
		 'assv             (lambda (val obj) (#_assv val (->value obj)))
		 'memq             (lambda (val obj) (#_memq val (->value obj)))
		 'memv             (lambda (val obj) (#_memv val (->value obj)))
		 'member           (lambda (val obj . args) (apply #_member val (->value obj) (no-mock-pairs args)))
		 'let-ref-fallback (lambda (obj ind) 
				     (if (eq? ind 'value)
					 #<undefined>
					 (let ((val (begin 
						      (coverlet obj)
						      (#_list-ref (obj 'value) ind))))
					   (openlet obj) 
					   val)))
		 'let-set-fallback (lambda (obj ind val) 
				     (if (eq? ind 'value)
					 #<undefined>
					 (let ((val (begin 
						      (coverlet obj)
						      (#_list-set! (obj 'value) ind val))))
					   (openlet obj)
					   val)))
		 'arity            (lambda (obj) (#_arity (->value obj)))
		 'fill!            (lambda (obj val) (#_fill! (->value obj) val))
		 'reverse          (lambda (obj) (#_reverse (->value obj)))
		 'reverse!         (lambda (obj) 
				     (if (mock-pair? obj)
					 (set! (obj 'value) (#_reverse (obj 'value)))
					 (#_reverse! obj)))

		 'sort!            (lambda (obj f)
				     (if (mock-pair? f)
					 (error 'wrong-type-arg "sort function arg is a mock-pair? ~S" f))
				     (#_sort! (->value obj) f))

		 'make-iterator    (lambda (obj) (#_make-iterator (->value obj)))
		 'eval             (lambda (f obj) (#_eval (->value obj)))
		 'list->vector     (lambda (obj) (#_list->vector (->value obj)))
		 
		 'list-tail        (lambda (obj . args)
				     (when (and (pair? args)
						(not (integer? (car args))))
				       (error 'wrong-type-arg "list-tail: index is not an integer: ~A~%" (car args)))
				     (apply #_list-tail (->value obj) args))
		 
		 'copy             (lambda (obj . args) 
				     (when (and (pair? args)
						(pair? (cdr args)))
				       (if (or (not (integer? (cadr args)))
					       (and (pair? (cddr args))
						    (not (integer? (caddr args)))))
					   (error 'wrong-type-arg "copy: start or end point is not an integer: ~A~%" (cdr args))))
				     (apply #_copy (->value obj) args))
		 
		 'subvector          (lambda* (obj dims (off 0))
				       (if (or (mock-pair? obj)
					       (mock-pair? off))
					   (error 'wrong-type-arg "subvector: stray mock-pair? ~S ~S~%" obj off))
				       (#_subvector obj (->value dims) off))

		 'make-vector      (lambda (dims . args) 
				     (apply #_make-vector (->value dims) (no-mock-pairs args)))

		 'list-ref         (lambda (obj ind)
				     (#_list-ref (->value obj) (->value ind)))
		 
		 'list-set!        (lambda (obj ind val)
				     (#_list-set! (->value obj) (->value ind) (->value val)))

		 'pair?            (lambda (obj) #t)
		 'length           (lambda (obj) (#_length (->value obj)))
		 'append           (make-local-method #_append)
		 'class-name       '*mock-pair*)))
    
    (define (mock-pair . args)
      (openlet
       (sublet (*mock-pair* 'mock-pair-class)
	 'value (copy args)
	 'object->string (lambda (obj . args)
			   (format #f (cond ((null? args) "~S")
					    ((not (car args)) "~A")
					    ((eq? (car args) :readable) "'~S")
					    (else "~S"))
				   (obj 'value))))))
    
    (set! mock-pair? (lambda (obj)
		       (and (openlet? obj)
			    (outlet-member obj mock-pair-class))))
    
    (set! no-mock-pairs (lambda (lst)
			  (for-each
			   (lambda (c)
			     (if (mock-pair? c)
				 (error 'wrong-type-arg "stray mock-pair?")))
			   lst)
			  lst))

    (set! ->value (lambda (obj)
      (if (mock-pair? obj)
	  (obj 'value)
	  obj)))

    (curlet))))

#|
(let ((immutable-list-class 
       (sublet (*mock-pair* 'mock-pair-class)
	 'object->string   mock->string
	 
	 'let-set-fallback (lambda (obj i val) 
			     (set! (obj 'value) (append (copy (obj 'value) (make-list (+ i 1))) (list-tail (obj 'value) (+ i 1))))
			     (list-set! (obj 'value) i val))
	 'list-set!        (lambda (obj i val) 
			     (set! (obj 'value) (append (copy (obj 'value) (make-list (+ i 1))) (list-tail (obj 'value) (+ i 1))))
			     (list-set! (obj 'value) i val))
	 'set-car!         (lambda (obj val) 
			     (set! (obj 'value) (cons val (cdr (obj 'value)))))
	 'set-cdr!         (lambda (obj val) 
			     (set! (obj 'value) (cons (car (obj 'value)) val)))
	 'fill!            (lambda (obj val) 
			     (set! (obj 'value) (fill! (copy (obj 'value)) val)))
	 'reverse!         (lambda (obj) 
			     (set! (obj 'value) (reverse (obj 'value))))
	 'sort!            (lambda (obj func) 
			     (set! (obj 'value) (sort! (copy (obj 'value)) func))))))
  
  (define (immutable-list lst)
    (openlet 
     (sublet immutable-list-class
       'value lst))))
|#

;;; since a mock-pair prints itself as if a list, you can get some strange printout results:
;;;    (cons 'a ((*mock-pair* 'mock-pair) 'b 'c)) -> '(a . (b c))



;;; --------------------------------------------------------------------------------

(define *mock-symbol*
  (let ((mock-symbol? #f)
	(no-mock-symbols #f)
	(->value #f))
    (let ((mock-symbol-class
	  (inlet 'object->string        (lambda args 
					  (let ((w (or (null? (cdr args)) (cadr args))))
					    (copy (if (eq? w :readable) "*mock-symbol*" "#<mock-symbol-class>"))))
		 'equivalent?           (lambda (x y) (#_equivalent? (->value x) (->value y)))
		 'gensym?               (lambda (obj) (#_gensym? (->value obj)))
		 'append                (lambda args (apply #_append (map ->value args)))
		 'symbol->string        (lambda (obj) (#_symbol->string (->value obj)))
		 'symbol->value         (lambda (obj . args) (apply #_symbol->value (->value obj) (no-mock-symbols args)))
		 'symbol->dynamic-value (lambda (obj) (#_symbol->dynamic-value (->value obj)))
		 'setter                (lambda (obj . args) (apply #_setter (->value obj) (no-mock-symbols args)))
		 'provided?             (lambda (obj) (#_provided? (->value obj)))
		 'provide               (lambda (obj) (#_provide (->value obj)))
		 'defined?              (lambda* (obj e globals) (#_defined? (->value obj) (or e (curlet))))
		 'symbol->keyword       (lambda (obj) (#_symbol->keyword (->value obj)))
		 'keyword?              (lambda (obj) (#_keyword? (->value obj)))
		 'keyword->symbol       (lambda (obj) (#_keyword->symbol (->value obj)))

		 'format                (lambda (port str . args) 
					  (if (mock-symbol? port)
					      (error 'wrong-type-arg "format: port arg is a mock-symbol? ~A" (port 'value)))
					  (if (mock-symbol? str)
					      (error 'wrong-type-arg "format: control string arg is a mock-symbol? ~A" (str 'value)))
					  (apply #_format port str (map ->value args)))
		 
		 'symbol?               (lambda (obj) #t)
		 'class-name            '*mock-symbol*
		 )))
    
    (define (mock-symbol s)
      (if (and (symbol? s)
	       (not (let? s)))
	  (immutable!
	   (openlet
	    (sublet (*mock-symbol* 'mock-symbol-class)
	      'value s
	      'object->string mock->string)))
	  (error 'wrong-type-arg "mock-symbol ~S is not a symbol" s)))
    
    (set! mock-symbol? (lambda (obj)
			 (and (openlet? obj)
			      (outlet-member obj mock-symbol-class))))
    
    (set! no-mock-symbols (lambda (lst)
			    (for-each
			     (lambda (c)
			       (if (mock-symbol? c)
				   (error 'wrong-type-arg "stray mock-symbol?")))
			     lst)
			    lst))

    (set! ->value (lambda (obj)
      (if (mock-symbol? obj)
	  (obj 'value)
	  obj)))

    (curlet))))


;;; --------------------------------------------------------------------------------

(define *mock-c-pointer*
  (let ((->value #f))
    (let ((mock-c-pointer-class
	   (inlet 'object->string  (lambda args "*c-pointer*")
		  'c-pointer?      (lambda (obj) #t)
		  'c-pointer-type  (lambda (obj) (vector-ref (->value obj) 1))
		  'c-pointer-info  (lambda (obj) (vector-ref (->value obj) 2))
		  'c-pointer-weak1 (lambda (obj) (vector-ref (->value obj) 3))
		  'c-pointer-weak2 (lambda (obj) (vector-ref (->value obj) 4))
		  'c-pointer->list (lambda (obj) (vector->list (->value obj))))))

    (define* (mock-c-pointer (int 0) type info weak1 weak2)
      (immutable!
       (openlet 
	(sublet (*mock-c-pointer* 'mock-c-pointer-class)
	  'value (vector int type info weak1 weak2)))))
		
    (define (mock-c-pointer? obj)
      (and (openlet? obj)
	   (outlet-member obj mock-c-pointer-class)))
    
    (set! ->value (lambda (obj)
      (if (mock-c-pointer? obj)
	  (obj 'value)
	  obj)))

    (curlet))))


;;; --------------------------------------------------------------------------------

(define *mock-port*
  (let ((mock-port? #f)
	(no-mock-ports #f)
	(->value #f))
    (let ((mock-port-class
	  (inlet 'port?               (lambda (obj) (or (mock-port? obj) (#_port? obj)))
		 'equivalent?         (lambda (x y) (#_equivalent? (->value x) (->value y)))
		 'append              (lambda args  (error 'wrong-type-arg "stray mock-port?"))

		 'close-input-port    (lambda (obj) (#_close-input-port (->value obj)))
		 'close-output-port   (lambda (obj) (#_close-output-port (->value obj)))
		 'flush-output-port   (lambda args
					(if (null? args)
					    (#_flush_output_port)
					    (#_flush-output-port (->value (car args)))))

		 'set-current-output-port (lambda (obj) (#_set-current-output-port (->value obj)))
		 'set-current-input-port  (lambda (obj) (#_set-current-input-port (->value obj)))
		 'set-current-error-port  (lambda (obj) (#_set-current-error-port (->value obj)))
		 
		 'write               (lambda* (x (obj *stdout*)) (#_write (->value x) (->value obj)))
		 'display             (lambda* (x (obj *stdout*)) (#_display (->value x) (->value obj)))

		 'get-output-string   (lambda args
					(if (null? args) 
					    (#_get-output-string)
					    (let ((obj (car args))) 
					      (#_get-output-string (->value obj)))))
					      
		 'newline             (lambda args  
					(if (null? args) 
					    (#_newline)
					    (let ((obj (car args))) 
					      (#_newline (->value obj)))))

		 'read-char           (lambda args 
					(if (null? args) 
					    (#_read-char)
					    (let ((obj (car args))) 
					      (#_read-char (->value obj)))))

		 'peek-char           (lambda args 
					(if (null? args) 
					    (#_peek-char)
					    (let ((obj (car args))) 
					      (#_peek-char (->value obj)))))

		 'read-byte           (lambda args 
					(if (null? args) 
					    (#_read-byte)
					    (let ((obj (car args))) 
					      (#_read-byte (->value obj)))))

		 'read-line           (lambda args
					(if (null? args) 
					    (#_read-line)
					    (let ((obj (car args))) 
					      (apply #_read-line (->value obj) (cdr args)))))

		 'read                (lambda args 
					(if (null? args) 
					    (#_read)
					    (let ((obj (car args))) 
					      (#_read (->value obj)))))

		 'input-port?         (lambda (obj) (#_input-port? (->value obj)))
		 'output-port?        (lambda (obj) (#_output-port? (->value obj)))
		 'port-closed?        (lambda (obj) (#_port-closed? (->value obj)))
		 'char-ready?         (lambda (obj) (#_char-ready? (->value obj)))
		 'port-line-number    (lambda (obj) (#_port-line-number (->value obj)))
		 'port-filename       (lambda (obj) (#_port-filename (->value obj)))

		 'object->string      (lambda args 
					(let ((w (or (null? (cdr args)) (cadr args))))
					  (copy (if (eq? w :readable) "*mock-port*" "#<mock-port-class>"))))

		 'format              (lambda (port str . args)
					(if (mock-port? str)
					    (error 'wrong-type-arg "format control-string is a mock-port: ~A" str))
					(if (mock-port? port)
					    (apply #_format (port 'value) str args)
					    (#_format port str (->value (car args)))))
		 
		 'write-char          (lambda (c obj) 
					(if (mock-port? c)
					    (error 'wrong-type-arg "write-char: first arg should be a char: ~A" c))
					(#_write-char c (->value obj)))
		 
		 'write-string        (lambda (s obj . args) 
					(if (mock-port? s)
					    (error 'wrong-type-arg "write-string: first arg should be a string: ~A" s))
					(apply #_write-string s (->value obj) (no-mock-ports args)))
		 
		 'write-byte          (lambda (c obj) 
					(if (mock-port? c)
					    (error 'wrong-type-arg "write-byte: first arg should be a char: ~A" c))
					(#_write-byte c (->value obj)))
		 
		 'read-string         (lambda (k obj) 
					(if (mock-port? k)
					    (error 'wrong-type-arg "read-string: first arg should be an integer: ~A" c))
					(#_read-string k (->value obj)))

		 'class-name          '*mock-port*
		 )))
    
    (define (mock-port port)
      (if (and (or (input-port? port)
		   (output-port? port))
	       (not (let? port)))
	  (openlet
	   (sublet (*mock-port* 'mock-port-class)
	     'value port
	     'object->string mock->string))
	  (error 'wrong-type-arg "mock-port ~S is not a port" port)))
    
    (set! mock-port? (lambda (obj)
		       (and (openlet? obj)
			    (outlet-member obj mock-port-class))))
    
    (set! no-mock-ports (lambda (lst)
			  (for-each
			   (lambda (c)
			     (if (mock-port? c)
				 (error 'wrong-type-arg "stray mock-port?")))
			   lst)
			  lst))

    (set! ->value (lambda (obj)
      (if (mock-port? obj)
	  (obj 'value)
	  obj)))

    (curlet))))

;;; sublet of any of these needs to include the value field or a let-ref-fallback
#|
(require libc.scm)

(define *input-file*
  (let ((file-write-date (lambda (file)
			   (with-let (sublet *libc* :file file)
			     (let ((buf (stat.make)))
			       (stat file buf)
			       (let ((date (stat.st_mtime buf)))
				 (free buf)
				 date)))))
	(file-size (lambda (file)
		     (with-let (sublet *libc* :file file)
		       (let ((buf (stat.make)))
			 (stat file buf)
			 (let ((size (stat.st_size buf)))
			   (free buf)
			   size)))))
	(file-owner (lambda (file)
		      (with-let (sublet *libc* :file file)
			(let ((buf (stat.make)))
			  (stat file buf)
			  (let ((uid (stat.st_uid buf)))
			    (free buf)
			    (let ((pwd (getpwuid uid)))
			      (passwd.pw_name pwd))))))))
    (openlet
     (sublet (*mock-port* 'mock-port-class)
       'value      #f
       'length     (lambda (obj) (file-size (obj 'file-name)))
       'owner      (lambda (obj) (file-owner (obj 'file-name)))
       'write-date (lambda (obj) (file-write-date (obj 'file-name)))))))

(define (open-a-file file)
  (let ((p (openlet
	    (sublet *input-file* 
	      'file-name file))))
    (set! (p 'value) (open-input-file "oboe.snd"))
    p))

(define p (open-a-file "oboe.snd"))
(length p) -> 101684
((p 'owner) p) -> "bil"
|#

