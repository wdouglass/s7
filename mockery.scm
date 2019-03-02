(provide 'mockery.scm)


;;; the exported mock data classes
(define *mock-vector* #f)
(define *mock-pair* #f)
(define *mock-string* #f)
(define *mock-hash-table* #f)
(define *mock-symbol* #f)
(define *mock-c-pointer* #f)
(define *mock-random-state* #f)
(define *mock-char* #f)
(define *mock-number* #f)
(define *mock-iterator* #f)
(define *mock-port* #f)


(let () ; rest of file is in this let

  (define (->value obj)
    (if (and (let? obj)
	     (symbol? (obj 'mock-type)))
	(obj 'value)
	obj))

  (define (cover-lets args) ; TODO: replace this
    (let ((opens ()))
      (for-each (lambda (arg)
		  (when (openlet? arg)
		    (coverlet arg)
		    (set! opens (cons arg opens))))
		args)
      opens))

  
  (define (mock? obj)
    (and (let? obj)
	 (symbol? (obj 'mock-type))))

  (define (with-mock-wrapper func)
    (lambda (obj)
      (if (mock? obj)
	  (dynamic-wind coverlets (lambda () (func (obj 'value))) openlets)
	  (if (and (openlet? obj)           ; (with-let (mock-number 3) (abs (openlet (inlet 'abs (lambda (val) 0)))))
		   (not (procedure? obj)))  ; (display (openlet (with-let (mock-number 0) (lambda () 1))))
	      ;; TODO: and c-pointer? c-object?
	      (let ((func-name (string->symbol (object->string func))))
		(if (procedure? (obj func-name))
		    ((obj func-name) obj)
		    (func obj)))
	      (func obj)))))

  (define (with-mock-wrapper* func)
    (lambda args
      (let ((unknown-openlets #f))
	(let ((new-args (map (lambda (arg)
			       (if (mock? arg)
				   (arg 'value)
				   (begin
				     (if (and (openlet? arg)
					      (not (procedure? arg))
					      (not (c-pointer? arg)))
					 (set! unknown-openlets #t))
				     arg)))
			     args)))
	  (if unknown-openlets
	      (apply func new-args)
	      (dynamic-wind coverlets (lambda () (apply func new-args)) openlets))))))

  ;; one tricky thing here is that a mock object can be the let of with-let: (with-let (mock-port ...) ...)
  ;;   so a mock object's method can be called even when no argument is a mock object.  Even trickier, the
  ;;   mock object can be a closure's containing (open)let: (display (openlet (with-let (mock-c-pointer 0) (lambda () 1))))

  ;; TODO: still need display et al for port?
  ;;       and with-mock-wrapper
  ;;       and everything is a mess...
  ;;       and char-ci* / string-ci* 

  ;; --------------------------------------------------------------------------------

  (set! *mock-vector*
	(let ((mock-vector? #f))
	  (let ((mock-vector-class
		 (inlet 'local-set!         (lambda (obj i val)          ; reactive-vector uses this as a hook into vector-set!
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
			
			'let-ref-fallback   (lambda (obj i) 
					      (if (and (integer? i)
						       (defined? 'value obj))
						  (#_vector-ref (obj 'value) i)   ; the implicit case
						  (error 'out-of-range "unknown field: ~S" i)))
			
			'equivalent?        (with-mock-wrapper* #_equivalent?)
			'vector-ref         (with-mock-wrapper* #_vector-ref)
			'vector-length      (with-mock-wrapper #_length)
			'reverse            (with-mock-wrapper #_reverse)
			'sort!              (with-mock-wrapper* #_sort!)
			'make-iterator      (with-mock-wrapper #_make-iterator)
			'arity              (with-mock-wrapper #_arity)
			'object->string     (with-mock-wrapper* #_object->string)
			'format             (with-mock-wrapper* #_format)
			'write              (with-mock-wrapper* #_write)
			'display            (with-mock-wrapper* #_display)
			'vector-dimensions  (with-mock-wrapper #_vector-dimensions)
			'fill!              (with-mock-wrapper* #_fill!)
			'vector-fill!       (with-mock-wrapper* #_vector-fill!)
			'vector->list       (with-mock-wrapper* #_vector->list)
			'subvector          (with-mock-wrapper* #_subvector)
			'copy               (with-mock-wrapper* #_copy)
			'vector?            (with-mock-wrapper #_vector?)
			'int-vector?        (with-mock-wrapper #_int-vector?)
			'byte-vector?       (with-mock-wrapper #_byte-vector?)
			'float-vector?      (with-mock-wrapper #_float-vector?)
			'length             (with-mock-wrapper #_length)
			'vector-append      (with-mock-wrapper* #_vector-append)
			'append             (with-mock-wrapper* #_append)
			'class-name         '*mock-vector*)))
	    
	    (define (make-mock-vector len . rest)
	      (openlet 
	       (sublet mock-vector-class 
		 'value (apply #_make-vector len rest)
		 'mock-type 'mock-vector?)))
	    
	    (define (mock-vector . args)
	      (openlet
	       (sublet mock-vector-class 
		 'value (apply #_vector args)
		 'mock-type 'mock-vector?)))
	    
	    (set! mock-vector? (lambda (obj) 
				 (and (let? obj)
				      (defined? 'mock-type obj #t)
				      (eq? (obj 'mock-type) 'mock-vector?))))
	    
	    (curlet))))


#|
  ;; vector that grows to accommodate vector-set!
  (define (stretchable-vector)
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
	 'mock-type 'mock-vector?
	 'initial-element #f
	 'vector-ref local-ref
	 'let-ref-fallback local-ref
	 'vector-set! local-set!
	 'let-set-fallback local-set!))))
|#


  ;; --------------------------------------------------------------------------------
  
  (set! *mock-hash-table*
	(let ((mock-hash-table? #f))
	  (let ((mock-hash-table-class
		 (inlet 'let-ref-fallback   (lambda (obj key)
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

			'hash-table-ref     (with-mock-wrapper* #_hash-table-ref)
			'hash-table-set!    (with-mock-wrapper* #_hash-table-set!)
			'equivalent?        (with-mock-wrapper* #_equivalent?)
			'hash-table-entries (with-mock-wrapper #_hash-table-entries)
			'make-iterator      (with-mock-wrapper #_make-iterator)
			'fill!              (with-mock-wrapper* #_fill!)
			'object->string     (with-mock-wrapper* #_object->string)
			'format             (with-mock-wrapper* #_format)
			'write              (with-mock-wrapper* #_write)
			'display            (with-mock-wrapper* #_display)
			'reverse            (with-mock-wrapper #_reverse)
			'arity              (with-mock-wrapper #_arity)
			'copy               (with-mock-wrapper* #_copy)
			'hash-table?        (with-mock-wrapper #_hash-table?)
			'length             (with-mock-wrapper #_length)
			'append             (with-mock-wrapper* #_append)
			'class-name         '*mock-hash-table*)))
	    
	    (define (make-mock-hash-table . rest)
	      (openlet 
	       (sublet mock-hash-table-class 
		 'value (apply #_make-hash-table rest)
		 'mock-type 'mock-hash-table?)))
	    
	    (define (mock-hash-table . args)
	      (openlet 
	       (sublet mock-hash-table-class 
		 'value (apply #_hash-table args)
		 'mock-type 'mock-hash-table?)))
	    
	    (set! mock-hash-table? (lambda (obj)
				     (and (let? obj)
					  (defined? 'mock-type obj #t)
					  (eq? (obj 'mock-type) 'mock-hash-table?))))
	    
	    (curlet))))
  
  
#|
  ;; hash-table that returns a special identifier when key is not in the table
  
  (define (gloomy-hash-table)
    (openlet
     (sublet (*mock-hash-table* 'mock-hash-table-class) ; ideally this would be a separate (not copied) gloomy-hash-table-class 
       'value #f
       'mock-type 'mock-hash-table?
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
    (let ((ht (gloomy-hash-table)))
      (set! (ht 'value) (make-hash-table len))
      (set! (ht 'not-a-key) not-a-key)
      ht))
|#
  
  
  ;; --------------------------------------------------------------------------------
  
  (set! *mock-string*
	(let ((mock-string? #f))
	  (let ((mock-string-class
		 (inlet 'equivalent?            (with-mock-wrapper* #_equivalent?)
			'reverse                (with-mock-wrapper #_reverse)
			'arity                  (with-mock-wrapper #_arity)
			'make-iterator          (with-mock-wrapper* #_make-iterator)

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
			
			'string-length          (with-mock-wrapper #_length)
			'string-append          (with-mock-wrapper* #_string-append)
			'string-copy            (with-mock-wrapper #_copy)
			
			'string=?               (with-mock-wrapper* #_string=?)
			'string<?               (with-mock-wrapper* #_string<?)
			'string>?               (with-mock-wrapper* #_string>?)
			'string<=?              (with-mock-wrapper* #_string<=?)
			'string>=?              (with-mock-wrapper* #_string>=?)
			
			'string-downcase        (with-mock-wrapper #_string-downcase)
			'string-upcase          (with-mock-wrapper #_string-upcase)
			'string->symbol         (with-mock-wrapper #_string->symbol)
			'symbol                 (with-mock-wrapper #_symbol)
			'string->keyword        (with-mock-wrapper #_string->keyword)
			'open-input-string      (with-mock-wrapper #_open-input-string)
			'directory?             (with-mock-wrapper #_directory?)
			'file-exists?           (with-mock-wrapper #_file-exists?)
			'getenv                 (with-mock-wrapper #_getenv)
			'delete-file            (with-mock-wrapper #_delete-file)
			'string->byte-vector    (with-mock-wrapper #_string->byte-vector)
			'object->string         (with-mock-wrapper* #_object->string)
			'format                 (with-mock-wrapper* #_format)
			'write                  (with-mock-wrapper* #_write)
			'display                (with-mock-wrapper* #_display)
			'char-position          (with-mock-wrapper* #_char-position)
			'string-fill!           (with-mock-wrapper* #_string-fill!)
			'gensym                 (with-mock-wrapper* #_gensym)
			'call-with-input-string (with-mock-wrapper* #_call-with-input-string)
			'with-input-from-string (with-mock-wrapper* #_with-input-from-string)
			'system                 (with-mock-wrapper* #_system)
			'load                   (with-mock-wrapper* #_load)
			'eval-string            (with-mock-wrapper* #_eval-string)
			'string->list           (with-mock-wrapper* #_string->list)
			'bignum                 (with-mock-wrapper #_bignum)
			'fill!                  (with-mock-wrapper* #_fill!)
			'write-string           (with-mock-wrapper* #_write-string)
			'copy                   (with-mock-wrapper* #_copy)
			'substring              (with-mock-wrapper* #_substring)
			'string->number         (with-mock-wrapper* #_string->number)
			'string-position        (with-mock-wrapper* #_string-position)
			'string-ref             (with-mock-wrapper* #_string-ref)
			'string-set!            (with-mock-wrapper* #_string-set!)
			'string-ci=?            (with-mock-wrapper* #_string-ci=?)
			'string-ci<?            (with-mock-wrapper* #_string-ci<?)
			'string-ci>?            (with-mock-wrapper* #_string-ci>?)
			'string-ci<=?           (with-mock-wrapper* #_string-ci<=?)
			'string-ci>=?           (with-mock-wrapper* #_string-ci>=?)
			'string?                (with-mock-wrapper #_string?)
			'length                 (with-mock-wrapper #_string-length)
			'append                 (with-mock-wrapper* #_append)
			'class-name             '*mock-string*)))
	    
	    (define* (make-mock-string len (init #\null))
	      (openlet 
	       (sublet mock-string-class 
		 'value (#_make-string len init)
		 'mock-type 'mock-string?)))
	    
	    (define (mock-string . args)
	      (let ((v (make-mock-string 0)))
		(set! (v 'value) 
		      (if (string? (car args))
			  (car args)
			  (apply #_string args)))
		v))
	    
	    (set! mock-string? (lambda (obj)
				 (and (let? obj)
				      (defined? 'mock-type obj #t)
				      (eq? (obj 'mock-type) 'mock-string?))))
	    
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
  

  ;; --------------------------------------------------------------------------------
  
  (set! *mock-char*
	(let ((mock-char? #f))
	  (let ((mock-char-class
		 (inlet 'equivalent?        (with-mock-wrapper* #_equivalent?)
			'char-upcase        (with-mock-wrapper #_char-upcase)
			'char-downcase      (with-mock-wrapper #_char-downcase)
			'char->integer      (with-mock-wrapper #_char->integer)
			'char-upper-case?   (with-mock-wrapper #_char-upper-case?)
			'char-lower-case?   (with-mock-wrapper #_char-lower-case?)
			'char-alphabetic?   (with-mock-wrapper #_char-alphabetic?)
			'char-numeric?      (with-mock-wrapper #_char-numeric?)
			'char-whitespace?   (with-mock-wrapper #_char-whitespace?)
			'char=?             (with-mock-wrapper* #_char=?)
			'char<?             (with-mock-wrapper* #_char<?)
			'char>?             (with-mock-wrapper* #_char>?)
			'char<=?            (with-mock-wrapper* #_char<=?)
			'char>=?            (with-mock-wrapper* #_char>=?)
			'char-ci=?          (with-mock-wrapper* #_char-ci=?)
			'char-ci<?          (with-mock-wrapper* #_char-ci<?)
			'char-ci>?          (with-mock-wrapper* #_char-ci>?)
			'char-ci<=?         (with-mock-wrapper* #_char-ci<=?)
			'char-ci>=?         (with-mock-wrapper* #_char-ci>=?)
			'string             (with-mock-wrapper* #_string)
			'string-fill!       (with-mock-wrapper* #_string-fill!)
			'object->string     (with-mock-wrapper* #_object->string)
			'format             (with-mock-wrapper* #_format)
			'write              (with-mock-wrapper* #_write)
			'display            (with-mock-wrapper* #_display)
			'arity              (with-mock-wrapper #_arity)
			'make-string        (with-mock-wrapper* #_make-string)
			'char-position      (with-mock-wrapper* #_char-position)
			'write-char         (with-mock-wrapper* #_write-char)
			'string-set!        (with-mock-wrapper* #_string-set!)
			'copy               (with-mock-wrapper* #_copy)
			'char?              (with-mock-wrapper #_char?)
			'class-name         '*mock-char*
			'length             (lambda (obj) #f))))
	    
	    (define (mock-char c) 
	      (if (and (char? c)
		       (not (let? c)))
		  (immutable!
		   (openlet
		    (sublet (*mock-char* 'mock-char-class)
		      'value c
		      'mock-type 'mock-char?)))
		  (error 'wrong-type-arg "mock-char arg ~S is not a char" c)))
	    
	    (set! mock-char? (lambda (obj)
			       (and (let? obj)
				    (defined? 'mock-type obj #t)
				    (eq? (obj 'mock-type) 'mock-char?))))
	    
	    (curlet))))
  
  ;; eventually I'll conjure up unichars like (define lambda (byte-vector #xce #xbb)) via mock-char,
  ;;   then combine those into unistring via mock-string
  ;;
  ;; (string-length obj)->g_utf8_strlen etc 
  ;;   (g_unichar_isalpha (g_utf8_get_char (byte-vector #xce #xbb))) -> #t
  ;;   (g_utf8_strlen (byte-vector #xce #xbb #xce #xba) 10) -> 2
  ;;   (g_utf8_normalize (byte-vector #xce #xbb #xce #xba) 4 G_NORMALIZE_DEFAULT)
  ;;   but the ones that return gunichar (toupper) currently don't return a byte-vector or a string
  ;;   maybe gunichar->byte-vector?
  ;;   need glib.scm, or unicode.scm to load the stuff



  ;; --------------------------------------------------------------------------------
  
  (set! *mock-number*
	(let ((mock-number? #f))
	  
	  (let ((mock-number-class
		 (inlet 
		  'equivalent?      (with-mock-wrapper* #_equivalent?)
		  'arity            (with-mock-wrapper #_arity)
		  'real-part        (with-mock-wrapper #_real-part)
		  'imag-part        (with-mock-wrapper #_imag-part)
		  'numerator        (with-mock-wrapper #_numerator)
		  'denominator      (with-mock-wrapper #_denominator)
		  'even?            (with-mock-wrapper #_even?)
		  'odd?             (with-mock-wrapper #_odd?)
		  'zero?            (with-mock-wrapper #_zero?)
		  'positive?        (with-mock-wrapper #_positive?)
		  'negative?        (with-mock-wrapper #_negative?)
		  'infinite?        (with-mock-wrapper #_infinite?)
		  'nan?             (with-mock-wrapper #_nan?)
		  'append           (with-mock-wrapper* #_append)
		  'magnitude        (with-mock-wrapper #_magnitude)
		  'angle            (with-mock-wrapper #_angle)
		  'rationalize      (with-mock-wrapper* #_rationalize)
		  'abs              (with-mock-wrapper #_abs)
		  'exp              (with-mock-wrapper #_exp)
		  'log              (with-mock-wrapper* #_log)
		  'sin              (with-mock-wrapper #_sin)
		  'cos              (with-mock-wrapper #_cos)
		  'tan              (with-mock-wrapper #_tan)
		  'asin             (with-mock-wrapper #_asin)
		  'acos             (with-mock-wrapper #_acos)
		  'atan             (with-mock-wrapper* #_atan)
		  'sinh             (with-mock-wrapper #_sinh)
		  'cosh             (with-mock-wrapper #_cosh)
		  'tanh             (with-mock-wrapper #_tanh)
		  'asinh            (with-mock-wrapper #_asinh)
		  'acosh            (with-mock-wrapper #_acosh)
		  'atanh            (with-mock-wrapper #_atanh)
		  'sqrt             (with-mock-wrapper #_sqrt)
		  'expt             (with-mock-wrapper* #_expt)
		  'floor            (with-mock-wrapper #_floor)
		  'ceiling          (with-mock-wrapper #_ceiling)
		  'truncate         (with-mock-wrapper #_truncate)
		  'round            (with-mock-wrapper #_round)
		  'integer->char    (with-mock-wrapper #_integer->char)
		  'inexact->exact   (with-mock-wrapper #_inexact->exact)
		  'exact->inexact   (with-mock-wrapper #_exact->inexact)
		  'integer-length   (with-mock-wrapper #_integer-length)
		  'integer-decode-float (with-mock-wrapper #_integer-decode-float)
		  'number?          (with-mock-wrapper #_number?)
		  'integer?         (with-mock-wrapper #_integer?)
		  'real?            (with-mock-wrapper #_real?)
		  'complex?         (with-mock-wrapper #_complex?)
		  'rational?        (with-mock-wrapper #_rational?)
		  'exact?           (with-mock-wrapper #_exact?)
		  'inexact?         (with-mock-wrapper #_inexact?)
		  'lognot           (with-mock-wrapper #_lognot)
		  'logior           (with-mock-wrapper* #_logior)
		  'logxor           (with-mock-wrapper* #_logxor)
		  'logand           (with-mock-wrapper* #_logand)
		  'number->string   (with-mock-wrapper* #_number->string)
		  'lcm              (with-mock-wrapper* #_lcm)
		  'gcd              (with-mock-wrapper* #_gcd)
		  '+                (with-mock-wrapper* #_+)
		  '-                (with-mock-wrapper* #_-)
		  '*                (with-mock-wrapper* #_*)
		  '/                (with-mock-wrapper* #_/)
		  'max              (with-mock-wrapper* #_max)
		  'min              (with-mock-wrapper* #_min)
		  '=                (with-mock-wrapper* #_=)
		  '<                (with-mock-wrapper* #_<)
		  '>                (with-mock-wrapper* #_>)
		  '<=               (with-mock-wrapper* #_<=)
		  '>=               (with-mock-wrapper* #_>=)

		  'make-polar       (if (provided? 'pure-s7)
					(lambda (mag ang) (#_complex (* mag (cos ang)) (* mag (sin ang))))
					(lambda args (apply #_make-polar (map ->value args))))
		  'make-rectangular (with-mock-wrapper* #_complex)
		  'complex          (with-mock-wrapper* #_complex)
		  'random-state     (with-mock-wrapper* #_random-state)
		  'ash              (with-mock-wrapper* #_ash)
		  'logbit?          (with-mock-wrapper* #_logbit?)
		  'quotient         (with-mock-wrapper* #_quotient)
		  'remainder        (with-mock-wrapper* #_remainder)
		  'modulo           (with-mock-wrapper* #_modulo)
		  'random           (with-mock-wrapper* #_random)
		  'write-byte       (with-mock-wrapper* #_write-byte)
		  'make-list        (with-mock-wrapper* #_make-list)
		  'make-vector      (with-mock-wrapper* #_make-vector)
		  'make-float-vector (with-mock-wrapper* #_make-float-vector)
		  'make-int-vector  (with-mock-wrapper* #_make-int-vector)
		  'make-byte-vector  (with-mock-wrapper* #_make-byte-vector)
		  'make-hash-table  (with-mock-wrapper* #_make-hash-table)
		  'object->string   (with-mock-wrapper* #_object->string)
		  'format           (with-mock-wrapper* #_format)
		  'write            (with-mock-wrapper* #_write)
		  'display          (with-mock-wrapper* #_display)
		  'string-fill!     (with-mock-wrapper* #_string-fill!)
		  'copy             (with-mock-wrapper* #_copy)
		  'vector->list     (with-mock-wrapper* #_vector->list)
		  'string->list     (with-mock-wrapper* #_string->list)
		  'substring        (with-mock-wrapper* #_substring)
		  'vector-fill!     (with-mock-wrapper* #_vector-fill!)
		  'make-string      (with-mock-wrapper* #_make-string)
		  'string-ref       (with-mock-wrapper* #_string-ref)
		  'string-set!      (with-mock-wrapper* #_string-set!)
		  'string->number   (with-mock-wrapper* #_string->number)
		  'list-ref         (with-mock-wrapper* #_list-ref)
		  'list-set!        (with-mock-wrapper* #_list-set!)
		  'list-tail        (with-mock-wrapper* #_list-tail)
		  'vector-ref       (with-mock-wrapper* #_vector-ref)
		  'float-vector-ref (with-mock-wrapper* #_float-vector-ref)
		  'int-vector-ref   (with-mock-wrapper* #_int-vector-ref)
		  'byte-vector-ref  (with-mock-wrapper* #_byte-vector-ref)
		  'vector-set!      (with-mock-wrapper* #_vector-set!)
		  'float-vector-set! (with-mock-wrapper* #_float-vector-set!)
		  'int-vector-set!  (with-mock-wrapper* #_int-vector-set!)
		  'byte-vector-set! (with-mock-wrapper* #_byte-vector-set!)
		  'float-vector     (with-mock-wrapper* #_float-vector)
		  'int-vector       (with-mock-wrapper* #_int-vector)
		  'byte-vector      (with-mock-wrapper* #_byte-vector)
		  'subvector        (with-mock-wrapper* #_subvector)
		  'read-string      (with-mock-wrapper* #_read-string)
		  'length           (with-mock-wrapper #_length)
		  'number?          (with-mock-wrapper #_number?)
		  'class-name       '*mock-number*)))
	    
	    (define (mock-number x)
	      (if (and (number? x)
		       (not (let? x)))
		  (immutable!
		   (openlet
		    (sublet (*mock-number* 'mock-number-class)
		      'value x
		      'mock-type 'mock-number?)))
		  (error 'wrong-type-arg "mock-number ~S is not a number" x)))
	    
	    (set! mock-number? (lambda (obj)
				 (and (let? obj)
				      (defined? 'mock-type obj #t)
				      (eq? (obj 'mock-type) 'mock-number?))))
	    (curlet))))

#|
  ;; fuzzy number
  
  (define fuzzy-number
    (let ((fuzz (lambda (fx)
		  (#_* fx (#_- 1.05 (#_random .1))))))
      (lambda (fx)
	(openlet 
	 (sublet 
	     (*mock-number* 'mock-number-class)
	   'let-ref-fallback (lambda (obj sym) (fuzz fx))
	   'object->string (lambda (obj . args) (#_number->string (fuzz fx))))))))
  
  
  ;; interval arithmetic 
  ;; 
  ;; from Wikipedia:
  ;; x + y =	[a+c, b+d]
  ;; x - y =	[a-d, b-c]
  ;; x × y =	[min(ac, ad, bc, bd), max(ac, ad, bc, bd)]
  ;; x / y =	[min(a/c, a/d, b/c, b/d), max(a/c, a/d, b/c, b/d)]
  
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



  ;; --------------------------------------------------------------------------------
  
  (set! *mock-pair*
	(let ((mock-pair? #f))
	  (let ((mock-pair-class
		 (inlet 'equivalent?      (with-mock-wrapper* #_equivalent?)
			'pair-line-number (with-mock-wrapper #_pair-line-number)
			'list->string     (with-mock-wrapper #_list->string)
			'object->string   (with-mock-wrapper* #_object->string)
			'format           (with-mock-wrapper* #_format)
			'write            (with-mock-wrapper* #_write)
			'display          (with-mock-wrapper* #_display)
			'list?            (with-mock-wrapper #_list?)
			'car              (with-mock-wrapper #_car)
			'cdr              (with-mock-wrapper #_cdr)
			'set-car!         (with-mock-wrapper* #_set-car!)
			'set-cdr!         (with-mock-wrapper* #_set-cdr!)
			'caar             (with-mock-wrapper #_caar)
			'cadr             (with-mock-wrapper #_cadr)
			'cdar             (with-mock-wrapper #_cdar)
			'cddr             (with-mock-wrapper #_cddr)
			'caaar            (with-mock-wrapper #_caaar)
			'caadr            (with-mock-wrapper #_caadr)
			'cadar            (with-mock-wrapper #_cadar)
			'cdaar            (with-mock-wrapper #_cdaar)
			'caddr            (with-mock-wrapper #_caddr)
			'cdddr            (with-mock-wrapper #_cdddr)
			'cdadr            (with-mock-wrapper #_cdadr)
			'cddar            (with-mock-wrapper #_cddar)
			'caaaar           (with-mock-wrapper #_caaaar)
			'caaadr           (with-mock-wrapper #_caaadr)
			'caadar           (with-mock-wrapper #_caadar)
			'cadaar           (with-mock-wrapper #_cadaar)
			'caaddr           (with-mock-wrapper #_caaddr)
			'cadddr           (with-mock-wrapper #_cadddr)
			'cadadr           (with-mock-wrapper #_cadadr)
			'caddar           (with-mock-wrapper #_caddar)
			'cdaaar           (with-mock-wrapper #_cdaaar)
			'cdaadr           (with-mock-wrapper #_cdaadr)
			'cdadar           (with-mock-wrapper #_cdadar)
			'cddaar           (with-mock-wrapper #_cddaar)
			'cdaddr           (with-mock-wrapper #_cdaddr)
			'cddddr           (with-mock-wrapper #_cddddr)
			'cddadr           (with-mock-wrapper #_cddadr)
			'cdddar           (with-mock-wrapper #_cdddar)
			'assoc            (with-mock-wrapper* #_assoc)
			'assq             (with-mock-wrapper* #_assq)
			'assv             (with-mock-wrapper* #_assv)
			'member           (with-mock-wrapper* #_member)
			'memq             (with-mock-wrapper* #_memq)
			'memv             (with-mock-wrapper* #_memv)

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

			'reverse!         (lambda (obj) 
					    (if (mock-pair? obj)
						(set! (obj 'value) (#_reverse (obj 'value)))
						(#_reverse! obj)))
			
			'list-tail        (with-mock-wrapper* #_list-tail)
			'sort!            (with-mock-wrapper* #_sort!)
			'reverse          (with-mock-wrapper #_reverse)
			'arity            (with-mock-wrapper #_arity)
			'make-iterator    (with-mock-wrapper #_make-iterator)
			'eval             (with-mock-wrapper #_eval)
			'list->vector     (with-mock-wrapper #_list->vector)
			'fill!            (with-mock-wrapper* #_fill!)
			'copy             (with-mock-wrapper* #_copy)
			'subvector        (with-mock-wrapper* #_subvector)
			'make-vector      (with-mock-wrapper* #_make-vector)
			'list-ref         (with-mock-wrapper* #_list-ref)
			'list-set!        (with-mock-wrapper* #_list-set!)
			'pair?            (with-mock-wrapper #_pair?)
			'length           (with-mock-wrapper #_length)
			'append           (with-mock-wrapper* #_append)
			'class-name       '*mock-pair*)))
	    
	    (define (mock-pair . args)
	      (openlet
	       (sublet (*mock-pair* 'mock-pair-class)
		 'value (copy args)
		 'mock-type 'mock-pair?)))
	    
	    (set! mock-pair? (lambda (obj)
			       (and (let? obj)
				    (defined? 'mock-type obj #t)
				    (eq? (obj 'mock-type) 'mock-pair?))))
	    
	    (curlet))))

#|  
  (let ((immutable-list-class 
	 (sublet (*mock-pair* 'mock-pair-class)
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
	 'value lst
	 'mock-type 'mock-pair?)))
|#

;; since a mock-pair prints itself as if a list, you can get some strange printout results:
;;    (cons 'a ((*mock-pair* 'mock-pair) 'b 'c)) -> '(a . (b c))


  
  ;; --------------------------------------------------------------------------------
  
  (set! *mock-symbol*
	(let ((mock-symbol? #f))
	  (let ((mock-symbol-class
		 (inlet 'equivalent?           (with-mock-wrapper* #_equivalent?)
			'gensym?               (with-mock-wrapper #_gensym?)
			'append                (with-mock-wrapper* #_append)
			'symbol->string        (with-mock-wrapper #_symbol->string)
			'symbol->value         (with-mock-wrapper* #_symbol->value)
			'symbol->dynamic-value (with-mock-wrapper #_symbol->dynamic-value)
			'setter                (with-mock-wrapper #_setter)
			'provided?             (with-mock-wrapper #_provided?)
			'provide               (with-mock-wrapper #_provide)
			'defined?              (with-mock-wrapper #_defined?)
			'symbol->keyword       (with-mock-wrapper #_symbol->keyword)
			'keyword?              (with-mock-wrapper #_keyword?)
			'keyword->symbol       (with-mock-wrapper #_keyword->symbol)
			'object->string        (with-mock-wrapper* #_object->string)
			'format                (with-mock-wrapper* #_format)
			'write                 (with-mock-wrapper* #_write)
			'display               (with-mock-wrapper* #_display)
			'symbol?               (with-mock-wrapper #_symbol?)
			'class-name            '*mock-symbol*
			)))
	    
	    (define (mock-symbol s)
	      (if (and (symbol? s)
		       (not (let? s)))
		  (immutable!
		   (openlet
		    (sublet (*mock-symbol* 'mock-symbol-class)
		      'value s
		      'mock-type 'mock-symbol?)))
		  (error 'wrong-type-arg "mock-symbol ~S is not a symbol" s)))
	    
	    (set! mock-symbol? (lambda (obj)
				 (and (let? obj)
				      (defined? 'mock-type obj #t)
				      (eq? (obj 'mock-type) 'mock-symbol?))))
	    
	    (curlet))))
  
  
  ;; --------------------------------------------------------------------------------
  
  (set! *mock-c-pointer*
	(let ((mock-c-pointer? #f))
	  (let ((mock-c-pointer-class
		 (inlet 'c-pointer?      (with-mock-wrapper #_c-pointer?)
			'c-pointer-type  (with-mock-wrapper #_c-pointer-type)
			'c-pointer-info  (with-mock-wrapper #_c-pointer-info)
			'c-pointer-weak1 (with-mock-wrapper #_c-pointer-weak1)
			'c-pointer-weak2 (with-mock-wrapper #_c-pointer-weak2)
			'c-pointer->list (with-mock-wrapper #_c-pointer->list)
			'object->string  (with-mock-wrapper* #_object->string)
			'format          (with-mock-wrapper* #_format)
			'write           (with-mock-wrapper* #_write)
			'display         (with-mock-wrapper* #_display)
			)))
	    
	    (define* (mock-c-pointer (int 0) type info weak1 weak2)
	      (immutable!
	       (openlet 
		(sublet (*mock-c-pointer* 'mock-c-pointer-class)
		  'value (#_c-pointer (->value int) (->value type) (->value info) (->value weak1) (->value weak2))
		  'mock-type 'mock-c-pointer?))))
	    
	    (set! mock-c-pointer? 
		  (lambda (obj)
		    (and (let? obj)
			 (defined? 'mock-type obj #t)
			 (eq? (obj 'mock-type) 'mock-c-pointer?))))
	    
	    (curlet))))
  
  
  ;; --------------------------------------------------------------------------------
  
  (set! *mock-random-state*
	(let ((mock-random-state? #f))
	  (let ((mock-random-state-class
		 (inlet 'random-state?      (with-mock-wrapper #_random-state?)
			'random-state->list (with-mock-wrapper #_random-state->list)
			'random             (with-mock-wrapper* #_random)
			'object->string     (with-mock-wrapper* #_object->string)
			'format             (with-mock-wrapper* #_format)
			'write              (with-mock-wrapper* #_write)
			'display            (with-mock-wrapper* #_display)
			)))
	    
	    (define* (mock-random-state seed (carry 1675393560))
	      (immutable!
	       (openlet 
		(sublet (*mock-random-state* 'mock-random-state-class)
		  'value (#_random-state seed carry)
		  'mock-type 'mock-random-state?))))
	    
	    (set! mock-random-state? 
		  (lambda (obj)
		    (and (let? obj)
			 (defined? 'mock-type obj #t)
			 (eq? (obj 'mock-type) 'mock-random-state?))))
	    
	    (curlet))))
  
  
  ;; --------------------------------------------------------------------------------
  
  (set! *mock-iterator*
	(let ((mock-iterator? #f))
	  (let ((mock-iterator-class
		 (inlet 'iterator?         (with-mock-wrapper #_iterator?)
			'iterate           (with-mock-wrapper #_iterate)
			'iterator-at-end?  (with-mock-wrapper #_iterator-at-end?)
			'iterator-sequence (with-mock-wrapper #_iterator-sequence)
			'object->string    (with-mock-wrapper* #_object->string)
			'format            (with-mock-wrapper* #_format)
			'write             (with-mock-wrapper* #_write)
			'display           (with-mock-wrapper* #_display)
			)))
	    
	    (define (make-mock-iterator . args)
	      (immutable!
	       (openlet 
		(sublet (*mock-iterator* 'mock-iterator-class)
		  'value (apply #_make-iterator args)
		  'mock-type 'mock-iterator?))))
	    
	    (set! mock-iterator? 
		  (lambda (obj)
		    (and (let? obj)
			 (defined? 'mock-type obj #t)
			 (eq? (obj 'mock-type) 'mock-iterator?))))
	    
	    (curlet))))
  
  
  ;; --------------------------------------------------------------------------------
  
  (set! *mock-port*
	(let ((mock-port? #f))
	  (let ((mock-port-class
		 (inlet 'input-port?         (with-mock-wrapper #_input-port?)
			'output-port?        (with-mock-wrapper #_output-port?)
			'port-closed?        (with-mock-wrapper #_port-closed?)
			'equivalent?         (with-mock-wrapper* #_equivalent?)
			'append              (with-mock-wrapper* #_append)
			'set-current-output-port (with-mock-wrapper #_set-current-output-port)
			'set-current-input-port  (with-mock-wrapper #_set-current-input-port)
			'set-current-error-port  (with-mock-wrapper #_set-current-error-port)
			'close-input-port    (with-mock-wrapper #_close-input-port)
			'close-output-port   (with-mock-wrapper #_close-output-port)
			'flush-output-port   (with-mock-wrapper* #_flush-output-port)
			'get-output-string   (with-mock-wrapper* #_get-output-string)
			'newline             (with-mock-wrapper* #_newline)
		        'read-char           (with-mock-wrapper* #_read-char)
                        'peek-char           (with-mock-wrapper* #_peek-char)
                        'read-byte           (with-mock-wrapper* #_read-byte)
                        'read-line           (with-mock-wrapper* #_read-line)
			'read                (with-mock-wrapper* #_read)
			'char-ready?         (with-mock-wrapper* #_char-ready?)
		        'port-line-number    (with-mock-wrapper* #_port-line-number)
			'port-filename       (with-mock-wrapper* #_port-filename)
			'object->string      (with-mock-wrapper* #_object->string)
			'display             (with-mock-wrapper* #_display)
			'write               (with-mock-wrapper* #_write)
			'format              (with-mock-wrapper* #_format)
			'write-char          (with-mock-wrapper* #_write-char)
			'write-string        (with-mock-wrapper* #_write-string)
			'write-byte          (with-mock-wrapper* #_write-byte)
			'read-string         (with-mock-wrapper* #_read-string)
			'class-name          '*mock-port*
			)))
	    
	    (define (mock-port port)
	      (if (and (or (input-port? port)
			   (output-port? port))
		       (not (let? port)))
		  (openlet
		   (sublet (*mock-port* 'mock-port-class)
		     'value port
		     'mock-type 'mock-port?))
		  (error 'wrong-type-arg "mock-port ~S is not a port" port)))
	    
	    (set! mock-port? (lambda (obj)
			       (and (let? obj)
				    (defined? 'mock-type obj #t)
				    (eq? (obj 'mock-type) 'mock-port?))))
	    
	    (curlet))))
  
  ;; sublet of any of these needs to include the value field or a let-ref-fallback

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
	 'mock-type 'mock-port?
	 'length     (lambda (obj) (file-size (obj 'file-name)))
	 'owner      (lambda (obj) (file-owner (obj 'file-name)))
	 'write-date (lambda (obj) (file-write-date (obj 'file-name)))))))
  
  (define (open-a-file file)
    (let ((p (openlet
	      (sublet *input-file* 
		'file-name file))))
      (set! (p 'value) (open-input-file file))
      p))
  
  (define p (open-a-file "oboe.snd"))
  (length p) -> 101684
  ((p 'owner) p) -> "bil"
|#

  ) ; end of outer let
