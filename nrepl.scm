;;; nrepl.scm -- notcurses-based repl
;;; 
;;; work-in-progress!

(set! (*s7* 'history-enabled) #f)

(provide 'nrepl.scm)
(require libc.scm)
(load "notcurses_s7.so" (inlet 'init_func 'notcurses_s7_init))

(define old-debug (*s7* 'debug))
(set! (*s7* 'debug) 0)
(define nrepl-debugging #f)

(autoload 'lint "lint.scm")
(autoload 'pretty-print "write.scm")
(autoload 'pp "write.scm")
(autoload '*libm* "libm.scm")
(autoload '*libgsl* "libgsl.scm")
(autoload 'trace "debug.scm")
(autoload 'untrace "debug.scm")
(autoload 'break "debug.scm")
(autoload 'unbreak "debug.scm")
(autoload 'watch "debug.scm")
(autoload 'unwatch "debug.scm")
(autoload 'ow! "stuff.scm")

(unless (defined? '*nrepl*)
  (define *nrepl*
    (let* ((ncd #f)
	   (nc #f)
	   (nc-cols 0)
	   (nc-rows 0)
	   (keymap (make-hash-table 512))
	   (top-level-let 
	    (sublet (rootlet) ; environment in which evaluation takes place

	      :history #f        ; set below
	      :nc-let #f
	      :display-status #f
	      :key-functions (make-hash-table)

	      :exit (let ((+documentation+ "(exit) stops notcurses and then calls #_exit"))
		      (let-temporarily (((*s7* 'debug) 0))
			(lambda ()
			  (notcurses_stop (*nrepl* 'nc))
			  (#_exit))))
	      
	      :time (macro (expr) 
		      `(let ((start (*s7* 'cpu-time)))
			 ,expr 
			 (- (*s7* 'cpu-time) start)))
	      
	      :apropos
	      (let ((levenshtein 
		     (lambda (s1 s2)
		       (let ((L1 (length s1))
			     (L2 (length s2)))
			 (cond ((zero? L1) L2)
			       ((zero? L2) L1)
			       (else (let ((distance (make-vector (list (+ L2 1) (+ L1 1)) 0)))
				       (do ((i 0 (+ i 1)))
					   ((> i L1))
					 (set! (distance 0 i) i))
				       (do ((i 0 (+ i 1)))
					   ((> i L2))
					 (set! (distance i 0) i))
				       (do ((i 1 (+ i 1)))
					   ((> i L2))
					 (do ((j 1 (+ j 1)))
					     ((> j L1))
					   (let ((c1 (+ (distance i (- j 1)) 1))
						 (c2 (+ (distance (- i 1) j) 1))
						 (c3 (if (char=? (s2 (- i 1)) (s1 (- j 1)))
							 (distance (- i 1) (- j 1))
							 (+ (distance (- i 1) (- j 1)) 1))))
					     (set! (distance i j) (min c1 c2 c3)))))
				       (distance L2 L1)))))))
		    
		    (make-full-let-iterator             ; walk the entire let chain
		     (lambda* (lt (stop (rootlet))) 
		       (if (eq? stop lt)
			   (make-iterator lt)
			   (letrec ((iterloop 
				     (let ((iter (make-iterator lt))
					   (+iterator+ #t))
				       (lambda ()
					 (let ((result (iter)))
					   (if (and (eof-object? result)
						    (iterator-at-end? iter)
						    (not (eq? stop (iterator-sequence iter))))
					       (begin 
						 (set! iter (make-iterator (outlet (iterator-sequence iter))))
						 (iterloop))
					       result))))))
			     (make-iterator iterloop))))))
		
		(lambda* (name (e (*nrepl* 'top-level-let)))
		  (let ((ap-name (if (string? name) name 
				     (if (symbol? name) 
					 (symbol->string name)
					 (error 'wrong-type-arg "apropos argument 1 should be a string or a symbol"))))
			(ap-env (if (let? e) e 
				    (error 'wrong-type-arg "apropos argument 2 should be an environment"))))
		    (let ((strs ())
			  (min2 (floor (log (length ap-name) 2)))
			  (have-orange (string=? ((*libc* 'getenv) "TERM") "xterm-256color")))
		      (for-each
		       (lambda (binding)
			 (if (pair? binding)
			     (let ((symbol-name (symbol->string (car binding))))
			       (if (string-position ap-name symbol-name)
				   (set! strs (cons (cons binding 0) strs))
				   (let ((distance (levenshtein ap-name symbol-name)))
				     (if (< distance min2)
					 (set! strs (cons (cons binding distance) strs))))))))
		       (make-full-let-iterator ap-env))
		      
		      (if (not (pair? strs))
			  'no-match
			  (let ((data "")
				(name-len (length name)))
			    (for-each (lambda (b)
					(set! data (string-append data 
								  (if (> (length data) 0) (string #\newline) "")
								  (if (procedure? (cdar b))
								      (let ((doc (documentation (cdar b)))) ; returns "" if no doc
									(if (positive? (length doc))
									    doc
									    (object->string (caar b))))
								      (object->string (caar b))))))
				      (sort! strs (lambda (a b)
						    (or (< (cdr a) (cdr b))
							(and (= (cdr a) (cdr b))
							     (< (abs (- (length (symbol->string (caar a))) name-len))
								(abs (- (length (symbol->string (caar b))) name-len))))))))
			    data))))))
	      )))
      
      ;; to call notcurses functions in the repl, use *nrepl*:  (notcurses_refresh (*nrepl* 'nc))
      
      ;; -------- completion --------
      (define (symbol-completion text)
	(let ((st (symbol-table))
	      (text-len (length text))
	      (match #f))
	  (call-with-exit
	   (lambda (return)
	     (for-each
	      (lambda (par)
		(let* ((sym (symbol->string par))
		       (sym-len (length sym)))
		  (when (and (>= sym-len text-len)
			     (string=? text (substring sym 0 text-len)))
		    (if match
			;; more than one match, save the longest text that all syms match
			(do ((min-len (min (string-length match) sym-len))
			     (i text-len (+ i 1)))
			    ((or (= i min-len)
				 (not (char=? (match i) (sym i))))
			     (if (= min-len text-len)
				 (return text)
				 (set! match (substring match 0 i)))))
			(set! match sym)))))
	      st)
	     ;(or match text)
	     match
	     ))))
      
      (define (filename-completion text)
	(and (> (length text) 0)
	     (with-let (sublet *libc* :text text)
	       (let ((g (glob.make)))
		 (glob (string-append text "*")
		       (logior (if (and (defined? 'GLOB_TILDE)
					(char=? (text 0) #\~))
				   GLOB_TILDE
				   0)
			       GLOB_MARK)
		       g)
		 (let ((files (map (lambda (f) ; get rid of emacs' *~ files
				     (if (and (> (length f) 1)
					      (char=? #\~ (f (- (length f) 1))))
					 (values)
					 f))
				   (glob.gl_pathv g))))
		   (globfree g) 
		   (if (or (null? files)
			   (not (null? (cdr files))))
		       #f ;text
		       (car files)))))))

      (define (move-cursor ncd y x)   ; this was (format *stderr* "~C[~D;~DH" #\escape y x) in repl.scm (and it works here)
	(ncdirect_cursor_move_yx ncd y x))
      
      ;; -------- run --------
      (define* (run orig-col orig-row)     ; TODO: need prompt arg
	(let ((ncp-nc-col (or orig-col 0)) ; ncp top-left in nc (possibly started offset from nc 0,0)
	      (ncp-nc-row (or orig-row 0))
	      (ncp-col 0)                  ; top-left in ncp (possibly scrolled from original 0,0)
	      (ncp-row 0)
	      (ncp-cols (max 100 nc-cols))
	      (ncp-rows (max 100 nc-rows))
	      (ncp-max-row 0)
	      (col 0)                      ; ncplane-relative row/column
	      (row 0)
	      (prompt ">")
	      (prompt-len 2)
	      (unbound-case #f)
	      (prev-pars #f)
	      (old-history (top-level-let 'history))) ; see below, old restored upon exit from this ncplane
	  (set! (setter 'col) integer?)
	  (set! (setter 'row) integer?)
	  
	  (let ((ncp (ncplane_new nc ncp-rows ncp-cols ncp-nc-row ncp-nc-col (c-pointer 0)))
		(eols (make-int-vector ncp-rows 0))
		(bols (make-int-vector ncp-rows 0)))

	    ;; -------- scrolling and resizing --------
	    
	    (define (set-row val)
	      (set! row val)
	      (when (>= row ncp-rows)
		(ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 (+ row 40) cols)
		(set! ncp-rows (+ row 40)))
	      (cond ((>= row (+ ncp-row nc-rows)) ; current row is outside (below) the terminal window
		     (set! ncp-row (- nc-rows row 2))
		     (ncplane_move_yx ncp ncp-row ncp-col))
		    ((<= row ncp-row)             ; current row is outside (above) the terminal window
		     (set! ncp-row (max 0 (- row 2)))
		     (ncplane_move_yx ncp ncp-row ncp-col))))

	    (define (increment-row incr)
	      (set! row (+ row incr))
	      (when (>= row ncp-rows)
		(ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 (+ row 40) cols)
		(set! ncp-rows (+ row 40)))
	      (when (>= row (+ ncp-row nc-rows))
		(set! ncp-row (- nc-rows row 2))
		(ncplane_move_yx ncp ncp-row ncp-col)))

	    (define (decrement-row incr)
	      (set! row (- row incr))
	      (when (<= row ncp-row) ; current row is outside (above) the terminal window
		(set! ncp-row (max 0 (- row 2)))
		(ncplane_move_yx ncp ncp-row ncp-col)))
	      

	    (define (set-col val)
	      (set! col val)
	      (when (>= col ncp-cols)
		(ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 ncp-rows (+ col 10))
		(set! ncp-cols (+ col 10)))
	      (cond ((>= col (+ ncp-col nc-cols))  ; current column is outside (to the right of) the terminal window
		     (set! ncp-col (- nc-cols col 2))
		     (ncplane_move_yx ncp ncp-row ncp-col))
		    ((<= col ncp-col) ; current column is outside (to the left of) the terminal window
		     (set! ncp-col (max 0 (- col 2)))
		     (ncplane_move_yx ncp ncp-row ncp-col))))
	      
	    (define (increment-col incr)
	      (set! col (+ col incr))
	      (when (>= col ncp-cols)
		(ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 ncp-rows (+ col 10))
		(set! ncp-cols (+ col 10)))
	      (when (>= col (+ ncp-col nc-cols)) 
		(set! ncp-col (- nc-cols col 2))
		(ncplane_move_yx ncp ncp-row ncp-col)))

	    (define (decrement-col incr)
	      (set! col (- col incr))
	      (when (<= col ncp-col) ; current column is outside (to the left of) the terminal window
		(set! ncp-col (max 0 (- col 2)))
		(ncplane_move_yx ncp ncp-row ncp-col)))

	    ;; TODO: all row/col changes go through the funcs above
	    ;; TODO: c-l == move window as in emacs: move row to mid-view, then top, then bottom, then back to mid etc
	    ;; cursor: y->ncp-row + y, x->ncp-col+x, but it uses row/col which ought to be ok

	    (define (nc-multiline-display ncp r c str)
	      (let ((len (length str)))
		(if (char-position #\newline str)
		    (do ((start 0)
			 (i 0 (+ i 1)))
			((= i len)
			 (nc-display row 0 (substring str start len))
			 (set! (eols row) (- len start)))
		      (if (char=? #\newline (str i))
			  (begin
			    (nc-display row 0 (substring str start i))
			    (set! (eols row) (- i start))
			    (increment-row 1)
			    (set! ncp-max-row (max ncp-max-row row))
			    (set! start (+ i 1)))))
		    (nc-display row 0 str))
		(set! (eols row) (length str))))


	    ;; -------- top-level-let --------
	    (set! (top-level-let 'nc-let) (curlet))

	    (set! (top-level-let 'history)
		  (lambda (filename)
		    (call-with-output-file filename
		      (lambda (p)
			(let ((timestamp (with-let (sublet *libc*)
					   (let ((timestr (make-string 128))) 
					     (let ((len (strftime timestr 128 "%a %d-%b-%Y %H:%M:%S %Z"
								  (localtime 
								   (time.make (time (c-pointer 0 'time_t*)))))))
					       (substring timestr 0 len))))))
			  (format p ";;; nrepl: ~A~%~%" timestamp))
			(do ((i 0 (+ i 1)))
			    ((= i ncp-max-row))
			  (if (> (bols i) 0)
			      (format p "~A ~A~%" (ncplane_contents ncp i 0 1 (bols i)) (ncplane_contents ncp i (bols i) 1 (eols i)))
			      (format p "~A~%" (ncplane_contents ncp i 0 1 (eols i)))))))))

	    (set! (top-level-let 'display-status)
		  (lambda (str)
		    (ncplane_putstr_yx ncp (- nc-rows 2) 0 str)
		    (notcurses_render nc)))

	    
	    ;; --------display --------
	    (define (clear-line row)
	      (ncplane_putstr_yx ncp row (bols row) (make-string (- (eols row) (bols row)) #\space)))

	    (define (nc-display r c str)
	      (let ((len (length str)))
		(when (>= (+ c len) ncp-cols)
		  (ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 ncp-rows (+ c len 10))
		  (set! ncp-rows (+ c len 10))))
	      (ncplane_putstr_yx ncp r c str))
      
	    (define (nc-debug-display r c str)
	      (when nrepl-debugging
		(clear-line r)
		(nc-display r c str)
		(notcurses_render nc)))

	    (define (reprompt y)
	      (ncplane_cursor_move_yx ncp y 0)
	      (ncplane_putstr_yx ncp y 0 prompt)
	      (notcurses_render nc)
	      (move-cursor ncd y prompt-len)
	      (set! (bols row) prompt-len)
	      (set! (eols row) prompt-len)
	      (set! col prompt-len)
	      (set-row y))

	    (define (display-error ncp row type info)
	      (ncplane_putstr_yx ncp row 0 "error:")
	      (set! (eols row) 7)
	      (if (and (pair? info)
		       (string? (car info)))
		  (let ((err (apply format #f info)))
		    (nc-display row 7 err)
		    (set! (eols row) (+ (length err) 7)))
		  (if (not (null? info))
		      (let ((err (format #f "~S" info)))
			(nc-display row 1 err)
			(set! (eols row) (+ (length err) 7)))))
	      row)
	    

	    ;; -------- evaluation ---------
	    (define (badexpr h)            ; *missing-close-paren-hook* function for Enter command
	      (let ((ow (owlet)))
		(if (and (ow 'error-file)
			 (not (equal? (ow 'error-file) "nrepl.scm")))
		    (error 'syntax-error "missing close paren in ~S" (ow 'error-file))
		    (set! (h 'result) 'string-read-error))))
	    
	    (define (shell? h)             ; *unbound-variable-hook* function, also for Enter
	      (nc-debug-display 9 0 (format #f "in shell? ~S ~S ~S ~S ~S" row col (bols (- row 1)) (eols (- row 1)) ((rootlet) 'system)))

	      ;; examine cur-line -- only call system if the unbound variable matches the first non-whitespace chars
	      ;;   of cur-line, and command -v name returns 0 (indicating the shell thinks it is an executable command)
	      (let ((cur-line (ncplane_contents ncp (- row 1) (bols (- row 1)) 1 col)))
		;; at this point (eols row) has not been set, so use col?
		(nc-debug-display 11 0 cur-line)
		(do ((i 0 (+ i 1)))
		    ((or (= i (length cur-line))
			 (not (char-whitespace? (cur-line i))))
		     (let ((var-name (symbol->string (h 'variable))))
		       (when (and (>= (- (length cur-line) i) (length var-name)) ; var-name might be unrelated to cur-line
				  (string=? var-name (substring cur-line i (+ i (length var-name))))
				  (zero? (system (string-append "command -v " var-name " >/dev/null"))))
			 (set! unbound-case #t)
			 (if (procedure? ((rootlet) 'system))
			     (set! (h 'result) (((rootlet) 'system) cur-line #t))
			     (set! (h 'result) #f))))))))
	    
	    (define new-eval 
	      (let ((+documentation+ "this is the repl's eval replacement; its default is to use the repl's top-level-let.")
		    (+signature+ '(values #t let?)))
		(lambda (form . rest) ; use lambda (not lambda*) so we can handle forms like :key
		  (let ((e (if (pair? rest) 
			       (car rest)
			       (*nrepl* 'top-level-let))))
		    (let-temporarily (((hook-functions *unbound-variable-hook*) (list shell?)) ; so pwd et al will work
				      ((*s7* 'history-enabled) #t))
		      (eval form e))))))


	    (define (current-expression ncp row)
	      (if (> (bols row) 0)
		  (ncplane_contents ncp row (bols row) 1 (- (eols row) (bols row)))
		  (do ((i (- row 1) (- i 1)))
		      ((not (zero? (bols i)))
		       (let ((expr (ncplane_contents ncp i (bols i) 1 (- (eols i) (bols i)))))
			 (nc-debug-display 11 0 (format #f "expr: ~S ~S" expr (eols i)))
			 (do ((nrow (+ i 1) (+ nrow 1)))
			     ((> nrow row)
			      expr)
			   (set! expr (append expr " "
					      (ncplane_contents ncp nrow (bols nrow) 1 (- (eols nrow) (bols nrow)))))))))))


	    ;; -------- match close paren --------
	    (define (match-close-paren ncp row col indenting)
	      ;; if row/col is just after #|), get start of current expr, scan until row/col
	      ;;   return either matching row/col or #f if none
	      (do ((r row (- r 1)))
		  ((> (bols r) 0)
		   
		   (do ((cur-row r (+ cur-row 1))
			(oparens ()))
		       ((> cur-row row)
			(and (pair? oparens)
			     (car oparens)))
		     (let* ((cur-line (ncplane_contents ncp cur-row (bols cur-row) 1 (- (eols cur-row) (bols cur-row))))
			    (len (if (and (= cur-row row) 
					  (not indenting))
				     (min (- col (bols row) 1) (length cur-line))
				     (length cur-line))))
		       
		       (nc-debug-display 30 0 (format #f "~S ~S ~S, ~S ~S ~S" row cur-row col cur-line (length cur-line) (bols row)))
		       
		       (do ((i 0 (+ i 1)))
			   ((>= i len))
			 (case (cur-line i)
			   ((#\()
			    (set! oparens (cons (cons cur-row (+ i (bols cur-row))) oparens)))
			   
			   ((#\))
			    (if (pair? oparens)
				(set! oparens (cdr oparens))))
			   
			   ((#\;)
			    (set! i (+ len 1)))
			   
			   ((#\")
			    (do ((k (+ i 1) (+ k 1)))
				((or (>= k len)
				     (and (char=? (cur-line k) #\")
					  (not (char=? (cur-line (- k 1)) #\\))))
				 (set! i k))))
			   
			   ((#\#)
			    (if (char=? (cur-line (+ i 1)) #\|)
				(do ((k (+ i 1) (+ k 1)))
				    ((or (>= k len)
					 (and (char=? (cur-line k) #\|)
					      (char=? (cur-line (+ k 1)) #\#)))
				     (set! i (+ k 1)))))))))))))

	    ;; -------- indentation --------
	    ;;
	    ;; find last (, send spaces to match its col + some if or/and/cond/etc [might be trailing on cur-row moving back or forward]

	    (define (indent ncp row col)
	      (if (not (zero? (bols row)))
		  col
		  (let ((pars (match-close-paren ncp (- row 1) (eols (- row 1)) #t)))
		    
		    (nc-debug-display 25 0 (format #f "pars: ~S" pars))
		    (if (not pars)
			(set! (eols row) (bols row))
			(let ((new-col (cdr pars))
			      (new-pos (+ col (cdr pars)))
			      (trailer (ncplane_contents ncp row (bols row) 1 (- (eols row) (bols row)))))
			  (nc-debug-display 29 0 (format #f "trailer: ~A" trailer))
			  (do ((i (- (length trailer) 1) (- i 1)))
			      ((or (< i 0)
				   (not (char-whitespace? (trailer i))))
			       (if (< i (- (length trailer) 1))
				   (set! trailer (substring trailer 0 (+ i 1))))
			       (do ((i 0 (+ i 1)))
				   ((or (= i (length trailer))
					(not (char-whitespace? (trailer i))))
				    (when (> i 0)
				      (set! new-pos (- new-pos i))
				      (set! trailer (substring trailer i)))))))
			  
					;(nc-debug-display 30 0 (format #f "trailer: ~A" trailer))
			  ;; now fixup new-col and new-pos based on what's after the ( we found above
			  
			  (do ((name (ncplane_contents ncp (car pars) (+ (cdr pars) 1) 1 (eols (car pars))))
			       (i 0 (+ i 1)))
			      ((or (= i (length name))
				   (char-whitespace? (name i)))
			       ;; name = (substring name 0 i))
			       (do ((k (+ i 1) (+ k 1)))
				   ((or (>= k (length name))
					(not (char-whitespace? (name k))))
				    (let ((increment (if (< k (length name))
							 (+ i 2)
							 2)))
				      (set! new-col (+ new-col increment))
				      (set! new-pos (+ new-pos increment)))))))
			  
			  ;; might be moving back, so we need to erase the current line
			  (clear-line row)
			  (nc-display row (bols row) (format #f "~A~A" (make-string (- new-col (bols row)) #\space) trailer))
			  (set! (eols row) (+ new-col (length trailer)))
			  (min new-pos (eols row))))))) ; keep cursor in its relative-to-trailer position if possible
	    
	    
	    ;(nc-display (- nc-rows 3) 0 (make-string nc-cols #\_))
	    (reprompt 0)
	    
	    (catch #t
	      (lambda ()
		(let ((ni (ncinput_make))
		      (selection #f)
		      (mouse-col #f)
		      (mouse-row #f)
		      (error-row #f)
		      (repl-done #f))


		  ;; -------- keymap --------
		  (define control-key (ash 1 33)) ; notcurses getc returns 32 bits
		  
		  (define (normal-char c)
		    (nc-debug-display 16 0 (format #f "bol: ~S col: ~S eol: ~S" (bols row) col (eols row)))
		    
		    (let ((trailing (and (> (eols row) col)
					 (ncplane_contents ncp row col 1 (- (eols row) col -1)))))
		      (nc-display row col (string (integer->char c)))
		      (if (and trailing (> (length trailing) 0))
			  (nc-display row (+ col 1) trailing)))
		    
		    (if (char=? (integer->char c) #\space)
			(notcurses_refresh nc))
		    (increment-col 1) ; might be midline
		    (set! (eols row) (+ (eols row) 1)) ; in any case we've added a character
		    
		    (nc-debug-display 17 0 (format #f "~C: bol: ~S col: ~S eol: ~S" (integer->char c) (bols row) col (eols row)))
		    )
		  
		  (do ((i 0 (+ i 1)))
		      ((= i 256))
		    (set! (keymap i) normal-char))
		  
		  (set! (keymap (char->integer #\tab))
			(lambda (c)
			  (if (< col (eols row))
			      (set-col (indent ncp row col))
			      
			      (let ((start (bols row))
				    (end (eols row)))
				(if (= end start)
				    (begin
				      (nc-display row end "    ")
				      (set! (eols row) (+ end 4))
				      (increment-col 4))
				    
				    (let ((cur-line (ncplane_contents ncp row (bols row) 1 (- (eols row) (bols row)))))

				      (if (not (let loop ((i 0) (len (length cur-line)))
						 (and (< i len)
						      (or (not (char-whitespace? (cur-line i)))
							  (loop (+ i 1) len)))))
					  ;; an empty line, tab = try to indent
					  (set-col (indent ncp row col))
					  (begin
				      
				      (nc-debug-display 19 0 (format #f "start: ~S end: ~S col: ~S cur-line: ~S" start end col cur-line))
				      
				      (let ((completion #f)
					    (loc (do ((i (- (length cur-line) 1) (- i 1)))
						     ((or (< i 0)
							  (char-whitespace? (cur-line i))
							  (memv (cur-line i) '(#\( #\' #\" #\))))
						      i))))
					(set! completion (if (< loc 0) ; match whole cur-line
							     (symbol-completion cur-line)
							     ((if (char=? (cur-line loc) #\") filename-completion symbol-completion)
							      (substring cur-line (+ loc 1)))))
					
					(nc-debug-display 20 0 (format #f "completion: ~S loc: ~S len: ~S" completion loc (length completion)))
					
					(if (not completion)
					    (set-col (min (indent ncp row col) (eols row))))
					
					(when (and completion
						   (not (string=? completion cur-line)))
					  
					  (when (>= loc 0)
					    (set! completion (string-append (substring cur-line 0 (+ loc 1)) completion))
					    (if (char=? (cur-line loc) #\")
						(set! completion (string-append completion "\""))))
					  
					  (nc-display row (bols row) completion)
					  (set-col (+ (bols row) (length completion)))
					  (set! (eols row) col)
					  
					  (nc-debug-display 21 0 (make-string 80 #\space)))
					(nc-debug-display 21 0 (format #f "tab end: ~S ~S" col (eols row)))
					))))
				    )))))
		  
		  (set! (keymap (+ control-key (char->integer #\A)))
			(lambda (c)
			  (set-col (bols row))))
			
		  (set! (keymap (+ control-key (char->integer #\B)))
			(lambda (c)
			  (if (and (= col (bols row))
				   (> row 0))
			      (begin
				(decrement-row 1)
				(set-col (eols row)))
			      (set-col (max (bols row) (- col 1))))))
			
		  (set! (keymap (+ control-key (char->integer #\D)))
			(lambda (c)
			  (when (> (eols row) (bols row))
			    (let ((trailing (ncplane_contents ncp row (+ col 1) 1 (- (eols row) col -1))))
			      (nc-display row col trailing)
			      (nc-display row (+ col (length trailing)) " "))
			    (set! (eols row) (- (eols row) 1)))))

		  (set! (keymap (+ control-key (char->integer #\E)))
			(lambda (c)
			  (set-col (eols row))))
			
		  (set! (keymap (+ control-key (char->integer #\F)))
			(lambda (c)
			  (if (and (= col (eols row))
				   (< row ncp-max-row))
			      (begin
				(increment-row 1)
				(set! ncp-max-row (max ncp-max-row row))
				(set-col (bols row)))
			      (set-col (min (eols row) (+ col 1))))))
			
		  (set! (keymap (+ control-key (char->integer #\K)))
			(lambda (c)
			  (set! selection (ncplane_contents ncp row col 1 (- (eols row) col -1)))
			  (nc-display row col (make-string (- (eols row) col) #\space))
			  (set! (eols row) col)))
			
		  (set! (keymap (+ control-key (char->integer #\N)))
			(lambda (c)
			  (increment-row 1)
			  (set! ncp-max-row (max ncp-max-row row))
			  (set-col (min (max col (bols row)) (eols row)))))
			
		  (set! (keymap (+ control-key (char->integer #\O)))
			(lambda (c)
			  (let ((trailer (ncplane_contents ncp row col 1 (- (eols row) col -1))))
			    (nc-display row col (make-string (- (eols row) col) #\space))
			    (set! (eols row) col)
			    (increment-row 1)
			    (nc-display row 0 trailer)
			    (set! (eols row) (length trailer))
			    (decrement-row 1))))

		  (set! (keymap (+ control-key (char->integer #\Q)))
			(lambda (c)
			  (set! repl-done #t)
			  (set! (top-level-let 'history) old-history)))
			
		  (set! (keymap (+ control-key (char->integer #\P)))
			(lambda (c)
			  (set-row (max 0 (- row 1)))
			  (set-col (min (max col (bols row)) (eols row)))))
			
		  (set! (keymap (+ control-key (char->integer #\Y)))
			(lambda (c)
			  (when (string? selection)
			    (let ((trailing (and (> (eols row) col)
						 (ncplane_contents ncp row col 1 (- (eols row) col -1)))))
			      (nc-debug-display 12 0 (format #f "~D: sel: [~S] ~D, trail: [~S]~%" c selection (length selection) trailing))
			      (nc-display row col selection)
			      (if (and trailing 
				       (> (length trailing) 0))
				  (nc-display row (+ col (length selection)) trailing)))
			    (set! (eols row) (+ (eols row) (length selection))))))

		  (set! (keymap NCKEY_LEFT)       ; arrow keys
			(lambda (c)
			  (set-col (max (bols row) (- col 1)))))
		  
		  (set! (keymap NCKEY_RIGHT)
			(lambda (c)
			  (set-col (min (eols row) (+ col 1)))))
		  
		  (set! (keymap NCKEY_UP)
			(lambda (c)
			  (set-row (max 0 (- row 1)))
			  (set-col (min (max col (bols row)) (eols row)))))
		  
		  (set! (keymap NCKEY_DOWN)
			(lambda (c)
			  (increment-row 1)
			  (set! ncp-max-row (max ncp-max-row row))
			  (set-col (min (max col (bols row)) (eols row)))))
		  ;; TODO: see below if multiline -- should we go on past current entry?

		  (set! (keymap NCKEY_HOME)
			(lambda (c)
			  (set! ncp-row 0)
			  (set! ncp-col 0)
			  (ncplane_move_yx ncp 0 0)))
		  ;; TODO: fixup the cursor

		  (set! (keymap NCKEY_PGUP)
			(lambda (c)
			  (set! ncp-row (max 0 (- row nc-rows)))
			  (ncplane_move_yx ncp ncp-row ncp-col)))

		  (set! (keymap NCKEY_PGDOWN)
			(lambda (c)
			  (set! ncp-row (+ row nc-rows))
			  (when (>= ncp-row ncp-rows)
			    (ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 (+ ncp-row nc-rows) cols)
			    (set! ncp-rows (+ ncp-rows nc-rows)))
			  (ncplane_move_yx ncp ncp-row ncp-col)))

		  (set! (keymap NCKEY_RESIZE)      ; terminal window resized
			(lambda (c)
			  (let ((new-size (ncplane_dim_yx (notcurses_stdplane nc))))
			    (set! nc-cols (cadr new-size))
			    (set! nc-rows (car new-size)))))
		  ;; perhaps scroll so cursor is in view
		  ;; TODO: resize ncp if it's smaller
		  
		  (set! (keymap NCKEY_BACKSPACE)   ; backspace
			(lambda (c)
			  (when (> col (bols row))
			    (let ((trailing (and (> (eols row) col)
						 (ncplane_contents ncp row col 1 (- (eols row) col -1)))))
			      (if trailing
				  (begin
				    (nc-display row (- col 1) trailing)
				    (nc-display row (+ col (- (length trailing) 1)) " "))
				  (nc-display row (- col 1) " "))
			      
			      (set! (eols row) (- (eols row) 1))
			      (decrement-col 1)))))
		  ;; check this: if we backspaced past ncp-col, scroll-left
		  
		  (set! (keymap NCKEY_BUTTON1) ; mouse clisk, doesn't work in rxvt apparently
			(lambda (c)
			  (set-row (min ncp-max-row (ncinput_y ni)))
			  (if (< (eols row) (bols row))
			      (begin
				(nc-debug-display 12 0 "eol<bol! ~S ~S ~S" row (eols row) (bols row))
				(set! (eols row) (bols row))))
			  (set-col (max (min (eols row) (ncinput_x ni)) (bols row)))
			  (when (not mouse-col)
			    (set! mouse-col col)
			    (set! mouse-row row))))
		  
		  (set! (keymap NCKEY_RELEASE)  ; mouse button release
			(lambda (c)
			  (nc-debug-display 12 0 (format #f "mouse: ~S ~S to ~S ~S" mouse-row mouse-col (ncinput_y ni) (ncinput_x ni)))

			  ;; TODO: selection here could be multiline, but for now...
			  ;; TODO: highlight selected text
			  (when (and mouse-col (not (= col mouse-col)))
			    (set! selection (ncplane_contents ncp mouse-row (min col mouse-col) 1 (abs (- col mouse-col)))))
			  (set! mouse-col #f)))
		  
		  (set! (keymap NCKEY_ENTER)   ; enter: either eval/print or insert newline
			(lambda (c)
			  (let ((cur-line (current-expression ncp row)))
			    (nc-debug-display 12 0 (format #f "expr: ~S ~S" cur-line (bols row)))
			    
			    (increment-row 1)
			    (set! ncp-max-row (max ncp-max-row row))
			    
			    (if (> (eols row) 0)
				(clear-line row))
			    
			    (call-with-exit
			     (lambda (return)
			       (let ((len (length cur-line)))
				 
				 (do ((i 0 (+ i 1)))               ; check for just whitespace
				     ((or (= i len)
					  (not (char-whitespace? (cur-line i))))
				      (when (= i len)
					(set! (eols row) col)
					(return))))
				 
				 (catch #t
				   (lambda ()
				     
				     (catch 'string-read-error ; this matches (throw #t 5) -- is this correct? *missing-close-paren-hook* returns 'string-read-error
				       
				       (lambda ()
					 
					 ;; get the newline out if the expression does not involve a read error
					 (let-temporarily (((hook-functions *missing-close-paren-hook*) (list badexpr)))
					   
					   (let ((form (with-input-from-string cur-line #_read)))    ; not libc's read
					     
					     (let ((val #f))
					       (let ((str (with-output-to-string
							    (lambda ()
							      (set! val (list (new-eval form (*nrepl* 'top-level-let)))))))) ; list, not lambda -- confuses trace!
						 (when (and (string? str)
							    (> (length str) 0))
						   (nc-multiline-display ncp row col str)
						   (increment-row 1)))

					       ;; TODO: what about read* from stdin?
					       ;;   (let-temporarily (((current-input-port) nc-stdin)) ; a function port
					       ;;   then nc-stdin waits for calls (notcurses_getc...) itself
					       ;;     do input function ports handle char-at-a-time for all cases? yes, but can't be done from scheme

					       (if (or (null? val)   ; try to trap (values) -> #<unspecified>
						       (and (unspecified? (car val))
							    (null? (cdr val))))
						   (set! val #<unspecified>)
						   (set! val (if (pair? (cdr val))  ; val is a list, it must have caught multiple values if cdr is a pair
								 (cons 'values val)
								 (car val))))
					       
					       (if unbound-case
						   (begin
						     (set! unbound-case #f)
						     (nc-display row 0 (format #f "~A" (substring val 0 (- (length val) 1))))
						     (set! (eols row) (length val)))
						   (nc-multiline-display ncp row col (object->string val)))
					       ))))
				       
				       (lambda (type info)
					 (if (eq? type 'string-read-error)
					     (begin
					       ;; missing close paren, newline already added, spaces here are not optional!
					       (nc-display row 0 (make-string col #\space))
					       (set! (eols row) col)
					       (return))
					     (apply throw type info)))))   ; re-raise error
				   
				   (lambda (type info)
				     (set! error-row (display-error ncp row type info))))
				 
				 (increment-row 1)
				 (set! ncp-max-row (max ncp-max-row row))
				 (reprompt row)
					;(notcurses_render nc)
				 (notcurses_refresh nc)
				 ))))))
		  
		  
		  
		  ;; -------- read/eval/print loop --------
		  (let repl-loop ()
		    (unless repl-done
		      (let ((c (notcurses_getc nc (c-pointer 0) (c-pointer 0) ni)))
		    
			(nc-debug-display 15 0 (format #f "loop body start: ~S col: ~S end: ~S" (bols row) col (eols row)))

			(let ((func (hash-table-ref keymap (if (ncinput_ctrl ni) (+ c control-key) c))))
			  (if (procedure? func)
			      (func c)))

			(nc-debug-display 14 0 (format #f "loop end c: ~S start: ~S col: ~S end: ~S" c (bols row) col (eols row)))
			
			(notcurses_render nc)
			
			(when (integer? error-row)
			  (move-cursor ncd error-row 0)
			  (ncdirect_fg ncd #xff0000)
			  (format *stdout* "error:")
			  (ncdirect_fg_default ncd)
			  (set! error-row #f))
			
			;; if cursor is after ), look for matching open, highlight if found
			(when prev-pars
			  (move-cursor ncd (car prev-pars) (cdr prev-pars))
			  (format *stdout* "(")
			  (set! prev-pars #f))
			
			(unless (or (<= col (+ (bols row) 1)) ; got to be room for #\(
				    (not (string=? ")" (ncplane_contents ncp row (- col 1) 1 1)))
				    (and (>= col (+ (bols row) 3))
					 (string=? (ncplane_contents ncp row (- col 3) 1 3) "#\\)")))
			  (let ((pars (match-close-paren ncp row col #f)))
			    (when pars
			      (move-cursor ncd (car pars) (cdr pars))
			      (ncdirect_fg ncd #xff0000)
			      (format *stdout* "(")
			      (ncdirect_fg_default ncd)
			      (set! prev-pars pars))))
			
			(move-cursor ncd row col)
			(repl-loop))))
		  ))

	      (lambda (type info)
		(notcurses_stop nc)
		(apply format *stderr* info)
		(format *stderr* "~%line ~A: ~A~%" ((owlet) 'error-line) ((owlet) 'error-code))
#|
		(let ((elist (list () (rootlet) *libc*)))
		  ;; show the enclosing contexts
		  (let-temporarily (((*s7* 'print-length) 8))
		    (do ((e (outlet (owlet)) (outlet e)))
			((memq e elist))
		      (if (and (number? (length e)) ; with-let + mock-data + length method?
			       (> (length e) 0))
			  (format *stderr* "~%~{~A~| ~}~%" e)
			  (format *stderr* "e: ~S~%" e))
		      (set! elist (cons e elist)))))
|#
		(#_exit)
		)
	      )
	    ))) ; run
      
      
      (define (stop)
	(notcurses_stop nc)
	(#_exit))
      
      
      ;; -------- emacs --------
      (define (emacs-repl)
	;; someday maybe use the language server protocol? (not our own rpc stuff or epc), for json, see json.scm 
	;;   probably will need an argument/function for repl to open the channel or whatever
	;; also this does not resend the entire expression after editing
	;;   and does not notice in-place edits
	;;   can <cr> get entire expr?
	(with-let (sublet *libc*)
	  (let ((buf (c-pointer->string (calloc 512 1) 512)))
	    (format *stderr* "> ")
	    (do ((b (fgets buf 512 stdin) (fgets buf 512 stdin)))
		((zero? (length b))
		 (#_exit))
	      (let ((len (strlen buf)))
		(when (positive? len)
		  (do ((i 0 (+ i 1)))
		      ((or (not (char-whitespace? (buf i)))
			   (= i len))
		       (when (< i len)
			 (let ((str (substring buf 0 (- (strlen buf) 1))))
			   (catch #t
			     (lambda ()
			       (do ()
				   ((= (string-length str) 0))
				 (catch 'string-read-error
				   (lambda ()
				     (format *stderr* "~S~%> " (eval-string str (*nrepl* 'top-level-let)))
				     (set! str ""))
				   (lambda (type info)
				     (fgets buf 512 stdin)
				     (set! str (string-append str " " (substring buf 0 (- (strlen buf) 1))))))))
			     (lambda (type info)
			       (set! str "")
			       (apply format *stderr* info)
			       (format *stderr* "~%> ")))))))))))))

      (define (start)
	(if (with-let *libc*
	      (or (zero? (isatty (fileno stdin)))      ; not a terminal -- input from pipe probably
		  (string=? (getenv "TERM") "dumb")))  ; no vt100 codes -- emacs subjob for example
	    (emacs-repl)                            ; TODO: restore support for the pipe case
	    (begin
	      (set! ncd (ncdirect_init (c-pointer 0)))
	      (let ((noptions (notcurses_options_make)))
		(set! (notcurses_options_flags noptions) NCOPTION_SUPPRESS_BANNERS)
		(set! nc (notcurses_init noptions)))
	      (notcurses_cursor_enable nc)
	      (unless (string-position "rxvt" ((*libc* 'getenv) "TERM"))
		(notcurses_mouse_enable nc))
	      (let ((size (ncplane_dim_yx (notcurses_stdplane nc))))
		(set! nc-cols (cadr size))
		(set! nc-rows (car size))))))
      
      (curlet)))
  
  (with-let *nrepl*
    (start)
    (run)
    (stop)))


;; TODO: cursor at end, tab->no effect, anywhere else it's ok I think (not correct for do et al yet)
;; TODO: C-_, how to get M-*?
;; PERHAPS: box at bottom showing signatures: see glistener, [hover->underline] if clicked -> object->let in a box
;;          nrepl eval access to status area (and remember to move it)
;; TODO: test recursive call, check other prompts
;; TODO: stack/let-trace if error? too much irrelevant info -- maybe scan error-code
;; TODO: watch vars (debug.scm) in floating boxes?
;; PERHAPS: if several completions, display somewhere
;; from repl: drop-into-repl+debug.scm connection
;; xclip access the clipboard?? (system "xclip -o")=current contents, (system "echo ... | xclip")=set contents
;;   so if middle mouse=get from xclip if it exists etc, or maybe add example function, also selection-setter/getter
;; preload libc/notcurses

(set! (*s7* 'debug) old-debug)
*nrepl*
