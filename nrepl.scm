;;; nrepl.scm -- notcurses-based repl
;;; 
;;; work-in-progress!

(set! (*s7* 'history-enabled) #f)

(provide 'nrepl.scm)

(define libc-let (if (defined? '*nlibc*) ; nrepl.c has the parts of *libc* we need under the name *nlibc*
		     *nlibc*
		     (begin
		       (require libc.scm)
		       *libc*)))

(unless (defined? '*notcurses*)          ; nrepl.c has notcurses_s7.c (thus *notcurses*) built-in
  (load "notcurses_s7.so" (inlet 'init_func 'notcurses_s7_init)))

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
	   (status-cells (with-let *notcurses* (vector (cell_make) (cell_make) (cell_make) (cell_make) (cell_make) (cell_make))))
	   (statp #f)

	   (top-level-let 
	    (sublet (rootlet) ; environment in which evaluation takes place
	      :history #f        ; set below
	      :nc-let #f
	      :ncp-let #f
	      :display-status #f
	      
	      :exit (let ((+documentation+ "(exit) stops notcurses and then calls #_exit"))
		      (let-temporarily (((*s7* 'debug) 0))
			(lambda ()
			  ((*notcurses* 'notcurses_stop) (*nrepl* 'nc))
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
			  (min2 (floor (log (length ap-name) 2))))
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

      
      ;; to call notcurses functions in the repl, use *notcurses* and *nrepl* or ncp-let:  ((*notcurses* 'notcurses_refresh) (ncp-let 'nc))
      
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
	     (with-let (sublet libc-let :text text)
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
		   (and (not (null? files)) 
			(null? (cdr files)) 
			(car files)))))))
      
      ;; -------- run --------
      (define* (run (prompt ">") (origin-row 0) (origin-col 0)) ; TODO: also header
 
	(with-let (sublet *notcurses* :libc-let libc-let :origin-col origin-col :origin-row origin-row :prompt prompt 
			  :status-cells status-cells :statp statp
			  :ncd ncd :nc nc :nc-cols nc-cols :nc-rows nc-rows :top-level-let top-level-let
			  :symbol-completion symbol-completion :filename-completion filename-completion)

	(let ((ncp-nc-col origin-col)      ; ncp top-left in nc (possibly started offset from nc 0,0)
	      (ncp-nc-row origin-row)
	      (ncp-col 0)                  ; top-left in ncp (possibly scrolled from original 0,0)
	      (ncp-row 0)
	      (ncp-cols (max 100 nc-cols))
	      (ncp-rows (max 100 nc-rows))
	      (ncp-max-row 0)
	      (col 0)                      ; ncplane-relative row/column
	      (row 0)
	      (prompt-len (+ (length prompt) 1))
	      (prev-pars #f)
	      (control-key (ash 1 33))    ; notcurses getc returns 32 bits
	      (keymap (make-hash-table 512))
	      (old-history (top-level-let 'history)) ; see below, old restored upon exit from this ncplane
	      (statp-row (- nc-rows 3)))  ; if nc size changes, status-area box needs to be resized

	  (when nrepl-debugging
	    (set! (setter 'col) (lambda (s v) (if (and (integer? v) (not (negative? v))) v (error "col ~S is bad" v))))
	    (set! (setter 'row) (lambda (s v) (if (and (integer? v) (not (negative? v))) v (error "row ~S is bad" v)))))

	  (define (display-status-area)
	    (ncplane_cursor_move_yx statp 0 0)
	    (ncplane_box statp 
			 (status-cells 0) (status-cells 1) (status-cells 2) 
			 (status-cells 3) (status-cells 4) (status-cells 5) 
			 2 (- nc-cols 1) 0)
	    (ncplane_move_yx statp statp-row 0)
	    (notcurses_render nc))

	  (define (statp-set-bg-color r g b)
	    ;; (statp-bg-set-color 0.85 0.85 0.85): light gray background
	    (let ((c1 (cell_make))
		  (color (logior (ash (floor (* r 256)) 16)
				 (ash (floor (* g 256)) 8)
				 (floor (* b 256)))))
	      (set! (cell_gcluster c1) (char->integer #\space))
	      (set! (cell_channels c1) (logior CELL_FGDEFAULT_MASK CELL_BGDEFAULT_MASK color))
	      (set! (cell_attrword c1) 0)
	      (ncplane_set_base_cell statp c1)
	      (notcurses_render nc)
	      c1))


	  (define (statp-display str)
	    (ncplane_putstr_yx statp 1 1 (make-string (- nc-cols 5) #\space))
	    (ncplane_putstr_yx statp 1 2 (substring str 0 (min (length str) (- nc-cols 3))))
	    (notcurses_render nc))

	  (set! (top-level-let 'display-status) statp-display)

	  
	  (define (move-cursor ncd y x)   ; this was (format *stderr* "~C[~D;~DH" #\escape y x) in repl.scm (and it works here)
	    (ncdirect_cursor_move_yx ncd (+ y ncp-row) (+ x ncp-col)))
      
	  (let ((ncp (ncplane_new nc ncp-rows ncp-cols ncp-nc-row ncp-nc-col (c-pointer 0)))
		(eols (make-int-vector ncp-rows 0))
		(bols (make-int-vector ncp-rows 0)))

	    (ncplane_move_below ncp statp) ; statp always displayed with ncplanes sliding underneath conceptually

	    ;; opaque plane
	    (let ((c1 (cell_make)))
	      (set! (cell_gcluster c1) (char->integer #\space))
	      (set! (cell_channels c1) 0) 
	      (set! (cell_attrword c1) 0)
	      (ncplane_set_base_cell ncp c1)
	      (notcurses_render nc))
#|
	    ;; kinda kludgey looking
	    ;;   maybe use another ncp 2+ on left with color there, and current ncp is 2- cols shifted to fit
	    ;;  ----->
	    ;;  header
	    ;;  ----->
	    ;; |
	    ;; | prompt> ... [row 0]
	    ;; 
	    (let ((br 0)
		  (c1 (cell_make)) (c2 (cell_make)) (c3 (cell_make)) (c4 (cell_make)) (c5 (cell_make)) (c6 (cell_make)))
	      ;(cells_load_box ncp br 0 c1 c2 c3 c4 c5 c6 "/\\\\/-|")
	      (cells_double_box ncp 0 0 c1 c2 c3 c4 c5 c6)
	      (ncplane_cursor_move_yx ncp 0 0)
	      (ncplane_box ncp c1 c2 c3 c4 c5 c6 (- statp-row 1) (- nc-cols 1) 0))
|#

	    (let ((last-name ""))
	      (set! (hook-functions *load-hook*)
		    (cons (lambda (h)
			    (unless (string=? (h 'name) last-name) 
			      (statp-display (format #f "loading ~S" (h 'name)))))
			  (hook-functions *load-hook*)))

	      (set! (hook-functions *autoload-hook*)
		    (cons (lambda (h)
			    (set! last-name (h 'file))
			    (statp-display (format #f "autoloading ~S from ~S" (h 'name) (h 'file))))
			  (hook-functions *autoload-hook*))))

	    
	    ;; -------- scrolling and resizing --------
	    ;;
	    ;; row and col are ncp-relative, its ncp-row|col are the location of its top left corner (0,0)
	    ;;   in the terminal window.  ncp-rows|cols are its size.  nc-rows|cols is the terminal size.
	    ;;   ncp-row = -5 means ncp is shifted left (to display the rightward portion), 
	    ;;   ncp-row = 5 means ncp is shifted right (moving its top-left corner to (5,0) in the terminal
	    ;;   (using (row,col) throughout).  This won't happen in normal editing, but the user 
	    ;;   can easily move ncp: (ncplane_move_yx (ncp-let 'ncp) 5 0).
	    ;; Here the assumption is we want val to be visible and the current row is already visible. 
	    ;;   The currently visible portion of ncp is from ((max 0 -ncp-row), (max 0 -ncp-col)) to (bottom right)
	    ;;   ((nc-rows - ncp-row), (nc-cols - ncp-col)) (assuming we're not completely off screen).

	    (define (set-ncp-row val)
	      (when prev-pars
		(set! prev-pars #f)
		(notcurses_refresh nc))
	      (set! ncp-row val))

	    (define (set-ncp-col val)
	      (when prev-pars
		(set! prev-pars #f)
		(notcurses_refresh nc))
	      (set! ncp-col val))
	    
	    (define (ncp-move y x)
	      (set-ncp-row y)
	      (set-ncp-col x)
	      (ncplane_move_yx ncp x y))


	    (define (nc-resize new-rows new-cols)
	      (let ((old-nc-cols nc-cols))
		(set! nc-cols new-cols)
		(set! nc-rows new-rows)

		(set! statp-row (- nc-rows 3))
		(ncplane_putstr_yx statp 1 (- old-nc-cols 1) " ")
		(ncplane_putstr_yx statp 2 (- old-nc-cols 1) " ")
		(ncplane_resize statp 0 0 3 (min old-nc-cols nc-cols) 0 0 3 nc-cols)

		(when (or (< ncp-rows nc-rows)
			  (< ncp-cols nc-cols))
		  (ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 (max ncp-rows nc-rows) (max ncp-cols nc-cols))
		  (when (> nc-rows ncp-rows)
		    (set! bols (copy bols (make-int-vector nc-rows)))
		    (set! eols (copy eols (make-int-vector nc-rows)))
		    (set! ncp-rows nc-rows))
		  (set! ncp-cols (max ncp-cols nc-cols)))))
	      

	    (define (set-row val)
	      (set! row val)
	      (when (>= row ncp-rows)
		(ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 (+ row 40) ncp-cols)
		(set! ncp-rows (+ row 40))
		(set! bols (copy bols (make-int-vector ncp-rows)))
		(set! eols (copy eols (make-int-vector ncp-rows))))
	      (cond ((>= row (- statp-row ncp-row)) ; current row is outside (below) the terminal window
		     (set-ncp-row (min 0 (- statp-row row 2)))
		     (ncplane_move_yx ncp ncp-row ncp-col))
		    ((< row (- ncp-row))             ; current row is outside (above) the terminal window
		     (set-ncp-row (- row))
		     (ncplane_move_yx ncp ncp-row ncp-col))))
	    
	    (define (set-col val)
	      (set! col val)
	      (when (>= col ncp-cols)
		(ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 ncp-rows (+ col 10))
		(set! ncp-cols (+ col 10)))
	      (cond ((>= col (- nc-cols ncp-col))  ; current column is outside (to the right of) the terminal window
		     (set-ncp-col (min 0 (- nc-cols col 2)))
		     (ncplane_move_yx ncp ncp-row ncp-col))
		    ((< col (- ncp-col))           ; current column is outside (to the left of) the terminal window, ncp-col < 0 means the ncp plane is shifted left
		     (set-ncp-col (- col))
		     (ncplane_move_yx ncp ncp-row ncp-col))
		    ((and (= col (bols row))       ; special case: we're stuck against the prompt, but want move it into view
			  (< ncp-col 0))
		     (set-ncp-col 0)
		     (ncplane_move_yx ncp ncp-row ncp-col))))


	    (define (increment-row incr)
	      (set! row (+ row incr))
	      (when (>= row ncp-rows)
		(ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 (+ row 40) ncp-cols)
		(set! ncp-rows (+ row 40))
		(set! bols (copy bols (make-int-vector ncp-rows)))
		(set! eols (copy eols (make-int-vector ncp-rows))))
	      (when (>= row (+ ncp-row statp-row))
		(set-ncp-row (min 0 (- statp-row row 2))) ; this moves to bottom, (- row) to top??
		(ncplane_move_yx ncp ncp-row ncp-col)))
	    
	    (define (increment-col incr)
	      (set! col (+ col incr))
	      (when (>= col ncp-cols)
		(ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 ncp-rows (+ col 10))
		(set! ncp-cols (+ col 10)))
	      (when (>= col (+ ncp-col nc-cols)) 
		(set-ncp-col (min 0 (- nc-cols col 2)))
		(ncplane_move_yx ncp ncp-row ncp-col)))
	    

	    (define (decrement-row incr)
	      (set! row (- row incr))
	      (when (<= row (- ncp-row)) ; current row is outside (above) the terminal window
		(set-ncp-row (- row))
		(ncplane_move_yx ncp ncp-row ncp-col)))

	    (define (decrement-col incr)
	      (set! col (- col incr))
	      (when (<= col (- ncp-col)) ; current column is outside (to the left of) the terminal window
		(set-ncp-col (- col))
		(ncplane_move_yx ncp ncp-row ncp-col)))

	    
	    (define (nc-multiline-display str)
	      (let ((len (length str)))
		(if (not (char-position #\newline str))
		    (nc-display row 0 str)
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
			    (set! start (+ i 1))))))
		(set! (eols row) (length str))))
	    
	    
	    ;; -------- top-level-let --------
	    (set! (top-level-let 'nc-let) (curlet))
	    
	    (set! (top-level-let 'history)
		  (lambda (filename)
		    (call-with-output-file filename
		      (lambda (p)
			(let ((timestamp (with-let (sublet libc-let)
					   (let* ((timestr (make-string 128))
						  (len (strftime timestr 128 "%a %d-%b-%Y %H:%M:%S %Z"
								 (localtime 
								  (time.make (time (c-pointer 0 'time_t*)))))))
					     (substring timestr 0 len)))))
			  (format p ";;; nrepl: ~A~%~%" timestamp))
			(do ((i 0 (+ i 1)))
			    ((= i ncp-max-row))
			  (if (> (bols i) 0)
			      (format p "~A ~A~%" (ncplane_contents ncp i 0 1 (bols i)) (ncplane_contents ncp i (bols i) 1 (eols i)))
			      (format p "~A~%" (ncplane_contents ncp i 0 1 (eols i)))))))))
	    
	    
	    ;; --------display --------
	    (define (clear-line row)
	      (ncplane_putstr_yx ncp row (bols row) (make-string (max 80 (- (eols row) (bols row))) #\space)))
	    
	    (define (nc-display r c str)
	      (if (char-position #\newline str)
		  (ncplane_putstr_yx ncp 20 0 (format #f "str has newline: ~S" str)))

	      (let ((len (length str)))
		(when (>= (+ c len) ncp-cols)
		  (ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 ncp-rows (+ c len 10))
		  (set! ncp-cols (+ c len 10)))
		(ncplane_putstr_yx ncp r c str)
		(if (= c 0) (set! (bols r) 0))))
	      
	    (define (nc-debug-display r c str)
	      (when nrepl-debugging
		(clear-line r)
		(nc-display r c str)
		(notcurses_render nc)))

	    (define (reprompt y)
	      (ncplane_cursor_move_yx ncp y 0)
	      (clear-line y)
	      (ncplane_putstr_yx ncp y 0 prompt)
	      (notcurses_render nc)
	      (move-cursor ncd y prompt-len)
	      (when (zero? (bols row)) ; might be in pre-existing section where we're just re-establishing an existing prompt
		(set! (bols row) prompt-len)
		(set! (eols row) prompt-len))
	      (set! col prompt-len)
	      (set-row y))

	    (define (display-error ncp row info)
	      (ncplane_putstr_yx ncp row 0 "error:")
	      (set! (eols row) 7)
	      (if (and (pair? info)
		       (string? (car info)))
		  (let ((err (apply format #f info)))
		    (nc-display row 7 err)
		    (set! (eols row) (+ (length err) 7)))
		  (if (not (null? info))
		      (let ((err (object->string info)))
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
	      ;; examine cur-line -- only call system if the unbound variable matches the first non-whitespace chars
	      ;;   of cur-line, and command -v name returns 0 (indicating the shell thinks it is an executable command)
	      (let ((cur-line (ncplane_contents ncp (- row 1) (bols (- row 1)) 1 col)))
		;; at this point (eols row) has not been set, so use col?
		(do ((i 0 (+ i 1)))
		    ((or (= i (length cur-line))
			 (not (char-whitespace? (cur-line i))))
		     (let ((var-name (symbol->string (h 'variable))))
		       (when (and (>= (- (length cur-line) i) (length var-name)) ; var-name might be unrelated to cur-line
				  (string=? var-name (substring cur-line i (+ i (length var-name))))
				  (zero? (system (string-append "command -v " var-name " >/dev/null"))))
			 (set! (h 'result)
			       (and (procedure? ((rootlet) 'system))
				    (((rootlet) 'system) cur-line #t)))))))))
	    
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
			 (do ((nrow (+ i 1) (+ nrow 1)))
			     ((> nrow row)
			      expr)
			   (set! expr (append expr "\n" ; need newline to terminate comments
					      (ncplane_contents ncp nrow (bols nrow) 1 (- (eols nrow) (bols nrow)))))))))))


	    ;; -------- match close paren --------
	    (define (match-close-paren ncp row col indenting)
	      ;; if row/col is just after #|), get start of current expr, scan until row/col
	      ;;   return either matching row/col or #f if none
	      (call-with-exit
	       (lambda (return)
		 (do ((r row (- r 1)))
		     ((> (bols r) 0)
		      
		      (do ((cur-row r (+ cur-row 1))
			   (oparens ()))
			  ((> cur-row row)
			   ;; ((top-level-let 'display-status) (format #f "~S ~S ~S" row cur-row oparens))
			   (and (pair? oparens)
				oparens))
			
			(let* ((cur-line (ncplane_contents ncp cur-row (bols cur-row) 1 (- (eols cur-row) (bols cur-row))))
			       (len (if (and (= cur-row row) 
					     (not indenting))
					(min (- col (bols row) 1) (length cur-line))
					(length cur-line))))
			  
			  (do ((i 0 (+ i 1)))
			      ((>= i len))
			    (case (cur-line i)
			      ((#\()
			       (set! oparens (cons (list cur-row (+ i (bols cur-row)) 0 #f) oparens)))
			      
			      ((#\))
			       (when (pair? oparens)
				 (set! oparens (cdr oparens))
				 (when (and indenting
					    (pair? oparens))
				   (let ((top (cddar oparens)))
				     (set-car! top (+ (car top) 1))))))
			      
			      ((#\;)
			       (set! i (+ len 1)))
			      
			      ((#\")
			       (let ((found-close-quote #f))
				 (do ()
				     (found-close-quote) ; need to look possibly across several rows
				   (do ((k (+ i 1) (+ k 1)))
				       ((or (>= k len)
					    (and (char=? (cur-line k) #\")
						 (not (char=? (cur-line (- k 1)) #\\))))
					((top-level-let 'display-status) (format #f "~S ~S ~S ~S" cur-row row k len))
					(if (>= k len)  ; no close quotes by eol
					    (if (= cur-row row)
						(return #f)
						(begin
						  (set! cur-row (+ cur-row 1))
						  (set! i 0)
						  (set! cur-line (ncplane_contents ncp cur-row (bols cur-row) 1 (- (eols cur-row) (bols cur-row))))
						  (set! len (if (and (= cur-row row) 
								     (not indenting))
								(min (- col (bols row) 1) (length cur-line))
								(length cur-line)))))
					    (begin
					      (set! found-close-quote #t)
					      (set! i k))))))))
			      
			      ((#\#)
				   (if (and (< i (- len 1))
					    (char=? (cur-line (+ i 1)) #\|))
				       (let ((found-close-comment #f))
					 (do ()
					     (found-close-comment)
					   (do ((k (+ i 1) (+ k 1)))
					       ((or (>= k (- len 1))
						    (and (char=? (cur-line k) #\|)
							 (char=? (cur-line (+ k 1)) #\#)))
						(if (>= k len)
						    (if (= cur-row row)
							(return #f)
							(begin
							  (set! cur-row (+ cur-row 1))
							  (set! i -1)
							  (set! cur-line (ncplane_contents ncp cur-row (bols cur-row) 1 (- (eols cur-row) (bols cur-row))))
							  (set! len (if (and (= cur-row row) 
									     (not indenting))
									(min (- col (bols row) 1) (length cur-line))
									(length cur-line)))))
						    (begin
						      (set! found-close-comment #t)
						      (set! i (+ k 1))))))))))
			      
			      ((#\space #\newline #\tab #\linefeed #\return))
			      
			      (else
			       (when (and indenting
					  (pair? oparens))
				 (let ((top (cdddar oparens))
				       (c (+ i (bols cur-row))))
				   (if (pair? (car top))
				       (set-car! (cdar top) c)
				       (set! (car top) (list c c)))))))))))))))


	    ;; -------- indentation --------
	    (define white-space (string #\newline #\space #\return #\tab #\linefeed))

	    (define (indent ncp row col)
	      (if (not (zero? (bols row)))
		  col
		  (let ((pars (match-close-paren ncp (- row 1) (eols (- row 1)) #t)))
		    (if (not pars)
			(set! (eols row) 0)
			(let ((unmatched-par (car pars))) 
			  ;;     -> (row col-of-( visits #f-or-(name-start name-end)
			  ;;  row/col args = cursor pos
			  (let ((open-row (car unmatched-par))
				(open-col (cadr unmatched-par))
				(visits (caddr unmatched-par))       ; how many complete lists follow this (
				(name-bounds (cadddr unmatched-par)) ; bounds of non-white-space after (
				(trailer (ncplane_contents ncp row 0 1 (eols row)))
				(slice 0))

			    (let ((tlen (length trailer)))
			      (when (positive? tlen)
				(do ((i 0 (+ i 1)))
				    ((or (= i tlen)
					 (not (char-whitespace? (trailer i))))
				     (set! slice i)
				     (set! trailer (substring trailer i))))))
			    
			    (if (not (pair? name-bounds))
				(let ((incr 2)
				      (uname (and (pair? (cdr pars))
						  (let* ((upar (cadr pars))
							 (uopen-row (car upar))
							 (uopen-col (cadr upar))
							 (uvisits (caddr upar))
							 (uname-bounds (cadddr upar))
							 (uname+args (ncplane_contents ncp uopen-row (car uname-bounds) 1 (+ (- (cadr uname-bounds) (car uname-bounds)) 1)))
							 (uwpos (char-position white-space uname+args)))
						    (if (integer? uwpos)
							(substring uname+args 0 uwpos)
							uname+args)))))
				  (if (string=? uname "do")
				      (set! incr 1))

				  (when nrepl-debugging
				    (clear-line 30)
				    (nc-display 30 0 (format #f "639: ~S ~S ~S ~S ~S ~S ~S"
							     open-row open-col visits name-bounds trailer
							     (cdr pars) uname)))

				  ;; nothing after the ( on its row, might be moving back, so we need to erase the current line
				  (clear-line row)
				  (nc-display row 0 (format #f "~NC~A" (+ open-col incr) #\space trailer))
				  (let ((old-eol (eols row)))
				    (set! (eols row) (+ open-col incr (length trailer)))
				    ;(nc-display 30 0 (format #f "old: ~S col ~S eol: ~S open: ~S" old-eol col (eols row) open-col))
				    (min (+ open-col incr (- col slice)) (eols row)))) ; keep cursor in its relative-to-trailer position if possible

			    (let* ((name+args (ncplane_contents ncp open-row (car name-bounds) 1 (+ (- (cadr name-bounds) (car name-bounds)) 1)))
				   (wpos (char-position white-space name+args))
				   (name (if (integer? wpos)
					     (substring name+args 0 wpos)
					     name+args)))

			      (when nrepl-debugging
				(clear-line 30)
				(nc-display 30 0 (format #f "658: ~S ~S ~S ~S ~S ~S ~S ~S ~S" 
							 row col open-row open-col visits name-bounds
							 name+args name
							 trailer)))

			      (let ((increment 
				     (if (not (member name '("cond" "when" "unless" "lambda" "lambda*" "begin" "case" "with-let") string=?))
					 (if (string=? name "do")
					     (if (= visits 1) 4
						 2)
					     (+ (length name) 2))
					 2)))
				
				;; might be moving back, so we need to erase the current line
				(clear-line row)
				(nc-display row 0 (format #f "~NC~A" (+ increment open-col) #\space trailer))
				(set! (eols row) (+ open-col increment (length trailer)))
				;(statp-display (format #f "~S ~S ~S ~S" open-col increment col slice))
				(max 0 (min (+ open-col increment (- col slice)) (eols row))))) 
			        ;; keep cursor in its relative-to-trailer position if possible, but squeeze slice (space) if not enough room
			    )))))))
	    
	    ;;-------- tab --------
	    (define (tab c)
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
			      (set-col (indent ncp row col))
			      (let ((loc (do ((i (- (length cur-line) 1) (- i 1)))
					     ((or (< i 0)
						  (char-whitespace? (cur-line i))
						  (memv (cur-line i) '(#\( #\' #\" #\))))
					      i))))
				
				(if (= (+ loc 1) (eols row))
				    (set-col (indent ncp row col))
				    (let ((completion (if (< loc 0) ; match whole cur-line
							  (symbol-completion cur-line)
							  ((if (char=? (cur-line loc) #\") filename-completion symbol-completion)
							   (substring cur-line (+ loc 1))))))
				      
				      (if (not completion)
					  (set-col (min (indent ncp row col) (eols row)))
					  (unless (string=? completion cur-line)
					    
					    (when (>= loc 0)
					      (set! completion (string-append (substring cur-line 0 (+ loc 1)) completion))
					      (if (char=? (cur-line loc) #\")
						  (set! completion (string-append completion "\""))))
					    
					    (nc-display row (bols row) completion)
					    (set-col (+ (bols row) (length completion)))
					    (set! (eols row) col))))))))))))


	    ;; -------- run ---------
	    (reprompt 0)
	    
	    (catch #t
	      (lambda ()
		(let ((ni (ncinput_make))
		      (mouse-col #f)
		      (mouse-row #f)
		      (error-row #f)
		      (repl-done #f)
		      (selection #f))

		  (set! (top-level-let 'ncp-let) (curlet))
		  
		  (define stdin-wrapper (open-input-function ; current-input-port for scheme input in repl
					 (lambda (choice)
					   (case choice
					     ((read char-ready? peek-char) "?")
					     ((read-char) (integer->char (notcurses_getc nc (c-pointer 0) (c-pointer 0) ni)))
					     ((read-line)
					      (let loop ((str ""))
						(let ((c (notcurses_getc nc (c-pointer 0) (c-pointer 0) ni)))
						  (if (or (= c (char->integer #\newline))
							  (> c 255))
						      str
						      (loop (append str (string (integer->char c))))))))))))

		  ;; -------- enter --------
		  (define (enter c)                      ; either eval/print or insert newline
		    (let ((cur-line (current-expression ncp row)))
		      (increment-row 1)
		      (set! ncp-max-row (max ncp-max-row row))
		      
		      (if (> (eols row) 0)
			  (clear-line row))
		      
		      (call-with-exit
		       (lambda (return)
			 (do ((len (length cur-line))
			      (i 0 (+ i 1)))               ; check for just whitespace
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
				   
				   (let ((form (with-input-from-string cur-line #_read))
					 (val #f))
				     (let-temporarily (((current-input-port) stdin-wrapper)  ; for scheme side input (read-char etc)
						       ((hook-functions *ncp-move-hook*)     ; scheme code calls ncplane_move_yx??
							(list (lambda (h)
								(when (eq? (h 'plane) ncp)
								  (set! ncp-row (h 'y))
								  (set! ncp-col (h 'x)))))))
				       (let ((str (with-output-to-string                     ; for scheme side output
						    (lambda ()
						      (set! val (list (new-eval form (*nrepl* 'top-level-let)))))))) ; list, not lambda -- confuses trace!
					 (when (> (length str) 0)
					   (set-col ncp 0)
					   (nc-multiline-display str)
					   (increment-row 1))))
				     
				     (set! val (if (or (null? val)   ; try to trap (values) -> #<unspecified>
						       (and (unspecified? (car val))
							    (null? (cdr val))))
						   #<unspecified>
						   (if (pair? (cdr val))  ; val is a list, it must have caught multiple values if cdr is a pair
						       (cons 'values val)
						       (car val))))
				     
				     (nc-multiline-display (object->string val)))))					   
			       
			       (lambda (type info)
				 (if (eq? type 'string-read-error)
				     (begin
				       ;; missing close paren, newline already added, spaces here are not optional!
				       (nc-display row 0 (make-string col #\space))
				       (set! (eols row) col)
				       (return))
				     (apply throw type info)))))   ; re-raise error
			   
			   (lambda (type info)
			     (if (and (eq? type 'read-error)       ; maybe we hit <enter> in a block comment
				      (equal? info (list "unexpected end of input while reading #|")))
				 (begin
				   (nc-display row 0 (make-string col #\space))
				   (set! (eols row) col)
				   (return)))
			     (set! error-row (display-error ncp row info))))
			 
			 (increment-row 1)
			 (set! ncp-max-row (max ncp-max-row row))
			 (reprompt row)
			 (notcurses_refresh nc)))))
		  

		  ;; -------- keymap --------
		  (define (normal-char c)
		    (let ((trailing (and (> (eols row) col)
					 (ncplane_contents ncp row col 1 (- (eols row) col -1)))))
		      (nc-display row col (string (integer->char c)))
		      (if (and trailing (> (length trailing) 0))
			  (nc-display row (+ col 1) trailing)))
		    
		    (if (char=? (integer->char c) #\space)
			(notcurses_refresh nc))
		    (increment-col 1) ; might be midline
		    (set! (eols row) (+ (eols row) 1))) ; in any case we've added a character
		  
		  (do ((i 0 (+ i 1)))
		      ((= i 256))
		    (set! (keymap i) normal-char))
		  
		  (set! (keymap (char->integer #\tab)) tab)
		  
		  (set! (keymap (+ control-key (char->integer #\A)))
			(lambda (c)
			  (unless (>= ncp-col 0)
			    (set-ncp-col 0)
			    (ncplane_move_yx ncp ncp-row ncp-col))
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
			  (when (and (< col (eols row)) ; should this delete the newline?
				     (> (eols row) (bols row)))
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

		  (set! (keymap (+ control-key (char->integer #\G))) ; get a prompt
			(lambda (c)
			  (increment-row 1)
			  (set-ncp-col 0)
			  (ncplane_move_yx ncp ncp-row ncp-col)
			  (reprompt row)
			  (notcurses_refresh nc)
			  ))

		  (set! (keymap (+ control-key (char->integer #\K)))
			(lambda (c)
			  (set! selection (ncplane_contents ncp row col 1 (- (eols row) col -1)))
			  (nc-display row col (make-string (- (eols row) col) #\space))
			  (set! (eols row) col)))

		  (set! (keymap (+ control-key (char->integer #\L))) ; not the same as emacs's C-l (moves current row to top)
			(lambda (c)
			  (unless (= row (- ncp-row))
			    (set-ncp-row (- row))
			    (ncplane_move_yx ncp ncp-row ncp-col))))
			
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
			  (set! (top-level-let 'history) old-history))) ; fix up the ncp pointer I think
			
		  (set! (keymap (+ control-key (char->integer #\P)))
			(lambda (c)
			  (set-row (max 0 (- row 1)))
			  (set-col (min (max col (bols row)) (eols row)))))
			
		  (set! (keymap (+ control-key (char->integer #\Y)))
			(lambda (c)
			  (when (string? selection)
			    (let ((trailing (and (> (eols row) col)
						 (ncplane_contents ncp row col 1 (- (eols row) col -1)))))
			      (begin
				(nc-display row col selection)
				(if (char=? #\space (selection (- (length selection) 1)))
				    (notcurses_refresh nc)))
			      (if (and trailing 
				       (> (length trailing) 0))
				  (nc-display row (+ col (length selection)) trailing)))
			    (set! (eols row) (+ (eols row) (length selection)))
			    (set! col (eols row)))))

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

		  (set! (keymap NCKEY_HOME)
			(lambda (c)
			  (ncp-move 0 0)))

		  (set! (keymap NCKEY_PGUP)
			(lambda (c)
			  (set-ncp-row (max 0 (- row nc-rows)))
			  (ncplane_move_yx ncp ncp-row ncp-col)))

		  (set! (keymap NCKEY_PGDOWN)
			(lambda (c)
			  (set-ncp-row (+ row nc-rows))
			  (when (>= ncp-row ncp-rows)
			    (ncplane_resize ncp 0 0 ncp-rows ncp-cols 0 0 (+ ncp-row nc-rows) ncp-cols)
			    (set! ncp-rows (+ ncp-rows nc-rows))
			    (set! bols (copy bols (make-int-vector ncp-rows)))
			    (set! eols (copy eols (make-int-vector ncp-rows))))
			  (ncplane_move_yx ncp ncp-row ncp-col)))

		  (set! (keymap NCKEY_RESIZE)      ; terminal window resized (not a key event)
			(lambda (c)
			  (apply nc-resize (ncplane_dim_yx (notcurses_stdplane nc)))))
		  
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
		  
		  (set! (keymap NCKEY_BUTTON1) ; mouse click, doesn't work in rxvt apparently, right button = button2? click scroll??
			(lambda (c)
			  (set-row (min ncp-max-row (ncinput_y ni)))
			  ;(set! (eols row) (max (bols row) (eols row))) ; is this needed?
			  (set-col (max (min (eols row) (ncinput_x ni)) (bols row)))
			  (unless mouse-col
			    (set! mouse-col col)
			    (set! mouse-row row))))
		  
		  (set! (keymap NCKEY_RELEASE)  ; mouse button release
			(lambda (c)
			  (when (and (number? mouse-col)
				     (not (= col mouse-col)))
			    (set! selection (ncplane_contents ncp mouse-row (min col mouse-col) 1 (abs (- col mouse-col)))))
			  (set! mouse-col #f)))
		  
		  (set! (keymap NCKEY_ENTER) enter)  
		  

		  ;; -------- read/eval/print loop --------

		  (let repl-loop ()
		    (display-status-area)

		    (if repl-done
			(begin
			  ;; remove ncplane and free it
			  ;; refresh 
			  (ncplane_destroy ncp) ; does this free ncp?
			  (set! ncp #f)
			  (notcurses_render nc)
			  (notcurses_refresh nc)
			  )
			(begin
		      (let* ((c (notcurses_getc nc (c-pointer 0) (c-pointer 0) ni))
			     (func (hash-table-ref keymap (if (ncinput_ctrl ni) (+ c control-key) c))))

			(if (procedure? func)
			    (catch #t
			      (lambda ()
				(func c))
			      (lambda (type info) ; this is usually an internal (nrepl) error, or sometimes a read error
				(display-error ncp row info)
				(increment-row 1)
				(reprompt row)
				(notcurses_refresh nc)))))

		      (notcurses_render nc)
		      
		      (when (and (integer? error-row)
				 (= ncp-col 0))
			(move-cursor ncd error-row 0)
			(ncdirect_fg ncd #xff0000)
			(format *stdout* "error:")
			(ncdirect_fg_default ncd)
			(set! error-row #f))
		      
		      ;; if cursor is after ), look for matching open, highlight if found
		      (when (pair? prev-pars)
			(move-cursor ncd (car prev-pars) (cadr prev-pars))
			(format *stdout* "(")
			(set! prev-pars #f))

		      (unless (or (<= col (+ (bols row) 1)) ; got to be room for #\(
			          ;(< col (- ncp-col))
				  ;(> col (+ ncp-col nc-cols))
			      (not (string=? ")" (ncplane_contents ncp row (- col 1) 1 1)))
			      (and (>= col (+ (bols row) 3))
				   (string=? (ncplane_contents ncp row (- col 3) 1 3) "#\\)")))
			(let ((pars (match-close-paren ncp row col #f)))
			  (when (pair? pars)
			    (move-cursor ncd (caar pars) (cadar pars))
			    (ncdirect_fg ncd #xff0000)
			    (format *stdout* "(")
			    (ncdirect_fg_default ncd)
			    (set! prev-pars (car pars))
			    
			    ;; if sig of (name...), check against args 
			    ;;    also arity
			    ;;    try a simple case
			    (when (= row (caar pars))
			      (let ((expr (ncplane_contents ncp row (cadar pars) 1 (- col (cadar pars)))))
				(catch #t
				  (lambda ()
				    (let ((form (with-input-from-string expr read)))
				      (when (and (pair? form)
						 (symbol? (car form)))
					(let* ((func (symbol->value (car form)))
					       (sig (signature func))
					       (ary (arity func)))
					  (when (and (pair? sig) 
						     (pair? ary))
					    (if (< (length (cdr form)) (car ary))
						(statp-display (format #f "~S: not enough arguments" (car form)))
						(if (> (length (cdr form)) (cdr ary))
						    (statp-display (format #f "~S: too many arguments" (car form)))))
					    )))))
				  (lambda (type info)
				    #f)))))))

		      (move-cursor ncd row col)
		      (repl-loop))))))

	      (lambda (type info)
		(display-error ncp row info)
		(notcurses_stop nc)
		(apply format *stderr* info)
		(format *stderr* "~%line ~A: ~A~%" ((owlet) 'error-line) ((owlet) 'error-code))
		(#_exit)
		)
	      )
	    ))) ; run
	)
      
      (define (stop)
	((*notcurses* 'notcurses_stop) nc)
	(#_exit))
      
      
      ;; -------- emacs --------
      (define (emacs-repl)
	;; someday maybe use the language server protocol? (not our own rpc stuff or epc), for json, see json.scm 
	;;   probably will need an argument/function for repl to open the channel or whatever
	;; also this does not resend the entire expression after editing
	;;   and does not notice in-place edits
	;;   can <cr> get entire expr?
	(with-let (sublet libc-let)
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
	(if (with-let libc-let
	      (or (zero? (isatty (fileno stdin)))      ; not a terminal -- input from pipe probably
		  (string=? (getenv "TERM") "dumb")))  ; no vt100 codes -- emacs subjob for example
	    (emacs-repl)
	    (begin
	      (set! ncd ((*notcurses* 'ncdirect_init) (c-pointer 0)))
	      (let ((nc+size (with-let (sublet *notcurses* :libc-let libc-let)
			       (let ((nc #f))
				 (let ((noptions (notcurses_options_make)))
				   (set! (notcurses_options_flags noptions) NCOPTION_SUPPRESS_BANNERS)
				   (set! nc (notcurses_init noptions)))
				 (notcurses_cursor_enable nc)
				 (unless (string-position "rxvt" ((libc-let 'getenv) "TERM"))
				   (notcurses_mouse_enable nc))
				 (list nc (ncplane_dim_yx (notcurses_stdplane nc)))))))
		(set! nc (car nc+size))
		(set! nc-cols (cadadr nc+size))
		(set! nc-rows (caadr nc+size))

		(set! statp ((*notcurses* 'ncplane_new) nc nc-rows nc-cols 0 0 (c-pointer 0)))
		((*notcurses* 'cells_double_box) statp 0 0 (status-cells 0) (status-cells 1) (status-cells 2) (status-cells 3) (status-cells 4) (status-cells 5))
		((*notcurses* 'ncplane_putstr_yx) statp 1 1 (make-string (- nc-cols 5) #\space))))))

      (curlet)))
  
  (with-let *nrepl*
    (start)
    (run)
    (stop)))

;; PERHAPS: status showing signatures (or help): see glistener, if (double)clicked -> object->let in a box
;;          if several completions, display as list in status
;;          if arg type mismatch, orange arg + note in status, could even connect lint
;; TODO: stack/let-trace if error? too much irrelevant info -- maybe scan error-code
;; TODO: watch vars (debug.scm) in floating boxes?
;; from repl: drop-into-repl+debug.scm connection
;; xclip access the clipboard?? (system "xclip -o")=current contents, (system "echo ... | xclip")=set contents
;;   so if mouse(2)=get from xclip if it exists etc, or maybe add example function
;; perhaps test via input-file | repl+nrepl -> history + diff?
;; new notcurses stuff 
;; might be nice to have some indication that there is data outside the view
;; statp-fg ncp-bg|fg?? statp-set-bg-color only works once apparently
;; in recursive call, header giving reason?

(set! (*s7* 'debug) old-debug)
*nrepl*
