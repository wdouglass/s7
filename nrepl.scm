;;; nrepl.scm -- notcurses-based repl

(set! (*s7* 'history-enabled) #f)

(provide 'nrepl.scm)
(require libc.scm)
(load "notcurses_s7.so" (inlet 'init_func 'notcurses_s7_init))

(define old-debug (*s7* 'debug))
(set! (*s7* 'debug) 0)
(define nrepl-debugging #t)

(unless (defined? '*nrepl*)
  (define *nrepl*
    (let* ((prompt ">")
	   (prompt-len 2)
	   (ncd #f)
	   (nc #f)
	   (nc-cols 0)
	   (nc-rows 0)
	   (top-level-let (sublet (rootlet) ; environment in which evaluation takes place
			    :nc nc
			    :exit (let ((+documentation+ "(exit) stops notcurses and then calls #_exit"))
				    (let-temporarily (((*s7* 'debug) 0))
				      (lambda ()
					(notcurses_stop nc)
					(#_exit))))))
	  )

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
	     (or match text)))))
      
      (define (filename-completion text)
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
		text
		(car files))))))


      
      (define (move-cursor ncd y x)   ; this was (format *stderr* "~C[~D;~DH" #\escape y x) in repl.scm (and it works here)
	(ncdirect_cursor_move_yx ncd y x))

      (define* (run orig-col orig-row)
	;; ncp-cols and rows, origs
	(let ((ncp-nc-col (or orig-col 0)) ; ncp top-left in nc (possibly started offset from nc 0,0)
	      (ncp-nc-row (or orig-row 0))
	      (ncp-col 0)                  ; top-left in ncp (possibly scrolled from original 0,0)
	      (ncp-row 0)
	      (ncp-cols 100)
	      (ncp-rows 100)
	      (ncp-max-row 0)
	      (col 0)
	      (row 0)
	      (unbound-case #f))


	  (let ((ncp (ncplane_new nc ncp-rows ncp-cols ncp-nc-row ncp-nc-col (c-pointer 0)))
		(eols (make-int-vector ncp-rows 0))
		(bols (make-int-vector ncp-rows 0)))
	    
	    ;; -------- evaluation ---------
	    (define (badexpr h)            ; *missing-close-paren-hook* function for Enter command
	      (let ((ow (owlet)))
		(if (and (ow 'error-file)
			 (not (equal? (ow 'error-file) "repl.scm")))
		    (error 'syntax-error "missing close paren in ~S" (ow 'error-file))
		    (set! (h 'result) 'string-read-error))))
	    
	    (define (shell? h)             ; *unbound-variable-hook* function, also for Enter
	      (if nrepl-debugging
		  (ncplane_putstr_yx ncp 9 0 (format #f "in shell? ~S ~S ~S ~S ~S" row col (bols (- row 1)) (eols (- row 1)) ((rootlet) 'system))))
	      ;; examine cur-line -- only call system if the unbound variable matches the first non-whitespace chars
	      ;;   of cur-line, and command -v name returns 0 (indicating the shell thinks it is an executable command)
	      (let ((cur-line (ncplane_contents ncp (- row 1) (bols (- row 1)) 1 col)))
		;; at this point (eols row) has not been set, so use col?
		(if nrepl-debugging (ncplane_putstr_yx ncp 11 0 cur-line))
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
	    
	    (define (clear-line row)
	      (ncplane_putstr_yx ncp row 0 (make-string (eols row) #\space)))

	    (define (reprompt y)
	      (ncplane_cursor_move_yx ncp y 0)
	      (ncplane_putstr_yx ncp y 0 prompt)
	      (notcurses_render nc)
	      (move-cursor ncd y prompt-len)
	      (set! (bols row) prompt-len)
	      (set! (eols row) prompt-len)
	      (set! col prompt-len)
	      (set! row y))
	
	    (reprompt 0)

	(catch #t
	  (lambda ()
	    (let ((ni (ncinput_make))
		  (selection #f)
		  (mouse-col #f)
		  (mouse-row #f)
		  (error-row #f))

	      (do ((c (notcurses_getc nc (c-pointer 0) (c-pointer 0) ni)
		      (notcurses_getc nc (c-pointer 0) (c-pointer 0) ni))
		   (c-ctr 0 (+ c-ctr 1)))
		  ((and (= c (char->integer #\Q))
			(ncinput_ctrl ni)))
		
		(when nrepl-debugging
		  (ncplane_putstr_yx ncp 15 0 (make-string 80 #\space))
		  (ncplane_putstr_yx ncp 15 0 (format #f "loop body ~S: start: ~S col: ~S end: ~S" c-ctr (bols row) col (eols row))))

		(cond
		 
		 ;; normal character typed
		 ((and (< c 256)
		       (not (ncinput_ctrl ni)))
		  (if (and nrepl-debugging
			   (= c (char->integer #\tab)))
		      (ncplane_putstr_yx ncp 18 0 (format #f "~S: bol: ~S col: ~S eol: ~S" c-ctr (bols row) col (eols row))))
		  (if (= c (char->integer #\tab)) ; TODO: tab as indentation
		      (when (= col (eols row))
			(let ((start (bols row))
			      (end (eols row)))
			  (if (= end start)
			      (begin
				(ncplane_putstr_yx ncp row end "    ")
				(set! (eols row) (+ end 4))
				(set! col (+ col 4)))
			      
			      (let ((cur-line (ncplane_contents ncp row (bols row) 1 (- (eols row) (bols row)))))
				
				(when nrepl-debugging
				  (ncplane_putstr_yx ncp 19 0 (make-string 80 #\space))
				  (ncplane_putstr_yx ncp 19 0 (format #f "~S: start: ~S end: ~S col: ~S cur-line: ~S" c-ctr start end col cur-line)))
				
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
				  
				  (when nrepl-debugging
				    (ncplane_putstr_yx ncp 20 0 (make-string 80 #\space))
				    (ncplane_putstr_yx ncp 20 0 (format #f "~S: completion: ~S loc: ~S len: ~S" c-ctr completion loc (length completion))))
				  
				  (when (and completion
					     (not (string=? completion cur-line)))
				    
				    (when (>= loc 0)
				      (set! completion (string-append (substring cur-line 0 (+ loc 1)) completion))
				      (if (char=? (cur-line loc) #\")
					  (set! completion (string-append completion "\""))))
				    
				    (ncplane_putstr_yx ncp row (bols row) completion)
				    (set! col (+ (bols row) (length completion)))
				    (set! (eols row) col)
				    
				    (if nrepl-debugging 
					(ncplane_putstr_yx ncp 21 0 (make-string 80 #\space))))
				  (if nrepl-debugging
				      (ncplane_putstr_yx ncp 21 0 (format #f "tab end: ~S ~S" col (eols row))))
				  ))
			      )))
		      
		      (begin
			(when nrepl-debugging
			  (ncplane_putstr_yx ncp 16 0 (make-string 80 #\space))
			  (ncplane_putstr_yx ncp 16 0 (format #f "~S: bol: ~S col: ~S eol: ~S" c-ctr (bols row) col (eols row))))
			
			(let ((trailing (and (> (eols row) col)
					     (ncplane_contents ncp row col 1 (- (eols row) col -1)))))
			  (ncplane_putstr_yx ncp row col (string (integer->char c)))
			  (if (and trailing (> (length trailing) 0))
			      (ncplane_putstr_yx ncp row (+ col 1) trailing)))
			
			(if (char=? (integer->char c) #\space)
			    (notcurses_refresh nc))
			(set! col (+ col 1)) ; might be midline
			(set! (eols row) (+ (eols row) 1)) ; in any case we've added a character
			
			(when nrepl-debugging
			  (ncplane_putstr_yx ncp 17 0 (make-string 80 #\space))
			  (ncplane_putstr_yx ncp 17 0 (format #f "~S: ~C: bol: ~S col: ~S eol: ~S" c-ctr (integer->char c) (bols row) col (eols row))))
			)
		      ;; (if (> (- col ncp-col) ncp-cols) (plane-expand-rows ncp col))
		      ;; (if (< nc-cols (+ (- col ncp-col) ncp-nc-col)) (scroll-plane-right ncp))
		      ))
		 
		 
		 ;; terminal window resized
		 ((= c NCKEY_RESIZE)
		  (let ((new-size (ncplane_dim_yx (notcurses_stdplane nc))))
		    (set! nc-cols (car new-size))
		    (set! nc-rows (cadr new-size))))
		 ;; perhaps scroll so cursor is in view
		 
		 
		 ;; backspace
		 ((= c NCKEY_BACKSPACE)
		  (when (> col (bols row))
		    (let ((trailing (and (> (eols row) col)
					 (ncplane_contents ncp row col 1 (- (eols row) col -1)))))
		      (if trailing
			  (begin
			    (ncplane_putstr_yx ncp row (- col 1) trailing)
			    (ncplane_putstr_yx ncp row (+ col (- (length trailing) 1)) " "))
			  (ncplane_putstr_yx ncp row (- col 1) " "))
		      
		      (set! (eols row) (- (eols row) 1))
		      (set! col (- col 1)))))
		 ;; if we backspaced past ncp-col, scroll-left
		 
		 ;; mouse click
		 ((= c NCKEY_BUTTON1) ; doesn't work in rxvt apparently
		  (set! row (min ncp-max-row (ncinput_y ni)))
		  (set! col (min (eols row) (max (ncinput_x ni) (bols row))))
		  (when (not mouse-col)
		    (set! mouse-col col)
		    (set! mouse-row row)))
		 
		 ((= c NCKEY_RELEASE)
		  (if nrepl-debugging
		      (ncplane_putstr_yx ncp 12 0 (format #f "mouse: ~S ~S to ~S ~S" mouse-row mouse-col (ncinput_y ni) (ncinput_x ni))))
		  ;; TODO: selection here could be multiline, but for now...
		  ;; TODO: highlight selected text
		  (when (and mouse-col (not (= col mouse-col)))
		    (set! selection (ncplane_contents ncp mouse-row (min col mouse-col) 1 (abs (- col mouse-col)))))
		  (set! mouse-col #f))

		 
		 ;; enter: either eval/print or insert newline
		 ((= c NCKEY_ENTER)
		  ;; TODO: see if we're at the end of expr else insert newline, catch exit so we can stop notcurses etc
		  (let ((cur-line (ncplane_contents ncp row (bols row) 1 (- (eols row) (bols row)))))
		    (set! row (+ row 1))
		    (if (> (eols row) 0)
			(clear-line row))


		    (call-with-exit
		     (lambda (return)
		       (let ((len (length cur-line)))
			 
			 (do ((i 0 (+ i 1)))               ; check for just whitespace
			     ((or (= i len)
				  (not (char-whitespace? (cur-line i))))
			      (when (= i len)
				(set! row (+ row 1)) ; append newline
				(return))))
			 
			 ;(set! red-par-pos #f)

			 (catch #t
			   (lambda ()
			     
			     (catch 'string-read-error ; this matches (throw #t 5) -- is this correct?
			       
			       (lambda ()
				 
				 ;; get the newline out if the expression does not involve a read error
				 (let-temporarily (((hook-functions *missing-close-paren-hook*) (list badexpr)))
				   (let ((form (with-input-from-string cur-line #_read)))    ; not libc's read

				     (let ((val (list (new-eval form (*nrepl* 'top-level-let))))) ; list, not lambda -- confuses trace!

				       (when nrepl-debugging
					 (ncplane_putstr_yx ncp 9 0 (make-string 80 #\space))
					 (ncplane_putstr_yx ncp 9 0 (format #f "val: ~S unbound: ~S" val unbound-case)))

				       (if (or (null? val)   ; try to trap (values) -> #<unspecified>
					       (and (unspecified? (car val))
						    (null? (cdr val))))
					   (set! val #<unspecified>)
					   (set! val (if (pair? (cdr val))
							 (cons 'values val)
							 (car val))))
				       (if unbound-case
					   (begin
					     (set! unbound-case #f)
					     (ncplane_putstr_yx ncp row 0 (format #f "~A" (substring val 0 (- (length val) 1)))))
					   ;; TODO: system results can be multiline
					   (ncplane_putstr_yx ncp row 0 (format #f "~S" val)))))))

			       
			       (lambda (type info)
				 (if (eq? type 'string-read-error)
				     (begin
				       ;(append-newline)
				       (return))
				     (apply throw type info)))))   ; re-raise error
			   
			   (lambda (type info)
			     (with-let (unlet)

			       (set! error-row row)

			       (ncplane_putstr_yx ncp row 0 "error:") ; TODO: red text
			       (let ((op (*s7* 'print-length)))
				 (if (< op 32) (set! (*s7* 'print-length) 32))
				 (if (and (pair? info)
					  (string? (car info)))
				     (ncplane_putstr_yx ncp row 7 (apply format #f info))
				     (if (not (null? info))
					 (ncplane_putstr_yx ncp row 1 (format #f "~S" info))))
				 (if (< op 32) (set! (*s7* 'print-length) op)))
			       ))))))

		    (set! row (+ row 1))
		    (set! ncp-max-row (max ncp-max-row row))
		    (reprompt row)
					;(notcurses_render nc)
		    (notcurses_refresh nc)
		    ))
		 
		 ((= c NCKEY_LEFT)
		  (set! col (max (bols row) (- col 1))))

		 ((= c NCKEY_RIGHT)
		  (set! col (min (eols row) (+ col 1))))

		 ((= c NCKEY_UP)
		  (set! row (max 0 (- row 1)))
		  (set! col (max col (bols row))))

		 ((= c NCKEY_DOWN)
		  (set! row (+ row 1))
		  (set! col (max col (bols row))))
		 ;; TODO: see below if multiline -- should we go on past current entry?
		 ;;   to test this we need multiline entries

		 ((ncinput_ctrl ni)
		  (case c		     
		     
		    ((68) ; #\d
		     (when (> (eols row) (bols row))
		       (let ((trailing (ncplane_contents ncp row (+ col 1) 1 (- (eols row) col -1))))
			 (ncplane_putstr_yx ncp row col trailing)
			 (ncplane_putstr_yx ncp row (+ col (length trailing)) " "))
		       (set! (eols row) (- (eols row) 1))))
		    
		    ((65) ; #\a
		     (set! col (bols row)))
		    ;; if offscreen scroll?
		    
		    ((66) ; #\b
		     (set! col (max (bols row) (- col 1))))
		    ;; TODO: this should move to the previous row if at left col, similarly for f
		    
		    ((69) ; #\e
		     (set! col (eols row)))
		    
		    ((70) ; #\f
		     (set! col (min (eols row) (+ col 1))))

		    ((75) ; #\k
		     (set! selection (ncplane_contents ncp row col 1 (- (eols row) col -1)))
		     (ncplane_putstr_yx ncp row col (make-string (- (eols row) col) #\space))
		     (set! (eols row) col))
		    
		    ((78) ; #\n
		     (set! row (+ row 1))
		     (set! col (max col (bols row))))

		    ((80) ; #\p
		     (set! row (max 0 (- row 1)))
		     (set! col (max col (bols row))))
		    
		    ((89) ; #\y
		     (when (string? selection)
		       (let ((trailing (and (> (eols row) col)
					    (ncplane_contents ncp row col 1 (- (eols row) col -1)))))
			 (ncplane_putstr_yx ncp 12 0 (format #f "~D: sel: [~S] ~D, trail: [~S]~%" c selection (length selection) trailing))
			 (ncplane_putstr_yx ncp row col selection)
			 (if (and trailing 
				  (> (length trailing) 0))
			     (ncplane_putstr_yx ncp row (+ col (length selection)) trailing)))
		       (set! (eols row) (+ (eols row) (length selection)))))
		     
		    
		    ((90)
		     #f)
		    ;; c-z suspends !! catch SIGTSTP
		    ))
		 )

		(when nrepl-debugging
		  (ncplane_putstr_yx ncp 14 0 (make-string 80 #\space))
		  (ncplane_putstr_yx ncp 14 0 (format #f "loop end ~S: c: ~S start: ~S col: ~S end: ~S" c-ctr c (bols row) col (eols row))))
		
		(notcurses_render nc)

		(when (integer? error-row)
		  (move-cursor ncd error-row 0)
		  (ncdirect_styles_on ncd NCSTYLE_STANDOUT)
		  (ncdirect_bg ncd #xff0000) ; weird: norcurses has the terminology backwards
		  (ncdirect_fg ncd #xffffff) ; TODO: need actual color here, but it's not ncplane_fg 
		  (format *stdout* "error:")
		  (ncdirect_fg_default ncd)
		  (ncdirect_bg_default ncd)
		  (ncdirect_styles_off ncd NCSTYLE_STANDOUT)
		  (set! error-row #f))

		(move-cursor ncd row col)

				)))

	  (lambda (type info)
	    (notcurses_stop nc)
	    (apply format *stderr* info)
	    (#_exit))
	  )
	))) ; run


      (define (stop)
	(notcurses_stop nc)
	(#_exit))
	      

      (define (start)
	(set! ncd (ncdirect_init (c-pointer 0)))
	(let ((noptions (notcurses_options_make)))
	  (set! (notcurses_options_flags noptions) NCOPTION_SUPPRESS_BANNERS)
	  (set! nc (notcurses_init noptions)))
	(notcurses_cursor_enable nc)
	(notcurses_mouse_enable nc)
#|	
	(let ((size (ncplane_dim_yx (notcurses_stdplane nc))))
	  (set! nc-cols (car size))
	  (set! nc-rows (cadr size)))
|#
	)

      (curlet)))

  (with-let *nrepl*
    (start)
    (run)
    (stop)))

;; history could return ncplane_contents as a string
;; TODO: reflect matching paren red
;; PERHAPS: box at bottom showing signatures, hover->underline if clicked -> object->let in a box
;; TODO: scroll all directions, resize as necessary, test recursive call, check other prompts
;; TODO: stack/let-trace if error
;; TODO: watch vars (debug.scm) in floating boxes?

(set! (*s7* 'debug) old-debug)
*nrepl*
