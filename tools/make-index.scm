;;; make-index.scm translated from index.cl
;;;   run this -noinit so that loads in ~/.snd_s7 don't confuse matters

(if (provided? 'pure-s7)
    (define (char-ci=? . chars) (apply char=? (map char-upcase chars))))

;(set! (hook-functions *load-hook*) (list (lambda (hook) (format () "loading ~S~%" (hook 'name)))))
(set! (hook-functions *unbound-variable-hook*) ())

(define scheme-variable-names
  (let ((h (make-hash-table)))
    (for-each
     (lambda (name)
       (set! (h name) #t))
     '("after-graph-hook" "lisp-graph-hook" "before-transform-hook" "mix-release-hook" "stop-playing-channel-hook" "save-hook" "mus-error-hook"
       "mouse-enter-graph-hook" "mouse-leave-graph-hook" "open-raw-sound-hook" "select-channel-hook" "after-open-hook" "close-hook" "drop-hook" "update-hook"
       "just-sounds-hook" "mark-click-hook" "mark-drag-hook" "name-click-hook" "open-hook" "help-hook"
       "output-comment-hook" "play-hook" "snd-error-hook" "snd-warning-hook" "start-playing-hook" "stop-playing-hook"
       "stop-playing-region-hook" "mouse-enter-listener-hook" "mouse-leave-listener-hook" "window-property-changed-hook" "select-sound-hook"
       "print-hook" "exit-hook" "during-open-hook" "transform-hook" "mouse-enter-label-hook" "mouse-leave-label-hook" "initial-graph-hook"
       "graph-hook" "key-press-hook" "mouse-drag-hook" "mouse-press-hook" "enved-hook" "read-hook" "mouse-click-hook" "new-widget-hook"
       "mark-hook" "previous-files-select-hook" "dac-hook" "stop-dac-hook" "stop-playing-selection-hook" "after-apply-controls-hook"
       "draw-mark-hook" "bad-header-hook" "save-state-hook" "new-sound-hook" "color-hook" "orientation-hook" "listener-click-hook"
       "mix-click-hook" "after-save-state-hook" "mouse-enter-text-hook" "mouse-leave-text-hook" "mix-drag-hook"
       "start-playing-selection-hook" "selection-changed-hook" "*current-sound*"
       "before-save-state-hook" "after-save-as-hook" "after-transform-hook" "before-save-as-hook"))
    h))

(define scheme-constant-names
  (let ((h (make-hash-table)))
    (for-each
     (lambda (name)
       (set! (h name) #t))
     '("mus-out-format" "mus-unsupported" "mus-next" "mus-aifc" "mus-riff" "mus-rf64" "mus-nist" "mus-raw" "mus-ircam" "mus-aiff" "mus-bicsf"
       "mus-voc" "mus-svx" "mus-soundfont" "mus-unknown" "mus-bshort" "mus-lshort" "mus-mulaw" "mus-alaw" "mus-byte" "mus-ubyte"
       "mus-bfloat" "mus-lfloat" "mus-bint" "mus-lint" "mus-bintn" "mus-lintn" "mus-b24int" "mus-l24int" "mus-bdouble" "mus-ldouble"
       "mus-ubshort" "mus-ulshort" "mus-bdouble-unscaled" "mus-ldouble-unscaled" "mus-bfloat-unscaled" "mus-lfloat-unscaled"
       "mus-audio-default"
       "rectangular-window" "hann-window" "welch-window"
       "parzen-window" "bartlett-window" "hamming-window" "blackman2-window" "blackman3-window" "blackman4-window" "exponential-window"
       "riemann-window" "kaiser-window" "cauchy-window" "poisson-window" "gaussian-window" "tukey-window" "dolph-chebyshev-window"
       "samaraki-window" "ultraspherical-window" "blackman5-window" "blackman6-window" "blackman7-window" "blackman8-window" 
       "blackman9-window" "blackman10-window" "rv2-window" "rv3-window" "rv4-window"
       "zoom-focus-left" "zoom-focus-right" "zoom-focus-active" "zoom-focus-middle" "graph-once"
       "graph-as-wavogram" "graph-as-sonogram" "graph-as-spectrogram" "cursor-cross" "cursor-line" "graph-lines" "graph-dots"
       "graph-filled" "graph-dots-and-lines" "graph-lollipops" "x-axis-in-seconds" "x-axis-in-samples" "x-axis-in-beats" "x-axis-in-measures"
       "x-axis-as-percentage" "show-all-axes" "show-all-axes-unlabelled" "show-no-axes" "show-x-axis" "show-x-axis-unlabelled"
       "cursor-in-view" "cursor-on-left" "cursor-on-right" "cursor-in-middle" "keyboard-no-action" "fourier-transform"
       "wavelet-transform" "haar-transform" "cepstrum" "hadamard-transform" "walsh-transform" "autocorrelation" "dont-normalize"
       "normalize-by-channel" "normalize-by-sound" "normalize-globally" "current-edit-position" "channels-separate"
       "channels-combined" "channels-superimposed" "speed-control-as-float" "speed-control-as-ratio" "speed-control-as-semitone"
       "enved-amplitude" "enved-spectrum" "enved-srate" "envelope-linear" "envelope-exponential" "enved-add-point"
       "enved-delete-point" "enved-move-point" "time-graph" "transform-graph" "lisp-graph" "copy-context" "cursor-context"
       "selection-context" "mark-context" "mus-interp-all-pass" "mus-interp-bezier" "mus-interp-hermite" "mus-interp-lagrange"
       "mus-interp-linear" "mus-interp-none" "mus-interp-sinusoidal"
       "sync-none" "sync-all" "sync-by-sound"))
    h))


(define (without-dollar-sign str)
  (if (char=? (string-ref str 0) #\$)
      (substring str 1)
      str))

(define (creation-date)
  (strftime "%d-%b-%y %H:%M %Z" (localtime (current-time))))

(define (alphanumeric? c) (or (char-alphabetic? c) (char-numeric? c)))

(define (find-if pred lst)
  (cond ((null? lst) #f)
	((pred (car lst)) (car lst))
	(else (find-if pred (cdr lst)))))

(define* (make-ind name sortby indexed char)
  (vector name sortby indexed char))

(define-expansion (ind-name obj)    `(vector-ref ,obj 0))
(define-expansion (ind-sortby obj)  `(vector-ref ,obj 1))
(define-expansion (ind-char obj)    `(vector-ref ,obj 3))
(define ind-indexed (dilambda (lambda (obj) (vector-ref obj 2)) (lambda (obj val) (vector-set! obj 2 val))))


(define (remove-all item sequence)
  (map (lambda (x)
	 (if (eq? x item)
	     (values)
	     x))
       sequence))

(define (remove-one item sequence)
  (let ((got-it #f))
    (map (lambda (x)
	   (if (and (not got-it)
		    (eq? x item))
	       (begin
		 (set! got-it #t)
		 (values))
	       x))
	 sequence)))


(define (count-table commands)
  (do ((count 0 (+ count 1))
       (c (memq 'table commands) (memq 'table (cdr c))))
      ((not c) count)))

#|
(define (count-table commands)
  (let ((count 0))
    (for-each
     (lambda (c)
       (if (eq? c 'table)
	   (set! count (+ count 1))))
     commands)
    count))
|#


(define (string</* a b)
  (and (not (= (length b) 0))
       (or (= (length a) 0)
	   (string=? a b)
	   (if (char=? (string-ref a 0) #\*)
	       (string<? (if (char=? (string-ref b 0) #\*) ; both start with *
			     a (substring a 1))
			 b)
	       (string<? a (if (char=? (string-ref b 0) #\*) (substring b 1) b))))))


(define (clean-and-downcase-first-char str caps file)
  (if (char=? (str 0) #\|)
      ;; this is a main-index entry
      (let* ((colonpos (or (char-position #\: str) 
			   (format () "no : in ~A~%" str)))
	     (line (string-append "<a href=\"" 
				  (or file "") 
				  "#"
				  (substring str 1 colonpos) 
				  "\">" 
				  (substring str (+ colonpos 1)) 
				  "</a>")))
	(make-ind :name line 
		  :sortby (string-downcase (substring str (+ colonpos 1)))))

      (begin 
       (let ((def-pos (string-position " class=def" str)))
	 (when def-pos
	       ;(format () "str: ~S, def-pos: ~A~%" str def-pos)
	   (set! str (string-append "<a "
				    (if (char=? (str (+ def-pos 10)) #\n)
					(substring str (+ def-pos 10))
					(values "name=" (substring str (+ def-pos 14))))))))
       
       (let ((line (string-append "<a href=\"" 
				   (or file "") 
				   "#" 
				   (substring str 9))))
	 (let ((ipos (string-position "<em" line)))
	   (when ipos
	     (let ((ispos (string-position "</em>" line)))
	       (if (not ispos) 
		   (format () "<em...> but no </em> for ~A~%" str)
		   (set! line (string-append (substring line 0 ipos) 
					     (substring line (+ ipos 14) ispos) 
					     (substring line (+ ispos 5))))))))
	 
	 (let ((hpos (let ((start (string-position "<h" line)))
		       (and (integer? start)
			    (or (string-position "<h2>" line start) 
				(string-position "<h1>" line start) 
				(string-position "<h3>" line start) 
				(string-position "<h4>" line start))))))
	   (when hpos
	     (let ((hspos (let ((start (string-position "</h" line)))
			    (and (integer? start)
				 (or (string-position "</h2>" line start) 
				     (string-position "</h1>" line start) 
				     (string-position "</h3>" line start) 
				     (string-position "</h4>" line start))))))
	       (if (not hspos) 
		   (format () "<hn> but no </hn> for ~A~%" str)
		   (set! line (string-append (substring line 0 hpos) 
					     (substring line (+ hpos 4) hspos) 
					     (substring line (+ hspos 5))))))))
	 
	 (if (not (and caps     ; caps is the list of names with upper case chars from the make-index-1 invocation ("AIFF" for example) 
		       (do ((cap caps (cdr cap)))
			   ((or (null? cap) 
				(string-position (car cap) line))
			    (pair? cap)))))
	     ;; find the first character of the >name< business and downcase it
	     (let ((bpos (+ 1 (char-position #\> line))))
	       (set! (line bpos) (char-downcase (line bpos)))))
	 
	 (let ((bpos (char-position #\> line))
	       (epos (or (string-position "</a>" line) 
			 (string-position "</em>" line) 
			 (string-position "</A>" line))))
	   (make-ind :name line 
		     :sortby (string-downcase (substring line (+ bpos 1) epos))))))))


(define (create-general str file)
  (let* ((mid (char-position #\: str))
	 (name (string-append "<a href=\"" 
			      (or file "") 
			      "#" 
			      (substring str 0 mid) 
			      "\"><b>" 
			      (substring str (+ mid 1)) 
			      "</b></a>")))
    (make-ind :name name
	      :sortby (string-downcase (substring str (+ mid 1))))))


(define (scheme->ruby scheme-name)
  (cond ((assoc scheme-name
		'(("frame*"        . "frame_multiply")
		  ("frame+"        . "frame_add")
		  ("float-vector*" . "float-vector_multiply")
		  ("float-vector+" . "float-vector_add")
		  ("mixer*"        . "mixer_multiply")
		  ("mixer+"        . "mixer_add")
		  ("redo"          . "redo_edit")
		  ("in"            . "call_in"))
		string=?) => cdr)
	(else
	 (let* ((len (length scheme-name))
		(var-case (hash-table-ref scheme-variable-names scheme-name))
		(strlen (if var-case (+ len 1) len)))
	   (let ((rb-name (make-string strlen #\space))
		 (i 0)
		 (j 0))
	     (if var-case
		 (begin
		   (set! (rb-name 0) #\$)
		   (set! j 1))
		 (if (hash-table-ref scheme-constant-names scheme-name)
		     (begin
		       (set! (rb-name 0) (char-upcase (scheme-name 0)))
		       (set! i 1)
		       (set! j 1))))
	     (do ()
		 ((>= i len))
	       (let ((c (scheme-name i)))
		 (if (or (alphanumeric? c)
			 (memv c '(#\? #\!)))
		     (begin
		       (set! (rb-name j) c)
		       (set! i (+ i 1))
		       (set! j (+ j 1)))
		     (if (and (char=? c #\-)
			      (char=? (scheme-name (+ i 1)) #\>))
			 (begin
			   (set! (rb-name j) #\2)
			   (set! j (+ j 1))
			   (set! i (+ i 2)))
			 (begin
			   (set! (rb-name j) #\_)
			   (set! i (+ i 1))
			   (set! j (+ j 1)))))))
	     (if (= j strlen)
		 rb-name
		 (substring rb-name 0 j)))))))

(define (clean-up-xref xref file)
  (let* ((len (length xref))
	 (outstr (make-string (* len 2) #\space)))
    (let ((url-str "")
	  (j 0))
      (do ((loc 0))
	  ((>= loc len))
	(let* ((leof (or (char-position #\newline xref loc) len))	    
	       (href (let* ((href-normal-start (or (string-position "<a href=" xref loc)
						   (string-position "<A HREF=" xref loc)))
			    (href-start (or href-normal-start 
					    (string-position "<a class=quiet href=" xref loc)
					    (string-position "<a class=def href=" xref loc))))
		       (let ((href-len (if href-normal-start 8 20))
			     (href-end (and (integer? href-start)
					    (< href-start leof)
					    (char-position #\> xref (+ href-start 1)))))
			 (and (integer? href-end)
			      (substring xref (+ href-start href-len) href-end))))))
	  (set! url-str (string-append url-str
				       (if href
					   (if (char=? (href 1) #\#)
					       (values "\"" file (substring href 1) (format #f ",~%  "))
					       (values href (format #f ",~%  ")))
					   (format #f "NULL,~%  "))))
	  (set! loc (+ leof 1))))

      (set! (outstr j) #\")
      (set! j (+ j 1))
      (do ((need-start #f)
	   (in-bracket #f)
	   (in-href #f)
	   (in-name #f)
	   (i 0 (+ i 1)))
	  ((= i len))
	(let ((c (xref i)))
	  (if in-bracket
	      (if (char=? c #\>)
		  (begin
		    (set! in-bracket #f)
		    (if in-href
			(set! in-name #t))
		    (set! in-href #f)))
	      (case c
		((#\<)
		 (if in-name
		     (begin
		       (set! (outstr j) #\})
		       (set! j (+ j 1))
		       (set! in-name #f)))
		 (set! in-bracket #t)
		 (if (or (and (< (+ i 7) len) 
			      (string=? "<a href" (substring xref i (+ i 7))))
			 (and (< (+ i 17) len) 
			      (string=? "<a class=def href" (substring xref i (+ i 17))))
			 (and (< (+ i 19) len) 
			      (string=? "<a class=quiet href" (substring xref i (+ i 19)))))
		     (begin
		       (if need-start
			   (begin
			     (set! (outstr j) #\,)
			     (set! (outstr (+ j 1)) #\newline)
			     (set! (outstr (+ j 2)) #\space)
			     (set! (outstr (+ j 3)) #\space)
			     (set! (outstr (+ j 4)) #\")
			     (set! j (+ j 5))
			     (set! need-start #f)))
		       (set! in-href #t)
		       (set! (outstr j) #\{)
		       (set! j (+ j 1)))))
		
		((#\")
		 (set! (outstr j) #\\)
		 (set! j (+ j 1))
		 (set! (outstr j) c)
		 (set! j (+ j 1)))
		
		((#\&)
		 (if (and (< (+ i 4) len) 
			  (string=? (substring xref i (+ i 4)) "&gt;"))
		     (begin
		       (set! (outstr j) #\>)
		       (set! j (+ j 1))
		       (set! i (+ i 3))))) ; incf'd again below
		
		((#\newline)
		 (set! (outstr j) #\")
		 (set! j (+ j 1))
		 (set! need-start #t))
		
		(else
		 (if need-start
		     (begin
		       (set! (outstr j) #\,)
		       (set! (outstr (+ j 1)) #\newline)
		       (set! (outstr (+ j 2)) #\space)
		       (set! (outstr (+ j 3)) #\space)
		       (set! (outstr (+ j 4)) #\")
		       (set! j (+ j 5))
		       (set! need-start #f)))
		 (set! (outstr j) c)
		 (set! j (+ j 1))))
	      )))
      (list 
       (substring outstr 0 j)
       url-str))))
  
  
;;; --------------------------------------------------------------------------------
;;; get indexer.data

(define-macro (provide sym) ; this picks up most of the (provide ...) statements for the autoloader
  `(define (,(cadr sym)) ,sym))

(load "ws.scm")
					;(load "clm23.scm")		   
					;(load "edit123.scm")		   
					;(load "snd11.scm")
(load "analog-filter.scm")	   
					; (load "new-backgrounds.scm")
(load "animals.scm")		   
					; (load "new-effects.scm")
(load "autosave.scm")	   
(load "noise.scm")
(load "binary-io.scm")
					;(load "bess1.scm")		   
(load "nrev.scm")
					;(load "bess.scm")		  
(load "numerics.scm")
(load "big-gens.scm")	  
(load "bird.scm")		   
(load "clean.scm")	
(load "peak-phases.scm")
(load "piano.scm")
(load "clm-ins.scm")		   
(load "play.scm")
(load "dlocsig.scm")		   
(load "poly.scm")
(load "draw.scm")		  
(load "dsp.scm")		   
(load "prc95.scm")
					; (load "edit-menu.scm")	   
(load "primes.scm")
					; (load "effects-utils.scm")	   
(load "pvoc.scm")
(load "enved.scm")		   
(load "rgb.scm")
(load "env.scm")		   
(load "examp.scm")		   
(load "rubber.scm")
(load "expandn.scm")		   
					;(load "s7-slib-init.scm")
(load "extensions.scm")	   
					;(load "s7test.scm")
(load "fade.scm")		   
(load "selection.scm")
					; (load "fft-menu.scm")	   
(load "singer.scm")
					; (load "fmv.scm")		   
(load "freeverb.scm")	   
(load "fullmix.scm")		   
(load "generators.scm")	   
(load "grani.scm")		   
					; (load "gtk-effects.scm")	   
(load "snddiff.scm")
					; (load "gtk-effects-utils.scm")  
					; (load "snd-gl.scm")
					; (load "snd-gtk.scm")
(load "hooks.scm")		   
					;(load "sndlib-ws.scm")
(load "index.scm")		   
					; (load "snd-motif.scm")
(load "jcrev.scm")		   
					;(load "snd-test.scm")
(load "jcvoi.scm")		   
(load "sndwarp.scm")
(load "tankrev.scm")
					; (load "special-menu.scm")
(load "maraca.scm")		   
(load "spectr.scm")
					; (load "marks-menu.scm")	   
(load "spokenword.scm")
(load "marks.scm")		   
(load "stochastic.scm")
(load "maxf.scm")		   
(load "strad.scm")
					; (load "misc.scm")		   
(load "v.scm")
(load "mix.scm")		   
(load "moog.scm")		   
					; (load "xm-enved.scm")
(load "musglyphs.scm")	   
(load "zip.scm")
(load "nb.scm")

(load "write.scm")
;(load "lint.scm")
(load "r7rs.scm")
(load "cload.scm")
(load "stuff.scm")
(load "mockery.scm")
;(load "repl.scm")
(load "profile.scm")

(let ((names (make-hash-table)))
  
  (define (apropos-1 e)
    (for-each
     (lambda (binding)
       (when (pair? binding)
	 (let ((symbol (car binding))
	       (value (cdr binding)))
	   (when (and (procedure? value)
		      (let? (funclet value)))
	     (let ((file (let ((addr (with-let (funclet value) __func__)))
			   ;; this misses scheme-side pws because their environment is (probably) the global env
			   (and (pair? addr)
				(pair? (cdr addr))
				(cadr addr)))))
	       (when (and file
			  (not (member file '("~/.snd_s7" "/home/bil/.snd_s7" "t.scm" "/home/bil/cl/t.scm" "make-index.scm" "/home/bil/cl/make-index.scm"))))
		 (let ((pos (char-position #\/ file)))
		   (if pos
		       (do ((k (char-position #\/ file (+ pos 1)) (char-position #\/ file (+ pos 1))))
			   ((not k)
			    (set! file (substring file (+ pos 1))))
			 (set! pos k)))
		   (let ((cur-names (hash-table-ref names file)))
		     (if cur-names
			 (if (not (memq symbol cur-names))
			     (hash-table-set! names file (cons symbol cur-names)))
			 (hash-table-set! names file (list symbol)))))))))))
     e))
  
  ;; handle the main macros by hand
  (for-each
   (lambda (symbol-and-file)
     (let ((symbol (car symbol-and-file))
	   (file (cadr symbol-and-file)))
       (if (not (file-exists? file))
	   (format *stderr* ";~S says it is in ~S which does not exist~%" symbol file))
       
       (let ((cur-names (hash-table-ref names file)))
	 (hash-table-set! names file (cons symbol (or cur-names ()))))))
   '((*libm* "libm.scm")
    (*libgdbm* "libgdbm.scm")
    (*libdl* "libdl.scm")
    (*libc* "libc.scm")
    (*libgsl* "libgsl.scm")
    (*repl* "repl.scm")
    (*rgb* "rgb.scm")
    (*spectr* "spectr.scm")
    (*lint* "lint.scm")

    (with-sound "ws.scm")
    (with-mixed-sound "ws.scm")
    (with-full-sound "ws.scm")
    (with-temp-sound "ws.scm")
    (with-marked-sound "ws.scm")
    (with-simple-sound "ws.scm")
    (sound-let "ws.scm")
    (definstrument "ws.scm")
    (defgenerator "generators.scm")
    
    (channel-sync "extensions.scm")
    (xe-envelope "xm-enved.scm")
    (*clm-srate* "ws.scm")
    (*clm-file-name* "ws.scm")
    (*clm-channels* "ws.scm")
    (*clm-sample-type* "ws.scm")
    (*clm-header-type* "ws.scm")
    (*clm-verbose* "ws.scm")
    (*clm-play* "ws.scm")
    (*clm-statistics* "ws.scm")
    (*clm-reverb* "ws.scm")
    (*clm-reverb-channels* "ws.scm")
    (*clm-reverb-data* "ws.scm")
    (*clm-table-size* "ws.scm")
    (*clm-file-buffer-size* "ws.scm")
    (*clm-locsig-type* "ws.scm")
    (*clm-clipped* "ws.scm")
    (*clm-array-print-length* "ws.scm")
    (*clm-player* "ws.scm")
    (*clm-notehook* "ws.scm")
    (*clm-with-sound-depth* "ws.scm")
    (*clm-delete-reverb* "ws.scm")
    (*to-snd* "ws.scm")
    (*clm-search-list* "ws.scm")
    (*definstrument-hook* "ws.scm")

    ;; these are hidden in defgenerator
    (make-abcos "generators.scm")
    (make-absin "generators.scm")
    (make-adjustable-oscil "generators.scm")
    (make-adjustable-sawtooth-wave "generators.scm")
    (make-adjustable-square-wave "generators.scm")
    (make-adjustable-triangle-wave "generators.scm")
    (make-asyfm "generators.scm")
    (make-bess "generators.scm")
    (make-blackman "generators.scm")
    (make-brown-noise "generators.scm")
    (make-dblsum "generators.scm")
    (make-eoddcos "generators.scm")
    (make-ercos "generators.scm")
    (make-erssb "generators.scm")
    (make-exponentially-weighted-moving-average "generators.scm")
    (make-flocsig "generators.scm")
    (make-fmssb "generators.scm")
    (make-green-noise "generators.scm")
    (make-green-noise-interp "generators.scm")
    (make-izcos "generators.scm")
    (make-j0evencos "generators.scm")
    (make-j0j1cos "generators.scm")
    (make-j2cos "generators.scm")
    (make-jjcos "generators.scm")
    (make-jncos "generators.scm")
    (make-jpcos "generators.scm")
    (make-jycos "generators.scm")
    (make-k2cos "generators.scm")
    (make-k2sin "generators.scm")
    (make-k2ssb "generators.scm")
    (make-k3sin "generators.scm")
    (make-krksin "generators.scm")
    (make-moving-autocorrelation "generators.scm")
    (make-moving-fft "generators.scm")
    (make-moving-length "generators.scm")
    (make-moving-pitch "generators.scm")
    (make-moving-rms "generators.scm")
    (make-moving-scentroid "generators.scm")
    (make-moving-spectrum "generators.scm")
    (make-moving-sum "generators.scm")
    (make-moving-variance "generators.scm")
    (make-n1cos "generators.scm")
    (make-nchoosekcos "generators.scm")
    (make-ncos2 "generators.scm")
    (make-ncos4 "generators.scm")
    (make-nkssb "generators.scm")
    (make-noddcos "generators.scm")
    (make-noddsin "generators.scm")
    (make-noddssb "generators.scm")
    (make-noid "generators.scm")
    (make-npcos "generators.scm")
    (make-nrcos "generators.scm")
    (make-nrsin "generators.scm")
    (make-nrssb "generators.scm")
    (make-nsincos "generators.scm")
    (make-nssb "generators.scm")
    (make-nxy1cos "generators.scm")
    (make-nxy1sin "generators.scm")
    (make-nxycos "generators.scm")
    (make-nxysin "generators.scm")
    (make-pink-noise "generators.scm")
    (make-polyoid "generators.scm")
    (make-r2k!cos "generators.scm")
    (make-r2k2cos "generators.scm")
    (make-rcos "generators.scm")
    (make-rk!cos "generators.scm")
    (make-rk!ssb "generators.scm")
    (make-rkcos "generators.scm")
    (make-rkoddssb "generators.scm")
    (make-rksin "generators.scm")
    (make-rkssb "generators.scm")
    (make-round-interp "generators.scm")
    (make-rssb "generators.scm")
    (make-rxycos "generators.scm")
    (make-rxysin "generators.scm")
    (make-safe-rxycos "generators.scm")
    (make-sinc-train "generators.scm")
    (make-table-lookup-with-env "generators.scm")
    (make-tanhsin "generators.scm")
    (make-wave-train-with-env "generators.scm")
    (make-waveshape "generators.scm")
    (make-weighted-moving-average "generators.scm")
    (nssb? "generators.scm")
    (nxysin? "generators.scm")
    (nxycos? "generators.scm")
    (nxy1cos? "generators.scm")
    (nxy1sin? "generators.scm")
    (noddsin? "generators.scm")
    (noddcos? "generators.scm")
    (noddssb? "generators.scm")
    (ncos2? "generators.scm")
    (npcos? "generators.scm")
    (nrsin? "generators.scm")
    (nrcos? "generators.scm")
    (nrssb? "generators.scm")
    (nkssb? "generators.scm")
    (nsincos? "generators.scm")
    (n1cos? "generators.scm")
    (rcos? "generators.scm")
    (rssb? "generators.scm")
    (rxysin? "generators.scm")
    (rxycos? "generators.scm")
    (safe-rxycos? "generators.scm")
    (ercos? "generators.scm")
    (erssb? "generators.scm")
    (eoddcos? "generators.scm")
    (rkcos? "generators.scm")
    (rksin? "generators.scm")
    (rkssb? "generators.scm")
    (rk!cos? "generators.scm")
    (rk!ssb? "generators.scm")
    (r2k!cos? "generators.scm")
    (k2sin? "generators.scm")
    (k2cos? "generators.scm")
    (k2ssb? "generators.scm")
    (dblsum? "generators.scm")
    (rkoddssb? "generators.scm")
    (krksin? "generators.scm")
    (abcos? "generators.scm")
    (absin? "generators.scm")
    (r2k2cos? "generators.scm")
    (asyfm? "generators.scm")
    (bess? "generators.scm")
    (jjcos? "generators.scm")
    (j0evencos? "generators.scm")
    (j2cos? "generators.scm")
    (jpcos? "generators.scm")
    (jncos? "generators.scm")
    (j0j1cos? "generators.scm")
    (jycos? "generators.scm")
    (blackman "generators.scm")
    (blackman? "generators.scm")
    (fmssb? "generators.scm")
    (k3sin? "generators.scm")
    (izcos? "generators.scm")
    (adjustable-square-wave? "generators.scm")
    (adjustable-triangle-wave? "generators.scm")
    (adjustable-sawtooth-wave? "generators.scm")
    (adjustable-oscil? "generators.scm")
    (round-interp? "generators.scm")
    (nchoosekcos? "generators.scm")
    (sinc-train? "generators.scm")
    (pink-noise? "generators.scm")
    (brown-noise? "generators.scm")
    (green-noise? "generators.scm")
    (green-noise-interp? "generators.scm")
    (moving-sum? "generators.scm")
    (moving-variance? "generators.scm")
    (moving-rms? "generators.scm")
    (moving-length? "generators.scm")
    (weighted-moving-average? "generators.scm")
    (exponentially-weighted-moving-average? "generators.scm")
    (polyoid "generators.scm")
    (polyoid-tn "generators.scm")
    (polyoid-un "generators.scm")
    (noid "generators.scm")
    (waveshape? "generators.scm")
    (waveshape "generators.scm")
    (tanhsin? "generators.scm")
    (moving-fft? "generators.scm")
    (moving-spectrum? "generators.scm")
    (moving-scentroid? "generators.scm")
    (moving-autocorrelation? "generators.scm")
    (moving-pitch? "generators.scm")
    (flocsig? "generators.scm")

    (vector-for-each "r7rs.scm")
    (string-for-each "r7rs.scm")
    (list-copy "r7rs.scm")
    (r7rs-string-fill! "r7rs.scm")
    (char-foldcase "r7rs.scm")
    (exact-integer? "r7rs.scm")
    (inexact "r7rs.scm")
    (exact "r7rs.scm")
    (truncate-quotient "r7rs.scm")
    (truncate-remainder "r7rs.scm")
    (floor-remainder "r7rs.scm")
    (open-binary-input-file "r7rs.scm")
    (open-binary-output-file "r7rs.scm")
    (bytevector-length "r7rs.scm")
    (write-bytevector "r7rs.scm")
    (open-input-bytevector "r7rs.scm")
    (open-output-bytevector "r7rs.scm")
    (read-u8 "r7rs.scm")
    (write-u8 "r7rs.scm")
    (u8-ready? "r7rs.scm")
    (peek-u8 "r7rs.scm")
    (write-simple "r7rs.scm")
    (raise "r7rs.scm")
    (raise-continuable "r7rs.scm")
    (interaction-environment "r7rs.scm")
    (jiffies-per-second "r7rs.scm")
    (current-jiffy "r7rs.scm")
    (current-second "r7rs.scm")
    (get-environment-variable "r7rs.scm")
    (get-environment-variables "r7rs.scm")
    (r7rs-file-exists? "r7rs.scm")
    (os-type "r7rs.scm")
    (cpu-architecture "r7rs.scm")
    (machine-name "r7rs.scm")
    (os-version "r7rs.scm")
    (implementation-name "r7rs.scm")
    (implementation-version "r7rs.scm")
    (box-type? "r7rs.scm")
    (make-box-type "r7rs.scm")
    (define-library "r7rs.scm")
    (define-record-type "r7rs.scm")
    (define-values "r7rs.scm")
    
    (c?r "stuff.scm")
    (do* "stuff.scm")
    (typecase "stuff.scm")
    (enum "stuff.scm")
    (while "stuff.scm")
    (let*-temporarily "stuff.scm")
    (define-class "stuff.scm")
    (elambda "stuff.scm")
    (value->symbol "stuff.scm")
    (progv "stuff.scm")
    (reflective-let "stuff.scm")
    (reflective-probe "stuff.scm")
    (reactive-let "stuff.scm")
    (reactive-let* "stuff.scm")
    (reactive-lambda* "stuff.scm")
    (pretty-print "write.scm")
    (fully-macroexpand "stuff.scm")
    (null-environment "stuff.scm")
    (*mock-vector* "mockery.scm")
    (*mock-port* "mockery.scm")
    (*mock-symbol* "mockery.scm")
    (*mock-pair* "mockery.scm")
    (*mock-number* "mockery.scm")
    (*mock-char* "mockery.scm")
    (*mock-string* "mockery.scm")
    (*mock-hash-table* "mockery.scm")

    (lint "lint.scm")
    (html-lint "lint.scm")

    (c-define "cload.scm")

    (moog? "moog.scm")
    (make-moog "moog.scm")

    (snd-clm23.scm "clm23.scm")
    (snd-edit123.scm "edit123.scm")
;    (snd-snd11.scm "snd11.scm")
    (snd-new-backgrounds.scm "new-backgrounds.scm")
    (snd-new-effects.scm "new-effects.scm")
    (snd-bess1.scm "bess1.scm")
    (snd-bess.scm "bess.scm")
    (snd-edit-menu.scm "edit-menu.scm")
    (snd-effects-utils.scm "effects-utils.scm")
    (snd-fft-menu.scm "fft-menu.scm")
    (snd-fmv.scm "fmv.scm")
    (snd-gtk-effects.scm "gtk-effects.scm")
    (snd-gtk-effects-utils.scm "gtk-effects-utils.scm")
    (snd-snd-gl.scm "snd-gl.scm")
    (snd-snd-gtk.scm "snd-gtk.scm")
    (snd-snd-motif.scm "snd-motif.scm")
    (snd-special-menu.scm "special-menu.scm")
    (snd-marks-menu.scm "marks-menu.scm")
    (snd-misc.scm "misc.scm")
    (snd-xm-enved.scm "xm-enved.scm")

    (snd-ws.scm "ws.scm")
    (sndlib-ws.scm "sndlib-ws.scm")
    ))

  ;; still missing (if not above):
  ;;  copy-sample-reader free-sample-reader make-mix-sample-reader make-region-sample-reader make-sample-reader 
  ;;  mix-sample-reader? region-sample-reader? sample-reader-at-end? sample-reader-home sample-reader? sample-reader-position 
  ;;  spectro-cutoff spectro-end clear-selection cursor-follows-play 
  ;;  penv? make-penv 
  ;;  big-ring-modulate big-oscil? make-big-oscil big-ncos? make-big-ncos big-nsin? make-big-nsin big-table-lookup? 
  ;;  make-big-table-lookup big-one-zero? make-big-one-zero big-one-pole? make-big-one-pole 
  ;;  hilbert-transform highpass lowpass bandpass bandstop differentiator butter channel-lp-inf savitzky-golay-filter 
  ;;  stop-dac dlocsig array-ref sound-comment 


  ;; alternate: (autoload sym (lambda (e) (let ((m (load file))) (varlet (rootlet) (cons sym (m sym))))))
  (for-each
   (lambda (sym&file)
     (let* ((file (cadr sym&file))
	    (ce (let ((e (car sym&file)))
		  (if (not (defined? e)) (load file) (symbol->value e))))
	    (flst (or (hash-table-ref names file) ())))
       (for-each
	(lambda (slot)
	  (let ((name (car slot)))
	    (if (not (defined? name))
		(set! flst (cons name flst)))))
	ce)
       (hash-table-set! names file flst)))
   '((*libm* "libm.scm")
     (*libgdbm* "libgdbm.scm")
     (*libdl* "libdl.scm")
     (*libc* "libc.scm")
     (*libgsl* "libgsl.scm")))

  (apropos-1 (rootlet))
  
  (let ((syms ())
	(size 0))
    (call-with-output-file "indexer.data"
      (lambda (p)
	(let ((hti (make-iterator names)))
	  (do ((ns (iterate hti) (iterate hti)))
	      ((eof-object? ns))
	    (let ((file (car ns))
		  (symbols (cdr ns)))
	      (set! size (+ size (length symbols)))
	      (for-each
	       (lambda (symbol)
		 (set! syms (cons (cons (symbol->string symbol) file) syms)))
	       symbols))))
	(set! syms (sort! syms (lambda (a b) (string<? (car a) (car b)))))
	(format p "~%static const char *snd_names[~D] = {" (* size 2))
	(for-each
	 (lambda (sf)
	   (format p "~%    ~S, ~S," (car sf) (cdr sf)))
	 syms)
	(format p "~%};~%")
	(format p "~%static void autoload_info(s7_scheme *sc)~%{~%  s7_autoload_set_names(sc, snd_names, ~D);~%}~%" size)))
    ))



;;; --------------------------------------------------------------------------------
;;; make-index 


(define (make-vector-name str)
  (let ((pos (char-position #\space str)))
    (if (not pos)
	str
	(let ((len (length str)))
	  (string-set! str pos #\_)
	  (do ((i (+ pos 1) (+ i 1)))
	      ((= i len) str)
	    (if (char=? (string-ref str i) #\space)
		(string-set! str i #\_)))))))

(define ids (make-hash-table))
(define n-array-length 2048)
(define g-array-length 128)

(define* (make-index-1 file-names (output "test.html") (cols 3) capitalized no-bold with-scm)
  ;; read html file, gather all names, create index (in lower-case, *=space in sort)
  (let ((n 0)
	(g 0)
	(xrefing #f)
	(current-general 0)
	(got-tr #f)
	(topic #f)
	(xrefs (make-vector g-array-length #f))
	(generals (make-vector g-array-length #f))
	(topics (make-vector n-array-length #f))
	(gfiles (make-vector g-array-length #f))
	(files (make-vector n-array-length #f))
	(names (make-vector n-array-length #f))
	(local-ids ())
	)
    (fill! ids ())
    
    (do ((file file-names (cdr file)))
	((null? file))
      (set! local-ids ())
      (call-with-input-file (car file)
        (lambda (f)
	  (do ((line (read-line f) (read-line f)))
	      ((eof-object? line))
	    (when (char-position #\< line)
	      (let* ((dline line)
		     (compos (string-position "<!-- INDEX" dline))
		     (indpos (string-position "<!-- main-index" dline))
		     (pos (and (not compos) 
			       (not indpos)
			       (string-position "<em class=def id=" dline))))
		(let ((xpos (string-position "<TABLE " dline))
		      (tpos (and (not pos) 
				 (string-position "<!-- TOPIC " line))))
		  (if (string-position "</TABLE>" dline)
		      (set! xrefing #f))
		  
		  (let ((id-pos (string-position " id=" dline)))
		    (if id-pos
			(let ((sym-name (let ((start (- (char-position #\" dline id-pos) id-pos)))
					  (string->symbol
					   (substring dline 
						      (+ id-pos start 1)
						      (let ((end-start (+ id-pos start 2)))	   
							(- (+ id-pos start (char-position #\" dline end-start) 2) end-start)))))))
			  (if (not (hash-table-ref ids sym-name))
			      (hash-table-set! ids sym-name 0)
			      (if (memq sym-name local-ids)
				  (format () "~S: id ~S is set twice~%" file sym-name)))
			  (set! local-ids (cons sym-name local-ids)))))
		
		  (cond (tpos
			 (let ((epos (string-position " -->" dline)))
			   (if (not epos) 
			       (format () "<!-- TOPIC but no --> for ~A~%" dline)
			       (set! topic (substring dline (+ tpos 11) epos)))))
			(compos
			 (let ((epos (string-position " -->" dline)))
			   (if (not epos) 
			       (format () "<!-- INDEX but no --> for ~A~%" dline)
			       (when (or (not no-bold)
					 with-scm)
				 (set! current-general g)
				 (set! (generals g) (substring dline (+ compos 11) epos))
				 (set! (gfiles g) (car file))
				 (set! (xrefs g) "")
				 (set! g (+ g 1))))))
			(indpos
			 (let ((epos (string-position " -->" dline)))
			   (if (not epos) 
			       (format () "<!-- main-index but no --> for ~A~%" dline)
			       (when (or (not no-bold)
					 with-scm)
				 (set! (names n) (substring dline (+ indpos 16) epos))
				 (set! (files n) (car file))
				 (set! n (+ n 1))))))
			(xpos
			 (set! xrefing #t))
			(else
			 (do ()
			     ((not pos))
			   (set! dline (substring dline pos))
			   (let ((epos (or (string-position "</a>" dline) 
					   (string-position "</em>" dline) 
					   (string-position "</A>" dline))))
			     (if (not epos) 
				 (format () "<a> but no </a> for ~A~%" dline)
				 (begin
				   (set! (names n) (string-append (substring dline 0 epos) "</a>"))
				   (set! (files n) (car file))
				   (set! (topics n) topic)
				   (set! n (+ n 1))
				   (set! dline (substring dline (+ epos 4)))
				   (set! pos (string-position "<em class=def id=" dline))))))))
		  (if (and xrefing
			   (or (not (char=? (dline 0) #\<))
			       (string-position "<a href" dline)
			       (string-position "<a class=quiet href" dline)
			       (string-position "<a class=def href" dline)))
		      (set! (xrefs current-general) (string-append (xrefs current-general) dline (format #f "~%"))))
		  (when (and topic
			     (string-position "<hr>" dline))
		    (set! topic #f)))))))))
    ;; end call-with-input-file loop

    (let ((tnames (make-vector (+ n g)))
	  (ctr 0))
      (do ((i 0 (+ i 1)))
	  ((= i n))
        (set! (tnames ctr)
	      (clean-and-downcase-first-char (names i) capitalized (files i)))
#|	
	;; catch forgotten stuff
	(let ((old-name (ind-sortby (tnames i))))
	  (when (not (char-position " ,(" old-name))
	    (let ((&pos (char-position #\& old-name)))
	      (when &pos
		(set! old-name (string-append (substring old-name 0 &pos) (string #\>) (substring old-name (+ &pos 4)))))
	      (set! old-name (string->symbol old-name))
	      (unless (or (memq old-name '(list->float-vector float-vector-fill! float-vector-length float-vector-reverse! float-vector->list ;vct.c but not s7
					   min-db filter-control-in-db enved-in-db ; -dB in s7
					   init-ladspa list-ladspa analyse-ladspa ladspa-descriptor apply-ladspa
					   bignum? bignum))
			  (defined? old-name))
		  (format *stderr* "??: ~A~%" old-name)))))
|#
	(if (positive? (length (ind-sortby (tnames ctr))))
	    (set! ctr (+ ctr 1))))

      (unless (= ctr n)
        (set! n ctr)
	(set! tnames (copy tnames (make-vector n))))
      
      (when (positive? g)
	(if (< (length tnames) (+ g n))
	    (set! tnames (copy tnames (make-vector (+ g n) #f))))

	(do ((i 0 (+ i 1)))
	    ((= i g))
	  (set! (tnames (+ i n))
		(create-general (generals i) (gfiles i))))
	(set! n (+ n g)))

      (set! tnames (sort! tnames (lambda (a b)
				   (string</* (ind-sortby a) 
					      (ind-sortby b)))))

      (let ((len (length tnames)))
	(let ((new-names (make-vector (+ len 100)))
	      (j 0)
	      (last-char #f))
	  (do ((i 0 (+ i 1)))
	      ((= i len))
	    (let ((name (tnames i)))
	      (if (and name
		       (ind-sortby name))
		  (let ((this-char ((ind-sortby name) 0)))
		    (if (char=? this-char #\*)
			(set! this-char ((ind-sortby name) 1)))
		    (if (and last-char
			     (not (char-ci=? last-char this-char)))
			(begin
			 (set! (new-names j) (make-ind :name #f :sortby #f))
			 (set! j (+ j 1))
			 (set! (new-names j) (make-ind :name "    " 
						       :char (char-upcase this-char)
						       :sortby #f))
			 (set! j (+ j 1))
			 (set! (new-names j) (make-ind :name #f :sortby #f))
			 (set! j (+ j 1))))
		    (set! (new-names j) name)
		    (set! j (+ j 1))
		    (set! last-char this-char)))))
	  
	    (set! tnames new-names)
	    (set! n j)))
      
      (call-with-output-file output
        (lambda (ofil)
	  (format ofil "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=utf-8\" >
<title>Snd Index</title>
<style type=\"text/css\">
	EM.red {color:red; font-style:normal}
        EM.typing {color:maroon; font-style: normal}
        EM.listener {color:darkblue; font-style: normal}
        EM.tab {font-size: small; font-style: normal}
	EM.def {font-weight: bold; font-style: normal}
	H1 {text-align: center}
	UL {list-style-type: none}
        DIV.centered {text-align: center}

	A {text-decoration:none}
	A:hover {text-decoration:underline}
	A.quiet {color:black; text-decoration:none}
	A.quiet:hover {text-decoration:underline}

        TD.green {background-color: lightgreen}
	TD.beige {background-color: beige}
        DIV.topheader {margin-top: 10px;
	            margin-bottom: 40px;
	            border: 4px solid #00ff00; /* green */
		    background-color: #f5f5dc; /* beige */
		    font-family: 'Helvetica';
		    font-size: 30px;
		    text-align: center;
		    padding-top: 10px;
		    padding-bottom: 10px;
	           }
        BODY.body {background-color: #ffffff;    /* white */
	           /* margin-left: 0.5cm; */
                   }
</style>
</head>
<body class=\"body\">
<div class=\"topheader\">Index</div>
<!-- created ~A -->~%" (creation-date))
		      
	  (format ofil "<table>~%  <tr>")
	  (set! got-tr #t)
	  (do ((row 0)
	       (ctr 0)
	       (offset (ceiling (/ n cols)))
	       (i 0 (+ i 1)))
	      ((= row offset))
	    (let ((x (+ row (* ctr offset))))
	      (if (>= x n)
		  (format ofil "~%")
		  (let ((name (tnames x)))
		    (format ofil 
			    "<td~A>~A~A~A</td>" 
			    (if (or (not (ind-name name))
				    (ind-sortby name))
				""
				" class=\"green\"")
			    (if (ind-char name)
				"<div class=\"centered\">"
				"<em class=tab>")
			    (or (ind-char name)
				(ind-name name)
				"    ")
			    (if (ind-char name)
				"</div>"
				"</em>")
			    )
		    (if (ind-indexed name) 
			(format () "~A indexed twice~%" (ind-name name)))
		    (set! (ind-indexed name) #t))))
	    (set! ctr (+ ctr 1))
	    (when (< ctr cols)
	      (format ofil "<td></td>"))
	    
	    (when (= ctr cols)
	      (if got-tr (begin (format ofil "</tr>~%") (set! got-tr #f)))
	      (set! row (+ row 1))
	      (if (< i n) (begin (format ofil "  <tr>") (set! got-tr #t)))
	      (set! ctr 0)))
	  (format ofil "~%</table>~%</body></html>~%")))
      ;; end output

      (do ((i 0 (+ i 1)))
	  ((= i n))
	(if (not (ind-indexed (tnames i)))
	    (format () "unindexed: ~A (~A)~%" (ind-name (tnames i)) i)))

      (do ((i 0 (+ i 1)))
	  ((= i (- n 1)))
	(let ((n1 (tnames i))
	      (n2 (tnames (+ i 1))))
	  (if (and (string? (ind-sortby n1))
		   (string? (ind-sortby n2))
		   (string=? (ind-sortby n1) (ind-sortby n2))
		   (string=? (ind-name n1) (ind-name n2)))
	      (format () "duplicated name: ~A (~A ~A)~%" (ind-sortby n1) (ind-name n1) (ind-name n2)))))
      
      (when with-scm
	(call-with-output-file "test.c"
	  (lambda (sfil)
	    (let ((help-names ())
		  (help-urls ()))
	      (format sfil "/* Snd help index (generated by make-index.scm) */~%")
	      (do ((i 0 (+ i 1)))
		  ((= i n))
		(if (and (tnames i)
			 (ind-sortby (tnames i)))
		    (let* ((line (substring (ind-name (tnames i)) 8))
			   (dpos (char-position #\> line))
			   (ind (substring line (+ dpos 1) (char-position #\< line))))
		      (let ((url (substring line 1 (- dpos 1)))
			    (gpos (string-position "&gt;" ind)))
			(if gpos 
			    (set! ind (string-append (substring ind 0 gpos) 
						     ">" 
						     (substring ind (+ gpos 4)))))
			(when (positive? (length ind))
			  (set! help-names (cons ind help-names))
			  (set! help-urls (cons url help-urls)))))))
	      
	      (set! help-names (reverse help-names))
	      (set! help-urls (reverse help-urls))
	      
	      (format sfil "#define HELP_NAMES_SIZE ~D~%" (length help-names))
	      (format sfil "#if HAVE_SCHEME || HAVE_FORTH~%")
	      (format sfil "static const char *help_names[HELP_NAMES_SIZE] = {~%  ")
	      (format sfil "~S" (car help-names))
	      (do ((ctr 1 (+ ctr 1))
		   (lname (cdr help-names) (cdr lname)))
		  ((null? lname))
		(format sfil (if (= (modulo ctr 6) 0) ",~% ~S" ", ~S") (car lname)))
	      (format sfil "};~%")
	      
	      (format sfil "#endif~%#if HAVE_RUBY~%")
	      (format sfil "static const char *help_names[HELP_NAMES_SIZE] = {~%  ")
	      (format sfil "~S" (car help-names))
	      (do ((ctr 1 (+ ctr 1))
		   (lname (cdr help-names) (cdr lname)))
		  ((null? lname))
		(let ((name (car lname)))
		  (format sfil (if (= (modulo ctr 6) 0) ",~% ~S" ", ~S") (without-dollar-sign (scheme->ruby name)))))
	      
	      (format sfil "};~%#endif~%")
	      (format sfil "#if (!HAVE_EXTENSION_LANGUAGE)~%static const char **help_names = NULL;~%#endif~%")
	      (format sfil "static const char *help_urls[HELP_NAMES_SIZE] = {~%  ")
	      (format sfil "~S" (car help-names))
	      
	      (do ((ctr 1 (+ ctr 1))
		   (lname (cdr help-urls) (cdr lname)))
		  ((null? lname))
		(let ((url (car lname)))
		  (format sfil (if (= (modulo ctr 4) 0) ",~% ~S" ", ~S") url)))
	      (format sfil "};~%")
	      
	      (do ((i 0 (+ i 1)))
		  ((= i g))
		(if (and (xrefs i)
			 (> (length (xrefs i)) 1))
		    (let ((vals (clean-up-xref (xrefs i) (gfiles i))))
		      (format sfil "~%static const char *~A_xrefs[] = {~%  ~A,~%  NULL};~%"
			      (let* ((str (generals i))
				     (mid (char-position #\: str)))
				(make-vector-name (substring str (+ mid 1))))
			      (car vals))
		      (format sfil "~%static const char *~A_urls[] = {~%  ~ANULL};~%"
			      (let* ((str (generals i))
				     (mid (char-position #\: str)))
				(make-vector-name (substring str (+ mid 1))))
			      (cadr vals)))))
	      
	      (format sfil "~%~%#if HAVE_SCHEME~%"))))

	(system "cat indexer.data >> test.c") 
	(system "echo '#endif\n\n' >> test.c")))))


;;; --------------------------------------------------------------------------------
;;; html-check

;;; (html-check '("sndlib.html" "snd.html" "sndclm.html" "extsnd.html" "grfsnd.html" "sndscm.html" "fm.html" "balance.html" "snd-contents.html" "s7.html"))

(define (html-check files)
  (let ((name 0)
	(href 0)
	(names (make-hash-table 2048))
	(closables (let ((h (make-hash-table)))
		     (for-each (lambda (c)
				 (set! (h c) #t))
			       '(ul tr td table small sub blockquote p details summary
				 a A i b title pre span h1 h2 h3 code body html
				 em head h4 sup map smaller bigger th tbody div))
		     h)))
    (for-each
     (lambda (file)
       (call-with-input-file file
	 (lambda (f)
	   (do ((linectr -1)
		(commands ())
		(comments 0)
		(openctr 0)
		(warned #f)
		(p-parens 0)
		(p-quotes 0)
		(p-curlys 0)
		(in-comment #f)
		(scripting #f)
		(line (read-line f) (read-line f)))
	       ((eof-object? line)
		(if (pair? commands) 
		    (format () "open directives at end of ~A: ~A~%" file commands)))
	     (set! linectr (+ linectr 1))
	     (let* ((len (length line))
		    (opos (and (positive? len)
			       (char-position "<>\"(){}&" line)))
		    (cpos (and (not opos)
			       in-comment
			       (string-position " -- " line))))
	       (when cpos
		 (format () "~A[~D]: possible -- in comment: ~A~%" file linectr line))
	       (when opos
		 ;; open/close html entities
		 (do ((i opos (or (char-position "<>\"(){}&" line (+ i 1)) len)))
		     ((>= i len))
		   (case (string-ref line i)
		     ((#\<)
		      (unless scripting
			(if (not (or (zero? openctr)
				     (positive? p-quotes)
				     in-comment))
			    (format () "~A[~D]: ~A has unclosed <?~%" file linectr line))
			(set! openctr (+ openctr 1))
			(if (and (< i (- len 3))
				 (char=? (line (+ i 1)) #\!)
				 (char=? (line (+ i 2)) #\-)
				 (char=? (line (+ i 3)) #\-))
			    (begin
			      (set! comments (+ comments 1))
			      (if (> comments 1)
				  (begin 
				    (format () "~A[~D]: nested <!--?~%" file linectr)
				    (set! comments (- comments 1))))
			      (set! in-comment #t)))
			(if (and (not in-comment)
				 (< i (- len 1))
				 (char=? (line (+ i 1)) #\space))
			    (format () "~A[~D]: '< ' in ~A?~%" file linectr line))))
		     ;; else c != <
		     
		     ((#\>)
		      (unless scripting
			(set! openctr (- openctr 1))
			(if (and (>= i 2)
				 (char=? (line (- i 1)) #\-)
				 (char=? (line (- i 2)) #\-))
			    (begin
			      (set! in-comment #f)
			      (set! comments (- comments 1))
			      (if (< comments 0)
				  (begin
				    (format () "~A[~D]: extra -->?~%" file linectr)
				    (set! comments 0))))
			    (if (not (or (zero? openctr)
					 (positive? p-quotes)
					 in-comment))
				(format () "~A[~D]: ~A has unmatched >?~%" file linectr line)))
			(set! openctr 0)
			(if (and (not in-comment)
				 (>= i 2)
				 (char=? (line (- i 1)) #\-)
				 (not (char=? (line (- i 2)) #\-))
				 (< i (- len 1))
				 (alphanumeric? (line (+ i 1))))
			    (format () "~A[~D]: untranslated '>': ~A~%" file linectr line))))
		     ;; else c != < or >
		     
		     ((#\()
		      (set! p-parens (+ p-parens 1)))
		     
		     ((#\)) 
		      (set! p-parens (- p-parens 1)))
		     
		     ((#\")
		      (if (or (= i 0)
			      (not (char=? (line (- i 1)) #\\)))
			  (set! p-quotes (+ p-quotes 1))))
		     
		     ((#\&) 
		      (if (and (not in-comment)
			       (case (string-ref line (+ i 1))
				 ((#\g) (not (string=? "&gt;" (substring line i (min len (+ i 4))))))
				 ((#\l) (not (or (string=? "&lt;" (substring line i (min len (+ i 4))))
						 (string=? "&lambda;" (substring line i (min len (+ i 8)))))))
				 ((#\a) (not (string=? "&amp;" (substring line i (min len (+ i 5))))))
				 ((#\q) (not (string=? "&quot;" (substring line i (min len (+ i 6))))))
				 ((#\o) (not (string=? "&ouml;" (substring line i (min len (+ i 6))))))
				 ((#\m) (not (member (substring line i (min len (+ i 7))) '("&mdash;" "&micro;") string=?)))
				 ((#\n) (not (string=? "&nbsp;" (substring line i (min len (+ i 6))))))
				 ((#\&) (not (string=? "&&" (substring line i (min len (+ i 2))))))
				 ((#\space) (not (string=? "& " (substring line i (min len (+ i 2)))))) ; following char -- should skip this
				 (else #t)))
			  (format () "~A[~D]: unknown escape sequence: ~A~%" file linectr line)))
		     
		     ((#\{) 
		      (set! p-curlys (+ p-curlys 1)))
		     
		     ((#\})
		      (set! p-curlys (- p-curlys 1)))))
		 
		 ;; end line scan
		 (unless in-comment
		   (let ((start #f)
			 (closing #f)
			 (pos (char-position #\< line)))
		     (when pos
		       (do ((i pos (or (char-position "</! >" line (+ i 1)) len)))
			   ((>= i len))
			 (case (string-ref line i)
			   ((#\space #\>)
			    (when start
			      (if closing
				  (let ((closer (string->symbol (substring line (+ start 2) i))))
				    (if (eq? closer 'TABLE) (set! closer 'table))
				    (cond ((memq closer '(center big font))
					   (format () "~A[~D]: ~A is obsolete, ~A~%" file linectr closer line))
					  
					  ((eq? closer 'script)
					   (set! scripting #f))
					  
					  (scripting)
					  
					  ((not (memq closer commands))
					   (format () "~A[~D]: ~A without start? ~A from [~D:~D] (commands: ~A)~%" 
						   file linectr closer line (+ start 2) i commands))
					  
					  ((not (hash-table-ref closables closer))
					   (set! commands (remove-all closer commands)))
					  
					  (else 
					   (if (not (eq? (car commands) closer))
					       (format () "~A[~D]: ~A -> ~A?~%" file linectr closer commands))
					   
					   (if (memq closer '(p td pre))
					       (begin
						 (if (odd? p-quotes)
						     (format () "~A[~D]: unmatched quote~%" file linectr))
						 (set! p-quotes 0)
						 (cond ((= p-curlys 1) 
							(format () "~A[~D]: extra '{'~%" file linectr))
						       ((= p-curlys -1) 
							(format () "~A[~D]: extra '}'~%" file linectr))
						       ((not (= p-curlys 0)) 
							(format () "~A[~D]: curlys: ~D~%" file linectr p-curlys)))
						 (set! p-curlys 0)
						 (cond ((= p-parens 1) 
							(format () "~A[~D]: extra '('~%" file linectr))
						       ((= p-parens -1) 
							(format () "~A[~D]: extra ')'~%" file linectr))
						       ((not (= p-parens 0)) 
							(format () "~A[~D]: parens: ~D~%" file linectr p-parens)))
						 (set! p-parens 0)))
					   
					   (set! commands (remove-one closer commands))
					   (when (and (not warned)
						      (eq? closer 'table)
						      (not (memq 'table commands)))
					     (if (memq 'tr commands)
						 (begin
						   (set! warned #t)
						   (set! commands (remove-all 'tr commands))
						   (format () "~A[~D]: unclosed tr at table (~A)~%" file linectr commands)))
					     (if (memq 'td commands)
						 (begin
						   (set! warned #t)
						   (set! commands (remove-all 'td commands))
						   (format () "~A[~D]: unclosed td at table (~A)~%" file linectr commands))))))
				    (set! closing #f))
				  
				  ;; not closing
				  (unless scripting
				    (let ((opener (string->symbol (substring line (+ start 1) i))))
				      (if (eq? opener 'TABLE) (set! opener 'table))
				      (cond ((memq opener '(center big font))
					     (format () "~A[~D]: ~A is obsolete, ~A~%" file linectr opener line))
					    
					    ((eq? opener 'script)
					     (set! scripting #t))
					    
					    ((eq? opener 'img)
					     (let* ((rest-line (substring line (+ start 4)))
						    (src-pos (string-position "src=" rest-line)))
					       (if (not (string-position "alt=" rest-line))
						   (format () "~A[~D]: img but no alt: ~A~%" file linectr line))
					       (if src-pos
						   (let ((png-pos (string-position ".png" rest-line)))
						     (if png-pos
							 (let ((file (substring rest-line (+ src-pos 5) (+ png-pos 4))))
							   (if (not (file-exists? file))
							       (format () "~A[~D]: src not found: ~S~%" file linectr file))))))))
					    
					    ((and (not (memq opener '(br spacer li hr area ul tr td table small sub blockquote)))
						  (memq opener commands)
						  (= p-quotes 0))
					     (format () "~A[~D]: nested ~A? ~A from: ~A~%" file linectr opener line commands))
					    (else
					     (case opener
					       ((td)
						(if (not (eq? 'tr (car commands)))
						    (format () "~A[~D]: td without tr?~%" file linectr))
						(if (and (not warned)
							 (memq 'td commands)
							 (< (count-table commands) 2))
						    (begin
						      (set! warned #t)
						      (set! commands (remove-one 'td commands))
						      (format () "~A[~D]: unclosed td at table~%" file linectr))))
					       ((tr)
						(if (not (or (eq? (car commands) 'table)
							     (eq? (cadr commands) 'table)))
						    (format () "~A[~D]: tr without table?~%" file linectr))
						(if (and (not warned)
							 (memq 'tr commands)
							 (< (count-table commands) 2))
						    (begin
						      (set! warned #t)
						      (set! commands (remove-one 'tr commands))
						      (format () "~A[~D]: unclosed tr at table~%" file linectr))))
					       ((p)
						(if (eq? (car commands) 'table)
						    (format () "~A[~D]: unclosed table?~%" file linectr)))
					       
					       ((pre br table hr img ul)
						(if (memq 'p commands)
						    (format () "~A[~D]: ~A within <p>?~%" file linectr opener)))
					       ((li)
						(if (not (memq 'ul commands))
						    (format () "~A[~D]: li without ul~%" file linectr)))
					       ((small)
						(if (memq (car commands) '(pre code))
						    (format () "~A[~D]: small shouldn't follow ~A~%" file linectr (car commands))))
					       ((--)
						(format () "~A[~D]: <-- missing !?~%" file linectr)))
					     (if (not (memq opener '(br meta spacer li hr area)))
						 (set! commands (cons opener commands))))))))
			      ;; end if closing
			      (set! start #f)))
			   
			   ((#\<)
			    (if start
				(if (not (or scripting
					     (positive? p-quotes)))
				    (format () "~A[~D]: nested < ~A~%" file linectr line))
				(set! start i)))
			   ((#\/)
			    (if (and (integer? start) (= start (- i 1)))
				(set! closing #t)))
			   
			   ((#\!)
			    (if (and (integer? start) (= start (- i 1)))
				(set! start #f)))))))
		   ) ; if not in-comment...
		 
		 ;; search for name
		 (let ((dline line))
		   (do ((pos (string-position "<em class=def id=" dline))
			(pos-len 18))
		       ((not pos))
		     (set! dline (substring dline (+ pos pos-len)))
		     (let ((epos (or (string-position "</a>" dline) 
				     (string-position "</em>" dline) 
				     (string-position "</A>" dline))))
		       ;;actually should look for close double quote
		       (if (not epos) 
			   (format () "~A[~D]: <em...> but no </em> for ~A~%" file linectr dline)
			   (begin
			     (let ((min-epos (char-position #\space dline)))
			       (set! epos (char-position #\> dline))
			       (if (and (real? min-epos)
					(< min-epos epos))
				   (set! epos min-epos)))
			     
			     (let ((new-name (string-append file "#" (substring dline 0 (- epos 1)))))
			       (if (hash-table-ref names new-name)
				   (format () "~A[~D]: ambiguous name: ~A~%" file linectr new-name))
			       (hash-table-set! names new-name file))
			     
			     (set! name (+ name 1))
			     (set! dline (substring dline epos))
			     (set! pos (string-position "<em class=def id=" dline))
			     (set! pos-len 18))))))
		 
		 ;; search for href
		 (let ((dline line))
		   (do ((pos (string-position " href=" dline)) ; ignore A HREF
			(pos-len 7))
		       ((not pos))
		     ;; (format () "~A dline: ~A~%" pos dline)
		     (if (zero? (length dline)) (exit))
		     (set! dline (substring dline (+ pos pos-len)))
		     (let ((epos (char-position #\> dline)))
		       (if (not epos) 
			   (format () "~A[~D]: <a href but no </a> for ~A~%" file linectr dline)
			   (begin
			     (set! epos (char-position #\" dline 1))
			     (let ((cur-href #f))
			       (if (char=? (dline 0) #\#)
				   (set! cur-href (string-append file (substring dline 0 epos)))
				   (begin
				     (set! cur-href (substring dline 0 epos))
				     (let ((pos (char-position #\# cur-href)))
				       (unless (or pos
						   (<= epos 5)
						   (file-exists? cur-href)
						   (string=? (substring cur-href 0 4) "ftp:")
						   (string=? (substring cur-href 0 5) "http:")
						   (string=? (substring cur-href 0 6) "https:"))
					 (format () "~A[~D]: reference to missing file ~S~%" file linectr cur-href)))))
			       
			       ;; cur-href here is the full link: sndclm.html#make-filetosample for example
			       ;;   it can also be a bare file name
			       (let* ((name (let ((start (char-position #\# cur-href)))
					      (and (number? start) 
						   (string->symbol (substring cur-href (+ start 1))))))
				      (data (and (symbol? name) 
						 (hash-table-ref ids name))))
				 (if name 
				     (if (not data)
					 (format () ";can't find id ~A~%" name)
					 (hash-table-set! ids name (+ data 1))))))
			     (set! href (+ href 1))
			     (set! dline (substring dline epos))
			     (set! pos (string-position " href=" dline))
			     (set! pos-len 7))))))))))))
     files)
    ;; end file scan
    
    (format () "found ~D names and ~D references~%" name href)

    (for-each
     (lambda (data)
       (if (zero? (cdr data))
	   (format () ";~A unref'd~%" (car data))))
     ids)
    ))


(make-index-1 '("snd.html" "extsnd.html" "grfsnd.html" "sndscm.html" "sndlib.html" "sndclm.html" "fm.html" "s7.html")
	      "test.html" 5 '("AIFF" "NeXT" "Sun" "RIFF" "IRCAM" "FIR" "IIR" "Hilbert" "AIFC") #t #t)

(html-check '("sndlib.html" "snd.html" "extsnd.html" "grfsnd.html" "sndclm.html" "sndscm.html" "fm.html" "s7.html" "index.html"))

(s7-version)
(exit)


