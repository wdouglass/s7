;;; with-sound and friends

(provide 'snd-ws.scm)

;;; -------- with-sound defaults --------

(set! *clm-srate* *default-output-srate*)

(define *clm-file-name*         "test.snd")
(define *clm-reverb-file-name*  "test.rev")
(define *clm-channels*          *default-output-chans*)
(define *clm-sample-type*       *default-output-sample-type*)
(define *clm-header-type*       *default-output-header-type*)
(define *clm-verbose*           #f)
(define *clm-play*              #f)
(define *clm-statistics*        #f)
(define *clm-reverb*            #f)
(define *clm-reverb-channels*   1)
(define *clm-reverb-data*       ())
(define *clm-locsig-type*       mus-interp-linear)
(define *clm-clipped*           #t)
(define *clm-array-print-length* *print-length*)
(define *clm-player*            #f)
(define *clm-notehook*          #f)
(define *clm-with-sound-depth*  0)           ; for CM, not otherwise used
(define *clm-delete-reverb*     #f)          ; should with-sound clean up reverb stream

(define *to-snd*                #t)
(define *default-player* (lambda (s) (play s :wait #t)))

(set! *clm-file-buffer-size* 65536)

(define times->samples 
  (let ((+documentation+ "(times->samples beg dur) converts beg and (+ beg dur) to samples, returning both in a list"))
    (lambda (beg dur) 
      (list (seconds->samples beg) (seconds->samples (+ beg dur))))))

(define *clm-default-frequency* 0.0) ; this is obsolete


;;; -------- definstrument --------

;(define definstrument define*) -- old form 2-Nov-05
(define *definstrument-hook* #f) ; for CM

(define-macro (definstrument args . body)
  (let* ((name (car args))
	 (targs (cdr args))
	 (utargs (let ((arg-names ()))
		   (for-each
		    (lambda (a)
		      (if (not (keyword? a))
			  (set! arg-names (cons (if (symbol? a) a (car a)) arg-names))))
		    targs)
		   (reverse arg-names))))
    (if (string? (car body))
	`(begin
	   (define ,name
	     (let ((+documentation+ ,(car body)))
	       (lambda* ,targs
		 (if *clm-notehook*
		     (*clm-notehook* (symbol->string ',name) ,@utargs))
		 ,@(cdr body))))
	   ,@(if *definstrument-hook*
		 (list (*definstrument-hook* name targs))
		 ()))
	`(begin 
	   (define* (,name ,@targs)
	     (if *clm-notehook*
		 (*clm-notehook* (symbol->string ',name) ,@utargs))
	     ,@body)
	   ,@(if *definstrument-hook*
		 (list (*definstrument-hook* name targs))
		 ())))))

     

;;; -------- with-sound --------

(define* (with-sound-helper thunk 
			    (output *clm-file-name*) 
			    (channels *clm-channels*)
			    (srate *clm-srate*) 
			    (sample-type *clm-sample-type*)
			    (header-type *clm-header-type*)
			    comment
			    (verbose *clm-verbose*)
			    (reverb *clm-reverb*)
			    (revfile *clm-reverb-file-name*)
			    (reverb-data *clm-reverb-data*)
			    (reverb-channels *clm-reverb-channels*)
			    continue-old-file
			    (statistics *clm-statistics*)
			    scaled-to
			    (play *clm-play*)
			    (to-snd *to-snd*)
			    (clipped 'unset)
			    (notehook *clm-notehook*)               ; (with-sound (:notehook (lambda args (display args))) (fm-violin 0 1 440 .1))
			    scaled-by
			    ignore-output)
  (let ((old-srate *clm-srate*)
	(old-*output* *output*)
	(old-*reverb* *reverb*)
	(old-notehook *clm-notehook*)
	(old-verbose *clm-verbose*)
	(old-auto-update-interval *auto-update-interval*)
	(output-1 output)                    ; protect during nesting
	(output-to-file (string? output))
	(reverb-1 revfile)
	(reverb-to-file (and reverb (string? revfile))))

    (if ignore-output
	(begin
	  (set! output-1 *clm-file-name*)
	  (set! output-to-file (string? output-1))))

    (dynamic-wind 

     (lambda () 
       (set! *clm-verbose* verbose)
       (set! *clm-notehook* notehook)
       (set! (locsig-type) *clm-locsig-type*)
       (set! *mus-array-print-length* *clm-array-print-length*)
       (set! *auto-update-interval* 0.0) 
       (set! (mus-clipping) 
	     (if (not (eq? clipped 'unset))
		 clipped
		 (and (not (and (or scaled-by scaled-to)
				(member sample-type (list mus-bfloat mus-lfloat mus-bdouble mus-ldouble))))
		      *clm-clipped*)))
       (set! *clm-srate* srate))

     (lambda ()
       (if output-to-file
	   (if continue-old-file
	       (begin
		 (set! *output* (continue-sample->file output-1))
		 (set! *clm-srate* (mus-sound-srate output-1)) ; "srate" arg shadows the generic func
		 (let ((ind (find-sound output-1)))
		   (if (sound? ind)
		       (close-sound ind))))
	       (begin
		 (if (file-exists? output-1) 
		     (delete-file output-1))
		 (set! *output* (make-sample->file output-1 channels sample-type header-type comment))))
	   (begin
	     (if (and (not continue-old-file)
		      (vector? output-1))
		 (fill! output-1 0.0))
	     (set! *output* output-1)))
       
       (if reverb
	   (if reverb-to-file
	       (if continue-old-file
		   (set! *reverb* (continue-sample->file reverb-1))
		   (begin
		     (if (file-exists? reverb-1) 
			 (delete-file reverb-1))
		     (set! *reverb* (make-sample->file reverb-1 
						       reverb-channels 
						       (if (mus-header-writable header-type mus-ldouble)
							   mus-ldouble
							   sample-type)
						       header-type))))
	       (begin
		 (if (and (not continue-old-file)
			  (vector? reverb-1))
		     (fill! reverb-1 0.0))
		 (set! *reverb* reverb-1))))

       (let ((start (if statistics (get-internal-real-time)))
	     (revmax #f))
	 (let ((flush-reverb #f))
	   (catch 'mus-error
	     
	     (lambda ()
	       (catch 'with-sound-interrupt
		 thunk
		 (lambda args 
		   (snd-print (format #f "with-sound interrupted: ~{~A~^ ~}" (cdr args)))
		   (set! flush-reverb #t)
		   args)))
	     
	     (lambda args
	       ;; hit mus-error, for example:
	       ;;   (with-sound () (fm-violin 0 1 440 .1 :amp-env '(0 0 1 1 1 2 3 0)))
	       
	       ;; user might have listener closed, or no listener so...
	       (format () ";~%with-sound mus-error: ~{~A~^ ~}~%" (cdr args))
	       
	       ;; now try to get something to listener, since there may be no stdout
	       (snd-print (format #f ";~%with-sound mus-error: ~{~A~^ ~}~%" (cdr args)))
	       (set! flush-reverb #t)))
	   
	   (if (and reverb 
		    (not flush-reverb)) ; i.e. not interrupted by error and trying to jump out
	       (begin
		 (if reverb-to-file
		     (mus-close *reverb*))
		 (if (and statistics 
			  (or reverb-to-file
			      (vector? reverb-1)))
		     (set! revmax (maxamp reverb-1)))
		 (if reverb-to-file
		     (set! *reverb* (make-file->sample reverb-1)))
		 (apply reverb reverb-data)                                   ; here is the reverb call(!)
		 (when reverb-to-file
		   (mus-close *reverb*)
		   (if *clm-delete-reverb*
		       (delete-file reverb-1))))))

	 (if output-to-file
	     (mus-close *output*))

	 (let ((snd-output #f)
	       (cycles 0)
	       (cur-sync #f))
	   (if statistics
	       (set! cycles (* 1.0 (- (get-internal-real-time) start))))

	   (if (and to-snd output-to-file)
	       (let ((cur (find-sound output-1)))
		 (set! cur-sync (and cur (sync cur)))
		 (set! snd-output (if cur 
				      (update-sound cur)
				      (if (= header-type mus-raw)
					  (open-raw-sound output-1 channels (floor srate) sample-type)
					  ;; open-sound here would either ask for raw settings or use possibly irrelevant defaults
					  (open-sound output-1))))
		 (set! (sync snd-output) #t)))

	   (if statistics
	       ((if (procedure? statistics) ; :statistics (lambda (str) (snd-print str)) -- intended for auto test suite
		    statistics 
		    (if to-snd 
			snd-print 
			display))
		(format #f (if (not (member sample-type (list mus-bdouble mus-ldouble)))
			       "~%;~A:~%  maxamp~A:~{ ~,4F~}~%~A  compute time: ~,3F~%"
			       "~%;~A:~%  maxamp~A:~{ ~,8F~}~%~A  compute time: ~,3F~%")
			(if output-to-file
			    output-1
			    (if (vector? output-1) "vector" "flush"))
			(if (or scaled-to scaled-by) 
			    " (before scaling)" 
			    "")
			(if output-to-file
			    (if to-snd
				(maxamp snd-output #t) ; this is a list of chan maxs '(.1 .2)
				(do ((lst (mus-sound-maxamp output-1))
				     (i 0 (+ i 2)))
				      ((>= i (length lst))
				       lst)
				  (set! (lst i) (/ (lst i) *clm-srate*))))
			    (if (vector? output-1)
				(list (maxamp output-1))
				'(0.0)))
			(if revmax (format #f "  rev max: ~,4F~%" revmax) "")
			cycles)))

	   (cond ((not (or scaled-to scaled-by)))

		 (output-to-file
		  (let* ((scale-output (or snd-output (open-sound output-1)))
			 (old-sync (sync scale-output)))
		    (set! (sync scale-output) (+ (sync-max) 1))          ; make sure scaling doesn't follow sync
		    (if scaled-to
			(scale-to scaled-to scale-output)
			(scale-by scaled-by scale-output))
		    (set! (sync scale-output) old-sync)
		    (save-sound scale-output)
		    (if (not to-snd) 
			(close-sound scale-output))))
		 
		 ((float-vector? output-1)
		  (if scaled-to
		      (let ((pk (float-vector-peak output-1)))
			(if (> pk 0.0)
			    (float-vector-scale! output-1 (/ scaled-to pk))))
		      (float-vector-scale! output-1 scaled-by)))
		 
		 ((not (vector? output-1)))
		 
		 (scaled-to
		  (let ((pk (maxamp output-1)))
		    (if (> pk 0.0)
			(do ((scl (/ scaled-to pk))
			     (i 0 (+ i 1)))
			    ((= i (length output-1)))
			  (set! (output-1 i) (* scl (output-1 i)))))))
		 (else
		  (do ((i 0 (+ i 1)))
		      ((= i (length output-1)))
		    (set! (output-1 i) (* scaled-by (output-1 i))))))
	   
	   (when output-to-file
	     (when play
	       (if to-snd
		   ((or *clm-player* *default-player*) snd-output)
		   (*default-player* output-1)))

	     (when to-snd
	       (update-time-graph snd-output)
	       (goto-listener-end)
	       (if (number? cur-sync) (set! (sync snd-output) cur-sync)))))

	 output-1))

     (lambda () 
       (set! *clm-verbose* old-verbose)
       (set! *clm-notehook* old-notehook)
       (set! *auto-update-interval* old-auto-update-interval)
       (if *reverb*
	   (begin
	     (mus-close *reverb*)
	     (set! *reverb* old-*reverb*)))
       (if *output*
	   (begin
	     (if (mus-output? *output*)
		 (mus-close *output*))
	     (set! *output* old-*output*)))
       (set! *clm-srate* old-srate)))))


(define-macro (with-sound args . body)
  `(with-sound-helper (lambda () ,@body) ,@args))


;;; -------- with-full-sound --------

(define-macro (with-full-sound args . body)
  ;; with-sound but display full sound in Snd window
  `(let ((snd (with-sound-helper (lambda () ,@body) ,@args)))
     (set! (x-bounds *snd-opened-sound*) (list 0.0 (/ (framples *snd-opened-sound*) (srate *snd-opened-sound*))))
     (let ((mx (apply max (maxamp *snd-opened-sound* #t))))
       (if (> mx 1.0)
	 (set! (y-bounds *snd-opened-sound*) (list (- mx) mx))))
     snd))

(define-macro (with-fullest-sound args . body)
  ;; with-sound but display full sound in Snd window
  `(let ((snd (with-sound-helper (lambda () ,@body) ,@args)))
     (set! (x-bounds *snd-opened-sound*) (list 0.0 (/ (framples *snd-opened-sound*) (srate *snd-opened-sound*))))
     (set! (channel-style *snd-opened-sound*) channels-separate)
     (do ((chn 0 (+ chn 1)))
	 ((= chn (channels *snd-opened-sound*)))
       (let ((mx (maxamp *snd-opened-sound* chn)))
	 (if (> mx 1.0)
	     (set! (y-bounds *snd-opened-sound* chn) (list (- mx) mx)))))
     snd))


;;; -------- with-temp-sound --------

(define-macro (with-temp-sound args . body)
  `(let ((old-file-name *clm-file-name*)
	 (old-to-snd *to-snd*))
     ;; with-sound but using tempnam for output (can be over-ridden by explicit :output) and does not open result in Snd
     (dynamic-wind
	 (lambda () 
	   (set! *clm-file-name* (snd-tempnam))
	   (set! *to-snd* #f))
	 (lambda ()
	   (with-sound-helper (lambda () ,@body) ,@args)) ; dynamic-wind returns this as its result
	 (lambda ()
	   (set! *to-snd* old-to-snd)
	   (set! *clm-file-name* old-file-name)))))


;;; -------- clm-load --------

(define clm-load
  (let ((+documentation+ "(clm-load file . args) loads 'file' in the context of with-sound"))
    (lambda (file . args)
      (apply with-sound-helper (lambda () (load file)) args))))


;;; -------- with-mixed-sound --------

(define (with-mixed-sound-mix-info id snd)
  (let find-if ((pred (lambda (info)
			(and (>= id (car info))
			     (< id (+ (car info) (caddr info))))))
		(lst (sound-property 'with-mixed-sound-info snd)))   ; each entry is '(mx-id beg chans note)
    (cond ((null? lst) #f)
	  ((pred (car lst)) (car lst))
	  (else (find-if pred (cdr lst))))))


(define-macro (with-mixed-sound args . body)
  `(let* ((output (with-sound-helper (lambda () #f) ,@args :to-snd #t)) ; pick up args for output
	  (outsnd (find-sound output)))

     (if (sound? outsnd)
	 (let ((mix-info ())
	       (old-sync (sync outsnd)))

	   ;; if multichannel output, make sure cross-chan mixes move together 
	   (if (> (channels outsnd) 1)
	       (begin
		 (set! (hook-functions mix-release-hook) ())
		 (hook-push mix-release-hook
			    (lambda (hook)
			      (let ((id (hook 'id))
				    (samps-moved (hook 'samples)))
				(let ((new-pos (+ samps-moved (mix-position id)))
				      (base (sync id)))
				  (do ((mx (integer->mix base) (integer->mix (+ (mix->integer mx) 1))))
				      ((or (not (mix? mx))
					   (not (= (sync mx) base))))
				    (set! (mix-position mx) new-pos))
				  (set! (hook 'result) #t)))))))

	   ;; click shows the original note list entry
	   (set! (hook-functions mix-click-hook) ())
	   (hook-push mix-click-hook
		      (lambda (hook)
			(let ((info (with-mixed-sound-mix-info (hook 'id) outsnd)))
			  (status-report (format #f "mix ~A: ~A" 
						 (hook 'id) (or (and info
							     (cadddr info))
							(/ (mix-position id) (* 1.0 (srate outsnd))))))
			  (set! (hook 'result) #t)))) ; #t -> don't print the mix id in the status area

	   (dynamic-wind
	       (lambda ()
		 (set! (sync outsnd) 0)
		 (do ((chan 0 (+ 1 chan)))
		     ((= chan (channels outsnd)))
		   (set! (squelch-update outsnd chan) #t)))

	       (lambda ()
		 (for-each
		  (lambda (note)
		    (let* ((snd (with-temp-sound (,@args :ignore-output #t :clipped #f) (eval (append (list (car note) 0.0) (cddr note)) (curlet))))
			   ;; I can't immediately find a way around the "eval" 
			   (beg (floor (* (srate outsnd) (cadr note))))
			   ;; can't use seconds->samples here because the global mus-srate value might not match the local one
			   (mx (car (mix snd beg #t outsnd #f #t #t)))     ; all chans mixed, current output sound, with mixes, with auto-delete
			   (chans (mus-sound-chans snd)))
		      (set! (mix-name mx) (format #f "(~A ~A)" (car note) (cadr note)))
		      (do ((chan 0 (+ 1 chan)))
			  ((= chan chans))
			(set! (sync (integer->mix (+ (mix->integer mx) chan))) (mix->integer mx)))
		      (set! mix-info (cons (list mx beg chans note) mix-info))))
		  ',body)
		 (set! (sound-property 'with-mixed-sound-info outsnd) (reverse mix-info)))

	       (lambda ()
		 (set! (sync outsnd) old-sync)
		 (do ((chan 0 (+ 1 chan)))
		     ((= chan (channels outsnd)))
		   (set! (squelch-update outsnd chan) #f)
		   ;; fixup y bounds to show waveform unclipped
		   (let ((mx (ceiling (maxamp outsnd chan))))
		     (set! (y-bounds outsnd chan) (list (- mx) mx)))
		   ;; fixup mix tags so they overlap less often
		   (let ((mxs (mixes outsnd chan)))
		     (let ((hgt 0))
		       (for-each
			(lambda (m)
			  (set! (mix-tag-y m) hgt)
			  (set! hgt (modulo (+ hgt 30) 100)))
			mxs)))
		   (update-time-graph outsnd chan))))))
     output))

;(with-mixed-sound () (fm-violin 0 .1 440 .1) (fm-violin 1 .1 660 .1))
;(with-mixed-sound (:channels 2) (fm-violin 0 .1 440 .1 :degree 0) (fm-violin 1 .1 660 .1 :degree 45))


(define* (with-mixed-sound->notelist (output-file "temp.scm") snd)
  (let* ((outsnd (or snd (selected-sound) (car (sounds))))
	 (mix-info (sound-property 'with-mixed-sound-info outsnd)))
    (if (not mix-info)
	(error 'no-such-mixed-sound (list "with-mixed-sound->notelist" outsnd))
	(let ((cur-mixes (mixes outsnd 0)) ; for now assume each mix is multichannel
	      (oput (open-output-file output-file)))
	  (format oput "(with-sound (:channels ~D)~%" (channels snd))
	  (for-each
	   (lambda (id)
	     (let ((info (with-mixed-sound-mix-info id snd)))
	       (if info
		   (let ((call (cadddr info)))
		     (format oput (if (= (cadr info) (mix-position id))
				      (values "  ~A~%" call)
				      (values "  (~A ~,3F~{ ~A~})~%"
					      (car call) 
					      (/ (mix-position id) (* 1.0 (srate snd)))
					      (cddr call)))))
		   (status-report "can't find note associated with mix ~A" id))))
	   cur-mixes)
	  (format oput ")~%")
	  (close-output-port oput)))))



;;; -------- with-marked-sound --------

(define-macro (with-marked-sound args . body)
  `(let ((old-notehook *clm-notehook*)
	 (mark-list ()))
     (dynamic-wind
	 (lambda ()
	   (set! *clm-notehook* (lambda (name . args)
				  (set! mark-list (cons (cons name args) mark-list)))))

	 (lambda ()
	   (let* ((result (with-sound-helper (lambda () ,@body) ,@args))
		  (snd (find-sound result))
		  (old-update (squelch-update snd 0)))
	     (dynamic-wind
		 (lambda ()
		   (set! (squelch-update snd 0) #t))
		 (lambda ()
		   (for-each
		    (lambda (descr)
		      (let ((m (add-mark (floor (* (srate snd) (cadr descr))) snd)))
			(set! (mark-name m) (format #f "~A ~A ~A" (car descr) (cadr descr) (caddr descr)))))
		    mark-list))
		 (lambda ()
		   (set! (squelch-update snd 0) old-update)))
	     result))
		  
	 (lambda ()
	   (set! *clm-notehook* old-notehook)))))

;;; (with-marked-sound () (do ((i 0 (+ 1 i))) ((= i 5)) (fm-violin i .1 440 .1)))



;;; -------- sound-let --------
;;;
;;; (with-sound () (sound-let ((a () (fm-violin 0 .1 440 .1))) (mus-mix "test.snd" a)))

(define-macro (sound-let snds . body) 
  `(let ((temp-files ())
	 (old-hook-list (hook-functions new-sound-hook))) ; save old new-sound-hook (nested sound-lets etc)
     (set! (hook-functions new-sound-hook)
	   (list (lambda (hook)       ; save current sound-let temp file list
		   (let ((file (hook 'name)))
		     (if (string? file) ; try to ignore float-vectors
			 (set! temp-files (cons file temp-files)))))))
     (let ((val (let ,(map (lambda (arg) 
			     (if (> (length arg) 2)
					; if with-sound, embed with-temp-sound
				 `(,(car arg) (with-temp-sound ,(cadr arg) ,@(cddr arg)))
				 arg))              ; else use direct (normal var in the let)
			   snds)
		  ,@body)))                         ; sound-let body
       (for-each (lambda (file)                     ; clean up all local temps
		   (if (and (string? file)          ; is it a file? (might be a float-vector)
			    (file-exists? file))
		       (delete-file file)))
		 temp-files)
       (set! (hook-functions new-sound-hook) old-hook-list)
       val)))



;;; -------- Common Music --------

(define* (init-with-sound
	  (srate *clm-srate*) 
	  (output *clm-file-name*) 
	  (channels *clm-channels*)
	  (header-type *clm-header-type*)
	  data-format
	  (sample-type *clm-sample-type*)
	  comment
	  ;(verbose *clm-verbose*) ; why is this commented out?
	  (reverb *clm-reverb*)
	  (revfile *clm-reverb-file-name*)
	  (reverb-data *clm-reverb-data*)
	  (reverb-channels *clm-reverb-channels*)
	  continue-old-file
	  (statistics *clm-statistics*)
	  scaled-to
	  (play *clm-play*)
	  (to-snd *to-snd*)
	  scaled-by)
  (let ((old-srate *clm-srate*)
	(start (if statistics (get-internal-real-time)))
	(output-to-file (string? output))
	(reverb-to-file (and reverb (string? revfile))))
    (set! *clm-srate* srate)
    (if output-to-file
	(if continue-old-file
	    (begin
	      (set! *output* (continue-sample->file output))
	      (set! *clm-srate* (mus-sound-srate output))
	      (let ((ind (find-sound output)))
		(if (sound? ind)
		    (close-sound ind))))
	    (begin
	      (if (file-exists? output) 
		  (delete-file output))
	      (set! *output* (make-sample->file output channels (or data-format sample-type) header-type comment))))
	(begin
	  (if (and (not continue-old-file)
		   (vector? output))
	      (fill! output 0.0))
	  (set! *output* output)))

    (if reverb
	(if reverb-to-file
	    (if continue-old-file
		(set! *reverb* (continue-sample->file revfile))
		(begin
		  (if (file-exists? revfile) 
		      (delete-file revfile))
		  (set! *reverb* (make-sample->file revfile reverb-channels (or data-format sample-type) header-type))))
	    (begin
	      (if (and (not continue-old-file)
		       (vector? revfile))
		  (fill! revfile 0.0))
	      (set! *reverb* revfile))))

    (list 'with-sound-data
	  output
	  reverb
	  revfile
	  old-srate
	  statistics
	  to-snd
	  scaled-to
	  scaled-by
	  play
	  reverb-data
	  start)))

(define (finish-with-sound wsd)
  (if (not (eq? (car wsd) 'with-sound-data))
      (error 'wrong-type-arg (list "finish-with-sound" wsd))
      (let ((output (wsd 1))
	    (old-srate (wsd 4))
	    (statistics (wsd 5))
	    (to-snd (wsd 6))
	    (scaled-to (wsd 7))
	    (scaled-by (wsd 8))
	    (play (wsd 9))
	    (start (wsd 11)))

	(let ((reverb (wsd 2))
	      (revfile (wsd 3))
	      (reverb-data (wsd 10)))
	  (if reverb
	      (begin
		(mus-close *reverb*)
		(set! *reverb* (if (string? revfile)
				   (make-file->sample revfile)
				   revfile))
		(apply reverb reverb-data)
		(mus-close *reverb*))))

	(if (mus-output? *output*)
	    (mus-close *output*))
	(if (and to-snd (string? output))
	    (let ((snd-output (open-sound output)))
	      (set! (sync snd-output) #t)
	      (if statistics
		  (snd-print (format #f "~A:~%  maxamp: ~A,~%  compute time: ~A~%"
				     output
				     (maxamp snd-output #t)
				     (/ (- (get-internal-real-time) start) 100))))
	      (if (or scaled-to scaled-by)
		  (begin
		    (if scaled-to
			(scale-to scaled-to snd-output)
			(if scaled-by
			    (scale-by scaled-by snd-output)))
		    (save-sound snd-output)))
	      (if play (*default-player* snd-output))
	      (update-time-graph snd-output)))
	(set! *clm-srate* old-srate)
	output)))


(define wsdat-play ; for cm
  (dilambda
   (let ((+documentation+ "accessor for play field of init-with-sound struct"))
     (lambda (w)
       (w 9)))
   (lambda (w val)
     (set! (w 9) val))))


;;; -------- with-sound save state --------

(define ws-save-state 
  (let ((+documentation+ "(ws-save-state filename) is an after-save-state-hook function that saves the current with-sound global settings"))
    (lambda (hook)
      (let ((fd (open-output-file (hook 'name) "a")))
	;; should the save-state file load this file if it hasn't been loaded? (what path?)
	(format fd "~%~%;;; from ws.scm~%")
	(format fd "(if (defined? '*clm-srate*)~%")
	(format fd "    (begin~%")
	(format fd "      (set! *clm-srate* ~A)~%" *clm-srate*)
	(format fd "      (set! *clm-file-name* ~S)~%" *clm-file-name*)
	(format fd "      (set! *clm-channels* ~A)~%" *clm-channels*)
	(format fd "      (set! *clm-sample-type* ~A)~%" (mus-sample-type->string *clm-sample-type*))
	(format fd "      (set! *clm-header-type* ~A)))~%" (mus-header-type->string *clm-header-type*))
	(close-output-port fd)))))

(if (not (memq ws-save-state (hook-functions after-save-state-hook)))
    (set! (hook-functions after-save-state-hook) (list ws-save-state)))


;;; -------- ->frequency --------

(define ->frequency
  (let ((main-pitch (/ 440.0 (expt 2.0 19/4))) ;(/ 57 12) ; a4 = 440Hz is pitch 57 in our numbering
	(last-octave 0)                                   ; octave number can be omitted
	(ratios (vector 1.0 256/243 9/8 32/27 81/64 4/3 1024/729 3/2 128/81 27/16 16/9 243/128 2.0))
	(+documentation+ "(->frequency pitch pythagorean) returns the frequency (Hz) of the 'pitch', a CLM/CM style note name as a \
symbol: 'e4 for example.  If 'pythagorean', the frequency calculation uses small-integer ratios, rather than equal-tempered tuning."))
    (lambda* (pitch pythagorean)          ; pitch can be pitch name or actual frequency
      (if (not (symbol? pitch))
	  pitch
	  (let* ((name (string-downcase (symbol->string pitch)))
		 (octave-char (if (and (> (length name) 1)
				       (char-numeric? (name 1))) 
				  (name 1)
				  (and (> (length name) 2) 
				       (char-numeric? (name 2))
				       (name 2))))
		 (sign-char (and (> (length name) 1)
				 (not (char-numeric? (name 1)))
				 (not (char=? (name 1) #\n))
				 (name 1)))
		 (octave (if octave-char (- (char->integer octave-char) (char->integer #\0)) last-octave))
		 (base-pitch (let ((base (modulo (- (+ (char->integer (name 0)) 5) (char->integer #\a)) 7)) ; c-based (diatonic) octaves	   
				   (sign (case sign-char ((#f) 0) ((#\f) -1) (else 1))))
			       (+ sign (case base ((0)) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11)))))
		 (et-pitch (+ base-pitch (* 12 octave))))
	    (set! last-octave octave)
	    (* main-pitch (if pythagorean
			      (* (expt 2 octave) (ratios base-pitch))
			      (expt 2.0 (/ et-pitch 12)))))))))


;;; -------- ->sample --------

(define ->sample 
  (let ((+documentation+ "(->sample time-in-seconds) -> time-in-samples"))
    (lambda (beg)
      (round (* (if (pair? (sounds)) (srate) *clm-srate*) beg)))))



;;; -------- defgenerator --------

;;; (defgenerator osc a b)
;;; (defgenerator (osc :methods (list (cons 'mus-frequency (lambda (obj) 100.0)))) a b)

(define-macro (defgenerator struct-name . fields)

  (define (list->bindings lst)
    (let ((nlst (make-list (* (length lst) 2))))
      (do ((old lst (cdr old))
	   (nsym nlst (cddr nsym)))
	  ((null? old) nlst)
	(if (pair? (car old))
	    (begin
	      (list-set! nsym 1 (caar old))
	      (list-set! nsym 0 (list 'quote (caar old))))
	    (begin
	      (list-set! nsym 1 (car old))
	      (list-set! nsym 0 (list 'quote (car old))))))))

  (let* ((sname (let ((name (if (pair? struct-name) 
				(car struct-name) 
				struct-name)))
		  (if (string? name) 
		      name 
		      (symbol->string name))))
	 (wrapper (let ((wrap (and (pair? struct-name)
				   (or (and (> (length struct-name) 2)
					    (eq? (struct-name 1) :make-wrapper)
					    (struct-name 2))
				       (and (= (length struct-name) 5)
					    (eq? (struct-name 3) :make-wrapper)
					    (struct-name 4))))))
		    (or wrap (lambda (gen) gen))))
	 (methods (and (pair? struct-name)
		       (or (and (> (length struct-name) 2)
				(eq? (struct-name 1) :methods)
				(struct-name 2))
			   (and (= (length struct-name) 5)
				(eq? (struct-name 3) :methods)
				(struct-name 4))))))
    `(begin 
       (define ,(symbol sname "?") #f)
       (define ,(symbol "make-" sname) #f)

       (let ((gen-type ',(symbol "+" sname "+"))
	     (gen-methods (and ,methods (apply inlet ,methods))))
	 
	 (set! ,(symbol sname "?")
	       (lambda (obj)
		 (and (let? obj)
		      (eq? (obj 'mus-generator-type) gen-type))))

	 (set! ,(symbol "make-" sname)
	       (lambda* ,(map (lambda (n) 
				(if (pair? n) n (list n 0.0)))
			      fields)
  	         (,wrapper 
		  (openlet
		   ,(if methods
		       `(sublet gen-methods
			  ,@(list->bindings (reverse fields)) 'mus-generator-type gen-type)
		       `(inlet 'mus-generator-type gen-type ,@(list->bindings fields)))))))))))



;;; -------- clm-display-globals --------
;;;
;;; display all the globals that might affect with-sound unexpectedly

(define (clm-display-globals)

  (format #f ";CLM globals:~%;  *clm-srate*: ~A (default: ~A)~%;  *clm-file-name*: ~A~%;  *clm-channels: ~A (default: ~A)~%;  *clm-sample-type*: ~A (default: ~A)~%;  *clm-header-type*: ~A (default: ~A)~%;  *clm-reverb-channels*: ~A, *clm-reverb-data*: ~A~%;  *clm-table-size*: ~A~%;  *clm-file-buffer-size*: ~A~%;  *clm-locsig-type*: ~A~%;  *clm-array-print-length*: ~A (~A)~%;  *clm-notehook*: ~A~%;  *clm-clipped*: ~A, mus-clipping: ~A~%~%"

	  *clm-srate* *default-output-srate*
	  *clm-file-name*
	  *clm-channels* *default-output-chans*
	  (mus-sample-type->string *clm-sample-type*) (mus-sample-type->string *default-output-sample-type*)
	  (mus-header-type->string *clm-header-type*) (mus-header-type->string *default-output-header-type*)
	  *clm-reverb-channels* *clm-reverb-data*
	  *clm-table-size*
	  *clm-file-buffer-size*
	  *clm-locsig-type*
	  *clm-array-print-length* *print-length*
	  *clm-notehook*
	  *clm-clipped* (mus-clipping)))




;;; -------- *clm-search-list*

(define *clm-search-list* (list "."))

(define (clm-find-file name)
  (if (file-exists? name)
      name
      (call-with-exit
       (lambda (return)
	 (for-each
	  (lambda (path)
	    (let ((len (length path)))
	      (if (> len 0)
		  (let ((new-name (string-append path (if (not (char=? (path (- len 1)) #\/)) "/" "") name)))
		    (if (file-exists? new-name)
			(return new-name))))))
	  *clm-search-list*)
	 #f))))

#|
(with-sound ()
  (let ((fd (make-file->sample "oboe.snd"))
	(len (mus-sound-framples "oboe.snd")))
    (do ((i 0 (+ i 1)))
	((= i len))
      (outa i (* .5 (file->sample fd i 0))))))
|#


(define (mix-notelists . notelists)
  ;; assume the second parameter is the begin time in seconds (the first is the instrument name)
  (sort! 
   (apply append notelists)
   (lambda (note1 note2)
     (< (cadr note1) (cadr note2)))))

#|
(mix-notelists '((fm-violin 0 1 440 .1)
		 (fm-violin 1 1 550 .1))
	       '((bird 0 .1 )
		 (bird .2 .1)
		 (bird 1.2 .3)
		 (bird .5 .5)))
((bird 0 0.1) (fm-violin 0 1 440 0.1) (bird 0.2 0.1) (bird 0.5 0.5) (fm-violin 1 1 550 0.1) (bird 1.2 0.3))
|#


(define* (with-simple-sound-helper thunk (output "test.snd") (channels 1) (srate 44100) (sample-type mus-lfloat) (header-type mus-next))
  (let ((old-output *output*))
    (dynamic-wind
	(lambda ()
	  (set! *output* (if (string? output)
			     (make-sample->file output channels sample-type header-type "with-simple-sound output")
			     output)))
	(lambda ()
	  (thunk)
	  output)
	(lambda ()
	  (if (string? output)
	      (mus-close *output*))
	  (set! *output* old-output)))))

(define-macro (with-simple-sound args . body)
  `(with-simple-sound-helper (lambda () ,@body) ,@args))
