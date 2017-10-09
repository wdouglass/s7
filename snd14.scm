(define trap-segfault
  (dilambda
   (lambda () #f)
   (lambda (val) val)))

(define* (if-cursor-follows-play-it-stays-where-play-stopped (enable #t))
  (set! *with-tracking-cursor* (and enable :track-and-stay)))


(define* (find-channel func (beg 0) snd chn edpos) 
  (scan-channel func beg #f snd chn edpos))

(define scan-chan scan-channel)

(define* (map-chan proc beg end origin snd chn edpos) 
  (map-channel proc beg (- (+ end 1) beg) snd chn edpos origin))


(define smooth-vct smooth-float-vector)
(define vct-polynomial float-vector-polynomial)
(define mix->vct mix->float-vector)
(define pan-mix-vct pan-mix-float-vector)
(define vct-size float-vector-size)



#|
;;; -------- sound segmentation
;;;
;;; this code was used to return note on and off points for the Iowa Musical Instrument Sound library
;;;   the main function takes the top level directory of the sounds, and returns (eventually) a text
;;;   file containing the start times (in samples) and durations of all the notes (each sound file in
;;;   this library can have about 12 notes).  The results of this function need to be fixed up by hand
;;;   in some cases (violin notes in particular).

;;; this code needs closedir, readdir and opendir (Guile-isms, I think)

(define* (sounds->segment-data main-dir (output-file "sounds.data"))

  (define (lower-case-and-no-spaces name)
    (let* ((new-name (string-downcase name))
	   (len (length new-name)))
      (do ((i 0 (+ i 1)))
	  ((= i len) new-name)
	(if (char=? (new-name i) #\space)
	    (set! (new-name i) #\-)))))

  (define (directory->list dir)
    (let ((dport (opendir dir)) 
	  (ctr 0)) ; try to avoid infinite loop in the broken cases
      (let loop ((entry (readdir dport))
		 (files ()))
	(set! ctr (+ ctr 1))
	(if (and (< ctr 2000)
		 (not (eof-object? entry)))
	    (loop (readdir dport) (cons entry files))
	    (begin
	      (closedir dport)
	      (reverse! files))))))

  (define (segment-maxamp name beg dur)
    (float-vector-peak (samples beg dur name)))

  (define (segment-sound name high low)
    (let* ((end (framples name))
	   (reader (make-sampler 0 name))    ; no need to open the sound and display it
	   (avg (make-moving-average :size 128))
	   (lavg (make-moving-average :size 2048)) ; to distinguish between slow pulse train (low horn) and actual silence
	   (segments (make-float-vector 100))
	   (segctr 0)
	   (possible-end 0)
	   (in-sound #f))
       ;; this block is where 99% of the time goes
       (do ((i 0 (+ i 1)))
	   ((= i end))
	 (let* ((samp (abs (next-sample reader)))
		(val (moving-average avg samp))
		(lval (moving-average lavg samp)))
	   (if in-sound
	       (if (< val low)
		   (begin
		     (set! possible-end i)
		     (if (< lval low)
			 (begin
			   (set! (segments segctr) (+ possible-end 128))
			   (set! segctr (+ segctr 1))
			   (set! in-sound #f)))))
	       (if (> val high)
		   (begin
		     (set! (segments segctr) (- i 128))
		     (set! segctr (+ segctr 1))
		     (set! in-sound #t))))))
      (if in-sound
	  (begin
	    (set! (segments segctr) end)
	    (list (+ 1 segctr) segments))
	  (list segctr segments))))

  (define* (do-one-directory fd dir-name ins-name (high .01) (low .001))
    (snd-print (format #f ";~A~%" dir-name))
    (for-each
     (lambda (sound)
       (let* ((sound-name (string-append dir-name "/" sound))
	      (boundary-data (segment-sound sound-name high low))
	      (boundaries (cadr boundary-data))
	      (segments (car boundary-data)))
	 (format fd "~%~%;;;    ~A" sound)
	 (format fd "~%(~A ~S" ins-name (string-append dir-name "/" sound))
	 (do ((bnd 0 (+ bnd 2)))
	     ((>= bnd segments))
	   (let* ((segbeg (floor (boundaries bnd)))
		  (segdur (floor (- (boundaries (+ 1 bnd)) segbeg))))
	     (format fd " (~A ~A ~A)" segbeg segdur (segment-maxamp sound-name segbeg segdur))))
	 (format fd ")")
	 (mus-sound-forget (string-append dir-name "/" sound))))
     (sound-files-in-directory dir-name)))

  (call-with-output-file
      output-file
    (lambda (fd)
      (let ((old-fam (with-file-monitor))) 
	(set! (with-file-monitor) #f) ; no need to monitor these guys
	(format fd ";;; sound data from ~S" main-dir)
	(if (not (char=? (main-dir (- (length main-dir) 1)) #\/))
	    (set! main-dir (string-append main-dir "/")))
	(for-each
	 (lambda (dir)
	   (if (not (char=? (dir 0) #\.))
	       (let ((ins-name (lower-case-and-no-spaces dir)))
		 (format fd "~%~%;;; ---------------- ~A ----------------" dir)
		 (if (string=? dir "Piano")
		     (for-each
		      (lambda (inner-dir)
			(if (not (char=? (inner-dir 0) #\.))
			    (do-one-directory fd (string-append main-dir dir "/" inner-dir) ins-name .001 .0001))) ; pp piano notes are .01 maxamp and short
		      (directory->list (string-append main-dir dir)))
		     (do-one-directory fd (string-append main-dir dir) ins-name)))))
	 (directory->list main-dir))
	(set! (with-file-monitor) old-fam)))))

;;; (sounds->segment-data "/home/bil/test/iowa/sounds/" "iowa.data")
|#





;;; not replaced:
;;;   dac-hook stop-dac-hook with-level-meters make-level-meter save-mixes
;;;   mus-sound-close-input mus-sound-close-output mus-sound-open-input mus-sound-open-output
;;;   mus-sound-read mus-sound-reopen-output mus-sound-seek-frame mus-sound-write

(define continue-float-vector->file continue-frample->file)

(define continue-frame->file continue-frample->file)

(define file->float-vector file->frample)

(define file->float-vector? file->frample?)

(define file->frame file->frample)

(define file->frame? file->frample?)

(define float-vector->file frample->file)

(define float-vector->file? frample->file?)

(define (float-vector-mix f m o)
  (frample->frample m f o))

(define frame float-vector)

(define (frame* f1 f2 outf)
  (float-vector-multiply! (copy f1 (or outf (make-float-vector (length f1)))) f2))

(define (frame+ f1 f2 outf)
  (float-vector-add! (copy f1 (or outf (make-float-vector (length f1)))) f2))

(define frame->file frample->file)

(define frame->file? frample->file?)

(define (frame->frame a b c)
  (frample->frample (if (> (length a) (length b))
			(values a b) 
			(values b a)) 
		    c))

(define (frame->list a) 
  (copy a (make-list (length a))))

(define frame->sample dot-product)

(define sample->frame frame*)

(define frame-ref float-vector-ref)

(define frame-set! float-vector-set!)

(define (frame? obj)
  (and (float-vector? obj)
       (= (vector-rank obj) 1)))

(define frames framples)

(define make-file->float-vector make-file->frample)

(define make-file->frame make-file->frample)

(define make-float-vector->file make-frample->file)

(define (make-frame chans . args)
  (do ((v (make-float-vector chans))
       (i 0 (+ i 1))
       (arg args (cdr arg)))
      ((null? arg) v)
    (set! (v i) (car arg))))

(define make-frame! make-frame)

(define make-frame->file make-frample->file)

(define (mixer-rows mx) (floor (sqrt (length mx))))

(define (make-mixer chans . args)
  (do ((v (make-float-vector (* chans chans)))
       (i 0 (+ i 1))
       (arg args (cdr arg)))
      ((null? arg) v)
    (set! (v i) (car arg))))

(define make-mixer! make-mixer)

(define (make-scalar-mixer chans val)
  (do ((m (make-float-vector (* chans chans)))
       (i 0 (+ i 1)))
      ((= i chans) m)
    (set! (m (+ (* i chans) i)) val)))

(define (make-sound-data c f)
  (make-float-vector (list c f)))

(define (mixer . args)
  (apply make-mixer (floor (sqrt (length args))) args))

(define mixer+ frame+)

(define (mixer-ref mx i j)
  (mx (+ (* i (mixer-rows mx)) j)))

(define (mixer-set! mx i j val)
  (set! (mx (+ (* i (mixer-rows mx)) j)) val))

(define (mixer? obj)
  (and (float-vector? obj)
       (= (vector-rank obj) 1)
       (let ((d (mixer-rows mx)))
	 (= (* d d) (length obj)))))

(define* (mixer* m1 m2 m3)
  (do ((rows (mixer-rows m1))
       (m4 (or m3 (make-float-vector (length m1))))
       (i 0 (+ i 1)))
      ((= i rows) m4)
    (do ((j 0 (+ j 1)))
	((= j rows))
      (set! (m4 (+ (* i rows) j))
	    (do ((sum 0.0)
		 (k 0 (+ k 1)))
		((= k rows) sum)
	      (set! sum (+ sum (* (m1 (+ (* i rows) k)) (m2 (+ (* k rows) j))))))))))

(define multiply-arrays float-vector-multiply!)

(define mus-sound-frames mus-sound-framples)

(define region-frames region-framples)

(define selection-frames selection-framples)

(define (sound-data+ x1 x2)
  (if (real? x1)
      ((if (real? x2) + float-vector-offset!) x2 x1)
      ((if (real? x2) float-vector-offset! float-vector-add!) x1 x2)))

(define (sound-data* x1 x2)
  (if (real? x1)
      ((if (real? x2) * float-vector-scale!) x2 x1)
      ((if (real? x2) float-vector-scale! float-vector-multiply!) x1 x2)))

(define sound-data-add! float-vector-add!)

(define (sound-data-chans s) 
  ((vector-dimensions s) 0))

(define sound-data-copy copy)

(define sound-data-fill! fill!)

(define (sound-data-length s) 
  ((vector-dimensions s) 1))

(define (sound-data-maxamp s)
  (let ((chans (sound-data-chans s)))
    (do ((maxamps (make-list chans))
	 (i 0 (+ i 1)))
	((= i chans) maxamps)
      (set! (maxamps i) (float-vector-peak (s i))))))

(define sound-data-multiply! float-vector-multiply!)

(define sound-data-offset! float-vector-offset!)

(define sound-data-peak float-vector-peak)

(define sound-data-ref float-vector-ref)

(define (sound-data-reverse! s)
  (let ((chans (sound-data-chans s)))
    (do ((i 0 (+ i 1)))
	((= i chans) s)
      (set! (s i) (reverse (s i))))))

(define sound-data-scale! float-vector-scale!)

(define sound-data-set! float-vector-set!)

(define (sound-data? s) 
  (and (float-vector? s) 
       (= (vector-rank s) 2)))

(define transform-frames transform-framples)

(define vct-copy copy)
(define vct-fill! fill!)
(define vct float-vector)
(define vct-length length)
(define vct-reverse! reverse!)  ; slight difference: no optional length arg (use make-shared-vector)
(define vct->list vector->list)
(define (list->vct x) (apply float-vector x))
(define make-vct make-float-vector)
(define (vector->vct v) (copy v (make-float-vector (length v))))
(define (vct->vector v) (copy v (make-vector (length v))))
(define vct? float-vector?)
