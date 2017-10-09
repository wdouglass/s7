;;; examples of extensions to Snd's graphics

(provide 'snd-draw.scm)

(define overlay-rms-env
  
  ;; these functions are an optimization to speed up calculating the rms env graph.
  ;; ideally we'd use something like:
  ;;
  ;;   (let* ((x1 (x->position (/ i (srate)) snd chn))
  ;;          (y1 (y->position (moving-rms rms (reader)) snd chn)))
  ;;     (draw-line x0 y0 x1 y1)
  ;;
  ;; in the do-loop below that runs through the samples, but I haven't added x|y->position or draw-line
  ;; to the optimizer ("run"), and each would be looking up the graph axis info on each call even if
  ;; available to the optimizer -- this seems wasteful.  So, the grf-it function below is using the
  ;; axis info in axinf to get the pixel location for the envelope line segment break point.
  ;; Also, draw-lines takes a vector for some reason, so we need to tell "run" that it is an
  ;; integer vector (and preload it with 0).  We save the vector in the channel property 'rms-lines,
  ;; and the associated axis info in 'rms-axis-info.  Since redisplay is common in Snd, it reduces
  ;; flicker a lot to have this data instantly available.
  
  (let ((pack-x-info (lambda (axinf)
		       (float-vector (axinf 2) ;  x0
				     (axinf 4) ;  x1
				     (axinf 10) ; x_axis_x0
				     (axinf 12) ; x_axis_x1
				     (axinf 15) ; scale
				     (- (axinf 10) (* (axinf 2) (axinf 15)))))) ; base
	(pack-y-info (lambda (axinf)
			      (float-vector (axinf 3) ;  y0
					    (axinf 5) ;  y1
					    (axinf 11) ; y_axis_y0
					    (axinf 13) ; y_axis_y1
					    (axinf 16) ; scale
					    (- (axinf 11) (* (axinf 3) (axinf 16)))))) ; base
	(grf-it (lambda (val v)
		  (round
		   (if (>= val (v 1))
		       (v 3)
		       (if (<= val (v 0))
			   (v 2)
			   (+ (v 5) (* val (v 4))))))))
	
	(make-moving-rms (lambda* ((size 128))
			   (make-moving-average size)))
	
	(moving-rms (lambda (gen y)
		      (sqrt (moving-average gen (* y y))))))
    
    (lambda (snd chn)
      (let ((red (make-color 1 0 0))            ; rms env displayed in red
	    (left (left-sample snd chn))
	    (right (right-sample snd chn))
	    (rms-size 128)                      ; this could be a parameter -- not sure what the "right" size is
	    (sr (/ 1.0 (srate snd)))
	    (old-color (foreground-color snd chn))
	    (axinf (axis-info snd chn))
	    (old-axinf (channel-property 'rms-axis-info snd chn))
	    (cr (make-cairo (car (channel-widgets snd chn)))))
	
	(if (equal? axinf old-axinf)                    ; the previously calculated lines can be re-used
	    (begin
	      (set! (foreground-color snd chn) red)
	      (draw-lines (channel-property 'rms-lines snd chn) snd chn time-graph cr)
	      (set! (foreground-color snd chn) old-color))
	    (let ((start (max 0 (- left rms-size))))
	      (let ((xdata (pack-x-info axinf))
		    (ydata (pack-y-info axinf))
		    (reader (make-sampler start snd chn))
		    (rms (make-moving-rms rms-size))
		    (x0 0)
		    (y0 0)
		    (line-ctr 2)
		    (lines (make-vector (* 2 (- (+ (axinf 12) 1) (axinf 10))) 0)))
		(dynamic-wind
		    (lambda ()
		      (set! (foreground-color snd chn) red))
		    (lambda ()
		      (if (< start left)                 ; check previous samples to get first rms value
			  (do ((i start (+ 1 i))) 
			      ((= i left))
			    (moving-rms rms (reader))))
		      (let ((first-sample (next-sample reader)))
			(set! x0 (grf-it (* left sr) xdata))
			(set! y0 (grf-it first-sample ydata)))
		      (set! (lines 0) x0)                ; first graph point
		      (set! (lines 1) y0)
		      (do ((i (+ left 1) (+ 1 i)))       ; loop through all samples calling moving-rms
			  ((= i right))
			(let ((x1 (grf-it (* i sr) xdata))
			      (y (moving-rms rms (next-sample reader))))
			  (if (> x1 x0)                 ; very often many samples are represented by one pixel
			      (let ((y1 (grf-it y ydata)))
				(set! (lines line-ctr) x1)
				(set! (lines (+ 1 line-ctr)) y1)
				(set! line-ctr (+ line-ctr 2))
				(set! x0 x1)
				(set! y0 y1)))))      ; else should we do "max" here? or draw a vertical line from min to max?
		      (if (< line-ctr (length lines))
			  (do ((j line-ctr (+ j 2)))       ; off-by-one in vector size calc -- need to pad so we don't get a bogus line to (0, 0)
			      ((>= j (length lines)))
			    (set! (lines j) x0)
			    (set! (lines (+ j 1)) y0)))
		      (draw-lines lines snd chn time-graph cr)
		      (set! (channel-property 'rms-lines snd chn) lines)  ; save current data for possible redisplay
		      (set! (channel-property 'rms-axis-info snd chn) axinf))
		    (lambda ()
		      (set! (foreground-color snd chn) old-color))))))
	(free-cairo cr)))))
    
;; (hook-push after-graph-hook (lambda (hook) (overlay-rms-env (hook 'snd) (hook 'chn))))


(define display-colored-samples 
  (let ((+documentation+ "(display-colored-samples color beg dur snd chn) displays samples from beg for dur in color 
whenever they're in the current view."))
    (lambda* (color beg dur snd chn)
      (let ((left (left-sample snd chn))
	    (right (right-sample snd chn))
	    (end (+ beg dur))
	    (old-color (foreground-color snd chn))
	    (cr (make-cairo (car (channel-widgets snd chn)))))
	(when (and (< left end)
		   (> right beg))
	  (let ((data (make-graph-data snd chn)))
	    (if (float-vector? data)
		(let ((new-data (let ((samps (- (min right end) (max left beg)))
				      (offset (max 0 (- beg left))))
				  (float-vector-subseq data offset (+ offset samps)))))
		  (set! (foreground-color snd chn) color)
		  (graph-data new-data snd chn copy-context (max beg left) (min end right) (time-graph-style snd chn) cr)
		  (set! (foreground-color snd chn) old-color))
		(let* ((size (length (car data)))
		       (samps (- right left))
		       (left-bin (floor (/ (* size (max 0 (- beg left))) samps)))
		       (right-bin (floor (/ (* size (- (min end right) left)) samps)))
		       (new-low-data (float-vector-subseq (car data) left-bin right-bin))
		       (new-high-data (float-vector-subseq (cadr data) left-bin right-bin)))
		  (set! (foreground-color snd chn) color)
		  (graph-data (list new-low-data new-high-data) snd chn copy-context left-bin right-bin (time-graph-style snd chn) cr)
		  (set! (foreground-color snd chn) old-color)))))
	(free-cairo cr)))))


(define (display-samples-in-color hook)
  (let ((snd (hook 'snd))
	(chn (hook 'chn)))
    ;; intended as after-graph-hook member 
    ;; run through 'colored-samples lists passing each to display-colored-samples
    (let ((colors (channel-property 'colored-samples snd chn)))
      (if (pair? colors)
	  (for-each
	   (lambda (vals)
	     (apply display-colored-samples (append vals (list snd chn))))
	   colors)))))


(define color-samples 
  (let ((+documentation+ "(color-samples color beg dur snd chn) causes samples from beg to beg+dur to be displayed in color"))
    (lambda* (color ubeg udur usnd uchn)
      (if (not (member display-samples-in-color (hook-functions after-graph-hook)))
	  (hook-push after-graph-hook display-samples-in-color))
      (let ((snd (or usnd (selected-sound) (car (sounds)))))
	(let ((chn (or uchn (selected-channel snd) 0))
	      (beg (or ubeg 0)))
	  (let ((dur (or udur (- (framples snd chn) beg)))
		(old-colors (or (channel-property 'colored-samples snd chn) ())))
	    (set! (channel-property 'colored-samples snd chn) (cons (list color beg dur) old-colors))
	    (update-time-graph snd chn)))))))


(define uncolor-samples 
  (let ((+documentation+ "(uncolor-samples snd chn) cancels sample coloring in the given channel"))
    (lambda* (usnd uchn)
      (let* ((snd (or usnd (selected-sound) (car (sounds))))
	     (chn (or uchn (selected-channel snd) 0)))
	(set! (channel-property 'colored-samples snd chn) ())
	(update-time-graph snd chn)))))


(define display-previous-edits 
  (let ((+documentation+ "(display-previous-edits snd chn) displays all edits of the current sound, with older versions gradually fading away"))
    (lambda (snd chn)
      (let ((edits (edit-position snd chn)))
	(when (> edits 0)
	  (let* ((old-color (foreground-color snd chn))
		 (clist (color->list old-color)))
	    (let ((rinc (/ (- 1.0 (car clist)) (+ edits 1)))
		  (ginc (/ (- 1.0 (cadr clist)) (+ edits 1)))
		  (binc (/ (- 1.0 (caddr clist)) (+ edits 1)))
		  (cr (make-cairo (car (channel-widgets snd chn))))) 
	      (do ((pos 0 (+ 1 pos))
		   (re (- 1.0 rinc) (- re rinc))
		   (ge (- 1.0 ginc) (- ge ginc))
		   (be (- 1.0 binc) (- be binc)))
		  ((> pos edits))
		(let ((data (make-graph-data snd chn pos)))
		  (set! (foreground-color snd chn) (make-color re ge be))
		  (graph-data data snd chn copy-context #f #f (time-graph-style snd chn) cr)))
	      (set! (foreground-color snd chn) old-color)
	      (free-cairo cr))))))))


(define overlay-sounds
  (let ((+documentation+ "(overlay-sounds . args) overlays onto its first argument all subsequent arguments: (overlay-sounds 1 0 3)"))
    (lambda args
      (let ((base (if (integer? (car args)) 
		      (integer->sound (car args)) 
		      (car args))))
	(hook-push after-graph-hook
		   (lambda (hook)
		     (let ((snd (hook 'snd))
			   (chn (hook 'chn)))
		       (if (equal? snd base)
			   (let ((cr (make-cairo (car (channel-widgets snd chn)))))
			     (for-each 
			      (lambda (nsnd)
				(if (and (sound? nsnd)
					 (> (chans nsnd) chn))
				    (graph-data (make-graph-data nsnd chn) base chn copy-context #f #f graph-dots cr)))
			      (cdr args))
			     (free-cairo cr))))))))))


(define samples-via-colormap 
  (let ((+documentation+ "(samples-via-colormap snd chn) displays time domain graph using current colormap (just an example of colormap-ref)"))
    (lambda (snd chn)
      (let ((data (make-graph-data snd chn))
	    (cr (make-cairo (car (channel-widgets snd chn)))))
	
	(define (samples-1 cur-data)
	  (let ((left (left-sample snd chn))
		(right (right-sample snd chn))
		(old-color (foreground-color snd chn))
		(y0 (y->position (cur-data 0)))
		(colors (make-vector *colormap-size* #f))
		(len (length cur-data)))
	    (let ((x0 (x->position (/ left (srate snd))))
		  (incr (/ (- (+ right 1) left) len)))
	      (do ((i (+ left incr) (+ i incr))
		   (j 1 (+ 1 j)))
		  ((or (>= i right)
		       (>= j len)))
		(let ((x1 (x->position (/ i (srate snd))))
		      (y1 (y->position (cur-data j))))
		  (let* ((x (abs (cur-data j)))
			 (ref (floor (* *colormap-size* x))))
		    (set! (foreground-color snd chn) 
			  (or (colors ref)
			      (let ((new-color (apply make-color (colormap-ref (colormap) x))))
				(set! (colors ref) new-color)))))
		  (draw-line x0 y0 x1 y1 snd chn time-graph cr)
		  (set! x0 x1)
		  (set! y0 y1)))
	      (set! (foreground-color snd chn) old-color))))

	(if data
	    (if (float-vector? data)
		(samples-1 data)
		(begin
		  (samples-1 (car data))
		  (samples-1 (cadr data)))))
	(free-cairo cr)))))


